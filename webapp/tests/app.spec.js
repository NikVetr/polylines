// End-to-end checks of the webapp UI. Run with: npx playwright test (from webapp/).
const { test, expect } = require('@playwright/test');
const path = require('node:path');
const fs = require('node:fs');

test.beforeEach(async ({ page }) => {
  page.on('pageerror', (err) => { throw err; });
  await page.goto('/index.html');
  await page.evaluate(() => localStorage.clear());
  await page.evaluate(() => window.SankeyApp.loadExample('labeled'));
});

const state = (page) => page.evaluate(() => JSON.parse(JSON.stringify(window.SankeyApp.state)));
const ribbonCenter = (page, index) => page.evaluate((k) => {
  const app = window.SankeyApp;
  const f = app.layout.flows[k];
  const mid = f.centerline[Math.floor(f.centerline.length / 2)];
  const box = document.querySelector('#plot').getBoundingClientRect();
  return { x: box.left + 18 + mid[0], y: box.top + (box.height - 18) - mid[1] };
}, index);

test('renders the default example without errors', async ({ page }) => {
  await expect(page.locator('#plot .ribbon')).toHaveCount(14);
  await expect(page.locator('#plot .node-label')).toHaveCount(9);
  await expect(page.locator('#banner')).toBeHidden();
});

test('hovering a strand shows its details in the inspector and dims the others', async ({ page }) => {
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#hover-info')).toBeVisible();
  await expect(page.locator('#hover-info')).toContainText('→');
  await expect(page.locator('#hover-info')).toContainText('of source outflow');
  await expect(page.locator('#inspector-main')).toBeHidden();
  await expect(page.locator('#plot')).toHaveClass(/dimmed/);
  expect(await page.locator('#plot .ribbon.hl').count()).toBe(1);
  await page.mouse.move(5, 5);
  await expect(page.locator('#hover-info')).toBeHidden();
  await expect(page.locator('#inspector-main')).toContainText('Inspector');
});

test('dragging a node moves it and undo restores it', async ({ page }) => {
  const before = (await state(page)).loc[4].y;
  const dot = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes[4];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { x: box.left + 18 + n.X, y: box.top + (box.height - 18) - n.Y };
  });
  await page.mouse.move(dot.x, dot.y);
  await page.mouse.down();
  await page.mouse.move(dot.x, dot.y + 40, { steps: 5 });
  await page.mouse.up();
  const after = (await state(page)).loc[4].y;
  expect(after).toBeLessThan(before);
  await page.keyboard.press('Control+z');
  expect((await state(page)).loc[4].y).toBeCloseTo(before, 6);
});

test('dragging a strand reorders it in the stack at its nearer end', async ({ page }) => {
  // Strand from "Clinical A" (node key 1:1) to the lower mediator, grabbed near its source.
  const info = await page.evaluate(() => {
    const app = window.SankeyApp;
    const f = app.layout.flows.find((x) => x.stage === 0 && x.i === 0 && x.j === 1);
    const pt = f.centerline[Math.floor(f.centerline.length * 0.12)];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { key: f.key, offset: f.startOffset, x: box.left + 18 + pt[0], y: box.top + (box.height - 18) - pt[1] };
  });
  await page.mouse.move(info.x, info.y);
  await page.mouse.down();
  await page.mouse.move(info.x, info.y - 60, { steps: 6 });
  await page.mouse.up();
  const s = await state(page);
  expect(s.manualOrder.start['1:1']).toBeTruthy();
  expect(s.manualOrder.start['1:1'][s.manualOrder.start['1:1'].length - 1]).toBe(info.key);
  const offset = await page.evaluate((key) => window.SankeyApp.layout.flows.find((f) => f.key === key).startOffset, info.key);
  expect(offset).toBeGreaterThan(info.offset);
  const bundle = await page.evaluate(() => window.SankeyApp.bundleFiles());
  const stackOrder = bundle.find((f) => f.name === 'stack_order.csv');
  expect(stackOrder.text).toContain('start,1,1,2,');
  expect(bundle.find((f) => f.name === 'sankey.R').text).toContain('stack_order = stack_order');
});

test('selecting a strand lets you edit its color and value', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.state.style.gradient = false;
    window.SankeyApp.update();
  });
  const p = await ribbonCenter(page, 2);
  await page.mouse.click(p.x, p.y);
  await expect(page.locator('#inspector-main .edit-head')).toHaveText('Edit strand');
  const color = page.locator('#inspector [data-color="flow"] input[type="color"]');
  await color.evaluate((el) => {
    el.value = '#ff0000';
    el.dispatchEvent(new Event('change', { bubbles: true }));
  });
  await expect(page.locator('#plot .ribbon[data-flow="2"]')).toHaveAttribute('fill', '#FF0000');
  const key = await page.evaluate(() => window.SankeyApp.layout.flows[2].key);
  expect((await state(page)).flowOverrides[key]).toMatch(/^#FF0000/);
  await page.locator('#ins-value').fill('0.2');
  await page.locator('#ins-value').dispatchEvent('change');
  const value = await page.evaluate((k) => window.SankeyApp.layout.flows.find((f) => f.key === k).value, key);
  expect(value).toBe(0.2);
});

test('uploading CSV files builds a graph Sankey and bad files fail loudly', async ({ page }, testInfo) => {
  const dir = testInfo.outputPath('upload');
  fs.mkdirSync(dir, { recursive: true });
  const adjacency = path.join(dir, 'adjacency.csv');
  const loc = path.join(dir, 'loc.csv');
  fs.writeFileSync(adjacency, '0,0,0.05,0.02\n0,0,0.03,0\n0,0,0,0.08\n0,0,0,0\n');
  fs.writeFileSync(loc, 'set,node,x,y,label\n1,1,0,0.7,A\n1,2,0,0.3,B\n2,1,1,0.5,C\n3,1,2,0.55,D\n');
  await page.setInputFiles('#file-input', [adjacency, loc]);
  await expect(page.locator('#mode-select')).toHaveValue('graph');
  await expect(page.locator('#plot .ribbon')).toHaveCount(4);
  await expect(page.locator('#banner')).toBeHidden();

  const bad = path.join(dir, 'broken.csv');
  fs.writeFileSync(bad, 'set,node,x,y\n1,1,0,abc\n');
  await page.setInputFiles('#file-input', [bad]);
  await expect(page.locator('#banner')).toBeVisible();
  await expect(page.locator('#banner')).toContainText('not a number');
});

test('matrices without a loc file get generated node locations', async ({ page }, testInfo) => {
  const dir = testInfo.outputPath('auto');
  fs.mkdirSync(dir, { recursive: true });
  const s1 = path.join(dir, 'stage_1.csv');
  const s2 = path.join(dir, 'stage_2.csv');
  fs.writeFileSync(s1, ',Out A,Out B\nIn 1,0.1,0.05\nIn 2,0.02,0.08\n');
  fs.writeFileSync(s2, '0.06,0.06\n0.1,0.03\n');
  await page.setInputFiles('#file-input', [s2, s1]);
  await expect(page.locator('#notice')).toContainText('Generated node locations');
  await expect(page.locator('#plot .ribbon')).toHaveCount(8);
  const s = await state(page);
  expect(s.stageNames).toEqual(['stage_1.csv', 'stage_2.csv']);
  expect(s.loc.filter((r) => r.label).map((r) => r.label)).toEqual(['In 1', 'In 2', 'Out A', 'Out B']);
});

test('boundary routing can be set per stage', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('gradients'));
  await page.locator('#route-scope').selectOption('2');
  await page.locator('#ctl-route').selectOption('boundary');
  const s = await state(page);
  expect(s.stageParams[2].route).toBe('boundary');
  expect(s.params.route).toBe('bezier');
  await expect(page.locator('#banner')).toBeHidden();
});

test('exports an SVG without interaction layers and an R bundle', async ({ page }) => {
  await page.locator('#export-btn').click();
  const [svgDownload] = await Promise.all([page.waitForEvent('download'), page.locator('[data-export="svg"]').click()]);
  const svgText = fs.readFileSync(await svgDownload.path(), 'utf8');
  expect(svgText).toContain('<svg');
  expect(svgText).not.toContain('layer-hits');

  const bundle = await page.evaluate(() => window.SankeyApp.bundleFiles());
  const files = bundle.map((f) => f.name);
  expect(files).toEqual(expect.arrayContaining(['stage_1.csv', 'stage_2.csv', 'colors_1.csv', 'loc.csv', 'sankey.R', 'project.json']));
  // Every label here sits inside its node body, so R gets no (zero-length) positioned text() call.
  const code = bundle.find((f) => f.name === 'sankey.R').text;
  expect(code).toContain('labels$label[centred]');
  expect(code).not.toContain('labels$label[!centred]');
  await page.locator('#export-btn').click();
  const [zip] = await Promise.all([page.waitForEvent('download'), page.locator('[data-export="bundle"]').click()]);
  expect(zip.suggestedFilename()).toBe('sankey_r_bundle.zip');
  expect(fs.readFileSync(await zip.path()).subarray(0, 2).toString()).toBe('PK');
});

test('undo after hovering a strand that no longer exists does not crash', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('gradients'));
  const p = await ribbonCenter(page, 27);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#hover-info')).toBeVisible();
  await page.keyboard.press('Control+z');
  await expect(page.locator('#plot .ribbon')).toHaveCount(14);
  await expect(page.locator('#stage-list li')).toHaveCount(2);
  await expect(page.locator('#redo')).toBeEnabled();
});

test('moving a stage carries its color matrix and per-stage settings', async ({ page }) => {
  await page.evaluate(() => {
    const s = window.SankeyApp.state;
    s.stageParams = [{ source_order: 'target_y_desc' }];
    window.SankeyApp.update();
  });
  const before = await state(page);
  await page.locator('#stage-list li').first().locator('[data-dir="1"]').click();
  const after = await state(page);
  expect(after.stageNames).toEqual([before.stageNames[1], before.stageNames[0]]);
  expect(after.colorNames).toEqual([before.colorNames[1], before.colorNames[0]]);
  expect(after.colorMatrices[0]).toEqual(before.colorMatrices[1]);
  expect(after.stageParams[1]).toEqual({ source_order: 'target_y_desc' });
  expect(after.stageParams[0] || null).toBeNull();
});

test('inspector color sliders keep working through a drag', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.state.style.gradient = false;
    window.SankeyApp.update();
  });
  const p = await ribbonCenter(page, 2);
  await page.mouse.click(p.x, p.y);
  const slider = page.locator('#inspector [data-color="flow"] input[type="range"]');
  const handle = await slider.elementHandle();
  for (const v of ['0.6', '0.4', '0.2']) {
    await handle.evaluate((el, value) => {
      el.value = value;
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }, v);
    await page.waitForTimeout(40);
  }
  expect(await handle.evaluate((el) => el.isConnected)).toBe(true);
  await expect(page.locator('#plot .ribbon[data-flow="2"]')).toHaveAttribute('fill-opacity', '0.2');
});

test('toggling a mid color on and off restores the default gradient', async ({ page }) => {
  const p = await ribbonCenter(page, 2);
  await page.mouse.click(p.x, p.y);
  const key = await page.evaluate(() => window.SankeyApp.layout.flows[2].key);
  await page.locator('#ins-mid-on').check();
  expect((await state(page)).midOverrides[key]).toBeTruthy();
  await page.locator('#ins-mid-on').uncheck();
  expect(key in (await state(page)).midOverrides).toBe(false);
});

test('clearing a node x position is rejected', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.update());
  const dot = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes[4];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { x: box.left + 18 + n.X, y: box.top + (box.height - 18) - n.Y };
  });
  await page.mouse.click(dot.x, dot.y);
  await page.locator('#ins-x').fill('');
  await page.locator('#ins-x').dispatchEvent('change');
  await expect(page.locator('#banner')).toContainText('x cannot be empty');
  expect((await state(page)).loc[4].x).toBe(1.2);
});

test('graph-mode R export keeps stage settings and node colors', async ({ page }) => {
  await page.evaluate(() => {
    const app = window.SankeyApp;
    app.loadExample('graph');
    app.state.stageParams = [{ source_order: 'target_y_desc' }];
    app.state.nodeColors['2:1'] = '#FF0000';
    app.update();
  });
  const code = await page.evaluate(() => window.SankeyApp.bundleFiles().find((f) => f.name === 'sankey.R').text);
  expect(code).toContain('source_order = "target_y_desc"');
  expect(code).toContain('merge_cols = merge_cols');
  expect(code).toContain('#FF0000');
});

test('the table importer builds a Sankey from a pasted wide table', async ({ page }) => {
  await page.locator('.topbar [data-open-import]').click();
  await expect(page.locator('#import-dialog')).toBeVisible();
  await page.locator('#import-text').fill('region,plan,outcome,customers\nNorth,Basic,Renewed,120\nNorth,Pro,Churned,30\n' +
    'South,Basic,Churned,45\nSouth,Pro,Renewed,80\nWest,Pro,Renewed,25\n');
  await expect(page.locator('#import-summary')).toContainText('2 stages · 7 nodes · 9 strands');
  await expect(page.locator('#import-format')).toHaveValue('wide');
  await page.locator('#import-labels').selectOption('beside');
  await page.locator('#import-apply').click();
  await expect(page.locator('#import-dialog')).toBeHidden();
  await expect(page.locator('#plot .ribbon')).toHaveCount(9);
  const s = await state(page);
  expect(s.stageNames).toEqual(['region → plan', 'plan → outcome']);
  expect(s.valueScale).not.toBe(1);
  expect(s.loc.every((r) => r.width === 0)).toBe(true);
  // Imported tables carry pathways, so hovering a strand traces the pathways through it.
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#hover-info')).toContainText('North → Basic → Renewed');
  await expect(page.locator('#hover-info')).toContainText('120 customers');
});

test('opening a long table file starts the importer, which flags bad mappings', async ({ page }, testInfo) => {
  const file = testInfo.outputPath('visits.csv');
  fs.writeFileSync(file, 'patient,visit,status\nA,1,sick\nA,2,well\nB,1,sick\nB,2,sick\nC,1,well\nC,2,well\n');
  await page.setInputFiles('#file-input', file);
  await expect(page.locator('#import-dialog')).toBeVisible();
  await expect(page.locator('#import-format')).toHaveValue('long');
  await expect(page.locator('#import-preview th')).toHaveCount(3);
  await expect(page.locator('#import-summary')).toContainText('1 stage · 4 nodes · 3 strands');
  await page.locator('#imp-category').selectOption('visit');
  await expect(page.locator('#import-summary')).toHaveClass(/error/);
  await expect(page.locator('#import-apply')).toBeDisabled();
  await page.locator('#imp-category').selectOption('status');
  await page.locator('#import-apply').click();
  await expect(page.locator('#plot .ribbon')).toHaveCount(3);
});

test('all-numeric link tables open the importer and matrix typos report the bad cell', async ({ page }, testInfo) => {
  const links = testInfo.outputPath('links.csv');
  fs.writeFileSync(links, 'source,target,value\n1,2,10\n1,3,5\n2,4,7\n3,4,5\n');
  await page.setInputFiles('#file-input', links);
  await expect(page.locator('#import-dialog')).toBeVisible();
  await expect(page.locator('#import-format')).toHaveValue('links');
  await page.locator('#import-dialog button[value="cancel"]').click();

  const typo = testInfo.outputPath('stage_1.csv');
  fs.writeFileSync(typo, '"V1","V2"\n0.1,0.2\n0.3,O.4\n');
  await page.setInputFiles('#file-input', typo);
  await expect(page.locator('#import-dialog')).toBeHidden();
  await expect(page.locator('#banner')).toContainText('"O.4" is not a number');
});

test('imports keep numeric category names and later matrices reset the value scale', async ({ page }, testInfo) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const table = T.parseTable('before,after,n\n1,2,30\n1,3,10\n2,3,20\n');
    window.SankeyApp.applyImport(T.build(table, { format: 'wide', columns: ['before', 'after'], weight: 'n' }), { labels: 'beside' });
  });
  let s = await state(page);
  expect(s.loc.map((r) => r.label)).toEqual(['1', '2', '2', '3']);
  expect(s.valueScale).not.toBe(1);
  const code = await page.evaluate(() => window.SankeyApp.bundleFiles().find((f) => f.name === 'sankey.R').text);
  expect(code).toContain('pos = c(2, 2, 4, 4)');

  const m1 = testInfo.outputPath('m_1.csv');
  fs.writeFileSync(m1, '0.1\n0.05\n0.02\n');
  await page.setInputFiles('#file-input', m1);
  await expect(page.locator('#notice')).toContainText('Generated node locations');
  s = await state(page);
  expect(s.valueScale).toBe(1);
  expect(s.loc.every((r) => !r.label)).toBe(true);
});

test('path gradients reach their node colors where strands meet the merge band', async ({ page }) => {
  const stops = await page.evaluate(() => {
    const app = window.SankeyApp;
    app.state.style.gradientMode = 'path';
    app.update();
    const L = app.layout;
    return L.flows.map((f) => {
      const offsets = [...document.querySelectorAll(`#gs${f.index} stop`)].map((s) => parseFloat(s.getAttribute('offset')));
      return { first: offsets[0], last: offsets[offsets.length - 1], band: 100 * Math.min(L.mergeLength / Math.abs(f.endX - f.startX), 0.25) };
    });
  });
  expect(stops.length).toBe(14);
  stops.forEach((s) => {
    expect(s.band).toBeGreaterThan(0);
    expect(Math.abs(s.first - s.band)).toBeLessThan(0.02);
    expect(Math.abs(s.last - (100 - s.band))).toBeLessThan(0.02);
  });
});

const nodeAt = (page, label) => page.evaluate((l) => {
  const n = window.SankeyApp.layout.nodes.find((x) => x.label === l);
  const box = document.querySelector('#plot').getBoundingClientRect();
  return { x: box.left + 18 + n.X, y: box.top + (box.height - 18) - n.Y, dataX: n.x };
}, label);

test('dragging a node into its neighbour stops at a wall by default', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('graph'));
  const relay = await nodeAt(page, 'Relay');
  const sink = await nodeAt(page, 'Sink');
  await page.mouse.move(relay.x, relay.y);
  await page.mouse.down();
  await page.mouse.move(sink.x + 80, relay.y, { steps: 15 });
  await page.mouse.up();
  await expect(page.locator('#banner')).toBeHidden();
  await expect(page.locator('#plot')).not.toHaveClass(/stale/);
  const relayAfter = await nodeAt(page, 'Relay');
  const sinkAfter = await nodeAt(page, 'Sink');
  expect(relayAfter.dataX).toBeGreaterThan(relay.dataX + 0.2);
  expect(relayAfter.dataX).toBeLessThan(sinkAfter.dataX);
  expect(sinkAfter.dataX).toBeCloseTo(sink.dataX, 6);
  // The node can still be picked up and dragged back.
  await page.mouse.move(relayAfter.x, relayAfter.y);
  await page.mouse.down();
  await page.mouse.move(relayAfter.x - 100, relayAfter.y, { steps: 5 });
  await page.mouse.up();
  expect((await nodeAt(page, 'Relay')).dataX).toBeLessThan(relayAfter.dataX);
});

test('push mode shoves colliding neighbours along instead', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('graph');
    window.SankeyApp.state.style.collision = 'push';
    window.SankeyApp.update();
  });
  const relay = await nodeAt(page, 'Relay');
  const sink = await nodeAt(page, 'Sink');
  await page.mouse.move(relay.x, relay.y);
  await page.mouse.down();
  await page.mouse.move(sink.x + 40, relay.y, { steps: 15 });
  await page.mouse.up();
  await expect(page.locator('#banner')).toBeHidden();
  const relayAfter = await nodeAt(page, 'Relay');
  const sinkAfter = await nodeAt(page, 'Sink');
  expect(relayAfter.dataX).toBeGreaterThan(sink.dataX);
  expect(sinkAfter.dataX).toBeGreaterThan(relayAfter.dataX);
});

test('an invalid edit keeps the last plot dimmed and offers undo', async ({ page }) => {
  await page.evaluate(() => {
    const s = window.SankeyApp.state;
    s.loc[4].x = s.loc[0].x;
    window.SankeyApp.update({ commit: true });
  });
  await expect(page.locator('#plot')).toHaveClass(/stale/);
  await expect(page.locator('#plot .ribbon').first()).toBeAttached();
  await page.locator('#banner .banner-action').click();
  await expect(page.locator('#plot')).not.toHaveClass(/stale/);
  await expect(page.locator('#banner')).toBeHidden();
});

test('labeled node bodies tuck under the merge band so no seam shows', async ({ page }) => {
  const overlap = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const n = L.nodes.find((x) => x.label === 'Outcome 1');
    const body = document.querySelector(`path[fill="url(#gn${n.id}_b)"]`).getBBox();
    const band = document.querySelector(`path[fill="url(#gn${n.id}_t)"]`).getBBox();
    return Math.min(body.x + body.width, band.x + band.width) - Math.max(body.x, band.x);
  });
  expect(overlap).toBeGreaterThan(1);
});

test('hovering traces every pathway through a strand or node; Ctrl traces the one under the pointer', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const point = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const f = L.flows.find((x) => x.stage === 0 && L.nodes[x.from].label === 'Stage II' && L.nodes[x.to].label === 'Chemotherapy');
    const c = f.centerline[Math.floor(f.centerline.length * 0.45)];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { x: box.left + 18 + c[0], y: box.top + (box.height - 18) - c[1] - f.w * 0.3 };
  });
  await page.mouse.move(point.x, point.y);
  await expect(page.locator('#plot .trace')).toHaveCount(2);
  await expect(page.locator('#hover-info')).toContainText('2 pathways · 63 patients');
  await expect(page.locator('#hover-info .focus-table').first()).toContainText('none via Stage I, Stage III, Stage IV');
  await page.keyboard.down('Control');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info')).toContainText('41 patients on this pathway');
  await expect(page.locator('#hover-info')).toContainText('Remission');
  await page.keyboard.up('Control');
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x, node.y);
  await expect(page.locator('#plot .trace')).toHaveCount(5);
  await expect(page.locator('#hover-info')).toContainText('5 pathways · 288 patients');
  await expect(page.locator('#hover-info')).toContainText('none via Stage IV');
  // Traced nodes fade like the rest; only their labels stay in focus.
  await expect(page.locator('#plot .trunk.hl')).toHaveCount(0);
  await expect(page.locator('#plot .node-label.hl')).toHaveCount(7);
  // Labels sit above solid nodes, so traces run on through the sink's whole body.
  const reach = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes.find((x) => x.label === 'Remission');
    const right = Math.max(...[...document.querySelectorAll('#plot .trace')].map((el) => { const b = el.getBBox(); return b.x + b.width; }));
    return right - (18 + n.X + n.widthPx / 2);
  });
  expect(Math.abs(reach)).toBeLessThan(1);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace')).toHaveCount(0);
});

test('hovering draws a bar under every node showing the share the selection fills', async ({ page }) => {
  // Plain strands: their two nodes are partly filled, every other node shows an empty bar.
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .node-fill')).toHaveCount(9);
  await expect(page.locator('#plot .node-fill-solid')).toHaveCount(2);
  // Traced pathways: every node they pass through, e.g. 63 of Stage II's 174 patients.
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const point = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const f = L.flows.find((x) => x.stage === 0 && L.nodes[x.from].label === 'Stage II' && L.nodes[x.to].label === 'Chemotherapy');
    const c = f.centerline[Math.floor(f.centerline.length / 2)];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { x: box.left + 18 + c[0], y: box.top + (box.height - 18) - c[1], from: f.from };
  });
  await page.mouse.move(point.x, point.y);
  await expect(page.locator('#plot .node-fill')).toHaveCount(11);
  await expect(page.locator('#plot .node-fill-solid')).toHaveCount(4);
  const missed = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes.find((x) => x.label === 'Stage I');
    return document.querySelector(`#plot .node-fill[data-fill="${n.id}"]`).textContent;
  });
  expect(missed).toBe('0%100%');
  const bar = page.locator(`#plot .node-fill[data-fill="${point.from}"]`);
  await expect(bar).toContainText('36%');
  await expect(bar).toContainText('64%');
  const widths = await bar.locator('rect').evaluateAll((rects) => rects.map((r) => Number(r.getAttribute('width'))));
  expect(widths).toHaveLength(3);
  expect(widths[2] / widths[1]).toBeCloseTo(63 / 174, 2);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .node-fill')).toHaveCount(0);
  const exported = await page.evaluate(() => window.SankeyApp.exportSvgText());
  expect(exported).not.toContain('layer-fills');
});

test('clicking a node pins its traces and breakdown in the inspector until Esc', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Remission');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace')).toHaveCount(5);
  await expect(page.locator('#inspector-main')).toContainText('5 pathways · 288 patients');
  await expect(page.locator('#inspector-main .share').first()).toBeVisible();
  await expect(page.locator('#inspector-main .edit-head')).toHaveText('Edit node');
  await page.keyboard.press('Escape');
  await expect(page.locator('#plot .trace')).toHaveCount(0);
  await expect(page.locator('#plot')).not.toHaveClass(/dimmed/);
});

test('a pinned node is outlined inside its edge, only while it is what the plot shows', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Stable');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  const outline = page.locator('#plot .layer-selection .selection-outline');
  await expect(outline).toHaveCount(1);
  expect(await outline.getAttribute('clip-path')).toBe('url(#selection-clip)');
  // One outline around band and body together, so no dashed line runs through the node.
  expect((await outline.getAttribute('d')).match(/M/g)).toHaveLength(1);
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(outline).toHaveCount(0);
  await page.mouse.move(5, 5);
  await expect(outline).toHaveCount(1);
  await page.keyboard.press('Escape');
  await expect(outline).toHaveCount(0);
});

test('dimmed nodes stay opaque so their necks match their bodies', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  const dimmed = page.locator('#plot .trunk:not(.hl)').first();
  expect(await dimmed.evaluate((el) => getComputedStyle(el).opacity)).toBe('1');
  expect(await dimmed.evaluate((el) => getComputedStyle(el).filter)).toContain('dim-node');
});

test('autosave keeps only non-default style, so later default changes reach restored sessions', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await page.waitForTimeout(600);
  const saved = await page.evaluate(() => JSON.parse(localStorage.getItem('polylines-sankey-v1')).style);
  expect(saved.labelPosition).toBe('above');
  expect('gatherInNodes' in saved).toBe(false);
  // A session saved by an older version, with a since-renamed key, restores cleanly.
  await page.evaluate(() => {
    const s = JSON.parse(localStorage.getItem('polylines-sankey-v1'));
    s.style.gatherTraces = true;
    localStorage.setItem('polylines-sankey-v1', JSON.stringify(s));
  });
  await page.reload();
  const style = await page.evaluate(() => window.SankeyApp.state.style);
  expect(style.gatherInNodes).toBe(false);
  expect('gatherTraces' in style).toBe(false);
  expect(style.labelPosition).toBe('above');
});

test('Alt-click adds and removes substrands from a pool traced and tabulated together', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const altClick = async (label) => {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Alt');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Alt');
  };
  await altClick('Surgery');
  await altClick('Radiation');
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace')).toHaveCount(8);
  await expect(page.locator('#inspector-main h2').first()).toHaveText('Pool');
  await expect(page.locator('#inspector-main')).toContainText('8 pathways · 308 patients');
  await altClick('Surgery');
  await page.mouse.move(5, 5);
  await expect(page.locator('#inspector-main')).toContainText('4 pathways · 76 patients');
  await page.locator('#pool-clear').click();
  await expect(page.locator('#plot .trace')).toHaveCount(0);
});

test('SVG and PNG export in one click and keep the focused view', async ({ page }) => {
  await expect(page.locator('.topbar .actions > [data-export="svg"]')).toBeVisible();
  await expect(page.locator('.topbar .actions > [data-export="png"]')).toBeVisible();
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Remission');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  await page.waitForTimeout(200);
  const svg = await page.evaluate(() => window.SankeyApp.exportSvgText());
  expect(svg).toContain('class="trace"');
  expect(svg).toContain('class="node-fill"');
  expect(svg).toContain('opacity="0.16"');
  expect(svg).toContain('filter="url(#dim-node)"');
  expect(svg).not.toContain('selection-outline');
});

test('nodes and strands fully covered by traces hide their faded copies', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x, node.y);
  const ids = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const n = L.nodes.find((x) => x.label === 'Remission');
    return { node: n.id, flow: n.inFlows[0], other: L.nodes.find((x) => x.label === 'Stable').id };
  });
  const opacity = (sel) => page.locator(sel).first().evaluate((el) => getComputedStyle(el).opacity);
  await expect(page.locator(`#plot .trunk[data-node="${ids.node}"]`).first()).toHaveClass(/covered/);
  await expect.poll(() => opacity(`#plot .trunk[data-node="${ids.node}"]`)).toBe('0');
  await expect.poll(() => opacity(`#plot .ribbon[data-flow="${ids.flow}"]`)).toBe('0');
  await expect(page.locator(`#plot .trunk[data-node="${ids.other}"]`).first()).not.toHaveClass(/covered/);
  await expect(page.locator(`#plot .node-label[data-node="${ids.node}"]`)).not.toHaveClass(/covered/);
});

test('imported tables label nodes above solid bodies in darker node colors', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const s = await state(page);
  expect(s.style.labelPosition).toBe('above');
  expect(s.style.labelColorBy).toBe('node');
  const check = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const n = L.nodes.find((x) => x.label === 'Surgery');
    const label = document.querySelector(`#plot .node-label[data-node="${n.id}"]`);
    const top = document.querySelector('#plot').getBoundingClientRect().height - 18 - (n.Y + Math.max(n.inPx, n.outPx) / 2);
    const body = document.querySelector(`#plot linearGradient[id="gn${n.id}_i"]`);
    return {
      above: Number(label.getAttribute('y')) < top,
      fill: label.getAttribute('fill'),
      clear: body ? [...body.querySelectorAll('stop')].some((st) => st.getAttribute('stop-opacity') === '0') : false
    };
  });
  expect(check.above).toBe(true);
  expect(check.fill).not.toBe('#1F2937');
  expect(check.clear).toBe(false);
});

test('one-row-per-patient data traces single patients and can draw substrand lines', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  // The same patients as the aggregated pathways example, one row each.
  const s = await state(page);
  expect(s.paths.reduce((t, p) => t + p.members.length, 0)).toBe(560);
  expect(s.matrices).toEqual(await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    const m = window.SankeyApp.state.matrices;
    window.SankeyApp.loadExample('cohort');
    return m;
  }));
  expect(s.pathUnit).toBe('patients');
  await expect(page.locator('#plot .substrand-lines')).toHaveCount(0);
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  expect(await page.locator('#plot .trace').count()).toBeGreaterThan(1);
  // Ctrl traces the pathway (a unique trajectory) under the pointer, Shift the single patient.
  await page.keyboard.down('Control');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText('One pathway');
  await page.keyboard.up('Control');
  await page.keyboard.down('Shift');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText(/^P\d{3}$/);
  await expect(page.locator('#hover-info')).toContainText(/patients? on this pathway/);
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x - 10, node.y);
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText(/^P\d{3}$/);
  await page.keyboard.up('Shift');
  await page.evaluate(() => { window.SankeyApp.state.style.substrands = true; window.SankeyApp.update(); });
  await expect(page.locator('#plot .substrand-lines').first()).toBeAttached();
  const exported = await page.evaluate(() => window.SankeyApp.exportSvgText());
  expect(exported).toContain('substrand-lines');
});

test('pathway tracing turns off with a warning once flow values no longer match the data', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    window.SankeyApp.state.matrices[0][0][0] += 1;
    window.SankeyApp.update();
  });
  await expect(page.locator('#warnings')).toContainText('pathway tracing is off');
  const exported = await page.evaluate(() => window.SankeyApp.exportSvgText());
  expect(exported).not.toContain('layer-traces');
});

test('works when opened directly from disk', async ({ browser }) => {
  const page = await browser.newPage();
  await page.goto(`file://${path.resolve(__dirname, '..', 'index.html')}`);
  await expect(page.locator('#plot .ribbon').first()).toBeVisible();
  await page.close();
});
