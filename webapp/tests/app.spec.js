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

test('hovering a strand shows its details and dims the others', async ({ page }) => {
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#tooltip')).toBeVisible();
  await expect(page.locator('#tooltip')).toContainText('→');
  await expect(page.locator('#tooltip')).toContainText('of source outflow');
  await expect(page.locator('#plot')).toHaveClass(/dimmed/);
  expect(await page.locator('#plot .ribbon.hl').count()).toBe(1);
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
  await expect(page.locator('#inspector h2')).toHaveText('Strand');
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
  await expect(page.locator('#tooltip')).toBeVisible();
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
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#tooltip')).toContainText('value 120');
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

test('works when opened directly from disk', async ({ browser }) => {
  const page = await browser.newPage();
  await page.goto(`file://${path.resolve(__dirname, '..', 'index.html')}`);
  await expect(page.locator('#plot .ribbon').first()).toBeVisible();
  await page.close();
});
