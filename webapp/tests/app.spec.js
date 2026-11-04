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

const pathwayPoint = (page, weight = 41) => page.evaluate((w) => {
  const app = window.SankeyApp;
  const L = app.layout;
  const pi = app.state.paths.findIndex((p) => p.weight === w);
  const subs = window.SK.paths.substrands(L, app.state.paths);
  const f = L.flows[subs.pathFlows[pi][0]];
  const part = subs.byFlow[f.index].find((e) => e.path === pi);
  const X = f.startX + 0.45 * (f.endX - f.startX);
  const lo = window.SK.layout.pointAtX(f.lower, X)[1];
  const hi = window.SK.layout.pointAtX(f.upper, X)[1];
  const Y = lo + (hi - lo) * (part.a + part.b) / 2;
  const box = document.querySelector('#plot').getBoundingClientRect();
  return { x: box.left + 18 + X, y: box.top + box.height - 18 - Y, path: pi, flow: f.index };
}, weight);

const importCohortTable = async (page) => {
  await page.locator('.topbar [data-open-import]').click();
  await page.locator('#import-text').fill('patient_id,sex,TR00,TR01,TR02,n\nP1,Female,A,X,Z,8\nP2,Male,A,Y,Z,4\nP3,Female,B,Y,W,6\nP4,Male,B,X,W,2\nP5,Female,A,Y,Z,3\n');
  await expect(page.locator('#import-format')).toHaveValue('wide');
  await page.locator('#import-mapping [data-col="sex"]').uncheck();
  await page.locator('#import-labels').selectOption('above');
  await page.locator('#import-apply').click();
};

const keepOnly = async (page, column, value) => {
  const details = page.locator(`[data-filter-column="${column}"]`);
  if (!(await details.evaluate((el) => el.open))) await details.locator('summary').click();
  await details.locator(`[data-only-value="${value}"]`).click();
};

// The arrange strip above the plot: a column's badge, and points to drag badges to, in the row of
// shown columns ('in') or of those not shown ('out').
const badge = (page, column) => page.locator(`#column-strip .strip-badge[data-strip-column="${column}"]`);
const stripRowY = async (page, row) => {
  const box = await page.locator(`#column-strip .strip-${row}`).boundingBox();
  return row === 'out' ? box.y + box.height / 2 : box.y + 14;
};
const leftOfBadge = async (page, column, row = 'in') => ({ x: (await badge(page, column).boundingBox()).x - 6, y: await stripRowY(page, row) });
// The middle of the white gap between two shown columns' zones.
const betweenBadges = async (page, a, b, row) => {
  const zone = (c) => page.locator(`#column-strip .strip-zone[data-strip-zone="${c}"]`).boundingBox();
  const [p, q] = [await zone(a), await zone(b)];
  return { x: (p.x + p.width + q.x) / 2, y: await stripRowY(page, row) };
};
const holdBadge = async (page, column, to) => {
  const from = await badge(page, column).boundingBox();
  await page.mouse.move(from.x + from.width / 2, from.y + from.height / 2);
  await page.mouse.down();
  await page.mouse.move(to.x, to.y, { steps: 6 });
};
const dragBadge = async (page, column, to) => {
  await holdBadge(page, column, to);
  await page.mouse.up();
};

test('renders the default example without errors', async ({ page }) => {
  await expect(page.locator('#plot .ribbon')).toHaveCount(14);
  await expect(page.locator('#plot .node-label')).toHaveCount(9);
  await expect(page.locator('#ctl-labelCounts')).toBeChecked();
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

for (const invalidRole of ['colors', 'mids', 'loc']) {
  test(`a rejected ${invalidRole} file leaves the data, drawing and exports unchanged`, async ({ page }) => {
    const p = await ribbonCenter(page, 0);
    await page.mouse.click(p.x, p.y);
    await page.mouse.move(5, 5);
    const before = await state(page);
    const bundleBefore = await page.evaluate(() => window.SankeyApp.bundleFiles());
    const drawingBefore = await page.locator('#plot').innerHTML();
    const files = before.matrices.map((m, i) => ({
      name: `stage_${i + 1}.csv`, mimeType: 'text/csv',
      buffer: Buffer.from(m.map((row) => row.map((v) => v * 2).join(',')).join('\n'))
    }));
    const invalid = invalidRole === 'loc'
      ? { name: 'loc.csv', text: ['set,node,x,y,label', ...before.loc.map((r) => [r.set, r.node, 0, r.y, r.label].join(','))].join('\n') }
      : { name: `${invalidRole === 'mids' ? 'mid_colors' : 'colors'}_1.csv`, text: '#FF0000\n' };
    await page.setInputFiles('#file-input', files.concat([{ name: invalid.name, mimeType: 'text/csv', buffer: Buffer.from(invalid.text) }]));
    await expect(page.locator('#banner')).toBeVisible();
    await expect(page.locator('#banner')).toContainText(invalidRole === 'loc' ? 'horizontal room' : 'matrix is 1×1');
    expect(await state(page)).toEqual(before);
    expect(await page.locator('#plot').innerHTML()).toBe(drawingBefore);
    expect(await page.evaluate(() => window.SankeyApp.bundleFiles())).toEqual(bundleBefore);
    await expect(page.locator('#inspector-main .edit-head')).toHaveText('Edit strand');

    await page.setInputFiles('#file-input', files);
    await expect(page.locator('#banner')).toBeHidden();
    expect(await page.evaluate(() => window.SankeyApp.layout.flows[0].value)).toBe(before.matrices[0][0][0] * 2);
    await page.keyboard.press('Control+z');
    expect(await state(page)).toEqual(before);
  });
}

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

test('Data controls rebuild weighted flows from column order, visibility and row filters', async ({ page }) => {
  await importCohortTable(page);
  await expect(page.locator('#table-controls')).toBeVisible();
  expect((await state(page)).sourceTable.table.rows).toHaveLength(5);
  const scale = (await state(page)).valueScale;
  await keepOnly(page, 'sex', 'Female');
  let s = await state(page);
  expect(s.sourceTable.summary.rowsFiltered).toBe(3);
  expect(s.sourceTable.summary.total).toBe(17);
  expect(s.valueScale).not.toBe(scale);
  expect(s.paths.flatMap((p) => p.members.map((m) => m.id)).sort()).toEqual(['P1', 'P3', 'P5']);
  await expect(page.locator('#plot .layer-subset')).toHaveText('sex ∈ {Female}');
  await expect(page.locator('#plot .layer-subset [font-weight="600"]')).toHaveText('sex');
  await badge(page, 'TR01').click();
  await dragBadge(page, 'TR02', await leftOfBadge(page, 'TR00'));
  s = await state(page);
  expect(s.setNames).toEqual(['TR02', 'TR00']);
  expect(s.matrices).toEqual([[[11, 0], [0, 6]]]);
  expect(s.paths).toHaveLength(2);
  expect(s.sourceTable.table.rows).toHaveLength(5);
  await expect(badge(page, 'TR02')).toHaveClass(/locked/);
  await expect(badge(page, 'TR00')).toHaveClass(/locked/);
  await expect(badge(page, 'TR01')).toHaveAttribute('aria-pressed', 'false');
  await badge(page, 'TR00').click();
  await expect(page.locator('#notice')).toHaveText('Keep at least two diagram columns.');
  expect((await state(page)).setNames).toEqual(['TR02', 'TR00']);
  await expect(page.locator('#banner')).toBeHidden();
  const captionAbove = await page.evaluate(() => {
    const caption = document.querySelector('#plot .layer-subset').getBBox();
    return caption.y + caption.height < Math.min(...[...document.querySelectorAll('#plot .ribbon')].map((el) => el.getBBox().y));
  });
  expect(captionAbove).toBe(true);
  const node = await nodeAt(page, 'Z');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveText('11');
  const files = await page.evaluate(() => window.SankeyApp.bundleFiles());
  expect(files.find((f) => f.name === 'sankey.R').text).toContain('expression(paste(bold("sex")');
  const project = JSON.parse(files.find((f) => f.name === 'project.json').text);
  expect(project.sourceTable.filters).toEqual({ sex: ['Female'] });
  expect(project.matrices).toEqual([[[11, 0], [0, 6]]]);
  expect(await page.evaluate(() => window.SankeyApp.exportSvgText())).toContain('class="layer-subset"');
  await badge(page, 'TR01').click();
  expect((await state(page)).setNames).toEqual(['TR02', 'TR00', 'TR01']);
  await page.locator('[data-clear-filters]').click();
  expect((await state(page)).sourceTable.summary.total).toBe(23);
  await expect(page.locator('#plot .layer-subset')).toHaveCount(0);
});

test('multiple row filters intersect, preserve the original table and reject empty subsets atomically', async ({ page }) => {
  await importCohortTable(page);
  await keepOnly(page, 'sex', 'Female');
  await keepOnly(page, 'TR00', 'B');
  await page.mouse.move(5, 5);
  const before = await state(page);
  expect(before.sourceTable.summary.total).toBe(6);
  const plot = await page.locator('#plot').innerHTML();
  const exports = await page.evaluate(() => window.SankeyApp.bundleFiles());
  await keepOnly(page, 'TR02', 'Z');
  await expect(page.locator('#banner')).toContainText('No rows match these filters');
  expect(await state(page)).toEqual(before);
  expect(await page.locator('#plot').innerHTML()).toBe(plot);
  expect(await page.evaluate(() => window.SankeyApp.bundleFiles())).toEqual(exports);
  await expect(page.locator('#plot .layer-subset')).toHaveText('sex ∈ {Female}   ·   TR00 ∈ {B}');
  await page.locator('[data-filter-column="TR00"] [data-keep-all]').click();
  await expect(page.locator('#banner')).toBeHidden();
  expect((await state(page)).sourceTable.summary.total).toBe(17);
  await keepOnly(page, 'sex', 'Male');
  expect((await state(page)).sourceTable.summary.total).toBe(6);
  expect((await state(page)).sourceTable.table.rows).toHaveLength(5);
});

test('true long tables can reorder and skip stages before checking missing visits', async ({ page }) => {
  await page.locator('.topbar [data-open-import]').click();
  await page.locator('#import-text').fill('id,visit,status,sex\nA,1,A,Female\nA,2,B,Female\nA,3,C,Female\nB,1,A,Male\nB,2,B,Male\nB,3,D,Male\nC,1,B,Female\nC,3,C,Female\n');
  await expect(page.locator('#import-format')).toHaveValue('long');
  await page.locator('#import-apply').click();
  await keepOnly(page, 'sex', 'Female');
  expect((await state(page)).sourceTable.summary.total).toBe(1);
  await badge(page, '2').click();
  await dragBadge(page, '3', await leftOfBadge(page, '1'));
  const s = await state(page);
  expect(s.setNames).toEqual(['visit 3', 'visit 1']);
  expect(s.sourceTable.summary.total).toBe(2);
  expect(s.sourceTable.summary.rowsFiltered).toBe(5);
  expect(s.matrices).toEqual([[[1, 1]]]);
  expect(s.paths.flatMap((p) => p.members.map((m) => m.id))).toEqual(['A', 'C']);
  await expect(page.locator('#plot .layer-subset')).toHaveText('sex ∈ {Female}');
  await expect(page.locator('#banner')).toBeHidden();
});

test('table column choices and row filters survive history, autosave and project reloads', async ({ page }, testInfo) => {
  await importCohortTable(page);
  await badge(page, 'TR01').click();
  const unfiltered = await state(page);
  await keepOnly(page, 'sex', 'Female');
  const filtered = await state(page);
  await page.keyboard.press('Control+z');
  expect(await state(page)).toEqual(unfiltered);
  await expect(page.locator('#plot .layer-subset')).toHaveCount(0);
  await page.keyboard.press('Control+Shift+z');
  expect(await state(page)).toEqual(filtered);
  await expect.poll(() => page.evaluate(() => JSON.parse(localStorage.getItem('polylines-sankey-v1'))?.sourceTable?.filters)).toEqual({ sex: ['Female'] });
  const project = (await page.evaluate(() => window.SankeyApp.bundleFiles())).find((f) => f.name === 'project.json').text;
  await page.reload();
  await expect(badge(page, 'TR01')).toHaveAttribute('aria-pressed', 'false');
  await expect(page.locator('#plot .layer-subset')).toHaveText('sex ∈ {Female}');
  await page.evaluate(() => window.SankeyApp.loadExample('labeled'));
  const filename = testInfo.outputPath('filtered-project.json');
  fs.writeFileSync(filename, project);
  await page.setInputFiles('#file-input', filename);
  await expect(page.locator('#table-controls')).toBeVisible();
  await expect(page.locator('#plot .layer-subset')).toHaveText('sex ∈ {Female}');
  await keepOnly(page, 'sex', 'Male');
  expect((await state(page)).sourceTable.summary.total).toBe(6);
});

test('table diagrams grow their automatic canvas for many columns and categories', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const cols = Array.from({ length: 10 }, (_, i) => `TR${String(i).padStart(2, '0')}`);
    const rows = Array.from({ length: 15 }, (_, i) => `P${i},${cols.map(() => `Category ${i}`).join(',')}`);
    const table = T.parseTable(`patient_id,${cols.join(',')}\n${rows.join('\n')}\n`);
    window.SankeyApp.applyImport(T.build(table, { format: 'wide', id: 'patient_id', columns: cols }), { labels: 'above' });
  });
  const dimensions = await page.evaluate(() => {
    const plot = document.querySelector('#plot');
    const stage = document.querySelector('#stage').getBoundingClientRect();
    return { width: Number(plot.getAttribute('width')), height: Number(plot.getAttribute('height')), stageWidth: stage.width, stageHeight: stage.height };
  });
  expect(dimensions.width).toBeGreaterThan(dimensions.stageWidth);
  expect(dimensions.height).toBeGreaterThan(dimensions.stageHeight);
  const overlap = await page.locator('#plot .node-label').evaluateAll((els) => {
    const boxes = els.map((el) => el.getBoundingClientRect());
    return boxes.some((a, i) => boxes.slice(i + 1).some((b) => a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top));
  });
  expect(overlap).toBe(false);
  await expect(page.locator('#banner')).toBeHidden();
});

test('automatic table layouts add room for constant-width turns and preserve it on reload', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const table = T.parseTable('source,middle,target,n\nA,Middle,Low,10\nB,Middle,High,10\n');
    window.SankeyApp.applyImport(T.build(table, { format: 'wide', columns: ['source', 'middle', 'target'], weight: 'n' }), { labels: 'above' });
  });
  const snapshot = () => page.evaluate(() => {
    const app = window.SankeyApp;
    return { width: Number(document.querySelector('#plot').getAttribute('width')), minWidth: app.state.sourceTable.minCanvasWidth,
      loc: app.state.loc, widths: app.layout.flows.map(f => f.w) };
  });
  const before = await snapshot();
  expect(before.minWidth).toBeGreaterThan(0);
  expect(before.width).toBeGreaterThan(await page.locator('#stage').evaluate(el => el.clientWidth));
  await page.evaluate(() => window.SankeyApp.update());
  expect(await snapshot()).toEqual(before);
  await page.waitForTimeout(700);
  await page.reload();
  expect(await snapshot()).toEqual(before);
  const widthError = await page.evaluate(() => Math.max(...window.SankeyApp.layout.flows.flatMap(f => f.centerline.slice(1, -1).map((_, j) => {
    const i = j + 1, a = f.centerline[i - 1], b = f.centerline[i + 1], dx = b[0] - a[0], dy = b[1] - a[1];
    return Math.abs(Math.abs((f.upper[i][0] - f.lower[i][0]) * -dy + (f.upper[i][1] - f.lower[i][1]) * dx) / Math.hypot(dx, dy) / f.w - 1);
  }))));
  expect(widthError).toBeLessThan(0.001);
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
  const app = window.SankeyApp;
  const n = app.layout.nodes.find((x) => app.state.loc[x.id].label === l || x.label === l);
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
    const n = L.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Outcome 1');
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
    const f = L.flows.find((x) => x.stage === 0 && window.SankeyApp.state.loc[x.from].label === 'Stage II' && window.SankeyApp.state.loc[x.to].label === 'Chemotherapy');
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
    const n = window.SankeyApp.layout.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Remission');
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
    const f = L.flows.find((x) => x.stage === 0 && window.SankeyApp.state.loc[x.from].label === 'Stage II' && window.SankeyApp.state.loc[x.to].label === 'Chemotherapy');
    const c = f.centerline[Math.floor(f.centerline.length / 2)];
    const box = document.querySelector('#plot').getBoundingClientRect();
    return { x: box.left + 18 + c[0], y: box.top + (box.height - 18) - c[1], from: f.from };
  });
  await page.mouse.move(point.x, point.y);
  await expect(page.locator('#plot .node-fill')).toHaveCount(11);
  await expect(page.locator('#plot .node-fill-solid')).toHaveCount(4);
  const missed = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Stage I');
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

test('node share bars shrink text to reserve a visible bar inside narrow nodes', async ({ page }) => {
  await page.evaluate(() => {
    const app = window.SankeyApp;
    const scale = app.layout.tf.sx;
    app.state.loc[0].width = 120 / scale;
    app.state.loc[1].width = 200 / scale;
    app.state.loc[2].width = 90 / scale;
    app.state.loc[3].width = 60 / scale;
    app.state.loc[4].width = 30 / scale;
    app.update();
  });
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  const sizes = await page.locator('#plot .node-fill').evaluateAll((els) => els.slice(0, 5).map((el) => {
    const box = el.getBBox();
    const n = window.SankeyApp.layout.nodes[Number(el.dataset.fill)];
    const background = el.querySelector('g rect');
    const solid = el.querySelector('.node-fill-solid');
    return { nodeWidth: n.widthPx, barWidth: Number(background.getAttribute('width')), wholeWidth: box.width,
      textSizes: [...el.querySelectorAll('text')].map((t) => Number(t.getAttribute('font-size'))),
      ratio: solid ? Number(solid.getAttribute('width')) / Number(background.getAttribute('width')) : 0 };
  }));
  expect(sizes.map((s) => s.barWidth)).toEqual([40, 60, 24, 20, 10]);
  expect(sizes.every((s) => s.wholeWidth <= s.nodeWidth)).toBe(true);
  [10.5, 10.5, 8.6625, 5.25, 2.625].forEach((font, i) => {
    sizes[i].textSizes.forEach(size => expect(size).toBeCloseTo(font, 4));
  });
  const fraction = await page.evaluate(() => {
    const f = window.SankeyApp.layout.flows[0];
    return f.value / window.SankeyApp.layout.nodes[f.from].outValue;
  });
  expect(sizes[0].ratio).toBeCloseTo(fraction, 2);
});

test('node share bars sit just beneath the local body instead of distant ribbon faces', async ({ page }) => {
  await page.evaluate(() => {
    const app = window.SankeyApp, tf = app.layout.tf;
    const row = app.state.loc.find(n => n.set === 2 && n.node === 1);
    row.width = 300 / tf.sx;
    row.height = 10 / tf.sy;
    app.update();
  });
  const p = await ribbonCenter(page, 0);
  await page.mouse.move(p.x, p.y);
  const gap = await page.evaluate(() => {
    const app = window.SankeyApp, n = app.layout.nodes.find(n => n.set === 2 && n.node === 1);
    const shape = SK.shapes.interiorTrunk(app.layout, n);
    const bodyBottom = Number(document.querySelector('#plot').getAttribute('height')) - 18 - SK.layout.pointAtX(shape.lower, n.X)[1];
    const rect = document.querySelector(`[data-fill="${n.id}"] g rect`);
    return Number(rect.getAttribute('y')) - bodyBottom;
  });
  expect(gap).toBeCloseTo(7, 1);
});

test('hovered substrands show endpoint counts and their share of each edge along its direction', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await expect(page.locator('#ctl-traceCounts')).toBeChecked();
  await expect(page.locator('#ctl-tracePercents')).not.toBeChecked();
  await page.locator('#ctl-tracePercents').check();
  const p = await pathwayPoint(page);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .trace-count')).toHaveCount(2);
  await expect(page.locator('#plot .trace-percent')).toHaveCount(4);
  await expect(page.locator(`#plot .trace-count[data-path="${p.path}"]`)).toHaveText('41');
  await expect(page.locator(`#plot .trace-percent[data-path="${p.path}"][data-label-flow="${p.flow}"]`)).toHaveText('65.1%');
  // The same 41 patients fill 41/69 of the later Chemotherapy → Remission edge.
  const shares = await page.locator(`#plot .trace-percent[data-path="${p.path}"]`).allTextContents();
  expect(shares).toEqual(['65.1%', '59.4%']);
  const geometry = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    return [...document.querySelectorAll('#plot .trace-count, #plot .trace-percent')].map((el) => {
      const box = el.getBoundingClientRect();
      const svg = document.querySelector('#plot').getBoundingClientRect();
      const n = L.nodes[Number(el.dataset.labelNode)];
      const matrix = el.transform.baseVal.consolidate()?.matrix;
      return { right: box.right <= svg.right, bottom: box.bottom <= svg.bottom,
        countRight: n ? Number(el.getAttribute('x')) > 18 + n.X + n.widthPx / 2 : true,
        white: !matrix || el.getAttribute('fill') === '#FFFFFF',
        angle: matrix ? Math.atan2(matrix.b, matrix.a) : null };
    });
  });
  expect(geometry.every((g) => g.right && g.bottom && g.countRight && g.white)).toBe(true);
  expect(geometry.some((g) => g.angle !== null && Math.abs(g.angle) > 0.2)).toBe(true);
  await page.keyboard.down('Control');
  await expect(page.locator('#plot .trace-count')).toHaveCount(1);
  await expect(page.locator('#plot .trace-percent')).toHaveCount(2);
  await expect(page.locator('#plot .trace-count')).toHaveText('41');
  await page.mouse.click(p.x, p.y);
  await page.keyboard.up('Control');
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveText('41');
  const exported = await page.evaluate(() => window.SankeyApp.exportSvgText());
  expect(exported).toContain('class="trace-count"');
  expect(exported).toContain('class="trace-percent"');
  expect(exported).toContain('59.4%');
  await page.keyboard.press('Escape');
  await expect(page.locator('#plot .trace-count, #plot .trace-percent')).toHaveCount(0);
  expect(await page.evaluate(() => window.SankeyApp.exportSvgText())).not.toContain('layer-trace-labels');
});

test('Alt-hover annotations use the patient slice count, and pools retain that slice', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  await page.locator('#ctl-tracePercents').check();
  const p = await pathwayPoint(page);
  await page.keyboard.down('Alt');
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .trace-count')).toHaveText('1');
  expect(await page.locator('#plot .trace-percent').allTextContents()).toEqual(['1.6%', '1.4%']);
  const member = await page.locator('#plot .trace-count').getAttribute('data-member');
  expect(Number(member)).toBeGreaterThanOrEqual(0);
  await page.keyboard.down('Shift');
  await page.mouse.click(p.x, p.y);
  await page.keyboard.up('Shift');
  await page.keyboard.up('Alt');
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveText('1');
  await expect(page.locator('#plot .trace-count')).toHaveAttribute('data-member', member);
  expect(await page.locator('#plot .trace-percent').allTextContents()).toEqual(['1.6%', '1.4%']);
});

test('substrand label toggles work independently and persist across reloads', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await page.locator('#ctl-tracePercents').check();
  const n = await nodeAt(page, 'Remission');
  await page.mouse.click(n.x, n.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveCount(5);
  await expect(page.locator('#plot .trace-percent')).toHaveCount(10);
  const overlapping = await page.locator('#plot .trace-percent').evaluateAll((els) => {
    const boxes = els.map((el) => el.getBoundingClientRect());
    return boxes.some((a, i) => boxes.slice(i + 1).some((b) => a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top));
  });
  expect(overlapping).toBe(false);
  await page.locator('#ctl-traceCounts').uncheck();
  await expect(page.locator('#plot .trace-count')).toHaveCount(0);
  await expect(page.locator('#plot .trace-percent')).toHaveCount(10);
  await page.locator('#ctl-traceCounts').check();
  await page.locator('#ctl-tracePercents').uncheck();
  await expect(page.locator('#plot .trace-count')).toHaveCount(5);
  await expect(page.locator('#plot .trace-percent')).toHaveCount(0);
  await page.locator('#ctl-traceCounts').uncheck();
  expect(await page.evaluate(() => window.SankeyApp.exportSvgText())).not.toContain('layer-trace-labels');
  const project = JSON.parse((await page.evaluate(() => window.SankeyApp.bundleFiles())).find((f) => f.name === 'project.json').text);
  expect(project.style.traceCounts).toBe(false);
  expect(project.style.tracePercents).toBe(false);
  await expect.poll(() => page.evaluate(() => {
    const style = JSON.parse(localStorage.getItem('polylines-sankey-v1'))?.style;
    return style && [style.traceCounts, Object.hasOwn(style, 'tracePercents')];
  })).toEqual([false, false]);
  await page.reload();
  await expect(page.locator('#ctl-traceCounts')).not.toBeChecked();
  await expect(page.locator('#ctl-tracePercents')).not.toBeChecked();
  const p = await pathwayPoint(page);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .trace')).toHaveCount(2);
  await expect(page.locator('#plot .trace-count, #plot .trace-percent')).toHaveCount(0);
});

test('crowded endpoint counts spread apart and follow gathered traces', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    window.SankeyApp.state.valueScale = 0.00001;
    window.SankeyApp.update();
  });
  const n = await nodeAt(page, 'Remission');
  await page.mouse.click(n.x, n.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveCount(5);
  expect((await page.locator('#plot .trace-count').allTextContents()).sort()).toEqual(['120', '28', '35', '41', '64']);
  expect(await page.locator('#plot .trace-count-leader').count()).toBeGreaterThan(0);
  const crowded = async () => page.locator('#plot .trace-count').evaluateAll((els) => {
    const canvas = document.querySelector('#plot').getBoundingClientRect();
    const boxes = els.map((el) => el.getBoundingClientRect()).sort((a, b) => a.top - b.top);
    return boxes.some((a, i) => a.bottom > canvas.bottom || a.top < canvas.top || (i > 0 && a.top < boxes[i - 1].bottom));
  });
  expect(await crowded()).toBe(false);
  await page.locator('#ctl-gatherInNodes').check();
  await expect(page.locator('#plot .trace-count')).toHaveCount(5);
  expect(await crowded()).toBe(false);
  const distance = await page.locator('#plot .trace-count').evaluateAll((els) => els.map((el) => {
    const path = document.querySelector(`#plot .trace[data-path="${el.dataset.path}"]`);
    const box = path.getBBox();
    return Number(el.getAttribute('x')) - (box.x + box.width);
  }));
  distance.forEach((d) => expect(d).toBeCloseTo(48, 1));
});

test('many endpoint counts fan farther out, clear node labels and stay inside exports', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform, app = window.SankeyApp;
    const rows = ['A', 'B', 'C', 'D'].flatMap((source, i) => Array.from({ length: 6 }, (_, j) =>
      [`${source},TR0${j},Low-risk only,${i * 6 + j + 1}`, `${source},TR0${j},Open/all data,${j + 1}`])).flat();
    const table = T.parseTable('source,tranche,consent,n\n' + rows.join('\n'));
    app.applyImport(T.build(table, { format: 'wide', columns: ['source', 'tranche', 'consent'], weight: 'n' }), { labels: 'above' });
    Object.assign(app.state.style, { canvasWidth: 1000, canvasHeight: 700, fontSize: 18 });
    app.state.valueScale = 0.00001;
    app.update({ fit: true });
  });
  const node = await nodeAt(page, 'Low-risk only');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveCount(24);
  const result = await page.evaluate(() => {
    const plot = document.querySelector('#plot');
    const counts = [...plot.querySelectorAll('.trace-count')].map(el => el.getBBox());
    const labels = [...plot.querySelectorAll('.node-label')].map(el => el.getBBox());
    const endpoint = Math.max(...[...plot.querySelectorAll('.trace')].map(el => { const b = el.getBBox(); return b.x + b.width; }));
    return { gap: Math.min(...counts.map(b => b.x)) - endpoint,
      overlap: counts.some(a => labels.some(b => a.x < b.x + b.width && a.x + a.width > b.x && a.y < b.y + b.height && a.y + a.height > b.y)),
      clipped: counts.some(b => b.x + b.width > plot.width.baseVal.value || b.y < 0 || b.y + b.height > plot.height.baseVal.value),
      svg: window.SankeyApp.exportSvgText() };
  });
  expect(result.gap).toBeGreaterThan(75);
  expect(result.overlap).toBe(false);
  expect(result.clipped).toBe(false);
  expect(result.svg).toContain('mask="url(#trace-count-label-mask)"');
  expect(result.svg).toContain('class="trace-count"');
  // Let the app's own autosave (400 ms after the last redraw) run first, or it could overwrite the
  // older session written below before the reload.
  await page.waitForTimeout(700);
  const oldView = await page.evaluate(() => {
    const saved = JSON.parse(JSON.stringify(window.SankeyApp.state));
    delete saved.traceCountsVersion;
    saved.style.labelCounts = false;
    saved.view.xlim[1] -= 0.3 * (saved.view.xlim[1] - saved.view.xlim[0]);
    localStorage.setItem('polylines-sankey-v1', JSON.stringify(saved));
    return saved.view;
  });
  await page.reload();
  await expect(page.locator('#ctl-labelCounts')).not.toBeChecked();
  expect((await state(page)).view).not.toEqual(oldView);
  const restoredNode = await nodeAt(page, 'Low-risk only');
  await page.mouse.click(restoredNode.x, restoredNode.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-count')).toHaveCount(24);
  const restoredClipping = await page.locator('#plot .trace-count').evaluateAll(els => {
    const width = document.querySelector('#plot').width.baseVal.value;
    return els.some(el => { const b = el.getBBox(); return b.x + b.width > width; });
  });
  expect(restoredClipping).toBe(false);
});

test('older saved sessions enable counts, default percentages off and fit space for counts', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    const app = window.SankeyApp;
    delete app.state.labelCountsVersion;
    delete app.state.style.traceCounts;
    delete app.state.style.tracePercents;
    app.update({ fit: true });
    localStorage.setItem('polylines-sankey-v1', JSON.stringify(app.state));
  });
  await page.reload();
  await expect(page.locator('#ctl-traceCounts')).toBeChecked();
  await expect(page.locator('#ctl-tracePercents')).not.toBeChecked();
  const p = await pathwayPoint(page);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .trace-count')).toHaveCount(2);
  const clipped = await page.locator('#plot .trace-count').evaluateAll((els) => {
    const right = document.querySelector('#plot').getBoundingClientRect().right;
    return els.some((el) => el.getBoundingClientRect().right > right);
  });
  expect(clipped).toBe(false);
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

test('pinning a node draws no outline around it', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Stable');
  await page.mouse.click(node.x, node.y);
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace').first()).toBeAttached();
  await expect(page.locator('#plot [stroke-dasharray]')).toHaveCount(0);
  const dashed = await page.locator('#plot .trunk, #plot .ribbon').evaluateAll((els) => els.filter((el) => getComputedStyle(el).strokeDasharray !== 'none').length);
  expect(dashed).toBe(0);
});

test('thin plot outlines default on, hide during focus and persist in exports and settings', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  const toggle = page.locator('#ctl-thinOutline');
  const outlines = page.locator('#plot .layer-outline');
  await expect(toggle).toBeChecked();
  await expect(outlines.first()).toBeVisible();
  await expect(outlines.locator('path').first()).toHaveAttribute('stroke', '#000000');
  await page.locator('#ctl-substrands').check();
  const widths = await page.locator('#plot .plot-outline, #plot .substrand-lines').evaluateAll((els) => [...new Set(els.map(el => Number(el.getAttribute('stroke-width')) / ((el.getAttribute('mask') || '').includes('node-edge-mask') ? 2 : 1)))]);
  expect(widths).toEqual([0.6]);
  expect(await page.evaluate(() => window.SankeyApp.exportSvgText())).toContain('class="layer-outline"');
  const n = await nodeAt(page, 'Remission');
  await page.mouse.move(n.x, n.y);
  expect(await outlines.evaluateAll((els) => [...new Set(els.map(el => getComputedStyle(el).opacity))])).toEqual(['0']);
  await page.mouse.click(n.x, n.y);
  await page.mouse.move(5, 5);
  expect(await outlines.evaluateAll((els) => [...new Set(els.map(el => getComputedStyle(el).opacity))])).toEqual(['0']);
  const exported = await page.evaluate(() => {
    const doc = new DOMParser().parseFromString(window.SankeyApp.exportSvgText(), 'image/svg+xml');
    return [...new Set([...doc.querySelectorAll('.layer-outline')].map(el => el.getAttribute('opacity')))];
  });
  expect(exported).toEqual(['0']);
  await page.keyboard.press('Escape');
  expect(await outlines.evaluateAll((els) => [...new Set(els.map(el => getComputedStyle(el).opacity))])).toEqual(['1']);
  await toggle.uncheck();
  await expect(outlines).toHaveCount(0);
  await expect.poll(() => page.evaluate(() => JSON.parse(localStorage.getItem('polylines-sankey-v1'))?.style.thinOutline)).toBe(false);
  await page.reload();
  await expect(toggle).not.toBeChecked();
  await expect(outlines).toHaveCount(0);
  expect(await page.evaluate(() => window.SankeyApp.exportSvgText())).not.toContain('class="layer-outline"');
});

test('a covered strand outline stays hidden beneath an opaque node polygon', async ({ page }) => {
  await page.mouse.move(5, 5);
  const pixels = await page.evaluate(async () => {
    const app = window.SankeyApp;
    Object.assign(app.state, { mode: 'graph', matrices: [[[0, 0, 0, 0.08], [0, 0, 0.35, 0], [0, 0, 0, 0], [0, 0, 0, 0]]],
      loc: [{ set: 1, node: 1, x: 0, y: 0.5, width: 0.2 }, { set: 1, node: 2, x: 1.5, y: 0.5, width: 0.4 },
        { set: 1, node: 3, x: 2.5, y: 0.5, width: 0.2 }, { set: 1, node: 4, x: 3.5, y: 0.5, width: 0.2 }],
      paths: null, sourceTable: null, valueScale: 1, dimnames: null, view: { xlim: [-0.4, 3.9], ylim: [0, 1] } });
    Object.assign(app.state.params, { adjust_loc: false });
    Object.assign(app.state.style, { opacity: 1, colorBy: 'single', singleColor: '#EF8E91', gradient: false, nodeBorder: 'none', labels: false });
    app.update();
    const n = app.layout.nodes[1];
    const f = app.layout.flows.find(f => f.from === 0);
    const X = 18 + n.X;
    const Y = Number(document.querySelector('#plot').getAttribute('height')) - 18 - window.SK.layout.pointAtX(f.upper, n.X)[1];
    const pixel = async (thinOutline) => {
      app.state.style.thinOutline = thinOutline;
      app.update();
      const text = app.exportSvgText();
      // Exports crop to the drawing: their viewBox starts at its corner.
      const [vx, vy] = new DOMParser().parseFromString(text, 'image/svg+xml').documentElement.getAttribute('viewBox').split(' ').map(Number);
      const url = URL.createObjectURL(new Blob([text], { type: 'image/svg+xml' }));
      const image = new Image();
      await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
      const canvas = document.createElement('canvas');
      canvas.width = image.width; canvas.height = image.height;
      const ctx = canvas.getContext('2d');
      ctx.drawImage(image, 0, 0);
      URL.revokeObjectURL(url);
      return [...ctx.getImageData(Math.round(X - vx), Math.round(Y - vy), 1, 1).data];
    };
    return { on: await pixel(true), off: await pixel(false) };
  });
  expect(pixels.off).toEqual([239, 142, 145, 255]);
  expect(pixels.on).toEqual(pixels.off);
});

// Pixels of the uncropped export at plot points (y up), with the bottom canvas margin of 18.
const exportPixels = (page, points, styleChanges = {}) => page.evaluate(async ([pts, changes]) => {
  const app = window.SankeyApp;
  Object.assign(app.state.style, { cropExports: false }, changes);
  app.update();
  const url = URL.createObjectURL(new Blob([app.exportSvgText()], { type: 'image/svg+xml' }));
  const image = new Image();
  await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
  const canvas = document.createElement('canvas');
  canvas.width = image.width; canvas.height = image.height;
  const ctx = canvas.getContext('2d');
  ctx.drawImage(image, 0, 0);
  URL.revokeObjectURL(url);
  return pts.map(([x, y]) => [...ctx.getImageData(Math.floor(18 + x), Math.floor(image.height - 18 - y), 1, 1).data]);
}, [points, styleChanges]);

test('without a merge band, strands meet node faces with no light seam', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('cohort');
    Object.assign(window.SankeyApp.state.params, { merge_length: 0 });
    // A fixed canvas keeps faces at fractional pixels, where a seam would show.
    Object.assign(window.SankeyApp.state.style, { canvasWidth: 1000, canvasHeight: 850 });
    window.SankeyApp.update();
  });
  // Points straddling each interior node's in-face, through the middle of its first strand.
  const points = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    return L.nodes.filter((n) => n.inFlows.length && n.outFlows.length && n.widthPx > 0).flatMap((n) => {
      const f = L.flows[n.inFlows[0]];
      const y = (window.SK.layout.pointAtX(f.upper, f.endX - 3)[1] + window.SK.layout.pointAtX(f.lower, f.endX - 3)[1]) / 2;
      return [[f.endX, y], [f.endX + 3, y]];
    });
  });
  const px = await exportPixels(page, points, { thinOutline: false });
  for (let i = 0; i < px.length; i += 2) {
    const face = px[i].slice(0, 3).reduce((a, b) => a + b), body = px[i + 1].slice(0, 3).reduce((a, b) => a + b);
    expect(face - body).toBeLessThan(25);
  }
});

test('where strands cross, only the strand on top keeps its outline', async ({ page }) => {
  const probe = await page.evaluate(() => {
    const app = window.SankeyApp;
    Object.assign(app.state, { mode: 'layered', matrices: [[[0, 0.25], [0.25, 0]]], paths: null, sourceTable: null, valueScale: 1, dimnames: null,
      loc: [{ set: 1, node: 1, x: 0, y: 0.8 }, { set: 1, node: 2, x: 0, y: 0.2 }, { set: 2, node: 1, x: 1, y: 0.8 }, { set: 2, node: 2, x: 1, y: 0.2 }],
      view: { xlim: [-0.2, 1.2], ylim: [-0.1, 1.1] } });
    Object.assign(app.state.params, { adjust_loc: false });
    Object.assign(app.state.style, { opacity: 0.8, colorBy: 'single', singleColor: '#EF8E91', gradient: false, nodeBorder: 'none', labels: false,
      canvasWidth: 900, canvasHeight: 700 });
    app.update();
    const [under, over] = app.layout.flows;
    const at = (points, x) => window.SK.layout.pointAtX(points, x)[1];
    // The point of the lower strand's edges deepest inside the strand drawn over it.
    let best = { depth: -Infinity };
    for (let x = under.startX + 2; x < under.endX - 2; x += 0.5) {
      [under.upper, under.lower].forEach((edge) => {
        const y = at(edge, x), a = at(over.upper, x), b = at(over.lower, x);
        const depth = (y - a) * (y - b) < 0 ? Math.min(Math.abs(y - a), Math.abs(y - b)) : -1;
        if (depth > best.depth) best = { point: [x, y], depth, inside: depth > 0 };
      });
    }
    return best;
  });
  expect(probe.inside).toBe(true);
  expect(probe.depth).toBeGreaterThan(3);
  const [on] = await exportPixels(page, [probe.point], { thinOutline: true });
  const [off] = await exportPixels(page, [probe.point], { thinOutline: false });
  on.slice(0, 3).forEach((v, i) => expect(Math.abs(v - off[i])).toBeLessThan(6));
});

// In the page: rasterize the uncropped export `scale` times and return a reader of RGB sums at plot
// points (y up).
const rasterHelper = `async (scale) => {
  const app = window.SankeyApp;
  const text = app.exportSvgText();
  const image = new Image();
  const url = URL.createObjectURL(new Blob([text], { type: 'image/svg+xml' }));
  await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
  const canvas = document.createElement('canvas');
  canvas.width = image.width * scale; canvas.height = image.height * scale;
  const ctx = canvas.getContext('2d');
  ctx.drawImage(image, 0, 0, canvas.width, canvas.height);
  URL.revokeObjectURL(url);
  return (x, y) => { const d = ctx.getImageData(Math.floor((18 + x) * scale), Math.floor((image.height - 18 - y) * scale), 1, 1).data; return d[0] + d[1] + d[2]; };
}`;

test('a covered strand outline ends exactly at the edge of the strand on top', async ({ page }) => {
  const result = await page.evaluate(async (helper) => {
    const raster = eval(helper);
    const app = window.SankeyApp;
    // A thin strand crossing a wide one.
    Object.assign(app.state, { mode: 'layered', matrices: [[[0, 0, 0.09], [0.015, 0, 0], [0.09, 0, 0]]], paths: null, sourceTable: null,
      valueScale: 1, dimnames: null,
      loc: [{ set: 1, node: 1, x: 0, y: 0.85 }, { set: 1, node: 2, x: 0, y: 0.4 }, { set: 1, node: 3, x: 0, y: 0.15 },
        { set: 2, node: 1, x: 1, y: 0.85 }, { set: 2, node: 2, x: 1, y: 0.5 }, { set: 2, node: 3, x: 1, y: 0.15 }],
      view: { xlim: [-0.2, 1.2], ylim: [-0.1, 1.1] } });
    Object.assign(app.state.params, { adjust_loc: false });
    Object.assign(app.state.style, { opacity: 0.8, colorBy: 'source', gradient: false, nodeBorder: 'none', labels: false,
      canvasWidth: 700, canvasHeight: 600, cropExports: false, thinOutline: true });
    app.update();
    const L = app.layout;
    const thin = L.flows.find((f) => f.value < 0.02), wide = L.flows.find((f) => f.from === 0);
    const [under, over] = thin.index < wide.index ? [thin, wide] : [wide, thin];
    const at = (points, x) => window.SK.layout.pointAtX(points, x)[1];
    const distance = (p, line) => Math.min(...line.slice(1).map((b, i) => {
      const a = line[i], dx = b[0] - a[0], dy = b[1] - a[1];
      const t = Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / (dx * dx + dy * dy || 1)));
      return Math.hypot(p[0] - a[0] - t * dx, p[1] - a[1] - t * dy);
    }));
    // Points along the lower strand's outline (0.3 px outside its edges), 0.8-1 px inside the upper strand.
    const points = [];
    for (let x = under.startX + 2; x < under.endX - 2; x += 0.05) {
      [[under.upper, 0.3], [under.lower, -0.3]].forEach(([edge, off]) => {
        const p = [x, at(edge, x) + off], a = at(over.upper, x), b = at(over.lower, x);
        if ((p[1] - a) * (p[1] - b) >= 0) return;
        const d = Math.min(distance(p, over.upper), distance(p, over.lower));
        if (d > 0.8 && d < 1) points.push(p);
      });
    }
    const on = await raster(8);
    const withLines = points.map((p) => on(...p));
    app.state.style.thinOutline = false;
    app.update();
    const off = await raster(8);
    return { count: points.length, worst: Math.max(...points.map((p, i) => off(...p) - withLines[i])) };
  }, rasterHelper);
  expect(result.count).toBeGreaterThan(2);
  expect(result.worst).toBeLessThan(30);
});

for (const mergeLength of [0, null]) {
  test(`a strand outline takes over from its node's outline without a step (merge length ${mergeLength === 0 ? '0' : 'auto'})`, async ({ page }) => {
    await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
    const rows = await page.evaluate(async ([helper, m]) => {
      const raster = eval(helper);
      const app = window.SankeyApp;
      app.state.params.merge_length = m;
      Object.assign(app.state.style, { canvasWidth: 1000, canvasHeight: 850, cropExports: false });
      app.update();
      const read = await raster(8);
      const L = app.layout;
      const at = (points, x) => window.SK.layout.pointAtX(points, x)[1];
      // Above each interior node's top outgoing strand, where the node's outline (face or merge band)
      // hands over to the strand's: the outline's lowest dark row, relative to the edge, on either side.
      return L.nodes.filter((n) => n.inFlows.length && n.outFlows.length).map((n) => {
        const f = L.flows.filter((g) => g.from === n.id).sort((a, b) => b.startOffset - a.startOffset)[0];
        const x = f.startX + f.dir * L.mergeLength;
        const lowest = (px, base) => {
          for (let dy = -1.5; dy <= 1.5; dy += 1 / 8) if (read(px, base + dy) < 200) return dy;
          return null;
        };
        const before = x - f.dir * 0.7, after = x + f.dir * 0.7;
        return [lowest(before, L.mergeLength > 0 ? at(f.upper, before) : f.upper[0][1]), lowest(after, at(f.upper, after))];
      });
    }, [rasterHelper, mergeLength]);
    expect(rows.length).toBeGreaterThan(2);
    rows.forEach(([node, strand]) => {
      expect(node).not.toBeNull();
      expect(Math.abs(node - strand)).toBeLessThanOrEqual(1 / 8 + 1e-9);
    });
  });
}

test('where traced substrands cross, only the trace on top keeps its edge', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform, app = window.SankeyApp;
    // In alphabetical order A sits above B and Y above Z, so A -> Z and B -> Y cross.
    const rows = Array.from({ length: 6 }, () => 'A,Z').concat(Array.from({ length: 6 }, () => 'B,Y'));
    app.applyImport(T.build(T.parseTable('a,b\n' + rows.join('\n') + '\n'), { format: 'wide', columns: ['a', 'b'] }, { order: 'alphabetical' }), { labels: 'above' });
    Object.assign(app.state.style, { canvasWidth: 900, canvasHeight: 700, cropExports: false });
    app.update({ fit: true });
  });
  // Shift-click both strands into the pool; the first is traced (drawn) first, underneath.
  const clicks = await page.evaluate(() => {
    const box = document.querySelector('#plot').getBoundingClientRect();
    return window.SankeyApp.layout.flows.map((f) => {
      const [X, Y] = f.centerline[Math.floor(f.centerline.length * 0.2)];
      return { x: box.left + 18 + X, y: box.top + box.height - 18 - Y };
    });
  });
  for (const p of clicks) {
    await page.keyboard.down('Shift');
    await page.mouse.click(p.x, p.y);
    await page.keyboard.up('Shift');
  }
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace-edge')).toHaveCount(2);
  const result = await page.evaluate(async () => {
    const L = window.SankeyApp.layout, at = (points, x) => window.SK.layout.pointAtX(points, x)[1];
    const [under, over] = L.flows;
    let best = { depth: -Infinity };
    for (let x = under.startX + 2; x < under.endX - 2; x += 0.5) {
      [under.upper, under.lower].forEach((edge) => {
        const y = at(edge, x), a = at(over.upper, x), b = at(over.lower, x);
        const depth = (y - a) * (y - b) < 0 ? Math.min(Math.abs(y - a), Math.abs(y - b)) : -1;
        if (depth > best.depth) best = { x, y, depth };
      });
    }
    const raster = async (text) => {
      const image = new Image();
      const url = URL.createObjectURL(new Blob([text], { type: 'image/svg+xml' }));
      await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
      const canvas = document.createElement('canvas');
      canvas.width = image.width; canvas.height = image.height;
      canvas.getContext('2d').drawImage(image, 0, 0);
      URL.revokeObjectURL(url);
      return [...canvas.getContext('2d').getImageData(Math.floor(18 + best.x), Math.floor(image.height - 18 - best.y), 1, 1).data];
    };
    const text = window.SankeyApp.exportSvgText();
    return { depth: best.depth, edged: await raster(text), bare: await raster(text.replace(/<path class="trace-edge"[^>]*\/>/g, '')) };
  });
  expect(result.depth).toBeGreaterThan(3);
  result.edged.slice(0, 3).forEach((v, i) => expect(Math.abs(v - result.bare[i])).toBeLessThan(3));
});

for (const interior of [false, true]) {
  test(`${interior ? 'interior' : 'source'} merge joins keep tapered gaps and borders on the filled curves`, async ({ page }) => {
    await page.mouse.move(5, 5);
    const pixels = await page.evaluate(async interior => {
      const app = window.SankeyApp;
      const loc = [{ set: 1, node: 1, x: 0, y: 0.5, width: 0.4, label: 'Sedentary' },
        { set: 2, node: 1, x: 1, y: 1.5, width: 0.1 }, { set: 2, node: 2, x: 1, y: -0.5, width: 0.1 }];
      if (interior) {
        loc.forEach(n => { n.set++; n.x++; });
        loc[0].label = 'Post-Suspension';
        loc.unshift({ set: 1, node: 1, x: 0, y: 0.5, width: 0.1 });
      }
      Object.assign(app.state, { mode: 'layered', matrices: interior ? [[[0.65]], [[0.5, 0.15]]] : [[[0.5, 0.15]]],
        loc, paths: null, sourceTable: null, valueScale: 1, dimnames: null,
        view: { xlim: [-0.3, interior ? 2.3 : 1.3], ylim: [-0.8, 1.8] } });
      Object.assign(app.state.params, { adjust_loc: false, merge_frac: 0.2 });
      Object.assign(app.state.style, { opacity: 0.8, colorBy: 'single', singleColor: '#EF8E91', gradient: false,
        nodeBorder: 'none', labels: true, labelPosition: 'above', flatten: true, canvasWidth: 900, canvasHeight: 900, cropExports: false });
      app.update();
      const n = app.layout.nodes[interior ? 1 : 0];
      const fs = n.outFlows.map(k => app.layout.flows[k]).sort((a, b) => a.startOffset - b.startOffset);
      const X = fs[0].startX + app.layout.mergeLength / 2;
      const at = (points, x) => window.SK.layout.pointAtX(points, x)[1];
      const gap = [X, (at(fs[0].upper, X) + at(fs[1].lower, X)) / 2];
      const edge = [X, at(fs[1].upper, X)];
      const slope = (at(fs[1].upper, X + 0.1) - at(fs[1].upper, X - 0.1)) / 0.2;
      const normal = [-slope / Math.hypot(1, slope), 1 / Math.hypot(1, slope)];
      const besideEdge = distance => [edge[0] + distance * normal[0], edge[1] + distance * normal[1]];
      const toScreen = ([x, y]) => [Math.round(18 + x), Math.round(882 - y)];
      const sample = async thinOutline => {
        app.state.style.thinOutline = thinOutline;
        app.update();
        const url = URL.createObjectURL(new Blob([app.exportSvgText()], { type: 'image/svg+xml' }));
        const image = new Image();
        await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
        const canvas = document.createElement('canvas');
        canvas.width = image.width; canvas.height = image.height;
        const ctx = canvas.getContext('2d');
        ctx.drawImage(image, 0, 0);
        URL.revokeObjectURL(url);
        const pixel = p => [...ctx.getImageData(...toScreen(p), 1, 1).data];
        const [x, y] = toScreen(edge);
        return { gap: pixel(gap), inside: pixel(besideEdge(-3)), outside: pixel(besideEdge(3)),
          border: [...ctx.getImageData(x - 1, y - 1, 3, 3).data] };
      };
      return { on: await sample(true), off: await sample(false), gapWidth: at(fs[1].lower, X) - at(fs[0].upper, X),
        folds: fs.some(f => [f.upper, f.lower].some(points => points.slice(1).some((p, i) => p[0] < points[i][0]))) };
    }, interior);
    expect(pixels.folds).toBe(false);
    expect(pixels.gapWidth).toBeGreaterThan(3);
    expect(pixels.on.gap).toEqual([255, 255, 255, 255]);
    expect(pixels.off.gap).toEqual(pixels.on.gap);
    expect(pixels.on.outside).toEqual([255, 255, 255, 255]);
    expect(pixels.on.inside).toEqual([242, 165, 167, 255]);
    expect(pixels.on.border.some((v, i) => i % 4 !== 3 && v < pixels.off.border[i] - 10)).toBe(true);
  });
}

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

test('a click emphasizes one strand or node, Shift-click adds or removes one, empty space clears', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const through = (label) => page.evaluate((l) => {
    const app = window.SankeyApp;
    const n = app.layout.nodes.find((x) => app.state.loc[x.id].label === l);
    return app.state.paths.filter((p) => p.nodes[n.set - 1] === n.node - 1).length;
  }, label);
  const click = async (label, shift = false) => {
    const n = await nodeAt(page, label);
    if (shift) await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    if (shift) await page.keyboard.up('Shift');
  };
  const traces = page.locator('#plot .trace');
  const [surgery, radiation, chemo] = [await through('Surgery'), await through('Radiation'), await through('Chemotherapy')];
  // A plain click emphasizes just that node; Shift-click adds the next one to it.
  await click('Surgery');
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(surgery);
  await click('Radiation', true);
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(surgery + radiation);
  await expect(page.locator('#inspector-main h2').first()).toHaveText('Emphasized substrands');
  // Hovering a third node previews it on top of the emphasis, which stays.
  const c = await nodeAt(page, 'Chemotherapy');
  await page.mouse.move(c.x, c.y);
  await expect(traces).toHaveCount(surgery + radiation + chemo);
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(surgery + radiation);
  // Shift-clicking an emphasized node removes just it.
  await click('Surgery', true);
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(radiation);
  // A plain click moves the emphasis to the node clicked, and clicking it again clears it.
  await click('Chemotherapy');
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(chemo);
  await expect(page.locator('#inspector-main .edit-head')).toBeVisible();
  await click('Chemotherapy');
  await page.mouse.move(5, 5);
  await expect(traces).toHaveCount(0);
  // Clicking empty space clears emphasis and picks alike.
  await click('Surgery', true);
  await click('Radiation', true);
  await expect(page.locator('#merge-nodes')).toBeVisible();
  const box = await page.locator('#plot').boundingBox();
  await page.mouse.click(box.x + 4, box.y + 4);
  await expect(traces).toHaveCount(0);
  await expect(page.locator('#merge-nodes')).toBeHidden();
  await expect(page.locator('#notice')).toBeHidden();
});

test('Shift-click adds and removes substrands from a pool traced and tabulated together', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const shiftClick = async (label) => {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  };
  await shiftClick('Surgery');
  await shiftClick('Radiation');
  await page.mouse.move(5, 5);
  await expect(page.locator('#plot .trace')).toHaveCount(8);
  await expect(page.locator("#inspector-main h2").first()).toHaveText("Emphasized substrands");
  await expect(page.locator('#inspector-main')).toContainText('8 pathways · 308 patients');
  await shiftClick('Surgery');
  await page.mouse.move(5, 5);
  await expect(page.locator('#inspector-main')).toContainText('4 pathways · 76 patients');
  await page.locator('#pool-clear').click();
  await expect(page.locator('#plot .trace')).toHaveCount(0);
});

test('Shift-clicked nodes in one column merge into a renamed node, laid out as if imported that way', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const shiftClick = async (label) => {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  };
  const merge = page.locator('#merge-nodes');
  await expect(merge).toBeHidden();
  await shiftClick('Surgery');
  await shiftClick('Stage I');
  await expect(merge).toBeHidden(); // different columns
  await shiftClick('Stage I');
  await shiftClick('Radiation');
  await expect(merge).toBeVisible();
  await expect(merge).toHaveText('Merge 2');
  await merge.click();
  await expect(page.locator('#merge-dialog')).toBeVisible();
  await expect(page.locator('#merge-name')).toHaveValue('Surgery + Radiation');
  await page.locator('#merge-name').fill('Local therapy');
  await page.locator('#merge-name').press('Enter');
  await expect(page.locator('#merge-dialog')).toBeHidden();
  const after = await page.evaluate(() => ({
    labels: window.SankeyApp.layout.nodes.filter((n) => n.set === 2).map((n) => window.SankeyApp.state.loc[n.id].label),
    loc: JSON.parse(JSON.stringify(window.SankeyApp.state.loc)),
    total: window.SankeyApp.state.paths.reduce((t, p) => t + p.weight, 0)
  }));
  expect(after.labels).toEqual(['Local therapy', 'Chemotherapy', 'Palliative']);
  expect(after.total).toBe(560);
  // Same node positions as importing the table with both treatments renamed to one from the start.
  const fresh = await page.evaluate(() => {
    const ex = window.SK.examples.find((e) => e.id === 'pathways');
    const text = ex.files[ex.table].replace(/,(Surgery|Radiation),/g, ',Local therapy,');
    const table = window.SK.transform.parseTable(text);
    const app = window.SankeyApp;
    app.applyImport(window.SK.transform.build(table, ex.importSpec, ex.importOptions), { labels: ex.labels, params: ex.params });
    const loc = JSON.parse(JSON.stringify(app.state.loc));
    app.update();
    return { loc };
  });
  expect(after.loc).toEqual(fresh.loc);
  await page.keyboard.press('Control+z');
  await expect(page.locator('#warnings')).toBeHidden();
  await expect(merge).toBeHidden();
  // Pathways still trace through the merged node; those that differed only there are now one.
  const node = await nodeAt(page, 'Local therapy');
  await page.mouse.move(node.x, node.y);
  await expect(page.locator('#hover-info')).toContainText('5 pathways · 308 patients');
  await page.mouse.move(5, 5);
  await page.keyboard.press('Control+z');
  expect(await page.evaluate(() => window.SankeyApp.layout.nodes.filter((n) => n.set === 2).length)).toBe(4);
});

test('a merge in an imported table is kept by rebuilds and shows as one value in the strip', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  for (const label of ['Surgery', 'Radiation']) {
    const n = await nodeAt(page, label);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  }
  await page.locator('#merge-nodes').click();
  await page.locator('#merge-name').fill('Local therapy');
  await page.locator('#merge-name').press('Enter');
  const listed = () => page.locator('[data-strip-values="treatment"] .strip-value').evaluateAll((els) => els.map((el) => el.dataset.stripValue));
  const labels = () => page.evaluate(() => window.SankeyApp.layout.nodes.filter((n) => n.set === 2).map((n) => window.SankeyApp.state.loc[n.id].label));
  expect(await listed()).toEqual(['Local therapy', 'Chemotherapy', 'Palliative']);
  await expect(page.locator('[data-strip-values="treatment"] [data-strip-value="Local therapy"]')).not.toHaveClass(/absent/);
  // A change elsewhere rebuilds the diagram from the table, merge included.
  await page.locator('[data-strip-values="response"] [data-strip-value="Stable"]').click();
  expect(await labels()).toEqual(['Local therapy', 'Chemotherapy', 'Palliative']);
  expect((await state(page)).sourceTable.merges).toEqual({ treatment: [{ name: 'Local therapy', values: ['Surgery', 'Radiation'] }] });
  // The merged value drops (and keeps) all its members' rows.
  await page.locator('[data-strip-values="treatment"] [data-strip-value="Local therapy"]').click();
  expect((await state(page)).sourceTable.filters.treatment).toEqual(['Chemotherapy', 'Palliative']);
  expect(await labels()).toEqual(['Chemotherapy', 'Palliative']);
  await expect(page.locator('[data-strip-values="treatment"] [data-strip-value="Local therapy"]')).toHaveClass(/off/);
});

test('nodes of matrix data can be picked and merged too, and merging can be cancelled', async ({ page }) => {
  const ids = await page.evaluate(() => window.SankeyApp.layout.nodes.filter((n) => n.set === 1).slice(0, 2).map((n) => window.SankeyApp.state.loc[n.id].label));
  for (const label of ids) {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  }
  await page.mouse.move(5, 5);
  await expect(page.locator("#inspector-main h2").first()).toHaveText("Emphasized");
  await page.locator('#merge-nodes').click();
  await page.locator('#merge-cancel').click();
  expect(await page.evaluate(() => window.SankeyApp.state.matrices[0].length)).toBe(4);
  await page.locator('#merge-nodes').click();
  await page.locator('#merge-dialog button[value="ok"]').click();
  expect(await page.evaluate(() => window.SankeyApp.state.matrices[0].length)).toBe(3);
  await expect(page.locator('#plot .node-label', { hasText: `${ids[0]} + ${ids[1]}` })).toHaveCount(1);
});

test('strands of matrix data are emphasized by click and Shift-click alike', async ({ page }) => {
  const last = await page.evaluate(() => window.SankeyApp.layout.flows.length - 1);
  const click = async (k, shift = false) => {
    const p = await ribbonCenter(page, k);
    if (shift) await page.keyboard.down('Shift');
    await page.mouse.click(p.x, p.y);
    if (shift) await page.keyboard.up('Shift');
    await page.mouse.move(5, 5);
  };
  const lit = () => page.locator('#plot .ribbon.hl').evaluateAll((els) => els.map((el) => Number(el.dataset.flow)).sort((a, b) => a - b));
  await click(0);
  expect(await lit()).toEqual([0]);
  await click(last, true);
  expect(await lit()).toEqual([0, last]);
  await expect(page.locator('#inspector-main h2').first()).toHaveText('Emphasized');
  await expect(page.locator('#inspector-main')).toContainText('Strands:');
  await click(0, true);
  expect(await lit()).toEqual([last]);
  await click(0);
  expect(await lit()).toEqual([0]);
  await click(0);
  await expect(page.locator('#plot')).not.toHaveClass(/dimmed/);
});

test('dragging one of several Shift-clicked nodes moves them all by the same amount', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  for (const name of ['Surgery', 'Remission']) {
    const n = await nodeAt(page, name);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  }
  const ys = async () => page.evaluate(() => Object.fromEntries(window.SankeyApp.state.loc.map((r) => [r.label, r.y])));
  const factor = await page.evaluate(() => {
    const plot = document.querySelector('#plot');
    return window.SankeyApp.layout.tf.sy * plot.getBoundingClientRect().height / Number(plot.getAttribute('height'));
  });
  const drag = async (name, dy) => {
    const n = await nodeAt(page, name);
    await page.mouse.move(n.x, n.y);
    await page.mouse.down();
    await page.mouse.move(n.x, n.y + dy, { steps: 5 });
    await page.mouse.up();
  };
  const before = await ys();
  await drag('Remission', 20);
  const after = await ys();
  const moved = Object.keys(before).filter((k) => after[k] !== before[k]);
  expect(moved.sort()).toEqual(['Remission', 'Surgery']);
  expect(after.Surgery - before.Surgery).toBeCloseTo(after.Remission - before.Remission, 6);
  expect(before.Remission - after.Remission).toBeCloseTo(20 / factor, 3);
  // An unpicked node still moves alone.
  await drag('Chemotherapy', -10);
  const last = await ys();
  expect(Object.keys(after).filter((k) => last[k] !== after[k])).toEqual(['Chemotherapy']);
});

for (const label of ['Remission', 'Progression']) {
  test(`redoing a merge clears pinned ${label} pathways from the previous data`, async ({ page }) => {
    await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
    for (const name of ['Surgery', 'Radiation']) {
      const n = await nodeAt(page, name);
      await page.keyboard.down('Shift');
      await page.mouse.click(n.x, n.y);
      await page.keyboard.up('Shift');
    }
    await page.locator('#merge-nodes').click();
    await page.locator('#merge-name').fill('Local therapy');
    await page.locator('#merge-name').press('Enter');
    await page.keyboard.press('Control+z');
    const n = await nodeAt(page, label);
    await page.mouse.click(n.x, n.y);
    await page.mouse.move(5, 5);
    await expect(page.locator('#plot .trace').first()).toBeAttached();
    await page.keyboard.press('Control+Shift+z');
    await expect(page.locator('#plot .trace')).toHaveCount(0);
    await expect(page.locator('#plot')).not.toHaveClass(/dimmed/);
    await expect(page.locator('#redo')).toBeDisabled();
    await expect(page.locator('#banner')).toBeHidden();
    const merged = await nodeAt(page, label);
    await page.mouse.click(merged.x, merged.y);
    await page.mouse.move(5, 5);
    const total = await page.evaluate((name) => window.SankeyApp.layout.nodes.find((n) => window.SankeyApp.state.loc[n.id].label === name).inValue, label);
    await expect(page.locator('#inspector-main')).toContainText(`${total} patients`);
    if (label === 'Remission') await expect(page.locator('#inspector-main')).not.toContainText('→ Progression');
  });
}

test('"No overlaps in nodes" is off by default and can be switched on; traces are as see-through as strands', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  expect(await page.evaluate(() => window.SankeyApp.state.style.tileInNodes)).toBe(false);
  const box = page.locator('#ctl-tileInNodes');
  await expect(box).toBeAttached();
  for (const on of [true, false]) {
    await page.evaluate((v) => { window.SankeyApp.state.style.tileInNodes = v; window.SankeyApp.update(); }, on);
    const node = await nodeAt(page, 'Remission');
    await page.mouse.move(node.x, node.y);
    expect(await page.locator('#plot .trace').count()).toBeGreaterThan(1);
    const opacity = await page.locator('#plot .trace-group').first().getAttribute('opacity');
    expect(Number(opacity)).toBeCloseTo(await page.evaluate(() => window.SankeyApp.state.style.opacity), 6);
    await page.mouse.move(5, 5);
  }
});

test('dragging a node past the edge or into the next column never grows the canvas', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const before = await page.evaluate(() => ({ width: document.querySelector('#plot').getAttribute('width'), view: JSON.stringify(window.SankeyApp.state.view) }));
  const drag = async (label, dx, dy) => {
    const p = await page.evaluate((l) => {
      const n = window.SankeyApp.layout.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === l);
      const r = [...document.querySelectorAll(`#plot .trunk[data-node="${n.id}"]`)].map((el) => el.getBoundingClientRect());
      return { x: (Math.min(...r.map((b) => b.left)) + Math.max(...r.map((b) => b.right))) / 2, y: (Math.min(...r.map((b) => b.top)) + Math.max(...r.map((b) => b.bottom))) / 2 };
    }, label);
    await page.mouse.move(p.x, p.y);
    await page.mouse.down();
    for (let k = 1; k <= 20; k++) await page.mouse.move(p.x + (dx * k) / 20, p.y + (dy * k) / 20);
    await page.mouse.up();
  };
  await drag('Surgery', 700, 0);
  await drag('Remission', 900, 0);
  const after = await page.evaluate(() => ({
    width: document.querySelector('#plot').getAttribute('width'),
    view: JSON.stringify(window.SankeyApp.state.view),
    minCanvasWidth: window.SankeyApp.state.sourceTable.minCanvasWidth,
    maxX: Math.max(...window.SankeyApp.state.loc.map((r) => r.x)),
    xlim: window.SankeyApp.state.view.xlim
  }));
  expect(after.width).toBe(before.width);
  expect(after.view).toBe(before.view);
  expect(after.minCanvasWidth || 0).toBe(0);
  expect(after.maxX).toBeLessThanOrEqual(after.xlim[1] + 1e-9);
  await expect(page.locator('#banner')).toBeHidden();
});

test('fit view shrinks a canvas that an earlier edit widened', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const normal = await page.evaluate(() => document.querySelector('#plot').getAttribute('width'));
  await page.evaluate(() => { window.SankeyApp.state.sourceTable.minCanvasWidth = 4000; window.SankeyApp.update(); });
  expect(await page.evaluate(() => document.querySelector('#plot').getAttribute('width'))).toBe('4000');
  await page.locator('#fit').click();
  expect(await page.evaluate(() => document.querySelector('#plot').getAttribute('width'))).toBe(normal);
});

// Wide table (as in the stand-in for the user's data) whose constant-width turns only draw at the
// plot's own resolution.
function tightTable() {
  let seed = 3;
  const next = () => (seed = (seed * 16807) % 2147483647) / 2147483647;
  const pick = (w) => { let u = next() * w.reduce((a, b) => a + b, 0); return w.findIndex((v) => (u -= v) < 0); };
  const rows = ['participant_id,activity,training,suspension,sharing'];
  for (let k = 0; k < 1029; k++) {
    const a = pick([0.27, 0.73]);
    const b = pick(a ? [0.45, 0.35, 0.2] : [0.5, 0.35, 0.15]);
    const c = pick(b === 2 ? [0.25, 0.75] : [0.2, 0.8]);
    const d = pick(c === 1 ? [0.25, 0.72, 0.03] : [0.35, 0.6, 0.05]);
    rows.push(`P${k},${['Highly Active', 'Sedentary'][a]},${['Resistance', 'Endurance', 'Control'][b]},${['Pre-Suspension', 'Post-Suspension'][c]},${['Low-risk only', 'Open/all data', 'No open sharing'][d]}`);
  }
  return rows.join('\n') + '\n';
}

test('nodes of tightly bent imported tables can still be dragged', async ({ page }) => {
  await page.evaluate((text) => {
    const T = window.SK.transform;
    window.SankeyApp.applyImport(T.build(T.parseTable(text), { format: 'wide', id: 'participant_id', columns: ['activity', 'training', 'suspension', 'sharing'], weight: null, missing: 'drop' }, { order: 'appearance' }), { labels: 'above' });
  }, tightTable());
  const box = await page.evaluate(() => {
    const app = window.SankeyApp;
    const id = app.state.loc.findIndex((r) => r.label === 'Endurance');
    const r = [...document.querySelectorAll(`#plot .trunk[data-node="${id}"]`)].map((el) => el.getBoundingClientRect());
    return { id, y: app.state.loc[id].y, x: (Math.min(...r.map((b) => b.left)) + Math.max(...r.map((b) => b.right))) / 2, top: (Math.min(...r.map((b) => b.top)) + Math.max(...r.map((b) => b.bottom))) / 2 };
  });
  await page.mouse.move(box.x, box.top);
  await page.mouse.down();
  for (let k = 1; k <= 8; k++) await page.mouse.move(box.x, box.top - 15 * k);
  await page.mouse.up();
  expect(await page.evaluate((id) => window.SankeyApp.state.loc[id].y, box.id)).toBeGreaterThan(box.y + 0.01);
  await expect(page.locator('#banner')).toBeHidden();
});

test('the wheel zooms about the pointer and dragging empty space pans', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const plot = await page.locator('#plot').boundingBox();
  const at = { x: plot.x + plot.width * 0.6, y: plot.y + plot.height * 0.5 };
  const before = await page.evaluate(() => window.SankeyApp.state.presentation.zoom);
  await page.mouse.move(at.x, at.y);
  await page.mouse.wheel(0, -600);
  await expect.poll(() => page.evaluate(() => window.SankeyApp.state.presentation.zoom)).toBeGreaterThan(before * 1.5);
  // The content point under the pointer stays (about) where it was.
  const after = await page.locator('#plot').boundingBox();
  const fx = (at.x - plot.x) / plot.width;
  expect(Math.abs(after.x + fx * after.width - at.x)).toBeLessThan(3);
  // Grab empty space (top-left corner of the plot) and drag: the panel scrolls.
  const scroll0 = await page.evaluate(() => [document.querySelector('#stage').scrollLeft, document.querySelector('#stage').scrollTop]);
  const empty = await page.evaluate(() => {
    const r = document.querySelector('#plot').getBoundingClientRect();
    const stage = document.querySelector('#stage').getBoundingClientRect();
    for (let x = Math.max(r.left, stage.left) + 4; x < stage.right - 4; x += 6) {
      for (let y = Math.max(r.top, stage.top) + 4; y < stage.bottom - 4; y += 6) {
        const el = document.elementFromPoint(x, y);
        if (el && (el.id === 'plot' || el.classList.contains('background'))) return { x, y };
      }
    }
    return null;
  });
  expect(empty).not.toBeNull();
  await page.mouse.move(empty.x, empty.y);
  await page.mouse.down();
  await page.mouse.move(empty.x - 120, empty.y - 80, { steps: 6 });
  await page.mouse.up();
  const scroll1 = await page.evaluate(() => [document.querySelector('#stage').scrollLeft, document.querySelector('#stage').scrollTop]);
  expect(scroll1[0] + scroll1[1]).toBeGreaterThan(scroll0[0] + scroll0[1] + 50);
});

test('overlapping traces add a shade, strands show behind nodes, and outlines sit above strand fills', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    window.SankeyApp.state.style.thinOutline = true;
    window.SankeyApp.update();
  });
  // Faint clipped copies of strands that pass behind nodes they do not touch.
  const ghosts = await page.evaluate(() => [...document.querySelectorAll('#plot .ribbon-ghost')].map((g) => {
    const f = window.SankeyApp.layout.flows[Number(g.dataset.flow)];
    const node = Number(g.getAttribute('clip-path').match(/behind(\d+)/)[1]);
    return f.from !== node && f.to !== node;
  }));
  expect(ghosts.length).toBeGreaterThan(0);
  expect(ghosts.every(Boolean)).toBe(true);
  // Every strand outline comes after every strand fill.
  expect(await page.evaluate(() => {
    const layers = [...document.querySelector('#plot').children].map((el) => el.getAttribute('class'));
    return layers.indexOf('layer-strand-outlines') === layers.indexOf('layer-ribbons') + 1 &&
      document.querySelector('#plot .layer-strand-outlines .plot-outline') !== null &&
      document.querySelector('#plot .layer-ribbons .plot-outline') === null;
  })).toBe(true);
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x, node.y);
  await expect(page.locator('#plot .trace-overlap')).toHaveCount(1);
  expect(await page.locator('#plot .trace-overlap > g').count()).toBe(await page.locator('#plot .trace').count());
});

test('the corner handle resizes the canvas while the plot keeps its scale', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const handle = page.locator('#canvas-resize');
  const plot = await page.locator('#plot').boundingBox();
  const h = await handle.boundingBox();
  expect(Math.abs(h.x + h.width - (plot.x + plot.width))).toBeLessThan(1.5);
  expect(Math.abs(h.y + h.height - (plot.y + plot.height))).toBeLessThan(1.5);
  const before = await page.evaluate(() => ({ sx: window.SankeyApp.layout.tf.sx, sy: window.SankeyApp.layout.tf.sy, width: Number(document.querySelector('#plot').getAttribute('width')) }));
  await page.mouse.move(h.x + 8, h.y + 8);
  await page.mouse.down();
  await page.mouse.move(h.x + 8 - 60, h.y + 8 - 40, { steps: 4 });
  await page.mouse.move(h.x + 8 + 150, h.y + 8 + 90, { steps: 6 });
  await page.mouse.up();
  const after = await page.evaluate(() => ({ sx: window.SankeyApp.layout.tf.sx, sy: window.SankeyApp.layout.tf.sy, width: Number(document.querySelector('#plot').getAttribute('width')), style: window.SankeyApp.state.style }));
  expect(after.width).toBeGreaterThan(before.width + 50);
  expect(after.style.canvasWidth).toBe(after.width);
  // Same scale, up to the rounding of view limits and whole-pixel canvas sizes.
  expect(Math.abs(after.sx / before.sx - 1)).toBeLessThan(1e-4);
  expect(Math.abs(after.sy / before.sy - 1)).toBeLessThan(1e-4);
});

test('dragging a node redraws without fade transitions', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Surgery');
  await page.mouse.move(node.x, node.y);
  await page.mouse.down();
  await page.mouse.move(node.x + 10, node.y + 20, { steps: 3 });
  const durations = await page.evaluate(() => [...document.querySelectorAll('#plot .ribbon, #plot .trunk')].map((el) => getComputedStyle(el).transitionDuration));
  await page.mouse.up();
  expect(durations.every((d) => d === '0s')).toBe(true);
  const idle = await page.evaluate(() => getComputedStyle(document.querySelector('#plot .ribbon')).transitionDuration);
  expect(idle).not.toBe('0s');
});

test('Align lines up the top and bottom nodes of every column; double-click fits the canvas', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await page.locator('#align').click();
  const edges = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const h = (n) => Math.max(n.inPx, n.outPx, n.bodyHeightPx || 0);
    const sets = [...new Set(L.nodes.map((n) => n.set))];
    return sets.map((s) => {
      const g = L.nodes.filter((n) => n.set === s);
      return { top: Math.max(...g.map((n) => n.Y + h(n) / 2)), bottom: Math.min(...g.map((n) => n.Y - h(n) / 2)), xs: [...new Set(g.map((n) => window.SankeyApp.state.loc[n.id].x))].length };
    });
  });
  edges.forEach((e) => {
    expect(Math.abs(e.top - edges[0].top)).toBeLessThan(1);
    expect(Math.abs(e.bottom - edges[0].bottom)).toBeLessThan(1);
    expect(e.xs).toBe(1);
  });
  await page.keyboard.press('Control+z');
  await page.locator('#zoom-in').click();
  const plot = await page.locator('#plot').boundingBox();
  await page.mouse.dblclick(plot.x + 6, plot.y + 6);
  expect(await page.evaluate(() => window.SankeyApp.state.presentation.mode)).toBe('panel');
});

test('labels above nodes keep clear of their own strands', async ({ page }) => {
  await page.evaluate((text) => {
    const T = window.SK.transform;
    window.SankeyApp.applyImport(T.build(T.parseTable(text), { format: 'wide', id: 'participant_id', columns: ['activity', 'training', 'suspension', 'sharing'], weight: null, missing: 'drop' }, { order: 'appearance' }), { labels: 'above' });
  }, tightTable());
  const crossed = await page.evaluate(() => {
    const app = window.SankeyApp, L = app.layout, R = window.SK.render, m = R.makeMeasure(app.state.style);
    const out = [];
    L.nodes.forEach((n) => {
      const size = app.state.style.fontSize * n.cex;
      const pos = R.labelPlacement(n, app.state.style, size);
      const half = m(n.label, n.cex, n.font).width / 2;
      n.inFlows.concat(n.outFlows).forEach((k) => {
        const f = L.flows[k];
        for (let i = 0; i <= 20; i++) {
          const x = pos.X - half + (2 * half * i) / 20;
          if (x <= Math.min(f.startX, f.endX) || x >= Math.max(f.startX, f.endX)) continue;
          const lo = window.SK.layout.pointAtX(f.lower, x)[1], up = window.SK.layout.pointAtX(f.upper, x)[1];
          if (Math.min(lo, up) < pos.Y + 0.4 * size && Math.max(lo, up) > pos.Y - 0.4 * size) out.push(n.label);
        }
      });
    });
    return [...new Set(out)];
  });
  expect(crossed).toEqual([]);
});

test('imported tables fit labels in their nodes and spread tight columns, also on a fixed canvas', async ({ page }) => {
  const importTight = () => page.evaluate((text) => {
    const T = window.SK.transform;
    window.SankeyApp.applyImport(T.build(T.parseTable(text), { format: 'wide', id: 'participant_id', columns: ['activity', 'training', 'suspension', 'sharing'], weight: null, missing: 'drop' }, { order: 'appearance' }), { labels: 'above' });
  }, tightTable());
  const check = () => page.evaluate(() => {
    const app = window.SankeyApp, L = app.layout, R = window.SK.render, m = R.makeMeasure(app.state.style);
    const overhang = Math.max(...L.nodes.map((n) => {
      const pos = R.labelPlacement(n, app.state.style, app.state.style.fontSize * n.cex);
      const half = m(n.label, n.cex, n.font).width / 2;
      return Math.max(n.X - n.widthPx / 2 - (pos.X - half), pos.X + half - (n.X + n.widthPx / 2));
    }));
    const slopes = [];
    L.flows.forEach((f) => { slopes[f.stage] = Math.max(slopes[f.stage] || 0, Math.abs(f.endY + f.endOffset - f.startY - f.startOffset) / (f.endX - f.startX)); });
    return { overhang, slopes, steepest: Math.max(...slopes), width: Number(document.querySelector('#plot').getAttribute('width')), banner: !document.querySelector('#banner').hidden };
  });
  await importTight();
  const auto = await check();
  expect(auto.overhang).toBeLessThanOrEqual(0);
  expect(auto.steepest).toBeLessThan(2.1);
  // Every gap's steepest strand has the same slope.
  expect(auto.steepest / Math.min(...auto.slopes)).toBeLessThan(1.05);
  // Fit view keeps the room the generated layout needs.
  await page.locator('#fit').click();
  expect((await check()).width).toBe(auto.width);
  // Gaps within the limit are balanced as well: one steep and one gentle gap meet in the middle.
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const example = await check();
  expect(example.steepest).toBeLessThan(2.1);
  expect(example.steepest / Math.min(...example.slopes)).toBeLessThan(1.05);
  // A fixed canvas width too narrow for these turns still draws: the columns move apart and the
  // drawing shrinks instead of the canvas growing.
  await page.evaluate(() => { window.SankeyApp.state.style.canvasWidth = 900; });
  await importTight();
  const fixed = await check();
  expect(fixed.banner).toBe(false);
  expect(fixed.width).toBe(900);
  expect(fixed.overhang).toBeLessThanOrEqual(0);
});

test('exports crop to the visible drawing with a small margin, or keep the whole canvas', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  const exported = () => page.evaluate(() => {
    const doc = new DOMParser().parseFromString(window.SankeyApp.exportSvgText(), 'image/svg+xml').documentElement;
    const svg = document.querySelector('#plot');
    const hidden = [...svg.querySelectorAll(':scope > .background, :scope > .layer-hits')];
    hidden.forEach((el) => { el.style.display = 'none'; });
    const b = svg.getBBox();
    hidden.forEach((el) => el.style.removeProperty('display'));
    return { viewBox: doc.getAttribute('viewBox').split(' ').map(Number), width: Number(doc.getAttribute('width')),
      canvas: [Number(svg.getAttribute('width')), Number(svg.getAttribute('height'))], drawing: [b.x, b.y, b.x + b.width, b.y + b.height],
      background: doc.querySelector('.background') && doc.querySelector('.background').getAttribute('width') };
  });
  const cropped = await exported();
  const [x, y, w, h] = cropped.viewBox;
  expect(cropped.width).toBe(w);
  // 12 px around the drawing, rounded out to whole pixels.
  [cropped.drawing[0] - x, cropped.drawing[1] - y, x + w - cropped.drawing[2], y + h - cropped.drawing[3]].forEach((m) => {
    expect(m).toBeGreaterThanOrEqual(12);
    expect(m).toBeLessThan(13);
  });
  expect(w).toBeLessThan(cropped.canvas[0]);
  if (cropped.background) expect(Number(cropped.background)).toBe(w);
  // A fixed canvas size is cropped too; with cropping off the export keeps the whole canvas.
  await page.evaluate(() => { window.SankeyApp.state.style.canvasWidth = 900; window.SankeyApp.update(); });
  expect((await exported()).viewBox[2]).toBeLessThan(900);
  await page.locator('#ctl-cropExports').uncheck();
  const whole = await exported();
  expect(whole.viewBox).toEqual([0, 0, ...whole.canvas]);
});

test('SVG and PNG export in one click and keep the focused view', async ({ page }) => {
  await expect(page.locator('.topbar .tool-group > [data-export="svg"]')).toBeVisible();
  await expect(page.locator('.topbar .tool-group > [data-export="png"]')).toBeVisible();
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
  expect(svg).not.toContain('stroke-dasharray');
});

test('toolbar groups alternate backgrounds; Auto-layout sits between Fit view and Align', async ({ page }) => {
  const backgrounds = await page.locator('.topbar .tool-group').evaluateAll((els) => els.map((el) => getComputedStyle(el).backgroundColor));
  expect(backgrounds.length).toBeGreaterThan(3);
  backgrounds.forEach((bg, i) => expect(bg).toBe(backgrounds[i % 2]));
  expect(backgrounds[0]).not.toBe(backgrounds[1]);
  await expect(page.locator('.topbar #fit + #auto-layout + #align')).toHaveCount(1);
  await expect(page.locator('.sidebar #auto-layout')).toHaveCount(0);
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const fresh = await page.evaluate(() => window.SankeyApp.state.loc.map((r) => [r.x, r.y]));
  await page.evaluate(() => { window.SankeyApp.state.loc[0].y -= 0.05; window.SankeyApp.update({ commit: true }); });
  await page.locator('#auto-layout').click();
  const after = await page.evaluate(() => window.SankeyApp.state.loc.map((r) => [r.x, r.y]));
  after.forEach(([x, y], i) => {
    expect(x).toBeCloseTo(fresh[i][0], 6);
    expect(y).toBeCloseTo(fresh[i][1], 6);
  });
});

test('toolbar arrangements center columns, spread nodes evenly and even out column gaps', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const column = () => page.evaluate(() => {
    const app = window.SankeyApp;
    const h = (n) => Math.max(n.inPx, n.outPx, n.bodyHeightPx || 0);
    const nodes = app.layout.nodes.filter((n) => n.set === 2).sort((a, b) => b.Y - a.Y);
    return { nodes: nodes.map((n) => ({ label: app.state.loc[n.id].label, x: app.state.loc[n.id].x, y: app.state.loc[n.id].y })),
      gaps: nodes.slice(1).map((n, i) => (nodes[i].Y - h(nodes[i]) / 2) - (n.Y + h(n) / 2)) };
  });
  const edit = (changes) => page.evaluate((c) => {
    const app = window.SankeyApp;
    Object.entries(c).forEach(([label, d]) => { const r = app.state.loc.find((x) => x.label === label); r.x += d.x || 0; r.y += d.y || 0; });
    app.update({ commit: true });
  }, changes);
  const pick = async (name) => {
    const n = await nodeAt(page, name);
    await page.keyboard.down('Shift');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Shift');
  };
  // Picked nodes of a column center on their own mean x; without picks every column centers.
  await edit({ Surgery: { x: 0.04 }, Chemotherapy: { x: -0.02 } });
  await pick('Surgery');
  await pick('Chemotherapy');
  await page.locator('#center-columns').click();
  let c = await column();
  const x = (label) => c.nodes.find((n) => n.label === label).x;
  expect(x('Surgery')).toBeCloseTo(x('Chemotherapy'), 6);
  expect(x('Radiation')).not.toBeCloseTo(x('Surgery'), 3);
  await page.keyboard.press('Escape');
  await page.locator('#center-columns').click();
  c = await column();
  expect(new Set(c.nodes.map((n) => n.x)).size).toBe(1);
  // Spreading keeps the top and bottom nodes and evens out the gaps between them.
  await edit({ Radiation: { y: 0.05 } });
  const before = await column();
  await page.locator('#distribute-columns').click();
  c = await column();
  expect(c.nodes[0].y).toBeCloseTo(before.nodes[0].y, 6);
  expect(c.nodes[c.nodes.length - 1].y).toBeCloseTo(before.nodes[before.nodes.length - 1].y, 6);
  expect(Math.max(...c.gaps) - Math.min(...c.gaps)).toBeLessThan(1);
  expect(Math.max(...before.gaps) - Math.min(...before.gaps)).toBeGreaterThan(5);
  // Evening out the column gaps gives every gap's steepest strand the same slope (here the wide
  // right gap lends run to the left one).
  await page.evaluate(() => {
    const app = window.SankeyApp;
    app.state.loc.filter((r) => r.set === 3).forEach((r) => { r.x += 0.3; });
    app.update({ commit: true });
  });
  await page.locator('#space-columns').click();
  const slopes = await page.evaluate(() => {
    const out = [];
    window.SankeyApp.layout.flows.forEach((f) => { out[f.stage] = Math.max(out[f.stage] || 0, Math.abs(f.endY + f.endOffset - f.startY - f.startOffset) / (f.endX - f.startX)); });
    return out;
  });
  expect(Math.max(...slopes) / Math.min(...slopes)).toBeLessThan(1.05);
  await expect(page.locator('#banner')).toBeHidden();
});

test('setting cartoons show slider ends, the chosen option and what a checkbox does', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  const row = (id) => page.locator(`.sidebar .row:has(#${id})`);
  await expect(row('ctl-curvature').locator('svg.glyph')).toHaveCount(2);
  const routeGlyph = () => row('ctl-route').locator('svg.glyph').evaluate((el) => el.innerHTML);
  await expect(row('ctl-route').locator('svg.glyph title')).toHaveText('Bezier curves');
  const bezier = await routeGlyph();
  await page.locator('#ctl-route').selectOption('boundary');
  await expect(row('ctl-route').locator('svg.glyph title')).toHaveText('Packed boundaries');
  expect(await routeGlyph()).not.toBe(bezier);
  const gather = row('ctl-gatherInNodes').locator('.with-glyph');
  await expect(gather).toHaveClass(/off/);
  await page.locator('#ctl-gatherInNodes').check();
  await expect(gather).not.toHaveClass(/off/);
  // Every slider shows its low and high end.
  const bare = await page.locator('.sidebar .range').evaluateAll((els) =>
    els.filter((el) => el.querySelectorAll('svg.glyph').length !== 2).map((el) => el.querySelector('.range-value').id));
  expect(bare).toEqual([]);
  // Settings without a cartoon keep their plain control.
  await expect(row('ctl-source_order').locator('svg.glyph')).toHaveCount(0);
});

test('numeric settings are sliders whose numbers can be typed, past the slider unless bounded', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  const slide = (id, value) => page.locator(`.sidebar .row:has(#${id}) input[type="range"]`).evaluate((el, v) => {
    el.value = v;
    el.dispatchEvent(new Event('input', { bubbles: true }));
    el.dispatchEvent(new Event('change', { bubbles: true }));
  }, value);
  const type = async (id, text) => {
    await page.locator(`#${id}`).click();
    await page.locator(`#${id}`).fill(text);
    await page.locator(`#${id}`).press('Enter');
  };
  const st = () => page.evaluate(() => JSON.parse(JSON.stringify(window.SankeyApp.state)));
  const sliderValue = (id) => page.locator(`.sidebar .row:has(#${id}) input[type="range"]`).inputValue();
  // Every numeric setting has a slider.
  expect(await page.locator('.sidebar input[type="number"]:not(.range-value):not(.pair input)').count()).toBe(0);
  await slide('ctl-merge_frac', 0.1);
  expect((await st()).params.merge_frac).toBe(0.1);
  await expect(page.locator('#ctl-merge_frac')).toHaveValue('0.1');
  // Typed values may pass the slider's end, which then stays at its end.
  await type('ctl-merge_frac', '0.45');
  expect((await st()).params.merge_frac).toBe(0.45);
  expect(Number(await sliderValue('ctl-merge_frac'))).toBe(0.3);
  // Bounded settings reject values outside their range, loudly.
  const opacity = (await st()).style.opacity;
  await type('ctl-opacity', '1.5');
  await expect(page.locator('#banner')).toContainText('Opacity must be between 0.05 and 1');
  expect((await st()).style.opacity).toBe(opacity);
  // Clearing a nullable setting makes it automatic again.
  await type('ctl-merge_length', '0.1');
  expect((await st()).params.merge_length).toBe(0.1);
  await type('ctl-merge_length', '');
  expect((await st()).params.merge_length).toBeNull();
  await expect(page.locator('#ctl-merge_length')).toHaveAttribute('placeholder', 'auto');
  // Strand thickness is relative to the imported scale (1) and slides on a log scale, 0.1 to 10.
  await expect(page.locator('#ctl-valueScale')).toHaveValue('1');
  const scale = (await st()).valueScale;
  await slide('ctl-valueScale', 1);
  expect((await st()).valueScale / scale).toBeCloseTo(10, 3);
  await expect(page.locator('#ctl-valueScale')).toHaveValue('10');
  await type('ctl-valueScale', '0.5');
  expect((await st()).valueScale / scale).toBeCloseTo(0.5, 3);
  await type('ctl-canvasWidth', '900');
  await expect(page.locator('#plot')).toHaveAttribute('width', '900');
});

test('setting names wrap instead of being cut off', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  const cut = await page.evaluate(() => [...document.querySelectorAll('.sidebar .row > label')].filter((l) => {
    const row = l.closest('.row');
    const hidden = row.hidden;
    row.hidden = false;
    const over = l.scrollWidth > l.clientWidth + 1;
    row.hidden = hidden;
    return over;
  }).map((l) => l.textContent));
  expect(cut).toEqual([]);
});

test('drag bars resize the settings panel and the inspector, persist, and reset on double-click', async ({ page }) => {
  const widths = () => page.evaluate(() => ({
    sidebar: Math.round(document.querySelector('.sidebar').getBoundingClientRect().width),
    stage: Math.round(document.querySelector('#stage').getBoundingClientRect().width),
    inspector: Math.round(document.querySelector('#inspector').getBoundingClientRect().width),
    canvas: Number(document.querySelector('#plot').getAttribute('width')),
    overflow: document.documentElement.scrollHeight - window.innerHeight
  }));
  const dragBar = async (id, dx) => {
    const box = await page.locator(id).boundingBox();
    await page.mouse.move(box.x + box.width / 2, box.y + 200);
    await page.mouse.down();
    await page.mouse.move(box.x + box.width / 2 + dx, box.y + 200, { steps: 4 });
    await page.mouse.up();
  };
  const before = await widths();
  expect(before.overflow).toBeLessThanOrEqual(0);
  await dragBar('#resize-sidebar', 100);
  await dragBar('#resize-inspector', -80);
  const after = await widths();
  expect(after.sidebar).toBe(before.sidebar + 100);
  expect(after.inspector).toBe(before.inspector + 80);
  expect(after.stage).toBe(before.stage - 180);
  // The automatic canvas follows the narrower plot panel.
  await expect.poll(async () => (await widths()).canvas).toBeLessThan(before.canvas);
  await page.reload();
  const reloaded = await widths();
  expect([reloaded.sidebar, reloaded.inspector]).toEqual([after.sidebar, after.inspector]);
  await page.locator('#resize-sidebar').dblclick();
  await page.locator('#resize-inspector').dblclick();
  const reset = await widths();
  expect([reset.sidebar, reset.inspector]).toEqual([before.sidebar, before.inspector]);
});

test('totals can follow node labels', async ({ page }) => {
  await page.evaluate(() => {
    window.SankeyApp.loadExample('pathways');
    window.SankeyApp.state.style.labelCounts = true;
    window.SankeyApp.update({ fit: true });
  });
  const n = await page.evaluate(() => window.SankeyApp.layout.nodes.find((x) => x.label.startsWith('Surgery')).id);
  await expect(page.locator(`#plot .node-label[data-node="${n}"]`)).toHaveText('Surgery (232)');
  const node = await nodeAt(page, 'Surgery (232)');
  await page.mouse.move(node.x, node.y);
  await expect(page.locator('#hover-info h2')).toHaveText('Surgery');
  const loc = await page.evaluate(() => window.SankeyApp.bundleFiles().find((f) => f.name === 'loc.csv').text);
  expect(loc).toContain('Surgery (232)');
});

for (const storedCountSetting of [undefined, false]) {
  test(`legacy sessions show node counts after refresh and retain a later opt-out (${storedCountSetting})`, async ({ page }) => {
    await page.evaluate(storedCountSetting => {
      const app = window.SankeyApp;
      app.loadExample('pathways');
      app.state.style.labelCounts = false;
      app.update({ fit: true });
      const saved = JSON.parse(JSON.stringify(app.state));
      delete saved.labelCountsVersion;
      if (storedCountSetting === undefined) delete saved.style.labelCounts;
      localStorage.setItem('polylines-sankey-v1', JSON.stringify(saved));
    }, storedCountSetting);
    const before = await state(page);
    await page.reload();
    await expect(page.locator('#ctl-labelCounts')).toBeChecked();
    const node = page.locator('#plot .node-label').filter({ hasText: 'Surgery' });
    await expect(node).toHaveText('Surgery (232)');
    expect((await state(page)).matrices).toEqual(before.matrices);
    const clipped = await page.locator('#plot .node-label').evaluateAll(els => {
      const plot = document.querySelector('#plot');
      return els.some(el => { const b = el.getBBox(); return b.x < 0 || b.y < 0 || b.x + b.width > plot.width.baseVal.value || b.y + b.height > plot.height.baseVal.value; });
    });
    expect(clipped).toBe(false);
    await page.locator('#ctl-labelCounts').uncheck();
    await expect.poll(() => page.evaluate(() => {
      const saved = JSON.parse(localStorage.getItem('polylines-sankey-v1'));
      return { counts: saved?.style.labelCounts, version: saved?.labelCountsVersion };
    })).toEqual({ counts: false, version: 1 });
    await page.reload();
    await expect(page.locator('#ctl-labelCounts')).not.toBeChecked();
    await expect(node).toHaveText('Surgery');
  });
}

test('node label counts use compact scientific notation above four digits without changing data', async ({ page }) => {
  const counts = [9999, 10000, 12345, 99999, 10000000, 1250000, 3.75];
  const suffixes = ['9,999', '1E04', '1E04', '1E05', '1E07', '1E06', '3.75'];
  await page.evaluate(counts => {
    const T = window.SK.transform;
    const table = T.parseTable('source,target,n\n' + counts.map((n, i) => `Count${i},All,${n}`).join('\n'));
    window.SankeyApp.applyImport(T.build(table, { format: 'wide', columns: ['source', 'target'], weight: 'n' }), { labels: 'above' });
  }, counts);
  await page.locator('#ctl-labelCounts').check();
  const labels = counts.map((_, i) => `Count${i} (${suffixes[i]})`);
  for (const label of labels) await expect(page.locator('#plot .node-label').filter({ hasText: label })).toHaveText(label);
  const exports = await page.evaluate(() => {
    const app = window.SankeyApp;
    return { svg: app.exportSvgText(), files: app.bundleFiles(), values: app.state.matrices[0].map(row => row[0]), labels: app.state.loc.map(row => row.label) };
  });
  expect(exports.values).toEqual(counts);
  expect(exports.labels).toEqual([...counts.map((_, i) => `Count${i}`), 'All']);
  for (const label of labels) {
    expect(exports.svg).toContain(label);
    expect(exports.files.find(f => f.name === 'loc.csv').text).toContain(label);
    expect(exports.files.find(f => f.name === 'sankey.R').text).toContain(label);
  }
  await page.locator('#ctl-labelCounts').uncheck();
  await expect(page.locator('#plot .node-label').filter({ hasText: 'Count0' })).toHaveText('Count0');
  await expect(page.locator('#banner')).toBeHidden();
});

test('nodes and strands fully covered by traces hide their faded copies', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x, node.y);
  const ids = await page.evaluate(() => {
    const L = window.SankeyApp.layout;
    const n = L.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Remission');
    return { node: n.id, flow: n.inFlows[0], other: L.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Stable').id };
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
    const n = L.nodes.find((x) => window.SankeyApp.state.loc[x.id].label === 'Surgery');
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
  // Ctrl traces the pathway (a unique trajectory) under the pointer, Alt the single patient.
  await page.keyboard.down('Control');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText('One pathway');
  await page.keyboard.up('Control');
  await page.keyboard.down('Alt');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText(/^P\d{3}$/);
  await expect(page.locator('#hover-info')).toContainText(/patients? on this pathway/);
  const node = await nodeAt(page, 'Remission');
  await page.mouse.move(node.x - 10, node.y);
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText(/^P\d{3}$/);
  await page.keyboard.up('Alt');
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

test('single-value filters hide their diagram column, restore it and allow a manual override', async ({ page }) => {
  await importCohortTable(page);
  await badge(page, 'sex').click();
  await keepOnly(page, 'sex', 'Female');
  await expect(badge(page, 'sex')).toHaveAttribute('aria-pressed', 'false');
  expect((await state(page)).setNames).toEqual(['TR00', 'TR01', 'TR02']);
  expect((await state(page)).sourceTable.summary.total).toBe(17);
  await page.locator('[data-filter-column="sex"] [data-keep-all]').click();
  await expect(badge(page, 'sex')).toHaveAttribute('aria-pressed', 'true');
  expect((await state(page)).setNames).toEqual(['TR00', 'TR01', 'TR02', 'sex']);
  await keepOnly(page, 'sex', 'Female');
  await badge(page, 'sex').click();
  await expect(badge(page, 'sex')).toHaveAttribute('aria-pressed', 'true');
  await badge(page, 'sex').click();
  await page.locator('[data-clear-filters]').click();
  await expect(badge(page, 'sex')).toHaveAttribute('aria-pressed', 'false');
  await page.locator('#undo').click();
  expect((await state(page)).sourceTable.filters).toEqual({ sex: ['Female'] });
});

test('strip badges reorder columns by dragging, with one place per gap, and with Alt+arrows', async ({ page }) => {
  await importCohortTable(page);
  await expect(page.locator('#table-controls [data-table-column]')).toHaveCount(0);
  // Badges sit over their columns, so show the whole drawing.
  await page.locator('#fit-panel').click();
  // Each shown column has a zone.
  await expect(page.locator('#column-strip .strip-zone')).toHaveCount(3);
  // Sweeping a dragged badge across the strip passes one place per gap between the other columns
  // (its own place among them lights its zone); dropped there, nothing changes.
  const home = await badge(page, 'TR00').boundingBox();
  await holdBadge(page, 'TR00', { x: home.x + home.width / 2 + 8, y: await stripRowY(page, 'in') });
  const strip = await page.locator('#column-strip').boundingBox();
  const places = new Set();
  for (let x = strip.x + 4; x < strip.x + strip.width - 4; x += 12) {
    await page.mouse.move(x, await stripRowY(page, 'in'));
    places.add(await page.locator('#column-strip .strip-marker').evaluate((el) => `${el.classList.contains('home')} ${el.style.left}`));
  }
  await page.mouse.move(home.x + home.width / 2 + 8, await stripRowY(page, 'in'));
  await expect(page.locator('#column-strip .strip-marker.home')).toBeVisible();
  await page.mouse.up();
  expect(places.size).toBe(3);
  expect((await state(page)).setNames).toEqual(['TR00', 'TR01', 'TR02']);
  await dragBadge(page, 'TR02', await leftOfBadge(page, 'TR00'));
  expect((await state(page)).setNames).toEqual(['TR02', 'TR00', 'TR01']);
  // A column not shown, dragged down, comes into the diagram where dropped.
  await dragBadge(page, 'sex', await betweenBadges(page, 'TR02', 'TR00', 'in'));
  expect((await state(page)).setNames).toEqual(['TR02', 'sex', 'TR00', 'TR01']);
  await badge(page, 'sex').focus();
  await page.keyboard.press('Alt+ArrowLeft');
  expect((await state(page)).setNames).toEqual(['sex', 'TR02', 'TR00', 'TR01']);
  await expect(badge(page, 'sex')).toBeFocused();
  await page.locator('#undo').click();
  expect((await state(page)).setNames).toEqual(['TR02', 'sex', 'TR00', 'TR01']);
  // Dragged up, a shown column is hidden, listed left-justified after "Not shown:"; dragged down
  // from there, it comes back where dropped.
  await dragBadge(page, 'TR01', { x: (await badge(page, 'TR01').boundingBox()).x + 10, y: await stripRowY(page, 'out') });
  expect((await state(page)).setNames).toEqual(['TR02', 'sex', 'TR00']);
  const label = await page.locator('#column-strip .strip-out-label').boundingBox();
  await expect(page.locator('#column-strip .strip-out-label')).toHaveText('Not shown:');
  expect((await badge(page, 'TR01').boundingBox()).x - (label.x + label.width)).toBeLessThan(10);
  await dragBadge(page, 'TR01', await leftOfBadge(page, 'TR02'));
  expect((await state(page)).setNames).toEqual(['TR01', 'TR02', 'sex', 'TR00']);
});

test('the arrange strip gives each node column a zone, with its badge and values left-justified in it', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const columns = () => page.evaluate(() => {
    const app = window.SankeyApp;
    const plot = document.querySelector('#plot');
    const box = plot.getBoundingClientRect();
    const k = box.width / Number(plot.getAttribute('width'));
    const source = app.state.sourceTable;
    const shown = source.order.filter((c) => source.enabled.includes(c));
    const centres = shown.map((c, s) => {
      const nodes = app.layout.nodes.filter((n) => n.set === s + 1);
      return box.left + (18 + nodes.reduce((t, n) => t + n.X, 0) / nodes.length) * k;
    });
    return shown.map((c, s) => {
      const nodes = app.layout.nodes.filter((n) => n.set === s + 1).sort((a, b) => b.Y - a.Y);
      const zone = document.querySelector(`.strip-zone[data-strip-zone="${c}"]`).getBoundingClientRect();
      const b = document.querySelector(`.strip-badge[data-strip-column="${c}"]`).getBoundingClientRect();
      const list = document.querySelector(`[data-strip-values="${c}"]`).getBoundingClientRect();
      const chips = [...document.querySelector(`[data-strip-values="${c}"]`).children];
      // Zones meet (3 px of white each side) halfway between neighbouring columns' centres.
      const ends = [s > 0 ? Math.abs(zone.left - 3 - (centres[s - 1] + centres[s]) / 2) : 0,
        s < shown.length - 1 ? Math.abs(zone.right + 3 - (centres[s] + centres[s + 1]) / 2) : 0];
      return { off: Math.max(...ends), inside: zone.left < centres[s] && centres[s] < zone.right,
        badgeIn: b.left - zone.left, listIn: list.left - b.left, chips: chips.map((el) => el.dataset.stripValue),
        colored: chips.filter((el) => el.style.getPropertyValue('--node')).length, nodes: nodes.map((n) => app.state.loc[n.id].label),
        counts: chips.map((el) => el.querySelector('.strip-count').textContent),
        totals: nodes.map((n) => Math.max(n.inValue, n.outValue).toLocaleString()) };
    });
  });
  const check = async () => {
    const cols = await columns();
    expect(cols.map((c) => c.chips)).toEqual(cols.map((c) => c.nodes));
    // Each value is followed by its node's count.
    expect(cols.map((c) => c.counts)).toEqual(cols.map((c) => c.totals));
    cols.forEach((c) => {
      expect(c.off).toBeLessThan(1);
      expect(c.inside).toBe(true);
      expect(c.badgeIn).toBeCloseTo(6, 0);
      expect(c.listIn).toBeCloseTo(10, 0);
      expect(c.colored).toBe(c.chips.length);
    });
  };
  await check();
  expect((await columns()).map((c) => c.nodes[0])).toEqual(['Stage I', 'Surgery', 'Remission']);
  // The zones follow their columns when the plot zooms or pans.
  await page.locator('#zoom-in').click();
  await check();
  await page.locator('#stage').evaluate((el) => { el.scrollLeft = 60; });
  await expect.poll(async () => Math.max(...(await columns()).map((c) => c.off))).toBeLessThan(1);
});

test('strip badges and zones take their columns\' colors, and grey gaps set the canvas off', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const colors = await page.evaluate(() => [...document.querySelectorAll('#column-strip .strip-in .strip-badge')].map((b) => ({
    badge: b.style.getPropertyValue('--col'), background: getComputedStyle(b).backgroundColor, colored: b.classList.contains('colored') })));
  const zones = await page.locator('#column-strip .strip-zone').evaluateAll((els) => els.map((el) => el.style.getPropertyValue('--col')));
  expect(colors.every((c) => c.colored && /^#[0-9A-F]{6}$/.test(c.badge))).toBe(true);
  expect(new Set(colors.map((c) => c.badge)).size).toBe(3);
  expect(zones).toEqual(colors.map((c) => c.badge));
  // The badge is filled with its column's color.
  const rgb = (hex) => `rgb(${[1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16)).join(', ')})`;
  expect(colors.map((c) => c.background)).toEqual(colors.map((c) => rgb(c.badge)));
  // White text, on a color dark enough for it (contrast above 3).
  const contrast = await page.evaluate((hexes) => hexes.map((hex) => 1.05 / (window.SK.color.luminance(window.SK.color.parse(hex)) + 0.05)), colors.map((c) => c.badge));
  expect(contrast.every((c) => c > 3)).toBe(true);
  expect(await page.locator('#column-strip .strip-in .strip-badge').evaluateAll((els) => [...new Set(els.map((el) => getComputedStyle(el).color))])).toEqual(['rgb(255, 255, 255)']);
  // White space between neighbouring zones, each dashed at both edges, and along the strip's foot.
  const spans = await page.locator('#column-strip .strip-zone').evaluateAll((els) => els.map((el) => el.getBoundingClientRect()).map((r) => [r.left, r.right]));
  spans.slice(1).forEach(([left], i) => expect(left - spans[i][1]).toBeGreaterThanOrEqual(5));
  expect(await page.locator('#column-strip .strip-zone').evaluateAll((els) => [...new Set(els.map((el) => {
    const cs = getComputedStyle(el);
    return `${cs.borderLeftStyle} ${cs.borderRightStyle}`;
  }))])).toEqual(['dashed dashed']);
  const foot = await page.locator('#column-strip .strip-in').evaluate((row) => row.getBoundingClientRect().bottom -
    Math.max(...[...row.querySelectorAll('.strip-zone')].map((el) => el.getBoundingClientRect().bottom)));
  expect(foot).toBeGreaterThanOrEqual(5);
  const [strip, stage, plot] = [await page.locator('#column-strip').boundingBox(), await page.locator('#stage').boundingBox(), await page.locator('#plot').boundingBox()];
  expect(plot.y - (strip.y + strip.height)).toBeGreaterThanOrEqual(6);
  expect(plot.x - stage.x).toBeGreaterThanOrEqual(6);
  // The canvas still fills the rest of the panel without scrolling.
  expect(await page.locator('#stage').evaluate((el) => el.scrollWidth <= el.clientWidth && el.scrollHeight <= el.clientHeight)).toBe(true);
});

test('a column scrolled off the strip takes its values and zone with it', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  for (let i = 0; i < 3; i++) await page.locator('#zoom-in').click();
  await page.locator('#stage').evaluate((el) => { el.scrollLeft = 420; });
  const strip = await page.locator('#column-strip').boundingBox();
  await expect.poll(async () => (await badge(page, 'diagnosis').boundingBox()).x + (await badge(page, 'diagnosis').boundingBox()).width).toBeLessThan(strip.x);
  const list = await page.locator('[data-strip-values="diagnosis"]').boundingBox();
  expect(list.x + list.width).toBeLessThan(strip.x);
  await expect(badge(page, 'treatment')).toBeInViewport();
  // A zone partly off the left end keeps its badge and values in its visible part.
  const zone = await page.locator('#column-strip .strip-zone[data-strip-zone="treatment"]').boundingBox();
  await page.locator('#stage').evaluate((el, by) => { el.scrollLeft += by; }, zone.x - strip.x + 60);
  await expect.poll(async () => (await page.locator('#column-strip .strip-zone[data-strip-zone="treatment"]').boundingBox()).x).toBeLessThan(strip.x - 40);
  const [b, l] = [await badge(page, 'treatment').boundingBox(), await page.locator('[data-strip-values="treatment"]').boundingBox()];
  expect(b.x).toBeGreaterThanOrEqual(strip.x);
  expect(b.x).toBeLessThan(strip.x + 12);
  expect(l.x - b.x).toBeCloseTo(10, 0);
});

test('lit nodes glow as one shape each, with no seam between an end node\'s parts', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await badge(page, 'diagnosis').hover();
  // End nodes are drawn in two parts; one masked copy of all of them carries the glow.
  const glow = await page.evaluate(() => {
    const trunks = [...document.querySelectorAll('#plot .trunk.column-focus')];
    const shape = document.querySelector('#plot .column-glow .column-glow-shape');
    return { trunks: trunks.length, nodes: new Set(trunks.map((el) => el.dataset.node)).size,
      filters: [...new Set(trunks.map((el) => getComputedStyle(el).filter))],
      shape: shape && trunks.every((el) => shape.getAttribute('d').includes(el.getAttribute('d'))),
      shadow: shape && getComputedStyle(shape).filter.includes('drop-shadow'),
      masked: !!document.querySelector('#plot .column-glow[mask]') };
  });
  expect(glow.trunks).toBeGreaterThan(glow.nodes);
  expect(glow).toMatchObject({ nodes: 4, filters: ['none'], shape: true, shadow: true, masked: true });
  await page.mouse.move(5, 500);
  await expect(page.locator('#plot .column-glow')).toHaveCount(0);
});

test('hovering a strip badge lights its column of nodes under a beam, and a value its node', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  await badge(page, 'treatment').hover();
  await expect(page.locator('#plot .trunk.column-focus')).toHaveCount(4);
  await expect(page.locator('#plot .node-label.column-focus')).toHaveCount(4);
  expect(await page.locator('#plot .node-label.column-focus').first().evaluate((el) => getComputedStyle(el).fontWeight)).toBe('700');
  await expect(page.locator('#column-beam')).toBeVisible();
  // The beam starts as wide as the column's zone and narrows to the lit nodes' drawn bodies, which
  // lie wholly inside it, without overshooting them (labels of end nodes reach past their nodes).
  const inBeam = (column) => page.evaluate((c) => {
    const beam = document.querySelector('#column-beam');
    const box = beam.getBoundingClientRect();
    const points = beam.style.clipPath.match(/polygon\((.*)\)/)[1].split(',').map((point) => point.trim().split(/\s+/)
      .map((v, i) => (v.endsWith('%') ? parseFloat(v) / 100 * (i ? box.height : box.width) : parseFloat(v)) + (i ? box.top : box.left)));
    const inside = (x, y) => points.reduce((hit, [xi, yi], i) => {
      const [xj, yj] = points[(i + points.length - 1) % points.length];
      return (yi > y) !== (yj > y) && x < (xj - xi) * (y - yi) / (yj - yi) + xi ? !hit : hit;
    }, false);
    const trunks = [...document.querySelectorAll('#plot .trunk.column-focus')].map((el) => el.getBoundingClientRect());
    const zone = document.querySelector(`#column-strip .strip-zone[data-strip-zone="${c}"]`).getBoundingClientRect();
    const top = points.filter(([, y]) => Math.abs(y - box.top) < 0.5).map(([x]) => x);
    const foot = points.filter(([, y]) => Math.abs(y - box.bottom) < 0.5).map(([x]) => x);
    // Corners half a pixel in: the beam's edges run along the nodes' own.
    return { within: trunks.every((r) => [[r.left + 0.5, r.top + 0.5], [r.right - 0.5, r.top + 0.5], [r.left + 0.5, r.bottom - 0.5],
      [r.right - 0.5, r.bottom - 0.5]].every(([x, y]) => inside(x, y))),
      top: [Math.min(...top) - zone.left, Math.max(...top) - zone.right],
      foot: [Math.min(...foot) - Math.min(...trunks.map((r) => r.left)), Math.max(...foot) - Math.max(...trunks.map((r) => r.right))] };
  }, column);
  const fits = (b) => {
    expect(b.within).toBe(true);
    b.top.concat(b.foot).forEach((d) => expect(Math.abs(d)).toBeLessThan(1));
  };
  fits(await inBeam('treatment'));
  await badge(page, 'response').hover();
  await expect(page.locator('#plot .node-label.column-focus')).toHaveCount(3);
  fits(await inBeam('response'));
  await page.locator('[data-strip-values="treatment"] [data-strip-value="Surgery"]').hover();
  await expect(page.locator('#plot .trunk.column-focus')).toHaveCount(1);
  expect(await page.locator('#plot .trunk.column-focus').getAttribute('data-node'))
    .toBe(String(await page.evaluate(() => window.SankeyApp.state.loc.findIndex((r) => r.label === 'Surgery'))));
  fits(await inBeam('treatment'));
  await page.mouse.move(5, 500);
  await expect(page.locator('#plot .column-focus')).toHaveCount(0);
  await expect(page.locator('#column-beam')).toBeHidden();
});

test('clicking a strip value drops its rows and greys it out, and clicking it again keeps them', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const chip = page.locator('[data-strip-values="treatment"] [data-strip-value="Surgery"]');
  expect((await state(page)).sourceTable.summary.total).toBe(560);
  await chip.click();
  const s = await state(page);
  expect(s.sourceTable.filters).toEqual({ treatment: ['Radiation', 'Chemotherapy', 'Palliative'] });
  expect(s.sourceTable.summary.total).toBe(560 - 232);
  expect(s.setNames).toEqual(['diagnosis', 'treatment', 'response']);
  await expect(chip).toHaveAttribute('aria-pressed', 'false');
  await expect(chip).toHaveClass(/off/);
  // The dropped value keeps its place in the list, and the other nodes theirs.
  const listed = () => page.locator('[data-strip-values="treatment"] .strip-value').evaluateAll((els) => els.map((el) => el.dataset.stripValue));
  expect(await listed()).toEqual(['Surgery', 'Radiation', 'Chemotherapy', 'Palliative']);
  expect(s.dimnames[0].cols).toEqual(['Radiation', 'Chemotherapy', 'Palliative']);
  await expect(badge(page, 'treatment')).toContainText('3/4');
  await expect(page.locator('#plot .layer-subset')).toHaveText('treatment ∈ {Radiation, Chemotherapy, Palliative}');
  await expect(page.locator('#plot .node-label', { hasText: 'Surgery' })).toHaveCount(0);
  await expect(page.locator('[data-filter-column="treatment"] summary')).toContainText('3 kept');
  await chip.click();
  expect((await state(page)).sourceTable.filters).toEqual({});
  await expect(chip).toHaveAttribute('aria-pressed', 'true');
  await expect(page.locator('#plot .layer-subset')).toHaveCount(0);
  // Keeping a single value leaves its column in the diagram.
  await page.locator('[data-strip-values="response"] [data-strip-value="Stable"]').click();
  await page.locator('[data-strip-values="response"] [data-strip-value="Progression"]').click();
  expect((await state(page)).sourceTable.filters).toEqual({ response: ['Remission'] });
  expect((await state(page)).setNames).toEqual(['diagnosis', 'treatment', 'response']);
  await expect(badge(page, 'response')).toHaveAttribute('aria-pressed', 'true');
});

test('dragging a strip value up or down moves its node within the column', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const chip = (v) => page.locator(`[data-strip-values="treatment"] [data-strip-value="${v}"]`);
  const listed = () => page.locator('[data-strip-values="treatment"] .strip-value').evaluateAll((els) => els.map((el) => el.dataset.stripValue));
  expect(await listed()).toEqual(['Surgery', 'Radiation', 'Chemotherapy', 'Palliative']);
  const [from, to] = [await chip('Palliative').boundingBox(), await chip('Surgery').boundingBox()];
  await page.mouse.move(from.x + 20, from.y + from.height / 2);
  await page.mouse.down();
  await page.mouse.move(from.x + 20, to.y + 3, { steps: 8 });
  await expect(page.locator('#column-strip .strip-value-marker')).toBeVisible();
  await page.mouse.up();
  const s = await state(page);
  expect(s.sourceTable.valueOrders).toEqual({ treatment: ['Palliative', 'Surgery', 'Radiation', 'Chemotherapy'] });
  expect(s.sourceTable.filters).toEqual({});
  expect(s.dimnames[0].cols).toEqual(['Palliative', 'Surgery', 'Radiation', 'Chemotherapy']);
  const fromTop = await page.evaluate(() => {
    const app = window.SankeyApp;
    return app.layout.nodes.filter((n) => n.set === 2).sort((a, b) => b.Y - a.Y).map((n) => app.state.loc[n.id].label);
  });
  expect(fromTop).toEqual(['Palliative', 'Surgery', 'Radiation', 'Chemotherapy']);
  expect(await listed()).toEqual(fromTop);
  await page.locator('#undo').click();
  expect(await listed()).toEqual(['Surgery', 'Radiation', 'Chemotherapy', 'Palliative']);
});

test('strip values show in full where the columns leave room', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const values = [['Sedentary', 'Light activity', 'Moderate-to-vigorous'], ['Endurance training', 'Resistance training', 'Combined program'], ['Weekly', 'Daily']];
    const rows = Array.from({ length: 30 }, (_, i) => [`p${i}`, ...values.map((v, k) => v[(i * (k + 1) + k) % v.length])]);
    window.SankeyApp.applyImport(T.build({ columns: ['id', 'baseline', 'program', 'frequency'], rows },
      { format: 'wide', id: 'id', columns: ['baseline', 'program', 'frequency'] }), { labels: 'above' });
  });
  // Each value's text (a range spans it in full, ellipsis or not) fits inside its chip.
  const cut = await page.locator('#column-strip .strip-chip').evaluateAll((chips) => chips.filter((chip) => {
    const range = document.createRange();
    range.selectNodeContents(chip);
    const cs = getComputedStyle(chip);
    const inner = chip.getBoundingClientRect().width - ['paddingLeft', 'paddingRight', 'borderLeftWidth', 'borderRightWidth'].reduce((t, k) => t + parseFloat(cs[k]), 0);
    return range.getBoundingClientRect().width > inner + 0.01;
  }).map((chip) => chip.textContent));
  expect(cut).toEqual([]);
});

test('a column of many nodes gets a canvas tall enough that labels clear the nodes above them', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const assays = Array.from({ length: 21 }, (_, i) => `Assay with a long name ${i + 1}`);
    const rows = assays.flatMap((a, i) => Array.from({ length: 18 }, (_, j) => [`p${i}-${j}`, ['A', 'B', 'C'][j % 3], a, ['X', 'Y', 'Z', 'W'][(i + j) % 4]]));
    window.SankeyApp.applyImport(T.build({ columns: ['id', 'group', 'assay', 'out'], rows }, { format: 'wide', id: 'id', columns: ['group', 'assay', 'out'] }),
      { labels: 'above' });
  });
  // Each label of the 21-node middle column against the other nodes' bodies there.
  const clashes = () => page.evaluate(() => {
    const app = window.SankeyApp;
    const plot = document.querySelector('#plot');
    const box = plot.getBoundingClientRect();
    const k = box.width / Number(plot.getAttribute('width'));
    const h = Number(plot.getAttribute('height')) - 36;
    const body = (n) => {
      const half = Math.max(n.inPx, n.outPx) / 2;
      return { left: box.left + (18 + n.X - n.widthPx / 2) * k, right: box.left + (18 + n.X + n.widthPx / 2) * k,
        top: box.top + (18 + h - n.Y - half) * k, bottom: box.top + (18 + h - n.Y + half) * k };
    };
    const meet = (a, b) => a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom;
    const nodes = app.layout.nodes.filter((n) => n.set === 2);
    return nodes.flatMap((l) => nodes.filter((u) => u !== l && meet(document.querySelector(`#plot .node-label[data-node="${l.id}"]`).getBoundingClientRect(), body(u)))
      .map((u) => `${app.state.loc[l.id].label} over ${app.state.loc[u.id].label}`));
  });
  expect(await clashes()).toEqual([]);
  // Thicker strands shrink the gaps between the nodes; the canvas grows to keep the labels clear.
  const before = Number(await page.locator('#plot').getAttribute('height'));
  await page.evaluate(() => {
    const app = window.SankeyApp;
    app.state.valueScale = app.state.valueScaleRef * 2;
    app.update({ commit: true });
  });
  expect(await clashes()).toEqual([]);
  expect(Number(await page.locator('#plot').getAttribute('height'))).toBeGreaterThan(before);
  // A fixed canvas height is a minimum: it grows too.
  await page.evaluate(() => {
    const app = window.SankeyApp;
    app.state.style.canvasHeight = 1400;
    app.update({ commit: true });
  });
  expect(await clashes()).toEqual([]);
  expect(Number(await page.locator('#plot').getAttribute('height'))).toBeGreaterThan(1400);
});

test('a value left without rows by another column\'s filter keeps its place in the strip', async ({ page }) => {
  await importCohortTable(page);
  const listed = (c) => page.locator(`[data-strip-values="${c}"] .strip-value`).evaluateAll((els) => els.map((el) => el.dataset.stripValue));
  const first = await listed('TR02');
  expect(first).toEqual(['Z', 'W']);
  // Only patients P3 and P4 have TR00 = B, and both have TR02 = W.
  await page.locator('[data-strip-values="TR00"] [data-strip-value="A"]').click();
  await expect(page.locator('[data-strip-values="TR02"] [data-strip-value="Z"]')).toHaveClass(/absent/);
  expect(await listed('TR02')).toEqual(first);
  await page.locator('[data-strip-values="TR00"] [data-strip-value="A"]').click();
  await expect(page.locator('[data-strip-values="TR02"] [data-strip-value="Z"]')).not.toHaveClass(/absent/);
  expect(await listed('TR02')).toEqual(first);
});

test('long value lists flow down numbered sub-columns instead of scrolling, and long values are cut short', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const many = Array.from({ length: 14 }, (_, i) => (i === 0 ? 'A value whose name is much too long to show in full in its badge' : `Site ${i + 1}`));
    const rows = Array.from({ length: 140 }, (_, i) => [`p${i}`, 'xy'[i % 2], many[i % 14], 'uvw'[i % 3]]);
    window.SankeyApp.applyImport(T.build({ columns: ['id', 'arm', 'site', 'end'], rows }, { format: 'wide', id: 'id', columns: ['arm', 'site', 'end'] }),
      { labels: 'above' });
  });
  const list = page.locator('[data-strip-values="site"]');
  await expect(list).toHaveClass(/multi/);
  const chips = await list.locator('.strip-value').evaluateAll((els) => els.map((el) => {
    const r = el.getBoundingClientRect();
    return { index: el.querySelector('.strip-index').textContent, left: r.left, top: r.top, value: el.dataset.stripValue, title: el.title,
      chip: el.querySelector('.strip-chip').getBoundingClientRect().width };
  }));
  // 14 values over at most six rows: three sub-columns of five, numbered down each in turn.
  expect(chips.map((c) => c.index)).toEqual(Array.from({ length: 14 }, (_, i) => String(i + 1)));
  expect(chips[5].left).toBeGreaterThan(chips[0].left + 20);
  expect(chips[5].top).toBeCloseTo(chips[0].top, 0);
  expect(chips[10].left).toBeGreaterThan(chips[5].left + 20);
  expect(await list.evaluate((el) => el.scrollHeight <= el.clientHeight + 1)).toBe(true);
  const zone = await page.locator('#column-strip .strip-zone[data-strip-zone="site"]').boundingBox();
  const box = await list.boundingBox();
  expect(box.x + box.width).toBeLessThanOrEqual(zone.x + zone.width);
  // Short lists stay in one unnumbered column.
  await expect(page.locator('[data-strip-values="end"]')).not.toHaveClass(/multi/);
  await expect(page.locator('[data-strip-values="end"] .strip-index').first()).toBeHidden();
  // A long value is cut short, in full in its tooltip and while dragged.
  const long = chips.find((c) => c.value.startsWith('A value'));
  expect(long.chip).toBeLessThan(200);
  expect(long.title).toContain(long.value);
  const from = await list.locator('.strip-value').filter({ hasText: 'A value' }).boundingBox();
  await page.mouse.move(from.x + 20, from.y + from.height / 2);
  await page.mouse.down();
  await page.mouse.move(from.x + 24, from.y + from.height / 2 + 6, { steps: 4 });
  const ghost = await page.locator('#column-strip .strip-ghost .strip-chip').evaluate((el) => el.scrollWidth <= el.clientWidth + 1);
  expect(ghost).toBe(true);
  await page.mouse.up();
  // Dragged across sub-columns, a value lands before the one it is dropped on.
  const target = await list.locator('.strip-value').nth(0).boundingBox();
  const moved = await list.locator('.strip-value').nth(7).boundingBox();
  const name = await list.locator('.strip-value').nth(7).getAttribute('data-strip-value');
  await page.mouse.move(moved.x + 20, moved.y + moved.height / 2);
  await page.mouse.down();
  await page.mouse.move(target.x + 20, target.y + 3, { steps: 8 });
  await page.mouse.up();
  expect((await state(page)).sourceTable.valueOrders.site[0]).toBe(name);
});

test('the strip is as tall as the longest value list of a shown column', async ({ page }) => {
  await page.evaluate(() => {
    const T = window.SK.transform;
    const rows = Array.from({ length: 10 }, (_, i) => [`p${i}`, 'abcde'[i % 5], 'xy'[i % 2], 'uv'[(i >> 1) % 2], 'mn'[(i >> 2) % 2]]);
    window.SankeyApp.applyImport(T.build({ columns: ['id', 'five', 'b', 'c', 'd'], rows }, { format: 'wide', id: 'id', columns: ['five', 'b', 'c', 'd'] }),
      { labels: 'above' });
  });
  const rows = async () => ((await page.locator('#column-strip .strip-in').boundingBox()).height - 46) / 20;
  expect(await rows()).toBe(5);
  const panel = (await page.locator('#stage').boundingBox()).height;
  await badge(page, 'five').click();
  expect(await rows()).toBe(2);
  expect((await page.locator('#stage').boundingBox()).height).toBe(panel + 60);
});

test('a long table\'s values are kept or dropped per stage', async ({ page }) => {
  await page.locator('.topbar [data-open-import]').click();
  await page.locator('#import-text').fill('id,visit,status,sex\nA,1,A,Female\nA,2,B,Female\nA,3,C,Female\nB,1,A,Male\nB,2,B,Male\nB,3,D,Male\nC,1,B,Female\nC,3,C,Female\n');
  await page.locator('#import-apply').click();
  expect((await state(page)).sourceTable.summary.total).toBe(2);
  await page.locator('[data-strip-values="3"] [data-strip-value="D"]').click();
  const s = await state(page);
  expect(s.sourceTable.stageFilters).toEqual({ 3: ['C'] });
  expect(s.sourceTable.filters).toEqual({});
  expect(s.sourceTable.summary.total).toBe(1);
  expect(s.paths.flatMap((p) => p.members.map((m) => m.id))).toEqual(['A']);
  await expect(page.locator('#plot .layer-subset')).toHaveText('status at visit 3 ∈ {C}');
  // The same category at another stage is a different value.
  await expect(page.locator('[data-strip-values="1"] [data-strip-value="A"]')).toHaveAttribute('aria-pressed', 'true');
  await page.locator('[data-clear-filters]').click();
  expect((await state(page)).sourceTable.stageFilters).toEqual({});
  expect((await state(page)).sourceTable.summary.total).toBe(2);
});

test('the strip\'s tab hides and shows it, the choice survives a reload, and other data has no strip', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const height = async () => (await page.locator('#stage').boundingBox()).height;
  const open = await height();
  const tab = page.locator('#column-strip .strip-tab');
  // The tab hangs at the strip's right end, beside the inspector.
  await expect(tab).toHaveText(/Arrange/);
  const [t, strip] = [await tab.boundingBox(), await page.locator('#column-strip').boundingBox()];
  expect(strip.x + strip.width - (t.x + t.width)).toBeLessThan(20);
  await tab.click();
  await expect(page.locator('#column-strip .strip-body')).toBeHidden();
  await expect(tab).toHaveAttribute('aria-expanded', 'false');
  expect(await height()).toBeGreaterThan(open + 60);
  await expect.poll(async () => (await page.locator('#plot').boundingBox()).height).toBeGreaterThan(open + 60);
  await page.waitForTimeout(700);
  await page.reload();
  await expect(page.locator('#column-strip .strip-body')).toBeHidden();
  await tab.click();
  await expect(badge(page, 'treatment')).toBeVisible();
  await page.evaluate(() => window.SankeyApp.loadExample('labeled'));
  await expect(page.locator('#column-strip')).toBeHidden();
});

const importManyPaths = (page) => page.evaluate(() => {
  const T = window.SK.transform;
  const columns = ['source', ...Array.from({ length: 8 }, (_, i) => `s${i}`), 'end'];
  const rows = Array.from({ length: 201 }, (_, i) => ['A', ...Array.from({ length: 8 }, (_, b) => String((i >> b) & 1)), 'Z']);
  window.SankeyApp.applyImport(T.build({ columns, rows }, { format: 'wide', columns }), { labels: 'above' });
});

const screenPoint = (page, point) => page.evaluate(({ X, Y }) => {
  const plot = document.querySelector('#plot');
  const app = window.SankeyApp;
  // The bottom margin remains 18, including when a subset caption reserves extra top margin.
  const p = new DOMPoint(X + 18, Number(plot.getAttribute('height')) - 18 - Y).matrixTransform(plot.getScreenCTM());
  return { x: p.x, y: p.y };
}, point);

test('large pathway diagrams suppress ordinary hover but retain modifier hover and a configurable limit', async ({ page }) => {
  await importManyPaths(page);
  await page.locator('#fit-panel').click();
  const point = await page.evaluate(() => {
    const f = window.SankeyApp.layout.flows[0];
    return { X: (f.startX + f.endX) / 2, Y: window.SK.layout.pointAtX(f.centerline, (f.startX + f.endX) / 2)[1] };
  });
  const p = await screenPoint(page, point);
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot')).not.toHaveClass(/dimmed/);
  await expect(page.locator('#plot .trace')).toHaveCount(0);
  await expect(page.locator('#plot .node-fill')).toHaveCount(0);
  await expect(page.locator('#hover-info')).toBeHidden();
  await expect(page.locator('#inspector-main')).toContainText('201 pathways (limit 200)');
  // Alt traces one item (here, without item ids, one pathway); Shift above the limit one pathway.
  for (const key of ['Alt', 'Shift']) {
    await page.keyboard.down(key);
    await expect(page.locator('#plot .trace')).toHaveCount(1);
    await expect(page.locator('#hover-info h3')).toHaveText('One pathway');
    await page.keyboard.up(key);
  }
  await expect(page.locator('#plot .trace')).toHaveCount(0);
  await expect(page.locator('#plot')).not.toHaveClass(/dimmed/);
  await page.keyboard.down('Control');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await page.keyboard.up('Control');
  await page.locator('#ctl-maxHoverSubstrands').fill('201');
  await page.locator('#ctl-maxHoverSubstrands').dispatchEvent('change');
  await page.mouse.move(p.x, p.y);
  expect(await page.locator('#plot .trace').count()).toBeGreaterThan(1);
  await page.mouse.move(5, 5);
  const saved = await page.evaluate(() => JSON.parse(window.SankeyApp.bundleFiles().find(f => f.name === 'project.json').text));
  expect(saved.style.maxHoverSubstrands).toBe(201);
  await page.locator('#undo').click();
  await expect(page.locator('#ctl-maxHoverSubstrands')).toHaveValue('200');
});

test('panel and height fits and zoom scale the entire drawing uniformly and persist', async ({ page }) => {
  await importManyPaths(page);
  const snapshot = await state(page);
  const metrics = () => page.evaluate(() => {
    const plot = document.querySelector('#plot');
    const box = plot.getBoundingClientRect();
    const label = plot.querySelector('.node-label').getBoundingClientRect();
    const panel = document.querySelector('#stage');
    const cs = getComputedStyle(panel);
    return { width: box.width, height: box.height, labelWidth: label.width, labelHeight: label.height,
      panelWidth: panel.clientWidth, panelHeight: panel.clientHeight, scrollWidth: panel.scrollWidth, scrollHeight: panel.scrollHeight,
      padding: parseFloat(cs.paddingTop) + parseFloat(cs.paddingBottom) };
  });
  await page.locator('#fit-panel').click();
  const fit = await metrics();
  expect(fit.width).toBeLessThanOrEqual(fit.panelWidth);
  expect(fit.height).toBeLessThanOrEqual(fit.panelHeight);
  await page.locator('#zoom-in').click();
  const zoom = await metrics();
  expect(zoom.width / fit.width).toBeCloseTo(1.25, 2);
  expect(zoom.height / fit.height).toBeCloseTo(1.25, 2);
  expect(zoom.labelWidth / fit.labelWidth).toBeCloseTo(1.25, 2);
  expect(zoom.labelHeight / fit.labelHeight).toBeCloseTo(1.25, 2);
  await page.locator('#zoom-out').click();
  expect((await metrics()).width).toBeCloseTo(fit.width, 1);
  await page.locator('#fit-height').click();
  const height = await metrics();
  expect(height.height).toBeLessThanOrEqual(height.panelHeight);
  // The canvas fills the panel inside its grey padding, but for a scroll bar.
  expect(height.panelHeight - height.padding - height.height).toBeLessThanOrEqual(18);
  expect(height.scrollWidth).toBeGreaterThan(height.panelWidth);
  const s = await state(page);
  expect(s.view).toEqual(snapshot.view);
  expect(s.loc).toEqual(snapshot.loc);
  expect(s.valueScale).toBe(snapshot.valueScale);
  const exported = await page.evaluate(() => {
    const doc = new DOMParser().parseFromString(window.SankeyApp.exportSvgText(), 'image/svg+xml');
    return { width: doc.documentElement.style.width, height: doc.documentElement.style.height, text: doc.documentElement.getAttribute('text-rendering') };
  });
  expect(exported).toEqual({ width: '', height: '', text: 'geometricPrecision' });
  await expect.poll(async () => page.evaluate(() => JSON.parse(localStorage.getItem('polylines-sankey-v1'))?.presentation?.mode)).toBe('height');
  await page.reload();
  await expect(page.locator('#fit-height')).toHaveAttribute('aria-pressed', 'true');
  expect((await state(page)).presentation).toEqual({ mode: 'height', zoom: 1 });
});

test('zoomed pointer coordinates and drags follow the displayed geometry', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  await page.locator('#fit-panel').click();
  await page.locator('#zoom-in').click();
  const point = await page.evaluate(() => {
    const app = window.SankeyApp;
    const pi = app.state.paths.findIndex(p => p.members.length >= 3);
    const subs = window.SK.paths.substrands(app.layout, app.state.paths);
    const trace = window.SK.paths.tracePolygon(app.layout, subs, pi, { member: 0, fadeLabels: false });
    const f = trace.flows[0];
    const X = (f.startX + f.endX) / 2;
    const Y = (window.SK.layout.pointAtX(trace.lower, X)[1] + window.SK.layout.pointAtX(trace.upper, X)[1]) / 2;
    return { X, Y, member: app.state.paths[pi].members[0].id };
  });
  const p = await screenPoint(page, point);
  await page.keyboard.down('Alt');
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#hover-info h3')).toHaveText(point.member);
  await page.keyboard.up('Alt');
  const node = await page.evaluate(() => {
    const n = window.SankeyApp.layout.nodes.find(n => n.set === 1);
    return { X: n.X, Y: n.Y, row: n.id, before: n.y };
  });
  const n = await screenPoint(page, node);
  const factor = await page.evaluate(() => {
    const plot = document.querySelector('#plot');
    return window.SankeyApp.layout.tf.sy * plot.getBoundingClientRect().height / Number(plot.getAttribute('height'));
  });
  await page.mouse.move(n.x, n.y);
  await page.keyboard.down('Shift');
  await page.mouse.down();
  await page.mouse.move(n.x, n.y - 30, { steps: 5 });
  await page.mouse.up();
  await page.keyboard.up('Shift');
  expect((await state(page)).loc[node.row].y - node.before).toBeCloseTo(30 / factor, 3);
});
