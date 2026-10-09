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
  await page.locator('[data-enable-column="TR01"]').uncheck();
  await page.locator('[data-table-column="TR02"] .column-grip').dragTo(page.locator('[data-table-column="TR00"]'), { targetPosition: { x: 30, y: 2 } });
  s = await state(page);
  expect(s.setNames).toEqual(['TR02', 'TR00']);
  expect(s.matrices).toEqual([[[11, 0], [0, 6]]]);
  expect(s.paths).toHaveLength(2);
  expect(s.sourceTable.table.rows).toHaveLength(5);
  await expect(page.locator('[data-enable-column="TR02"]')).toBeDisabled();
  await expect(page.locator('[data-enable-column="TR00"]')).toBeDisabled();
  await expect(page.locator('[data-enable-column="TR01"]')).toBeEnabled();
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
  await page.locator('[data-enable-column="TR01"]').check();
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
  await page.locator('[data-enable-column="2"]').uncheck();
  await page.locator('[data-table-column="3"] .column-grip').dragTo(page.locator('[data-table-column="1"]'), { targetPosition: { x: 30, y: 2 } });
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
  await page.locator('[data-enable-column="TR01"]').uncheck();
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
  await expect(page.locator('[data-enable-column="TR01"]')).not.toBeChecked();
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

test('Shift-hover annotations use the patient slice count, and pools retain that slice', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('cohort'));
  await page.locator('#ctl-tracePercents').check();
  const p = await pathwayPoint(page);
  await page.keyboard.down('Shift');
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#plot .trace-count')).toHaveText('1');
  expect(await page.locator('#plot .trace-percent').allTextContents()).toEqual(['1.6%', '1.4%']);
  const member = await page.locator('#plot .trace-count').getAttribute('data-member');
  expect(Number(member)).toBeGreaterThanOrEqual(0);
  await page.keyboard.down('Alt');
  await page.mouse.click(p.x, p.y);
  await page.keyboard.up('Alt');
  await page.keyboard.up('Shift');
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
  distance.forEach((d) => expect(d).toBeCloseTo(9, 1));
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
  const widths = await page.locator('#plot .plot-outline, #plot .substrand-lines').evaluateAll((els) => [...new Set(els.map(el => Number(el.getAttribute('stroke-width')) / (el.hasAttribute('mask') ? 2 : 1)))]);
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
      const url = URL.createObjectURL(new Blob([app.exportSvgText()], { type: 'image/svg+xml' }));
      const image = new Image();
      await new Promise((resolve, reject) => { image.onload = resolve; image.onerror = reject; image.src = url; });
      const canvas = document.createElement('canvas');
      canvas.width = image.width; canvas.height = image.height;
      const ctx = canvas.getContext('2d');
      ctx.drawImage(image, 0, 0);
      URL.revokeObjectURL(url);
      return [...ctx.getImageData(Math.round(X), Math.round(Y), 1, 1).data];
    };
    return { on: await pixel(true), off: await pixel(false) };
  });
  expect(pixels.off).toEqual([239, 142, 145, 255]);
  expect(pixels.on).toEqual(pixels.off);
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
        nodeBorder: 'none', labels: true, labelPosition: 'above', flatten: true, canvasWidth: 900, canvasHeight: 900 });
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

test('Alt-clicked nodes in one column merge into a renamed node, laid out as if imported that way', async ({ page }) => {
  await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
  const altClick = async (label) => {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Alt');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Alt');
  };
  const merge = page.locator('#merge-nodes');
  await expect(merge).toBeHidden();
  await altClick('Surgery');
  await altClick('Stage I');
  await expect(merge).toBeHidden(); // different columns
  await altClick('Stage I');
  await altClick('Radiation');
  await expect(merge).toBeVisible();
  await expect(merge).toHaveText('Merge 2 nodes');
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

test('nodes of matrix data can be picked and merged too, and merging can be cancelled', async ({ page }) => {
  const ids = await page.evaluate(() => window.SankeyApp.layout.nodes.filter((n) => n.set === 1).slice(0, 2).map((n) => window.SankeyApp.state.loc[n.id].label));
  for (const label of ids) {
    const n = await nodeAt(page, label);
    await page.mouse.move(n.x, n.y);
    await page.keyboard.down('Alt');
    await page.mouse.click(n.x, n.y);
    await page.keyboard.up('Alt');
  }
  await page.mouse.move(5, 5);
  await expect(page.locator('#inspector-main h2').first()).toHaveText('Picked nodes');
  await page.locator('#merge-nodes').click();
  await page.locator('#merge-cancel').click();
  expect(await page.evaluate(() => window.SankeyApp.state.matrices[0].length)).toBe(4);
  await page.locator('#merge-nodes').click();
  await page.locator('#merge-dialog button[value="ok"]').click();
  expect(await page.evaluate(() => window.SankeyApp.state.matrices[0].length)).toBe(3);
  await expect(page.locator('#plot .node-label', { hasText: `${ids[0]} + ${ids[1]}` })).toHaveCount(1);
});

for (const label of ['Remission', 'Progression']) {
  test(`redoing a merge clears pinned ${label} pathways from the previous data`, async ({ page }) => {
    await page.evaluate(() => window.SankeyApp.loadExample('pathways'));
    for (const name of ['Surgery', 'Radiation']) {
      const n = await nodeAt(page, name);
      await page.keyboard.down('Alt');
      await page.mouse.click(n.x, n.y);
      await page.keyboard.up('Alt');
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
  expect(svg).not.toContain('stroke-dasharray');
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

test('single-value filters hide their diagram column, restore it and allow a manual override', async ({ page }) => {
  await importCohortTable(page);
  await page.locator('[data-enable-column="sex"]').check();
  await keepOnly(page, 'sex', 'Female');
  await expect(page.locator('[data-enable-column="sex"]')).not.toBeChecked();
  expect((await state(page)).setNames).toEqual(['TR00', 'TR01', 'TR02']);
  expect((await state(page)).sourceTable.summary.total).toBe(17);
  await page.locator('[data-filter-column="sex"] [data-keep-all]').click();
  await expect(page.locator('[data-enable-column="sex"]')).toBeChecked();
  expect((await state(page)).setNames).toEqual(['TR00', 'TR01', 'TR02', 'sex']);
  await keepOnly(page, 'sex', 'Female');
  await page.locator('[data-enable-column="sex"]').check();
  await page.locator('[data-enable-column="sex"]').uncheck();
  await page.locator('[data-clear-filters]').click();
  await expect(page.locator('[data-enable-column="sex"]')).not.toBeChecked();
  await page.locator('#undo').click();
  expect((await state(page)).sourceTable.filters).toEqual({ sex: ['Female'] });
});

test('diagram columns support drag-and-drop and keyboard ordering, including disabled entries', async ({ page }) => {
  await importCohortTable(page);
  await expect(page.locator('#table-controls [data-column-move]')).toHaveCount(0);
  await page.locator('[data-table-column="TR02"] .column-grip').dragTo(page.locator('[data-table-column="TR00"]'), { targetPosition: { x: 30, y: 2 } });
  expect((await state(page)).setNames).toEqual(['TR02', 'TR00', 'TR01']);
  await page.locator('[data-table-column="sex"] .column-grip').dragTo(page.locator('[data-table-column="TR00"]'), { targetPosition: { x: 30, y: 2 } });
  expect((await state(page)).setNames).toEqual(['TR02', 'TR00', 'TR01']);
  expect((await state(page)).sourceTable.order).toEqual(['TR02', 'sex', 'TR00', 'TR01']);
  await page.locator('[data-enable-column="sex"]').check();
  expect((await state(page)).setNames).toEqual(['TR02', 'sex', 'TR00', 'TR01']);
  await page.locator('[data-table-column="sex"]').focus();
  await page.keyboard.press('Alt+ArrowUp');
  expect((await state(page)).setNames).toEqual(['sex', 'TR02', 'TR00', 'TR01']);
  await expect(page.locator('[data-table-column="sex"]')).toBeFocused();
  await page.locator('#undo').click();
  expect((await state(page)).setNames).toEqual(['TR02', 'sex', 'TR00', 'TR01']);
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
  await page.keyboard.down('Alt');
  await expect(page.locator('#plot .trace')).toHaveCount(1);
  await expect(page.locator('#hover-info h3')).toHaveText('One pathway');
  await page.keyboard.up('Alt');
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
    return { width: box.width, height: box.height, labelWidth: label.width, labelHeight: label.height,
      panelWidth: panel.clientWidth, panelHeight: panel.clientHeight, scrollWidth: panel.scrollWidth, scrollHeight: panel.scrollHeight };
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
  expect(height.panelHeight - height.height).toBeLessThanOrEqual(18);
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
  await page.keyboard.down('Shift');
  await page.mouse.move(p.x, p.y);
  await expect(page.locator('#hover-info h3')).toHaveText(point.member);
  await page.keyboard.up('Shift');
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
