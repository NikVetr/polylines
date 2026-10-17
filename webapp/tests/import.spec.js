const { test, expect } = require('@playwright/test');

test.beforeEach(async ({ page }) => {
  page.on('pageerror', err => { throw err; });
  await page.goto('/index.html');
  await page.evaluate(() => SankeyApp.loadExample('labeled'));
});

for (const problem of ['negative flow', 'invalid palette', 'invalid presentation', 'malformed source']) {
  test(`rejecting a project with ${problem} preserves the working project and autosave`, async ({ page }) => {
    await expect.poll(() => page.evaluate(() => !!localStorage.getItem('polylines-sankey-v1'))).toBe(true);
    const before = await page.evaluate(() => ({
      state: JSON.stringify(SankeyApp.state), svg: SankeyApp.exportSvgText(),
      saved: localStorage.getItem('polylines-sankey-v1'), undo: document.querySelector('#undo').disabled
    }));
    const project = { app: 'polylines-sankey', ...JSON.parse(before.state) };
    if (problem === 'negative flow') project.matrices[0][0][0] = -100;
    if (problem === 'invalid palette') project.style.palette = 'unknown';
    if (problem === 'invalid presentation') project.presentation.zoom = 0;
    if (problem === 'malformed source') project.sourceTable = { table: {} };
    await page.setInputFiles('#file-input', { name: 'invalid.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(project)) });
    await expect(page.locator('#banner')).toBeVisible();
    // A rejected candidate must not become the debounced autosave either.
    await page.waitForTimeout(500);
    expect(await page.evaluate(() => ({
      state: JSON.stringify(SankeyApp.state), svg: SankeyApp.exportSvgText(),
      saved: localStorage.getItem('polylines-sankey-v1'), undo: document.querySelector('#undo').disabled
    }))).toEqual(before);
  });
}

test('Build cannot submit the previous table while edited text is being parsed', async ({ page }) => {
  await page.evaluate(() => SK.importer.open('id,A,B\nold,a,x', 'old.csv'));
  const immediate = await page.evaluate(() => {
    const before = JSON.stringify(SankeyApp.state);
    const input = document.querySelector('#import-text');
    input.value = 'id,A,B\nnew1,b,y\nnew2,c,z';
    input.dispatchEvent(new Event('input', { bubbles: true }));
    const button = document.querySelector('#import-apply');
    button.click();
    return { disabled: button.disabled, unchanged: JSON.stringify(SankeyApp.state) === before };
  });
  expect(immediate).toEqual({ disabled: true, unchanged: true });
  await expect(page.locator('#import-apply')).toBeEnabled();
  await page.locator('#import-apply').click();
  expect(await page.evaluate(() => SankeyApp.state.sourceTable.table.rows.map(r => r[0]))).toEqual(['new1', 'new2']);
});

test('opening another table cancels a pending text edit', async ({ page }) => {
  await page.evaluate(() => {
    SK.importer.open('id,A,B\nold,a,x', 'old.csv');
    const input = document.querySelector('#import-text');
    input.value = 'id,A,B\nstale,b,y';
    input.dispatchEvent(new Event('input', { bubbles: true }));
    SK.importer.open('id,A,B\nnew,c,z', 'new.csv');
  });
  await page.waitForTimeout(350);
  await expect(page.locator('#import-file-name')).toHaveText('new.csv');
  await page.locator('#import-apply').click();
  expect(await page.evaluate(() => SankeyApp.state.sourceTable.name)).toBe('new.csv');
});

test('filtering a table preserves thickness and per-stage routing and supports undo', async ({ page }) => {
  const before = await page.evaluate(() => {
    const table = SK.transform.parseTable('id,A,B,C\np1,a,x,k\np2,b,y,l\np3,a,y,k');
    SankeyApp.applyImport(SK.transform.build(table, { format: 'wide', columns: ['A', 'B', 'C'], id: 'id' }), { labels: 'above' });
    SankeyApp.state.valueScale *= 2;
    SankeyApp.state.stageParams = [{ route: 'boundary' }, { source_order: 'target_index' }];
    SankeyApp.update({ commit: true });
    return JSON.stringify(SankeyApp.state);
  });
  await page.locator('.strip-values[data-strip-values="A"] [data-strip-value="b"]').click();
  await expect(page.locator('#banner')).toBeHidden();
  expect(await page.evaluate(() => SankeyApp.state.valueScale / SankeyApp.state.valueScaleRef)).toBeCloseTo(2);
  expect(await page.evaluate(() => SankeyApp.state.stageParams)).toEqual(JSON.parse(before).stageParams);
  await page.locator('#undo').click();
  expect(await page.evaluate(() => JSON.stringify(SankeyApp.state))).toBe(before);
});

test('Fit view contains the full height of large labels beside nodes', async ({ page }) => {
  await page.evaluate(() => {
    const table = SK.transform.parseTable('id,A,B\np1,a,x\np2,b,y');
    SankeyApp.applyImport(SK.transform.build(table, { format: 'wide', columns: ['A', 'B'], id: 'id' }), { labels: 'beside' });
    SankeyApp.state.style.canvasHeight = 240;
    SankeyApp.state.style.fontSize = 36;
    SankeyApp.state.loc.forEach(n => { n.cex = 3; });
    SankeyApp.state.valueScale *= 0.1;
    SankeyApp.update({ fit: true });
  });
  await expect(page.locator('#banner')).toBeHidden();
  const bounds = await page.evaluate(() => {
    const plot = document.querySelector('#plot').getBoundingClientRect();
    return [...document.querySelectorAll('#plot .node-label')].map(el => {
      const r = el.getBoundingClientRect();
      return { left: r.left - plot.left, right: plot.right - r.right, top: r.top - plot.top, bottom: plot.bottom - r.bottom };
    });
  });
  expect(bounds).toHaveLength(4);
  bounds.forEach(box => Object.values(box).forEach(clearance => expect(clearance).toBeGreaterThanOrEqual(0)));
});
