const { test, expect } = require('@playwright/test');
const fs = require('node:fs/promises');
const path = require('node:path');
const { promisify } = require('node:util');
const execFile = promisify(require('node:child_process').execFile);
const sample = require('../examples/submission.json');

test('table submissions render and export without clicking and own their input and output', async ({ page }) => {
  page.on('pageerror', err => { throw err; });
  await page.goto('/index.html');
  const result = await page.evaluate(request => {
    request.style.labelColorBy = 'single';
    request.style.labelColor = '#123456';
    request.thickness = 1.5;
    const original = JSON.stringify(request);
    const project = SankeyApp.submit(request);
    const unchanged = JSON.stringify(request) === original;
    const total = project.paths.reduce((n, p) => n + p.weight, 0);
    request.spec.columns.reverse();
    project.matrices[0][0][0] = -1;
    const saved = SankeyApp.getProject();
    return { unchanged, total, columns: saved.sourceTable.spec.columns, labelColorBy: saved.style.labelColorBy,
      thickness: saved.valueScale / saved.valueScaleRef,
      valid: saved.matrices[0][0][0] >= 0, svg: SankeyApp.exportSvgText(), dialogs: document.querySelectorAll('dialog[open]').length };
  }, sample);
  expect(result.unchanged).toBe(true);
  expect(result.total).toBe(3);
  expect(result.columns).toEqual(sample.spec.columns);
  expect(result.valid).toBe(true);
  expect(result.labelColorBy).toBe('single');
  expect(result.thickness).toBeCloseTo(1.5);
  expect(result.svg).toContain('Remission');
  expect(result.dialogs).toBe(0);
});

for (const mode of ['layered', 'graph']) {
  test(`matrix submissions generate locations in ${mode} mode and projects round-trip`, async ({ page }) => {
    await page.goto('/index.html');
    const result = await page.evaluate(mode => {
      const project = SankeyApp.submit({ type: 'matrices', mode,
        matrices: mode === 'graph' ? [[[0, 0.1, 0.2], [0, 0, 0.1], [0, 0, 0]]] : [[[0.1, 0.2], [0.2, 0.1]]],
        style: { canvasWidth: 900, canvasHeight: 600 } });
      const svg = SankeyApp.exportSvgText();
      SankeyApp.submit({ type: 'table', text: 'A,B\nx,y\ny,z', spec: { format: 'wide', columns: ['A', 'B'] } });
      const restored = SankeyApp.submit({ type: 'project', project });
      return { same: JSON.stringify(project) === JSON.stringify(restored), loc: restored.loc.length,
        mode: restored.mode, sameSvg: SankeyApp.exportSvgText() === svg };
    }, mode);
    expect(result).toEqual({ same: true, loc: mode === 'graph' ? 3 : 4, mode, sameSvg: true });
  });
}

test('failed submissions throw and preserve the last accepted state and SVG', async ({ page }) => {
  await page.goto('/index.html');
  const failures = await page.evaluate(sample => {
    SankeyApp.submit(sample);
    const state = JSON.stringify(SankeyApp.getProject()), svg = SankeyApp.exportSvgText();
    return [
      { type: 'matrices', matrices: [[[1, 2], [3]]] },
      { type: 'matrices', matrices: [[[1]]], style: { palette: 'unknown' } },
      { type: 'matrices', matrices: [[[NaN]]] },
      { type: 'matrices', matrices: [], loc: [{ set: 1, node: 1, x: 0, y: 0 }] },
      { ...sample, thickness: -1 },
      { ...sample, style: { fontSze: 15 } },
      { ...sample, unexpected: true },
      { ...sample, text: 'A,B\nx,y', spec: { format: 'wide', columns: ['A'] } }
    ].map(request => {
      let error;
      try { SankeyApp.submit(request); } catch (err) { error = err.name; }
      return { error, unchanged: state === JSON.stringify(SankeyApp.getProject()), sameSvg: svg === SankeyApp.exportSvgText() };
    });
  }, sample);
  failures.forEach(result => expect(result).toEqual({ error: 'SankeyError', unchanged: true, sameSvg: true }));
});

test('headless CLI writes SVG, PNG, project and R bundle, and fails without overwriting output', async ({}, testInfo) => {
  test.setTimeout(60000);
  const dir = testInfo.outputPath('cli');
  await fs.mkdir(dir, { recursive: true });
  const input = path.join(dir, 'request.json');
  await fs.writeFile(input, JSON.stringify(sample));
  const script = path.join(__dirname, '..', 'scripts', 'render.cjs');
  for (const format of ['svg', 'png', 'json', 'zip']) {
    const output = path.join(dir, `diagram.${format}`);
    await execFile(process.execPath, [script, input, output]);
    const bytes = await fs.readFile(output);
    if (format === 'svg') expect(bytes.toString()).toContain('Remission');
    if (format === 'png') expect([...bytes.subarray(0, 8)]).toEqual([137, 80, 78, 71, 13, 10, 26, 10]);
    if (format === 'json') expect(JSON.parse(bytes).sourceTable.table.rows).toHaveLength(3);
    if (format === 'zip') { expect(bytes.toString()).toContain('sankey.R'); expect(bytes.toString()).toContain('project.json'); }
  }
  const output = path.join(dir, 'diagram.svg');
  const before = await fs.readFile(output);
  await fs.writeFile(input, JSON.stringify({ type: 'matrices', matrices: [[[-1]]] }));
  await expect(execFile(process.execPath, [script, input, output])).rejects.toMatchObject({ code: 1 });
  expect(await fs.readFile(output)).toEqual(before);
});
