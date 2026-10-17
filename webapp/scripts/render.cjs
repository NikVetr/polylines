#!/usr/bin/env node
/* Render a submission through the browser API without a server or GUI interaction. */
const fs = require('node:fs/promises');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const { chromium } = require('@playwright/test');

async function renderSubmission(request, format = 'svg') {
  if (!['svg', 'png', 'json', 'zip'].includes(format)) throw new Error('Output format must be svg, png, json or zip.');
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
    let pageError;
    page.on('pageerror', err => { pageError = err; });
    await page.goto(pathToFileURL(path.join(__dirname, '..', 'index.html')).href);
    await page.evaluate(input => {
      SankeyApp.submit(input);
      if (!SankeyApp.layout) throw new Error('The submission did not produce a drawable diagram.');
    }, request);
    // Let viewport/strip ResizeObservers settle before measuring export bounds.
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    const content = await page.evaluate(async kind => {
      if (kind === 'svg') return SankeyApp.exportSvgText();
      if (kind === 'json') return JSON.stringify(SankeyApp.getProject(), null, 2) + '\n';
      if (kind === 'zip') return Array.from(SankeyApp.exportBundle());
      return Array.from(new Uint8Array(await (await SankeyApp.exportPngBlob()).arrayBuffer()));
    }, format);
    if (pageError) throw pageError;
    return Buffer.from(content);
  } finally {
    await browser.close();
  }
}

async function main() {
  const [input, output, ...extra] = process.argv.slice(2);
  if (!input || !output || extra.length) throw new Error('Usage: npm run render -- request.json output.svg (also .png, .json, .zip; use - for stdin)');
  const text = input === '-' ? await new Promise((resolve, reject) => {
    let data = '';
    process.stdin.setEncoding('utf8');
    process.stdin.on('data', chunk => { data += chunk; });
    process.stdin.on('end', () => resolve(data));
    process.stdin.on('error', reject);
  }) : await fs.readFile(input, 'utf8');
  const content = await renderSubmission(JSON.parse(text), path.extname(output).slice(1).toLowerCase());
  await fs.mkdir(path.dirname(path.resolve(output)), { recursive: true });
  await fs.writeFile(output, content);
  process.stderr.write(`Wrote ${output}\n`);
}

if (require.main === module) main().catch(err => {
  process.stderr.write(`${err.message}\n`);
  process.exitCode = 1;
});

module.exports = { renderSubmission };
