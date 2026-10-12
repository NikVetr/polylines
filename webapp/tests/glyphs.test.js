// Setting cartoons. Run with: node --test tests/glyphs.test.js
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const js = (f) => path.join(__dirname, '..', 'js', f);
const SK = require(js('color.js'));
require(js('glyphs.js'));

test('every glyph renders a 30 x 18 SVG with finite coordinates, and palettes show their colors', () => {
  const names = SK.glyphs.names().concat(Object.keys(SK.color.PALETTES).map((p) => `palette:${p}`));
  assert.ok(names.length > 50);
  names.forEach((name) => {
    assert.ok(SK.glyphs.has(name), name);
    const svg = SK.glyphs.markup(name, 'A & B');
    assert.match(svg, /^<svg class="glyph" viewBox="0 0 30 18" aria-hidden="true"><title>A & B<\/title>/, name);
    assert.doesNotMatch(svg, /undefined|NaN|null/, name);
  });
  assert.ok(SK.glyphs.markup('palette:tableau').includes(SK.color.PALETTES.tableau[0]));
});

test('unknown names have no glyph, and gradients get distinct ids', () => {
  assert.strictEqual(SK.glyphs.has('nope'), false);
  assert.strictEqual(SK.glyphs.markup('nope'), '');
  assert.strictEqual(SK.glyphs.has('palette:nope'), false);
  const ids = ['gradient', 'gradientMode:merge', 'gradientMode:path', 'useMid']
    .map((name) => SK.glyphs.markup(name).match(/id="([^"]+)"/)[1]);
  assert.strictEqual(new Set(ids).size, ids.length);
});
