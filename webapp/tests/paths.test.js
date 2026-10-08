// Item-level pathway tracking and substrand geometry. Run with: node --test tests/paths.test.js
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const js = (f) => path.join(__dirname, '..', 'js', f);
const SK = require(js('layout.js'));
require(js('color.js'));
require(js('csv.js'));
require(js('transform.js'));
require(js('shapes.js'));
require(js('paths.js'));
const T = SK.transform;

const wide = 'a,b,c,n\nA,X,P,10\nA,Y,P,5\nB,X,Q,7\nA,X,Q,3\nB,X,P,2\n';

function buildLayout(result) {
  const counts = result.dimnames.map((d) => d.rows.length).concat([result.dimnames[result.dimnames.length - 1].cols.length]);
  const loc = [];
  counts.forEach((count, s) => {
    for (let k = 0; k < count; k++) loc.push({ set: s + 1, node: k + 1, x: s, y: 1 - (k + 0.5) / count });
  });
  return SK.layout.computeLayout(
    { mode: 'layered', matrices: result.matrices, loc, params: { n: 30 }, valueScale: 0.004 },
    { xlim: [-0.2, counts.length - 0.8], ylim: [0, 1], width: 800, height: 500 }
  );
}

test('wide imports keep weighted trajectories that reproduce the matrices', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  assert.strictEqual(r.unit, 'n');
  assert.strictEqual(r.paths.reduce((s, p) => s + p.weight, 0), 27);
  const layout = buildLayout(r);
  assert.strictEqual(SK.paths.consistent(layout, r.paths), true);
});

test('grouping into Other merges trajectories that become identical', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' }, { topN: 1 });
  assert.deepStrictEqual(r.dimnames[0].rows, ['A', 'B']);
  assert.ok(r.paths.every((p) => p.nodes.length === 3));
  assert.strictEqual(SK.paths.consistent(buildLayout(r), r.paths), true);
});

test('long imports trace each item, including kept missing values', () => {
  const t = T.parseTable('id,wave,s\n1,1,a\n1,2,b\n2,1,a\n2,2,\n3,1,b\n3,2,b\n');
  const r = T.build(t, { format: 'long', id: 'id', stage: 'wave', category: 's', missing: 'keep' });
  assert.strictEqual(r.paths.length, 3);
  assert.strictEqual(r.unit, 'items');
});

test('substrands tile every strand and a trace is a closed outline through each stage', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  const layout = buildLayout(r);
  const subs = SK.paths.substrands(layout, r.paths);
  layout.flows.forEach((f) => {
    const parts = subs.byFlow[f.index].slice().sort((x, y) => x.a - y.a);
    assert.ok(Math.abs(parts[0].a) < 1e-12 && Math.abs(parts[parts.length - 1].b - 1) < 1e-12);
    parts.slice(1).forEach((part, i) => assert.ok(Math.abs(part.a - parts[i].b) < 1e-12));
  });
  r.paths.forEach((p, pi) => {
    const trace = SK.paths.tracePolygon(layout, subs, pi);
    assert.strictEqual(trace.flows.length, 2);
    const xs = trace.polygon.map((pt) => pt[0]);
    assert.ok(Math.min(...xs) <= trace.flows[0].startX + 1e-9 && Math.max(...xs) >= trace.flows[1].endX - 1e-9);
  });
});

test('the substrand under a point identifies its pathway', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  const layout = buildLayout(r);
  const subs = SK.paths.substrands(layout, r.paths);
  const f = layout.flows.find((x) => x.stage === 0 && subs.byFlow[x.index].length > 1);
  subs.byFlow[f.index].forEach((part) => {
    const k = Math.floor(f.lower.length / 2);
    const t = (part.a + part.b) / 2;
    const X = f.lower[k][0] + (f.upper[k][0] - f.lower[k][0]) * t;
    const Y = f.lower[k][1] + (f.upper[k][1] - f.lower[k][1]) * t;
    assert.strictEqual(SK.paths.pathAt(layout, subs, f.index, X, Y), part.path);
  });
});

test('edited flow values make the pathways inconsistent', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  r.matrices[0][0][0] += 1;
  assert.strictEqual(SK.paths.consistent(buildLayout(r), r.paths), false);
});
