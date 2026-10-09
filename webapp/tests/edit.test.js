// Data edits on the app state. Run with: node --test tests/edit.test.js
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
require(js('edit.js'));

// Sets: 3 sources -> 3 middle nodes -> 2 sinks.
function sample() {
  return {
    mode: 'layered',
    matrices: [[[10, 0, 2], [0, 5, 1], [3, 0, 4]], [[8, 5], [2, 4], [4, 3]]],
    dimnames: [{ rows: ['a', 'b', 'c'], cols: ['X', 'Y', 'Z'] }, { rows: ['X', 'Y', 'Z'], cols: ['P', 'Q'] }],
    stageNames: ['s1', 's2'],
    colorMatrices: [[['#100', null, '#102'], [null, '#111', '#112'], ['#120', null, '#122']], [['#200', '#201'], ['#210', '#211'], ['#220', '#221']]],
    midMatrices: null,
    loc: [
      { set: 1, node: 1, x: 0, y: 0.9 }, { set: 1, node: 2, x: 0, y: 0.5 }, { set: 1, node: 3, x: 0, y: 0.1 },
      { set: 2, node: 1, x: 1, y: 0.8, label: 'X' }, { set: 2, node: 2, x: 1, y: 0.5, label: 'Y' }, { set: 2, node: 3, x: 1, y: 0.2, label: 'Z' },
      { set: 3, node: 1, x: 2, y: 0.7 }, { set: 3, node: 2, x: 2, y: 0.3 }
    ],
    nodeColors: { '2:1': '#aaa', '2:2': '#bbb', '2:3': '#ccc', '1:1': '#ddd' },
    mergeColors: {},
    flowOverrides: { '0:0:0': '#f00', '0:1:1': '#0f0', '1:2:0': '#00f', '0:0:2': '#ff0' },
    midOverrides: {},
    manualOrder: { start: { '1:1': ['0:0:0', '0:0:2'], '3:1': [] }, end: { '2:3': ['0:0:2'] } },
    paths: [
      { nodes: [0, 0, 0], weight: 8 }, { nodes: [0, 0, 1], weight: 2 }, { nodes: [1, 1, 0], weight: 2 },
      { nodes: [1, 1, 1], weight: 3 }, { nodes: [0, 2, 0], weight: 2 }, { nodes: [1, 2, 1], weight: 1 },
      { nodes: [2, 0, 1], weight: 3 }, { nodes: [2, 2, 0], weight: 2 }, { nodes: [2, 2, 1], weight: 2 }
    ],
    view: { xlim: [0, 1], ylim: [0, 1] }
  };
}

test('merging nodes sums their flows and remaps names, loc, colors and pathways', () => {
  const before = sample();
  const s = SK.edit.mergeNodes(before, 2, [0, 2], 'X + Z');
  assert.deepStrictEqual(s.matrices[0], [[12, 0], [1, 5], [7, 0]]);
  assert.deepStrictEqual(s.matrices[1], [[12, 8], [2, 4]]);
  assert.deepStrictEqual(s.dimnames[0].cols, ['X + Z', 'Y']);
  assert.deepStrictEqual(s.dimnames[1].rows, ['X + Z', 'Y']);
  // The merged cell takes the color of its larger flow (10 vs 2 from source a).
  assert.deepStrictEqual(s.colorMatrices[0].map((r) => r[0]), ['#100', '#112', '#122']);
  assert.deepStrictEqual(s.colorMatrices[1][0], ['#200', '#201']);
  const set2 = s.loc.filter((r) => r.set === 2);
  assert.deepStrictEqual(set2.map((r) => [r.node, r.label]), [[1, 'X + Z'], [2, 'Y']]);
  // Weighted by node totals: X = 13, Z = 7.
  assert.ok(Math.abs(set2[0].y - (0.8 * 13 + 0.2 * 7) / 20) < 1e-12);
  assert.deepStrictEqual(s.nodeColors, { '2:1': '#aaa', '2:2': '#bbb', '1:1': '#ddd' });
  // Strands of merged nodes are new strands; others keep their overrides under new indices.
  assert.deepStrictEqual(s.flowOverrides, { '0:1:1': '#0f0' });
  assert.deepStrictEqual(s.manualOrder, { start: {}, end: {} });
  assert.strictEqual(s.paths.reduce((t, p) => t + p.weight, 0), 25);
  assert.ok(s.paths.find((p) => p.nodes.join() === '0,0,0').weight === 10);
  assert.strictEqual(s.view, null);
  // Every node keeps its palette slot (its old loc position); the merged node keeps X's.
  assert.deepStrictEqual(s.nodeSlots, { '1:1': 0, '1:2': 1, '1:3': 2, '2:1': 3, '2:2': 4, '3:1': 6, '3:2': 7 });
  // The input state is left as it was.
  assert.deepStrictEqual(before, sample());
});

test('merged imported pathways still reproduce the flows exactly', () => {
  const table = 'patient,dx,tx,out\n1,A,S,R\n2,A,T,R\n3,B,S,Q\n4,B,U,R\n5,A,U,Q\n6,B,T,Q\n';
  const r = SK.transform.build(SK.transform.parseTable(table), { format: 'wide', id: 'patient', columns: ['dx', 'tx', 'out'] });
  const loc = [];
  r.dimnames.map((d) => d.rows).concat([r.dimnames[r.dimnames.length - 1].cols]).forEach((names, s) => {
    names.forEach((label, k) => loc.push({ set: s + 1, node: k + 1, x: s, y: 1 - (k + 0.5) / names.length, label }));
  });
  const state = { mode: 'layered', matrices: r.matrices, dimnames: r.dimnames, loc, paths: r.paths, manualOrder: { start: {}, end: {} } };
  const tx = r.dimnames[0].cols;
  const s = SK.edit.mergeNodes(state, 2, [tx.indexOf('S'), tx.indexOf('U')], 'S + U');
  const layout = SK.layout.computeLayout(
    { mode: 'layered', matrices: s.matrices, loc: s.loc, params: { n: 30 }, valueScale: 0.05 },
    { xlim: [-0.5, 2.5], ylim: [0, 1], width: 600, height: 400 }
  );
  assert.strictEqual(SK.paths.consistent(layout, s.paths), true);
  assert.strictEqual(s.paths.reduce((t, p) => t + p.members.length, 0), 6);
});

test('merging fails loudly outside layered stages or with fewer than two nodes', () => {
  assert.throws(() => SK.edit.mergeNodes(Object.assign(sample(), { mode: 'graph' }), 2, [0, 1], 'x'), /layered/);
  assert.throws(() => SK.edit.mergeNodes(sample(), 2, [1, 1], 'x'), /at least two/);
  assert.throws(() => SK.edit.mergeNodes(sample(), 2, [0, 5], 'x'), /no node/);
  assert.throws(() => SK.edit.mergeNodes(sample(), 4, [0, 1], 'x'), /no node set 4/);
});
