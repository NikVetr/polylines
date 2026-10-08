// Geometry parity with the R implementation, plus focused layout checks.
// Run with: node --test webapp/tests/
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const SK = require(path.join(__dirname, '..', 'js', 'layout.js'));
const cases = require('./fixtures/r_parity.json');

// sx = sy = 1 and zero offsets, so plot pixels equal R data coordinates with xyr = 1.
const unitView = { xlim: [0, 1], ylim: [0, 1], width: 1, height: 1 };
const TOL = 1e-8;

// R's stack_order rows (side, [stage], from, to, rank; 1-based) as the app's manualOrder
// ({ side: { nodeKey: [flowKey, ...] } }, bottom to top).
function manualOrderFrom(rows, c) {
  const out = { start: {}, end: {} };
  rows.slice().sort((a, b) => a.rank - b.rank).forEach((r) => {
    const stage = (r.stage || 1) - 1;
    let nodeKey;
    if (c.mode === 'graph') {
      const node = c.loc[(r.side === 'start' ? r.from : r.to) - 1];
      nodeKey = `${node.set}:${node.node}`;
    } else {
      nodeKey = r.side === 'start' ? `${stage + 1}:${r.from}` : `${stage + 2}:${r.to}`;
    }
    (out[r.side][nodeKey] = out[r.side][nodeKey] || []).push(`${stage}:${r.from - 1}:${r.to - 1}`);
  });
  return out;
}

function toModel(c) {
  const params = {};
  const stageParams = [];
  let manualOrder;
  Object.entries(c.params).forEach(([key, value]) => {
    if (key === 'stack_order') {
      manualOrder = manualOrderFrom(value, c);
    } else if (Array.isArray(value)) {
      value.forEach((v, s) => { stageParams[s] = Object.assign(stageParams[s] || {}, { [key]: v }); });
    } else {
      params[key] = value;
    }
  });
  return { mode: c.mode, matrices: c.matrices, loc: c.loc, params, stageParams, manualOrder };
}

function maxPointDiff(a, b) {
  assert.strictEqual(a.length, b.length, 'point counts differ');
  return Math.max(...a.map((p, i) => Math.max(Math.abs(p[0] - b[i][0]), Math.abs(p[1] - b[i][1]))));
}

cases.forEach((c) => {
  test(`R parity: ${c.name}`, () => {
    const layout = SK.layout.computeLayout(toModel(c), unitView);
    const byKey = new Map(layout.flows.map((f) => [f.key, f]));
    assert.strictEqual(layout.flows.length, c.expected.flows.length, 'flow count');
    c.expected.flows.forEach((e) => {
      const key = `${e.stage}:${e.i}:${e.j}`;
      const f = byKey.get(key);
      assert.ok(f, `missing flow ${key}`);
      assert.ok(Math.abs(f.startOffset - e.start_offset) < TOL, `${key} start offset`);
      assert.ok(Math.abs(f.endOffset - e.end_offset) < TOL, `${key} end offset`);
      assert.ok(Math.abs(f.startX - e.start_x) < TOL, `${key} start x`);
      assert.ok(Math.abs(f.endX - e.end_x) < TOL, `${key} end x`);
      assert.ok(maxPointDiff(f.centerline, e.centerline) < TOL, `${key} centerline`);
      assert.ok(maxPointDiff(f.upper, e.upper) < TOL, `${key} upper edge`);
      assert.ok(maxPointDiff(f.lower, e.lower) < TOL, `${key} lower edge`);
    });
    layout.nodes.forEach((n, k) => {
      assert.ok(Math.abs(n.y - c.expected.loc_y[k]) < TOL, `loc row ${k + 1} adjusted y`);
    });
    assert.ok(Math.abs(layout.mergeLength - c.expected.merge_length) < TOL, 'merge length');
  });
});

const simpleModel = () => ({
  mode: 'layered',
  matrices: [[[0.1, 0.05], [0.05, 0.1]]],
  loc: [
    { set: 1, node: 1, x: 0, y: 0.7 }, { set: 1, node: 2, x: 0, y: 0.3 },
    { set: 2, node: 1, x: 1, y: 0.7 }, { set: 2, node: 2, x: 1, y: 0.3 }
  ],
  params: {}
});

test('manual strand order overrides the computed stack order', () => {
  const model = simpleModel();
  const auto = SK.layout.computeLayout(model, unitView);
  const top = auto.flows.filter((f) => f.from === 0).sort((a, b) => b.startOffset - a.startOffset)[0];
  assert.strictEqual(top.to, 2, 'by default the strand to the upper target leaves from the top');
  model.manualOrder = { start: { '1:1': ['0:0:0', '0:0:1'] } };
  const manual = SK.layout.computeLayout(model, unitView);
  const manualTop = manual.flows.filter((f) => f.from === 0).sort((a, b) => b.startOffset - a.startOffset)[0];
  assert.strictEqual(manualTop.key, '0:0:1');
});

test('loc rows with blank labels are not treated as labeled', () => {
  const model = simpleModel();
  model.loc[0].label = 'Alpha';
  model.loc[1].label = '';
  const layout = SK.layout.computeLayout(model, { xlim: [-0.2, 1.2], ylim: [0, 1], width: 800, height: 500 });
  assert.strictEqual(layout.nodes[0].hasLabel, true);
  assert.strictEqual(layout.nodes[1].hasLabel, false);
  assert.ok(layout.nodes[0].widthPx > 0);
  assert.strictEqual(layout.nodes[1].widthPx, 0);
});

test('invalid inputs fail loudly with a SankeyError', () => {
  const missing = simpleModel();
  missing.loc.pop();
  assert.throws(() => SK.layout.computeLayout(missing, unitView), /missing node 2 of set 2/);
  const mismatch = simpleModel();
  mismatch.matrices.push([[1, 2, 3]]);
  assert.throws(() => SK.layout.computeLayout(mismatch, unitView), SK.layout.SankeyError);
  const badRoute = simpleModel();
  badRoute.params.route = 'straight';
  assert.throws(() => SK.layout.computeLayout(badRoute, unitView), /route must be one of/);
});

test('bounded node adjustment matches the R regression cases', () => {
  const close = (a, b) => a.every((v, i) => Math.abs(v - b[i]) < 1e-12);
  assert.ok(close(SK.layout.adjustOneSet([0, 0.01, 0.5, 1], [0.1, 0.1, 0.1, 0.1], 0, false), [0, 0.1, 0.5, 1]));
  assert.ok(close(SK.layout.adjustOneSet([0, 0.01, 0.5, 1], [0.1, 0.1, 0.1, 0.1], 0, true), [-0.045, 0.055, 0.5, 1]));
  assert.ok(close(SK.layout.adjustOneSet([0, 0.02, 0.2], [0.2, 0.2, 0.2], 0, false), [0, 0.1, 0.2]));
});
