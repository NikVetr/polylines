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

function buildLayout(result, params = {}, nodeOptions = {}) {
  const counts = result.dimnames.map((d) => d.rows.length).concat([result.dimnames[result.dimnames.length - 1].cols.length]);
  const loc = [];
  counts.forEach((count, s) => {
    for (let k = 0; k < count; k++) loc.push(Object.assign({ set: s + 1, node: k + 1, x: s, y: 1 - (k + 0.5) / count }, nodeOptions));
  });
  return SK.layout.computeLayout(
    { mode: 'layered', matrices: result.matrices, loc, paths: result.paths, params: Object.assign({ n: 30 }, params), valueScale: 0.004 },
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
    assert.deepStrictEqual(SK.paths.pathAt(layout, subs, f.index, X, Y), { path: part.path, member: -1 });
  });
});

for (const route of ['bezier', 'boundary']) {
  test(`percentage anchors stay inside their substrand with ${route} routing`, () => {
    const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
    const layout = buildLayout(r, { route });
    const subs = SK.paths.substrands(layout, r.paths);
    r.paths.forEach((p, pi) => {
      const t = SK.paths.tracePolygon(layout, subs, pi);
      assert.strictEqual(t.labelAnchors.length, 2);
      t.labelAnchors.forEach((a) => {
        const f = layout.flows[a.flow];
        for (const candidate of [a, ...a.alternatives]) {
          assert.ok(candidate.X > Math.min(f.startX, f.endX) && candidate.X < Math.max(f.startX, f.endX));
          assert.deepStrictEqual(SK.paths.pathAt(layout, subs, a.flow, candidate.X, candidate.Y), { path: pi, member: -1 });
          assert.ok(Number.isFinite(candidate.angle) && candidate.thickness > 0);
        }
      });
      const n = layout.nodes[t.countAnchor.node];
      assert.strictEqual(n.set, 3);
      assert.strictEqual(t.countAnchor.X, n.X + n.widthPx / 2);
    });
  });
}

test('leftward pathways put the count at the source, their rightmost node', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  const loc = [3, 2, 1].flatMap((x, s) => [0, 1].map((i) => ({ set: s + 1, node: i + 1, x, y: 0.8 - i * 0.5, width: 0.1 })));
  const layout = SK.layout.computeLayout({ mode: 'layered', matrices: r.matrices, loc, valueScale: 0.004 },
    { xlim: [0.8, 3.2], ylim: [0, 1], width: 800, height: 500 });
  const subs = SK.paths.substrands(layout, r.paths);
  r.paths.forEach((p, pi) => {
    const t = SK.paths.tracePolygon(layout, subs, pi, { fadeLabels: false });
    const n = layout.nodes[t.countAnchor.node];
    assert.strictEqual(n.set, 1);
    assert.strictEqual(t.countAnchor.X, n.X + n.widthPx / 2);
    t.labelAnchors.forEach((a) => assert.ok(Math.abs(a.angle) > Math.PI / 2));
  });
});

test('edited flow values make the pathways inconsistent', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  r.matrices[0][0][0] += 1;
  assert.strictEqual(SK.paths.consistent(buildLayout(r), r.paths), false);
});

const perPatient = 'patient_id,a,b,c\nP1,A,X,P\nP2,A,X,P\nP3,A,Y,P\nP4,B,X,Q\nP5,A,X,Q\nP6,A,X,P\n';

test('an id column keeps each item as a member of its pathway', () => {
  const r = T.build(T.parseTable(perPatient), { format: 'wide', id: 'patient_id', columns: ['a', 'b', 'c'] });
  assert.strictEqual(r.unit, 'patients');
  assert.deepStrictEqual(r.setNames, ['a', 'b', 'c']);
  const axp = r.paths.find((p) => p.members.length === 3);
  assert.deepStrictEqual(axp.members.map((m) => m.id), ['P1', 'P2', 'P6']);
  assert.strictEqual(axp.weight, 3);
  assert.throws(() => T.build(T.parseTable(`${perPatient}P1,B,Y,Q\n`), { format: 'wide', id: 'patient_id', columns: ['a', 'b', 'c'] }),
    /"P1" appears on more than one row/);
  assert.throws(() => T.build(T.parseTable(`${perPatient},B,Y,Q\n`), { format: 'wide', id: 'patient_id', columns: ['a', 'b', 'c'] }),
    /no item id/);
  const long = T.build(T.parseTable('respondent,wave,s\nR1,1,a\nR1,2,b\nR2,1,a\nR2,2,b\n'),
    { format: 'long', id: 'respondent', stage: 'wave', category: 's' });
  assert.strictEqual(long.unit, 'respondents');
  assert.deepStrictEqual(long.setNames, ['wave 1', 'wave 2']);
  assert.deepStrictEqual(long.paths[0].members.map((m) => m.id), ['R1', 'R2']);
});

test('members tile their substrand, can be hit and traced, and draw boundary lines', () => {
  const r = T.build(T.parseTable(perPatient), { format: 'wide', id: 'patient_id', columns: ['a', 'b', 'c'] });
  const layout = buildLayout(r);
  const subs = SK.paths.substrands(layout, r.paths);
  const pi = r.paths.findIndex((p) => p.members.length === 3);
  const f = layout.flows[subs.pathFlows[pi][0]];
  const part = subs.byFlow[f.index].find((e) => e.path === pi);
  const k = Math.floor(f.lower.length / 2);
  [0, 1, 2].forEach((member) => {
    const t = part.a + (part.b - part.a) * (member + 0.5) / 3;
    const X = f.lower[k][0] + (f.upper[k][0] - f.lower[k][0]) * t;
    const Y = f.lower[k][1] + (f.upper[k][1] - f.lower[k][1]) * t;
    assert.deepStrictEqual(SK.paths.pathAt(layout, subs, f.index, X, Y), { path: pi, member });
  });
  const whole = SK.paths.tracePolygon(layout, subs, pi);
  const one = SK.paths.tracePolygon(layout, subs, pi, { member: 1 });
  assert.ok(Math.abs(one.thickness * 3 - whole.thickness) < 1e-9);
  // One line between each pair of neighbouring items in a strand.
  const lines = SK.paths.boundaryLines(layout, subs);
  layout.flows.forEach((g) => assert.strictEqual(lines[g.index].length, g.value - 1));
  // The node under the pointer resolves to the item at that height on the nearer side.
  const n = layout.nodes[f.from];
  const hit = SK.paths.pathAtNode(layout, subs, n, n.X + 1, f.startY + f.startOffset);
  assert.ok(hit && hit.path >= 0 && hit.member >= 0);
});

test('gathered traces sit flush against a node edge at the centre of every node they share', () => {
  const r = T.build(T.parseTable(perPatient), { format: 'wide', id: 'patient_id', columns: ['a', 'b', 'c'] });
  const layout = buildLayout(r);
  const subs = SK.paths.substrands(layout, r.paths);
  // Trace the pathways leaving A: a subset of each later node's stack.
  const list = r.paths.map((p, pi) => (p.nodes[0] === 0 ? pi : -1)).filter((pi) => pi >= 0);
  const slots = SK.paths.gatherSlots(layout, subs, list);
  slots.forEach((places, id) => {
    const spans = [...places.values()].sort((a, b) => a[0] - b[0]);
    spans.slice(1).forEach((span, i) => assert.ok(Math.abs(span[0] - spans[i][1]) < 1e-9));
    // The bundle lines up with the node's bottom or top edge at its centre.
    const n = layout.nodes[id];
    const role = SK.shapes.nodeRole(n);
    let edges;
    if (role === 'interior') {
      const shape = SK.shapes.interiorTrunk(layout, n);
      edges = [SK.layout.pointAtX(shape.lower, n.X)[1], SK.layout.pointAtX(shape.upper, n.X)[1]];
    } else {
      const b = SK.shapes.bundle(layout, n, role === 'source' ? 'start' : 'end');
      edges = [b.lowerY, b.upperY];
    }
    assert.ok(Math.abs(spans[0][0] - edges[0]) < 1e-9 || Math.abs(spans[spans.length - 1][1] - edges[1]) < 1e-9);
  });
  list.forEach((pi) => {
    const t = SK.paths.tracePolygon(layout, subs, pi, { slots });
    const n = layout.nodes[t.flows[1].from];
    const [lo, up] = slots.get(n.id).get(pi);
    const near = (pts, y) => pts.filter((pt) => Math.abs(pt[1] - y) < 1e-9 && Math.abs(pt[0] - n.X) < 30).length;
    assert.ok(near(t.upper, up) >= 1 && near(t.lower, lo) >= 1);
  });
});

test('member hit positions follow rendered slices throughout expanded node bodies', () => {
  const table = T.parseTable('id,a,b,c\nP1,A,B,C\nP2,A,B,C\nP3,A,B,C\nP4,A,B,C\nP5,A,B,C\n');
  const r = T.build(table, { format: 'wide', id: 'id', columns: ['a', 'b', 'c'] });
  const layout = buildLayout(r, {}, { width: 0.35, height: 0.4, label: 'Node' });
  const subs = SK.paths.substrands(layout, r.paths);
  const whole = SK.paths.tracePolygon(layout, subs, 0, { fadeLabels: false });
  layout.nodes.forEach((n) => {
    for (const dx of [-0.2, 0, 0.2]) {
      const X = n.X + n.widthPx * dx;
      const lo = SK.layout.pointAtX(whole.lower, X)[1];
      const up = SK.layout.pointAtX(whole.upper, X)[1];
      for (const t of [0.01, 0.21, 0.51, 0.79, 0.99]) {
        const member = Math.floor(t * 5);
        const Y = lo + (up - lo) * t;
        assert.deepStrictEqual(SK.paths.pathAtNode(layout, subs, n, X, Y), { path: 0, member });
        const slice = SK.paths.tracePolygon(layout, subs, 0, { member, fadeLabels: false });
        assert.ok(Y >= SK.layout.pointAtX(slice.lower, X)[1] - 1e-8 && Y <= SK.layout.pointAtX(slice.upper, X)[1] + 1e-8);
      }
    }
  });
});

test('member hit positions invert the drawn offset edges on steep strands', () => {
  const r = T.build(T.parseTable('id,a,b\nP1,A,B\nP2,A,B\nP3,A,B\nP4,A,B\nP5,A,B\n'),
    { format: 'wide', id: 'id', columns: ['a', 'b'] });
  const layout = SK.layout.computeLayout({ mode: 'layered', matrices: r.matrices,
    loc: [{ set: 1, node: 1, x: 0, y: 0.9 }, { set: 2, node: 1, x: 1, y: 0.1 }],
    params: { n: 100 }, valueScale: 0.015 }, { xlim: [-0.2, 1.2], ylim: [0, 1], width: 600, height: 600 });
  const subs = SK.paths.substrands(layout, r.paths);
  const f = layout.flows[0];
  for (const i of [10, 25, 50, 75, 90]) {
    for (const t of [0.01, 0.21, 0.51, 0.79, 0.99]) {
      const X = f.lower[i][0] * (1 - t) + f.upper[i][0] * t;
      const Y = f.lower[i][1] * (1 - t) + f.upper[i][1] * t;
      assert.deepStrictEqual(SK.paths.pathAt(layout, subs, 0, X, Y), { path: 0, member: Math.floor(t * 5) });
    }
  }
});

test('reordered pathways tile the weighted stack without expanding to the label body', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  const layout = buildLayout(r, {}, { width: 0.35, height: 0.4, label: 'Node' });
  const subs = SK.paths.substrands(layout, r.paths);
  layout.nodes.filter(n => n.set === 2).forEach(n => {
    const paths = r.paths.map((p, pi) => p.nodes[1] === n.node - 1 ? pi : -1).filter(pi => pi >= 0);
    for (const dx of [-0.2, 0]) {
      const X = n.X + n.widthPx * dx;
      const lo = n.Y - n.inPx / 2, up = n.Y + n.inPx / 2;
      const spans = paths.map(pi => {
        const trace = SK.paths.tracePolygon(layout, subs, pi);
        return [SK.layout.pointAtX(trace.lower, X)[1], SK.layout.pointAtX(trace.upper, X)[1]];
      }).sort((a, b) => a[0] - b[0]);
      assert.ok(Math.abs(spans[0][0] - lo) < 1e-8 && Math.abs(spans[spans.length - 1][1] - up) < 1e-8);
      spans.slice(1).forEach((s, i) => assert.ok(Math.abs(s[0] - spans[i][1]) < 1e-8));
      for (const t of [0.01, 0.05, 0.5, 0.95, 0.99]) {
        const Y = lo + (up - lo) * t;
        const hit = SK.paths.pathAtNode(layout, subs, n, X, Y);
        const trace = SK.paths.tracePolygon(layout, subs, hit.path);
        assert.ok(Y >= SK.layout.pointAtX(trace.lower, X)[1] - 1e-8 && Y <= SK.layout.pointAtX(trace.upper, X)[1] + 1e-8);
      }
    }
  });
});

test('node traces follow their exact weighted slices through incoming and outgoing merge bands', () => {
  const table = T.parseTable('id,a,b,c,n\nP1,A,X,P,4\nP2,A,X,P,6\nP3,A,Y,P,5\nP4,B,X,Q,7\nP5,A,X,Q,3\nP6,B,X,P,2\n');
  const r = T.build(table, { format: 'wide', id: 'id', columns: ['a', 'b', 'c'], weight: 'n' });
  for (const route of ['bezier', 'boundary']) for (const dir of [1, -1]) {
    const loc = [0, 1, 2].flatMap(s => [0, 1].map(i => ({ set: s + 1, node: i + 1,
      x: dir * s, y: 0.8 - i * 0.6, width: 0.35, height: 0.4, label: 'Node' })));
    const layout = SK.layout.computeLayout({ mode: 'layered', matrices: r.matrices, loc,
      params: { route, n: 120, merge_frac: 0.2 }, valueScale: 0.004 },
    { xlim: [-2.2, 2.2], ylim: [0, 1], width: 800, height: 500 });
    const subs = SK.paths.substrands(layout, r.paths);
    r.paths.forEach((p, pi) => {
      for (const member of [-1, ...p.members.map((_, mi) => mi)]) {
        const trace = SK.paths.tracePolygon(layout, subs, pi, { member });
        trace.flows.forEach((f, stage) => {
          const part = subs.byFlow[f.index].find(e => e.path === pi);
          const cuts = member < 0 ? [0, 1] : subs.memberCuts[pi].slice(member, member + 2);
          const fractions = cuts.map(c => part.a + (part.b - part.a) * c);
          [trace.lower, trace.upper].forEach((edge, j) => {
            const fraction = fractions[j];
            const expected = f.lower.map((lo, i) => [lo[0] * (1 - fraction) + f.upper[i][0] * fraction,
              lo[1] * (1 - fraction) + f.upper[i][1] * fraction]);
            const m = Math.min(layout.mergeLength, Math.abs(f.endX - f.startX) / 4);
            for (const t of [0, 0.1, 0.5, 0.9, 1]) {
              const x = stage === 0 ? f.endX - dir * m * t : f.startX + dir * m * t;
              assert.ok(Math.abs(SK.layout.pointAtX(edge, x)[1] - SK.layout.pointAtX(expected, x)[1]) < 1e-8,
                `${route}, direction ${dir}, path ${pi}, member ${member}: merge slice must match its ribbon`);
            }
          });
        });
      }
    });
  }
});

test('node turns preserve path and member widths instead of stretching to expanded label bodies', () => {
  const tables = [
    ['id,a,b,c,n\nP1,A,X,P,4\nP2,A,X,P,6\nP3,A,X,Q,5\nP4,B,X,Q,7\n', 0.35],
    ['id,a,b,c,n\n' + ['A', 'B', 'C', 'D'].flatMap(a => ['P', 'Q', 'R', 'S'].map(c => `${a}${c},${a},X,${c},1`)).join('\n'), 0.01]
  ];
  for (const [csv, nodeWidth] of tables) {
    const r = T.build(T.parseTable(csv), { format: 'wide', id: 'id', columns: ['a', 'b', 'c'], weight: 'n' });
    const layout = buildLayout(r, {}, { width: nodeWidth, height: 0.8, label: 'Node' });
    const subs = SK.paths.substrands(layout, r.paths);
    r.paths.forEach((p, pi) => SK.paths.tracePolygon(layout, subs, pi));
    for (const [key, ribbon] of subs.nodeRibbons) {
      const pi = Number(key.split(':')[1]), p = r.paths[pi];
      const width = p.weight * layout.valueScale * layout.tf.sx;
      const mid = ribbon.lower.map((lo, i) => [(lo[0] + ribbon.upper[i][0]) / 2, (lo[1] + ribbon.upper[i][1]) / 2]);
      mid.slice(1, -1).forEach((_, j) => {
        const i = j + 1, dx = mid[i + 1][0] - mid[i - 1][0], dy = mid[i + 1][1] - mid[i - 1][1];
        const normal = Math.abs((ribbon.upper[i][0] - ribbon.lower[i][0]) * -dy + (ribbon.upper[i][1] - ribbon.lower[i][1]) * dx) / Math.hypot(dx, dy);
        assert.ok(Math.abs(normal / width - 1) < 0.001);
        p.members.forEach((member, mi) => {
          assert.ok(Math.abs(normal * (subs.memberCuts[pi][mi + 1] - subs.memberCuts[pi][mi]) - member.weight * layout.valueScale * layout.tf.sx) < width * 0.001);
        });
      });
      for (const edge of [ribbon.lower, ribbon.upper]) {
        for (const [a, b] of [[edge[0], edge[1]], [edge.at(-2), edge.at(-1)]]) {
          assert.ok(Math.abs((b[1] - a[1]) / (b[0] - a[0])) < 0.025);
        }
      }
    }
  }
});

test('body curves reserve smooth shoulders and endpoint fills share their outline edges', () => {
  const n = { X: 0, Y: 0, widthPx: 40, labelWidthPx: 40, bodyHeightPx: 80 };
  for (const dir of [-1, 1]) {
    const edge = SK.shapes.bridgeBody([-20 * dir, -5], [20 * dir, -15], n, 'lower', 16);
    edge.slice(1).forEach((p, i) => assert.ok(Math.abs(p[0] - edge[i][0]) > 1e-9 || Math.abs(p[1] - edge[i][1]) < 1e-9));
    assert.ok(edge.length >= 32);
  }
  const r = T.build(T.parseTable('a,b\nA,B\n'), { format: 'wide', columns: ['a', 'b'] });
  const layout = buildLayout(r, {}, { width: 0.35, height: 0.4, label: 'Node' });
  layout.nodes.forEach((node) => {
    const body = SK.shapes.endpointBody(layout, node, node.outFlows.length ? 'start' : 'end', 0);
    body.lowerBorder.concat(body.upperBorder).forEach((p) => assert.ok(body.polygon.some((q) => q[0] === p[0] && q[1] === p[1])));
    for (const edge of [body.lowerBorder, body.upperBorder]) {
      edge.slice(1).forEach((p, i) => assert.ok(Math.abs(p[0] - edge[i][0]) > 1e-9 || Math.abs(p[1] - edge[i][1]) < 1e-9));
    }
  });
});

test('strand edges meet without gaps or overlap at node faces and sum to the node thickness', () => {
  const r = T.build(T.parseTable(wide), { format: 'wide', columns: ['a', 'b', 'c'], weight: 'n' });
  const layout = buildLayout(r, {}, { width: 0.35, height: 0.4, label: 'Node' });
  layout.nodes.forEach(n => {
    for (const [ids, height, incoming] of [[n.inFlows, n.inPx, true], [n.outFlows, n.outPx, false]]) {
      if (!ids.length) continue;
      const spans = ids.map(k => {
        const f = layout.flows[k];
        const i = incoming ? f.lower.length - 1 : 0;
        return [f.lower[i][1], f.upper[i][1]];
      }).sort((a, b) => a[0] - b[0]);
      spans.slice(1).forEach((s, i) => assert.ok(Math.abs(s[0] - spans[i][1]) < 1e-9));
      assert.ok(Math.abs(spans[spans.length - 1][1] - spans[0][0] - height) < 1e-9);
    }
  });
});

const insidePolygon = ([x, y], polygon) => {
  let inside = false;
  polygon.forEach((b, i) => {
    const a = polygon[(i + polygon.length - 1) % polygon.length];
    if ((a[1] > y) !== (b[1] > y) && x < a[0] + (b[0] - a[0]) * (y - a[1]) / (b[1] - a[1])) inside = !inside;
  });
  return inside;
};

test('wide steep Bezier stages keep ordered edges within node faces in both directions', () => {
  for (const dir of [1, -1]) {
    const layout = SK.layout.computeLayout({ mode: 'layered', matrices: [[[0.5, 0.15]]],
      loc: [{ set: 1, node: 1, x: 0, y: 0.5, width: 0.4 },
        { set: 2, node: 1, x: dir, y: 1.5, width: 0.1 }, { set: 2, node: 2, x: dir, y: -0.5, width: 0.1 }] },
    { xlim: [-1.3, 1.3], ylim: [-0.8, 1.8], width: 800, height: 900 });
    layout.flows.forEach(f => {
      for (const edge of [f.lower, f.upper]) {
        edge.slice(1).forEach((p, i) => assert.ok(dir * (p[0] - edge[i][0]) > 0));
        assert.ok(edge.every(([x]) => dir * (x - f.startX) >= 0 && dir * (f.endX - x) >= 0));
      }
    });
    const flows = layout.flows.slice().sort((a, b) => a.startOffset - b.startOffset);
    for (const t of [0.0001, 0.01, 0.1]) {
      const x = flows[0].startX + (flows[0].endX - flows[0].startX) * t;
      assert.ok(SK.layout.pointAtX(flows[0].upper, x)[1] < SK.layout.pointAtX(flows[1].lower, x)[1], 'adjacent strands separate without overlapping');
    }
  }
});

test('merge fills preserve gaps between separated strands before the merge cutoff', () => {
  const layout = SK.layout.computeLayout({ mode: 'layered', matrices: [[[0.5, 0.15]]],
    loc: [{ set: 1, node: 1, x: 0, y: 0.5, width: 0.4 },
      { set: 2, node: 1, x: 1, y: 1.5, width: 0.1 }, { set: 2, node: 2, x: 1, y: -0.5, width: 0.1 }], params: { merge_frac: 0.2 } },
  { xlim: [-0.3, 1.3], ylim: [-0.8, 1.8], width: 800, height: 900 });
  const source = layout.nodes[0];
  const trunk = SK.shapes.trunkPolygon(layout, source, 'start');
  const [bottom, top] = layout.flows.slice().sort((a, b) => a.startOffset - b.startOffset);
  const x = bottom.startX + layout.mergeLength / 2;
  const lower = SK.layout.pointAtX(bottom.upper, x)[1], upper = SK.layout.pointAtX(top.lower, x)[1];
  assert.ok(upper - lower > 1);
  assert.ok(!trunk.polygons.some(polygon => insidePolygon([x, (lower + upper) / 2], polygon)));
  for (const f of layout.flows) {
    const y = (SK.layout.pointAtX(f.upper, x)[1] + SK.layout.pointAtX(f.lower, x)[1]) / 2;
    assert.ok(trunk.polygons.some(polygon => insidePolygon([x, y], polygon)));
  }
});

test('x clipping preserves every crossing of a folded edge', () => {
  const edge = [[0, 0], [2, 1], [-1, 2], [2, 3], [3, 4]];
  const pieces = SK.shapes.clipEdgeX(edge, 0.5, 1.5);
  assert.equal(pieces.length, 3);
  pieces.forEach(piece => {
    assert.ok(piece.every(([x]) => x >= 0.5 && x <= 1.5));
    assert.equal(Math.abs(piece[0][0] - piece[piece.length - 1][0]), 1);
  });
});
