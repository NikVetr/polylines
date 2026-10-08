/* Individual pathways ("substrands") through a layered Sankey built from item-level data.
 *
 * A path visits one node per node set and carries a weight; with item ids it also lists its
 * members (the items sharing that trajectory). Each strand is split into substrands, one per path
 * through it, and each substrand into its members' slices, in the same order in every strand. A
 * trace follows one path (or member) through its strands, across interior nodes on eased curves
 * from band edge to band edge, and into the bodies of its end nodes.
 * Coordinates are plot pixels (y up), like layout.js.
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const S = SK.shapes;

  // The flow index for (stage, i, j), or -1.
  function flowIndex(layout) {
    const map = new Map(layout.flows.map((f) => [f.key, f.index]));
    return (stage, i, j) => (map.has(`${stage}:${i}:${j}`) ? map.get(`${stage}:${i}:${j}`) : -1);
  }

  // True when the paths reproduce every strand value (so substrands tile the strands exactly).
  function consistent(layout, paths) {
    if (layout.mode !== 'layered' || !paths || !paths.length) return false;
    const stages = layout.stageCount;
    const sums = new Map();
    const find = flowIndex(layout);
    for (const p of paths) {
      if (p.nodes.length !== stages + 1) return false;
      for (let s = 0; s < stages; s++) {
        const k = find(s, p.nodes[s], p.nodes[s + 1]);
        if (k < 0) return false;
        sums.set(k, (sums.get(k) || 0) + p.weight);
      }
    }
    return layout.flows.every((f) => Math.abs((sums.get(f.index) || 0) - f.value) <= 1e-9 * Math.max(1, f.value));
  }

  // Merge band length on strand f, capped like the strand color stops in render.js.
  const bandLength = (layout, f) => Math.min(Math.max(layout.mergeLength, 0), 0.25 * Math.abs(f.endX - f.startX));

  // Split strands into substrands. Within a strand, paths are stacked by where they arrive at the
  // strand's source node (so paths heading into the same strand do not cross inside the node);
  // first-stage strands order by where paths go next. Returns per flow [{ path, a, b }] with
  // a/b = bottom/top fractions of the strand width, per path its flow index per stage, and per
  // path with members the cumulative member fractions of its substrand.
  function substrands(layout, paths) {
    const stages = layout.stageCount;
    const find = flowIndex(layout);
    const byFlow = layout.flows.map(() => []);
    const pathFlows = paths.map((p) => p.nodes.slice(0, -1).map((_, s) => find(s, p.nodes[s], p.nodes[s + 1])));
    const memberCuts = paths.map((p) => {
      if (!p.members) return null;
      let cum = 0;
      const cuts = [0].concat(p.members.map((m) => (cum += m.weight) / p.weight));
      cuts[cuts.length - 1] = 1;
      return cuts;
    });
    const arrivalY = paths.map(() => 0);
    // Heights of the nodes a path visits after stage s, compared stage by stage.
    const later = (s) => (a, b) => {
      for (let t = s + 1; t < stages; t++) {
        const d = layout.nodes[layout.flows[pathFlows[a][t]].to].Y - layout.nodes[layout.flows[pathFlows[b][t]].to].Y;
        if (d) return d;
      }
      return 0;
    };
    for (let s = 0; s < stages; s++) {
      const through = layout.flows.map(() => []);
      paths.forEach((p, pi) => through[pathFlows[pi][s]].push(pi));
      const next = later(s);
      layout.flows.forEach((f) => {
        if (f.stage !== s || !through[f.index].length) return;
        const list = through[f.index];
        list.sort(s === 0
          ? (a, b) => next(a, b) || (a - b)
          : (a, b) => (arrivalY[a] - arrivalY[b]) || next(a, b) || (a - b));
        let cum = 0;
        list.forEach((pi) => {
          const a = cum / f.value;
          cum += paths[pi].weight;
          const b = cum / f.value;
          byFlow[f.index].push({ path: pi, a, b });
          // Centre of this substrand where the strand reaches its target node face.
          arrivalY[pi] = f.endY + f.endOffset - f.w / 2 + f.w * (a + b) / 2;
        });
      });
    }
    return { byFlow, pathFlows, memberCuts };
  }

  const lerpEdge = (f, t) => f.lower.map((p, i) => [p[0] + (f.upper[i][0] - p[0]) * t, p[1] + (f.upper[i][1] - p[1]) * t]);

  // Bottom/top strand fractions of path pi's substrand on flow k, or of one member's slice of it.
  function sliceOf(subs, k, pi, member) {
    const part = subs.byFlow[k].find((e) => e.path === pi);
    const cuts = member >= 0 && subs.memberCuts[pi] ? subs.memberCuts[pi].slice(member, member + 2) : [0, 1];
    return [part.a + (part.b - part.a) * cuts[0], part.a + (part.b - part.a) * cuts[1]];
  }

  // Edges (plot pixels) tracing path pi, or member `member` of it when member >= 0, and the
  // trace's smallest strand thickness.
  function tracePolygon(layout, subs, pi, member) {
    const flows = subs.pathFlows[pi].map((k) => layout.flows[k]);
    const last = flows.length - 1;
    const upper = [];
    const lower = [];
    let thickness = Infinity;
    flows.forEach((f, s) => {
      const [a, b] = sliceOf(subs, f.index, pi, member);
      thickness = Math.min(thickness, f.w * (b - a));
      // Through the merge bands at the diagram's ends, band edge to band edge in between.
      const m = bandLength(layout, f);
      const x0 = s === 0 ? f.startX : f.startX + f.dir * m;
      const x1 = s === last ? f.endX : f.endX - f.dir * m;
      const up = S.sliceByX(lerpEdge(f, b), x0, x1);
      const lo = S.sliceByX(lerpEdge(f, a), x0, x1);
      if (s > 0) {
        // Ease across the node from the previous strand's band edge to this one's.
        const bridge = (from, to) => S.bridgeCurve(from, to, 18).slice(1, -1);
        upper.push(...bridge(upper[upper.length - 1], up[0]));
        lower.push(...bridge(lower[lower.length - 1], lo[0]));
      }
      upper.push(...up);
      lower.push(...lo);
    });
    // On into the end nodes' bodies, as far as those are drawn (a label body fades out at its centre).
    const reach = (n, side) => {
      const body = S.endpointBody(layout, n, side, 0);
      return body ? (n.hasLabel ? n.X : body.outerX) : null;
    };
    const xStart = reach(layout.nodes[flows[0].from], 'start');
    if (xStart !== null) {
      upper.unshift([xStart, upper[0][1]]);
      lower.unshift([xStart, lower[0][1]]);
    }
    const xEnd = reach(layout.nodes[flows[last].to], 'end');
    if (xEnd !== null) {
      upper.push([xEnd, upper[upper.length - 1][1]]);
      lower.push([xEnd, lower[lower.length - 1][1]]);
    }
    return { upper, lower, polygon: upper.concat(lower.slice().reverse()), flows, thickness };
  }

  // The path (and member, or -1) whose substrand lies under plot point (X, Y) on flow k, or null.
  function pathAt(layout, subs, k, X, Y) {
    const f = layout.flows[k];
    const lo = SK.layout.pointAtX(f.lower, X)[1];
    const up = SK.layout.pointAtX(f.upper, X)[1];
    if (Math.abs(up - lo) < 1e-9) return null;
    const t = Math.min(Math.max((Y - lo) / (up - lo), 0), 1);
    const hit = subs.byFlow[k].find((e) => t >= e.a && t <= e.b);
    if (!hit) return null;
    const cuts = subs.memberCuts[hit.path];
    if (!cuts) return { path: hit.path, member: -1 };
    const u = (t - hit.a) / Math.max(hit.b - hit.a, 1e-12);
    const i = cuts.findIndex((c, j) => j > 0 && u <= c);
    return { path: hit.path, member: i > 0 ? i - 1 : cuts.length - 2 };
  }

  // The path (and member) at height Y where strands meet node n, on the side nearer to X.
  function pathAtNode(layout, subs, n, X, Y) {
    const near = X < n.X ? n.inFlows : n.outFlows;
    const ks = near.length ? near : (X < n.X ? n.outFlows : n.inFlows);
    const incoming = ks === n.inFlows;
    let best = null;
    ks.forEach((k) => {
      const f = layout.flows[k];
      const m = bandLength(layout, f);
      const x = incoming ? f.endX - f.dir * m : f.startX + f.dir * m;
      const lo = SK.layout.pointAtX(f.lower, x)[1];
      const up = SK.layout.pointAtX(f.upper, x)[1];
      const d = Y < lo ? lo - Y : Y > up ? Y - up : 0;
      if (!best || d < best.d) best = { d, k, x, y: Math.min(Math.max(Y, lo), up) };
    });
    return best ? pathAt(layout, subs, best.k, best.x, best.y) : null;
  }

  // Per flow, the lines between neighbouring substrands and member slices, band edge to band edge.
  function boundaryLines(layout, subs) {
    return layout.flows.map((f) => {
      const keys = new Set();
      subs.byFlow[f.index].forEach((e) => {
        (subs.memberCuts[e.path] || [0, 1]).forEach((c) => keys.add(Math.round((e.a + (e.b - e.a) * c) * 1e9)));
      });
      const m = bandLength(layout, f);
      return [...keys].filter((key) => key > 0 && key < 1e9).sort((a, b) => a - b).map((key) => {
        const line = S.sliceByX(lerpEdge(f, key / 1e9), f.startX + f.dir * m, f.endX - f.dir * m);
        const step = Math.max(1, Math.ceil(line.length / 48));
        return line.filter((_, i) => i % step === 0 || i === line.length - 1);
      });
    });
  }

  SK.paths = { consistent, substrands, tracePolygon, pathAt, pathAtNode, boundaryLines };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
