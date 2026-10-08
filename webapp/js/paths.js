/* Individual pathways ("substrands") through a layered Sankey built from item-level data.
 *
 * A path visits one node per node set and carries a weight. Each strand is split into substrands,
 * one per path through it, stacked by weight; a path's trace is its substrand in every strand,
 * joined across nodes by eased curves that run from band edge to band edge.
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

  // Split strands into substrands. Within a strand, paths are stacked by where they arrive at the
  // strand's source node (so paths heading into the same strand do not cross inside the node);
  // first-stage strands order by where paths go next. Returns per flow [{ path, a, b }] with
  // a/b = bottom/top fractions of the strand width, plus per path its flow index per stage.
  function substrands(layout, paths) {
    const stages = layout.stageCount;
    const find = flowIndex(layout);
    const byFlow = layout.flows.map(() => []);
    const pathFlows = paths.map((p) => p.nodes.slice(0, -1).map((_, s) => find(s, p.nodes[s], p.nodes[s + 1])));
    const arrivalY = paths.map(() => 0);
    for (let s = 0; s < stages; s++) {
      const members = layout.flows.map(() => []);
      paths.forEach((p, pi) => members[pathFlows[pi][s]].push(pi));
      layout.flows.forEach((f) => {
        if (f.stage !== s || !members[f.index].length) return;
        const list = members[f.index];
        const nextY = (pi) => (s + 1 < stages ? layout.nodes[layout.flows[pathFlows[pi][s + 1]].to].Y : 0);
        list.sort(s === 0
          ? (a, b) => (nextY(a) - nextY(b)) || (a - b)
          : (a, b) => (arrivalY[a] - arrivalY[b]) || (nextY(a) - nextY(b)) || (a - b));
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
    return { byFlow, pathFlows };
  }

  const lerpEdge = (f, t) => f.lower.map((p, i) => [p[0] + (f.upper[i][0] - p[0]) * t, p[1] + (f.upper[i][1] - p[1]) * t]);

  // Polygon (plot pixels) tracing one path, and the x extent of each of its strands.
  function tracePolygon(layout, subs, pi) {
    const m = Math.max(layout.mergeLength, 0);
    const flows = subs.pathFlows[pi].map((k) => layout.flows[k]);
    const upper = [];
    const lower = [];
    const spans = [];
    flows.forEach((f, s) => {
      const part = subs.byFlow[f.index].find((e) => e.path === pi);
      // Strands run face to face at the ends of the diagram, band edge to band edge in between.
      const x0 = s === 0 ? f.startX : f.startX + f.dir * m;
      const x1 = s === flows.length - 1 ? f.endX : f.endX - f.dir * m;
      const up = S.sliceByX(lerpEdge(f, part.b), x0, x1);
      const lo = S.sliceByX(lerpEdge(f, part.a), x0, x1);
      if (s > 0) {
        // Ease across the node from the previous strand's band edge to this one's.
        const bridge = (from, to) => S.bridgeCurve(from, to, 18).slice(1, -1);
        upper.push(...bridge(upper[upper.length - 1], up[0]));
        lower.push(...bridge(lower[lower.length - 1], lo[0]));
      }
      upper.push(...up);
      lower.push(...lo);
      spans.push([x0, x1]);
    });
    return { polygon: upper.concat(lower.reverse()), spans, flows };
  }

  // The path whose substrand lies under plot point (X, Y) on flow k, or -1.
  function pathAt(layout, subs, k, X, Y) {
    const f = layout.flows[k];
    const lo = SK.layout.pointAtX(f.lower, X)[1];
    const up = SK.layout.pointAtX(f.upper, X)[1];
    if (Math.abs(up - lo) < 1e-9) return -1;
    const t = Math.min(Math.max((Y - lo) / (up - lo), 0), 1);
    const hit = subs.byFlow[k].find((e) => t >= e.a && t <= e.b);
    return hit ? hit.path : -1;
  }

  SK.paths = { consistent, substrands, tracePolygon, pathAt };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
