/* Individual pathways ("substrands") through a layered Sankey built from item-level data.
 *
 * A path visits one node per node set and carries a weight; with item ids it also lists its
 * members (the items sharing that trajectory). Each strand is split into substrands, one per path
 * through it, and each substrand into its members' slices, in the same order in every strand. A
 * trace follows one path (or member) through its strands, across interior nodes on eased curves
 * from band edge to band edge, and into the bodies of its end nodes; traces of several paths can
 * gather into one contiguous bundle at the centre of each node they share.
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

  // Path pi's (or one member's) slice of each of its strands: band edge to band edge, and through
  // the merge bands to the faces at the diagram's ends.
  function pieces(layout, subs, pi, member) {
    const flows = subs.pathFlows[pi].map((k) => layout.flows[k]);
    return flows.map((f, s) => {
      const [a, b] = sliceOf(subs, f.index, pi, member);
      const m = bandLength(layout, f);
      const x0 = s === 0 ? f.startX : f.startX + f.dir * m;
      const x1 = s === flows.length - 1 ? f.endX : f.endX - f.dir * m;
      return { f, thickness: f.w * (b - a), up: S.sliceByX(lerpEdge(f, b), x0, x1), lo: S.sliceByX(lerpEdge(f, a), x0, x1) };
    });
  }

  const first = (pts) => pts[0];
  const last = (pts) => pts[pts.length - 1];

  // How far a trace runs into an end node's body (to the centre of a fading label body), or null.
  function bodyReach(layout, n, side) {
    const body = S.endpointBody(layout, n, side, 0);
    return body ? (n.hasLabel ? n.X : body.outerX) : null;
  }

  // Places for traced paths gathered into one contiguous bundle at each node they share: per node
  // id, per path, its [lower, upper] y at the node centre. Paths keep the order in which they cross
  // the node, and the bundle is centred on their weighted mean height, within the node's stack.
  function gatherSlots(layout, subs, list) {
    const visits = new Map();
    const add = (id, pi, spans) => {
      if (!visits.has(id)) visits.set(id, []);
      visits.get(id).push({
        pi,
        mid: spans.reduce((t, [lo, up]) => t + lo + up, 0) / (2 * spans.length),
        w: spans.reduce((t, [lo, up]) => t + up - lo, 0) / spans.length
      });
    };
    const span = (lo, up) => [lo[1], up[1]];
    list.forEach((pi) => {
      const ps = pieces(layout, subs, pi, -1);
      add(ps[0].f.from, pi, [span(first(ps[0].lo), first(ps[0].up))]);
      for (let s = 1; s < ps.length; s++) {
        add(ps[s].f.from, pi, [span(last(ps[s - 1].lo), last(ps[s - 1].up)), span(first(ps[s].lo), first(ps[s].up))]);
      }
      add(last(ps).f.to, pi, [span(last(last(ps).lo), last(last(ps).up))]);
    });
    const slots = new Map();
    visits.forEach((items, id) => {
      const n = layout.nodes[id];
      const total = items.reduce((t, e) => t + e.w, 0);
      const centre = items.reduce((t, e) => t + e.mid * e.w, 0) / total;
      const half = Math.max(n.inPx, n.outPx) / 2;
      let y = Math.min(Math.max(centre - total / 2, n.Y - half), n.Y + half - total);
      const places = new Map();
      items.sort((a, b) => (a.mid - b.mid) || (a.pi - b.pi)).forEach((e) => {
        places.set(e.pi, [y, y + e.w]);
        y += e.w;
      });
      slots.set(id, places);
    });
    return slots;
  }

  // Edges (plot pixels) tracing path pi, or member `member` of it when member >= 0, and the
  // trace's smallest strand thickness. With slots (gatherSlots), the trace eases into its place in
  // each node's gathered bundle, holds it through the middle third, and eases back out.
  function tracePolygon(layout, subs, pi, member, slots) {
    const ps = pieces(layout, subs, pi, member);
    const ease = (from, to) => S.bridgeCurve(from, to, 18);
    const placeAt = (id, k) => (slots ? slots.get(id).get(pi)[k] : undefined);
    // Points strictly between the band-edge points a and b of consecutive strands.
    const cross = (a, b, y) => {
      if (y === undefined) return ease(a, b).slice(1, -1);
      const xc = (a[0] + b[0]) / 2;
      const hold = (b[0] - a[0]) / 6;
      return ease(a, [xc - hold, y]).slice(1).concat(ease([xc + hold, y], b).slice(0, -1));
    };
    // Points from a strand's end into the body up to x = reach (excluding the end point): level, or
    // eased into the gathered place by 40% of the way from the face to reach and level after.
    const intoBody = (end, faceX, reach, y) => {
      if (y === undefined) return [[reach, end[1]]];
      return ease(end, [faceX + (reach - faceX) * 0.4, y]).slice(1).concat([[reach, y]]);
    };
    const from = layout.nodes[ps[0].f.from];
    const to = layout.nodes[last(ps).f.to];
    const xStart = bodyReach(layout, from, 'start');
    const xEnd = bodyReach(layout, to, 'end');
    // Gathering in an end node starts at the band edge, so the bundle forms across band and body.
    const trim = (p, x0, x1) => {
      p.up = S.sliceByX(p.up, x0, x1);
      p.lo = S.sliceByX(p.lo, x0, x1);
    };
    if (slots && xStart !== null) trim(ps[0], ps[0].f.startX + ps[0].f.dir * bandLength(layout, ps[0].f), last(ps[0].up)[0]);
    if (slots && xEnd !== null) trim(last(ps), first(last(ps).up)[0], last(ps).f.endX - last(ps).f.dir * bandLength(layout, last(ps).f));
    const upper = [];
    const lower = [];
    ps.forEach((p, s) => {
      if (s > 0) {
        upper.push(...cross(last(upper), first(p.up), placeAt(p.f.from, 1)));
        lower.push(...cross(last(lower), first(p.lo), placeAt(p.f.from, 0)));
      }
      upper.push(...p.up);
      lower.push(...p.lo);
    });
    if (xStart !== null) {
      upper.unshift(...intoBody(first(upper), ps[0].f.startX, xStart, placeAt(from.id, 1)).reverse());
      lower.unshift(...intoBody(first(lower), ps[0].f.startX, xStart, placeAt(from.id, 0)).reverse());
    }
    if (xEnd !== null) {
      upper.push(...intoBody(last(upper), last(ps).f.endX, xEnd, placeAt(to.id, 1)));
      lower.push(...intoBody(last(lower), last(ps).f.endX, xEnd, placeAt(to.id, 0)));
    }
    return {
      upper,
      lower,
      polygon: upper.concat(lower.slice().reverse()),
      flows: ps.map((p) => p.f),
      thickness: Math.min(...ps.map((p) => p.thickness))
    };
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

  SK.paths = { consistent, substrands, gatherSlots, tracePolygon, pathAt, pathAtNode, boundaryLines };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
