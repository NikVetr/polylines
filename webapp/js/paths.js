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

  const nodeShapes = new WeakMap();
  function nodeShape(layout, n) {
    if (!nodeShapes.has(layout)) nodeShapes.set(layout, new Map());
    const cache = nodeShapes.get(layout);
    if (!cache.has(n.id)) {
      const role = S.nodeRole(n);
      const shape = role === 'interior' ? S.interiorTrunk(layout, n) : S.endpointBody(layout, n, role === 'source' ? 'start' : 'end', 0);
      cache.set(n.id, shape && (role === 'interior' ? shape : { lower: shape.lowerBorder, upper: shape.upperBorder }));
    }
    return cache.get(n.id);
  }

  function bodyEdge(layout, n, end, reach) {
    return Math.abs(reach - end[0]) < 1e-9 ? [] : [[reach, end[1]]];
  }

  function nodeSlots(layout, subs, n) {
    if (!subs.nodeSlots) subs.nodeSlots = new Map();
    if (!subs.nodeSlots.has(n.id)) {
      const incoming = n.inFlows.length > 0;
      const entries = (incoming ? n.inFlows : n.outFlows).flatMap((k) => {
        const f = layout.flows[k];
        return subs.byFlow[k].map((p) => ({ path: p.path, weight: f.value * (p.b - p.a),
          y: (incoming ? f.endY + f.endOffset : f.startY + f.startOffset) + f.w * ((p.a + p.b) / 2 - 0.5) }));
      }).sort((a, b) => a.y - b.y || a.path - b.path);
      const total = entries.reduce((sum, p) => sum + p.weight, 0);
      let cum = 0;
      subs.nodeSlots.set(n.id, new Map(entries.map((p, i) => {
        const a = cum / total;
        cum += p.weight;
        return [p.path, { a, b: i === entries.length - 1 ? 1 : cum / total }];
      })));
    }
    return subs.nodeSlots.get(n.id);
  }

  // Constant normal width through the body, with horizontal tangents at the faces: one S-curve
  // from the in-face to the out-face, so substrands start to part as they enter the node; gathered
  // paths bend through their bundle place at the centre instead. Members share the same centreline
  // and curve samples. tiled: instead ease every edge vertically on one shared curve, so
  // substrands that keep their order through the node never overlap, though they thin where steep.
  function packedBridge(layout, subs, n, pi, member, a, b, edge, gathered, tiled) {
    // Keep the real slice through each merge band: the bundle envelope can cut across
    // other strands, producing a notch where the trace joins its ribbon.
    let incoming = [a], outgoing = [b];
    const fs = subs.pathFlows[pi].map(k => layout.flows[k]);
    const f = fs.find(f => f.to === n.id), g = fs.find(f => f.from === n.id);
    if (n.widthPx > 0) {
      incoming = S.sliceByX(lerpEdge(f, sliceOf(subs, f.index, pi, member)[edge]), a[0], f.endX);
      outgoing = S.sliceByX(lerpEdge(g, sliceOf(subs, g.index, pi, member)[edge]), g.startX, b[0]);
      a = last(incoming);
      b = first(outgoing);
    }
    const cuts = member >= 0 && subs.memberCuts[pi] ? subs.memberCuts[pi].slice(member, member + 2) : [0, 1];
    const centerY = gathered ? (gathered[0] + gathered[1]) / 2 : null;
    if (tiled) {
      const part = subs.byFlow[f.index].find((p) => p.path === pi);
      const width = f.w * (part.b - part.a);
      const center = [n.X, centerY + width * (cuts[edge] - 0.5)];
      const body = gathered ? S.bridgeCurve(a, center, 25).concat(S.bridgeCurve(center, b, 25).slice(1)) : S.bridgeCurve(a, b, 49);
      return incoming.slice(0, -1).concat(body, outgoing.slice(1));
    }
    if (!subs.nodeRibbons) subs.nodeRibbons = new Map();
    const key = `${n.id}:${pi}:${gathered ? centerY : 'direct'}`;
    if (!subs.nodeRibbons.has(key)) {
      const part = subs.byFlow[f.index].find(p => p.path === pi);
      const width = f.w * (part.b - part.a);
      const mid = (flow, x) => {
        const [lo, up] = sliceOf(subs, flow.index, pi, -1);
        return SK.layout.pointAtX(lerpEdge(flow, (lo + up) / 2), x);
      };
      const center = [n.X, centerY];
      const legs = gathered ? [[mid(f, a[0]), center], [center, mid(g, b[0])]] : [[mid(f, a[0]), mid(g, b[0])]];
      const parts = legs.map(([p, q]) => SK.layout.roundedRibbon(p, q, width, 60));
      const join = (side) => parts.flatMap((r, i) => r[side].slice(i ? 1 : 0));
      subs.nodeRibbons.set(key, { lower: join('lower'), upper: join('upper') });
    }
    const ribbon = subs.nodeRibbons.get(key);
    const t = cuts[edge];
    const body = ribbon.lower.map((lo, i) => [lo[0] * (1 - t) + ribbon.upper[i][0] * t, lo[1] * (1 - t) + ribbon.upper[i][1] * t]);
    return incoming.slice(0, -1).concat(body, outgoing.slice(1));
  }

  // Interior nodes where traced units ([path, member] pairs) that keep their order through the node
  // would overlap as separate constant-width ribbons (beyond `tolerance` px), or where such ribbons
  // cannot turn at all (too tight a bend). Traces cross those nodes on packedBridge's shared ease;
  // pairs that swap order must cross either way and are ignored.
  function tiledNodes(layout, subs, units, opts) {
    const { slots = null, tolerance = 0.5 } = opts || {};
    const SAMPLES = 40;
    const tiled = new Set();
    const byNode = new Map();
    units.forEach(([pi, mi]) => {
      const ps = pieces(layout, subs, pi, mi);
      for (let s = 1; s < ps.length; s++) {
        const n = layout.nodes[ps[s].f.from];
        if (tiled.has(n.id)) continue;
        const gathered = slots && slots.get(n.id).get(pi);
        const edge = (k) => packedBridge(layout, subs, n, pi, mi, last(ps[s - 1][k ? 'up' : 'lo']), first(ps[s][k ? 'up' : 'lo']), k, gathered, false);
        let lower;
        let upper;
        try {
          lower = edge(0);
          upper = edge(1);
        } catch (err) {
          if (!(err instanceof SK.layout.SankeyError) || !/constant-width/i.test(err.message)) throw err;
          tiled.add(n.id);
          continue;
        }
        if (!byNode.has(n.id)) byNode.set(n.id, []);
        byNode.get(n.id).push({ lower, upper });
      }
    });
    byNode.forEach((items, id) => {
      if (items.length < 2 || tiled.has(id)) return;
      const n = layout.nodes[id];
      // The node body, or the bridge between band edges for a node without width.
      const x0 = n.widthPx > 0 ? n.X - n.widthPx / 2 : Math.max(...items.map((e) => e.lower[0][0]));
      const x1 = n.widthPx > 0 ? n.X + n.widthPx / 2 : Math.min(...items.map((e) => last(e.lower)[0]));
      const xs = Array.from({ length: SAMPLES + 1 }, (_, k) => x0 + ((x1 - x0) * k) / SAMPLES);
      const spans = items.map((e) => xs.map((x) => [SK.layout.pointAtX(e.lower, x)[1], SK.layout.pointAtX(e.upper, x)[1]]));
      const order = (i, j, k) => Math.sign(spans[i][k][0] + spans[i][k][1] - spans[j][k][0] - spans[j][k][1]);
      const collide = spans.some((_, i) => spans.some((__, j) => {
        if (j <= i) return false;
        // Same order at both faces and at the centre: these two need not cross.
        const o = order(i, j, 0);
        if (!o || order(i, j, SAMPLES) !== o || order(i, j, SAMPLES / 2) !== o) return false;
        return xs.some((_x, k) => Math.min(spans[i][k][1], spans[j][k][1]) - Math.max(spans[i][k][0], spans[j][k][0]) > tolerance);
      }));
      if (collide) tiled.add(id);
    });
    return tiled;
  }

  // Central label positions along a substrand, with local tangents and widths.
  function labelAnchor(layout, f, a, b) {
    const m = bandLength(layout, f);
    const center = S.sliceByX(lerpEdge(f, (a + b) / 2), f.startX + f.dir * m, f.endX - f.dir * m);
    const lengths = center.slice(1).map((p, i) => Math.hypot(p[0] - center[i][0], p[1] - center[i][1]));
    const length = lengths.reduce((t, v) => t + v, 0);
    const at = (fraction) => {
      let remaining = length * fraction;
      let i = 0;
      while (i < lengths.length - 1 && remaining > lengths[i]) remaining -= lengths[i++];
      const p = center[i];
      const q = center[i + 1];
      const t = lengths[i] > 0 ? remaining / lengths[i] : 0;
      const X = p[0] + (q[0] - p[0]) * t;
      const Y = p[1] + (q[1] - p[1]) * t;
      const angle = Math.atan2(q[1] - p[1], q[0] - p[0]);
      const width = Math.abs(SK.layout.pointAtX(f.upper, X)[1] - SK.layout.pointAtX(f.lower, X)[1]);
      return { X, Y, angle, thickness: width * (b - a) * Math.abs(Math.cos(angle)) };
    };
    return Object.assign({ flow: f.index, length, alternatives: [0.4, 0.6, 0.3, 0.7].map(at) }, at(0.5));
  }

  // How far a trace runs into an end node's body (to the centre of a fading label body), or null.
  function bodyReach(layout, n, side, fadeLabels) {
    const body = S.endpointBody(layout, n, side, 0);
    return body ? (fadeLabels && n.hasLabel ? n.X : body.outerX) : null;
  }

  // Places for traced paths gathered into one contiguous bundle at each node they share: per node
  // id, per path, its [lower, upper] y at the node centre. Paths keep the order in which they cross
  // the node, and the bundle lines up with the node's top or bottom edge, leaving the rest of the
  // node's height in one piece so bundles are easy to compare.
  function gatherSlots(layout, subs, list) {
    const visits = new Map();
    const add = (id, pi, spans) => {
      if (!visits.has(id)) visits.set(id, []);
      const f = layout.flows[subs.pathFlows[pi][0]], part = subs.byFlow[f.index].find(p => p.path === pi);
      visits.get(id).push({
        pi,
        mid: spans.reduce((t, [lo, up]) => t + lo + up, 0) / (2 * spans.length),
        w: f.w * (part.b - part.a)
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
      const [lo, hi] = nodeExtent(layout, layout.nodes[id]);
      const total = items.reduce((t, e) => t + e.w, 0);
      items.sort((a, b) => (a.mid - b.mid) || (a.pi - b.pi));
      const stack = (y0) => {
        let y = y0;
        return items.map((e) => [y, (y += e.w)]);
      };
      // Against the node's top or its bottom edge, whichever moves the paths less.
      const moved = (spans) => spans.reduce((t, [a, b], i) => t + items[i].w * Math.abs((a + b) / 2 - items[i].mid), 0);
      const top = stack(hi - total);
      const bottom = stack(lo);
      const spans = moved(top) <= moved(bottom) ? top : bottom;
      slots.set(id, new Map(items.map((e, i) => [e.pi, spans[i]])));
    });
    return slots;
  }

  // Bottom and top y of node n at its centre: across the trunk for interior nodes, the strand stack
  // at the face for end nodes.
  function nodeExtent(layout, n) {
    const role = S.nodeRole(n);
    if (role === 'interior') {
      return [n.Y - n.inPx / 2, n.Y + n.inPx / 2];
    } else {
      const b = S.bundle(layout, n, role === 'source' ? 'start' : 'end');
      if (b) return [b.lowerY, b.upperY];
    }
    const half = Math.max(n.inPx, n.outPx) / 2;
    return [n.Y - half, n.Y + half];
  }

  // Edges (plot pixels) tracing path pi, and the trace's smallest strand thickness. Options:
  // member (>= 0: just that member's slice), slots (gatherSlots: bend through the path's place in
  // each node's gathered bundle at the node centre), tiled (tiledNodes: node ids crossed on the
  // shared ease) and fadeLabels (labeled end-node bodies fade out at their centre, so traces stop there).
  function tracePolygon(layout, subs, pi, opts) {
    const { member = -1, slots = null, tiled = null, fadeLabels = true } = opts || {};
    const ps = pieces(layout, subs, pi, member);
    const placeAt = (id, k) => (slots ? slots.get(id).get(pi)[k] : undefined);
    const cross = (n, a, b, edge) => {
      return packedBridge(layout, subs, n, pi, member, a, b, edge, slots && slots.get(n.id).get(pi), !!(tiled && tiled.has(n.id))).slice(1, -1);
    };
    const intoBody = (n, end, reach, y, edge) => {
      if (y !== undefined && Math.abs(reach - end[0]) > 1e-9) {
        const width = ps[0].thickness, offset = (edge - 0.5) * width;
        const curve = SK.layout.roundedRibbon([end[0], end[1] - offset], [reach, y - offset], width, 60);
        return (edge ? curve.upper : curve.lower).slice(1);
      }
      return bodyEdge(layout, n, end, reach);
    };
    const from = layout.nodes[ps[0].f.from];
    const to = layout.nodes[last(ps).f.to];
    const faceStart = ps[0].f.startX;
    const faceEnd = last(ps).f.endX;
    const reachStart = bodyReach(layout, from, 'start', fadeLabels) ?? faceStart;
    const reachEnd = bodyReach(layout, to, 'end', fadeLabels) ?? faceEnd;
    const upper = [];
    const lower = [];
    ps.forEach((p, s) => {
      if (s > 0) {
        upper.push(...cross(layout.nodes[p.f.from], last(upper), first(p.up), 1));
        lower.push(...cross(layout.nodes[p.f.from], last(lower), first(p.lo), 0));
      }
      upper.push(...p.up);
      lower.push(...p.lo);
    });
    upper.unshift(...intoBody(from, first(upper), reachStart, placeAt(from.id, 1), 1).reverse());
    lower.unshift(...intoBody(from, first(lower), reachStart, placeAt(from.id, 0), 0).reverse());
    upper.push(...intoBody(to, last(upper), reachEnd, placeAt(to.id, 1), 1));
    lower.push(...intoBody(to, last(lower), reachEnd, placeAt(to.id, 0), 0));
    const rightmost = [from, ...ps.map((p) => layout.nodes[p.f.to])]
      .reduce((a, b) => (a.X + a.widthPx / 2 >= b.X + b.widthPx / 2 ? a : b));
    const rightX = rightmost.X + rightmost.widthPx / 2;
    return {
      upper,
      lower,
      polygon: upper.concat(lower.slice().reverse()),
      flows: ps.map((p) => p.f),
      thickness: Math.min(...ps.map((p) => p.thickness)),
      labelAnchors: ps.map((p) => {
        const [a, b] = sliceOf(subs, p.f.index, pi, member);
        return labelAnchor(layout, p.f, a, b);
      }),
      countAnchor: { node: rightmost.id, X: rightX,
        Y: (SK.layout.pointAtX(upper, rightX)[1] + SK.layout.pointAtX(lower, rightX)[1]) / 2 }
    };
  }

  // The path (and member, or -1) whose substrand lies under plot point (X, Y) on flow k, or null.
  function fractionAt(f, X, Y) {
    // Offset edges can have different x coordinates. Invert the actual interpolated slice,
    // rather than a vertical fraction that compresses hit positions on steep curves.
    let a = 0, b = 1;
    for (let i = 0; i < 24; i++) {
      const mid = (a + b) / 2;
      if (SK.layout.pointAtX(lerpEdge(f, mid), X)[1] < Y) a = mid;
      else b = mid;
    }
    return (a + b) / 2;
  }

  function pathAt(layout, subs, k, X, Y) {
    const f = layout.flows[k];
    const lo = SK.layout.pointAtX(f.lower, X)[1];
    const up = SK.layout.pointAtX(f.upper, X)[1];
    if (Math.abs(up - lo) < 1e-9) return null;
    const t = fractionAt(f, X, Y);
    const hit = subs.byFlow[k].find((e) => t >= e.a && t <= e.b);
    if (!hit) return null;
    const cuts = subs.memberCuts[hit.path];
    if (!cuts) return { path: hit.path, member: -1 };
    const u = (t - hit.a) / Math.max(hit.b - hit.a, 1e-12);
    const i = cuts.findIndex((c, j) => j > 0 && u <= c);
    return { path: hit.path, member: i > 0 ? i - 1 : cuts.length - 2 };
  }

  // Cache the local rendered slices once per node/layout; modifier hover only searches these.
  function nodeSlices(layout, subs, n) {
    if (!subs.nodeSlices) subs.nodeSlices = new Map();
    if (subs.nodeSlices.has(n.id)) return subs.nodeSlices.get(n.id);
    const paths = [...new Set(n.inFlows.concat(n.outFlows).flatMap((k) => subs.byFlow[k].map((p) => p.path)))];
    const at = (f, t, x) => SK.layout.pointAtX(lerpEdge(f, t), x);
    const slices = paths.map((pi) => {
      const ks = subs.pathFlows[pi];
      let lower, upper, body = null;
      if (n.inFlows.length && n.outFlows.length) {
        const f = layout.flows[ks[n.set - 2]];
        const g = layout.flows[ks[n.set - 1]];
        const [a, b] = sliceOf(subs, f.index, pi, -1);
        const [c, d] = sliceOf(subs, g.index, pi, -1);
        const x = f.endX - f.dir * bandLength(layout, f);
        const xx = g.startX + g.dir * bandLength(layout, g);
        lower = packedBridge(layout, subs, n, pi, -1, at(f, a, x), at(g, c, xx), 0);
        upper = packedBridge(layout, subs, n, pi, -1, at(f, b, x), at(g, d, xx), 1);
        body = subs.nodeRibbons.get(`${n.id}:${pi}:direct`);
      } else {
        const source = n.outFlows.length > 0;
        const f = layout.flows[source ? first(ks) : last(ks)];
        const [a, b] = sliceOf(subs, f.index, pi, -1);
        const face = source ? f.startX : f.endX;
        const edge = face + (source ? 1 : -1) * f.dir * bandLength(layout, f);
        const reach = bodyReach(layout, n, source ? 'start' : 'end', false) ?? face;
        const local = (t) => {
          const body = bodyEdge(layout, n, at(f, t, face), reach);
          const flow = S.sliceByX(lerpEdge(f, t), face, edge);
          return source ? body.reverse().concat(flow) : flow.concat(body);
        };
        lower = local(a); upper = local(b);
      }
      return { path: pi, lower, upper, body };
    });
    subs.nodeSlices.set(n.id, slices);
    return slices;
  }

  // Resolve the slice at the pointer's actual x, including widened node bodies.
  function pathAtNode(layout, subs, n, X, Y) {
    const shape = nodeShape(layout, n);
    // An end node's body has constant fractions: avoid constructing every path's slice on hover.
    const dir = layout.flows[n.outFlows[0] ?? n.inFlows[0]].dir;
    if (shape && Math.abs(X - n.X) <= n.widthPx / 4 + 1e-9 && (!n.inFlows.length || !n.outFlows.length)) {
      const height = Math.max(n.inPx, n.outPx);
      const lo = n.Y - height / 2, up = n.Y + height / 2;
      const t = Math.min(Math.max((Y - lo) / Math.max(up - lo, 1e-12), 0), 1);
      for (const [pi, slot] of nodeSlots(layout, subs, n)) {
        if (t < slot.a || t > slot.b) continue;
        const cuts = subs.memberCuts[pi];
        const u = (t - slot.a) / Math.max(slot.b - slot.a, 1e-12);
        const i = cuts ? cuts.findIndex((c, j) => j > 0 && u <= c) : -1;
        return { path: pi, member: cuts ? (i > 0 ? i - 1 : cuts.length - 2) : -1 };
      }
      return null;
    }
    let best = null;
    nodeSlices(layout, subs, n).forEach((slice) => {
      const lo = SK.layout.pointAtX(slice.lower, X)[1];
      const up = SK.layout.pointAtX(slice.upper, X)[1];
      const d = Y < lo ? lo - Y : Y > up ? Y - up : 0;
      if (!best || d < best.d) {
        const withinBody = slice.body && Math.abs(X - n.X) <= n.widthPx / 2;
        let t = withinBody ? fractionAt(slice.body, X, Y) : (Y - lo) / Math.max(up - lo, 1e-12);
        if (Math.abs(X - n.X) > n.widthPx / 2) {
          const stage = n.inFlows.length && (!n.outFlows.length || dir * (X - n.X) < 0) ? n.set - 2 : n.set - 1;
          const f = layout.flows[subs.pathFlows[slice.path][stage]], part = subs.byFlow[f.index].find(p => p.path === slice.path);
          t = (fractionAt(f, X, Y) - part.a) / (part.b - part.a);
        }
        best = { d, path: slice.path, t: Math.min(Math.max(t, 0), 1) };
      }
    });
    if (!best) return null;
    const cuts = subs.memberCuts[best.path];
    const i = cuts ? cuts.findIndex((c, j) => j > 0 && best.t <= c) : -1;
    return { path: best.path, member: cuts ? (i > 0 ? i - 1 : cuts.length - 2) : -1 };
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

  SK.paths = { consistent, substrands, gatherSlots, tiledNodes, tracePolygon, pathAt, pathAtNode, boundaryLines };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
