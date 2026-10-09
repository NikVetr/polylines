/* Derived polygons for drawing: ribbon bodies, merge trunks, node bodies and border paths.
 * Ports the drawing-geometry helpers in R/functions.R. Coordinates are plot pixels (y up).
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const { pointAtX } = SK.layout;
  const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), hi);

  // Port of path_segment_by_x(): the part of a polyline between two x values, in path order.
  function sliceByX(points, x0, x1) {
    const n = points.length;
    const increasing = points[n - 1][0] >= points[0][0];
    const lo = Math.min(x0, x1);
    const hi = Math.max(x0, x1);
    const interior = points.filter((p) => p[0] > lo && p[0] < hi);
    return increasing
      ? [pointAtX(points, lo), ...interior, pointAtX(points, hi)]
      : [pointAtX(points, hi), ...interior, pointAtX(points, lo)];
  }

  // x positions where a strand leaves the start trunk and enters the end trunk, or null.
  function strandCuts(layout, f) {
    const m = layout.mergeLength;
    if (!(m > 0) || !(f.pathLength > 2 * m)) return null;
    return [f.startX + f.dir * m, f.endX - f.dir * m];
  }

  function ribbonPolygon(f, cuts) {
    if (!cuts) return f.upper.concat(f.lower.slice().reverse());
    return sliceByX(f.upper, cuts[0], cuts[1]).concat(sliceByX(f.lower, cuts[0], cuts[1]).reverse());
  }

  function edgeSegments(f, cuts) {
    if (!cuts) return [f.upper, f.lower];
    return [sliceByX(f.upper, cuts[0], cuts[1]), sliceByX(f.lower, cuts[0], cuts[1])];
  }

  // Outermost strands of a node side: side "start" uses strands leaving the node.
  function bundle(layout, node, side) {
    const inds = side === 'start' ? node.outFlows : node.inFlows;
    if (!inds.length) return null;
    const flows = inds.map((k) => layout.flows[k]);
    const dirs = [...new Set(flows.map((f) => f.dir))];
    if (dirs.length !== 1) return null;
    const at = (pts) => (side === 'start' ? pts[0] : pts[pts.length - 1]);
    let bottom = flows[0];
    let top = flows[0];
    flows.forEach((f) => {
      if (at(f.lower)[1] < at(bottom.lower)[1]) bottom = f;
      if (at(f.upper)[1] > at(top.upper)[1]) top = f;
    });
    const faceX = side === 'start' ? flows[0].startX : flows[0].endX;
    return { flows, dir: dirs[0], bottom, top, faceX, lowerY: at(bottom.lower)[1], upperY: at(top.upper)[1] };
  }

  // Edge pieces running from the node face into the strands by `length` in x.
  function trunkEdges(b, side, length) {
    const inward = side === 'start' ? b.dir : -b.dir;
    const target = b.faceX + inward * length;
    const pick = (pts) => (side === 'start' ? sliceByX(pts, pts[0][0], target) : sliceByX(pts, target, pts[pts.length - 1][0]));
    return { lower: pick(b.bottom.lower), upper: pick(b.top.upper) };
  }

  function trunkPolygon(layout, node, side) {
    const b = bundle(layout, node, side);
    if (!b || !(layout.mergeLength > 0)) return null;
    const e = trunkEdges(b, side, layout.mergeLength);
    return { polygon: e.lower.concat(e.upper.slice().reverse()), faceX: b.faceX, dir: b.dir, edges: e, bundle: b };
  }

  // Cosine-eased bridge with horizontal tangents (R node_width_bridge_curve).
  function bridgeCurve(p0, p1, n) {
    if (Math.abs(p1[0] - p0[0]) <= 1e-12 && Math.abs(p1[1] - p0[1]) <= 1e-12) return [p0.slice()];
    // Bound the cosine curve's interpolation error to 0.1 plot pixels.
    const count = Math.max(n, 2 * Math.ceil(Math.sqrt(Math.abs(p1[1] - p0[1]) * Math.PI ** 2 / 1.6) / 2) + 1);
    return Array.from({ length: count }, (_, i) => {
      const t = i / (count - 1);
      const ease = 0.5 - 0.5 * Math.cos(Math.PI * t);
      return [p0[0] * (1 - t) + p1[0] * t, p0[1] * (1 - ease) + p1[1] * ease];
    });
  }

  const labelBodyHalfWidth = (node) => (node.labelWidthPx > 0 && node.widthPx > 0 ? node.widthPx / 2 : 0);

  // R node_width_bridge_curve_body(): through a flat body edge when the node has a body height.
  function bridgeBody(p0, p1, node, side, n) {
    if (!(node.bodyHeightPx > 0)) return bridgeCurve(p0, p1, n);
    const yMid = node.Y + (side === 'lower' ? -1 : 1) * node.bodyHeightPx / 2;
    const x0 = Math.min(p0[0], p1[0]);
    const x1 = Math.max(p0[0], p1[0]);
    // Reserve a shoulder at either face: a full-width plateau creates a vertical step.
    const half = Math.min(labelBodyHalfWidth(node), (x1 - x0) / 4);
    let left = [clamp(node.X - half, x0, x1), yMid];
    let right = [clamp(node.X + half, x0, x1), yMid];
    if (p0[0] > p1[0]) [left, right] = [right, left];
    const n0 = Math.max(16, Math.ceil(n / 3));
    const n2 = Math.max(16, n - 2 * n0 + 2);
    return bridgeCurve(p0, left, n0).concat([right], bridgeCurve(right, p1, n2).slice(1));
  }

  // Body of a source-only or sink-only node with positive width (R endpoint_node_body_polygon).
  // overlap (px) pushes the body's face edge under the merge band so the two antialiased edges
  // do not leave a light seam; borders stay at the true face.
  function endpointBody(layout, node, side, overlap) {
    if (!(node.widthPx > 0)) return null;
    const b = bundle(layout, node, side);
    if (!b) return null;
    const outerX = side === 'start' ? node.X - b.dir * node.widthPx / 2 : node.X + b.dir * node.widthPx / 2;
    let lo = b.lowerY;
    let hi = b.upperY;
    if (node.bodyHeightPx > 0) {
      lo = Math.min(lo, node.Y - node.bodyHeightPx / 2);
      hi = Math.max(hi, node.Y + node.bodyHeightPx / 2);
    }
    const half = node.bodyHeightPx > 0 ? Math.min(labelBodyHalfWidth(node), node.widthPx / 4) : node.widthPx / 2;
    const towardFace = b.faceX > node.X ? 1 : -1;
    const transitionX = clamp(node.X + towardFace * half, Math.min(b.faceX, outerX), Math.max(b.faceX, outerX));
    const fillFaceX = b.faceX + towardFace * Math.max(overlap || 0, 0);
    const lowerBorder = bridgeCurve([b.faceX, b.lowerY], [transitionX, lo], 16).concat([[outerX, lo]]);
    const upperBorder = bridgeCurve([b.faceX, b.upperY], [transitionX, hi], 16).concat([[outerX, hi]]);
    const polygon = [[fillFaceX, b.lowerY], ...lowerBorder, ...upperBorder.slice().reverse(), [fillFaceX, b.upperY]];
    return { polygon, faceX: b.faceX, outerX, lowerBorder, upperBorder, bundle: b };
  }

  // Flow-through node: incoming trunk, bridge across the node body, outgoing trunk.
  function interiorTrunk(layout, node) {
    const bin = bundle(layout, node, 'end');
    const bout = bundle(layout, node, 'start');
    if (!bin || !bout || bin.dir !== bout.dir) return null;
    const m = Math.max(layout.mergeLength, 0);
    const ein = trunkEdges(bin, 'end', m);
    const eout = trunkEdges(bout, 'start', m);
    const lowerBridge = bridgeBody(ein.lower[ein.lower.length - 1], eout.lower[0], node, 'lower', 16);
    const upperBridge = bridgeBody(ein.upper[ein.upper.length - 1], eout.upper[0], node, 'upper', 16);
    const lower = ein.lower.concat(lowerBridge.slice(1), eout.lower.slice(1));
    const upper = ein.upper.concat(upperBridge.slice(1), eout.upper.slice(1));
    const xs = lower.concat(upper).map((p) => p[0]);
    return {
      polygon: lower.concat(upper.slice().reverse()),
      lower,
      upper,
      lowerBridge,
      upperBridge,
      inFaceX: bin.faceX,
      outFaceX: bout.faceX,
      x0: Math.min(...xs),
      x1: Math.max(...xs)
    };
  }

  function nodeRole(node) {
    if (node.inFlows.length && node.outFlows.length) return 'interior';
    if (node.outFlows.length) return 'source';
    if (node.inFlows.length) return 'sink';
    return 'isolated';
  }

  // Bounding box (plot pixels) of every strand edge, for fitting the view.
  function geometryBounds(layout) {
    const box = { x0: Infinity, x1: -Infinity, y0: Infinity, y1: -Infinity };
    const add = (p) => {
      box.x0 = Math.min(box.x0, p[0]);
      box.x1 = Math.max(box.x1, p[0]);
      box.y0 = Math.min(box.y0, p[1]);
      box.y1 = Math.max(box.y1, p[1]);
    };
    layout.flows.forEach((f) => { f.upper.forEach(add); f.lower.forEach(add); });
    layout.nodes.forEach((n) => {
      add([n.X - n.widthPx / 2, n.Y]);
      add([n.X + n.widthPx / 2, n.Y]);
      if (n.bodyHeightPx > 0) {
        add([n.X, n.Y - n.bodyHeightPx / 2]);
        add([n.X, n.Y + n.bodyHeightPx / 2]);
      }
    });
    return box;
  }

  SK.shapes = {
    sliceByX, strandCuts, ribbonPolygon, edgeSegments, bundle, trunkPolygon, trunkEdges,
    bridgeCurve, bridgeBody, endpointBody, interiorTrunk, nodeRole, geometryBounds
  };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
