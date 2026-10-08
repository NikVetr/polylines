/* Sankey layout core: a port of the geometry in R/functions.R.
 *
 * All geometry is computed in "plot pixels": x to the right, y UP, with equal
 * pixel scaling on both axes. Strand thicknesses (in x data units, as in R)
 * are therefore isotropic and R's aspect-ratio correction `xyr` is always 1.
 * Data coordinates map to plot pixels through `makeTransform(view)`.
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};

  class SankeyError extends Error {
    constructor(message) {
      super(message);
      this.name = 'SankeyError';
    }
  }
  const fail = (message) => { throw new SankeyError(message); };

  const SOURCE_ORDERS = [
    'target_y', 'target_y_desc', 'target_index',
    'target_projected', 'target_projected_desc',
    'target_bezier', 'target_bezier_desc'
  ];
  const TARGET_ORDERS = [
    'source_y', 'source_y_desc', 'source_index',
    'source_projected', 'source_projected_desc',
    'source_bezier', 'source_bezier_desc'
  ];
  const CHOICES = {
    route: ['bezier', 'boundary'],
    boundary_group: ['target', 'source', 'stage'],
    boundary_compensation: ['none', 'full', 'gap_budget'],
    adjust_node_height: ['none', 'label', 'node'],
    balance: ['allow', 'warn', 'error'],
    source_order: SOURCE_ORDERS,
    target_order: TARGET_ORDERS
  };
  // Parameters that may be overridden per stage, as list-valued arguments in R.
  const STAGE_KEYS = ['route', 'boundary_group', 'boundary_compensation', 'boundary_ease',
    'source_order', 'target_order'];
  const DEFAULT_PARAMS = {
    n: 120,
    curvature: 0.45,
    merge_frac: 0.05,
    merge_length: null,
    default_node_width: 0,
    node_label_pad: 0,
    node_label_pad_frac: 0.04,
    adjust_node_height: 'none',
    node_label_vpad: 0,
    node_label_vpad_frac: 0.25,
    adjust_loc: false,
    min_node_gap: 0,
    adjust_loc_expand: true,
    route: 'bezier',
    boundary_group: 'target',
    boundary_compensation: 'none',
    boundary_ease: 1,
    source_order: 'target_y',
    target_order: 'source_y',
    require_forward: true,
    balance: 'allow',
    balance_tol: 1e-8
  };
  const PX_TOL = 1e-6;

  // ---------- small numeric helpers ----------

  const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), hi);
  const range = (a) => Math.max(...a) - Math.min(...a);
  const linspace = (n) => Array.from({ length: n }, (_, i) => (n === 1 ? 0 : i / (n - 1)));

  function median(values) {
    if (!values.length) return 0;
    const s = values.slice().sort((a, b) => a - b);
    const m = Math.floor(s.length / 2);
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
  }

  function toNumber(v) {
    if (v === null || v === undefined || (typeof v === 'string' && v.trim() === '')) return NaN;
    return Number(v);
  }

  // Pool-adjacent-violators isotonic (non-decreasing) regression, unit weights.
  function isotonic(values) {
    const blocks = [];
    values.forEach((v) => {
      blocks.push({ sum: v, count: 1 });
      while (blocks.length > 1) {
        const b = blocks[blocks.length - 1];
        const a = blocks[blocks.length - 2];
        if (a.sum / a.count <= b.sum / b.count) break;
        a.sum += b.sum;
        a.count += b.count;
        blocks.pop();
      }
    });
    const out = [];
    blocks.forEach((b) => { for (let i = 0; i < b.count; i++) out.push(b.sum / b.count); });
    return out;
  }

  function polylineLength(points) {
    let total = 0;
    for (let i = 1; i < points.length; i++) {
      total += Math.hypot(points[i][0] - points[i - 1][0], points[i][1] - points[i - 1][1]);
    }
    return total;
  }

  // ---------- parameters and coordinates ----------

  function checkParams(p, where) {
    const at = where ? ` (${where})` : '';
    Object.keys(CHOICES).forEach((key) => {
      if (p[key] !== undefined && !CHOICES[key].includes(p[key])) {
        fail(`${key}${at} must be one of: ${CHOICES[key].join(', ')}.`);
      }
    });
    const nonNegative = ['merge_frac', 'default_node_width', 'node_label_pad', 'node_label_pad_frac',
      'node_label_vpad', 'node_label_vpad_frac', 'min_node_gap', 'balance_tol'];
    nonNegative.forEach((key) => {
      if (p[key] !== undefined && !(Number.isFinite(p[key]) && p[key] >= 0)) {
        fail(`${key}${at} must be a non-negative number.`);
      }
    });
    if (p.n !== undefined && !(Number.isInteger(p.n) && p.n >= 2)) fail(`n${at} must be an integer >= 2.`);
    if (p.curvature !== undefined && !Number.isFinite(p.curvature)) fail(`curvature${at} must be a finite number.`);
    if (p.merge_length !== undefined && p.merge_length !== null &&
        !(Number.isFinite(p.merge_length) && p.merge_length >= 0)) {
      fail(`merge_length${at} must be empty or a non-negative number.`);
    }
    if (p.boundary_ease !== undefined && !(Number.isFinite(p.boundary_ease) && p.boundary_ease > 0)) {
      fail(`boundary_ease${at} must be a positive number.`);
    }
  }

  function stageParams(params, stageOverrides, stage) {
    const out = {};
    STAGE_KEYS.forEach((key) => { out[key] = params[key]; });
    const override = stageOverrides && stageOverrides[stage];
    if (override) {
      Object.keys(override).forEach((key) => {
        if (!STAGE_KEYS.includes(key)) fail(`"${key}" cannot be set per stage.`);
        out[key] = override[key];
      });
      checkParams(out, `stage ${stage + 1}`);
    }
    return out;
  }

  function makeTransform(view) {
    const sx = view.width / (view.xlim[1] - view.xlim[0]);
    const sy = view.height / (view.ylim[1] - view.ylim[0]);
    if (!(Number.isFinite(sx) && sx > 0 && Number.isFinite(sy) && sy > 0)) {
      fail('The plot view needs positive width/height and increasing x and y limits.');
    }
    return {
      sx,
      sy,
      X: (x) => (x - view.xlim[0]) * sx,
      Y: (y) => (y - view.ylim[0]) * sy,
      x: (X) => X / sx + view.xlim[0],
      y: (Y) => Y / sy + view.ylim[0]
    };
  }

  // Fallback text metrics in pixels; the browser passes a canvas-based measurer.
  function defaultMeasure(label, cex) {
    const px = 12 * cex;
    return { width: 0.55 * px * String(label).length, height: 0.75 * px };
  }

  // ---------- nodes ----------

  function nodeName(n) {
    return n.hasLabel ? `"${n.label}"` : `set ${n.set} node ${n.node}`;
  }

  function applyLabelWidths(nodes, p, tf, measure) {
    nodes.forEach((n) => {
      n.labelWidthPx = n.hasLabel ? measure(n.label, n.cex, n.font).width : 0;
    });
    const med = median(nodes.filter((n) => n.hasLabel && n.labelWidthPx > 0).map((n) => n.labelWidthPx));
    nodes.forEach((n) => {
      if (n.widthExplicit) return;
      n.widthPx = n.hasLabel
        ? n.labelWidthPx + 2 * p.node_label_pad * tf.sx + 2 * p.node_label_pad_frac * med
        : p.default_node_width * tf.sx;
    });
  }

  function buildNodes(loc, p, tf, measure) {
    if (!Array.isArray(loc) || !loc.length) fail('No node locations (loc) are loaded.');
    const nodes = loc.map((row, id) => {
      const n = { id, set: toNumber(row.set), node: toNumber(row.node), x: toNumber(row.x), y: toNumber(row.y) };
      if (!['set', 'node', 'x', 'y'].every((k) => Number.isFinite(n[k]))) {
        fail(`loc row ${id + 1}: set, node, x and y must be finite numbers.`);
      }
      n.key = `${n.set}:${n.node}`;
      n.label = row.label === null || row.label === undefined ? '' : String(row.label);
      n.hasLabel = n.label.trim() !== '';
      n.cex = Number.isFinite(toNumber(row.cex)) ? toNumber(row.cex) : 1;
      n.font = Number.isFinite(toNumber(row.font)) ? toNumber(row.font) : 1;
      if (!(n.cex > 0)) fail(`loc row ${id + 1}: cex must be positive.`);
      if (![1, 2, 3, 4].includes(n.font)) fail(`loc row ${id + 1}: font must be 1, 2, 3 or 4.`);
      const width = toNumber(row.width);
      if (!Number.isNaN(width) && !(width >= 0 && Number.isFinite(width))) {
        fail(`loc row ${id + 1}: width must be a non-negative number.`);
      }
      n.widthExplicit = Number.isFinite(width);
      n.widthPx = n.widthExplicit ? width * tf.sx : 0;
      const height = toNumber(row.height);
      if (!Number.isNaN(height) && !(height >= 0 && Number.isFinite(height))) {
        fail(`loc row ${id + 1}: height must be a non-negative number.`);
      }
      n.bodyHeightPx = Number.isFinite(height) && height > 0 ? height * tf.sy : null;
      n.labelBodyHeightPx = null;
      n.X = tf.X(n.x);
      n.Y = tf.Y(n.y);
      return n;
    });
    const seen = new Set();
    nodes.forEach((n) => {
      if (seen.has(n.key)) fail(`loc contains set ${n.set} node ${n.node} more than once.`);
      seen.add(n.key);
    });
    applyLabelWidths(nodes, p, tf, measure);
    return nodes;
  }

  // ---------- flows ----------

  function checkMatrix(m, label) {
    if (!Array.isArray(m) || !m.length || !Array.isArray(m[0]) || !m[0].length) fail(`${label} is empty.`);
    const cols = m[0].length;
    m.forEach((row, i) => {
      if (row.length !== cols) fail(`${label} row ${i + 1} has ${row.length} values; expected ${cols}.`);
      row.forEach((v, j) => {
        if (!(Number.isFinite(v) && v >= 0)) {
          fail(`${label} [${i + 1}, ${j + 1}] must be a finite, non-negative number.`);
        }
      });
    });
  }

  // Strand thickness is value * scale (x data units); value stays the raw quantity for display.
  function buildFlows(model, nodes, tf, scale) {
    const mats = model.matrices;
    if (!Array.isArray(mats) || !mats.length) fail('No flow matrices are loaded.');
    const flows = [];
    const push = (stage, i, j, from, to, value) => flows.push({
      key: `${stage}:${i}:${j}`, stage, i, j, from, to, value, w: value * scale * tf.sx
    });

    if (model.mode === 'graph') {
      if (mats.length !== 1) fail('Graph mode needs exactly one square adjacency matrix.');
      const m = mats[0];
      checkMatrix(m, 'The adjacency matrix');
      if (m.length !== m[0].length) fail('The graph adjacency matrix must be square.');
      if (m.length !== nodes.length) {
        fail(`The adjacency matrix is ${m.length}×${m.length}, but loc has ${nodes.length} rows; ` +
          'graph rows/columns refer to loc rows.');
      }
      m.forEach((row, i) => { if (row[i] > 0) fail('The graph adjacency matrix must have a zero diagonal.'); });
      for (let j = 0; j < m.length; j++) {
        for (let i = 0; i < m.length; i++) if (m[i][j] > 0) push(0, i, j, i, j, m[i][j]);
      }
    } else {
      mats.forEach((m, s) => checkMatrix(m, `Stage ${s + 1} matrix`));
      for (let s = 1; s < mats.length; s++) {
        if (mats[s].length !== mats[s - 1][0].length) {
          fail(`Stage ${s + 1} has ${mats[s].length} rows but stage ${s} has ${mats[s - 1][0].length} ` +
            'columns; adjacent stages must match.');
        }
      }
      const counts = [mats[0].length, ...mats.map((m) => m[0].length)];
      const byKey = new Map(nodes.map((n) => [n.key, n.id]));
      nodes.forEach((n) => {
        if (!(Number.isInteger(n.set) && n.set >= 1 && n.set <= counts.length)) {
          fail(`loc row ${n.id + 1} has set ${n.set}, but the ${mats.length} stage matrices define ` +
            `node sets 1–${counts.length}.`);
        }
        if (!(Number.isInteger(n.node) && n.node >= 1 && n.node <= counts[n.set - 1])) {
          fail(`loc row ${n.id + 1} has node ${n.node}, but set ${n.set} has ${counts[n.set - 1]} nodes.`);
        }
      });
      counts.forEach((count, si) => {
        for (let k = 1; k <= count; k++) {
          if (!byKey.has(`${si + 1}:${k}`)) fail(`loc is missing node ${k} of set ${si + 1}.`);
        }
      });
      mats.forEach((m, s) => {
        for (let j = 0; j < m[0].length; j++) {
          for (let i = 0; i < m.length; i++) {
            if (m[i][j] > 0) push(s, i, j, byKey.get(`${s + 1}:${i + 1}`), byKey.get(`${s + 2}:${j + 1}`), m[i][j]);
          }
        }
      });
    }
    if (!flows.length) fail('The flow matrices contain no positive values.');

    nodes.forEach((n) => { n.inFlows = []; n.outFlows = []; n.inValue = 0; n.outValue = 0; });
    flows.forEach((f, k) => {
      f.index = k;
      nodes[f.from].outFlows.push(k);
      nodes[f.to].inFlows.push(k);
      nodes[f.from].outValue += f.value;
      nodes[f.to].inValue += f.value;
    });
    nodes.forEach((n) => { n.inPx = n.inValue * scale * tf.sx; n.outPx = n.outValue * scale * tf.sx; });
    return flows;
  }

  function checkBalance(nodes, p, warnings) {
    const bad = nodes.filter((n) => n.inValue > 0 && n.outValue > 0 &&
      Math.abs(n.outValue - n.inValue) > p.balance_tol);
    if (!bad.length || p.balance === 'allow') return;
    const msg = `Flow-through nodes with unequal incoming and outgoing totals: ${bad.map(nodeName).join(', ')}.`;
    if (p.balance === 'error') fail(msg);
    warnings.push(msg);
  }

  // ---------- node position and height adjustment ----------

  // Port of sankey_adjust_one_node_set() with xyr = 1.
  function adjustOneSet(y, widths, gap, expand) {
    const k = y.length;
    if (k <= 1) return y.slice();
    const ord = y.map((v, i) => i).sort((a, b) => (y[a] - y[b]) || (a - b));
    const yo = ord.map((i) => y[i]);
    const wo = ord.map((i) => Math.max(0, widths[i]));
    const req = [];
    for (let i = 0; i < k - 1; i++) req.push((wo[i] + wo[i + 1]) / 2 + gap);
    if (req.every((r, i) => yo[i + 1] - yo[i] >= r - Math.sqrt(Number.EPSILON))) return y.slice();
    const cum = [0];
    req.forEach((r) => cum.push(cum[cum.length - 1] + r));
    const total = cum[k - 1];
    const lo = yo[0];
    const hi = yo[k - 1];
    let adj;
    if (!expand && total > hi - lo) {
      adj = cum.map((c) => lo + c * (hi - lo) / total);
    } else {
      let resid = isotonic(yo.map((v, i) => v - cum[i]));
      if (!expand) resid = resid.map((v) => clamp(v, lo, hi - total));
      adj = resid.map((v, i) => v + cum[i]);
    }
    const out = y.slice();
    ord.forEach((idx, i) => { out[idx] = adj[i]; });
    return out;
  }

  function adjustLocations(nodes, p, tf) {
    const sets = new Map();
    nodes.forEach((n) => {
      if (!sets.has(n.set)) sets.set(n.set, []);
      sets.get(n.set).push(n);
    });
    sets.forEach((group) => {
      const adjusted = adjustOneSet(
        group.map((n) => n.Y),
        group.map((n) => Math.max(n.inPx, n.outPx)),
        p.min_node_gap * tf.sx,
        p.adjust_loc_expand
      );
      group.forEach((n, i) => { n.Y = adjusted[i]; n.y = tf.y(n.Y); });
    });
  }

  function adjustNodeHeights(nodes, p, tf, measure) {
    if (p.adjust_node_height === 'none') return;
    const labeled = nodes.filter((n) => n.hasLabel);
    if (!labeled.length) return;
    const labelHeight = (n) => measure(n.label, n.cex, n.font).height;
    const flowHeight = (n) => Math.max(n.inPx, n.outPx);
    const maxLabel = Math.max(...labeled.map(labelHeight));
    if (!(maxLabel > 0)) return;
    const buffer = p.node_label_vpad * tf.sy + p.node_label_vpad_frac * maxLabel;
    if (p.adjust_node_height === 'label') {
      labeled.forEach((n) => {
        const h = labelHeight(n);
        const available = Math.max(flowHeight(n) - 2 * buffer, 0.05 * flowHeight(n));
        if (h > 0) n.cex *= Math.max(Math.min(1, available / h), 0.05);
      });
      applyLabelWidths(nodes, p, tf, measure);
    } else {
      labeled.forEach((n) => {
        const labelBody = labelHeight(n) + 2 * buffer;
        n.labelBodyHeightPx = labelBody;
        n.bodyHeightPx = Math.max(flowHeight(n), n.bodyHeightPx || 0, labelBody);
      });
    }
  }

  // ---------- stacking ----------

  function setOffsets(flows, order, side) {
    const field = side === 'start' ? 'startOffset' : 'endOffset';
    const total = order.reduce((s, k) => s + flows[k].w, 0);
    let cum = 0;
    order.forEach((k) => {
      cum += flows[k].w;
      flows[k][field] = cum - flows[k].w / 2 - total / 2;
    });
  }

  // side "start": the stack at a source node, ordered by each strand's target.
  function baseOrder(nodes, flows, inds, side, opt) {
    const other = (k) => nodes[side === 'start' ? flows[k].to : flows[k].from];
    const index = (k) => (side === 'start' ? flows[k].j : flows[k].i);
    if (opt.endsWith('_index')) return inds.slice().sort((a, b) => index(a) - index(b));
    const desc = opt.endsWith('_desc');
    return inds.slice().sort((a, b) => {
      const ya = other(a).Y;
      const yb = other(b).Y;
      if (ya !== yb) return desc ? yb - ya : ya - yb;
      return (other(a).X - other(b).X) || (index(a) - index(b));
    });
  }

  function stackFlows(nodes, flows, sp) {
    nodes.forEach((n) => {
      if (n.outFlows.length) {
        setOffsets(flows, baseOrder(nodes, flows, n.outFlows, 'start', sp(flows[n.outFlows[0]].stage).source_order), 'start');
      }
      if (n.inFlows.length) {
        setOffsets(flows, baseOrder(nodes, flows, n.inFlows, 'end', sp(flows[n.inFlows[0]].stage).target_order), 'end');
      }
    });
  }

  // Opposite-side face x closest to the shared node, for either flow direction.
  function nearestFace(flows, inds, side) {
    let best = inds[0];
    inds.forEach((k) => {
      if (Math.abs(flows[k].endX - flows[k].startX) < Math.abs(flows[best].endX - flows[best].startX)) best = k;
    });
    return side === 'end' ? flows[best].endX : flows[best].startX;
  }

  function linearY(f, x) {
    const y0 = f.startY + f.startOffset;
    const y1 = f.endY + f.endOffset;
    const d = f.endX - f.startX;
    const t = Math.abs(d) <= 1e-12 ? 0 : clamp((x - f.startX) / d, 0, 1);
    return y0 * (1 - t) + y1 * t;
  }

  // One repacking pass for the *_projected / *_bezier orders (sources, then targets).
  function refineOrder(nodes, flows, sp, kind, sampleY) {
    nodes.forEach((n) => {
      if (n.outFlows.length < 2) return;
      const opt = sp(flows[n.outFlows[0]].stage).source_order;
      if (!opt.startsWith(`target_${kind}`)) return;
      const ref = nearestFace(flows, n.outFlows, 'end');
      const y = new Map(n.outFlows.map((k) => [k, sampleY(k, ref)]));
      const ord = n.outFlows.slice().sort((a, b) => (y.get(a) - y.get(b)) || (flows[a].j - flows[b].j));
      if (opt.endsWith('_desc')) ord.reverse();
      setOffsets(flows, ord, 'start');
    });
    nodes.forEach((n) => {
      if (n.inFlows.length < 2) return;
      const opt = sp(flows[n.inFlows[0]].stage).target_order;
      if (!opt.startsWith(`source_${kind}`)) return;
      const ref = nearestFace(flows, n.inFlows, 'start');
      const y = new Map(n.inFlows.map((k) => [k, sampleY(k, ref)]));
      const ord = n.inFlows.slice().sort((a, b) => (y.get(a) - y.get(b)) || (flows[a].i - flows[b].i));
      if (opt.endsWith('_desc')) ord.reverse();
      setOffsets(flows, ord, 'end');
    });
  }

  // Manual orders come from dragging strands: { start|end: { nodeKey: [flowKey, ...] } },
  // listed bottom to top. Unknown or missing keys keep their computed relative order.
  function applyManualOrder(nodes, flows, manual) {
    if (!manual) return;
    nodes.forEach((n) => {
      [['start', n.outFlows, 'startOffset'], ['end', n.inFlows, 'endOffset']].forEach(([side, inds, field]) => {
        const keys = manual[side] && manual[side][n.key];
        if (!keys || inds.length < 2) return;
        const rank = new Map(keys.map((key, r) => [key, r]));
        const current = inds.slice().sort((a, b) => flows[a][field] - flows[b][field]);
        const score = (k, pos) => (rank.has(flows[k].key) ? rank.get(flows[k].key) : keys.length + pos);
        const ord = current.map((k, pos) => [k, score(k, pos)]).sort((a, b) => a[1] - b[1]).map((e) => e[0]);
        setOffsets(flows, ord, side);
      });
    });
  }

  function setFaces(nodes, flows, p, mode) {
    flows.forEach((f) => {
      const a = nodes[f.from];
      const b = nodes[f.to];
      if (mode === 'graph' && p.require_forward && !(b.X > a.X)) {
        fail(`Strand ${nodeName(a)} → ${nodeName(b)} moves right-to-left; graph flows must move ` +
          'left-to-right unless "require forward" is off.');
      }
      f.dir = Math.sign(b.X - a.X);
      f.startX = a.X + f.dir * a.widthPx / 2;
      f.endX = b.X - f.dir * b.widthPx / 2;
      if (f.dir === 0 || f.dir * (f.endX - f.startX) <= 0) {
        fail(`Node widths leave no horizontal room for the strand ${nodeName(a)} → ${nodeName(b)}.`);
      }
      f.startY = a.Y;
      f.endY = b.Y;
    });
  }

  // ---------- strand geometry ----------

  function cubicBezier(p0, p1, p2, p3, n) {
    return linspace(n).map((t) => {
      const u = 1 - t;
      return [
        u * u * u * p0[0] + 3 * u * u * t * p1[0] + 3 * u * t * t * p2[0] + t * t * t * p3[0],
        u * u * u * p0[1] + 3 * u * u * t * p1[1] + 3 * u * t * t * p2[1] + t * t * t * p3[1]
      ];
    });
  }

  // Port of centerline_edges(..., endpoint_normals = "vertical") with xyr = 1.
  function centerlineEdges(points, width, dir) {
    const n = points.length;
    const left = [];
    const right = [];
    for (let i = 0; i < n; i++) {
      let tx;
      let ty;
      if (i === 0 || i === n - 1) {
        tx = dir;
        ty = 0;
      } else {
        tx = points[i + 1][0] - points[i - 1][0];
        ty = points[i + 1][1] - points[i - 1][1];
      }
      const len = Math.hypot(tx, ty);
      if (!(len > 1e-10)) fail('A strand centerline contains repeated points; increase the node spacing.');
      const nx = -ty / len;
      const ny = tx / len;
      left.push([points[i][0] + nx * width / 2, points[i][1] + ny * width / 2]);
      right.push([points[i][0] - nx * width / 2, points[i][1] - ny * width / 2]);
    }
    return dir > 0 ? { upper: left, lower: right } : { upper: right, lower: left };
  }

  function bezierGeometry(f, n, curvature) {
    const dx = f.endX - f.startX;
    // Stack offsets are vertical (positive = up) whichever way the strand travels.
    const p0 = [f.startX, f.startY + f.startOffset];
    const p3 = [f.endX, f.endY + f.endOffset];
    const handle = curvature * dx;
    f.centerline = cubicBezier(p0, [p0[0] + handle, p0[1]], [p3[0] - handle, p3[1]], p3, n);
    const edges = centerlineEdges(f.centerline, f.w, Math.sign(dx));
    f.upper = edges.upper;
    f.lower = edges.lower;
  }

  // Port of boundary_transport_geometry() with xyr = 1.
  function boundaryGeometry(flows, inds, sp, n) {
    const t = linspace(n);
    let ease = t.map((v) => v * v * (3 - 2 * v));
    if (sp.boundary_ease !== 1) {
      const a = sp.boundary_ease;
      ease = ease.map((e) => Math.pow(e, a) / (Math.pow(e, a) + Math.pow(1 - e, a)));
      ease[0] = 0;
      ease[n - 1] = 1;
    }
    const groups = new Map();
    inds.forEach((k) => {
      const f = flows[k];
      const id = sp.boundary_group === 'target' ? f.to : sp.boundary_group === 'source' ? f.from : 'stage';
      if (!groups.has(id)) groups.set(id, []);
      groups.get(id).push(k);
    });
    const sc = (k) => flows[k].startY + flows[k].startOffset;
    const ec = (k) => flows[k].endY + flows[k].endOffset;

    groups.forEach((group) => {
      const ord = group.slice().sort(sp.boundary_group === 'source'
        ? (a, b) => (ec(a) - ec(b)) || (flows[a].j - flows[b].j) || (flows[a].i - flows[b].i)
        : (a, b) => (sc(a) - sc(b)) || (flows[a].i - flows[b].i) || (flows[a].j - flows[b].j));
      const m = ord.length;
      if (range(ord.map((k) => flows[k].startX)) > PX_TOL || range(ord.map((k) => flows[k].endX)) > PX_TOL) {
        fail('Boundary routing needs every strand in a group to share start and end x positions; ' +
          'try another boundary group or route "bezier".');
      }
      const w = ord.map((k) => flows[k].w);
      const sl = ord.map((k, i) => sc(k) - w[i] / 2);
      const su = ord.map((k, i) => sc(k) + w[i] / 2);
      const el = ord.map((k, i) => ec(k) - w[i] / 2);
      const eu = ord.map((k, i) => ec(k) + w[i] / 2);
      const startGap = [];
      const endGap = [];
      for (let i = 1; i < m; i++) {
        startGap.push(sl[i] - su[i - 1]);
        endGap.push(el[i] - eu[i - 1]);
      }
      if (startGap.some((g) => g < -PX_TOL) || endGap.some((g) => g < -PX_TOL)) {
        fail('Boundary routing needs non-overlapping endpoint stacks in each group (the strand order ' +
          'differs between the two ends); try another boundary group, ordering, or route "bezier".');
      }
      const x0 = flows[ord[0]].startX;
      const x1 = flows[ord[0]].endX;
      const x = t.map((v) => x0 * (1 - v) + x1 * v);
      let lower = ord.map((k, i) => ease.map((e) => sl[i] * (1 - e) + el[i] * e));
      let upper = ord.map((k, i) => ease.map((e) => su[i] * (1 - e) + eu[i] * e));

      if (sp.boundary_compensation !== 'none') {
        const center = lower.map((lo, i) => lo.map((v, r) => (v + upper[i][r]) / 2));
        const vwidth = center.map((c, i) => {
          const slope = [];
          for (let r = 1; r < n; r++) slope.push((c[r] - c[r - 1]) / Math.max(Math.abs(x[r] - x[r - 1]), 1e-12));
          const smooth = [slope[0]];
          for (let r = 1; r < n - 1; r++) smooth.push((slope[r - 1] + slope[r]) / 2);
          smooth.push(slope[slope.length - 1]);
          const vw = smooth.map((s) => w[i] * Math.sqrt(1 + s * s));
          vw[0] = w[i];
          vw[n - 1] = w[i];
          return vw;
        });
        if (sp.boundary_compensation === 'gap_budget') {
          const extra = vwidth.map((vw, i) => vw.map((v) => Math.max(0, v / 2 - w[i] / 2)));
          const lambda = t.map((_, r) => {
            let lam = Infinity;
            for (let i = 1; i < m; i++) {
              const needed = extra[i][r] + extra[i - 1][r];
              if (needed > 1e-12) lam = Math.min(lam, (lower[i][r] - upper[i - 1][r]) / needed);
            }
            return m > 1 ? clamp(lam, 0, 1) : 1;
          });
          lower = center.map((c, i) => c.map((v, r) => v - (w[i] / 2 + extra[i][r] * lambda[r])));
          upper = center.map((c, i) => c.map((v, r) => v + (w[i] / 2 + extra[i][r] * lambda[r])));
        } else {
          const newLower = lower.map((lo) => lo.slice());
          const newUpper = upper.map((up) => up.slice());
          for (let r = 0; r < n; r++) {
            let cursor = lower[0][r];
            const lc = [];
            const uc = [];
            for (let i = 0; i < m; i++) {
              lc.push(cursor);
              uc.push(cursor + vwidth[i][r]);
              cursor = uc[i];
              if (i < m - 1) cursor += startGap[i] * (1 - ease[r]) + endGap[i] * ease[r];
            }
            const shift = ((lower[0][r] + upper[m - 1][r]) - (lc[0] + uc[m - 1])) / 2;
            for (let i = 0; i < m; i++) {
              newLower[i][r] = lc[i] + shift;
              newUpper[i][r] = uc[i] + shift;
            }
          }
          lower = newLower;
          upper = newUpper;
        }
      }

      ord.forEach((k, i) => {
        const f = flows[k];
        f.upper = x.map((xx, r) => [xx, upper[i][r]]);
        f.lower = x.map((xx, r) => [xx, lower[i][r]]);
        f.centerline = x.map((xx, r) => [xx, (upper[i][r] + lower[i][r]) / 2]);
      });
    });
  }

  function pointAtX(points, targetX) {
    const n = points.length;
    const xs = points.map((p) => p[0]);
    const tx = clamp(targetX, Math.min(...xs), Math.max(...xs));
    if (Math.abs(tx - xs[0]) < 1e-12) return points[0].slice();
    if (Math.abs(tx - xs[n - 1]) < 1e-12) return points[n - 1].slice();
    const increasing = xs[n - 1] >= xs[0];
    let i = -1;
    for (let k = 0; k < n; k++) {
      if (xs[k] === tx) return points[k].slice();
      if (increasing ? xs[k] < tx : xs[k] > tx) i = k;
    }
    if (i < 0 || i >= n - 1) return points[clamp(i, 0, n - 1)].slice();
    const prop = (tx - xs[i]) / (xs[i + 1] - xs[i]);
    return [points[i][0] + prop * (points[i + 1][0] - points[i][0]), points[i][1] + prop * (points[i + 1][1] - points[i][1])];
  }

  // ---------- main entry point ----------

  function computeLayout(model, view, options) {
    const measure = (options && options.measure) || defaultMeasure;
    const p = Object.assign({}, DEFAULT_PARAMS, model.params);
    checkParams(p);
    const mode = model.mode === 'graph' ? 'graph' : 'layered';
    const stageCache = new Map();
    const sp = (stage) => {
      if (!stageCache.has(stage)) stageCache.set(stage, stageParams(p, model.stageParams, stage));
      return stageCache.get(stage);
    };
    const tf = makeTransform(view);
    const warnings = [];

    const scale = model.valueScale === undefined ? 1 : model.valueScale;
    if (!(Number.isFinite(scale) && scale > 0)) fail('The value scale must be a positive number.');
    const nodes = buildNodes(model.loc, p, tf, measure);
    const flows = buildFlows(Object.assign({}, model, { mode }), nodes, tf, scale);
    if (mode === 'graph') checkBalance(nodes, p, warnings);
    if (p.adjust_loc) adjustLocations(nodes, p, tf);
    adjustNodeHeights(nodes, p, tf, measure);
    setFaces(nodes, flows, p, mode);

    stackFlows(nodes, flows, sp);
    refineOrder(nodes, flows, sp, 'projected', (k, x) => linearY(flows[k], x));
    applyManualOrder(nodes, flows, model.manualOrder);

    const stages = [...new Set(flows.map((f) => f.stage))].sort((a, b) => a - b);
    const buildGeometry = () => {
      stages.forEach((s) => {
        const sps = sp(s);
        const inds = flows.filter((f) => f.stage === s).map((f) => f.index);
        if (sps.route === 'boundary') {
          if (sps.source_order.includes('bezier') || sps.target_order.includes('bezier')) {
            fail(`Bezier-sampled strand ordering requires route "bezier" (stage ${s + 1}).`);
          }
          boundaryGeometry(flows, inds, sps, p.n);
        } else {
          inds.forEach((k) => bezierGeometry(flows[k], p.n, p.curvature));
        }
      });
    };
    buildGeometry();
    const bezierOrder = stages.some((s) => sp(s).source_order.includes('bezier') || sp(s).target_order.includes('bezier'));
    if (bezierOrder) {
      refineOrder(nodes, flows, sp, 'bezier', (k, x) => pointAtX(flows[k].centerline, x)[1]);
      applyManualOrder(nodes, flows, model.manualOrder);
      buildGeometry();
    }

    flows.forEach((f) => { f.pathLength = polylineLength(f.centerline); });
    const mergeLength = p.merge_length !== null
      ? p.merge_length * tf.sx
      : p.merge_frac * Math.min(...flows.map((f) => f.pathLength));

    return { mode, nodes, flows, mergeLength, tf, view, params: p, stageParams: sp, warnings, stageCount: stages.length, valueScale: scale };
  }

  SK.layout = {
    SankeyError,
    SOURCE_ORDERS,
    TARGET_ORDERS,
    CHOICES,
    STAGE_KEYS,
    DEFAULT_PARAMS,
    computeLayout,
    makeTransform,
    adjustOneSet,
    isotonic,
    pointAtX,
    polylineLength,
    nodeName
  };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
