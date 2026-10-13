// Small cartoons (30 x 18 SVG units) that show what a setting does: one per checkbox (its effect
// when on), per select option ('key:value') and per slider end ('key:min' / 'key:max').
// Conventions: a node is a bar (a body when it holds a label), a label an ink bar, a strand a
// colored stroke; ink is for annotations. Absolute sizes are measured with a ruler, sizes relative
// to something show that reference (a meter, or ghost copies). Arrows mean motion.
(function (root) {
  const SK = root.SK || (root.SK = {});
  const INK = '#4b5563';
  const NODE = '#6b7280';
  const PALE = '#d5dbe3';
  const MID = '#aab4c3';
  const BLUE = '#4E79A7';
  const ORANGE = '#F28E2B';
  const GREEN = '#59A14F';
  const PURPLE = '#B07AA1';
  const DARK_BLUE = '#2c4a6e';
  const DARK_ORANGE = '#9a5414';

  const rect = (x, y, w, h, fill = NODE, r = 0.6, extra = '') =>
    `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${r}" fill="${fill}"${extra}/>`;
  const node = (x, y, h, fill = NODE) => rect(x, y, 2.5, h, fill);
  const strand = (d, color = BLUE, width = 3, opacity = 1) =>
    `<path d="${d}" fill="none" stroke="${color}" stroke-width="${width}" stroke-opacity="${opacity}" stroke-linejoin="round"/>`;
  const line = (d, color = INK, width = 1.2, extra = '') =>
    `<path d="${d}" fill="none" stroke="${color}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"${extra}/>`;
  const text = (x, y, s, size = 7.5, fill = INK) =>
    `<text x="${x}" y="${y}" font-size="${size}" font-family="system-ui, sans-serif" font-weight="600" text-anchor="middle" fill="${fill}">${s}</text>`;
  const haloText = (x, y, s, size, halo) =>
    text(x, y, s, size).replace('<text ', `<text stroke="${halo}" stroke-width="2.4" paint-order="stroke" `);
  const bar = (x, y, w, fill = INK, h = 2.2) => rect(x, y, w, h, fill, 1);
  const ghost = (x, y, w, h = 2.2) => rect(x, y, w, h, INK, 1, ' fill-opacity="0.3"');
  const dashed = (x, y, w, h) =>
    `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="0.6" fill="none" stroke="${INK}" stroke-width="0.9" stroke-dasharray="1.6 1.1"/>`;
  const tint = (x, y, w, h) => rect(x, y, w, h, ORANGE, 0, ' fill-opacity="0.35"');
  const pt = (x) => +x.toFixed(2);

  // Rulers measure absolute sizes: a line with ticks at its ends and every `step` units between.
  function ticks(a, b, step) {
    const at = [];
    for (let v = a; v < b - 1e-9; v += step) at.push(v);
    return at.concat(b);
  }
  const ruler = (x0, x1, y, step = 2.5) =>
    line(`M${x0} ${y}H${x1}` + ticks(x0, x1, step).map((x) => `M${pt(x)} ${y - 1}V${y + 1}`).join(''), INK, 1.2);
  const rulerV = (x, y0, y1, step = 2.5) =>
    line(`M${x} ${y0}V${y1}` + ticks(y0, y1, step).map((y) => `M${x - 1} ${pt(y)}H${x + 1}`).join(''), INK, 1.2);
  // Meters show a share of a reference: a pale track with an ink fill.
  const meter = (x0, x1, f0, f1, y) => rect(x0, y, x1 - x0, 1.6, PALE, 0.8) + rect(f0, y, f1 - f0, 1.6, INK, 0.8);
  const arrow = (d, head) => line(d + head, INK, 1.2);
  // A mouse pointer with its tip at (x, y).
  const cursor = (x, y) => `<path d="M${x} ${y}l0 8.3 2.1-1.8 1.7 3.6 1.4-.65-1.7-3.5h2.7z" fill="#fff" stroke="${INK}" stroke-width="0.8" stroke-linejoin="round"/>`;

  // Gradients: each glyph is built once, so a counter gives every gradient a page-unique id.
  let gradients = 0;
  function gradient(x0, x1, stops) {
    const id = `glyph-gradient-${++gradients}`;
    const list = stops.map((s, i) => (Array.isArray(s) ? s : [i / (stops.length - 1), s]));
    return { id, defs: `<defs><linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${x0}" x2="${x1}" y1="0" y2="0">` +
      list.map(([o, c]) => `<stop offset="${o}" stop-color="${c}"/>`).join('') + '</linearGradient></defs>' };
  }
  function band(x0, x1, y, h, stops) {
    const g = gradient(x0, x1, stops);
    return `${g.defs}<rect x="${x0}" y="${y}" width="${x1 - x0}" height="${h}" fill="url(#${g.id})"/>`;
  }
  function shaded(d, x0, x1, stops, width = 3) {
    const g = gradient(x0, x1, stops);
    return `${g.defs}<path d="${d}" fill="none" stroke="url(#${g.id})" stroke-width="${width}"/>`;
  }

  // Shared scenes.
  const crossing = (top, bottom, nodes = [NODE, NODE, NODE, NODE]) =>
    node(1.5, 2, 6, nodes[0]) + node(1.5, 10, 6, nodes[1]) + node(26, 2, 6, nodes[2]) + node(26, 10, 6, nodes[3]) +
    strand('M4 5 C15 5 15 13 26 13', top) + strand('M4 13 C15 13 15 5 26 5', bottom);
  // Two strands packing into one trunk from x = start to the node on the right.
  function merging(start) {
    const m = (2 + start) / 2;
    return node(26, 3, 12) + strand(`M2 4 C${m} 4 ${m} 7.7 ${start} 7.7 H26`, BLUE, 2.6) +
      strand(`M2 14 C${m} 14 ${m} 10.3 ${start} 10.3 H26`, ORANGE, 2.6);
  }
  // Samples of the S curve M3 14 C15 14 15 4 27 4, joined by straight segments.
  const S_CURVE = [[3, 14], [15, 14], [15, 4], [27, 4]];
  const sCurve = (t) => [0, 1].map((k) => (1 - t) ** 3 * S_CURVE[0][k] + 3 * (1 - t) ** 2 * t * S_CURVE[1][k] +
    3 * (1 - t) * t * t * S_CURVE[2][k] + t ** 3 * S_CURVE[3][k]);
  function samples(count, color, r) {
    const pts = Array.from({ length: count }, (_, i) => sCurve(i / (count - 1)));
    return line(`M${pts.map(([x, y]) => `${pt(x)} ${pt(y)}`).join(' L')}`, color, 1.2) +
      pts.map(([x, y]) => `<circle cx="${pt(x)}" cy="${pt(y)}" r="${r}" fill="${color}"/>`).join('');
  }
  // The boundary ease (layout.js boundaryGeometry): smoothstep, then sharpened (a > 1) or softened.
  const ease = (a, t) => {
    const s = t * t * (3 - 2 * t);
    return s ** a / (s ** a + (1 - s) ** a);
  };
  const easedBand = (a) => node(1.5, 6.5, 9) + node(26, 1.5, 9) + [8, 11, 14].map((y0, k) => strand(`M4 ${y0} L6 ${y0} L` +
    Array.from({ length: 19 }, (_, i) => `${7 + i} ${pt(y0 - 5 * ease(a, i / 18))}`).join(' L') + ` L26 ${y0 - 5}`,
  [BLUE, ORANGE, GREEN][k], 3)).join('');
  // Six faint pathways fanning out of one node, the one under the pointer traced when `traced`.
  const fan = (traced) => [2.5, 5.5, 8.5, 11.5, 14.5, 17].map((y) => strand(`M4 9 C15 9 15 ${y} 27 ${y}`,
    traced && y === 11.5 ? ORANGE : '#b8c0cc', traced && y === 11.5 ? 2 : 1.2)).join('') + node(1.5, 5, 8) + cursor(17, 8.4) +
    (traced ? '' : line('M15.5 16.8 24.2 7.6', INK, 1.4));
  const canvas = (x, y, w, h) => `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="0.6" fill="#fff" stroke="${INK}" stroke-width="1.1"/>` +
    rect(x + 1.2, y + h / 2 - 2, 1.4, 4, NODE, 0.3) + rect(x + w - 2.6, y + h / 2 - 2, 1.4, 4, NODE, 0.3) +
    strand(`M${x + 2.6} ${y + h / 2}H${x + w - 2.6}`, BLUE, 2);
  // Two stripes touching along vertical slices of one steep curve: they never overlap, but thin
  // where steep.
  function stripes() {
    const top = (x) => 3.6 + 6 * ease(2.2, Math.min(Math.max((x - 2) / 26, 0), 1));
    const xs = Array.from({ length: 27 }, (_, i) => 2 + i);
    const poly = (a, b, color) => `<path d="M${xs.map((x) => `${x} ${pt(top(x) + a)}`).join(' L')} L${xs.slice().reverse()
      .map((x) => `${x} ${pt(top(x) + b)}`).join(' L')}Z" fill="${color}"/>`;
    return poly(0, 2.6, BLUE) + poly(2.6, 5.2, ORANGE);
  }
  const bordered = (border) => ['M3 4.5 C15 4.5 15 8.5 27 8.5', 'M3 9 C15 9 15 13 27 13']
    .map((d, i) => strand(d, '#333', 4 + border) + strand(d, [BLUE, ORANGE][i], 4)).join('');
  const nodeBorder = (w) => strand('M2 9H11', BLUE, 4) + strand('M19 9H28', BLUE, 4) +
    `<rect x="11" y="2" width="8" height="14" rx="0.8" fill="${ORANGE}" stroke="#333" stroke-width="${w}"/>`;
  const doc = `<path d="M10 2h7l3 3v11H10z" fill="#fff" stroke="${INK}" stroke-width="1.1"/>` +
    line('M17 2v3h3M12.5 8.5h5M12.5 11.5h5', INK, 1.1);

  const GLYPHS = {
    // Layout
    'valueScale:min': node(1.5, 7.5, 3) + node(26, 7.5, 3) + strand('M4 9H26', BLUE, 1.5),
    'valueScale:max': node(1.5, 2.5, 13) + node(26, 2.5, 13) + strand('M4 9H26', BLUE, 12),
    'n:min': line('M3 14 C15 14 15 4 27 4', MID, 1.2) + samples(4, INK, 1.2),
    'n:max': line('M3 14 C15 14 15 4 27 4', MID, 1.2) + samples(13, BLUE, 0.7),
    'curvature:min': node(1.5, 11, 6) + node(26, 1, 6) + strand('M4 14 L26 4'),
    'curvature:max': node(1.5, 11, 6) + node(26, 1, 6) + strand('M4 14 C22 14 8 4 26 4'),
    // Merge fraction: the trunk as a share of the strand; merge length: the trunk measured.
    'merge_frac:min': merging(25) + meter(3, 26, 25, 26, 15.6),
    'merge_frac:max': merging(19) + meter(3, 26, 19, 26, 15.6),
    'merge_length:min': merging(23) + ruler(23, 26, 16.2, 3),
    'merge_length:max': merging(14) + ruler(14, 26, 16.2, 4),
    'default_node_width:min': strand('M2 9H14.5', BLUE, 4) + strand('M15.5 9H28', ORANGE, 4) + rect(14.5, 3, 1, 12, INK, 0),
    'default_node_width:max': rect(8, 3, 14, 11.5, NODE, 0.8) + strand('M2 8.75H8', BLUE, 4) + strand('M22 8.75H28', ORANGE, 4) + ruler(8, 22, 16.2, 7),
    // Label padding: absolute (a ruler) or relative to the label's width (ghost half-labels).
    'node_label_pad:min': rect(7.5, 4.5, 15, 8, PALE, 1) + tint(7.5, 4.5, 2.5, 8) + tint(20, 4.5, 2.5, 8) + bar(10, 7.4, 10) + ruler(20, 22.5, 15.5, 2.5),
    'node_label_pad:max': rect(2, 4.5, 26, 8, PALE, 1) + tint(2, 4.5, 8, 8) + tint(20, 4.5, 8, 8) + bar(10, 7.4, 10) + ruler(20, 28, 15.5, 4),
    'node_label_pad_frac:min': rect(7.5, 4.5, 15, 8, PALE, 1) + bar(10, 7.4, 10) + ghost(8, 7.4, 1.6) + ghost(20.4, 7.4, 1.6),
    'node_label_pad_frac:max': rect(3.5, 4.5, 23, 8, PALE, 1) + bar(10, 7.4, 10) + rect(14.8, 7.4, 0.4, 2.2, '#fff', 0) +
      ghost(4.5, 7.4, 5) + ghost(20.5, 7.4, 5),
    adjust_loc: dashed(13, 5.5, 2.5, 7) + node(13, 0.5, 5) + node(13, 12.5, 5) + strand('M15.5 3H26', BLUE, 3) + strand('M15.5 15H26', ORANGE, 3) +
      arrow('M7 8V1.8', 'M5 3.8 7 1.8 9 3.8') + arrow('M7 10V16.2', 'M5 14.2 7 16.2 9 14.2'),
    'min_node_gap:min': node(8, 2, 6.5) + node(8, 9, 6.5) + strand('M10.5 5.25H20', BLUE, 3) + strand('M10.5 12.25H20', ORANGE, 3),
    'min_node_gap:max': node(8, 1, 5) + node(8, 12, 5) + strand('M10.5 3.5H20', BLUE, 3) + strand('M10.5 14.5H20', ORANGE, 3) + rulerV(24, 6, 12, 3),
    adjust_loc_expand: line('M3 5H27M3 13H27', MID, 1.3) + node(8, 0.5, 4) + node(8, 13.5, 4) +
      arrow('M19 4.5V0.9', 'M17 2.9 19 0.9 21 2.9') + arrow('M19 13.5V17.1', 'M17 15.1 19 17.1 21 15.1'),
    // Fitting labels to node heights: what grows or shrinks, the size before it dashed.
    'adjust_node_height:none': rect(5, 7.5, 20, 3, PALE, 0.8) + text(15, 12.6, 'Ab', 9),
    'adjust_node_height:label': rect(5, 7.5, 20, 3, PALE, 0.8) + text(15, 12.6, 'Ab', 9, '#c3cad4') + text(15, 10.4, 'Ab', 3.6),
    'adjust_node_height:node': rect(5, 2.5, 20, 13, PALE, 1) + dashed(5, 7.5, 20, 3) + haloText(15, 12.6, 'Ab', 9, PALE),
    // Label vertical padding: absolute (a ruler) or relative to the label's height (ghost labels).
    'node_label_vpad:min': rect(7, 5.4, 16, 7.2, PALE, 0.8) + tint(7, 5.4, 16, 2.5) + tint(7, 10.1, 16, 2.5) + bar(10, 7.9, 10) + rulerV(25.5, 5.4, 7.9, 2.5),
    'node_label_vpad:max': rect(7, 1, 16, 16, PALE, 1) + tint(7, 1, 16, 6.9) + tint(7, 10.1, 16, 6.9) + bar(10, 7.9, 10) + rulerV(25.5, 1, 7.9, 3.45),
    'node_label_vpad_frac:min': rect(7, 5.3, 16, 7.4, PALE, 0.8) + bar(10, 7.5, 10, INK, 3) + ghost(10, 5.8, 10, 1.4) + ghost(10, 10.8, 10, 1.4),
    'node_label_vpad_frac:max': rect(7, 3.5, 16, 11, PALE, 1) + bar(10, 7.5, 10, INK, 3) + ghost(10, 4, 10, 3) + ghost(10, 11, 10, 3),
    require_forward: node(1.5, 6.5, 10) + node(26, 6.5, 10) + strand('M4 11.5H20.5', BLUE, 4) + `<path d="M20 7.3 25.4 11.5 20 15.7z" fill="${BLUE}"/>` +
      line('M22 3.2H9.5M12 1.2 9.5 3.2 12 5.2', MID, 1.3) + line('M13.5 1 18 5.4M18 1 13.5 5.4', INK, 1.3),
    // Routing & order
    'route:bezier': [8, 12, 16].map((y, i) => strand(`M4 ${y} C15 ${y} 15 ${y - 6} 26 ${y - 6}`, [BLUE, ORANGE, GREEN][i], 2.2)).join(''),
    'route:boundary': [8, 12, 16].map((y, i) => strand(`M4 ${y} C15 ${y} 15 ${y - 6} 26 ${y - 6}`, [BLUE, ORANGE, GREEN][i], 4)).join(''),
    'boundary_group:target': node(1.5, 1.5, 3) + node(1.5, 7.5, 3) + node(1.5, 13.5, 3) + node(26, 5, 8) +
      strand('M4 3 C15 3 15 7 26 7', BLUE, 1.6) + strand('M4 9H26', BLUE, 1.6) + strand('M4 15 C15 15 15 11 26 11', BLUE, 1.6),
    'boundary_group:source': node(1.5, 5, 8) + node(26, 1.5, 3) + node(26, 7.5, 3) + node(26, 13.5, 3) +
      strand('M4 7 C15 7 15 3 26 3', BLUE, 1.6) + strand('M4 9H26', BLUE, 1.6) + strand('M4 11 C15 11 15 15 26 15', BLUE, 1.6),
    'boundary_group:stage': rect(3.5, 0.5, 23, 17, '#e3e9f1', 2) + [4, 9, 14].map((y) => node(1.5, y - 1.25, 2.5) + node(26, y - 2.75, 2.5) +
      strand(`M4 ${y} C15 ${y} 15 ${y - 1.5} 26 ${y - 1.5}`, BLUE, 1.6)).join(''),
    'boundary_ease:min': easedBand(0.3),
    'boundary_ease:max': easedBand(3),
    // Colors
    'colorBy:file': crossing(GREEN, PURPLE) + doc,
    'colorBy:source': crossing(BLUE, ORANGE, [BLUE, ORANGE, NODE, NODE]),
    'colorBy:target': crossing(ORANGE, BLUE, [NODE, NODE, BLUE, ORANGE]),
    'colorBy:stage': node(1.5, 3, 12) + node(13.75, 3, 12) + node(26, 3, 12) + strand('M4 9H13.75', BLUE, 6) + strand('M16.25 9H26', ORANGE, 6),
    'colorBy:single': crossing(BLUE, BLUE),
    'opacity:min': strand('M3 4.5 C15 4.5 15 13.5 27 13.5', BLUE, 4.5, 0.25) + strand('M3 13.5 C15 13.5 15 4.5 27 4.5', ORANGE, 4.5, 0.25),
    'opacity:max': strand('M3 4.5 C15 4.5 15 13.5 27 13.5', BLUE, 4.5) + strand('M3 13.5 C15 13.5 15 4.5 27 4.5', ORANGE, 4.5),
    gradient: node(1.5, 2, 6, BLUE) + node(1.5, 10, 6, ORANGE) + node(26, 2, 6, GREEN) + node(26, 10, 6, PURPLE) +
      shaded('M4 5 C15 5 15 13 26 13', 4, 26, [BLUE, PURPLE]) + shaded('M4 13 C15 13 15 5 26 5', 4, 26, [ORANGE, GREEN]),
    'gradientMode:merge': node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [[0, BLUE], [0.22, PURPLE], [0.78, PURPLE], [1, ORANGE]]),
    'gradientMode:path': node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [BLUE, ORANGE]),
    useMid: node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [[0, BLUE], [0.35, GREEN], [0.65, GREEN], [1, ORANGE]]),
    flatten: strand('M2 3.5 C9 3.5 11 7 17 7', BLUE, 4, 0.45) + strand('M2 14.5 C9 14.5 11 11 17 11', BLUE, 4, 0.45) +
      rect(17, 5, 9, 8, '#a7bcd3', 0) + node(26, 3, 12, BLUE),
    // Borders & labels
    thinOutline: `<path d="M4 4 C15 4 15 9 26 9 V14 C15 14 15 9 4 9 Z" fill="${BLUE}" fill-opacity="0.55" stroke="#111" stroke-width="0.7"/>` +
      `<rect x="1.5" y="3" width="2.5" height="7" rx="0.6" fill="${NODE}" stroke="#111" stroke-width="0.7"/>` +
      `<rect x="26" y="8" width="2.5" height="7" rx="0.6" fill="${NODE}" stroke="#111" stroke-width="0.7"/>`,
    'borderWidth:min': bordered(0.4),
    'borderWidth:max': bordered(2.4),
    'nodeBorderWidth:min': nodeBorder(0.4),
    'nodeBorderWidth:max': nodeBorder(2),
    labels: node(9, 4, 10) + node(18.5, 4, 10) + strand('M11.5 9H18.5', BLUE, 4) + bar(1, 7.9, 6.5) + bar(22.5, 7.9, 6.5),
    showIndices: node(5.5, 9.5, 7.5) + node(22, 9.5, 7.5) + strand('M8 13.25H22', BLUE, 3) + text(6.75, 7.6, '1', 9) + text(23.25, 7.6, '2', 9),
    'fontSize:min': text(15, 12, 'A', 6),
    'fontSize:max': text(15, 16.5, 'A', 16),
    'labelPosition:auto': rect(5, 4, 20, 10, ORANGE, 1) + rect(8, 5.6, 14, 6.8, '#fff', 3.4, ' fill-opacity="0.75"') + bar(10, 7.9, 10),
    'labelPosition:above': bar(10, 2, 10) + rect(5, 6.5, 20, 9.5, ORANGE, 1),
    'labelColorBy:single': rect(2, 7, 11, 9, BLUE, 1) + rect(17, 7, 11, 9, ORANGE, 1) + bar(3.5, 2.8, 8) + bar(18.5, 2.8, 8),
    'labelColorBy:node': rect(2, 7, 11, 9, BLUE, 1) + rect(17, 7, 11, 9, ORANGE, 1) + bar(3.5, 2.8, 8, DARK_BLUE) + bar(18.5, 2.8, 8, DARK_ORANGE),
    labelCounts: bar(3, 3.4, 9) + text(20.5, 6.6, '(12)') + rect(3, 9, 24, 7.5, ORANGE, 1),
    showPoints: strand('M2 9H13', BLUE, 6) + strand('M17 9H28', ORANGE, 6) + rect(13, 3, 4, 12, NODE, 0.8) +
      `<circle cx="15" cy="9" r="2.1" fill="#fff" stroke="${INK}" stroke-width="1.1"/>`,
    nodeSums: strand('M2 9H13.5', BLUE, 12, 0.5) + strand('M16.5 9H28', ORANGE, 12, 0.5) + rect(13.5, 3, 3, 12, NODE, 0.8) +
      line('M8 3V15M6.6 3H9.4M6.6 15H9.4M22 3V15M20.6 3H23.4M20.6 15H23.4', INK, 1.4),
    // Canvas & dragging
    tracePaths: node(1.5, 2, 14) + node(13.75, 2, 14) + node(26, 2, 14) + strand('M4 5H13.75M16.25 13H26', '#b8c0cc', 3) +
      strand('M4 13 C9 13 9 9 13.75 9 M16.25 9 C21 9 21 5 26 5', ORANGE, 3) + cursor(19.5, 7),
    'maxHoverSubstrands:min': fan(false),
    'maxHoverSubstrands:max': fan(true),
    traceCounts: strand('M2 12 C10 12 10 7 18 7', ORANGE, 3) + node(18, 3, 8) + text(25.4, 9.7, '12', 7),
    tracePercents: node(1.5, 4, 10) + node(26, 4, 10) + strand('M4 9H26', ORANGE, 9) + text(15, 11.7, '41%', 7.5, '#fff'),
    gatherInNodes: rect(8, 4, 14, 13, PALE, 1) + strand('M2 7 C6 7 8 5 11 5 H19 C22 5 24 8 28 8', BLUE, 1.8) +
      strand('M2 14 C6 14 8 6.8 11 6.8 H19 C22 6.8 24 14 28 14', ORANGE, 1.8),
    tileInNodes: rect(8, 1, 14, 16, PALE, 1) + stripes(),
    substrands: `<path d="M3 4 C15 4 15 8 27 8 V15 C15 15 15 11 3 11 Z" fill="${ORANGE}" fill-opacity="0.8"/>` +
      line('M3 6.3 C15 6.3 15 10.3 27 10.3M3 8.6 C15 8.6 15 12.6 27 12.6', '#fff', 1),
    'collision:wall': rect(6, 4, 3.5, 10, NODE, 0.8) + arrow('M12 9H18', 'M15.5 6.5 18 9 15.5 11.5') + line('M21.5 2V16', INK, 2),
    'collision:push': rect(3, 4, 3.5, 10, NODE, 0.8) + arrow('M8.5 9H12', 'M10.5 7.2 12.3 9 10.5 10.8') + rect(14, 4, 3.5, 10, NODE, 0.8) +
      arrow('M19.5 9H23', 'M21.5 7.2 23.3 9 21.5 10.8') + dashed(24.5, 4, 3.5, 10),
    cropExports: `<rect x="1" y="1" width="28" height="16" rx="0.6" fill="#fff" stroke="${MID}" stroke-width="1.1"/>` +
      node(8.5, 6, 6) + node(19, 6, 6) + strand('M11 9H19', BLUE, 3) + dashed(6, 4, 18, 10),
    'canvasWidth:min': canvas(9, 3, 12, 12),
    'canvasWidth:max': canvas(1, 3, 28, 12),
    'canvasHeight:min': canvas(4, 6, 22, 6),
    'canvasHeight:max': canvas(4, 0.5, 22, 17)
  };

  // A palette: its first six colors.
  const swatches = (colors) => colors.slice(0, 6).map((c, i) => rect(1.5 + i * 4.6, 4, 4.2, 10, c, 0.8)).join('');

  function inner(name) {
    const [key, value] = name.split(':');
    if (key === 'palette' && SK.color && SK.color.PALETTES[value]) return swatches(SK.color.PALETTES[value]);
    return GLYPHS[name] || null;
  }

  const has = (name) => inner(name) !== null;

  // SVG markup for a glyph, or '' when there is none.
  function markup(name, title) {
    const body = inner(name);
    if (body === null) return '';
    return `<svg class="glyph" viewBox="0 0 30 18" aria-hidden="true">${title ? `<title>${title}</title>` : ''}${body}</svg>`;
  }

  SK.glyphs = { has, markup, names: () => Object.keys(GLYPHS) };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
