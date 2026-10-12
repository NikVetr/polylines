// Small cartoons (30 x 18 SVG units) that show what a setting does: one per checkbox (its effect
// when on), per select option, per slider end ('key:min' / 'key:max') and for some numbers.
(function (root) {
  const SK = root.SK || (root.SK = {});
  const INK = '#4b5563';
  const NODE = '#6b7280';
  const PALE = '#d5dbe3';
  const BLUE = '#4E79A7';
  const ORANGE = '#F28E2B';
  const GREEN = '#59A14F';
  const PURPLE = '#B07AA1';

  const rect = (x, y, w, h, fill = NODE, r = 0.6) => `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${r}" fill="${fill}"/>`;
  const node = (x, y, h, fill = NODE) => rect(x, y, 2.5, h, fill);
  const strand = (d, color = BLUE, width = 3, opacity = 0.9) =>
    `<path d="${d}" fill="none" stroke="${color}" stroke-width="${width}" stroke-opacity="${opacity}"/>`;
  const line = (d, color = INK, width = 1.2, extra = '') =>
    `<path d="${d}" fill="none" stroke="${color}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"${extra}/>`;
  const text = (x, y, s, size = 6.5, fill = INK) =>
    `<text x="${x}" y="${y}" font-size="${size}" font-family="system-ui, sans-serif" font-weight="600" text-anchor="middle" fill="${fill}">${s}</text>`;
  const bar = (x, y, w, fill = INK, h = 2.2) => rect(x, y, w, h, fill, 1);

  // A straight band shading through the stops. Each glyph is built once, so a counter gives its
  // gradient a page-unique id.
  let gradients = 0;
  function band(x0, x1, y, h, stops) {
    const id = `glyph-gradient-${++gradients}`;
    const offsets = stops.map((c, i) => `<stop offset="${(i / (stops.length - 1)).toFixed(3)}" stop-color="${c}"/>`).join('');
    return `<defs><linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${x0}" x2="${x1}" y1="0" y2="0">${offsets}</linearGradient></defs>` +
      `<rect x="${x0}" y="${y}" width="${x1 - x0}" height="${h}" fill="url(#${id})"/>`;
  }

  // Two strands between two pairs of nodes, crossing; colors per strand and node.
  const crossing = (top, bottom, nodes = [NODE, NODE, NODE, NODE]) =>
    node(1.5, 2, 6, nodes[0]) + node(1.5, 10, 6, nodes[1]) + node(26, 2, 6, nodes[2]) + node(26, 10, 6, nodes[3]) +
    strand('M4 5 C15 5 15 13 26 13', top) + strand('M4 13 C15 13 15 5 26 5', bottom);
  const nodeBody = '<rect x="8" y="2" width="14" height="14" rx="1" fill="' + PALE + '"/>';
  const bracketH = (x0, x1, y) => line(`M${x0} ${y - 1}V${y}H${x1}V${y - 1}`, INK, 0.9);

  const GLYPHS = {
    // Layout
    valueScale: strand('M3 9H21', ORANGE, 6) + line('M26 5V13M24.5 6.5 26 5 27.5 6.5M24.5 11.5 26 13 27.5 11.5', INK, 1),
    n: line('M3 14 8 13.2 12 10.6 15 9 18 7.4 22 4.8 27 4', BLUE, 1.2) +
      [[3, 14], [8, 13.2], [12, 10.6], [15, 9], [18, 7.4], [22, 4.8], [27, 4]].map(([x, y]) => `<circle cx="${x}" cy="${y}" r="1.1" fill="${INK}"/>`).join(''),
    'curvature:min': node(1.5, 11, 6) + node(26, 1, 6) + strand('M4 14 L26 4'),
    'curvature:max': node(1.5, 11, 6) + node(26, 1, 6) + strand('M4 14 C22 14 8 4 26 4'),
    merge_frac: rect(25, 3, 3, 12) + strand('M2 5 C10 5 14 7 20 7 H25', BLUE, 2.6) + strand('M2 13 C10 13 14 11 20 11 H25', ORANGE, 2.6) + bracketH(20, 25, 2.5),
    default_node_width: rect(9, 5, 12, 8, NODE, 1) + bracketH(9, 21, 16),
    node_label_pad: rect(4, 5, 22, 8, PALE, 1) + bar(9, 7.9, 12) + line('M4.5 15H9M21 15H25.5', INK, 0.9) + line('M9 14V16M21 14V16', INK, 0.9),
    adjust_loc: rect(12, 1, 6, 6.5, NODE, 1) + rect(12, 10.5, 6, 6.5, NODE, 1) + line('M22 7.5V2.5M20.5 4 22 2.5 23.5 4M22 10.5V15.5M20.5 14 22 15.5 23.5 14', INK, 1),
    min_node_gap: rect(8, 1, 9, 5.5, NODE, 1) + rect(8, 11.5, 9, 5.5, NODE, 1) + line('M21.5 6.5V11.5M19.5 6.5H23.5M19.5 11.5H23.5', INK, 1),
    adjust_loc_expand: line('M5 5H25M5 13H25', PALE, 1.2, ' stroke-dasharray="2 1.6"') + line('M15 1.5V16.5M13 3.5 15 1.5 17 3.5M13 14.5 15 16.5 17 14.5', INK, 1.1),
    'adjust_node_height:none': rect(5, 7, 20, 4, PALE, 1) + rect(9, 5.5, 12, 7, INK, 1),
    'adjust_node_height:label': rect(5, 7, 20, 4, PALE, 1) + bar(11, 7.9, 8),
    'adjust_node_height:node': rect(5, 3.5, 20, 11, PALE, 1) + rect(9, 5.5, 12, 7, INK, 1),
    node_label_vpad: rect(6, 3, 18, 12, PALE, 1) + bar(10, 7.9, 10) + line('M26 3.5V7.5M26 10.5V14.5', INK, 0.9) + line('M25 3.5H27M25 14.5H27', INK, 0.9),
    require_forward: node(1.5, 4, 10) + node(26, 4, 10) + line('M7 9H23M19.5 5.5 23 9 19.5 12.5', INK, 1.3),
    // Routing & order
    'route:bezier': [8, 12, 16].map((y, i) => strand(`M4 ${y} C15 ${y} 15 ${y - 6} 26 ${y - 6}`, [BLUE, ORANGE, GREEN][i], 2.2)).join(''),
    'route:boundary': [8, 12, 16].map((y, i) => strand(`M4 ${y} C15 ${y} 15 ${y - 6} 26 ${y - 6}`, [BLUE, ORANGE, GREEN][i], 4, 1)).join(''),
    'boundary_group:target': node(26, 5, 8) + line('M3 3 C15 3 15 9 26 9M3 9H26M3 15 C15 15 15 9 26 9', INK, 1.3),
    'boundary_group:source': node(1.5, 5, 8) + line('M4 9 C15 9 15 3 27 3M4 9H27M4 9 C15 9 15 15 27 15', INK, 1.3),
    'boundary_group:stage': line('M3 3 C15 3 15 6 27 6M3 7.5 C15 7.5 15 10.5 27 10.5M3 12 C15 12 15 15 27 15', INK, 1.3),
    'boundary_ease:min': line('M3 15 C3 7 27 11 27 3', INK, 1.4),
    'boundary_ease:max': line('M3 15 C19 15 11 3 27 3', INK, 1.4),
    // Colors
    'colorBy:file': crossing(GREEN, PURPLE),
    'colorBy:source': crossing(BLUE, ORANGE, [BLUE, ORANGE, NODE, NODE]),
    'colorBy:target': crossing(ORANGE, BLUE, [NODE, NODE, BLUE, ORANGE]),
    'colorBy:stage': node(1.5, 3, 12) + node(13.75, 3, 12) + node(26, 3, 12) + strand('M4 9H13.75', BLUE, 6) + strand('M16.25 9H26', ORANGE, 6),
    'colorBy:single': crossing('#6b8fb3', '#6b8fb3'),
    'opacity:min': strand('M3 4.5 C15 4.5 15 13.5 27 13.5', BLUE, 4.5, 0.25) + strand('M3 13.5 C15 13.5 15 4.5 27 4.5', ORANGE, 4.5, 0.25),
    'opacity:max': strand('M3 4.5 C15 4.5 15 13.5 27 13.5', BLUE, 4.5, 1) + strand('M3 13.5 C15 13.5 15 4.5 27 4.5', ORANGE, 4.5, 1),
    gradient: node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [BLUE, ORANGE]),
    'gradientMode:merge': node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [BLUE, BLUE, BLUE, BLUE, ORANGE]),
    'gradientMode:path': node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [BLUE, ORANGE]),
    useMid: node(1.5, 4, 10, BLUE) + node(26, 4, 10, ORANGE) + band(4, 26, 6, 6, [BLUE, GREEN, ORANGE]),
    flatten: strand('M2 4 C10 4 12 7 18 7', BLUE, 4, 0.5) + strand('M2 14 C10 14 12 11 18 11', BLUE, 4, 0.5) + rect(18, 5, 10, 8, '#a7bcd3', 0),
    // Borders & labels
    thinOutline: `<path d="M4 4 C15 4 15 9 26 9 V14 C15 14 15 9 4 9 Z" fill="${BLUE}" fill-opacity="0.55" stroke="#111" stroke-width="0.7"/>`,
    labels: bar(10, 2.5, 10) + rect(6, 7, 18, 9, ORANGE, 1),
    showIndices: node(5.5, 7, 9) + node(22, 7, 9) + text(6.75, 5.5, '1') + text(23.25, 5.5, '2'),
    fontSize: text(9, 13, 'A', 7) + text(20, 14, 'A', 11),
    'labelPosition:auto': rect(5, 4, 20, 10, '#f6c99f', 1) + bar(10, 7.9, 10),
    'labelPosition:above': bar(10, 2, 10) + rect(5, 6.5, 20, 9.5, ORANGE, 1),
    'labelColorBy:single': bar(10, 2, 10, '#374151') + rect(5, 6.5, 20, 9.5, ORANGE, 1),
    'labelColorBy:node': bar(10, 2, 10, '#9a5414') + rect(5, 6.5, 20, 9.5, ORANGE, 1),
    labelCounts: text(15, 5.6, 'A (12)', 6) + rect(5, 8, 20, 8, ORANGE, 1),
    showPoints: rect(11, 3, 8, 12, PALE, 1) + `<circle cx="15" cy="9" r="1.7" fill="${INK}"/>`,
    nodeSums: rect(13, 3, 4, 12, NODE, 1) + line('M9 3V15M21 5V13', INK, 1.3),
    // Canvas & dragging
    tracePaths: node(1.5, 2, 14) + node(13.75, 2, 14) + node(26, 2, 14) +
      strand('M4 5H13.75M16.25 13H26', PALE, 3, 1) + strand('M4 13 C9 13 9 9 13.75 9 M16.25 9 C21 9 21 5 26 5', ORANGE, 3, 1),
    traceCounts: strand('M2 12 C10 12 10 7 18 7', ORANGE, 3, 1) + node(18, 3, 8) + text(25.5, 9.5, '12', 6),
    tracePercents: strand('M2 9H28', ORANGE, 8, 1) + text(15, 11.3, '41%', 6, '#fff'),
    gatherInNodes: nodeBody + strand('M2 5 C6 5 8 3.4 11 3.4 H19 C22 3.4 24 6 28 6', BLUE, 1.8, 1) +
      strand('M2 12 C6 12 8 5.3 11 5.3 H19 C22 5.3 24 13 28 13', ORANGE, 1.8, 1),
    tileInNodes: nodeBody + strand('M2 4 C15 4 15 9.5 28 9.5', BLUE, 2.6, 1) + strand('M2 7 C15 7 15 12.5 28 12.5', ORANGE, 2.6, 1),
    substrands: `<path d="M3 4 C15 4 15 8 27 8 V15 C15 15 15 11 3 11 Z" fill="${ORANGE}" fill-opacity="0.75"/>` +
      line('M3 6.3 C15 6.3 15 10.3 27 10.3M3 8.6 C15 8.6 15 12.6 27 12.6', '#fff', 0.7),
    'collision:wall': rect(6, 4, 3.5, 10, NODE, 0.8) + line('M12 9H18M15.5 6.5 18 9 15.5 11.5', INK, 1.2) + line('M21.5 2V16', INK, 2),
    'collision:push': rect(4, 4, 3.5, 10, NODE, 0.8) + line('M10 9H14M12 7 14 9 12 11', INK, 1.1) + rect(16, 4, 3.5, 10, NODE, 0.8) +
      line('M22 6h4M22 9h5M22 12h4', INK, 0.9)
  };
  GLYPHS.merge_length = GLYPHS.merge_frac;
  GLYPHS.node_label_pad_frac = GLYPHS.node_label_pad;
  GLYPHS.node_label_vpad_frac = GLYPHS.node_label_vpad;

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
