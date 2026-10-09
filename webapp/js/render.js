/* SVG rendering of a computed layout: strands, merge trunks, node bodies, borders and labels.
 * Plot pixels (y up) map to screen pixels through `frame`.
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const C = SK.color;
  const S = SK.shapes;

  const FONT_FAMILY = 'Helvetica, Arial, sans-serif';
  const THIN_LINE_WIDTH = 0.6;
  const fmt = (v) => (Math.round(v * 100) / 100).toString();
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

  function fontFor(node, style) {
    const weight = node.font === 2 || node.font === 4 ? 'bold' : 'normal';
    const slant = node.font === 3 || node.font === 4 ? 'italic' : 'normal';
    return { size: style.fontSize * node.cex, weight, slant };
  }

  // ---------- colors ----------

  // Resolve every strand/node color. Precedence for strands: per-strand override, color file
  // (when colorBy = "file"), then the palette scheme. Node colors default to the value-weighted
  // average of attached strand colors, as in R; merge (trunk) colors default to node colors.
  function resolveColors(layout, data, style) {
    const palette = C.PALETTES[style.palette];
    if (!palette) throw new SK.layout.SankeyError(`Unknown palette "${style.palette}".`);
    const scheme = (index) => C.withAlpha(C.parse(palette[index % palette.length]), style.opacity);
    // Palette slot of a node: its position, unless pinned (merging nodes pins every node's slot so
    // the survivors keep their colors).
    const slots = data.nodeSlots || {};
    const slot = (id) => (slots[layout.nodes[id].key] !== undefined ? slots[layout.nodes[id].key] : id);
    const single = C.withAlpha(C.parse(style.singleColor), style.opacity);
    const overrides = data.flowOverrides || {};
    const midOverrides = data.midOverrides || {};

    const flow = layout.flows.map((f) => {
      if (overrides[f.key]) return C.parse(overrides[f.key]);
      if (style.colorBy === 'file') {
        const mat = data.flowMatrices && data.flowMatrices[f.stage];
        const value = mat && mat[f.i] && mat[f.i][f.j];
        if (!value) {
          throw new SK.layout.SankeyError(`The strand color file has no color for stage ${f.stage + 1} ` +
            `[${f.i + 1}, ${f.j + 1}], which carries a positive flow.`);
        }
        return C.parse(value);
      }
      if (style.colorBy === 'source') return scheme(slot(f.from));
      if (style.colorBy === 'target') return scheme(slot(f.to));
      if (style.colorBy === 'stage') return scheme(f.stage);
      return single;
    });

    // provided = false: no mid colors given, the strand color is the gradient midpoint (R thread).
    const mid = layout.flows.map((f) => {
      if (Object.prototype.hasOwnProperty.call(midOverrides, f.key)) {
        return { provided: true, color: C.parse(midOverrides[f.key]) };
      }
      if (data.midMatrices && style.useMid) {
        const mat = data.midMatrices[f.stage];
        return { provided: true, color: C.parse(mat && mat[f.i] ? mat[f.i][f.j] : null) };
      }
      return { provided: false, color: null };
    });

    const node = layout.nodes.map((n) => {
      const set = (data.nodeColors || {})[n.key] || (data.locColors || {})[n.key];
      if (set) return C.parse(set);
      const inds = n.inFlows.concat(n.outFlows);
      if (!inds.length) return C.parse('#999999');
      return C.weighted(inds.map((k) => flow[k]), inds.map((k) => layout.flows[k].value));
    });
    const merge = layout.nodes.map((n, id) => {
      const set = (data.mergeColors || {})[n.key] || (data.locMergeColors || {})[n.key];
      return set ? C.parse(set) : node[id];
    });
    const bgParsed = C.parse(style.bg);
    const flatBg = bgParsed && bgParsed.a > 0 ? C.opaque(bgParsed) : C.parse('#FFFFFF');
    return { flow, mid, node, merge, bg: bgParsed, flatBg };
  }

  // ---------- labels ----------

  // Labels inside a node fade its body to a transparent centre; labels drawn above leave it solid.
  const labelFades = (n, style) => n.hasLabel && !n.labelOutside && style.labelPosition !== 'above';

  // Where a node's label goes (plot pixels) and its text anchor. With labelPosition "above", above
  // every node; otherwise centred in a node body, left of sources, right of sinks and above
  // flow-through nodes.
  function labelPlacement(n, style, textSize) {
    const above = { X: n.X, Y: n.Y + Math.max(n.inPx, n.outPx, n.bodyHeightPx || 0) / 2 + 0.8 * textSize, anchor: 'middle' };
    if (style.labelPosition === 'above') return above;
    if (n.hasLabel && n.widthPx > 0 && !n.labelOutside) return { X: n.X, Y: n.Y, anchor: 'middle' };
    const role = S.nodeRole(n);
    if (role === 'source') return { X: n.X - 7, Y: n.Y, anchor: 'end' };
    if (role === 'sink') return { X: n.X + 7, Y: n.Y, anchor: 'start' };
    return above;
  }

  // One label color, or a darker shade of each node's color.
  function labelColor(n, colors, style) {
    if (style.labelColorBy === 'node') return C.mix(C.opaque(colors.node[n.id]), { r: 0, g: 0, b: 0, a: 1 }, 0.45);
    return C.parse(style.labelColor);
  }

  // ---------- svg helpers ----------

  function makeFrame(layout, frame) {
    const h = layout.view.height;
    return {
      x: (X) => frame.margin.left + X,
      y: (Y) => frame.margin.top + h - Y
    };
  }

  function pathD(points, map, close) {
    if (!points.length) return '';
    let d = `M${fmt(map.x(points[0][0]))} ${fmt(map.y(points[0][1]))}`;
    for (let i = 1; i < points.length; i++) d += `L${fmt(map.x(points[i][0]))} ${fmt(map.y(points[i][1]))}`;
    return close ? `${d}Z` : d;
  }

  function stopTags(stops) {
    return stops.map(([offset, c]) => {
      const p = C.paint(c);
      return `<stop offset="${fmt(Math.min(Math.max(offset, 0), 1) * 100)}%" stop-color="${p.color}" stop-opacity="${p.opacity}"/>`;
    }).join('');
  }

  function linearGradient(id, x1, x2, stops) {
    return `<linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${fmt(x1)}" y1="0" x2="${fmt(x2)}" y2="0">` +
      `${stopTags(stops)}</linearGradient>`;
  }

  function fillAttrs(c) {
    const p = C.paint(c);
    return `fill="${p.color}" fill-opacity="${p.opacity}"`;
  }

  // ---------- strand color stops ----------

  // Port of flow_gradient_color(). In "merge" mode the endpoint transitions are placed just past
  // the merge cut so they continue seamlessly from the trunk fill.
  function strandStops(layout, f, colors, style) {
    const start = colors.node[f.from];
    const end = colors.node[f.to];
    const mid = colors.mid[f.index];
    const thread = mid.provided ? mid.color : colors.flow[f.index];
    // Stops run between the band edges (one merge length in from each node face), so a strand
    // reaches its node color exactly where the opaque merge band starts, with no color step.
    const span = Math.abs(f.endX - f.startX);
    const m = Math.min(Math.max(layout.mergeLength, 0) / span, 0.25);
    if (!thread) return [[m, start], [1 - m, end]];
    if (style.gradientMode === 'path') return [[m, start], [0.5, thread], [1 - m, end]];
    if (!(m > 0)) return [[0, thread], [1, thread]];
    return [[m, start], [2 * m, thread], [1 - 2 * m, thread], [1 - m, end]];
  }

  // A node's fill colors at absolute x across its bands, faces and body (unflattened), or null.
  // Interior nodes are drawn from these; endpoint trunks and bodies below use the same colors.
  function nodeStops(layout, n, colors, style) {
    const node = colors.node[n.id];
    const merge = colors.merge[n.id];
    const clear = C.withAlpha(merge, 0);
    if (S.nodeRole(n) === 'interior') {
      const shape = S.interiorTrunk(layout, n);
      if (!shape) return null;
      const inX = shape.inFaceX <= shape.outFaceX ? shape.x0 : shape.x1;
      const outX = inX === shape.x0 ? shape.x1 : shape.x0;
      // The fade to a transparent centre is a label body, so it needs a node with width.
      const labelBody = labelFades(n, style) && n.widthPx > 0 && shape.x1 - shape.x0 > 1e-9;
      if (!style.gradient) return labelBody ? [[inX, merge], [n.X, clear], [outX, merge]] : [[inX, merge], [outX, merge]];
      // Node color at the band edges (where strands arrive), trunk color across the node faces.
      return [[inX, node], [shape.inFaceX, merge]].concat(labelBody ? [[n.X, clear]] : [], [[shape.outFaceX, merge], [outX, node]]);
    }
    const side = n.outFlows.length ? 'start' : 'end';
    const b = S.bundle(layout, n, side);
    if (!b) return null;
    const inward = b.faceX + (side === 'start' ? b.dir : -b.dir) * Math.max(layout.mergeLength, 0);
    const body = S.endpointBody(layout, n, side, 0);
    const stops = [[inward, style.gradient ? node : merge], [b.faceX, merge]];
    if (body) stops.push(labelFades(n, style) ? [n.X, clear] : [body.outerX, merge]);
    return stops;
  }

  // Colors at absolute x along a pathway through consecutive strands: each node's own colors and
  // each strand's colors between its band edges, so a trace looks like what it passes through.
  function traceStops(layout, colors, style, flows) {
    const stops = [];
    const addNode = (id) => stops.push(...(nodeStops(layout, layout.nodes[id], colors, style) || []));
    addNode(flows[0].from);
    flows.forEach((f) => {
      const m = Math.min(Math.max(layout.mergeLength, 0) / Math.abs(f.endX - f.startX), 0.25);
      const own = style.gradient ? strandStops(layout, f, colors, style) : [[m, colors.flow[f.index]], [1 - m, colors.flow[f.index]]];
      own.forEach(([t, c]) => stops.push([f.startX + (f.endX - f.startX) * t, c]));
      addNode(f.to);
    });
    return stops;
  }

  // ---------- main render ----------

  // extras.substrandLines: per flow, polylines between substrands (paths.boundaryLines).
  function render(layout, colors, style, frame, extras) {
    const map = makeFrame(layout, frame);
    // Dimmed nodes blend towards the background but stay opaque, so strands running under a node
    // and the overlap of its band and body do not show through as a differently colored neck.
    const bgFade = (c) => (0.65 * c / 255).toFixed(4);
    const defs = [`<filter id="dim-node" color-interpolation-filters="sRGB"><feColorMatrix type="matrix" values="` +
      `0.35 0 0 0 ${bgFade(colors.flatBg.r)} 0 0.35 0 0 ${bgFade(colors.flatBg.g)} 0 0 0.35 0 ${bgFade(colors.flatBg.b)} 0 0 0 1 0"/></filter>`];
    const ribbons = [];
    const trunks = [];
    const sums = [];
    const hits = [];
    const dots = [];
    const labels = [];
    const flat = (c) => (style.flatten ? C.flatten(c, colors.flatBg) : c);
    const borderOn = style.border && style.border !== 'none';
    const borderPaint = borderOn ? C.paint(C.parse(style.border)) : null;
    const strokeAttrs = (outside = false) => borderOn
      ? `fill="none" stroke="${borderPaint.color}" stroke-opacity="${borderPaint.opacity}" stroke-width="${style.borderWidth * (outside ? 2 : 1)}" stroke-linejoin="round"`
      : '';
    // Consistent winding makes overlapping band subpaths a union, rather than holes.
    const compoundD = polygons => polygons.map(polygon => {
      const area = polygon.reduce((sum, p, i) => {
        const q = polygon[(i + 1) % polygon.length];
        return sum + p[0] * q[1] - q[0] * p[1];
      }, 0);
      return pathD(area < 0 ? polygon.slice().reverse() : polygon, map, true);
    }).join('');
    const edgeMask = (n, polygons) => {
      const id = `node-edge-mask${n.id}`;
      const points = polygons.flat().map(([x, y]) => [map.x(x), map.y(y)]);
      const pad = Math.max(THIN_LINE_WIDTH, style.borderWidth) + 1;
      const x = Math.min(...points.map(p => p[0])) - pad, y = Math.min(...points.map(p => p[1])) - pad;
      const width = Math.max(...points.map(p => p[0])) - x + pad, height = Math.max(...points.map(p => p[1])) - y + pad;
      // Bound each mask to its node, avoiding a canvas-sized bitmap for every outline.
      defs.push(`<mask id="${id}" maskUnits="userSpaceOnUse" x="${fmt(x)}" y="${fmt(y)}" width="${fmt(width)}" height="${fmt(height)}">` +
        `<rect width="${frame.width}" height="${frame.height}" fill="white"/><path d="${compoundD(polygons)}" fill="black"/></mask>`);
      return `mask="url(#${id})"`;
    };
    const outline = (edges, layer, mask = '') => {
      if (!style.thinOutline) return;
      // The union mask hides the inward half, keeping the visible outline 0.6 px wide.
      const width = mask ? 2 * THIN_LINE_WIDTH : THIN_LINE_WIDTH;
      layer.push(`<g class="layer-outline"><path class="plot-outline" d="${edges.map((edge) => pathD(edge, map, false)).join('')}" ` +
        `fill="none" stroke="#000000" stroke-width="${width}" stroke-linejoin="round" ${mask}/></g>`);
    };

    // Strands. With flattened (opaque) trunks the full ribbon sits underneath; otherwise ribbons
    // are cut at the merge zone so translucent trunks do not double up.
    layout.flows.forEach((f) => {
      const cuts = S.strandCuts(layout, f);
      const polygon = S.ribbonPolygon(f, style.flatten ? null : cuts);
      let fill;
      if (style.gradient) {
        const id = `gs${f.index}`;
        defs.push(linearGradient(id, map.x(f.startX), map.x(f.endX), strandStops(layout, f, colors, style)));
        fill = `fill="url(#${id})"`;
      } else {
        fill = fillAttrs(colors.flow[f.index]);
      }
      ribbons.push(`<path class="ribbon" data-flow="${f.index}" d="${pathD(polygon, map, true)}" ${fill}/>`);
      // Paint edges with their fill so later strands and nodes cover both consistently.
      const edges = S.edgeSegments(f, style.flatten ? null : cuts);
      outline(edges, ribbons);
      const hitLine = cuts ? S.sliceByX(f.centerline, cuts[0], cuts[1]) : f.centerline;
      hits.push(`<path class="hit" data-flow="${f.index}" d="${pathD(hitLine, map, false)}" ` +
        `stroke-width="${fmt(Math.max(f.w, 7))}"/>`);
      if (borderOn) {
        edges.forEach((edge) => {
          ribbons.push(`<path class="strand-border" data-flow="${f.index}" d="${pathD(edge, map, false)}" ${strokeAttrs()}/>`);
        });
      }
      const polylines = extras && extras.substrandLines && extras.substrandLines[f.index];
      if (polylines && polylines.length) {
        const linePaint = C.paint(colors.flatBg);
        ribbons.push(`<g class="layer-substrands"><path class="substrand-lines" data-flow="${f.index}" d="${polylines.map((line) => pathD(line, map, false)).join('')}" ` +
          `fill="none" stroke="${linePaint.color}" stroke-opacity="0.6" stroke-width="${THIN_LINE_WIDTH}"/></g>`);
      }
    });

    const nodeFill = (n, id, polygons, x0, x1, stops) => {
      const gid = `gn${n.id}_${id}`;
      defs.push(linearGradient(gid, map.x(x0), map.x(x1), stops.map(([t, c]) => [t, flat(c)])));
      trunks.push(`<path class="trunk" data-node="${n.id}" d="${compoundD(polygons)}" fill="url(#${gid})"/>`);
    };
    const solidNode = (n, polygons, c) => {
      trunks.push(`<path class="trunk" data-node="${n.id}" d="${compoundD(polygons)}" ${fillAttrs(flat(c))}/>`);
    };
    // Node borders sit just inside the node: inward = +1 for bottom edges, -1 for top edges.
    const fadeStroke = (n, id, edge, c, alpha, inward) => {
      if (edge.length < 2) return;
      const line = edge.map(([x, y]) => [x, y + inward * style.nodeBorderWidth / 2]);
      const x0 = line[0][0];
      const x1 = line[line.length - 1][0];
      const gid = `gb${n.id}_${id}`;
      const stops = Array.from({ length: 13 }, (_, i) => {
        const t = i / 12;
        return [t, C.withAlpha(c, c.a * alpha(t))];
      });
      defs.push(linearGradient(gid, map.x(x0), map.x(Math.abs(x1 - x0) < 1e-9 ? x0 + 1 : x1), stops));
      trunks.push(`<path class="node-border" data-node="${n.id}" d="${pathD(line, map, false)}" fill="none" ` +
        `stroke="url(#${gid})" stroke-width="${style.nodeBorderWidth}" stroke-linecap="butt"/>`);
    };
    const nodeBorderColor = (n) => {
      if (style.nodeBorder === 'none') return null;
      // Auto borders outline label bodies, which only fade (and need outlining) with labels inside.
      if (style.nodeBorder === 'auto') return labelFades(n, style) ? C.opaque(colors.node[n.id]) : null;
      return C.parse(style.nodeBorder);
    };
    const sine = (t) => Math.sin(Math.PI * t);

    layout.nodes.forEach((n) => {
      const role = S.nodeRole(n);
      const nodeColor = colors.node[n.id];
      const mergeColor = colors.merge[n.id];
      if (role === 'interior') {
        const shape = S.interiorTrunk(layout, n);
        if (!shape) return;
        const span = shape.x1 - shape.x0;
        const labelBody = labelFades(n, style) && n.widthPx > 0 && span > 1e-9;
        if ((style.gradient || labelBody) && span > 1e-9) {
          const stops = nodeStops(layout, n, colors, style).map(([x, c]) => [(x - shape.x0) / span, c]);
          nodeFill(n, 'i', shape.polygons, shape.x0, shape.x1, stops.sort((a, b) => a[0] - b[0]));
        } else {
          solidNode(n, shape.polygons, mergeColor);
        }
        const mask = (style.thinOutline || borderOn) ? edgeMask(n, shape.polygons) : '';
        outline(shape.borderEdges, trunks, mask);
        if (borderOn) {
          shape.borderEdges.forEach((line) => {
            trunks.push(`<path class="node-outline" data-node="${n.id}" d="${pathD(line, map, false)}" ${strokeAttrs(true)} ${mask}/>`);
          });
        }
        const bc = nodeBorderColor(n);
        if (bc && n.widthPx > 0) {
          const span = shape.lowerBridge.length > 1
            ? Math.abs(shape.lowerBridge[shape.lowerBridge.length - 1][0] - shape.lowerBridge[0][0]) : 0;
          const center = span > 0 ? Math.abs(n.X - shape.lowerBridge[0][0]) / span : 0.5;
          const alpha = borderOn ? (t) => Math.min(1, Math.abs(t - center) / Math.max(center, 1 - center, 1e-8)) : sine;
          fadeStroke(n, 'l', shape.lowerBridge, bc, alpha, 1);
          fadeStroke(n, 'u', shape.upperBridge, bc, alpha, -1);
        }
        return;
      }
      if (role === 'isolated') return;
      const side = role === 'source' ? 'start' : 'end';
      const trunk = S.trunkPolygon(layout, n, side);
      if (trunk) {
        const inward = trunk.faceX + (side === 'start' ? trunk.dir : -trunk.dir) * layout.mergeLength;
        if (style.gradient) {
          nodeFill(n, 't', trunk.polygons, trunk.faceX, inward, [[0, mergeColor], [1, nodeColor]]);
        } else {
          solidNode(n, trunk.polygons, mergeColor);
        }
      }
      // Opaque trunks can hide a small body overlap; translucent ones would show it as a darker line.
      const body = S.endpointBody(layout, n, side, style.flatten && trunk ? Math.min(1.5, layout.mergeLength) : 0);
      const edges = trunk ? trunk.edges.slice() : [];
      if (body) {
        edges.push(body.lowerBorder, body.upperBorder,
          [body.lowerBorder[body.lowerBorder.length - 1], body.upperBorder[body.upperBorder.length - 1]]);
      } else {
        const b = S.bundle(layout, n, side);
        if (b) edges.push([[b.faceX, b.lowerY], [b.faceX, b.upperY]]);
      }
      if (body) {
        const tc = Math.abs(n.X - body.faceX) / Math.abs(body.outerX - body.faceX);
        // The trunk meets the body at the face in the trunk (merge) color, so the body starts there too.
        if (labelFades(n, style)) {
          nodeFill(n, 'b', [body.polygon], body.faceX, body.outerX,
            [[0, mergeColor], [tc, C.withAlpha(mergeColor, 0)], [1, C.withAlpha(mergeColor, 0)]]);
        } else {
          solidNode(n, [body.polygon], mergeColor);
        }
        const bc = nodeBorderColor(n);
        if (bc) {
          const alpha = borderOn ? (t) => 1 - t : sine;
          fadeStroke(n, 'l', body.lowerBorder, bc, alpha, 1);
          fadeStroke(n, 'u', body.upperBorder, bc, alpha, -1);
        }
      }
      const polygons = (trunk ? trunk.polygons : []).concat(body ? [body.polygon] : []);
      const mask = (style.thinOutline || borderOn) && polygons.length ? edgeMask(n, polygons) : '';
      if (borderOn) {
        const b = S.bundle(layout, n, side);
        if (trunk) {
          trunk.edges.forEach((line) => {
            trunks.push(`<path class="node-outline" data-node="${n.id}" d="${pathD(line, map, false)}" ${strokeAttrs(true)} ${mask}/>`);
          });
        }
        if (style.borderCaps && b) {
          trunks.push(`<path class="node-outline" data-node="${n.id}" d="${pathD([[b.faceX, b.lowerY], [b.faceX, b.upperY]], map, false)}" ${strokeAttrs()}/>`);
        }
      }
      outline(edges, trunks, mask);
    });

    if (style.nodeSums) {
      const xs = layout.nodes.map((n) => n.X);
      const offset = 0.06 * (Math.max(...xs) - Math.min(...xs));
      const sumPaint = C.paint(C.parse(style.labelColor));
      layout.nodes.forEach((n) => {
        [[n.outPx, -offset], [n.inPx, offset]].forEach(([total, dx]) => {
          if (!(total > 0)) return;
          const x = map.x(n.X + dx);
          sums.push(`<line class="node-sum" x1="${fmt(x)}" x2="${fmt(x)}" y1="${fmt(map.y(n.Y - total / 2))}" ` +
            `y2="${fmt(map.y(n.Y + total / 2))}" stroke="${sumPaint.color}" stroke-width="2"/>`);
        });
      });
    }

    layout.nodes.forEach((n) => {
      const nodePaint = C.paint(C.opaque(colors.node[n.id]));
      if (style.showPoints) {
        dots.push(`<circle class="node-dot" data-node="${n.id}" cx="${fmt(map.x(n.X))}" cy="${fmt(map.y(n.Y))}" r="4.5" ` +
          `fill="#FFFFFF" stroke="${nodePaint.color}" stroke-width="2"/>`);
      }
      const text = n.hasLabel ? n.label : (style.showIndices ? String(n.node) : '');
      if (!style.labels || !text) return;
      const font = fontFor(n, style);
      const pos = labelPlacement(n, style, font.size);
      const labelPaint = C.paint(labelColor(n, colors, style));
      labels.push(`<text class="node-label" data-node="${n.id}" x="${fmt(map.x(pos.X))}" y="${fmt(map.y(pos.Y))}" text-anchor="${pos.anchor}" ` +
        `dominant-baseline="central" font-family="${FONT_FAMILY}" font-size="${fmt(font.size)}" ` +
        `font-weight="${font.weight}" font-style="${font.slant}" fill="${labelPaint.color}" ` +
        `fill-opacity="${labelPaint.opacity}">${esc(text)}</text>`);
    });

    const bg = colors.bg && colors.bg.a > 0
      ? `<rect class="background" x="0" y="0" width="${frame.width}" height="${frame.height}" ${fillAttrs(colors.bg)}/>`
      : '';
    return `<defs>${defs.join('')}</defs>${bg}` +
      `<g class="layer-ribbons">${ribbons.join('')}</g>` +
      `<g class="layer-trunks">${trunks.join('')}</g>` +
      `<g class="layer-sums">${sums.join('')}</g>` +
      '<g class="layer-traces"></g>' +
      `<g class="layer-hits">${hits.join('')}</g>` +
      `<g class="layer-dots">${dots.join('')}</g>` +
      `<g class="layer-labels">${labels.join('')}</g>` +
      '<g class="layer-fills"></g><g class="layer-trace-labels" pointer-events="none"></g>';
  }

  // Canvas-based text metrics in pixels, matching the SVG label font.
  let measureContext = null;
  function makeMeasure(style) {
    return (label, cex, font) => {
      if (typeof document === 'undefined') return { width: 0.55 * style.fontSize * cex * String(label).length, height: 0.75 * style.fontSize * cex };
      measureContext = measureContext || document.createElement('canvas').getContext('2d');
      const f = fontFor({ cex, font }, style);
      measureContext.font = `${f.slant} ${f.weight} ${f.size}px ${FONT_FAMILY}`;
      return { width: measureContext.measureText(String(label)).width, height: 0.75 * f.size };
    };
  }

  SK.render = { render, resolveColors, makeMeasure, traceStops, labelPlacement, labelColor, FONT_FAMILY };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
