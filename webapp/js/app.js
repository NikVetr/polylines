/* Interactive Sankey app: state, controls, file loading, rendering, interaction and export. */
(function () {
  'use strict';
  const { layout: L, color: C, csv: CSV, shapes: S, render: R, paths: P } = window.SK;
  const $ = (selector) => document.querySelector(selector);
  const STORAGE_KEY = 'polylines-sankey-v1';
  const MARGIN = 18;
  const HISTORY_LIMIT = 100;
  const TRACE_LABEL = { font: 10.5, minFont: 8, countGap: 9, countSpacing: 13, crowdedGap: 48, maxGap: 180 };

  const DEFAULT_STYLE = {
    colorBy: 'source',
    palette: 'polylines',
    singleColor: '#5B8CBA',
    opacity: 0.8,
    gradient: false,
    gradientMode: 'merge',
    useMid: true,
    flatten: true,
    bg: '#FFFFFF',
    thinOutline: true,
    border: 'none',
    borderWidth: 1,
    borderCaps: true,
    nodeBorder: 'auto',
    nodeBorderWidth: 1,
    labels: true,
    showIndices: false,
    showPoints: false,
    nodeSums: false,
    fontSize: 13,
    labelPosition: 'auto',
    labelColorBy: 'single',
    labelColor: '#1F2937',
    labelCounts: true,
    canvasWidth: null,
    canvasHeight: null,
    collision: 'wall',
    tracePaths: true,
    maxHoverSubstrands: 200,
    traceCounts: true,
    tracePercents: false,
    gatherInNodes: false,
    tileInNodes: false,
    substrands: false
  };

  const svg = $('#plot');
  const stageEl = $('#stage');

  let state = blankState();
  let layout = null;
  let colors = null;
  let frame = null;
  let displayScale = 1;
  let traces = null; // { subs, paths, total } when item-level pathways can be traced
  let lastPointer = null;
  let selection = null; // { kind: 'flow' | 'node', key, paths, member, single }: traces pinned by the click
  let hover = null;
  let drag = null;
  let pan = null; // { x0, y0, left, top, moved } while dragging empty space to scroll the panel
  let routeScope = 'all';
  let renderQueued = false;
  let saveTimer = null;
  const history = { stack: [], index: -1 };

  function blankState() {
    return {
      version: 1,
      labelCountsVersion: 1,
      traceCountsVersion: 1,
      mode: 'layered',
      matrices: [],
      stageNames: [],
      dimnames: [],
      loc: [],
      locName: '',
      colorMatrices: null,
      colorNames: [],
      midMatrices: null,
      midNames: [],
      flowOverrides: {},
      midOverrides: {},
      nodeColors: {},
      mergeColors: {},
      nodeSlots: null,
      manualOrder: { start: {}, end: {} },
      params: Object.assign({}, L.DEFAULT_PARAMS),
      stageParams: [],
      style: Object.assign({}, DEFAULT_STYLE),
      valueScale: 1,
      trustedNames: false,
      paths: null,
      pathUnit: null,
      setNames: [],
      sourceTable: null,
      presentation: { mode: 'native', zoom: 1 },
      view: null
    };
  }

  // ---------- small helpers ----------

  const round = (v, digits = 6) => Number(Number(v).toPrecision(digits));
  const natural = (a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' });
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const fmtValue = (v) => (Number.isInteger(v) ? v.toLocaleString()
    : Math.abs(v) >= 100 ? v.toFixed(1) : Number(v.toPrecision(4)).toString());
  const fmtNodeCount = (v) => {
    if (Math.abs(v) < 10000) return fmtValue(v);
    const [mantissa, exponent] = v.toExponential(0).split('e');
    return `${Number(mantissa)}E${Number(exponent).toString().padStart(2, '0')}`;
  };
  const isDefaultName = (name) => !name || /^V?\d+$/.test(name);
  // Matrix dimnames like V1 or 1 are R defaults, but names from an imported table are real categories.
  const usableName = (name, data = state) => !!name && (data.trustedNames || !isDefaultName(name));

  function download(name, content, type) {
    const blob = content instanceof Blob ? content : new Blob([content], { type: type || 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = name;
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(url), 2000);
  }

  function showError(message, offerUndo) {
    const el = $('#banner');
    el.hidden = !message;
    el.textContent = message || '';
    const canUndo = history.index > 0 || (history.index === 0 && history.stack[0] !== JSON.stringify(state));
    if (message && offerUndo && canUndo) {
      const undo = document.createElement('button');
      undo.className = 'mini-btn banner-action';
      undo.textContent = 'Undo last change';
      // Committed bad edits are the top history entry; uncommitted ones revert to it.
      undo.addEventListener('click', () => {
        restore(history.stack[history.index] === JSON.stringify(state) ? history.index - 1 : history.index);
      });
      el.append(' ', undo);
    }
  }

  let noticeTimer = null;
  function notify(message) {
    const el = $('#notice');
    el.textContent = message;
    el.hidden = false;
    clearTimeout(noticeTimer);
    noticeTimer = setTimeout(() => { el.hidden = true; }, 7000);
  }

  function showWarnings(list) {
    const el = $('#warnings');
    el.hidden = !list.length;
    el.textContent = list.join(' ');
  }

  // ---------- model plumbing ----------

  // loc as drawn: with "Totals in labels", each label is followed by its node's total.
  function labeledLoc(data = state) {
    if (!data.style.labelCounts) return data.loc;
    const sum = (a) => a.reduce((t, v) => t + (v || 0), 0);
    const mats = data.matrices;
    const total = (row, id) => {
      if (data.mode === 'graph') return Math.max(sum(mats[0][id] || []), sum(mats[0].map((r) => r[id])));
      const s = row.set - 1;
      const i = row.node - 1;
      return Math.max(s > 0 && mats[s - 1] ? sum(mats[s - 1].map((r) => r[i])) : 0, mats[s] && mats[s][i] ? sum(mats[s][i]) : 0);
    };
    return data.loc.map((row, id) => (row.label ? Object.assign({}, row, { label: `${row.label} (${fmtNodeCount(total(row, id))})` }) : row));
  }

  function model(data = state) {
    return {
      mode: data.mode,
      matrices: data.matrices,
      loc: labeledLoc(data),
      params: data.params,
      stageParams: data.stageParams,
      manualOrder: data.manualOrder,
      valueScale: data.valueScale,
      paths: data.paths
    };
  }

  function colorData(data = state) {
    const locColors = {};
    const locMergeColors = {};
    data.loc.forEach((row) => {
      const key = `${row.set}:${row.node}`;
      if (row.color) locColors[key] = row.color;
      if (row.merge_col) locMergeColors[key] = row.merge_col;
    });
    return {
      flowMatrices: data.colorMatrices,
      midMatrices: data.midMatrices,
      flowOverrides: data.flowOverrides,
      midOverrides: data.midOverrides,
      nodeColors: data.nodeColors,
      mergeColors: data.mergeColors,
      nodeSlots: data.nodeSlots,
      locColors,
      locMergeColors
    };
  }

  function computeFrame(data = state) {
    const box = stageEl.getBoundingClientRect();
    let width = data.style.canvasWidth || Math.max(320, Math.floor(box.width));
    let minHeight = 240;
    if (data.sourceTable && data.matrices.length) {
      const sets = nodeSets(data);
      const longestLabel = Math.max(...sets.flat().map((n) => measure(data)(n.label, 1, 1).width));
      if (!data.style.canvasWidth) width = Math.max(width, data.sourceTable.minCanvasWidth || 0, data.sourceTable.spreadWidth || 0,
        Math.ceil((sets.length - 1) * Math.max(160, longestLabel + 50) + 2 * MARGIN));
      // Allow scrolling when many categories need more room than the viewport can give them.
      minHeight = Math.max(minHeight, Math.ceil((Math.max(...sets.map((g) => g.length)) - 1) *
        (data.style.fontSize * 1.5 + FILL_BAR.height + FILL_BAR.gap + 4) / (1 - STACK_FILL) + 2 * MARGIN));
    }
    const subset = window.SK.tableControls.header(data.sourceTable, width - 2 * MARGIN);
    const height = data.style.canvasHeight || Math.max(minHeight + subset.height, Math.floor(box.height));
    frame = { width, height, subset, margin: { left: MARGIN, top: MARGIN + subset.height, right: MARGIN, bottom: MARGIN } };
    if (data === state) {
      svg.setAttribute('width', width);
      svg.setAttribute('height', height);
      svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
    }
  }

  const viewFor = (v) => ({ xlim: v.xlim, ylim: v.ylim, width: frame.width - 2 * MARGIN, height: frame.height - frame.margin.top - MARGIN });
  const measure = (data = state) => R.makeMeasure(data.style);

  // How far (plot px) to move each stage's later columns so that the steepest forward strand of
  // every gap has the same slope (rise over run between node faces), with no gap narrower than
  // MIN_GAP_SHARE of the widest (a strand's rise counts as at least that share of the largest rise)
  // nor too narrow to draw its turns. Within the current width when that slope is at most
  // MAX_RISE_PER_RUN (or the width is fixed); otherwise at that slope, and the moves sum to how much
  // the canvas must grow. Empty when no column needs to move.
  const MAX_RISE_PER_RUN = 2;
  const MIN_GAP_SHARE = 0.5;
  function spreadMoves(lay, fixed) {
    if (lay.mode !== 'layered') return new Map();
    const flows = lay.flows.filter((f) => f.dir > 0).map((f) => ({ stage: f.stage, run: f.endX - f.startX,
      w: f.w, rise: Math.abs(f.endY + f.endOffset - f.startY - f.startOffset) }));
    const top = Math.max(0, ...flows.map((f) => f.rise));
    if (!(top > 0)) return new Map();
    // The shortest run at which layout.js roundedRibbon can still turn the strand, with a margin.
    const drawable = (f) => 1.15 * (f.rise > 1.1 * f.w ? 1.1 * f.w : Math.sqrt(f.rise * (2.2 * f.w - f.rise)));
    const shifts = (slope) => {
      const by = new Map();
      flows.forEach((f) => {
        const px = Math.max(Math.max(f.rise, MIN_GAP_SHARE * top) / slope, drawable(f)) - f.run;
        by.set(f.stage, Math.max(by.has(f.stage) ? by.get(f.stage) : -Infinity, px));
      });
      return by;
    };
    const sum = (m) => [...m.values()].reduce((t, px) => t + px, 0);
    // The moves shrink as the slope grows: bisect for the slope that keeps the width.
    let lo = 1e-6, hi = 1e6;
    for (let i = 0; i < 100; i++) {
      const mid = Math.sqrt(lo * hi);
      if (sum(shifts(mid)) > 0) lo = mid; else hi = mid;
    }
    const balanced = fixed || hi <= MAX_RISE_PER_RUN;
    const moves = shifts(balanced ? hi : MAX_RISE_PER_RUN);
    if (balanced) {
      const rest = sum(moves) / moves.size;
      moves.forEach((px, k) => moves.set(k, px - rest));
    }
    return [...moves.values()].some((px) => Math.abs(px) > 0.5) ? moves : new Map();
  }

  // Imported tables move later columns apart when strands need more run: always where a
  // constant-width turn cannot be drawn, and with opts.spread (a freshly generated layout) per
  // spreadMoves. The canvas widens to keep the scale, unless its width is fixed (then turns that
  // cannot be drawn widen the view, shrinking the drawing). Growth for drawable turns is kept in minCanvasWidth,
  // which Fit view resets (to be grown again as the layout needs); growth for spreadMoves in
  // spreadWidth, which belongs to the generated layout. Never during a drag: the drag must stop at
  // a drawable position instead of moving columns frame by frame.
  function computeLayout(data, view, opts = {}) {
    const grows = !drag && data.sourceTable;
    const move = (moves, key) => {
      const sx = (frame.width - 2 * MARGIN) / (view.xlim[1] - view.xlim[0]);
      moves.forEach((px, stage) => data.loc.filter(n => n.set >= stage + 2).forEach(n => { n.x += px / sx; }));
      const growth = [...moves.values()].reduce((t, px) => t + px, 0);
      if (growth > 1e-9) {
        view.xlim[1] += growth / sx;
        if (data.style.canvasWidth) return;
        frame.width += Math.ceil(growth);
        data.sourceTable[key] = frame.width;
      }
    };
    // A fixed canvas shrinks the drawing as columns move, so each move gains less: allow more passes.
    const passes = data.matrices.length * (opts.spread ? 4 : 2) * (data.style.canvasWidth ? 3 : 1);
    for (let pass = 0; pass <= passes; pass++) {
      let lay;
      try { lay = R.liftLabels(L.computeLayout(model(data), viewFor(view), { measure: measure(data) }), data.style); }
      catch (err) {
        if (!grows || !err.requiredRun || !err.flow || err.flow.dir < 0) throw err;
        const px = err.requiredRun - err.flow.dir * (err.flow.endX - err.flow.startX);
        if (!(px > 0)) throw err;
        move(new Map([[err.flow.stage, px]]), 'minCanvasWidth');
        continue;
      }
      // Lay out the moved columns once more; the last pass draws as is.
      const moves = opts.spread && grows && pass < passes ? spreadMoves(lay, !!data.style.canvasWidth) : new Map();
      if (!moves.size) return lay;
      move(moves, 'spreadWidth');
    }
    throw new L.SankeyError('Could not find enough column spacing for constant-width ribbons. Increase column spacing or reduce the value scale.');
  }

  function scalePresentation() {
    const { mode, zoom } = state.presentation;
    const box = stageEl.getBoundingClientRect();
    let base = 1;
    if (mode === 'panel') base = Math.min((box.width - 1) / frame.width, (box.height - 1) / frame.height);
    if (mode === 'height') {
      const scrollbar = frame.width * box.height / frame.height > box.width ? 17 : 0;
      base = (box.height - scrollbar - 1) / frame.height;
    }
    displayScale = base * zoom;
    svg.style.width = `${frame.width * displayScale}px`;
    svg.style.height = `${frame.height * displayScale}px`;
    // The resize handle sits on the canvas's bottom-right corner, in the panel's scrolled content
    // (SVG elements have no offsetLeft/offsetTop, so measure against the panel).
    const handle = $('#canvas-resize');
    const plot = svg.getBoundingClientRect();
    const panel = stageEl.getBoundingClientRect();
    handle.style.left = `${plot.left - panel.left + stageEl.scrollLeft + frame.width * displayScale - 16}px`;
    handle.style.top = `${plot.top - panel.top + stageEl.scrollTop + frame.height * displayScale - 16}px`;
    $('#fit-panel').setAttribute('aria-pressed', String(mode === 'panel'));
    $('#fit-height').setAttribute('aria-pressed', String(mode === 'height'));
    $('#zoom-out').disabled = zoom <= 0.1;
    $('#zoom-in').disabled = zoom >= 10;
  }

  // Zoom the display about a screen point (the content under it stays put).
  function zoomAt(cx, cy, factor) {
    const zoom = Math.max(0.1, Math.min(10, state.presentation.zoom * factor));
    if (zoom === state.presentation.zoom) return;
    const before = svg.getBoundingClientRect();
    const fx = (cx - before.left) / before.width;
    const fy = (cy - before.top) / before.height;
    state.presentation = Object.assign({}, state.presentation, { zoom });
    scalePresentation();
    const after = svg.getBoundingClientRect();
    stageEl.scrollLeft += after.left + fx * after.width - cx;
    stageEl.scrollTop += after.top + fy * after.height - cy;
    scheduleSave();
  }

  // The handle at the canvas's bottom-right corner resizes it. The plot keeps its on-screen size and
  // scale (the view grows or shrinks with the canvas), so the new room opens on the right and below.
  function bindCanvasResize() {
    const handle = $('#canvas-resize');
    let start = null;
    handle.addEventListener('pointerdown', (event) => {
      if (event.button !== 0 || !layout || !state.view) return;
      event.preventDefault();
      event.stopPropagation();
      handle.setPointerCapture(event.pointerId);
      if (state.presentation.mode !== 'native') state.presentation = { mode: 'native', zoom: displayScale };
      start = { x: event.clientX, y: event.clientY, width: frame.width, height: frame.height,
        plotW: frame.width - 2 * MARGIN, plotH: frame.height - frame.margin.top - MARGIN, view: JSON.parse(JSON.stringify(state.view)) };
      svg.classList.add('resizing');
    });
    handle.addEventListener('pointermove', (event) => {
      if (!start) return;
      const width = Math.max(320, Math.round(start.width + (event.clientX - start.x) / displayScale));
      const height = Math.max(240, Math.round(start.height + (event.clientY - start.y) / displayScale));
      const kx = (width - start.width + start.plotW) / start.plotW;
      const ky = (height - start.height + start.plotH) / start.plotH;
      const { xlim, ylim } = start.view;
      state.style.canvasWidth = width;
      state.style.canvasHeight = height;
      state.view = { xlim: [xlim[0], round(xlim[0] + (xlim[1] - xlim[0]) * kx)], ylim: [round(ylim[1] - (ylim[1] - ylim[0]) * ky), ylim[1]] };
      requestRender();
    });
    const end = (event) => {
      if (!start) return;
      start = null;
      svg.classList.remove('resizing');
      if (handle.hasPointerCapture(event.pointerId)) handle.releasePointerCapture(event.pointerId);
      update({ commit: true });
    };
    handle.addEventListener('pointerup', end);
    handle.addEventListener('pointercancel', end);
  }

  function changePresentation(mode, factor = 1) {
    const previous = state.presentation;
    state.presentation = { mode: mode || previous.mode, zoom: mode ? 1 : Math.max(0.1, Math.min(10, previous.zoom * factor)) };
    scalePresentation();
    stageEl.scrollTo(0, 0);
    pushHistory();
    scheduleSave();
  }

  // Labels drawn outside node bodies, in plot pixels, for view fitting.
  function labelBoxes(lay, data = state) {
    const out = [];
    const m = measure(data);
    lay.nodes.forEach((n) => {
      const bar = fillBarBox(lay, n);
      out.push([bar.x0 - bar.labelGap - bar.labelWidth, bar.Y - FILL_BAR.height],
        [bar.x0 + bar.width + bar.labelGap + bar.labelWidth, bar.Y]);
      const text = n.hasLabel ? n.label : (data.style.showIndices ? String(n.node) : '');
      if (!data.style.labels || !text) return;
      const size = m(text, n.cex, n.font);
      const role = S.nodeRole(n);
      if (data.style.labelPosition === 'above') {
        const pos = R.labelPlacement(n, data.style, data.style.fontSize * n.cex);
        out.push([pos.X - size.width / 2, pos.Y + size.height], [pos.X + size.width / 2, pos.Y + size.height]);
      } else if (n.hasLabel && n.widthPx > 0) {
        out.push([n.X - size.width / 2, n.Y], [n.X + size.width / 2, n.Y]);
      } else if (role === 'source') {
        out.push([n.X - 9 - size.width, n.Y]);
      } else if (role === 'sink') {
        out.push([n.X + 9 + size.width, n.Y]);
      } else {
        out.push([n.X, n.Y + Math.max(n.inPx, n.outPx) / 2 + 1.6 * size.height]);
      }
    });
    return out;
  }

  function nodeLabelBounds(lay, data) {
    const boxes = [];
    if (!data.style.labels) return boxes;
    const m = measure(data);
    lay.nodes.forEach(n => {
      const text = n.hasLabel ? n.label : (data.style.showIndices ? String(n.node) : '');
      if (!text) return;
      const height = data.style.fontSize * n.cex;
      const pos = R.labelPlacement(n, data.style, height);
      const width = m(text, n.cex, n.font).width;
      const x1 = pos.X + width * (pos.anchor === 'start' ? 1 : pos.anchor === 'middle' ? 0.5 : 0);
      boxes.push({ x0: x1 - width, x1, y0: pos.Y - height / 2, y1: pos.Y + height / 2 });
    });
    return boxes;
  }

  const labelRightEdge = (boxes, y0 = -Infinity, y1 = Infinity) =>
    Math.max(-Infinity, ...boxes.filter(b => b.y1 >= y0 && b.y0 <= y1).map(b => b.x1));

  const countGapFor = displacement => displacement > 1
    ? Math.min(TRACE_LABEL.maxGap, Math.max(TRACE_LABEL.crowdedGap, TRACE_LABEL.countGap + displacement / 2)) : TRACE_LABEL.countGap;

  function countGutter(lay, data) {
    const nodes = new Map(lay.nodes.map(n => [n.key, n]));
    const counts = new Map();
    data.paths.forEach(path => {
      const n = path.nodes.map((ni, s) => nodes.get(`${s + 1}:${ni + 1}`))
        .reduce((a, b) => a.X + a.widthPx / 2 >= b.X + b.widthPx / 2 ? a : b);
      counts.set(n.id, (counts.get(n.id) || 0) + (path.members ? Math.max(1, path.members.length) : 1));
    });
    return countGapFor((Math.max(...counts.values()) - 1) * TRACE_LABEL.countSpacing);
  }

  // Data limits that contain every strand, node body and label (a few refinement passes,
  // because strand thickness in pixels depends on the limits themselves).
  function fitView(data = state, opts = {}) {
    if (!data.loc.length) throw new L.SankeyError('No node locations (loc) are loaded.');
    const xs = data.loc.map((r) => r.x);
    const ys = data.loc.map((r) => r.y);
    const spanX = Math.max(...xs) - Math.min(...xs) || 1;
    const spanY = Math.max(...ys) - Math.min(...ys) || 1;
    let view = {
      xlim: [Math.min(...xs) - 0.1 * spanX, Math.max(...xs) + 0.1 * spanX],
      ylim: [Math.min(...ys) - 0.15 * spanY, Math.max(...ys) + 0.15 * spanY]
    };
    for (let pass = 0; pass < 4; pass++) {
      const lay = computeLayout(data, view, opts);
      const box = S.geometryBounds(lay);
      labelBoxes(lay, data).forEach((p) => {
        box.x0 = Math.min(box.x0, p[0]);
        box.x1 = Math.max(box.x1, p[0]);
        box.y0 = Math.min(box.y0, p[1]);
        box.y1 = Math.max(box.y1, p[1]);
      });
      if (lay.mode === 'layered' && data.paths && data.paths.length && data.style.tracePaths && data.style.traceCounts) {
        const countMeasure = R.makeMeasure({ fontSize: TRACE_LABEL.font });
        const width = Math.max(...data.paths.map((p) => countMeasure(fmtValue(p.weight), 1, 1).width));
        box.x1 = Math.max(box.x1, labelRightEdge(nodeLabelBounds(lay, data))) + countGutter(lay, data) + width;
      }
      const x0 = lay.tf.x(box.x0);
      const x1 = lay.tf.x(box.x1);
      const y0 = lay.tf.y(box.y0);
      const y1 = lay.tf.y(box.y1);
      const padX = 0.02 * (x1 - x0);
      const padY = 0.03 * (y1 - y0);
      view = { xlim: [x0 - padX, x1 + padX], ylim: [y0 - padY, y1 + padY] };
    }
    return { xlim: view.xlim.map((v) => round(v)), ylim: view.ylim.map((v) => round(v)) };
  }

  // ---------- render pipeline ----------

  function update(options) {
    const opts = options || {};
    // Fitting recomputes how wide an imported table's canvas must be, so a canvas widened earlier
    // shrinks back unless the current layout still needs the room (keepCanvas: refit within it).
    // spread: a freshly generated layout, whose tight column gaps computeLayout widens.
    if (opts.fit && !opts.keepCanvas && state.sourceTable) {
      state.sourceTable.minCanvasWidth = 0;
      if (opts.spread) state.sourceTable.spreadWidth = 0;
    }
    computeFrame();
    try {
      if (!state.matrices.length || !state.loc.length) {
        layout = null;
        svg.innerHTML = '';
        svg.classList.remove('stale');
        showError(state.matrices.length ? 'Load node locations (loc) or use "Auto-layout nodes".' : '');
      } else {
        if (opts.fit || !state.view) state.view = fitView(state, { spread: opts.spread });
        layout = computeLayout(state, state.view);
        colors = R.resolveColors(layout, colorData(), state.style);
        const warnings = layout.warnings.slice();
        traces = null;
        let subs = null;
        if (state.paths && P.consistent(layout, state.paths)) {
          if (state.style.tracePaths || state.style.substrands) subs = P.substrands(layout, state.paths);
          if (state.style.tracePaths) traces = { subs, paths: state.paths, total: state.paths.reduce((t, x) => t + x.weight, 0) };
        } else if (state.paths) {
          warnings.push('The pathway data no longer matches the flow values, so pathway tracing is off.');
        }
        const extras = { substrandLines: subs && state.style.substrands ? P.boundaryLines(layout, subs) : null };
        svg.setAttribute('width', frame.width);
        svg.setAttribute('height', frame.height);
        svg.setAttribute('viewBox', `0 0 ${frame.width} ${frame.height}`);
        svg.innerHTML = R.render(layout, colors, state.style, frame, extras);
        svg.insertAdjacentHTML('beforeend', frame.subset.markup);
        svg.classList.remove('stale');
        showError(null);
        showWarnings(warnings);
      }
    } catch (err) {
      // Keep the last good drawing visible (dimmed, inert) so there is context for undoing.
      layout = null;
      svg.classList.add('stale');
      showError(err instanceof L.SankeyError ? err.message : `Unexpected error: ${err.message}`, true);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
    scalePresentation();
    if (hover && !hover.single && state.paths && state.paths.length > state.style.maxHoverSubstrands) {
      hover = null;
      showHoverInfo(null);
    }
    if (selection && layout && !findSelected()) selection = null;
    // Hover indices refer to the previous layout; drop them when that strand/node is gone.
    if (hover && hover.paths && !traces) hover = Object.assign({}, hover, { paths: null });
    if (hover && !(layout && (hover.kind === 'flow' ? layout.flows : layout.nodes)[hover.index])) {
      hover = null;
      showHoverInfo(null);
    }
    applyHighlight();
    refreshControls();
    refreshDataPanel();
    if (!opts.keepInspector) refreshInspector();
    if (opts.commit) pushHistory();
    scheduleSave();
  }

  // keepInspector: live edits from inspector inputs must not rebuild the input being dragged.
  let queuedKeepInspector = true;
  function requestRender(keepInspector) {
    queuedKeepInspector = queuedKeepInspector && !!keepInspector;
    if (renderQueued) return;
    renderQueued = true;
    requestAnimationFrame(() => {
      const keep = queuedKeepInspector;
      renderQueued = false;
      queuedKeepInspector = true;
      if (drag && drag.goal) {
        placeNode(drag, drag.goal);
        drag.goal = null;
      }
      update({ keepInspector: keep });
    });
  }

  // ---------- controls ----------

  const PALETTE_OPTIONS = [['polylines', 'polylines'], ['okabe-ito', 'Okabe–Ito'], ['tableau', 'Tableau 10'],
    ['set2', 'Set 2'], ['dark2', 'Dark 2'], ['viridis', 'Viridis']];
  const isBoundary = () => currentParam('route') === 'boundary';

  const CONTROL_SECTIONS = [
    {
      title: 'Layout',
      items: [
        { key: 'valueScale', target: 'state', type: 'number', label: 'Value scale', min: 0, positive: true, step: 'any',
          help: 'Strand thickness = value × scale, in x units. Tooltips and the inspector show raw values.' },
        { key: 'n', target: 'params', type: 'number', label: 'Samples per strand', min: 2, step: 1, integer: true },
        { key: 'curvature', target: 'params', type: 'range', label: 'Curvature', min: 0, max: 1, step: 0.01 },
        { key: 'merge_frac', target: 'params', type: 'number', label: 'Merge fraction', min: 0, step: 0.005,
          help: 'Length of the merged trunk near nodes, as a fraction of the shortest strand.' },
        { key: 'merge_length', target: 'params', type: 'number', label: 'Merge length', min: 0, step: 0.01,
          nullable: true, placeholder: 'auto', help: 'Fixed merge length in x units; overrides the fraction.' },
        { key: 'default_node_width', target: 'params', type: 'number', label: 'Default node width', min: 0, step: 0.01 },
        { key: 'node_label_pad', target: 'params', type: 'number', label: 'Label padding', min: 0, step: 0.005,
          help: 'Horizontal padding (x units) added on each side of label-derived node widths.' },
        { key: 'node_label_pad_frac', target: 'params', type: 'number', label: 'Label padding (frac.)', min: 0, step: 0.01 },
        { key: 'adjust_loc', target: 'params', type: 'checkbox', label: 'Adjust node y positions',
          help: 'Isotonic adjustment so neighbouring node bundles do not overlap (adjust_loc).' },
        { key: 'min_node_gap', target: 'params', type: 'number', label: 'Minimum node gap', min: 0, step: 0.005,
          showIf: () => state.params.adjust_loc },
        { key: 'adjust_loc_expand', target: 'params', type: 'checkbox', label: 'Allow expanding y range',
          showIf: () => state.params.adjust_loc },
        { key: 'adjust_node_height', target: 'params', type: 'select', label: 'Fit labels by',
          options: [['none', 'nothing'], ['label', 'shrinking labels'], ['node', 'growing node bodies']] },
        { key: 'node_label_vpad', target: 'params', type: 'number', label: 'Label v-padding', min: 0, step: 0.005,
          showIf: () => state.params.adjust_node_height !== 'none' },
        { key: 'node_label_vpad_frac', target: 'params', type: 'number', label: 'Label v-padding (frac.)', min: 0, step: 0.05,
          showIf: () => state.params.adjust_node_height !== 'none' },
        { key: 'require_forward', target: 'params', type: 'checkbox', label: 'Require left-to-right',
          showIf: () => state.mode === 'graph' },
        { key: 'balance', target: 'params', type: 'select', label: 'Unbalanced nodes',
          options: [['allow', 'allow'], ['warn', 'warn'], ['error', 'error']], showIf: () => state.mode === 'graph' }
      ]
    },
    {
      title: 'Routing & order',
      scoped: true,
      items: [
        { key: 'route', target: 'stage', type: 'select', label: 'Route', options: [['bezier', 'Bezier curves'], ['boundary', 'Packed boundaries']] },
        { key: 'boundary_group', target: 'stage', type: 'select', label: 'Boundary group',
          options: [['target', 'by target (fan-in)'], ['source', 'by source (fan-out)'], ['stage', 'whole stage']], showIf: isBoundary },
        { key: 'boundary_compensation', target: 'stage', type: 'select', label: 'Width compensation',
          options: [['none', 'none'], ['full', 'full'], ['gap_budget', 'gap budget']], showIf: isBoundary },
        { key: 'boundary_ease', target: 'stage', type: 'range', label: 'Boundary easing', min: 0.3, max: 3, step: 0.05, showIf: isBoundary },
        { key: 'source_order', target: 'stage', type: 'select', label: 'Order at sources', options: L.SOURCE_ORDERS.map((o) => [o, o]) },
        { key: 'target_order', target: 'stage', type: 'select', label: 'Order at targets', options: L.TARGET_ORDERS.map((o) => [o, o]) }
      ]
    },
    {
      title: 'Colors',
      items: [
        { key: 'colorBy', target: 'style', type: 'select', label: 'Strand colors',
          options: () => [['file', 'from color files'], ['source', 'palette by source'], ['target', 'palette by target'],
            ['stage', 'palette by stage'], ['single', 'single color']].filter((o) => o[0] !== 'file' || state.colorMatrices) },
        { key: 'palette', target: 'style', type: 'select', label: 'Palette', options: PALETTE_OPTIONS,
          showIf: () => ['source', 'target', 'stage'].includes(state.style.colorBy) },
        { key: 'singleColor', target: 'style', type: 'color', label: 'Color', showIf: () => state.style.colorBy === 'single' },
        { key: 'opacity', target: 'style', type: 'range', label: 'Opacity', min: 0.05, max: 1, step: 0.01,
          showIf: () => state.style.colorBy !== 'file' },
        { key: 'gradient', target: 'style', type: 'checkbox', label: 'Gradients' },
        { key: 'gradientMode', target: 'style', type: 'select', label: 'Gradient mode',
          options: [['merge', 'merge (fade near nodes)'], ['path', 'path (along strand)']], showIf: () => state.style.gradient },
        { key: 'useMid', target: 'style', type: 'checkbox', label: 'Use mid colors',
          showIf: () => state.style.gradient && !!state.midMatrices },
        { key: 'flatten', target: 'style', type: 'checkbox', label: 'Opaque node trunks',
          help: 'Composite trunk colors onto the background so translucent strands do not double up.' },
        { key: 'bg', target: 'style', type: 'color', label: 'Background' }
      ]
    },
    {
      title: 'Borders & labels',
      items: [
        { key: 'thinOutline', target: 'style', type: 'checkbox', label: 'Thin plot outline',
          help: 'A thin black outline around strands and nodes, hidden while a selection is in focus.' },
        { key: 'border', target: 'style', type: 'optionalColor', label: 'Strand borders', defaultColor: '#333333' },
        { key: 'borderWidth', target: 'style', type: 'number', label: 'Border width', min: 0.1, step: 0.1,
          showIf: () => state.style.border !== 'none' },
        { key: 'borderCaps', target: 'style', type: 'checkbox', label: 'Border caps at nodes',
          showIf: () => state.style.border !== 'none' },
        { key: 'nodeBorder', target: 'style', type: 'nodeBorder', label: 'Node borders' },
        { key: 'nodeBorderWidth', target: 'style', type: 'number', label: 'Node border width', min: 0.1, step: 0.1,
          showIf: () => state.style.nodeBorder !== 'none' },
        { key: 'labels', target: 'style', type: 'checkbox', label: 'Show labels' },
        { key: 'showIndices', target: 'style', type: 'checkbox', label: 'Label unlabeled nodes by index',
          showIf: () => state.style.labels },
        { key: 'fontSize', target: 'style', type: 'number', label: 'Font size (px)', min: 4, step: 0.5, fit: true },
        { key: 'labelPosition', target: 'style', type: 'select', label: 'Label position',
          options: [['auto', 'in node bodies, else beside'], ['above', 'above nodes']],
          help: 'Above nodes, labels leave node bodies solid instead of fading them to a clear centre.' },
        { key: 'labelColorBy', target: 'style', type: 'select', label: 'Label color by',
          options: [['single', 'one color'], ['node', 'darker node color']] },
        { key: 'labelColor', target: 'style', type: 'color', label: 'Label color', showIf: () => state.style.labelColorBy === 'single' },
        { key: 'labelCounts', target: 'style', type: 'checkbox', label: 'Totals in labels', fit: true,
          help: 'Follow each label with its node total in parentheses, e.g. "Surgery (232)". Counts of 10,000 or more use compact scientific notation, e.g. "Cohort (1E04)"; hover details retain the full count.' },
        { key: 'showPoints', target: 'style', type: 'checkbox', label: 'Node points' },
        { key: 'nodeSums', target: 'style', type: 'checkbox', label: 'Node total bars' }
      ]
    },
    {
      title: 'Canvas & dragging',
      items: [
        { key: 'tracePaths', target: 'style', type: 'checkbox', label: 'Trace pathways on hover', fit: true,
          help: 'Hover a strand or node to trace every pathway through it; hold Alt or Ctrl/⌘ for a single pathway, or Shift for a single item. Above the hover limit, only single-substrand hover is enabled.',
          showIf: () => !!state.paths },
        { key: 'maxHoverSubstrands', target: 'style', type: 'number', label: 'Max substrands for hover', min: 0, step: 50, integer: true,
          help: 'Disable ordinary hover effects above this many unique pathways (default 200). Alt, Ctrl/⌘ and Shift hover still work. Raising this can slow the browser.',
          showIf: () => !!state.paths },
        { key: 'traceCounts', target: 'style', type: 'checkbox', label: 'Substrand counts', fit: true,
          help: 'Show each traced pathway or item count to the right of its rightmost node.',
          showIf: () => !!state.paths && state.style.tracePaths },
        { key: 'tracePercents', target: 'style', type: 'checkbox', label: 'Substrand percentages',
          help: 'White percentages along each traced substrand: its count divided by the full strand count at that edge.',
          showIf: () => !!state.paths && state.style.tracePaths },
        { key: 'gatherInNodes', target: 'style', type: 'checkbox', label: 'Gather traces in nodes',
          help: 'Traced pathways close up into one contiguous bundle along the top or bottom of each node they share, fanning back out to their strands at the node edges.',
          showIf: () => !!state.paths && state.style.tracePaths },
        { key: 'tileInNodes', target: 'style', type: 'checkbox', label: 'No overlaps in nodes',
          help: 'Where traced substrands that keep their order would overlap inside a node, cross that node on one shared curve, so they stay side by side but thin a little on steep stretches. Off (default): every substrand keeps its full width; overlaps show through the semi-transparent traces.',
          showIf: () => !!state.paths && state.style.tracePaths },
        { key: 'substrands', target: 'style', type: 'checkbox', label: 'Substrand lines',
          help: 'Thin lines dividing each strand into its pathways, or into single items when the data has item ids.',
          showIf: () => !!state.paths },
        { key: 'collision', target: 'style', type: 'select', label: 'Dragged node hits',
          options: [['wall', 'a wall (stops)'], ['push', 'neighbours (pushes them)']],
          help: 'What happens when a dragged node would leave a strand no horizontal room.' },
        { key: 'canvasWidth', target: 'style', type: 'number', label: 'Width (px)', min: 200, step: 10, nullable: true, placeholder: 'fit', integer: true },
        { key: 'canvasHeight', target: 'style', type: 'number', label: 'Height (px)', min: 150, step: 10, nullable: true, placeholder: 'fit', integer: true },
        { key: 'xlim', target: 'view', type: 'pair', label: 'x limits' },
        { key: 'ylim', target: 'view', type: 'pair', label: 'y limits' }
      ]
    }
  ];

  const controlRefs = [];

  function currentParam(key) {
    if (routeScope === 'all') return state.params[key];
    const override = state.stageParams[routeScope] || {};
    return key in override ? override[key] : state.params[key];
  }

  function getValue(item) {
    if (item.target === 'params') return state.params[item.key];
    if (item.target === 'stage') return currentParam(item.key);
    if (item.target === 'view') return state.view ? state.view[item.key] : null;
    if (item.target === 'state') return state[item.key];
    return state.style[item.key];
  }

  function setValue(item, value) {
    if (item.target === 'params') {
      state.params[item.key] = value;
    } else if (item.target === 'state') {
      state[item.key] = value;
    } else if (item.target === 'stage') {
      if (routeScope === 'all') {
        state.params[item.key] = value;
      } else {
        state.stageParams[routeScope] = Object.assign({}, state.stageParams[routeScope], { [item.key]: value });
      }
    } else if (item.target === 'view') {
      state.view = Object.assign({}, state.view, { [item.key]: value });
    } else {
      state.style[item.key] = value;
    }
  }

  function buildControls() {
    const host = $('#controls');
    CONTROL_SECTIONS.forEach((section) => {
      const details = document.createElement('details');
      details.className = 'section';
      details.open = true;
      details.innerHTML = `<summary>${section.title}</summary>`;
      if (section.scoped) {
        const row = document.createElement('div');
        row.className = 'row';
        row.innerHTML = '<label for="route-scope">Apply to</label><select id="route-scope"></select>';
        details.appendChild(row);
        row.querySelector('select').addEventListener('change', (e) => {
          routeScope = e.target.value === 'all' ? 'all' : Number(e.target.value);
          refreshControls();
        });
        const clear = document.createElement('button');
        clear.className = 'mini-btn wide';
        clear.id = 'clear-stage-params';
        clear.textContent = 'Clear per-stage settings';
        clear.addEventListener('click', () => {
          state.stageParams = [];
          routeScope = 'all';
          update({ commit: true });
        });
        details.appendChild(clear);
      }
      section.items.forEach((item) => details.appendChild(buildControl(item)));
      if (section.title === 'Canvas & dragging') {
        const fit = document.createElement('button');
        fit.className = 'mini-btn wide';
        fit.textContent = 'Fit view to content';
        fit.addEventListener('click', () => update({ fit: true, commit: true }));
        details.appendChild(fit);
      }
      host.appendChild(details);
    });
  }

  function buildControl(item) {
    const row = document.createElement('div');
    row.className = 'row';
    const id = `ctl-${item.key}`;
    row.innerHTML = `<label for="${id}" title="${esc(item.help || '')}">${esc(item.label)}</label>`;
    const commit = (value) => {
      setValue(item, value);
      update({ commit: true, fit: !!item.fit });
    };
    let input;
    let sync;
    if (item.type === 'select') {
      input = document.createElement('select');
      sync = (v) => {
        const options = typeof item.options === 'function' ? item.options() : item.options;
        input.innerHTML = options.map(([value, label]) => `<option value="${esc(value)}">${esc(label)}</option>`).join('');
        input.value = v;
      };
      input.addEventListener('change', () => commit(input.value));
    } else if (item.type === 'checkbox') {
      input = document.createElement('input');
      input.type = 'checkbox';
      sync = (v) => { input.checked = !!v; };
      input.addEventListener('change', () => commit(input.checked));
    } else if (item.type === 'color') {
      input = document.createElement('input');
      input.type = 'color';
      sync = (v) => { input.value = C.toHex(C.parse(v), false).toLowerCase(); };
      input.addEventListener('input', () => { setValue(item, input.value.toUpperCase()); requestRender(); });
      input.addEventListener('change', () => commit(input.value.toUpperCase()));
    } else if (item.type === 'range') {
      input = document.createElement('span');
      input.className = 'range';
      input.innerHTML = `<input type="range" min="${item.min}" max="${item.max}" step="${item.step}"><output></output>`;
      const slider = input.querySelector('input');
      const out = input.querySelector('output');
      sync = (v) => { slider.value = v; out.textContent = Number(v).toFixed(2); };
      slider.addEventListener('input', () => {
        out.textContent = Number(slider.value).toFixed(2);
        setValue(item, Number(slider.value));
        requestRender();
      });
      slider.addEventListener('change', () => commit(Number(slider.value)));
    } else if (item.type === 'optionalColor' || item.type === 'nodeBorder') {
      input = document.createElement('span');
      input.className = 'combo';
      const options = item.type === 'nodeBorder'
        ? '<option value="auto">auto (labeled)</option><option value="none">none</option><option value="custom">custom</option>'
        : '<option value="none">none</option><option value="custom">color</option>';
      input.innerHTML = `<select>${options}</select><input type="color">`;
      const select = input.querySelector('select');
      const picker = input.querySelector('input');
      sync = (v) => {
        const special = v === 'none' || v === 'auto';
        select.value = special ? v : 'custom';
        picker.hidden = special;
        if (!special) picker.value = C.toHex(C.parse(v), false).toLowerCase();
      };
      select.addEventListener('change', () => {
        commit(select.value === 'custom' ? (item.defaultColor || picker.value || '#333333').toUpperCase() : select.value);
      });
      picker.addEventListener('change', () => commit(picker.value.toUpperCase()));
    } else if (item.type === 'pair') {
      input = document.createElement('span');
      input.className = 'pair';
      input.innerHTML = '<input type="number" step="any"><input type="number" step="any">';
      const [a, b] = input.querySelectorAll('input');
      sync = (v) => {
        a.value = v ? round(v[0], 5) : '';
        b.value = v ? round(v[1], 5) : '';
      };
      const onChange = () => {
        const v = [Number(a.value), Number(b.value)];
        if (!(Number.isFinite(v[0]) && Number.isFinite(v[1]) && v[1] > v[0])) {
          showError(`${item.label} must be two increasing numbers.`);
          return;
        }
        commit(v);
      };
      a.addEventListener('change', onChange);
      b.addEventListener('change', onChange);
    } else {
      input = document.createElement('input');
      input.type = 'number';
      if (item.min !== undefined) input.min = item.min;
      input.step = item.step || 'any';
      if (item.placeholder) input.placeholder = item.placeholder;
      sync = (v) => { input.value = v === null || v === undefined ? '' : v; };
      input.addEventListener('change', () => {
        if (input.value === '' && item.nullable) {
          commit(null);
          return;
        }
        const v = item.integer ? Math.round(Number(input.value)) : Number(input.value);
        if (input.value === '' || !Number.isFinite(v) || (item.min !== undefined && v < item.min) ||
            (item.positive && !(v > 0))) {
          showError(`${item.label} must be a ${item.positive ? 'positive number' : 'number'}` +
            `${item.min !== undefined && !item.positive ? ` ≥ ${item.min}` : ''}.`);
          sync(getValue(item));
          return;
        }
        commit(v);
      });
    }
    input.id = id;
    row.appendChild(input);
    controlRefs.push({ item, row, sync });
    return row;
  }

  function refreshControls() {
    const scope = $('#route-scope');
    const stageCount = state.mode === 'graph' ? 1 : state.matrices.length;
    scope.innerHTML = '<option value="all">all stages</option>' +
      Array.from({ length: stageCount }, (_, s) => `<option value="${s}">stage ${s + 1}${state.stageParams[s] ? ' *' : ''}</option>`).join('');
    if (routeScope !== 'all' && routeScope >= stageCount) routeScope = 'all';
    scope.value = String(routeScope);
    $('#clear-stage-params').hidden = !state.stageParams.some(Boolean);
    controlRefs.forEach(({ item, row, sync }) => {
      row.hidden = item.showIf ? !item.showIf() : false;
      if (row.contains(document.activeElement) && document.activeElement.type !== 'checkbox' &&
          document.activeElement.tagName !== 'SELECT') return;
      sync(getValue(item));
    });
    $('#undo').disabled = history.index <= 0;
    $('#redo').disabled = history.index >= history.stack.length - 1;
  }

  // ---------- data panel ----------

  function fileList(el, names, onMove, onRemove) {
    el.innerHTML = names.map((name, i) => `<li><span title="${esc(name)}">${esc(name)}</span>` +
      (onMove ? `<button data-move="${i}" data-dir="-1" title="Move up"${i === 0 ? ' disabled' : ''}>↑</button>` +
      `<button data-move="${i}" data-dir="1" title="Move down"${i === names.length - 1 ? ' disabled' : ''}>↓</button>` : '') +
      (onRemove ? `<button data-remove="${i}" title="Remove">×</button>` : '') + '</li>').join('');
    el.querySelectorAll('[data-move]').forEach((b) => b.addEventListener('click', () => {
      onMove(Number(b.dataset.move), Number(b.dataset.dir));
    }));
    if (onRemove) {
      el.querySelectorAll('[data-remove]').forEach((b) => b.addEventListener('click', () => onRemove(Number(b.dataset.remove))));
    }
  }

  const swap = (arr, i, j) => {
    if (!arr || j < 0 || j >= arr.length) return;
    [arr[i], arr[j]] = [arr[j], arr[i]];
  };

  // Everything indexed by stage moves with its flow matrix.
  const stageArrays = () => [state.matrices, state.stageNames, state.dimnames, state.colorMatrices,
    state.colorNames, state.midMatrices, state.midNames, state.stageParams].filter(Boolean);

  function refreshDataPanel() {
    $('#mode-select').value = state.mode;
    window.SK.tableControls.render($('#table-controls'), state.sourceTable, rebuildTable);
    fileList($('#stage-list'), state.stageNames, state.sourceTable ? null : (i, d) => {
      if (i + d < 0 || i + d >= state.matrices.length) return;
      stageArrays().forEach((arr) => { [arr[i], arr[i + d]] = [arr[i + d], arr[i]]; });
      state.paths = null;
      clearKeyedEdits();
      update({ fit: true, commit: true });
    }, state.sourceTable ? null : (i) => {
      stageArrays().forEach((arr) => { if (i < arr.length) arr.splice(i, 1); });
      state.paths = null;
      if (state.colorMatrices && !state.colorMatrices.some(Boolean)) {
        state.colorMatrices = null;
        state.colorNames = [];
        if (state.style.colorBy === 'file') state.style.colorBy = 'source';
      }
      if (state.midMatrices && !state.midMatrices.some(Boolean)) {
        state.midMatrices = null;
        state.midNames = [];
      }
      clearKeyedEdits();
      update({ fit: true, commit: true });
    });
    $('#loc-name').textContent = state.loc.length ? `${state.locName || 'loc'} · ${state.loc.length} nodes` : 'none';
    const listFor = (key, namesKey, el) => fileList(el, state[namesKey], (i, d) => {
      swap(state[key], i, i + d);
      swap(state[namesKey], i, i + d);
      update({ commit: true });
    }, null);
    listFor('colorMatrices', 'colorNames', $('#color-list'));
    listFor('midMatrices', 'midNames', $('#mid-list'));
    $('#clear-colors').hidden = !state.colorMatrices;
    $('#clear-mids').hidden = !state.midMatrices;
  }

  // Per-strand edits are keyed by stage/row/column and become stale when matrices change.
  function clearKeyedEdits(data = state) {
    data.nodeSlots = null;
    data.flowOverrides = {};
    data.midOverrides = {};
    data.manualOrder = { start: {}, end: {} };
    if (data === state) selection = null;
  }

  // ---------- file loading ----------

  function looksLikeLoc(text) {
    const first = CSV.parse(text)[0] || [];
    const names = first.map((c) => c.trim().toLowerCase());
    return ['set', 'node', 'x', 'y'].every((n) => names.includes(n));
  }

  function classify(file) {
    if (/\.json$/i.test(file.name)) return 'project';
    if (looksLikeLoc(file.text)) return 'loc';
    if (/(^|[^a-z])loc([^a-z]|$)/i.test(file.name)) return 'loc';
    const T = window.SK.transform;
    const table = (() => {
      try { return T.parseTable(file.text); } catch (e) { return null; }
    })();
    // Named roles (source/target, id/stage, a count column) mark a data table even when all numeric.
    if (table && T.looksLikeDataTable(table)) return 'table';
    let numericError = null;
    try {
      CSV.parseMatrix(file.text, 'number', file.name);
      return 'stages';
    } catch (err) {
      numericError = err;
    }
    const colorsOk = (() => {
      try { CSV.parseMatrix(file.text, 'color', file.name); return true; } catch (e) { return false; }
    })();
    if (colorsOk) return /mid/i.test(file.name) ? 'mids' : 'colors';
    // A mostly numeric file is a flow matrix with a typo: report that, not a table import.
    const isNumber = (c) => c.trim() !== '' && Number.isFinite(Number(c));
    let body = CSV.parse(file.text);
    if (body.length && !body[0].every(isNumber)) body = body.slice(1);
    if (body.length && body.every((r) => !isNumber(r[0]))) body = body.map((r) => r.slice(1));
    const cells = body.flat();
    if (cells.length && cells.filter(isNumber).length / cells.length >= 0.75) {
      throw new L.SankeyError(`${file.name}: ${numericError.message}`);
    }
    if (table) return 'table';
    throw new L.SankeyError(`${file.name}: not recognised as a flow matrix, loc table, color matrix, data table or project file.`);
  }

  async function readFiles(fileList) {
    const files = await Promise.all([...fileList].map((f) => f.text().then((text) => ({ name: f.name, text }))));
    return files.sort((a, b) => natural(a.name, b.name));
  }

  // files: [{ name, text }]; role forces every file into one role.
  // Returns false when the files were handed to the table importer instead of applied.
  function applyFiles(files, role) {
    const groups = { stages: [], loc: [], colors: [], mids: [], project: [], table: [] };
    files.forEach((f) => groups[role || classify(f)].push(f));
    if (groups.table.length) {
      if (files.length > 1) {
        throw new L.SankeyError(`${groups.table[0].name} is a data table; load it on its own to open the table importer.`);
      }
      window.SK.importer.open(groups.table[0].text, groups.table[0].name);
      return false;
    }
    if (groups.project.length) {
      if (files.length > 1) throw new L.SankeyError('Load a project JSON file on its own.');
      let parsed;
      try {
        parsed = JSON.parse(groups.project[0].text);
      } catch (err) {
        throw new L.SankeyError(`${groups.project[0].name}: not valid JSON (${err.message}).`);
      }
      importProject(parsed);
      return true;
    }
    applyGroups(groups, {});
    return true;
  }

  function applyGroups(groups, opts) {
    if (groups.loc.length > 1) throw new L.SankeyError('Only one loc file can be loaded at a time.');
    const parseAll = (list, kind) => list.map((f) => {
      try {
        return CSV.parseMatrix(f.text, kind, f.name);
      } catch (err) {
        throw new L.SankeyError(`${f.name}: ${err.message}`);
      }
    });
    const stages = parseAll(groups.stages, 'number');
    const colorMats = parseAll(groups.colors, 'color');
    const midMats = parseAll(groups.mids, 'color');
    let loc = null;
    if (groups.loc.length) {
      try {
        loc = CSV.parseLoc(groups.loc[0].text);
      } catch (err) {
        throw new L.SankeyError(`${groups.loc[0].name}: ${err.message}`);
      }
    }

    // Work on a candidate: rejected files must leave the plot, edits and exports untouched.
    const next = Object.assign({}, state, { style: Object.assign({}, state.style) });
    const notices = [];
    if (stages.length) {
      next.matrices = stages.map((m) => m.values);
      next.stageNames = groups.stages.map((f) => f.name);
      next.dimnames = stages.map((m) => ({ rows: m.rowNames, cols: m.colNames }));
      clearKeyedEdits(next);
      next.stageParams = [];
      // New flow matrices are R thickness values, unrelated to an earlier table's scale.
      next.valueScale = 1;
      next.trustedNames = false;
      next.paths = null;
      next.sourceTable = null;
    }
    if (loc) {
      next.loc = loc;
      next.locName = groups.loc[0].name;
      next.nodeColors = {};
      next.mergeColors = {};
    }
    if (opts.mode) {
      next.mode = opts.mode;
    } else if (stages.length || loc) {
      // A single square matrix whose size equals the loc row count can only be a graph.
      const m = next.matrices;
      const squareGraph = m.length === 1 && m[0].length === m[0][0].length && next.loc.length === m[0].length;
      next.mode = squareGraph ? 'graph' : 'layered';
    }
    // Color matrices belong to specific flow matrices; new flows invalidate unpaired ones.
    if (stages.length && !colorMats.length && next.colorMatrices) {
      next.colorMatrices = null;
      next.colorNames = [];
      if (next.style.colorBy === 'file') next.style.colorBy = 'source';
      notices.push('Strand color files were cleared because new flow matrices were loaded.');
    }
    if (stages.length && !midMats.length) {
      next.midMatrices = null;
      next.midNames = [];
    }
    const checkShapes = (mats, names, what) => mats.forEach((m, s) => {
      const flow = next.matrices[s];
      if (!flow || flow.length !== m.values.length || flow[0].length !== m.values[0].length) {
        const shape = flow ? `${flow.length}×${flow[0].length}` : 'missing';
        throw new L.SankeyError(`${names[s]}: ${what} matrix is ${m.values.length}×${m.values[0].length}, ` +
          `but flow matrix ${s + 1} is ${shape}.`);
      }
    });
    if (colorMats.length) {
      checkShapes(colorMats, groups.colors.map((f) => f.name), 'color');
      next.colorMatrices = colorMats.map((m) => m.values);
      next.colorNames = groups.colors.map((f) => f.name);
      next.style.colorBy = 'file';
    }
    if (midMats.length) {
      checkShapes(midMats, groups.mids.map((f) => f.name), 'mid-color');
      next.midMatrices = midMats.map((m) => m.values);
      next.midNames = groups.mids.map((f) => f.name);
      next.style.useMid = true;
    }
    if (stages.length && !loc) {
      const fits = next.loc.length && (() => {
        try { L.computeLayout(model(next), { xlim: [0, 1], ylim: [0, 1], width: 1000, height: 1000 }); return true; } catch (e) { return false; }
      })();
      if (!fits) {
        autoLocate(undefined, next);
        notices.push('Generated node locations because no matching loc file was loaded.');
      }
    }
    computeFrame(next);
    next.view = next.matrices.length && next.loc.length ? fitView(next) : null;
    if (next.view) {
      const lay = L.computeLayout(model(next), viewFor(next.view), { measure: measure() });
      R.resolveColors(lay, colorData(next), next.style);
    }
    state = next;
    if (stages.length || loc) {
      selection = null;
      hover = null;
      lastPointer = null;
      pool = new Map();
      picked = new Set();
    }
    if (notices.length) notify(notices.join(' '));
  }

  async function loadFileList(fileList, role) {
    try {
      if (applyFiles(await readFiles(fileList), role)) update({ fit: true, commit: true });
    } catch (err) {
      computeFrame();
      showError(err.message);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
  }

  function loadExample(id) {
    const ex = window.SK.examples.find((e) => e.id === id);
    if (!ex) throw new L.SankeyError(`Unknown example "${id}".`);
    state = blankState();
    if (ex.table) {
      // Table examples go through the same converter as the "Import table" dialog.
      const table = window.SK.transform.parseTable(ex.files[ex.table]);
      state.style = Object.assign({}, DEFAULT_STYLE, ex.style || {});
      applyImport(window.SK.transform.build(table, ex.importSpec, ex.importOptions),
        { labels: ex.labels, name: ex.table, params: ex.params, example: ex.id });
      return;
    }
    const named = (names) => (names || []).map((name) => ({ name, text: ex.files[name] }));
    applyGroups({
      stages: named(ex.stages),
      loc: named([ex.loc]),
      colors: named(ex.colors),
      mids: named(ex.mids)
    }, { mode: ex.mode });
    Object.assign(state.params, ex.params || {});
    Object.assign(state.style, ex.style || {});
    state.example = ex.id;
    selection = null;
    routeScope = 'all';
    update({ fit: true, commit: true });
  }

  // Node columns for auto-layout: [{ height (raw value), label, id (graph) }] per set.
  function nodeSets(data = state) {
    const mats = data.matrices;
    if (!mats.length) throw new L.SankeyError('Load flow matrices before generating node locations.');
    let sets;
    if (data.mode === 'graph') {
      const m = mats[0];
      const n = m.length;
      if (m.some((r) => r.length !== n)) throw new L.SankeyError('Graph mode needs a square adjacency matrix.');
      const indeg = m[0].map((_, j) => m.filter((r) => r[j] > 0).length);
      const depth = new Array(n).fill(0);
      const queue = indeg.map((d, i) => (d === 0 ? i : -1)).filter((i) => i >= 0);
      let seen = 0;
      while (queue.length) {
        const u = queue.shift();
        seen++;
        m[u].forEach((v, j) => {
          if (v > 0) {
            depth[j] = Math.max(depth[j], depth[u] + 1);
            if (--indeg[j] === 0) queue.push(j);
          }
        });
      }
      if (seen < n) throw new L.SankeyError('The adjacency matrix has a cycle; load a loc file to place its nodes.');
      const names = data.dimnames[0] && data.dimnames[0].rows;
      sets = [];
      for (let i = 0; i < n; i++) {
        const total = Math.max(m[i].reduce((s, v) => s + v, 0), m.reduce((s, r) => s + r[i], 0));
        (sets[depth[i]] = sets[depth[i]] || []).push({ height: total, label: names && usableName(names[i], data) ? names[i] : '', id: i });
      }
      sets = sets.filter(Boolean);
    } else {
      const counts = [mats[0].length, ...mats.map((m) => m[0].length)];
      sets = counts.map((count, s) => Array.from({ length: count }, (_, k) => {
        const incoming = s > 0 ? mats[s - 1].reduce((sum, r) => sum + r[k], 0) : 0;
        const outgoing = s < mats.length ? mats[s][k].reduce((sum, v) => sum + v, 0) : 0;
        const names = s > 0 ? data.dimnames[s - 1] && data.dimnames[s - 1].cols : data.dimnames[0] && data.dimnames[0].rows;
        return { height: Math.max(incoming, outgoing), label: names && usableName(names[k], data) ? names[k] : '' };
      }));
    }
    return sets;
  }

  // Columns are spaced so the tallest column of stacks fills ~60% of the plot height.
  const STACK_FILL = 0.6;
  const X_PAD = 1.15;

  // Value scale giving unit spacing between auto-layout columns (so loc x is 0, 1, 2, ...).
  function autoValueScale(data = state) {
    const sets = nodeSets(data);
    computeFrame(data);
    const W = frame.width - 2 * MARGIN;
    const H = frame.height - frame.margin.top - MARGIN;
    const K = Math.max(sets.length - 1, 1);
    const tallest = Math.max(...sets.map((g) => g.reduce((s, n) => s + n.height, 0)));
    if (!(tallest > 0)) throw new L.SankeyError('The flow matrices contain no positive values.');
    return Number((STACK_FILL * H * K * X_PAD / (W * tallest)).toPrecision(3));
  }

  // Normalized y of each node of one column as auto-layout stacks them: centred, with even gaps,
  // filling most of the height (pxPerUnit turns value x value scale into pixels).
  function stackColumn(group, pxPerUnit, H, valueScale = state.valueScale) {
    const heights = group.map((n) => n.height * valueScale * pxPerUnit);
    const used = heights.reduce((a, b) => a + b, 0);
    const gap = group.length > 1 ? Math.max((0.8 * H - used) / (group.length - 1), 4) : 0;
    let top = H / 2 + (used + gap * (group.length - 1)) / 2;
    return heights.map((h) => {
      const Y = top - h / 2;
      top -= h + gap;
      return round(Y / H);
    });
  }

  // Re-stack one column of an auto-generated layout exactly as auto-layout would have.
  function restackColumn(set) {
    const sets = nodeSets();
    computeFrame();
    const H = frame.height - frame.margin.top - MARGIN;
    const tallest = Math.max(...sets.map((g) => g.reduce((s, n) => s + n.height * state.valueScale, 0)));
    const ys = stackColumn(sets[set - 1], STACK_FILL * H / tallest, H);
    state.loc.forEach((r) => { if (Number(r.set) === set) r.y = ys[Number(r.node) - 1]; });
  }

  // "Align": each column's nodes share one x; the top edges of the top nodes and the bottom edges
  // of the bottom nodes of all columns sit on two common lines (moved apart if the fullest column
  // needs more room), and the nodes in between are evenly spaced in their current order. Node
  // heights follow the x scale while positions follow the y scale, so after refitting the view the
  // columns are aligned once more.
  function alignColumns(opts = {}) {
    if (!layout) update();
    if (!layout) return;
    const minGap = state.style.labels && state.style.labelPosition === 'above'
      ? state.style.fontSize * 1.5 + FILL_BAR.height + FILL_BAR.gap + 4 : 8;
    const once = () => {
      const height = (n) => Math.max(n.inPx, n.outPx, n.bodyHeightPx || 0);
      const columns = new Map();
      layout.nodes.forEach((n) => columns.set(n.set, (columns.get(n.set) || []).concat(n)));
      let top = Math.max(...layout.nodes.map((n) => n.Y + height(n) / 2));
      let bottom = Math.min(...layout.nodes.map((n) => n.Y - height(n) / 2));
      const need = Math.max(...[...columns.values()].map((g) => g.reduce((t, n) => t + height(n), 0) + minGap * (g.length - 1)));
      if (need > top - bottom) {
        const mid = (top + bottom) / 2;
        top = mid + need / 2;
        bottom = mid - need / 2;
      }
      columns.forEach((g) => {
        g.sort((a, b) => b.Y - a.Y);
        const x = round(g.reduce((t, n) => t + Number(state.loc[n.id].x), 0) / g.length);
        const used = g.reduce((t, n) => t + height(n), 0);
        const gap = g.length > 1 ? (top - bottom - used) / (g.length - 1) : 0;
        let y = g.length > 1 ? top : (top + bottom + used) / 2;
        g.forEach((n) => {
          state.loc[n.id].x = x;
          state.loc[n.id].y = round(layout.tf.y(y - height(n) / 2));
          y -= height(n) + gap;
        });
      });
    };
    once();
    update({ fit: true, keepCanvas: true });
    once();
    update({ commit: opts.commit !== false });
  }

  // Default loc: node sets in evenly spaced columns, stacked to fill most of the height.
  // labels: "above" or "inside" (label-derived node widths; see style.labelPosition), "beside"
  // (zero-width nodes) or "none".
  function autoLocate(options, data = state) {
    const labels = (options && options.labels) || 'inside';
    const sets = nodeSets(data);
    computeFrame(data);
    const W = frame.width - 2 * MARGIN;
    const H = frame.height - frame.margin.top - MARGIN;
    const K = Math.max(sets.length - 1, 1);
    const tallest = Math.max(...sets.map((g) => g.reduce((s, n) => s + n.height * data.valueScale, 0)));
    const spacing = tallest * W / (STACK_FILL * H * K * X_PAD);
    const pxPerUnit = W / (K * spacing * X_PAD);
    const rowsByGraphId = [];
    const loc = [];
    sets.forEach((group, s) => {
      const ys = stackColumn(group, pxPerUnit, H, data.valueScale);
      group.forEach((n, k) => {
        const row = { set: s + 1, node: k + 1, x: round(s * spacing), y: ys[k] };
        if (n.label && labels !== 'none') row.label = n.label;
        if (n.label && labels === 'beside') row.width = 0;
        if (n.id !== undefined) rowsByGraphId[n.id] = row;
        else loc.push(row);
      });
    });
    data.loc = data.mode === 'graph' ? rowsByGraphId : loc;
    data.locName = 'auto-generated';
    data.view = null;
  }

  // Replace the data with flows built from a table (see transform.js), keeping the style.
  function applyImport(result, options) {
    const opts = options || {};
    const style = Object.assign({}, state.style);
    if (style.colorBy === 'file') style.colorBy = 'source';
    if (opts.labels && !opts.rebuild) {
      style.labelPosition = opts.labels === 'above' ? 'above' : 'auto';
      style.labelColorBy = opts.labels === 'above' ? 'node' : 'single';
    }
    const next = blankState();
    next.style = style;
    next.presentation = Object.assign({}, state.presentation);
    next.mode = result.mode;
    next.matrices = result.matrices;
    next.dimnames = result.dimnames;
    next.stageNames = result.stageNames;
    next.paths = result.paths || null;
    next.pathUnit = result.unit || null;
    next.setNames = result.setNames || [];
    next.trustedNames = true;
    next.sourceTable = opts.sourceTable || (result.source ? window.SK.tableControls.create(result.source,
      { name: opts.name || 'pasted table', labels: opts.labels || 'inside' }) : null);
    if (next.sourceTable) next.sourceTable = Object.assign({}, next.sourceTable, { minCanvasWidth: 0, spreadWidth: 0 });
    if (next.sourceTable && !next.sourceTable.summary) next.sourceTable.summary = Object.assign({ rowsFiltered: next.sourceTable.table.rows.length, unit: result.unit }, result.summary);
    Object.assign(next.params, opts.params || {});
    const previousFrame = frame;
    try {
      next.valueScale = autoValueScale(next);
      autoLocate({ labels: opts.labels }, next);
      next.locName = opts.name ? `built from ${opts.name}` : 'built from table';
      if (opts.example) next.example = opts.example;
      next.view = fitView(next, { spread: true });
      const candidate = computeLayout(next, next.view);
      R.resolveColors(candidate, colorData(next), next.style);
    } catch (err) {
      frame = previousFrame;
      throw err;
    }
    state = next;
    selection = null;
    hover = null;
    pool = new Map();
    picked = new Set();
    lastPointer = null;
    routeScope = 'all';
    update({ commit: true });
  }

  function rebuildTable(source) {
    try {
      const built = window.SK.tableControls.build(source);
      applyImport(built.result, { sourceTable: built.source, labels: source.labels, name: source.name,
        params: state.params, rebuild: true });
    } catch (err) {
      computeFrame();
      window.SK.tableControls.render($('#table-controls'), state.sourceTable, rebuildTable, true);
      showError(err.message);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
  }

  // ---------- interaction ----------

  function findSelected() {
    if (!selection || !layout) return null;
    if (selection.kind === 'flow') return layout.flows.find((f) => f.key === selection.key) || null;
    return layout.nodes.find((n) => n.key === selection.key) || null;
  }

  function nodeDisplayName(n) {
    // The label as entered (drawn labels may carry a total).
    if (n.hasLabel) return (state.loc[n.id] && state.loc[n.id].label) || n.label;
    const dn = state.dimnames;
    let name = null;
    if (state.mode === 'graph') {
      name = dn[0] && dn[0].rows && dn[0].rows[n.id];
    } else if (n.set === 1) {
      name = dn[0] && dn[0].rows && dn[0].rows[n.node - 1];
    } else {
      name = dn[n.set - 2] && dn[n.set - 2].cols && dn[n.set - 2].cols[n.node - 1];
    }
    return usableName(name) ? name : `Set ${n.set} · node ${n.node}`;
  }

  function targetOf(event) {
    const el = event.target && event.target.closest && event.target.closest('[data-flow], [data-node]');
    if (!el || !layout) return null;
    if (el.dataset.flow !== undefined) return { kind: 'flow', index: Number(el.dataset.flow) };
    return { kind: 'node', index: Number(el.dataset.node) };
  }

  // Pathways to trace for a hover target: every pathway through the strand or node; with Alt or Ctrl/⌘
  // the single pathway under the pointer; with Shift the single item (when items have ids). Null
  // when tracing does not apply.
  function tracedPaths(target, event) {
    if (!traces || !target || !event) return null;
    if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
      const p = plotPoint(event);
      const hit = target.kind === 'flow'
        ? P.pathAt(layout, traces.subs, target.index, p.X, p.Y)
        : P.pathAtNode(layout, traces.subs, layout.nodes[target.index], p.X, p.Y);
      return hit ? { paths: [hit.path], member: event.shiftKey ? hit.member : -1, single: true } : null;
    }
    const n = target.kind === 'node' ? layout.nodes[target.index] : null;
    const list = n
      ? traces.paths.map((path, pi) => (path.nodes[n.set - 1] === n.node - 1 ? pi : -1)).filter((pi) => pi >= 0)
      : traces.subs.byFlow[target.index].map((e) => e.path);
    return list.length ? { paths: list, member: -1, single: false } : null;
  }

  const nodeAtStage = (s, i) => layout.nodes.find((n) => n.set === s + 1 && n.node === i + 1);
  // What a traced target stands for: [path, member] units, member -1 for a whole pathway.
  const unitsOf = (t) => t.units || t.paths.map((pi) => [pi, t.single && t.member >= 0 ? t.member : -1]);
  const unitWeight = ([pi, mi]) => (mi >= 0 ? traces.paths[pi].members[mi].weight : traces.paths[pi].weight);
  const pathName = (pi) => traces.paths[pi].nodes.map((ni, s) => nodeDisplayName(nodeAtStage(s, ni))).join(' → ');

  // Overlay shapes for traced pathways, colored like the strands and nodes they pass through
  // (flattened, so a trace reads as its own strand at full strength). Outlines run along the
  // sides only and fade where the fill fades, so traces open smoothly into node bodies. A single
  // traced item too thin to see is widened around its slice instead of outlined.
  const MIN_SINGLE_PX = 4;

  // Overlapping traces get darker with every extra trace: each adds a faint black silhouette, so n
  // traces cover a point to 1 - (1 - silhouette)^n, and a filter maps that coverage to a shade that
  // is clear for one trace and grows by `gain` per unit of coverage beyond it, up to `max`.
  const OVERLAP = { silhouette: 0.12, gain: 0.8, max: 0.45 };
  function overlapFilter() {
    const { silhouette, gain, max } = OVERLAP;
    const table = Array.from({ length: 41 }, (_, i) => Math.min(max, Math.max(0, gain * (i / 40 - silhouette - 0.005))).toFixed(4)).join(' ');
    return `<filter id="trace-overlap" filterUnits="userSpaceOnUse" x="0" y="0" width="${frame.width}" height="${frame.height}">` +
      '<feComponentTransfer><feFuncR type="linear" slope="0"/><feFuncG type="linear" slope="0"/><feFuncB type="linear" slope="0"/>' +
      `<feFuncA type="table" tableValues="${table}"/></feComponentTransfer></filter>`;
  }
  function traceMarkup(target) {
    const h = layout.view.height;
    const sx = (X) => Math.round((MARGIN + X) * 100) / 100;
    const sy = (Y) => Math.round((frame.margin.top + h - Y) * 100) / 100;
    const line = (pts) => pts.map((p, i) => `${i ? 'L' : 'M'}${sx(p[0])} ${sy(p[1])}`).join('');
    const stopTag = (offset, color, opacity) => `<stop offset="${(offset * 100).toFixed(2)}%" stop-color="${color}" stop-opacity="${opacity}"/>`;
    const units = unitsOf(target);
    const slots = !target.single && state.style.gatherInNodes && units.length > 1 && units.every(([, mi]) => mi < 0)
      ? P.gatherSlots(layout, traces.subs, units.map(([pi]) => pi)) : null;
    const tiled = state.style.tileInNodes ? P.tiledNodes(layout, traces.subs, units, { slots }) : null;
    // Traces are as see-through as the strands (the Opacity setting), so where substrands overlap
    // both show. Each trace is one group, so its fill, outline and widening composite once.
    const alpha = Math.min(Math.max(Number(state.style.opacity) || 1, 0.05), 1);
    let defs = '';
    let body = '';
    const shades = [];
    const annotations = [];
    units.forEach(([pi, mi], n) => {
      const t = P.tracePolygon(layout, traces.subs, pi, { member: mi, slots, tiled, fadeLabels: state.style.labelPosition !== 'above' });
      annotations.push({ path: pi, member: mi, weight: unitWeight([pi, mi]), geometry: t });
      const xs = t.polygon.map((p) => p[0]);
      const x0 = Math.min(...xs);
      const x1 = Math.max(...xs);
      const at = (x) => (x1 > x0 ? Math.min(Math.max((x - x0) / (x1 - x0), 0), 1) : 0);
      const stops = R.traceStops(layout, colors, state.style, t.flows).sort((a, b) => a[0] - b[0]);
      // Colors relative to the group's opacity: full strength where the strands are, clear where
      // node bodies fade out.
      const fill = stops.map(([x, c]) => stopTag(at(x), C.paint(C.opaque(c)).color, Math.min(1, c.a / alpha).toFixed(3))).join('');
      const edge = stops.map(([x, c]) => stopTag(at(x), '#111827', Math.min(1, (0.45 * Math.min(1, 2 * c.a)) / alpha).toFixed(3))).join('');
      const grad = (id, tags) => `<linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${sx(x0)}" y1="0" x2="${sx(x1)}" y2="0">${tags}</linearGradient>`;
      defs += grad(`trace${n}`, fill) + grad(`trace-edge${n}`, edge);
      if ((target.single || mi >= 0) && t.thickness < MIN_SINGLE_PX) {
        const widen = (MIN_SINGLE_PX - t.thickness).toFixed(2);
        body += `<g class="trace-group" opacity="${alpha}"><path class="trace" data-path="${pi}" d="${line(t.polygon)}Z" fill="url(#trace${n})" stroke="url(#trace${n})" ` +
          `stroke-width="${widen}" stroke-linejoin="round"/></g>`;
        shades.push(`<path d="${line(t.polygon)}Z" fill="#000" stroke="#000" stroke-width="${widen}" stroke-linejoin="round"/>`);
        return;
      }
      shades.push(`<path d="${line(t.polygon)}Z" fill="#000"/>`);
      body += `<g class="trace-group" opacity="${alpha}"><path class="trace" data-path="${pi}" d="${line(t.polygon)}Z" fill="url(#trace${n})"/>` +
        `<path class="trace-edge" d="${line(t.upper)}${line(t.lower)}" fill="none" stroke="url(#trace-edge${n})" stroke-width="0.6"/></g>`;
    });
    if (shades.length > 1) {
      defs += overlapFilter();
      body += `<g class="trace-overlap" filter="url(#trace-overlap)">${shades.map((d) => `<g opacity="${OVERLAP.silhouette}">${d}</g>`).join('')}</g>`;
    }
    return { paths: `<defs>${defs}</defs>${body}`, labels: traceLabelMarkup(annotations) };
  }

  function traceLabelMarkup(annotations) {
    const { font, minFont, countGap, countSpacing } = TRACE_LABEL;
    const f2 = (v) => Math.round(v * 100) / 100;
    const sx = (X) => MARGIN + X;
    const sy = (Y) => frame.margin.top + layout.view.height - Y;
    const fontAttrs = `font-family="${R.FONT_FAMILY}" dominant-baseline="central"`;
    const counts = new Map();
    const occupied = [];
    const textMeasure = R.makeMeasure({ fontSize: font });
    const nodeLabels = nodeLabelBounds(layout, state);
    let out = '';
    let leaders = '';
    annotations.forEach(({ path, member, weight, geometry }) => {
      const attrs = `data-path="${path}" data-member="${member}"`;
      if (state.style.tracePercents) geometry.labelAnchors.forEach((a) => {
        const text = pct(weight, layout.flows[a.flow].value);
        // Prefer the midpoint, moving along the slice only to reduce collisions at crossings.
        const candidates = [a, ...a.alternatives].map((p) => {
          const size = Math.max(minFont, Math.min(font, p.thickness * 0.85));
          const w = textMeasure(text, size / font, 1).width + 3;
          const h = size + 3;
          const cos = Math.abs(Math.cos(p.angle));
          const sin = Math.abs(Math.sin(p.angle));
          return { p, size, x: sx(p.X), y: sy(p.Y), w: w * cos + h * sin, h: w * sin + h * cos };
        });
        const overlapArea = (a, b) => Math.max(0, Math.min(a.x + a.w / 2, b.x + b.w / 2) - Math.max(a.x - a.w / 2, b.x - b.w / 2)) *
          Math.max(0, Math.min(a.y + a.h / 2, b.y + b.h / 2) - Math.max(a.y - a.h / 2, b.y - b.h / 2));
        candidates.forEach((c) => { c.overlap = occupied.reduce((t, box) => t + overlapArea(c, box), 0); });
        const placed = candidates.reduce((best, c) => (c.overlap < best.overlap ? c : best));
        occupied.push(placed);
        let angle = -placed.p.angle * 180 / Math.PI;
        if (angle > 90) angle -= 180;
        if (angle < -90) angle += 180;
        out += `<text class="trace-percent" ${attrs} data-label-flow="${a.flow}" ` +
          `transform="translate(${f2(placed.x)} ${f2(placed.y)}) rotate(${f2(angle)})" text-anchor="middle" ` +
          `${fontAttrs} font-size="${f2(placed.size)}" fill="#FFFFFF" stroke="#1F2937" stroke-opacity="0.65" stroke-width="0.8" paint-order="stroke">${text}</text>`;
      });
      if (state.style.traceCounts) {
        const a = geometry.countAnchor;
        if (!counts.has(a.node)) counts.set(a.node, []);
        counts.get(a.node).push({ attrs, weight, anchorX: sx(a.X), y: sy(a.Y), anchorY: sy(a.Y) });
      }
    });
    counts.forEach((labels, node) => {
      labels.sort((a, b) => a.y - b.y);
      labels.forEach((a, i) => { if (i) a.y = Math.max(a.y, labels[i - 1].y + countSpacing); });
      // Keep crowded endpoint counts readable, centered on their substrands and inside the canvas.
      const first = labels[0];
      const last = labels[labels.length - 1];
      const centerShift = (first.y + last.y - first.anchorY - last.anchorY) / 2;
      const shift = Math.max(last.y - (frame.height - MARGIN - font / 2), Math.min(centerShift, first.y - MARGIN - font / 2));
      labels.forEach(a => { a.y -= shift; });
      const displacement = Math.max(...labels.map(a => Math.abs(a.y - a.anchorY)));
      const right = Math.max(...labels.map(a => a.anchorX), sx(labelRightEdge(nodeLabels,
        frame.margin.top + layout.view.height - last.y - font / 2,
        frame.margin.top + layout.view.height - first.y + font / 2)));
      const x = right + countGapFor(displacement);
      labels.forEach((a) => {
        const y = a.y;
        if (Math.abs(y - a.anchorY) > 1 || x - a.anchorX > countGap + 1) leaders += `<path class="trace-count-leader" ` +
          `d="M${f2(a.anchorX)} ${f2(a.anchorY)}L${f2(Math.min(right + countGap, x - 3))} ${f2(a.anchorY)}L${f2(x - 3)} ${f2(y)}" ` +
          'fill="none" stroke="#64748B" stroke-width="0.6"/>';
        out += `<text class="trace-count" ${a.attrs} data-label-node="${node}" x="${f2(x)}" y="${f2(y)}" ` +
          `text-anchor="start" ${fontAttrs} font-size="${font}" fill="#1F2937">${fmtValue(a.weight)}</text>`;
      });
    });
    if (leaders) {
      const holes = nodeLabels.map(b => `<rect x="${f2(sx(b.x0) - 3)}" y="${f2(sy(b.y1) - 3)}" ` +
        `width="${f2(b.x1 - b.x0 + 6)}" height="${f2(b.y1 - b.y0 + 6)}" fill="black"/>`).join('');
      out = `<defs><mask id="trace-count-label-mask" maskUnits="userSpaceOnUse" x="0" y="0" width="${frame.width}" height="${frame.height}">` +
        `<rect width="${frame.width}" height="${frame.height}" fill="white"/>${holes}</mask></defs>` +
        `<g mask="url(#trace-count-label-mask)">${leaders}</g>` + out;
    }
    return out;
  }

  // Elements to keep in focus. Traced pathways replace their strands with overlays, so only the
  // labels of the nodes they pass through stay at full strength.
  function highlightSets(target) {
    const flows = new Set();
    const nodes = new Set();
    if (!target || !layout) return { flows, nodes, labels: nodes };
    if (target.paths) {
      const labels = new Set();
      unitsOf(target).forEach(([pi]) => traces.paths[pi].nodes.forEach((ni, s) => labels.add(nodeAtStage(s, ni).id)));
      return { flows, nodes, labels };
    }
    if (target.kind === 'nodes') {
      target.nodes.forEach((id) => {
        nodes.add(id);
        layout.nodes[id].inFlows.concat(layout.nodes[id].outFlows).forEach((k) => flows.add(k));
      });
    } else if (target.kind === 'flow') {
      const f = layout.flows[target.index];
      flows.add(f.index);
      nodes.add(f.from);
      nodes.add(f.to);
    } else {
      const n = layout.nodes[target.index];
      nodes.add(n.id);
      n.inFlows.concat(n.outFlows).forEach((k) => flows.add(k));
    }
    return { flows, nodes, labels: nodes };
  }

  function selectionTarget() {
    const sel = findSelected();
    if (!sel) return null;
    return selection.kind === 'flow' ? { kind: 'flow', index: sel.index } : { kind: 'node', index: sel.id };
  }

  // The selection as a highlight target, with the pathways that were traced when it was clicked.
  function pinnedTarget() {
    const t = selectionTarget();
    if (!t) return null;
    return Object.assign(t, traces && selection.paths
      ? { paths: selection.paths, member: selection.member, single: selection.single }
      : { paths: null, member: -1, single: false });
  }

  // Weight of the hovered selection passing through each node id: the traced pathways (or item),
  // else the hovered strand, or the hovered node and its strands.
  function focusWeights(target) {
    const w = new Map();
    const add = (id, v) => w.set(id, (w.get(id) || 0) + v);
    if (target.paths && traces) {
      unitsOf(target).forEach((u) => traces.paths[u[0]].nodes.forEach((ni, s) => add(nodeAtStage(s, ni).id, unitWeight(u))));
    } else if (target.kind === 'flow') {
      const f = layout.flows[target.index];
      add(f.from, f.value);
      add(f.to, f.value);
    } else {
      // A node fills itself, and its neighbours by the strands between them (nodes of one group
      // fill themselves only once).
      const ids = target.kind === 'nodes' ? target.nodes : [target.index];
      ids.forEach((id) => {
        const n = layout.nodes[id];
        w.set(n.id, Math.max(n.inValue, n.outValue));
        n.inFlows.forEach((k) => { if (!ids.includes(layout.flows[k].from)) add(layout.flows[k].from, layout.flows[k].value); });
        n.outFlows.forEach((k) => { if (!ids.includes(layout.flows[k].to)) add(layout.flows[k].to, layout.flows[k].value); });
      });
    }
    return w;
  }

  // Nodes and strands made entirely of traced substrands: their faded copies are hidden, since the
  // traces stand for all of them and the copies would only peek out between crossing traces.
  function coverage(target) {
    const nodes = new Set();
    const flows = new Set();
    if (!(target && target.paths && traces)) return { nodes, flows };
    focusWeights(target).forEach((v, id) => {
      const n = layout.nodes[id];
      if (v >= Math.max(n.inValue, n.outValue) * (1 - 1e-9)) nodes.add(id);
    });
    const byFlow = new Map();
    unitsOf(target).forEach((u) => traces.subs.pathFlows[u[0]].forEach((k) => byFlow.set(k, (byFlow.get(k) || 0) + unitWeight(u))));
    byFlow.forEach((v, k) => { if (v >= layout.flows[k].value * (1 - 1e-9)) flows.add(k); });
    return { nodes, flows };
  }

  // ---------- pool (Alt-click) ----------

  // Substrands gathered with Alt-click across strands and nodes: per path index, null for the whole
  // pathway or a Set of its items (member indices). It belongs to the pathway data it was built on.
  let pool = new Map();
  let poolPaths = null;

  function poolTarget() {
    if (!traces || !pool.size || poolPaths !== state.paths) return null;
    const units = [];
    pool.forEach((members, pi) => {
      if (members === null) units.push([pi, -1]);
      else members.forEach((mi) => units.push([pi, mi]));
    });
    return { kind: 'pool', index: -1, paths: [...pool.keys()], member: -1, single: false, units };
  }

  // Add a traced target's substrands to the pool, or remove them if all are in it already.
  function togglePool(target) {
    if (poolPaths !== state.paths) {
      pool = new Map();
      poolPaths = state.paths;
    }
    const has = (pi, mi) => pool.has(pi) && (pool.get(pi) === null || (mi >= 0 && pool.get(pi).has(mi)));
    const units = unitsOf(target);
    const remove = units.every(([pi, mi]) => has(pi, mi));
    units.forEach(([pi, mi]) => {
      if (mi < 0) {
        if (remove) pool.delete(pi);
        else pool.set(pi, null);
        return;
      }
      // Items of a pathway are kept as a set; all of them together count as the whole pathway.
      const all = traces.paths[pi].members.map((_, k) => k);
      const set = pool.get(pi) === null ? new Set(all) : (pool.get(pi) || new Set());
      if (remove) set.delete(mi);
      else set.add(mi);
      if (!set.size) pool.delete(pi);
      else pool.set(pi, set.size === all.length ? null : set);
    });
  }

  // Nodes picked with a plain Alt-click (on any data), e.g. to merge them. Keyed by node key and
  // tied to the flow matrices they were picked on.
  let picked = new Set();
  let pickedFor = null;

  function togglePicked(key) {
    if (pickedFor !== state.matrices) {
      picked = new Set();
      pickedFor = state.matrices;
    }
    if (picked.has(key)) picked.delete(key);
    else picked.add(key);
  }

  const pickedNodes = () => (layout && pickedFor === state.matrices ? layout.nodes.filter((n) => picked.has(n.key)) : []);

  // What Alt-clicks gathered: the pool of substrands, else (without pathway data) the picked nodes.
  function groupTarget() {
    const target = poolTarget();
    if (target) return target;
    const nodes = pickedNodes().map((n) => n.id);
    return nodes.length ? { kind: 'nodes', index: -1, paths: null, member: -1, single: false, nodes } : null;
  }

  function clearPool() {
    pool = new Map();
    picked = new Set();
    applyHighlight();
    refreshInspector();
  }

  // Merging needs two or more picked nodes in one column of a layered diagram.
  function mergeCandidates() {
    const nodes = pickedNodes();
    const ok = state.mode === 'layered' && nodes.length >= 2 && nodes.every((n) => n.set === nodes[0].set);
    return ok ? nodes : null;
  }

  // The Merge button shows only while a merge is possible.
  function refreshMerge() {
    const button = $('#merge-nodes');
    const nodes = mergeCandidates();
    button.hidden = !nodes;
    if (nodes) {
      button.textContent = `Merge ${nodes.length} nodes`;
      button.title = `Merge ${nodes.map(nodeDisplayName).join(', ')} into one node (Ctrl/⌘+Z undoes)`;
    }
  }

  // Ask for the merged node's name, pre-filled with the members' names joined by " + ".
  function askMergeName() {
    const nodes = mergeCandidates();
    if (!nodes) return;
    $('#merge-members').textContent = `Merging ${nodes.map(nodeDisplayName).join(', ')}.`;
    $('#merge-name').value = nodes.map(nodeDisplayName).join(' + ');
    $('#merge-dialog').showModal();
    $('#merge-name').select();
  }

  // Merge the picked nodes as if the data had them as one node from the start: an auto-generated
  // layout re-stacks their column as auto-layout would; a loaded loc keeps the merged node at the
  // members' weighted mean height.
  function mergePicked(name) {
    const nodes = mergeCandidates();
    if (!nodes) return;
    const label = name.trim();
    if (!label) throw new L.SankeyError('The merged node needs a name.');
    const autoLoc = state.locName === 'auto-generated' || /^built from /.test(state.locName);
    const set = nodes[0].set;
    state = window.SK.edit.mergeNodes(state, set, nodes.map((n) => n.node - 1), label);
    if (autoLoc) {
      // Columns go back to auto-layout's spacing, to be spread for the merged data as on import.
      if (state.sourceTable) Object.assign(state.sourceTable, { minCanvasWidth: 0, spreadWidth: 0 });
      const fresh = Object.assign({}, state);
      autoLocate(undefined, fresh);
      state.loc.forEach((r) => { r.x = fresh.loc.find((f) => f.set === Number(r.set)).x; });
      restackColumn(set);
    }
    picked = new Set();
    pool = new Map();
    selection = null;
    hover = null;
    update({ fit: true, commit: true, spread: autoLoc });
    notify(`Merged into "${label}" (Ctrl/⌘+Z undoes).`);
  }

  // Under every node, a small bar: the share of the node the selection fills in the node's color
  // (none for nodes it misses), the rest in a faded node color, with both percentages beside it.
  const FILL_BAR = { width: 60, minWidth: 24, height: 8, gap: 7, font: 10.5, labelWidth: 36, labelGap: 4 };
  function fillBarBox(lay, n, labels) {
    const labelSpace = 2 * (FILL_BAR.labelWidth + FILL_BAR.labelGap);
    const width = n.widthPx > 0 ? Math.min(FILL_BAR.width,
      Math.max(Math.min(FILL_BAR.minWidth, n.widthPx / 3), n.widthPx - labelSpace)) : FILL_BAR.width;
    const scale = n.widthPx > 0 ? Math.min(1, (n.widthPx - width) / labelSpace) : 1;
    const font = FILL_BAR.font * scale, labelWidth = FILL_BAR.labelWidth * scale, labelGap = FILL_BAR.labelGap * scale;
    const role = S.nodeRole(n);
    const shape = role === 'interior' ? S.interiorTrunk(lay, n) : S.endpointBody(lay, n, role === 'source' ? 'start' : 'end', 0);
    const lower = shape && (shape.lower || shape.lowerBorder);
    let bottom = lower ? L.pointAtX(lower, n.X)[1] : n.Y - Math.max(n.inPx, n.outPx, n.bodyHeightPx || 0) / 2;
    const textMeasure = R.makeMeasure({ fontSize: font });
    const widths = labels ? labels.map(text => textMeasure(text, 1, 1).width) : [labelWidth, labelWidth];
    const x0 = n.X - width / 2 - labelGap - widths[0], x1 = n.X + width / 2 + labelGap + widths[1];
    // Only clear the drawn body beneath this widget, rather than remote ribbon ends.
    if (lower) S.sliceByX(lower, x0, x1).forEach(p => { bottom = Math.min(bottom, p[1]); });
    else n.inFlows.concat(n.outFlows).forEach(k => {
      const f = lay.flows[k];
      if (Math.max(f.startX, f.endX) < x0 || Math.min(f.startX, f.endX) > x1) return;
      S.sliceByX(f.lower, x0, x1).forEach(p => { bottom = Math.min(bottom, p[1]); });
    });
    return { width, font, labelWidth, labelGap, x0: n.X - width / 2, Y: bottom - FILL_BAR.gap };
  }
  function fillMarkup(target) {
    const h = layout.view.height;
    const f2 = (v) => Math.round(v * 100) / 100;
    let out = '';
    const weights = focusWeights(target);
    layout.nodes.forEach((n) => {
      const id = n.id;
      const v = weights.get(id) || 0;
      const total = Math.max(n.inValue, n.outValue);
      if (!(total > 0)) return;
      const t = Math.min(v / total, 1);
      // Both shares get one decimal when the smaller one is under 10%, so they add up to 100%.
      const digits = t > 0 && t < 1 && Math.min(t, 1 - t) < 0.0995 ? 1 : 0;
      const share = (x) => `${(100 * x).toFixed(digits)}%`;
      const { height } = FILL_BAR;
      const bar = fillBarBox(layout, n, [share(t), share(1 - t)]);
      const { font, labelGap } = bar;
      const width = f2(bar.width);
      const x0 = MARGIN + bar.x0;
      const y0 = frame.margin.top + h - bar.Y;
      const node = C.opaque(colors.node[id]);
      const solid = C.paint(node).color;
      const faded = C.paint(C.flatten(C.withAlpha(node, 0.22), colors.flatBg)).color;
      const text = C.paint(R.labelColor(n, colors, state.style)).color;
      const label = (x, anchor, value) => `<text x="${f2(x)}" y="${f2(y0 + height / 2)}" text-anchor="${anchor}" dominant-baseline="central" ` +
        `font-family="${R.FONT_FAMILY}" font-size="${font}" fill="${text}">${value}</text>`;
      // Both parts are clipped to one rounded bar, so they meet in a straight edge.
      out += `<g class="node-fill" data-fill="${id}"><clipPath id="fill-clip${id}">` +
        `<rect x="${f2(x0)}" y="${f2(y0)}" width="${width}" height="${height}" rx="2.5"/></clipPath>` +
        `<g clip-path="url(#fill-clip${id})"><rect x="${f2(x0)}" y="${f2(y0)}" width="${width}" height="${height}" fill="${faded}"/>` +
        (t > 0 ? `<rect class="node-fill-solid" x="${f2(x0)}" y="${f2(y0)}" width="${f2(width * t)}" height="${height}" fill="${solid}"/>` : '') + '</g>' +
        label(x0 - labelGap, 'end', share(t)) + label(x0 + width + labelGap, 'start', share(1 - t)) + '</g>';
    });
    return out;
  }

  function applyHighlight() {
    const active = (drag && drag.target) || hover || groupTarget() || pinnedTarget();
    const sets = highlightSets(active);
    const covered = coverage(active);
    svg.classList.toggle('dimmed', !!active);
    const traced = active && active.paths && traces ? traceMarkup(active) : null;
    const traceLayer = svg.querySelector('.layer-traces');
    if (traceLayer) traceLayer.innerHTML = traced ? traced.paths : '';
    const traceLabels = svg.querySelector('.layer-trace-labels');
    if (traceLabels) traceLabels.innerHTML = traced && !drag ? traced.labels : '';
    const fillLayer = svg.querySelector('.layer-fills');
    if (fillLayer) fillLayer.innerHTML = active && !drag ? fillMarkup(active) : '';
    svg.querySelectorAll('[data-flow]').forEach((el) => {
      el.classList.toggle('hl', sets.flows.has(Number(el.dataset.flow)));
      el.classList.toggle('covered', covered.flows.has(Number(el.dataset.flow)));
    });
    svg.querySelectorAll('[data-node]').forEach((el) => {
      const label = el.classList.contains('node-label');
      el.classList.toggle('hl', (label ? sets.labels : sets.nodes).has(Number(el.dataset.node)));
      el.classList.toggle('covered', !label && covered.nodes.has(Number(el.dataset.node)));
    });
    const sel = selectionTarget();
    svg.querySelectorAll('.selected').forEach((el) => el.classList.remove('selected'));
    if (sel) {
      const attr = sel.kind === 'flow' ? 'data-flow' : 'data-node';
      svg.querySelectorAll(`.ribbon[${attr}="${sel.index}"], .trunk[${attr}="${sel.index}"], .node-dot[${attr}="${sel.index}"]`)
        .forEach((el) => el.classList.add('selected'));
    }
    refreshMerge();
  }

  const pct = (v, total) => (total > 0 ? `${(100 * v / total).toFixed(1)}%` : '–');
  const setName = (s) => state.setNames[s] || `Set ${s + 1}`;
  const unitOf = (w) => esc(w === 1 ? (state.pathUnit || 'items').replace(/s$/, '') : (state.pathUnit || 'items'));

  // Hover details: the strand or node, then the traced pathways, if any.
  function hoverHtml(target) {
    let html;
    const nodes = pickedNodes();
    const pickedLine = nodes.length
      ? `<p>Picked nodes: <b>${nodes.map((n) => esc(nodeDisplayName(n))).join(', ')}</b>.</p>`
      : '';
    if (target.kind === 'pool') {
      return '<h2>Pool</h2><p class="muted small">Alt-click strands or nodes to add their substrands, or remove them when ' +
        'they are all in already (with Ctrl/⌘ or Shift: one pathway or one item).</p>' +
        pickedLine + focusHtml(target) + '<button class="mini-btn wide" id="pool-clear">Clear pool (Esc)</button>';
    }
    if (target.kind === 'nodes') {
      return '<h2>Picked nodes</h2><p class="muted small">Alt-click nodes to pick or unpick them.</p>' + pickedLine +
        '<button class="mini-btn wide" id="pool-clear">Clear (Esc)</button>';
    }
    if (target.kind === 'flow') {
      const f = layout.flows[target.index];
      const a = layout.nodes[f.from];
      const b = layout.nodes[f.to];
      html = `<h2>${esc(nodeDisplayName(a))} → ${esc(nodeDisplayName(b))}</h2>` +
        `<div>value <b>${fmtValue(f.value)}</b></div>` +
        `<div>${pct(f.value, a.outValue)} of source outflow · ${pct(f.value, b.inValue)} of target inflow</div>` +
        `<div class="muted">${state.mode === 'graph' ? 'graph' : `stage ${f.stage + 1}`} · matrix [${f.i + 1}, ${f.j + 1}]</div>`;
    } else {
      const n = layout.nodes[target.index];
      const balance = n.inValue > 0 && n.outValue > 0 && Math.abs(n.inValue - n.outValue) > 1e-9
        ? `<div class="warn-text">in − out = ${fmtValue(n.inValue - n.outValue)}</div>` : '';
      html = `<h2>${esc(nodeDisplayName(n))}</h2>` +
        `<div>in <b>${fmtValue(n.inValue)}</b> (${n.inFlows.length}) · out <b>${fmtValue(n.outValue)}</b> (${n.outFlows.length})</div>` +
        balance + `<div class="muted">set ${n.set} · node ${n.node} · x ${round(n.x, 4)}, y ${round(n.y, 4)}</div>`;
    }
    if (!target.paths) return html;
    return html + (target.single ? itemHtml(target) : focusHtml(target));
  }

  // One traced pathway, or one item on it.
  function itemHtml(target) {
    const path = traces.paths[target.paths[0]];
    const item = target.member >= 0 && path.members ? path.members[target.member] : null;
    const rows = path.nodes.map((ni, s) => `<tr><td>${esc(setName(s))}</td><td>${esc(nodeDisplayName(nodeAtStage(s, ni)))}</td></tr>`).join('');
    const count = `<b>${fmtValue(path.weight)}</b> ${unitOf(path.weight)} on this pathway (${pct(path.weight, traces.total)} of all)` +
      (item && item.weight !== 1 ? ` · this one weighs <b>${fmtValue(item.weight)}</b>` : '');
    const noIds = target.member >= 0 || path.members ? '' : ' (no item ids in this table, so Shift traces pathways too)';
    return `<h3 class="focus-head">${item ? esc(item.id) : 'One pathway'}</h3><table class="focus-table">${rows}</table>` +
      `<p>${count}</p><p class="muted small">Without Ctrl/⌘ or Shift, every pathway through this ${target.kind === 'flow' ? 'strand' : 'node'} is traced${noIds}.</p>`;
  }

  // Where the traced pathways run: their weight through each node and strand, as counts and as
  // shares of the traced total, plus the largest pathways.
  function focusHtml(target) {
    const TOP = 8;
    const byPath = new Map();
    unitsOf(target).forEach((u) => byPath.set(u[0], (byPath.get(u[0]) || 0) + unitWeight(u)));
    const list = [...byPath.keys()];
    const unit = esc(state.pathUnit || 'items');
    const weight = (pi) => byPath.get(pi);
    const total = list.reduce((t, pi) => t + weight(pi), 0);
    const stages = layout.stageCount;
    const nodeW = Array.from({ length: stages + 1 }, () => new Map());
    const strandW = Array.from({ length: stages }, () => new Map());
    const add = (map, key, w) => map.set(key, (map.get(key) || 0) + w);
    list.forEach((pi) => traces.paths[pi].nodes.forEach((ni, s, nodes) => {
      add(nodeW[s], ni, weight(pi));
      if (s < stages) add(strandW[s], `${ni},${nodes[s + 1]}`, weight(pi));
    }));
    const head = (title) => `<thead><tr><th>${title}</th><th class="num">${unit}</th><th class="num">share</th></tr></thead>`;
    const group = (title) => `<tr class="group"><th colspan="3">${esc(title)}</th></tr>`;
    // The share bar fills an outlined box that stands for 100% of the traced total.
    const row = (label, w) => `<tr><td>${label}</td><td class="num">${fmtValue(w)}</td>` +
      `<td class="num"><div class="share" style="--p:${(100 * w / total).toFixed(1)}%">${pct(w, total)}</div></td></tr>`;
    const note = (text) => `<tr class="note"><td colspan="3">${text}</td></tr>`;
    const more = (n) => (n > 0 ? note(`… and ${n} more`) : '');
    const name = (s, i) => esc(nodeDisplayName(nodeAtStage(s, i)));

    let nodes = head('Node');
    for (let s = 0; s <= stages; s++) {
      const set = layout.nodes.filter((n) => n.set === s + 1).sort((a, b) => b.Y - a.Y);
      const missed = set.filter((n) => !nodeW[s].has(n.node - 1));
      nodes += group(setName(s)) +
        set.filter((n) => nodeW[s].has(n.node - 1)).map((n) => row(name(s, n.node - 1), nodeW[s].get(n.node - 1))).join('') +
        (missed.length ? note(`none via ${missed.map((n) => name(s, n.node - 1)).join(', ')}`) : '');
    }
    let strands = head('Strand');
    strandW.forEach((map, s) => {
      const sorted = [...map].sort((a, b) => b[1] - a[1]);
      strands += group(state.stageNames[s] || `Stage ${s + 1}`) + sorted.slice(0, TOP).map(([key, w]) => {
        const [i, j] = key.split(',').map(Number);
        return row(`${name(s, i)} → ${name(s + 1, j)}`, w);
      }).join('') + more(sorted.length - TOP);
    });
    const top = list.slice().sort((a, b) => weight(b) - weight(a));
    const pathways = head('Pathway') + top.slice(0, TOP).map((pi) => row(esc(pathName(pi)), weight(pi))).join('') + more(top.length - TOP);
    const items = list.some((pi) => traces.paths[pi].members);
    return `<h3 class="focus-head">${list.length} pathway${list.length === 1 ? '' : 's'} · ${fmtValue(total)} ${unitOf(total)} ` +
      `<span class="muted">(${pct(total, traces.total)} of all)</span></h3>` +
      `<table class="focus-table">${nodes}</table><table class="focus-table">${strands}</table><table class="focus-table">${pathways}</table>` +
      (target.kind === 'pool' ? '' : `<p class="muted small">Hold Ctrl/⌘ to trace the single pathway under the pointer${items ? `, or Shift for the single ${unitOf(1)}` : ''}; ` +
        'Alt-click to add these substrands to a pool.</p>');
  }

  // Hover details replace the inspector's content until the pointer leaves the plot, so nothing
  // covers the strands being looked at.
  function showHoverInfo(target) {
    const info = $('#hover-info');
    info.hidden = !target;
    $('#inspector-main').hidden = !!target;
    $('#inspector').classList.toggle('hovering', !!target);
    info.innerHTML = target ? hoverHtml(target) : '';
  }

  function setHover(rawTarget, event) {
    if (state.paths && state.paths.length > state.style.maxHoverSubstrands &&
        !(event && state.style.tracePaths && (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey))) rawTarget = null;
    const target = rawTarget ? Object.assign({ paths: null, member: -1, single: false }, rawTarget, tracedPaths(rawTarget, event)) : null;
    const key = (t) => (t ? `${t.kind}:${t.index}:${t.paths ? t.paths.join(',') : ''}:${t.member}:${t.single}` : '');
    const same = key(target) === key(hover);
    hover = target;
    if (same) return;
    applyHighlight();
    showHoverInfo(target);
  }

  const plotPoint = (event) => {
    const p = new DOMPoint(event.clientX, event.clientY).matrixTransform(svg.getScreenCTM().inverse());
    return {
      X: p.x - MARGIN,
      Y: frame.height - MARGIN - p.y
    };
  };

  function startDrag(event) {
    if (event.button !== 0) return;
    const target = targetOf(event);
    if (!target) {
      // Empty space: drag to pan the plot panel; a click that does not move clears the selection.
      event.preventDefault();
      svg.setPointerCapture(event.pointerId);
      pan = { x0: event.clientX, y0: event.clientY, left: stageEl.scrollLeft, top: stageEl.scrollTop, moved: false };
      return;
    }
    event.preventDefault();
    svg.setPointerCapture(event.pointerId);
    showHoverInfo(null);
    const base = { target, x0: event.clientX, y0: event.clientY, moved: false, alt: event.altKey,
      ctrl: event.ctrlKey || event.metaKey, shift: event.shiftKey };
    if (target.kind === 'node') {
      const n = layout.nodes[target.index];
      drag = Object.assign(base, {
        row: n.id, locX: state.loc[n.id].x, locY: state.loc[n.id].y, sx: layout.tf.sx * displayScale, sy: layout.tf.sy * displayScale,
        layout0: layout, startX: state.loc.map((r) => r.x)
      });
    } else {
      const f = layout.flows[target.index];
      const p = plotPoint(event);
      const side = Math.abs(p.X - f.startX) <= Math.abs(p.X - f.endX) ? 'start' : 'end';
      const node = layout.nodes[side === 'start' ? f.from : f.to];
      const inds = side === 'start' ? node.outFlows : node.inFlows;
      const field = side === 'start' ? 'startOffset' : 'endOffset';
      const center = (k) => layout.flows[k][field];
      drag = Object.assign(base, {
        side,
        nodeKey: node.key,
        flowKey: f.key,
        center: center(f.index),
        others: inds.filter((k) => k !== f.index).map((k) => ({ key: layout.flows[k].key, center: center(k) }))
          .sort((a, b) => a.center - b.center)
      });
    }
    svg.classList.add('dragging');
    applyHighlight();
  }

  function moveDrag(event) {
    const dx = event.clientX - drag.x0;
    const dy = event.clientY - drag.y0;
    if (!drag.moved && Math.hypot(dx, dy) < 3) return;
    drag.moved = true;
    if (drag.target.kind === 'node') {
      // Placed once per frame in requestRender(), since collision checks run test layouts.
      // Within the current view: a node dragged past the edge stops there.
      const clamp = (v, [lo, hi]) => Math.min(Math.max(v, lo), hi);
      drag.goal = {
        x: clamp(event.shiftKey ? drag.locX : drag.locX + dx / drag.sx, state.view.xlim),
        y: clamp(event.altKey ? drag.locY : drag.locY - dy / drag.sy, state.view.ylim)
      };
      requestRender();
      return;
    }
    if (drag.others.length === 0) return;
    const goal = drag.center - dy / displayScale;
    const index = drag.others.filter((o) => o.center < goal).length;
    const order = drag.others.map((o) => o.key);
    order.splice(index, 0, drag.flowKey);
    const current = state.manualOrder[drag.side][drag.nodeKey];
    if (!current || current.join('|') !== order.join('|')) {
      state.manualOrder[drag.side] = Object.assign({}, state.manualOrder[drag.side], { [drag.nodeKey]: order });
      requestRender();
    }
  }

  const MIN_RUN_PX = 16;

  // A layout at the drawing's own resolution (constant-width turns that draw at full resolution can
  // fail at a coarser one) tells whether a candidate drag position is drawable, keeps every strand's
  // direction, and leaves each strand a minimum horizontal run (or its run at drag start).
  function dragIsValid(d) {
    let lay;
    try {
      lay = L.computeLayout(model(), viewFor(state.view), { measure: measure() });
    } catch (err) {
      if (err instanceof L.SankeyError) return false;
      throw err;
    }
    return lay.flows.every((f, k) => {
      const f0 = d.layout0.flows[k];
      return f.dir === f0.dir &&
        f.dir * (f.endX - f.startX) >= Math.min(MIN_RUN_PX, f0.dir * (f0.endX - f0.startX)) - 1e-9;
    });
  }

  // Push mode: shove nodes along each strand's direction until every strand touching a moved node
  // keeps a minimum horizontal run between node faces (positions relative to the drag start).
  function pushNeighbours(d) {
    const L0 = d.layout0;
    const halfWidth = L0.nodes.map((n) => n.widthPx / 2 / L0.tf.sx);
    const minRun = MIN_RUN_PX / L0.tf.sx;
    const queue = [d.row];
    let budget = 20 * L0.flows.length;
    while (queue.length && budget-- > 0) {
      const u = queue.shift();
      L0.flows.forEach((f) => {
        if (f.from !== u && f.to !== u) return;
        const v = f.from === u ? f.to : f.from;
        if (v === d.row) return;
        const dir = f.dir;
        const need = halfWidth[f.from] + halfWidth[f.to] + 1.02 * minRun;
        const run = dir * (state.loc[f.to].x - state.loc[f.from].x);
        if (run >= need) return;
        if (v === f.to) state.loc[v].x = round(state.loc[f.from].x + dir * need);
        else state.loc[v].x = round(state.loc[f.to].x - dir * need);
        queue.push(v);
      });
    }
  }

  // Move the dragged node toward goal without ever producing an invalid layout. In push mode
  // colliding neighbours move along; otherwise, or if it is still invalid, the node stops at the
  // last valid point on the way there (an invisible wall).
  function placeNode(d, goal) {
    const row = state.loc[d.row];
    const from = { x: row.x, y: row.y };
    const tryAt = (p) => {
      state.loc.forEach((r, i) => { r.x = d.startX[i]; });
      row.x = round(p.x);
      row.y = round(p.y);
      if (state.style.collision === 'push') pushNeighbours(d);
      return dragIsValid(d);
    };
    // Furthest valid point on the segment a -> b, given that a is valid (to 1/256 of the segment).
    const approach = (a, b) => {
      if (tryAt(b)) return b;
      let lo = 0;
      let hi = 1;
      for (let i = 0; i < 8; i++) {
        const mid = (lo + hi) / 2;
        if (tryAt({ x: a.x + (b.x - a.x) * mid, y: a.y + (b.y - a.y) * mid })) lo = mid;
        else hi = mid;
      }
      return { x: a.x + (b.x - a.x) * lo, y: a.y + (b.y - a.y) * lo };
    };
    if (tryAt(goal)) return;
    // Blocked: slide along the wall by settling y first, then x.
    let p = approach(from, { x: from.x, y: goal.y });
    p = approach(p, { x: goal.x, y: p.y });
    tryAt(p);
  }

  function endDrag(event) {
    const finished = drag;
    if (finished.goal) placeNode(finished, finished.goal);
    drag = null;
    svg.classList.remove('dragging');
    if (svg.hasPointerCapture(event.pointerId)) svg.releasePointerCapture(event.pointerId);
    if (!finished.moved) {
      const t = finished.target;
      const shown = hover && hover.kind === t.kind && hover.index === t.index ? hover : null;
      // Preserve ordinary Alt-click pooling below the limit; dense plots only pool the single slice.
      const pooled = finished.alt && !finished.ctrl && !finished.shift && traces && traces.paths.length <= state.style.maxHoverSubstrands
        ? Object.assign({}, t, tracedPaths(t, {})) : shown;
      // Alt-click: a plain one on a node picks it (e.g. for merging); with pathway data, the
      // substrands shown join or leave the pool.
      const pickNode = finished.alt && t.kind === 'node' && !finished.ctrl && !finished.shift;
      const poolIt = finished.alt && pooled && pooled.paths && traces;
      if (pickNode || poolIt) {
        if (pickNode) togglePicked(layout.nodes[t.index].key);
        if (poolIt) togglePool(pooled);
        const units = poolTarget() ? poolTarget().units : [];
        const nodes = pickedNodes();
        notify([
          units.length ? `Pool: ${fmtValue(units.reduce((sum, u) => sum + unitWeight(u), 0))} ${unitOf(2)} in ${new Set(units.map((u) => u[0])).size} pathways.` : '',
          nodes.length ? `Picked: ${nodes.map(nodeDisplayName).join(', ')}.` : '',
          'Esc clears.'
        ].filter(Boolean).join(' '));
        applyHighlight();
        refreshInspector();
        return;
      }
      selection = {
        kind: t.kind,
        key: (t.kind === 'flow' ? layout.flows : layout.nodes)[t.index].key,
        paths: shown && shown.paths,
        member: shown ? shown.member : -1,
        single: !!(shown && shown.single)
      };
      applyHighlight();
      refreshInspector();
      return;
    }
    update({ commit: true });
  }

  function endPan(event) {
    const clicked = !pan.moved;
    pan = null;
    svg.classList.remove('panning');
    if (svg.hasPointerCapture(event.pointerId)) svg.releasePointerCapture(event.pointerId);
    if (clicked && selection) {
      selection = null;
      applyHighlight();
      refreshInspector();
    }
  }

  function bindPlotEvents() {
    svg.addEventListener('pointerdown', startDrag);
    svg.addEventListener('pointermove', (event) => {
      lastPointer = { clientX: event.clientX, clientY: event.clientY, altKey: event.altKey, ctrlKey: event.ctrlKey, metaKey: event.metaKey, shiftKey: event.shiftKey };
      if (pan) {
        const dx = event.clientX - pan.x0;
        const dy = event.clientY - pan.y0;
        if (!pan.moved && Math.hypot(dx, dy) < 3) return;
        pan.moved = true;
        svg.classList.add('panning');
        stageEl.scrollLeft = pan.left - dx;
        stageEl.scrollTop = pan.top - dy;
        return;
      }
      if (drag) {
        moveDrag(event);
        return;
      }
      setHover(targetOf(event), event);
    });
    svg.addEventListener('pointerup', (event) => { if (pan) endPan(event); else if (drag) endDrag(event); });
    svg.addEventListener('pointercancel', (event) => { if (pan) endPan(event); else if (drag) endDrag(event); });
    // The wheel (or a trackpad pinch) over the plot panel zooms about the pointer.
    stageEl.addEventListener('wheel', (event) => {
      if (!layout) return;
      event.preventDefault();
      zoomAt(event.clientX, event.clientY, Math.exp(-event.deltaY * (event.deltaMode === 1 ? 0.05 : 0.0015)));
    }, { passive: false });
    svg.addEventListener('pointerleave', () => {
      if (!drag) { lastPointer = null; setHover(null); }
    });
  }

  // ---------- inspector ----------

  function colorEditor(name, label, color, isSet) {
    const hex = C.toHex(color, false).toLowerCase();
    const alpha = Math.round(color.a * 100) / 100;
    return `<div class="row"><label>${label}</label><span class="color-edit" data-color="${name}">` +
      `<input type="color" value="${hex}"><input type="range" min="0" max="1" step="0.01" value="${alpha}" title="opacity">` +
      `<button class="mini-btn" data-reset="${name}"${isSet ? '' : ' disabled'} title="Use the automatic color">auto</button></span></div>`;
  }

  function bindColorEditor(root, name, apply) {
    const host = root.querySelector(`[data-color="${name}"]`);
    if (!host) return;
    const [picker, alpha] = host.querySelectorAll('input');
    const value = () => C.toHex(C.withAlpha(C.parse(picker.value), Number(alpha.value)));
    [picker, alpha].forEach((el) => {
      el.addEventListener('input', () => { apply(value()); requestRender(true); });
      el.addEventListener('change', () => { apply(value()); update({ commit: true }); });
    });
    root.querySelector(`[data-reset="${name}"]`).addEventListener('click', () => { apply(null); update({ commit: true }); });
  }

  const setKeyed = (obj, key, value) => {
    if (value === null) delete obj[key];
    else obj[key] = value;
  };

  // A selected strand or node keeps its hover details (and traced pathways) pinned above its editor.
  const pinnedHtml = () => `<p class="pin-note muted small">Pinned · click empty space or press Esc to release</p>${hoverHtml(pinnedTarget())}`;

  const bindPool = (host) => {
    const button = host.querySelector('#pool-clear');
    if (button) button.addEventListener('click', clearPool);
  };

  function refreshInspector() {
    const host = $('#inspector-main');
    if (drag) return;
    const sel = findSelected();
    // A pool shows (and is highlighted) in place of the pinned selection's breakdown.
    const head = () => (groupTarget() ? hoverHtml(groupTarget()) : pinnedHtml());
    if (!sel && groupTarget()) {
      host.innerHTML = hoverHtml(groupTarget());
      bindPool(host);
      return;
    }
    if (!sel) {
      host.innerHTML = '<h2>Inspector</h2><p class="muted">Hover a strand or node for details; click to select and edit it.</p>' +
        (state.paths && state.paths.length > state.style.maxHoverSubstrands ? `<p class="muted small">Ordinary hover is paused for ${state.paths.length.toLocaleString()} pathways (limit ${state.style.maxHoverSubstrands}). Hold Alt or Ctrl/⌘ for one pathway, or Shift for one item. Change “Max substrands for hover” in Canvas & dragging to raise the limit.</p>` : '') +
        '<ul class="tips"><li>Drag a node to move it (Shift: vertical only, Alt: horizontal only).</li>' +
        '<li>Drag a strand up or down to reorder it in the stack at its nearer end.</li>' +
        (traces ? '<li>Hover a strand or node to trace every pathway through it, with a breakdown here; hold Alt or Ctrl/⌘ for the single pathway under the pointer, or Shift for the single item. Click to pin the traces and breakdown.</li>' : '') +
        '<li>Ctrl/⌘+Z undo, Ctrl/⌘+Shift+Z redo, F fit view, Esc deselect.</li></ul>';
      return;
    }
    if (selection.kind === 'flow') {
      const f = sel;
      const a = layout.nodes[f.from];
      const b = layout.nodes[f.to];
      const mid = colors.mid[f.index];
      const midSet = Object.prototype.hasOwnProperty.call(state.midOverrides, f.key);
      host.innerHTML = head() + '<h3 class="edit-head">Edit strand</h3>' +
        `<div class="row"><label for="ins-value">Value</label><input id="ins-value" type="number" min="0" step="any" value="${f.value}"></div>` +
        colorEditor('flow', 'Color', colors.flow[f.index], !!state.flowOverrides[f.key]) +
        `<div class="row"><label for="ins-mid-on">Mid color</label><input id="ins-mid-on" type="checkbox"${mid.color ? ' checked' : ''}></div>` +
        (mid.color ? colorEditor('mid', '', mid.color, midSet) : '') +
        (state.style.gradient ? '' : '<p class="muted small">Mid colors show when gradients are on.</p>') +
        '<button class="mini-btn wide" id="ins-reset-order">Reset stack order at both ends</button>';
      bindPool(host);
      host.querySelector('#ins-value').addEventListener('change', (e) => {
        const v = Number(e.target.value);
        if (!(Number.isFinite(v) && v >= 0)) {
          showError('Strand values must be non-negative numbers.');
          return;
        }
        state.matrices[f.stage][f.i][f.j] = v;
        if (v === 0) selection = null;
        update({ commit: true });
      });
      bindColorEditor(host, 'flow', (v) => setKeyed(state.flowOverrides, f.key, v));
      bindColorEditor(host, 'mid', (v) => {
        if (v === null) delete state.midOverrides[f.key];
        else state.midOverrides[f.key] = v;
      });
      host.querySelector('#ins-mid-on').addEventListener('change', (e) => {
        const fileMid = state.midMatrices && state.style.useMid && state.midMatrices[f.stage] &&
          state.midMatrices[f.stage][f.i] && state.midMatrices[f.stage][f.i][f.j];
        if (e.target.checked) {
          state.midOverrides[f.key] = C.toHex(C.mix(colors.node[f.from], colors.node[f.to], 0.5));
        } else if (fileMid) {
          state.midOverrides[f.key] = null; // explicitly drop the file's mid color (R's NA)
        } else {
          delete state.midOverrides[f.key];
        }
        update({ commit: true });
      });
      host.querySelector('#ins-reset-order').addEventListener('click', () => {
        delete state.manualOrder.start[a.key];
        delete state.manualOrder.end[b.key];
        update({ commit: true });
      });
      return;
    }
    const n = sel;
    const row = state.loc[n.id];
    const num = (v) => (v === null || v === undefined || v === '' ? '' : v);
    host.innerHTML = head() + '<h3 class="edit-head">Edit node</h3>' +
      `<div class="row"><label for="ins-label">Label</label><input id="ins-label" type="text" value="${esc(row.label || '')}"></div>` +
      ['x', 'y', 'width', 'height', 'cex'].map((k) => `<div class="row"><label for="ins-${k}">${k}</label>` +
        `<input id="ins-${k}" data-field="${k}" type="number" step="any" value="${num(row[k])}" ` +
        `placeholder="${k === 'cex' ? '1' : (k === 'x' || k === 'y' ? '' : 'auto')}"></div>`).join('') +
      `<div class="row"><label for="ins-font">Font</label><select id="ins-font">${[[1, 'plain'], [2, 'bold'], [3, 'italic'], [4, 'bold italic']]
        .map(([v, l]) => `<option value="${v}"${(row.font || 1) === v ? ' selected' : ''}>${l}</option>`).join('')}</select></div>` +
      colorEditor('node', 'Node color', colors.node[n.id], !!(state.nodeColors[n.key] || row.color)) +
      colorEditor('merge', 'Trunk color', colors.merge[n.id], !!(state.mergeColors[n.key] || row.merge_col)) +
      '<button class="mini-btn wide" id="ins-reset-order">Reset stack order at this node</button>' +
      (state.params.adjust_loc ? '<p class="muted small">y is adjusted by "Adjust node y positions"; edits change the requested y.</p>' : '');
    bindPool(host);
    host.querySelector('#ins-label').addEventListener('change', (e) => {
      row.label = e.target.value;
      update({ commit: true });
    });
    host.querySelectorAll('[data-field]').forEach((input) => input.addEventListener('change', () => {
      const k = input.dataset.field;
      if (input.value === '' && (k === 'x' || k === 'y')) {
        showError(`${k} cannot be empty.`);
        input.value = row[k];
        return;
      }
      if (input.value === '') {
        delete row[k];
      } else {
        const v = Number(input.value);
        if (!Number.isFinite(v)) {
          showError(`${k} must be a number.`);
          return;
        }
        row[k] = v;
      }
      update({ commit: true });
    }));
    host.querySelector('#ins-font').addEventListener('change', (e) => {
      row.font = Number(e.target.value);
      update({ commit: true });
    });
    bindColorEditor(host, 'node', (v) => {
      setKeyed(state.nodeColors, n.key, v);
      if (v === null) delete row.color;
    });
    bindColorEditor(host, 'merge', (v) => {
      setKeyed(state.mergeColors, n.key, v);
      if (v === null) delete row.merge_col;
    });
    host.querySelector('#ins-reset-order').addEventListener('click', () => {
      delete state.manualOrder.start[n.key];
      delete state.manualOrder.end[n.key];
      update({ commit: true });
    });
  }

  // ---------- exports ----------

  function exportSvgText() {
    if (!layout) throw new L.SankeyError('Nothing to export: the current data does not produce a plot.');
    const clone = svg.cloneNode(true);
    clone.style.removeProperty('width');
    clone.style.removeProperty('height');
    // The app's CSS dims or hides parts of the plot around a focus; copy those computed styles
    // onto the export so it shows the focused view, traces and fill bars included.
    const copies = clone.querySelectorAll('*');
    svg.querySelectorAll('*').forEach((el, i) => {
      const cs = getComputedStyle(el);
      if (cs.opacity !== '1') copies[i].setAttribute('opacity', cs.opacity);
      if (cs.filter && cs.filter !== 'none') copies[i].setAttribute('filter', cs.filter.replace(/url\("?([^")]+)"?\)/, 'url($1)'));
    });
    clone.querySelectorAll('.layer-hits').forEach((el) => el.remove());
    clone.querySelectorAll('.layer-traces, .layer-trace-labels, .layer-fills').forEach((el) => { if (!el.childElementCount) el.remove(); });
    clone.removeAttribute('class');
    clone.removeAttribute('id');
    clone.querySelectorAll('[class]').forEach((el) => {
      el.classList.remove('hl', 'selected', 'covered');
    });
    clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
    return `<?xml version="1.0" encoding="UTF-8"?>\n${new XMLSerializer().serializeToString(clone)}`;
  }

  function exportPng() {
    const text = exportSvgText();
    const url = URL.createObjectURL(new Blob([text], { type: 'image/svg+xml' }));
    const img = new Image();
    img.onload = () => {
      const scale = 2;
      const canvas = document.createElement('canvas');
      canvas.width = frame.width * scale;
      canvas.height = frame.height * scale;
      canvas.getContext('2d').drawImage(img, 0, 0, canvas.width, canvas.height);
      URL.revokeObjectURL(url);
      canvas.toBlob((blob) => download('sankey.png', blob), 'image/png');
    };
    img.onerror = () => showError('PNG export failed while rasterizing the SVG.');
    img.src = url;
  }

  const csvNumber = (v) => (v === null || v === undefined || v === '' ? null : round(v, 8));

  // loc in R's data-frame form; widths are written for every row when any row sets one,
  // because R treats a width column as explicit for all nodes.
  function locCsv(rows = state.loc) {
    const anyLabel = rows.some((r) => r.label);
    const anyWidth = rows.some((r) => Number.isFinite(r.width));
    const anyHeight = rows.some((r) => Number.isFinite(r.height));
    const anyColor = rows.some((r) => r.color) || Object.keys(state.nodeColors).length > 0;
    const anyMerge = rows.some((r) => r.merge_col) || Object.keys(state.mergeColors).length > 0;
    const header = ['set', 'node', 'x', 'y'];
    if (anyWidth) header.push('width');
    if (anyLabel) header.push('label', 'cex', 'font');
    if (anyHeight) header.push('height');
    if (anyColor) header.push('color');
    if (anyMerge) header.push('merge_col');
    const out = [header];
    rows.forEach((r, id) => {
      const key = `${r.set}:${r.node}`;
      const node = layout && layout.nodes[id];
      const line = [r.set, r.node, csvNumber(r.x), csvNumber(r.y)];
      if (anyWidth) line.push(csvNumber(node ? node.widthPx / layout.tf.sx : r.width));
      if (anyLabel) line.push(r.label || '', csvNumber(r.cex) || 1, r.font || 1);
      if (anyHeight) line.push(csvNumber(node && node.bodyHeightPx ? node.bodyHeightPx / layout.tf.sy : r.height));
      if (anyColor) line.push(state.nodeColors[key] || r.color || null);
      if (anyMerge) line.push(state.mergeColors[key] || r.merge_col || null);
      out.push(line);
    });
    return CSV.format(out);
  }

  function matrixCsv(m) {
    return CSV.format(m.map((r) => r.map((v) => (typeof v === 'number' ? csvNumber(v) : v))));
  }

  // Resolved strand (and mid) colors laid out like the flow matrices, NA where no strand.
  // Strands without a mid color use their own color as the midpoint, as R does without mid_col;
  // NA asks R for a direct endpoint-to-endpoint gradient.
  function colorMatricesFor(kind) {
    return state.matrices.map((m, s) => m.map((row, i) => row.map((v, j) => {
      const f = layout.flows.find((x) => x.stage === s && x.i === i && x.j === j);
      if (!f) return null;
      if (kind === 'flow') return C.toHex(colors.flow[f.index]);
      const mid = colors.mid[f.index];
      if (!mid.provided) return C.toHex(colors.flow[f.index]);
      return mid.color ? C.toHex(mid.color) : null;
    })));
  }

  // Manual strand orders as R's stack_order rows: side, stage, from, to, rank (1-based, 1 = bottom).
  function stackOrderRows() {
    const keys = new Set(layout.flows.map((f) => f.key));
    const rows = [];
    ['start', 'end'].forEach((side) => {
      Object.values(state.manualOrder[side]).forEach((list) => {
        list.filter((key) => keys.has(key)).forEach((key, r) => {
          const [stage, i, j] = key.split(':').map(Number);
          rows.push([side, stage + 1, i + 1, j + 1, r + 1]);
        });
      });
    });
    return rows;
  }

  function rString(s) {
    return `"${String(s).replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`;
  }

  function rValue(v) {
    if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
    if (typeof v === 'number') return String(round(v, 8));
    if (v === null) return 'NULL';
    return rString(v);
  }

  function rCode(files) {
    if (!layout) throw new L.SankeyError('Nothing to export: the current data does not produce a plot.');
    const graph = state.mode === 'graph';
    // Graph mode has a single stage, so its "stage 1" overrides are plain arguments in R.
    const p = graph ? Object.assign({}, state.params, state.stageParams[0] || {}) : state.params;
    const style = state.style;
    const scaled = state.valueScale !== 1;
    const readThic = graph
      ? `read_numbers("adjacency.csv")${scaled ? ' * value_scale' : ''}`
      : `lapply(c(${files.stages.map(rString).join(', ')}), ${scaled ? 'function(f) read_numbers(f) * value_scale' : 'read_numbers'})`;
    const lines = [
      '# Generated by the polylines Sankey webapp.',
      '# Run next to the exported CSV files, with R/functions.R from the polylines repository.',
      'source("R/functions.R")',
      '',
      'read_numbers <- function(f) unname(as.matrix(read.csv(f, header = FALSE)))',
      'read_colors <- function(f) unname(as.matrix(read.csv(f, header = FALSE, colClasses = "character")))',
      ...(scaled ? [`value_scale <- ${rValue(state.valueScale)}  # strand thickness (x units) per unit of value`] : []),
      `thic <- ${readThic}`,
      'loc <- read.csv("loc.csv", stringsAsFactors = FALSE)',
      ...(layout.nodes.some(n => n.labelOutside) ? [
        `loc$label[c(${layout.nodes.filter(n => n.labelOutside).map(n => n.id + 1).join(', ')})] <- NA  # labels outside bodies are drawn separately`
      ] : []),
      // Labels inside nodes fade their bodies in R too, so labels drawn above are dropped from loc
      // (and drawn separately below), keeping the label-derived widths measured here.
      ...(style.labelPosition === 'above' ? [
        `loc$width <- c(${layout.nodes.map((n) => rValue(round(n.widthPx / layout.tf.sx))).join(', ')})  # node widths (x units)`,
        'loc$label <- NA  # labels are drawn above the nodes, so node bodies stay solid'
      ] : []),
      graph ? 'col <- read_colors("colors.csv")' : `col <- lapply(c(${files.colors.map(rString).join(', ')}), read_colors)`
    ];
    if (files.mids.length) {
      lines.push(graph ? 'mid_col <- read_colors("mid_colors.csv")' : `mid_col <- lapply(c(${files.mids.map(rString).join(', ')}), read_colors)`);
    }
    if (files.stackOrder) lines.push('stack_order <- read.csv("stack_order.csv", stringsAsFactors = FALSE)');
    const setCount = graph ? 0 : state.matrices.length + 1;
    if (graph) {
      // One color per strand, in R's column-major order of positive adjacency entries:
      // trunk (merge) colors and strand gradient endpoint (node) colors.
      const ends = (cols, side) => layout.flows.map((f) => rString(C.toHex(cols[side === 'start' ? f.from : f.to])));
      const perStrand = (cols) => `list(\n  start = c(${ends(cols, 'start').join(', ')}),\n  end = c(${ends(cols, 'end').join(', ')})\n)`;
      lines.push(`merge_cols <- ${perStrand(colors.merge)}`, `endpoint_cols <- ${perStrand(colors.node)}`);
    } else {
      const bySet = Array.from({ length: setCount }, () => []);
      const mergeBySet = Array.from({ length: setCount }, () => []);
      layout.nodes.forEach((n) => {
        bySet[n.set - 1][n.node - 1] = C.toHex(colors.node[n.id]);
        mergeBySet[n.set - 1][n.node - 1] = C.toHex(colors.merge[n.id]);
      });
      lines.push(`node_col <- list(\n${bySet.map((s) => `  c(${s.map(rString).join(', ')})`).join(',\n')}\n)`);
      if (mergeBySet.some((s, i) => s.some((c, j) => c !== bySet[i][j]))) {
        lines.push(`merge_col <- list(\n${mergeBySet.map((s) => `  c(${s.map(rString).join(', ')})`).join(',\n')}\n)`);
      }
    }
    const plotW = frame.width - 2 * MARGIN;
    const plotH = layout.view.height;
    const margins = frame.subset.height
      ? `c(${[MARGIN, MARGIN, frame.margin.top, MARGIN].map((v) => round(v / 96, 4)).join(', ')})`
      : `rep(${round(MARGIN / 96, 4)}, 4)`;
    lines.push('', `png("sankey.png", width = ${frame.width}, height = ${frame.height}, res = 96, bg = ${rString(C.toHex(colors.flatBg))})`,
      `par(mai = ${margins}, xpd = NA, ps = ${round(style.fontSize * 0.75, 4)})`,
      'plot.new()',
      `plot.window(xlim = c(${state.view.xlim.map(rValue).join(', ')}), ylim = c(${state.view.ylim.map(rValue).join(', ')}), xaxs = "i", yaxs = "i")`);
    const args = [];
    const defaults = L.DEFAULT_PARAMS;
    const skip = graph ? ['adjust_loc', 'min_node_gap', 'adjust_loc_expand'] : ['require_forward', 'balance', 'balance_tol'];
    Object.keys(defaults).forEach((key) => {
      if (skip.includes(key)) return;
      const staged = L.STAGE_KEYS.includes(key) && state.stageParams.some((sp) => sp && key in sp);
      if (staged && !graph) {
        const values = state.matrices.map((_, s) => (state.stageParams[s] && key in state.stageParams[s] ? state.stageParams[s][key] : p[key]));
        args.push(`${key} = list(${values.map(rValue).join(', ')})`);
      } else if (p[key] !== defaults[key]) {
        args.push(`${key} = ${rValue(p[key])}`);
      }
    });
    if (files.stackOrder) args.push('stack_order = stack_order');
    lines.push(`sankey <- ${graph ? 'sankey_ribbons_graph' : 'sankey_ribbons'}(\n  thic,\n  loc${args.map((a) => `,\n  ${a}`).join('')}\n)`);
    const plotArgs = ['col = col'];
    if (graph) plotArgs.push('merge_cols = merge_cols', 'endpoint_cols = endpoint_cols');
    else plotArgs.push('node_col = node_col');
    if (lines.some((l) => l.startsWith('merge_col <-'))) plotArgs.push('merge_col = merge_col');
    if (files.mids.length) plotArgs.push('mid_col = mid_col');
    plotArgs.push(`gradient_slices = ${style.gradient ? 160 : 1}`);
    if (style.gradient) plotArgs.push(`gradient_mode = ${rString(style.gradientMode)}`);
    plotArgs.push(`border = ${style.border === 'none' ? 'NA' : rString(style.border)}`);
    if (style.border !== 'none') plotArgs.push(`border_lwd = ${rValue(style.borderWidth)}`, `border_caps = ${rValue(style.borderCaps)}`);
    plotArgs.push(`node_border = ${style.nodeBorder === 'auto' ? 'NULL' : style.nodeBorder === 'none' ? 'NA' : rString(style.nodeBorder)}`);
    plotArgs.push(`node_border_lwd = ${rValue(style.nodeBorderWidth)}`, `flatten_trunks = ${rValue(style.flatten)}`);
    if (style.nodeSums) {
      const xs = layout.nodes.map((n) => n.x);
      plotArgs.push('show_node_sums = TRUE', `node_sum_col = ${rString(C.toHex(C.parse(style.labelColor)))}`,
        `node_sum_offset = ${round(0.06 * (Math.max(...xs) - Math.min(...xs)))}`);
    }
    plotArgs.push('label_nodes = FALSE', 'add = TRUE');
    lines.push(`plot_sankey_ribbons(\n  sankey,\n  ${plotArgs.join(',\n  ')}\n)`);
    const labeled = style.labels ? layout.nodes.filter((n) => n.hasLabel) : [];
    if (labeled.length) {
      // Same placement as the webapp (render.js labelPlacement): centred text (pos 0), or left of
      // sources (pos 2) and right of sinks (pos 4).
      const rows = labeled.map((n) => {
        const pos = R.labelPlacement(n, style, style.fontSize * n.cex);
        const side = pos.anchor === 'end' ? 2 : pos.anchor === 'start' ? 4 : 0;
        return [n.x, side ? n.y : layout.tf.y(pos.Y), n, side, C.toHex(R.labelColor(n, colors, style))];
      });
      const col = (f) => rows.map(f).join(', ');
      lines.push(`labels <- data.frame(\n  x = c(${col((r) => rValue(r[0]))}),\n  y = c(${col((r) => rValue(r[1]))}),\n` +
        `  label = c(${col((r) => rString(r[2].label))}),\n  cex = c(${col((r) => rValue(r[2].cex))}),\n` +
        `  font = c(${col((r) => r[2].font)}),\n  pos = c(${col((r) => r[3])}),\n` +
        `  col = c(${col((r) => rString(r[4]))}),\n  stringsAsFactors = FALSE\n)`,
      'centred <- labels$pos == 0');
      // text() rejects zero-length labels, so only emit the calls that have rows.
      if (rows.some((r) => r[3] === 0)) {
        lines.push('text(labels$x[centred], labels$y[centred], labels$label[centred], cex = labels$cex[centred], font = labels$font[centred], col = labels$col[centred])');
      }
      if (rows.some((r) => r[3] !== 0)) {
        lines.push('text(labels$x[!centred], labels$y[!centred], labels$label[!centred], pos = labels$pos[!centred], cex = labels$cex[!centred], font = labels$font[!centred], col = labels$col[!centred])');
      }
    }
    if (style.showPoints) lines.push('points(sankey$nodes$x, sankey$nodes$y, pch = 21, bg = "white", lwd = 2)');
    frame.subset.lines.forEach((line, i) => {
      const text = line.map((part) => part.bold ? `bold(${rString(part.text)})` : rString(part.text)).join(', ');
      const y = layout.tf.y(plotH + frame.margin.top - MARGIN - i * 17);
      lines.push(`text(${rValue(state.view.xlim[0])}, ${rValue(y)}, expression(paste(${text})), adj = c(0, 0.5), cex = ${rValue(12 / style.fontSize)}, col = "#1F2937")`);
    });
    lines.push('dev.off()');
    lines.push(`# Plot region: ${plotW} x ${plotH} px.`);
    return `${lines.join('\n')}\n`;
  }

  function bundleFiles() {
    if (!layout) throw new L.SankeyError('Nothing to export: the current data does not produce a plot.');
    const graph = state.mode === 'graph';
    const out = [];
    const names = { stages: [], colors: [], mids: [] };
    state.matrices.forEach((m, s) => {
      const name = graph ? 'adjacency.csv' : `stage_${s + 1}.csv`;
      names.stages.push(name);
      out.push({ name, text: matrixCsv(m) });
    });
    colorMatricesFor('flow').forEach((m, s) => {
      const name = graph ? 'colors.csv' : `colors_${s + 1}.csv`;
      names.colors.push(name);
      out.push({ name, text: matrixCsv(m) });
    });
    if (colors.mid.some((m) => m.provided)) {
      colorMatricesFor('mid').forEach((m, s) => {
        const name = graph ? 'mid_colors.csv' : `mid_colors_${s + 1}.csv`;
        names.mids.push(name);
        out.push({ name, text: matrixCsv(m) });
      });
    }
    // R reads labels (with any totals) from loc, so it sizes label-width nodes as drawn here.
    out.push({ name: 'loc.csv', text: locCsv(labeledLoc()) });
    const stackRows = stackOrderRows();
    if (stackRows.length) {
      names.stackOrder = true;
      out.push({ name: 'stack_order.csv', text: CSV.format([['side', 'stage', 'from', 'to', 'rank'], ...stackRows]) });
    }
    out.push({ name: 'sankey.R', text: rCode(names) });
    out.push({ name: 'project.json', text: projectJson() });
    return out;
  }

  function projectJson() {
    return JSON.stringify(Object.assign({ app: 'polylines-sankey' }, state), null, 1);
  }

  function importProject(obj) {
    if (!obj || obj.app !== 'polylines-sankey' || obj.version !== 1) {
      throw new L.SankeyError('This JSON file is not a polylines Sankey project (version 1).');
    }
    const next = blankState();
    Object.keys(next).forEach((key) => { if (key in obj) next[key] = obj[key]; });
    next.params = Object.assign({}, L.DEFAULT_PARAMS, obj.params);
    // Only known style keys are restored (renamed or retired ones are dropped); the rest take defaults.
    next.style = Object.assign({}, DEFAULT_STYLE);
    Object.keys(DEFAULT_STYLE).forEach((k) => { if (obj.style && k in obj.style) next.style[k] = obj.style[k]; });
    // Older sessions stored the disabled default; enable counts once and fit their labels.
    if (!obj.labelCountsVersion) {
      next.style.labelCounts = true;
      next.view = null;
    }
    if (!obj.traceCountsVersion && next.paths && next.style.tracePaths && next.style.traceCounts) next.view = null;
    state = next;
    selection = null;
    routeScope = 'all';
  }

  function runExport(kind) {
    try {
      if (kind === 'svg') download('sankey.svg', exportSvgText(), 'image/svg+xml');
      else if (kind === 'png') exportPng();
      else if (kind === 'loc') download('loc.csv', locCsv(), 'text/csv');
      else if (kind === 'project') download('sankey_project.json', projectJson(), 'application/json');
      else if (kind === 'bundle') download('sankey_r_bundle.zip', new Blob([window.SK.zip.makeZip(bundleFiles())], { type: 'application/zip' }));
      else if (kind === 'rcode') {
        const files = bundleFiles();
        $('#code-text').value = files.find((f) => f.name === 'sankey.R').text;
        $('#code-dialog').showModal();
      }
    } catch (err) {
      showError(err.message);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
  }

  // ---------- history and persistence ----------

  function pushHistory() {
    const snap = JSON.stringify(state);
    if (history.stack[history.index] === snap) return;
    history.stack = history.stack.slice(0, history.index + 1);
    history.stack.push(snap);
    if (history.stack.length > HISTORY_LIMIT) history.stack.shift();
    history.index = history.stack.length - 1;
    refreshControls();
  }

  function restore(index) {
    if (index < 0 || index >= history.stack.length) return;
    history.index = index;
    state = JSON.parse(history.stack[index]);
    // Pins and pools refer to indices in the previous dataset, not the restored one.
    selection = null;
    pool = new Map();
    picked = new Set();
    hover = null;
    lastPointer = null;
    showHoverInfo(null);
    update();
  }

  function scheduleSave() {
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => {
      try {
        // Style values equal to the defaults are left out, so a later change of default reaches
        // restored sessions instead of being frozen in as if it had been chosen.
        const style = {};
        Object.keys(state.style).forEach((k) => {
          if (JSON.stringify(state.style[k]) !== JSON.stringify(DEFAULT_STYLE[k])) style[k] = state.style[k];
        });
        localStorage.setItem(STORAGE_KEY, JSON.stringify(Object.assign({}, state, { style })));
      } catch (err) {
        notify('This session is too large to autosave in the browser; export a project file to keep it.');
      }
    }, 400);
  }

  // ---------- startup ----------

  function bindChrome() {
    $('#file-input').addEventListener('change', (e) => { loadFileList(e.target.files); e.target.value = ''; });
    document.querySelectorAll('input[type="file"][data-role]').forEach((input) => {
      input.addEventListener('change', (e) => { loadFileList(e.target.files, input.dataset.role); e.target.value = ''; });
    });
    const exampleSelect = $('#example-select');
    exampleSelect.innerHTML = '<option value="">Examples…</option>' +
      window.SK.examples.map((ex) => `<option value="${ex.id}">${esc(ex.title)}</option>`).join('');
    exampleSelect.addEventListener('change', () => {
      if (!exampleSelect.value) return;
      try {
        loadExample(exampleSelect.value);
      } catch (err) {
        showError(err.message);
      }
      exampleSelect.value = '';
    });
    $('#mode-select').addEventListener('change', (e) => {
      state.mode = e.target.value;
      state.paths = null;
      clearKeyedEdits();
      update({ fit: true, commit: true });
    });
    $('#auto-layout').addEventListener('click', () => {
      try {
        autoLocate();
        update({ fit: true, commit: true, spread: true });
      } catch (err) {
        showError(err.message);
      }
    });
    $('#bake-loc').addEventListener('click', () => {
      if (!layout) return;
      layout.nodes.forEach((n) => { state.loc[n.id].y = round(n.y); });
      state.params.adjust_loc = false;
      notify('Adjusted y positions were written into loc and adjust_loc was turned off.');
      update({ commit: true });
    });
    $('#clear-order').addEventListener('click', () => {
      state.manualOrder = { start: {}, end: {} };
      update({ commit: true });
    });
    $('#clear-colors').addEventListener('click', () => {
      state.colorMatrices = null;
      state.colorNames = [];
      if (state.style.colorBy === 'file') state.style.colorBy = 'source';
      update({ commit: true });
    });
    $('#clear-mids').addEventListener('click', () => {
      state.midMatrices = null;
      state.midNames = [];
      update({ commit: true });
    });
    $('#download-examples').addEventListener('click', () => {
      const ex = window.SK.examples.find((e) => e.id === state.example) || window.SK.examples[0];
      const files = Object.entries(ex.files).map(([name, text]) => ({ name, text }));
      download(`${ex.id}_example_files.zip`, new Blob([window.SK.zip.makeZip(files)], { type: 'application/zip' }));
    });
    $('#undo').addEventListener('click', () => restore(history.index - 1));
    $('#redo').addEventListener('click', () => restore(history.index + 1));
    $('#fit').addEventListener('click', () => update({ fit: true, commit: true }));
    $('#align').addEventListener('click', () => {
      try {
        alignColumns();
      } catch (err) {
        showError(err.message);
      }
    });
    // Double-click the canvas to see all of it in the panel.
    svg.addEventListener('dblclick', () => changePresentation('panel'));
    bindCanvasResize();
    $('#fit-panel').addEventListener('click', () => changePresentation('panel'));
    $('#fit-height').addEventListener('click', () => changePresentation('height'));
    $('#zoom-in').addEventListener('click', () => changePresentation(null, 1.25));
    $('#zoom-out').addEventListener('click', () => changePresentation(null, 0.8));
    $('#merge-nodes').addEventListener('click', askMergeName);
    $('#merge-cancel').addEventListener('click', () => $('#merge-dialog').close('cancel'));
    // Only the Merge button submits (Cancel and Esc just close), and submitting merges at once.
    $('#merge-form').addEventListener('submit', () => {
      try {
        mergePicked($('#merge-name').value);
      } catch (err) {
        showError(err.message);
      }
    });
    const menu = $('#export-menu');
    $('#export-btn').addEventListener('click', (e) => {
      e.stopPropagation();
      menu.hidden = !menu.hidden;
    });
    document.addEventListener('click', () => { menu.hidden = true; });
    document.querySelectorAll('[data-export]').forEach((b) => b.addEventListener('click', () => runExport(b.dataset.export)));
    $('#help-btn').addEventListener('click', () => $('#help-dialog').showModal());
    $('#copy-code').addEventListener('click', () => {
      navigator.clipboard.writeText($('#code-text').value).then(() => notify('R code copied.'), () => {
        $('#code-text').select();
        notify('Select-all done; press Ctrl/⌘+C to copy.');
      });
    });

    // Modifier keys also start a single trace when ordinary hover is paused.
    const retrace = (e) => {
      if (!lastPointer || drag || !['Alt', 'Control', 'Meta', 'Shift'].includes(e.key)) return;
      lastPointer = Object.assign({}, lastPointer, { altKey: e.altKey, ctrlKey: e.ctrlKey, metaKey: e.metaKey, shiftKey: e.shiftKey });
      setHover(targetOf({ target: document.elementFromPoint(lastPointer.clientX, lastPointer.clientY) }), lastPointer);
    };
    document.addEventListener('keyup', retrace);
    document.addEventListener('keydown', (e) => {
      retrace(e);
      const typing = /^(INPUT|SELECT|TEXTAREA)$/.test(document.activeElement.tagName);
      const mod = e.ctrlKey || e.metaKey;
      if (mod && e.key.toLowerCase() === 'z' && !typing) {
        e.preventDefault();
        restore(history.index + (e.shiftKey ? 1 : -1));
      } else if (mod && e.key.toLowerCase() === 'y' && !typing) {
        e.preventDefault();
        restore(history.index + 1);
      } else if (!typing && !mod && e.key.toLowerCase() === 'f') {
        update({ fit: true, commit: true });
      } else if (e.key === 'Escape' && (selection || pool.size || picked.size)) {
        selection = null;
        pool = new Map();
        picked = new Set();
        applyHighlight();
        refreshInspector();
      } else if (!typing && e.key === '?') {
        $('#help-dialog').showModal();
      }
    });

    const overlay = $('#drop-overlay');
    let dragDepth = 0;
    window.addEventListener('dragenter', (e) => {
      if (![...e.dataTransfer.types].includes('Files')) return;
      dragDepth++;
      overlay.hidden = false;
    });
    window.addEventListener('dragleave', () => {
      dragDepth = Math.max(0, dragDepth - 1);
      if (!dragDepth) overlay.hidden = true;
    });
    window.addEventListener('dragover', (e) => e.preventDefault());
    window.addEventListener('drop', (e) => {
      e.preventDefault();
      dragDepth = 0;
      overlay.hidden = true;
      if (e.dataTransfer.files.length) loadFileList(e.dataTransfer.files);
    });
    new ResizeObserver(() => requestRender()).observe(stageEl);
  }

  function init() {
    buildControls();
    bindChrome();
    bindPlotEvents();
    let restored = false;
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) {
      try {
        importProject(Object.assign({ app: 'polylines-sankey' }, JSON.parse(saved)));
        restored = true;
      } catch (err) {
        notify('The autosaved session could not be restored; loading an example instead.');
      }
    }
    if (restored) update({ commit: true });
    else loadExample(window.SK.examples[0].id);
  }

  window.SankeyApp = {
    get state() { return state; },
    get layout() { return layout; },
    loadExample,
    update,
    applyImport,
    exportSvgText,
    bundleFiles,
    locCsv
  };
  init();
})();
