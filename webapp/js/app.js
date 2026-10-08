/* Interactive Sankey app: state, controls, file loading, rendering, interaction and export. */
(function () {
  'use strict';
  const { layout: L, color: C, csv: CSV, shapes: S, render: R } = window.SK;
  const $ = (selector) => document.querySelector(selector);
  const STORAGE_KEY = 'polylines-sankey-v1';
  const MARGIN = 18;
  const HISTORY_LIMIT = 100;

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
    labelColor: '#1F2937',
    canvasWidth: null,
    canvasHeight: null
  };

  const svg = $('#plot');
  const stageEl = $('#stage');
  const tooltip = $('#tooltip');

  let state = blankState();
  let layout = null;
  let colors = null;
  let frame = null;
  let selection = null; // { kind: 'flow' | 'node', key }
  let hover = null;
  let drag = null;
  let routeScope = 'all';
  let renderQueued = false;
  let saveTimer = null;
  const history = { stack: [], index: -1 };

  function blankState() {
    return {
      version: 1,
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
      manualOrder: { start: {}, end: {} },
      params: Object.assign({}, L.DEFAULT_PARAMS),
      stageParams: [],
      style: Object.assign({}, DEFAULT_STYLE),
      view: null
    };
  }

  // ---------- small helpers ----------

  const round = (v, digits = 6) => Number(Number(v).toPrecision(digits));
  const natural = (a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' });
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const fmtValue = (v) => (Math.abs(v) >= 100 ? v.toFixed(1) : Number(v.toPrecision(4)).toString());
  const isDefaultName = (name) => !name || /^V?\d+$/.test(name);

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

  function showError(message) {
    const el = $('#banner');
    el.hidden = !message;
    el.textContent = message || '';
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

  function model() {
    return {
      mode: state.mode,
      matrices: state.matrices,
      loc: state.loc,
      params: state.params,
      stageParams: state.stageParams,
      manualOrder: state.manualOrder
    };
  }

  function colorData() {
    const locColors = {};
    const locMergeColors = {};
    state.loc.forEach((row) => {
      const key = `${row.set}:${row.node}`;
      if (row.color) locColors[key] = row.color;
      if (row.merge_col) locMergeColors[key] = row.merge_col;
    });
    return {
      flowMatrices: state.colorMatrices,
      midMatrices: state.midMatrices,
      flowOverrides: state.flowOverrides,
      midOverrides: state.midOverrides,
      nodeColors: state.nodeColors,
      mergeColors: state.mergeColors,
      locColors,
      locMergeColors
    };
  }

  function computeFrame() {
    const box = stageEl.getBoundingClientRect();
    const width = state.style.canvasWidth || Math.max(320, Math.floor(box.width));
    const height = state.style.canvasHeight || Math.max(240, Math.floor(box.height));
    frame = { width, height, margin: { left: MARGIN, top: MARGIN, right: MARGIN, bottom: MARGIN } };
    svg.setAttribute('width', width);
    svg.setAttribute('height', height);
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
  }

  const viewFor = (v) => ({ xlim: v.xlim, ylim: v.ylim, width: frame.width - 2 * MARGIN, height: frame.height - 2 * MARGIN });
  const measure = () => R.makeMeasure(state.style);

  // Labels drawn outside node bodies, in plot pixels, for view fitting.
  function labelBoxes(lay) {
    const out = [];
    const m = measure();
    lay.nodes.forEach((n) => {
      const text = n.hasLabel ? n.label : (state.style.showIndices ? String(n.node) : '');
      if (!state.style.labels || !text) return;
      const size = m(text, n.cex, n.font);
      const role = S.nodeRole(n);
      if (n.hasLabel && n.widthPx > 0) {
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

  // Data limits that contain every strand, node body and label (a few refinement passes,
  // because strand thickness in pixels depends on the limits themselves).
  function fitView() {
    if (!state.loc.length) throw new L.SankeyError('No node locations (loc) are loaded.');
    const xs = state.loc.map((r) => r.x);
    const ys = state.loc.map((r) => r.y);
    const spanX = Math.max(...xs) - Math.min(...xs) || 1;
    const spanY = Math.max(...ys) - Math.min(...ys) || 1;
    let view = {
      xlim: [Math.min(...xs) - 0.1 * spanX, Math.max(...xs) + 0.1 * spanX],
      ylim: [Math.min(...ys) - 0.15 * spanY, Math.max(...ys) + 0.15 * spanY]
    };
    for (let pass = 0; pass < 4; pass++) {
      const lay = L.computeLayout(model(), viewFor(view), { measure: measure() });
      const box = S.geometryBounds(lay);
      labelBoxes(lay).forEach((p) => {
        box.x0 = Math.min(box.x0, p[0]);
        box.x1 = Math.max(box.x1, p[0]);
        box.y0 = Math.min(box.y0, p[1]);
        box.y1 = Math.max(box.y1, p[1]);
      });
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
    computeFrame();
    try {
      if (!state.matrices.length || !state.loc.length) {
        layout = null;
        svg.innerHTML = '';
        showError(state.matrices.length ? 'Load node locations (loc) or use "Auto-layout nodes".' : '');
      } else {
        if (opts.fit || !state.view) state.view = fitView();
        layout = L.computeLayout(model(), viewFor(state.view), { measure: measure() });
        colors = R.resolveColors(layout, colorData(), state.style);
        svg.innerHTML = R.render(layout, colors, state.style, frame);
        showError(null);
        showWarnings(layout.warnings);
      }
    } catch (err) {
      layout = null;
      svg.innerHTML = '';
      showError(err instanceof L.SankeyError ? err.message : `Unexpected error: ${err.message}`);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
    if (selection && layout && !findSelected()) selection = null;
    // Hover indices refer to the previous layout; drop them when that strand/node is gone.
    if (hover && !(layout && (hover.kind === 'flow' ? layout.flows : layout.nodes)[hover.index])) {
      hover = null;
      tooltip.hidden = true;
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
        { key: 'labelColor', target: 'style', type: 'color', label: 'Label color' },
        { key: 'showPoints', target: 'style', type: 'checkbox', label: 'Node points' },
        { key: 'nodeSums', target: 'style', type: 'checkbox', label: 'Node total bars' }
      ]
    },
    {
      title: 'Canvas',
      items: [
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
    return state.style[item.key];
  }

  function setValue(item, value) {
    if (item.target === 'params') {
      state.params[item.key] = value;
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
      details.open = section.title !== 'Canvas';
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
      if (section.title === 'Canvas') {
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
        if (input.value === '' || !Number.isFinite(v) || (item.min !== undefined && v < item.min)) {
          showError(`${item.label} must be a number${item.min !== undefined ? ` ≥ ${item.min}` : ''}.`);
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
      `<button data-move="${i}" data-dir="-1" title="Move up"${i === 0 ? ' disabled' : ''}>↑</button>` +
      `<button data-move="${i}" data-dir="1" title="Move down"${i === names.length - 1 ? ' disabled' : ''}>↓</button>` +
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
    fileList($('#stage-list'), state.stageNames, (i, d) => {
      if (i + d < 0 || i + d >= state.matrices.length) return;
      stageArrays().forEach((arr) => { [arr[i], arr[i + d]] = [arr[i + d], arr[i]]; });
      clearKeyedEdits();
      update({ fit: true, commit: true });
    }, (i) => {
      stageArrays().forEach((arr) => { if (i < arr.length) arr.splice(i, 1); });
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
  function clearKeyedEdits() {
    state.flowOverrides = {};
    state.midOverrides = {};
    state.manualOrder = { start: {}, end: {} };
    selection = null;
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
    const numeric = (() => {
      try { CSV.parseMatrix(file.text, 'number', file.name); return true; } catch (e) { return false; }
    })();
    if (numeric) return 'stages';
    const colorsOk = (() => {
      try { CSV.parseMatrix(file.text, 'color', file.name); return true; } catch (e) { return false; }
    })();
    if (colorsOk) return /mid/i.test(file.name) ? 'mids' : 'colors';
    throw new L.SankeyError(`${file.name}: not recognised as a flow matrix, loc table, color matrix or project file.`);
  }

  async function readFiles(fileList) {
    const files = await Promise.all([...fileList].map((f) => f.text().then((text) => ({ name: f.name, text }))));
    return files.sort((a, b) => natural(a.name, b.name));
  }

  // files: [{ name, text }]; role forces every file into one role.
  function applyFiles(files, role) {
    const groups = { stages: [], loc: [], colors: [], mids: [], project: [] };
    files.forEach((f) => groups[role || classify(f)].push(f));
    if (groups.project.length) {
      if (files.length > 1) throw new L.SankeyError('Load a project JSON file on its own.');
      let parsed;
      try {
        parsed = JSON.parse(groups.project[0].text);
      } catch (err) {
        throw new L.SankeyError(`${groups.project[0].name}: not valid JSON (${err.message}).`);
      }
      importProject(parsed);
      return;
    }
    applyGroups(groups, {});
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

    if (stages.length) {
      state.matrices = stages.map((m) => m.values);
      state.stageNames = groups.stages.map((f) => f.name);
      state.dimnames = stages.map((m) => ({ rows: m.rowNames, cols: m.colNames }));
      clearKeyedEdits();
      state.stageParams = [];
    }
    if (loc) {
      state.loc = loc;
      state.locName = groups.loc[0].name;
      state.nodeColors = {};
      state.mergeColors = {};
    }
    if (opts.mode) {
      state.mode = opts.mode;
    } else if (stages.length || loc) {
      // A single square matrix whose size equals the loc row count can only be a graph.
      const m = state.matrices;
      const squareGraph = m.length === 1 && m[0].length === m[0][0].length && state.loc.length === m[0].length;
      state.mode = squareGraph ? 'graph' : 'layered';
    }
    // Color matrices belong to specific flow matrices; new flows invalidate unpaired ones.
    if (stages.length && !colorMats.length && state.colorMatrices) {
      state.colorMatrices = null;
      state.colorNames = [];
      if (state.style.colorBy === 'file') state.style.colorBy = 'source';
      notify('Strand color files were cleared because new flow matrices were loaded.');
    }
    if (stages.length && !midMats.length) {
      state.midMatrices = null;
      state.midNames = [];
    }
    const checkShapes = (mats, names, what) => mats.forEach((m, s) => {
      const flow = state.matrices[s];
      if (!flow || flow.length !== m.values.length || flow[0].length !== m.values[0].length) {
        const shape = flow ? `${flow.length}×${flow[0].length}` : 'missing';
        throw new L.SankeyError(`${names[s]}: ${what} matrix is ${m.values.length}×${m.values[0].length}, ` +
          `but flow matrix ${s + 1} is ${shape}.`);
      }
    });
    if (colorMats.length) {
      checkShapes(colorMats, groups.colors.map((f) => f.name), 'color');
      state.colorMatrices = colorMats.map((m) => m.values);
      state.colorNames = groups.colors.map((f) => f.name);
      state.style.colorBy = 'file';
    }
    if (midMats.length) {
      checkShapes(midMats, groups.mids.map((f) => f.name), 'mid-color');
      state.midMatrices = midMats.map((m) => m.values);
      state.midNames = groups.mids.map((f) => f.name);
      state.style.useMid = true;
    }
    if (stages.length && !loc) {
      const fits = state.loc.length && (() => {
        try { L.computeLayout(model(), { xlim: [0, 1], ylim: [0, 1], width: 1000, height: 1000 }); return true; } catch (e) { return false; }
      })();
      if (!fits) {
        autoLocate();
        notify('Generated node locations because no matching loc file was loaded.');
      }
    }
    state.view = null;
  }

  async function loadFileList(fileList, role) {
    try {
      applyFiles(await readFiles(fileList), role);
      update({ fit: true, commit: true });
    } catch (err) {
      showError(err.message);
      if (!(err instanceof L.SankeyError)) console.error(err);
    }
  }

  function loadExample(id) {
    const ex = window.SK.examples.find((e) => e.id === id);
    if (!ex) throw new L.SankeyError(`Unknown example "${id}".`);
    state = blankState();
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

  // Default loc: node sets in evenly spaced columns, stacked to fill most of the height.
  function autoLocate() {
    const mats = state.matrices;
    if (!mats.length) throw new L.SankeyError('Load flow matrices before generating node locations.');
    let sets;
    if (state.mode === 'graph') {
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
      const names = state.dimnames[0] && state.dimnames[0].rows;
      sets = [];
      for (let i = 0; i < n; i++) {
        const total = Math.max(m[i].reduce((s, v) => s + v, 0), m.reduce((s, r) => s + r[i], 0));
        (sets[depth[i]] = sets[depth[i]] || []).push({ height: total, label: names && !isDefaultName(names[i]) ? names[i] : '', id: i });
      }
      sets = sets.filter(Boolean);
    } else {
      const counts = [mats[0].length, ...mats.map((m) => m[0].length)];
      sets = counts.map((count, s) => Array.from({ length: count }, (_, k) => {
        const incoming = s > 0 ? mats[s - 1].reduce((sum, r) => sum + r[k], 0) : 0;
        const outgoing = s < mats.length ? mats[s][k].reduce((sum, v) => sum + v, 0) : 0;
        const names = s > 0 ? state.dimnames[s - 1] && state.dimnames[s - 1].cols : state.dimnames[0] && state.dimnames[0].rows;
        return { height: Math.max(incoming, outgoing), label: names && !isDefaultName(names[k]) ? names[k] : '' };
      }));
    }
    computeFrame();
    const W = frame.width - 2 * MARGIN;
    const H = frame.height - 2 * MARGIN;
    const K = Math.max(sets.length - 1, 1);
    const tallest = Math.max(...sets.map((g) => g.reduce((s, n) => s + n.height, 0)));
    const spacing = tallest * W / (0.6 * H * K * 1.15);
    const pxPerUnit = W / (K * spacing * 1.15);
    const rowsByGraphId = [];
    const loc = [];
    sets.forEach((group, s) => {
      const heights = group.map((n) => n.height * pxPerUnit);
      const used = heights.reduce((a, b) => a + b, 0);
      const gap = group.length > 1 ? Math.max((0.8 * H - used) / (group.length - 1), 4) : 0;
      let top = H / 2 + (used + gap * (group.length - 1)) / 2;
      group.forEach((n, k) => {
        const Y = top - heights[k] / 2;
        top -= heights[k] + gap;
        const row = { set: s + 1, node: k + 1, x: round(s * spacing), y: round(Y / H) };
        if (n.label) row.label = n.label;
        if (n.id !== undefined) rowsByGraphId[n.id] = row;
        else loc.push(row);
      });
    });
    state.loc = state.mode === 'graph' ? rowsByGraphId : loc;
    state.locName = 'auto-generated';
    state.view = null;
  }

  // ---------- interaction ----------

  function findSelected() {
    if (!selection || !layout) return null;
    if (selection.kind === 'flow') return layout.flows.find((f) => f.key === selection.key) || null;
    return layout.nodes.find((n) => n.key === selection.key) || null;
  }

  function nodeDisplayName(n) {
    if (n.hasLabel) return n.label;
    const dn = state.dimnames;
    let name = null;
    if (state.mode === 'graph') {
      name = dn[0] && dn[0].rows && dn[0].rows[n.id];
    } else if (n.set === 1) {
      name = dn[0] && dn[0].rows && dn[0].rows[n.node - 1];
    } else {
      name = dn[n.set - 2] && dn[n.set - 2].cols && dn[n.set - 2].cols[n.node - 1];
    }
    return !isDefaultName(name) ? name : `Set ${n.set} · node ${n.node}`;
  }

  function targetOf(event) {
    const el = event.target.closest && event.target.closest('[data-flow], [data-node]');
    if (!el || !layout) return null;
    if (el.dataset.flow !== undefined) return { kind: 'flow', index: Number(el.dataset.flow) };
    return { kind: 'node', index: Number(el.dataset.node) };
  }

  function highlightSets(target) {
    const flows = new Set();
    const nodes = new Set();
    if (!target || !layout) return { flows, nodes };
    if (target.kind === 'flow') {
      const f = layout.flows[target.index];
      flows.add(f.index);
      nodes.add(f.from);
      nodes.add(f.to);
    } else {
      const n = layout.nodes[target.index];
      nodes.add(n.id);
      n.inFlows.concat(n.outFlows).forEach((k) => flows.add(k));
    }
    return { flows, nodes };
  }

  function selectionTarget() {
    const sel = findSelected();
    if (!sel) return null;
    return selection.kind === 'flow' ? { kind: 'flow', index: sel.index } : { kind: 'node', index: sel.id };
  }

  function applyHighlight() {
    const active = (drag && drag.target) || hover;
    const sets = highlightSets(active);
    svg.classList.toggle('dimmed', !!active);
    svg.querySelectorAll('[data-flow]').forEach((el) => el.classList.toggle('hl', sets.flows.has(Number(el.dataset.flow))));
    svg.querySelectorAll('[data-node]').forEach((el) => el.classList.toggle('hl', sets.nodes.has(Number(el.dataset.node))));
    const sel = selectionTarget();
    svg.querySelectorAll('.selected').forEach((el) => el.classList.remove('selected'));
    if (sel) {
      const attr = sel.kind === 'flow' ? 'data-flow' : 'data-node';
      svg.querySelectorAll(`.ribbon[${attr}="${sel.index}"], .trunk[${attr}="${sel.index}"], .node-dot[${attr}="${sel.index}"]`)
        .forEach((el) => el.classList.add('selected'));
    }
  }

  function tooltipHtml(target) {
    if (target.kind === 'flow') {
      const f = layout.flows[target.index];
      const a = layout.nodes[f.from];
      const b = layout.nodes[f.to];
      const share = (v, total) => (total > 0 ? `${(100 * v / total).toFixed(1)}%` : '–');
      return `<b>${esc(nodeDisplayName(a))} → ${esc(nodeDisplayName(b))}</b>` +
        `<div>value <b>${fmtValue(f.value)}</b></div>` +
        `<div>${share(f.value, a.outValue)} of source outflow · ${share(f.value, b.inValue)} of target inflow</div>` +
        `<div class="muted">${state.mode === 'graph' ? 'graph' : `stage ${f.stage + 1}`} · matrix [${f.i + 1}, ${f.j + 1}]</div>`;
    }
    const n = layout.nodes[target.index];
    const balance = n.inValue > 0 && n.outValue > 0 && Math.abs(n.inValue - n.outValue) > 1e-9
      ? `<div class="warn-text">in − out = ${fmtValue(n.inValue - n.outValue)}</div>` : '';
    return `<b>${esc(nodeDisplayName(n))}</b>` +
      `<div>in <b>${fmtValue(n.inValue)}</b> (${n.inFlows.length}) · out <b>${fmtValue(n.outValue)}</b> (${n.outFlows.length})</div>` +
      balance + `<div class="muted">set ${n.set} · node ${n.node} · x ${round(n.x, 4)}, y ${round(n.y, 4)}</div>`;
  }

  function moveTooltip(event) {
    const box = stageEl.getBoundingClientRect();
    let x = event.clientX - box.left + stageEl.scrollLeft + 14;
    let y = event.clientY - box.top + stageEl.scrollTop + 14;
    const w = tooltip.offsetWidth;
    const h = tooltip.offsetHeight;
    if (x + w > stageEl.scrollLeft + box.width - 6) x -= w + 28;
    if (y + h > stageEl.scrollTop + box.height - 6) y -= h + 28;
    tooltip.style.left = `${x}px`;
    tooltip.style.top = `${y}px`;
  }

  function setHover(target, event) {
    const same = target && hover && target.kind === hover.kind && target.index === hover.index;
    hover = target;
    if (!same) applyHighlight();
    if (target) {
      if (!same) tooltip.innerHTML = tooltipHtml(target);
      tooltip.hidden = false;
      moveTooltip(event);
    } else {
      tooltip.hidden = true;
    }
  }

  const plotPoint = (event) => {
    const box = svg.getBoundingClientRect();
    return {
      X: event.clientX - box.left - MARGIN,
      Y: (frame.height - MARGIN) - (event.clientY - box.top)
    };
  };

  function startDrag(event) {
    if (event.button !== 0) return;
    const target = targetOf(event);
    if (!target) {
      if (selection) {
        selection = null;
        applyHighlight();
        refreshInspector();
      }
      return;
    }
    event.preventDefault();
    svg.setPointerCapture(event.pointerId);
    tooltip.hidden = true;
    const base = { target, x0: event.clientX, y0: event.clientY, moved: false };
    if (target.kind === 'node') {
      const n = layout.nodes[target.index];
      drag = Object.assign(base, { row: n.id, locX: state.loc[n.id].x, locY: state.loc[n.id].y, sx: layout.tf.sx, sy: layout.tf.sy });
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
      const row = state.loc[drag.row];
      row.x = event.shiftKey ? drag.locX : round(drag.locX + dx / drag.sx);
      row.y = event.altKey ? drag.locY : round(drag.locY - dy / drag.sy);
      requestRender();
      return;
    }
    if (drag.others.length === 0) return;
    const goal = drag.center - dy;
    const index = drag.others.filter((o) => o.center < goal).length;
    const order = drag.others.map((o) => o.key);
    order.splice(index, 0, drag.flowKey);
    const current = state.manualOrder[drag.side][drag.nodeKey];
    if (!current || current.join('|') !== order.join('|')) {
      state.manualOrder[drag.side] = Object.assign({}, state.manualOrder[drag.side], { [drag.nodeKey]: order });
      requestRender();
    }
  }

  function endDrag(event) {
    const finished = drag;
    drag = null;
    svg.classList.remove('dragging');
    if (svg.hasPointerCapture(event.pointerId)) svg.releasePointerCapture(event.pointerId);
    if (!finished.moved) {
      const t = finished.target;
      selection = t.kind === 'flow'
        ? { kind: 'flow', key: layout.flows[t.index].key }
        : { kind: 'node', key: layout.nodes[t.index].key };
      applyHighlight();
      refreshInspector();
      return;
    }
    update({ commit: true });
  }

  function bindPlotEvents() {
    svg.addEventListener('pointerdown', startDrag);
    svg.addEventListener('pointermove', (event) => {
      if (drag) {
        moveDrag(event);
        return;
      }
      setHover(targetOf(event), event);
    });
    svg.addEventListener('pointerup', (event) => { if (drag) endDrag(event); });
    svg.addEventListener('pointercancel', (event) => { if (drag) endDrag(event); });
    svg.addEventListener('pointerleave', () => { if (!drag) setHover(null); });
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

  function refreshInspector() {
    const host = $('#inspector');
    if (drag) return;
    const sel = findSelected();
    if (!sel) {
      host.innerHTML = '<h2>Inspector</h2><p class="muted">Hover a strand or node for details; click to select and edit it.</p>' +
        '<ul class="tips"><li>Drag a node to move it (Shift: vertical only, Alt: horizontal only).</li>' +
        '<li>Drag a strand up or down to reorder it in the stack at its nearer end.</li>' +
        '<li>Ctrl/⌘+Z undo, Ctrl/⌘+Shift+Z redo, F fit view, Esc deselect.</li></ul>';
      return;
    }
    if (selection.kind === 'flow') {
      const f = sel;
      const a = layout.nodes[f.from];
      const b = layout.nodes[f.to];
      const mid = colors.mid[f.index];
      const midSet = Object.prototype.hasOwnProperty.call(state.midOverrides, f.key);
      host.innerHTML = `<h2>Strand</h2><p><b>${esc(nodeDisplayName(a))}</b> → <b>${esc(nodeDisplayName(b))}</b></p>` +
        `<p class="muted">${state.mode === 'graph' ? 'graph' : `stage ${f.stage + 1}`} · matrix [${f.i + 1}, ${f.j + 1}]</p>` +
        `<div class="row"><label for="ins-value">Value</label><input id="ins-value" type="number" min="0" step="any" value="${f.value}"></div>` +
        colorEditor('flow', 'Color', colors.flow[f.index], !!state.flowOverrides[f.key]) +
        `<div class="row"><label for="ins-mid-on">Mid color</label><input id="ins-mid-on" type="checkbox"${mid.color ? ' checked' : ''}></div>` +
        (mid.color ? colorEditor('mid', '', mid.color, midSet) : '') +
        (state.style.gradient ? '' : '<p class="muted small">Mid colors show when gradients are on.</p>') +
        '<button class="mini-btn wide" id="ins-reset-order">Reset stack order at both ends</button>';
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
    host.innerHTML = `<h2>Node</h2><p><b>${esc(nodeDisplayName(n))}</b></p>` +
      `<p class="muted">set ${n.set} · node ${n.node} · in ${fmtValue(n.inValue)} · out ${fmtValue(n.outValue)}</p>` +
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
    clone.querySelectorAll('.layer-hits').forEach((el) => el.remove());
    clone.removeAttribute('class');
    clone.removeAttribute('id');
    clone.querySelectorAll('[class]').forEach((el) => {
      el.classList.remove('hl', 'selected');
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
  function locCsv() {
    const rows = state.loc;
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
      if (anyWidth) line.push(Number.isFinite(r.width) ? csvNumber(r.width) : csvNumber(node ? node.widthPx / layout.tf.sx : 0));
      if (anyLabel) line.push(r.label || '', csvNumber(r.cex) || 1, r.font || 1);
      if (anyHeight) line.push(csvNumber(r.height));
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
    const lines = [
      '# Generated by the polylines Sankey webapp.',
      '# Run next to the exported CSV files, with R/functions.R from the polylines repository.',
      'source("R/functions.R")',
      '',
      'read_numbers <- function(f) unname(as.matrix(read.csv(f, header = FALSE)))',
      'read_colors <- function(f) unname(as.matrix(read.csv(f, header = FALSE, colClasses = "character")))',
      graph ? 'thic <- read_numbers("adjacency.csv")' : `thic <- lapply(c(${files.stages.map(rString).join(', ')}), read_numbers)`,
      'loc <- read.csv("loc.csv", stringsAsFactors = FALSE)',
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
    const plotH = frame.height - 2 * MARGIN;
    lines.push('', `png("sankey.png", width = ${frame.width}, height = ${frame.height}, res = 96, bg = ${rString(C.toHex(colors.flatBg))})`,
      `par(mai = rep(${round(MARGIN / 96, 4)}, 4), xpd = NA, ps = ${round(style.fontSize * 0.75, 4)})`,
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
    if (style.labels && state.loc.some((r) => r.label)) {
      lines.push('labeled <- sankey$nodes[sankey$nodes$has_label, ]',
        `text(labeled$x, labeled$y, labeled$label, cex = labeled$cex, font = labeled$font, col = ${rString(C.toHex(C.parse(style.labelColor)))})`);
    }
    if (style.showPoints) lines.push('points(sankey$nodes$x, sankey$nodes$y, pch = 21, bg = "white", lwd = 2)');
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
    out.push({ name: 'loc.csv', text: locCsv() });
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
    next.style = Object.assign({}, DEFAULT_STYLE, obj.style);
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
    hover = null;
    tooltip.hidden = true;
    update();
  }

  function scheduleSave() {
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => {
      try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
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
      clearKeyedEdits();
      update({ fit: true, commit: true });
    });
    $('#auto-layout').addEventListener('click', () => {
      try {
        autoLocate();
        update({ fit: true, commit: true });
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
    const menu = $('#export-menu');
    $('#export-btn').addEventListener('click', (e) => {
      e.stopPropagation();
      menu.hidden = !menu.hidden;
    });
    document.addEventListener('click', () => { menu.hidden = true; });
    menu.querySelectorAll('[data-export]').forEach((b) => b.addEventListener('click', () => runExport(b.dataset.export)));
    $('#help-btn').addEventListener('click', () => $('#help-dialog').showModal());
    $('#copy-code').addEventListener('click', () => {
      navigator.clipboard.writeText($('#code-text').value).then(() => notify('R code copied.'), () => {
        $('#code-text').select();
        notify('Select-all done; press Ctrl/⌘+C to copy.');
      });
    });

    document.addEventListener('keydown', (e) => {
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
      } else if (e.key === 'Escape' && selection) {
        selection = null;
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
    exportSvgText,
    bundleFiles,
    locCsv
  };
  init();
})();
