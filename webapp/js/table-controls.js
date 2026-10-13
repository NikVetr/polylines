/* Retained table inputs, diagram column and value choices, row filters and subset captions. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const T = SK.transform;
  const fail = (message) => { throw new SK.layout.SankeyError(message); };
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const displayValue = (v) => v === '' ? '(blank)' : v;

  // order: every diagram column (wide: source columns; long: stages), shown or not; enabled: those
  // in the diagram; filters: kept values per source column; stageFilters (long): kept categories
  // per stage.
  function create(input, options) {
    const source = JSON.parse(JSON.stringify(input));
    const available = T.diagramColumns(source.table, source.spec);
    const enabled = source.spec.format === 'wide' ? source.spec.columns : (source.spec.stages || available);
    return Object.assign(source, options, { order: enabled.concat(available.filter((c) => !enabled.includes(c))), enabled: enabled.slice(),
      filters: {}, stageFilters: {}, autoHidden: [] });
  }

  const columnLabel = (source, column) => (source.spec.format === 'long' ? T.stageLabel(source.spec, column) : column);

  // A diagram column's values (wide: the column's; long: the categories at that stage) and those kept.
  function valuesOf(source, column) {
    const long = source.spec.format === 'long';
    const values = long ? T.stageValues(source.table, source.spec, column) : T.columnValues(source.table, column);
    const kept = (long ? source.stageFilters || {} : source.filters)[column];
    return { values, kept: new Set(kept || values) };
  }

  // The source with one value of a diagram column dropped, or kept again.
  function withValue(source, column, value) {
    const { values, kept } = valuesOf(source, column);
    if (!values.includes(value)) fail(`"${value}" is not a value of ${columnLabel(source, column)}.`);
    const next = values.filter((v) => (v === value ? !kept.has(v) : kept.has(v)));
    if (!next.length) fail(`Keep at least one value of ${columnLabel(source, column)}.`);
    const key = source.spec.format === 'long' ? 'stageFilters' : 'filters';
    const filters = Object.assign({}, source[key] || {});
    if (next.length === values.length) delete filters[column];
    else filters[column] = next;
    return key === 'filters' ? withFilters(source, filters) : Object.assign({}, source, { stageFilters: filters });
  }

  function withColumn(source, column, include) {
    if (!source.order.includes(column)) fail(`"${column}" is not a diagram column.`);
    const enabled = include ? source.order.filter((c) => c === column || source.enabled.includes(c)) : source.enabled.filter((c) => c !== column);
    if (enabled.length < 2) fail('Keep at least two diagram columns.');
    return Object.assign({}, source, { enabled, autoHidden: (source.autoHidden || []).filter((c) => c !== column) });
  }

  // The source with a column moved to an index of the order without it.
  function withMove(source, column, index) {
    const order = source.order.filter((c) => c !== column);
    if (order.length === source.order.length) fail(`"${column}" is not a diagram column.`);
    order.splice(Math.max(0, Math.min(index, order.length)), 0, column);
    return Object.assign({}, source, { order });
  }

  function withFilters(source, filters) {
    let enabled = source.enabled.slice();
    let autoHidden = (source.autoHidden || []).slice();
    if (source.spec.format === 'wide') {
      autoHidden.slice().forEach((c) => {
        if (!filters[c] || filters[c].length !== 1) {
          enabled.push(c);
          autoHidden = autoHidden.filter((v) => v !== c);
        }
      });
      source.order.forEach((c) => {
        if (filters[c] && filters[c].length === 1 && (!source.filters[c] || source.filters[c].length !== 1) &&
            enabled.includes(c) && enabled.length > 2) {
          enabled = enabled.filter((v) => v !== c);
          autoHidden.push(c);
        }
      });
    }
    return Object.assign({}, source, { filters, enabled, autoHidden });
  }

  function build(source) {
    const table = T.filterTable(source.table, source.filters, source.spec, source.stageFilters);
    const columns = source.order.filter((c) => source.enabled.includes(c));
    const spec = Object.assign({}, source.spec, source.spec.format === 'wide' ? { columns } : { stages: columns });
    const result = T.build(table, spec, source.options);
    return { result, source: Object.assign({}, source, { summary: Object.assign({ rowsFiltered: table.rows.length, unit: result.unit }, result.summary) }) };
  }

  function header(source, width) {
    const conditions = source ? Object.entries(source.filters).concat(Object.entries(source.stageFilters || {})
      .map(([stage, kept]) => [`${source.spec.category} at ${columnLabel(source, stage)}`, kept])) : [];
    if (!conditions.length) return { height: 0, lines: [], markup: '' };
    const measure = SK.render.makeMeasure({ fontSize: 12 });
    const lines = [[]];
    let used = 0;
    const add = (text, bold = false) => {
      // Split long values as well as wrapping between values, so the caption stays inside the SVG.
      let part = '';
      for (const char of text) {
        if (measure(part + char, 1, bold ? 2 : 1).width > width && part) {
          add(part, bold);
          part = '';
        }
        part += char;
      }
      const size = measure(part, 1, bold ? 2 : 1).width;
      if (used + size > width && lines[lines.length - 1].length) { lines.push([]); used = 0; }
      lines[lines.length - 1].push({ text: part, bold });
      used += size;
    };
    conditions.forEach(([name, values], i) => {
      if (i) add('   ·   ');
      add(name, true);
      add(' ∈ {');
      values.forEach((v, j) => add(`${j ? ', ' : ''}${displayValue(v)}`));
      add('}');
    });
    return { height: lines.length * 17 + 10, lines, markup: `<g class="layer-subset" pointer-events="none" font-family="${SK.render.FONT_FAMILY}" font-size="12" fill="#1F2937">` +
      lines.map((line, i) => `<text x="18" y="${18 + i * 17}" dominant-baseline="central">` +
        line.map((t) => `<tspan${t.bold ? ' font-weight="600"' : ''}>${esc(t.text)}</tspan>`).join('') + '</text>').join('') + '</g>' };
  }

  let renderedSource;
  function render(host, source, onChange, force = false) {
    if (!force && renderedSource === source) return;
    renderedSource = source;
    host.hidden = !source;
    if (!source) { host.innerHTML = ''; return; }
    const open = new Set([...host.querySelectorAll('[data-filter-column][open]')].map((el) => el.dataset.filterColumn));
    const searches = new Map([...host.querySelectorAll('[data-filter-search]')].map((el) => [el.dataset.filterSearch, el.value]));
    const enabled = new Set(source.enabled);
    const count = source.summary ? source.summary.rowsFiltered : source.table.rows.length;
    const total = source.table.rows.length;
    const plotted = source.summary ? ` · ${source.summary.total.toLocaleString()} ${esc(source.summary.unit)} plotted` : '';
    const dropped = source.summary && source.summary.rowsDropped ? ` · ${source.summary.rowsDropped.toLocaleString()} incomplete records omitted` : '';
    host.innerHTML = `<div class="file-head">Imported table</div><div class="file-note">${esc(source.name)} · ${count.toLocaleString()} / ${total.toLocaleString()} source rows match${plotted}${dropped}</div>` +
      `<div class="table-control-title">${source.spec.format === 'long' ? 'Diagram stages' : 'Diagram columns'}</div>` +
      '<ol class="stage-picker table-column-picker">' + source.order.map((c) => `<li data-table-column="${esc(c)}" class="${enabled.has(c) ? 'on' : ''}" draggable="true" tabindex="0" title="Drag to reorder; Alt+↑/↓ for keyboard ordering">` +
        '<span class="column-grip" aria-hidden="true">⠿</span>' +
        `<label><input type="checkbox" data-enable-column="${esc(c)}"${enabled.has(c) ? ' checked' : ''}${enabled.has(c) && enabled.size <= 2 ? ' disabled' : ''}> ${esc(c)}</label>` +
        '</li>').join('') + '</ol>' +
      '<div class="table-control-title">Keep rows with these values</div>' + source.table.columns.map((name) => {
        const kept = source.filters[name];
        return `<details class="table-filter" data-filter-column="${esc(name)}"${open.has(name) ? ' open' : ''}><summary>${esc(name)} ` +
          `<span class="muted">${kept ? `${kept.length} kept` : 'all'}</span></summary><div class="filter-body"></div></details>`;
      }).join('') + '<button type="button" class="mini-btn wide" data-clear-filters>Reset row filters</button>' +
      '<p class="file-note small">' + (source.spec.format === 'wide' ? 'Single-value filters hide their diagram column automatically; expanding the filter restores it. ' : '') +
      'Keep at least two diagram columns. Changes rebuild and fit the diagram, clearing manual node and strand edits.</p>';
    host.querySelector('[data-clear-filters]').disabled = !Object.keys(source.filters).length;
    host.querySelector('[data-clear-filters]').addEventListener('click', () => onChange(withFilters(source, {})));
    host.querySelectorAll('[data-enable-column]').forEach((box) => box.addEventListener('change', () => {
      const name = box.dataset.enableColumn;
      onChange(Object.assign({}, source, { enabled: box.checked ? source.enabled.concat(name) : source.enabled.filter((c) => c !== name),
        autoHidden: (source.autoHidden || []).filter((c) => c !== name) }));
    }));
    const move = (name, destination) => {
      const i = source.order.indexOf(name);
      if (destination === i || destination < 0 || destination >= source.order.length) return;
      const order = source.order.slice();
      order.splice(i, 1);
      order.splice(destination, 0, name);
      onChange(Object.assign({}, source, { order }));
    };
    let dragged = null;
    const clearDrop = () => host.querySelectorAll('.drop-before, .drop-after').forEach((el) => el.classList.remove('drop-before', 'drop-after'));
    host.querySelectorAll('[data-table-column]').forEach((entry) => {
      entry.addEventListener('dragstart', (e) => {
        dragged = entry.dataset.tableColumn;
        e.dataTransfer.effectAllowed = 'move';
        e.dataTransfer.setData('text/plain', dragged);
        entry.classList.add('column-dragging');
      });
      entry.addEventListener('dragover', (e) => {
        if (dragged === null || dragged === entry.dataset.tableColumn) return;
        e.preventDefault();
        e.dataTransfer.dropEffect = 'move';
        clearDrop();
        const box = entry.getBoundingClientRect();
        entry.classList.add(e.clientY < box.top + box.height / 2 ? 'drop-before' : 'drop-after');
      });
      entry.addEventListener('drop', (e) => {
        if (dragged === null) return;
        e.preventDefault();
        const box = entry.getBoundingClientRect();
        const before = e.clientY < box.top + box.height / 2;
        const i = source.order.indexOf(dragged);
        let destination = source.order.indexOf(entry.dataset.tableColumn) + (before ? 0 : 1);
        if (i < destination) destination--;
        clearDrop();
        move(dragged, destination);
      });
      entry.addEventListener('dragend', () => { dragged = null; clearDrop(); entry.classList.remove('column-dragging'); });
      entry.addEventListener('keydown', (e) => {
        if (!e.altKey || !['ArrowUp', 'ArrowDown'].includes(e.key)) return;
        e.preventDefault();
        const name = entry.dataset.tableColumn;
        move(name, source.order.indexOf(name) + (e.key === 'ArrowUp' ? -1 : 1));
        [...host.querySelectorAll('[data-table-column]')].find((el) => el.dataset.tableColumn === name).focus();
      });
    });
    host.querySelectorAll('[data-filter-column]').forEach((details) => {
      const name = details.dataset.filterColumn;
      const populate = () => {
        const body = details.querySelector('.filter-body');
        if (body.childElementCount) return;
        const values = T.columnValues(source.table, name);
        const kept = new Set(source.filters[name] || values);
        body.innerHTML = `<input type="search" data-filter-search="${esc(name)}" aria-label="Find a value in ${esc(name)}" placeholder="Find a value…" value="${esc(searches.get(name) || '')}">` +
          '<button type="button" class="mini-btn" data-keep-all>All values</button><div class="filter-values"></div>';
        const change = (selected) => {
          const filters = { ...source.filters };
          if (selected.length === values.length) delete filters[name];
          else filters[name] = selected;
          onChange(withFilters(source, filters));
        };
        const drawValues = () => {
          const query = body.querySelector('input[type="search"]').value.toLocaleLowerCase();
          const list = body.querySelector('.filter-values');
          const visible = values.filter((v) => displayValue(v).toLocaleLowerCase().includes(query));
          list.innerHTML = visible.map((v) => `<div><label><input type="checkbox" data-filter-value="${esc(v)}"${kept.has(v) ? ' checked' : ''}> ${esc(displayValue(v))}</label>` +
            `<button type="button" data-only-value="${esc(v)}" aria-label="Keep only ${esc(displayValue(v))}">only</button></div>`).join('') || '<p class="muted small">No values match.</p>';
          list.querySelectorAll('[data-filter-value]').forEach((box) => box.addEventListener('change', () => {
            const selected = new Set(kept);
            if (box.checked) selected.add(box.dataset.filterValue);
            else selected.delete(box.dataset.filterValue);
            change(values.filter((v) => selected.has(v)));
          }));
          list.querySelectorAll('[data-only-value]').forEach((button) => button.addEventListener('click', () => change([button.dataset.onlyValue])));
        };
        body.querySelector('[data-keep-all]').addEventListener('click', () => change(values));
        body.querySelector('input[type="search"]').addEventListener('input', drawValues);
        drawValues();
      };
      details.addEventListener('toggle', () => { if (details.open) populate(); });
      if (details.open) populate();
    });
  }

  SK.tableControls = { create, withFilters, withValue, withColumn, withMove, valuesOf, columnLabel, displayValue, esc, build, header, render };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
