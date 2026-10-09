/* Retained table inputs, diagram column controls, row filters and subset captions. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const T = SK.transform;
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const displayValue = (v) => v === '' ? '(blank)' : v;

  function create(input, options) {
    const source = JSON.parse(JSON.stringify(input));
    const available = T.diagramColumns(source.table, source.spec);
    const enabled = source.spec.format === 'wide' ? source.spec.columns : (source.spec.stages || available);
    return Object.assign(source, options, { order: enabled.concat(available.filter((c) => !enabled.includes(c))), enabled: enabled.slice(), filters: {} });
  }

  function build(source) {
    const table = T.filterTable(source.table, source.filters);
    const columns = source.order.filter((c) => source.enabled.includes(c));
    const spec = Object.assign({}, source.spec, source.spec.format === 'wide' ? { columns } : { stages: columns });
    const result = T.build(table, spec, source.options);
    return { result, source: Object.assign({}, source, { summary: Object.assign({ rowsFiltered: table.rows.length, unit: result.unit }, result.summary) }) };
  }

  function header(source, width) {
    const conditions = source ? Object.entries(source.filters) : [];
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
      '<ol class="stage-picker table-column-picker">' + source.order.map((c, i) => `<li data-table-column="${esc(c)}" class="${enabled.has(c) ? 'on' : ''}">` +
        `<label><input type="checkbox" data-enable-column="${esc(c)}"${enabled.has(c) ? ' checked' : ''}${enabled.has(c) && enabled.size <= 2 ? ' disabled' : ''}> ${esc(c)}</label>` +
        (enabled.has(c) ? `<button type="button" data-column-move="${i}" data-dir="-1" aria-label="Move ${esc(c)} earlier"${!source.order.slice(0, i).some((v) => enabled.has(v)) ? ' disabled' : ''}>↑</button>` +
        `<button type="button" data-column-move="${i}" data-dir="1" aria-label="Move ${esc(c)} later"${!source.order.slice(i + 1).some((v) => enabled.has(v)) ? ' disabled' : ''}>↓</button>` : '') + '</li>').join('') + '</ol>' +
      '<div class="table-control-title">Keep rows with these values</div>' + source.table.columns.map((name) => {
        const kept = source.filters[name];
        return `<details class="table-filter" data-filter-column="${esc(name)}"${open.has(name) ? ' open' : ''}><summary>${esc(name)} ` +
          `<span class="muted">${kept ? `${kept.length} kept` : 'all'}</span></summary><div class="filter-body"></div></details>`;
      }).join('') + '<button type="button" class="mini-btn wide" data-clear-filters>Reset row filters</button>' +
      '<p class="file-note small">Changes rebuild and fit the diagram, clearing manual node and strand edits. Keep at least two diagram columns.</p>';
    host.querySelector('[data-clear-filters]').disabled = !Object.keys(source.filters).length;
    host.querySelector('[data-clear-filters]').addEventListener('click', () => onChange(Object.assign({}, source, { filters: {} })));
    host.querySelectorAll('[data-enable-column]').forEach((box) => box.addEventListener('change', () => {
      const name = box.dataset.enableColumn;
      onChange(Object.assign({}, source, { enabled: box.checked ? source.enabled.concat(name) : source.enabled.filter((c) => c !== name) }));
    }));
    host.querySelectorAll('[data-column-move]').forEach((button) => button.addEventListener('click', () => {
      const i = Number(button.dataset.columnMove);
      const dir = Number(button.dataset.dir);
      let j = i + dir;
      while (!enabled.has(source.order[j])) j += dir;
      const order = source.order.slice();
      [order[i], order[j]] = [order[j], order[i]];
      onChange(Object.assign({}, source, { order }));
    }));
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
          onChange(Object.assign({}, source, { filters }));
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

  SK.tableControls = { create, build, header, render };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
