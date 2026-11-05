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
  // per stage; valueOrders: a diagram column's values in the order of its nodes from the top;
  // merges: per diagram column, values merged into one node, [{ name, values }].
  function create(input, options) {
    const source = JSON.parse(JSON.stringify(input));
    const available = T.diagramColumns(source.table, source.spec);
    const enabled = source.spec.format === 'wide' ? source.spec.columns : (source.spec.stages || available);
    return Object.assign(source, options, { order: enabled.concat(available.filter((c) => !enabled.includes(c))), enabled: enabled.slice(),
      filters: {}, stageFilters: {}, valueOrders: {}, autoHidden: [] });
  }

  const columnLabel = (source, column) => (source.spec.format === 'long' ? T.stageLabel(source.spec, column) : column);

  // A diagram column's values in the source table (wide: the column's; long: the categories at that stage).
  const rawValues = (source, column) => (source.spec.format === 'long'
    ? T.stageValues(source.table, source.spec, column) : T.columnValues(source.table, column));

  // A diagram column's values as its nodes have them (merged values as one, named for the merge),
  // in their chosen order; those kept (a merged value while any of its members is); and the
  // members of each merged value.
  function valuesOf(source, column) {
    const raw = rawValues(source, column);
    const merges = (source.merges || {})[column] || [];
    const members = new Map(merges.map((m) => [m.name, m.values]));
    const into = new Map(merges.flatMap((m) => m.values.map((v) => [v, m.name])));
    const values = (source.valueOrders || {})[column] || [...new Set(raw.map((v) => into.get(v) || v))];
    const filter = (source.spec.format === 'long' ? source.stageFilters || {} : source.filters)[column];
    const keptRaw = new Set(filter || raw);
    return { values, kept: new Set(values.filter((v) => (members.get(v) || [v]).some((m) => keptRaw.has(m)))), members };
  }

  // The source with one value of a diagram column (all members of a merged one) dropped, or kept
  // again. Unlike a single-value row filter, keeping one value leaves its column in the diagram.
  function withValue(source, column, value) {
    const { values, kept, members } = valuesOf(source, column);
    if (!values.includes(value)) fail(`"${value}" is not a value of ${columnLabel(source, column)}.`);
    const keep = values.filter((v) => (v === value ? !kept.has(v) : kept.has(v)));
    if (!keep.length) fail(`Keep at least one value of ${columnLabel(source, column)}.`);
    // The row filter keeps raw values, in the column's order.
    const next = keep.flatMap((v) => members.get(v) || [v]);
    const key = source.spec.format === 'long' ? 'stageFilters' : 'filters';
    const filters = Object.assign({}, source[key] || {});
    if (next.length === rawValues(source, column).length) delete filters[column];
    else filters[column] = next;
    return Object.assign({}, source, { [key]: filters });
  }

  // The source with values of a diagram column merged into one node named `name` (a merged value
  // brings its members), placed where the first of them is in a chosen order.
  function withMerge(source, column, merged, name) {
    const label = columnLabel(source, column);
    const { values, members } = valuesOf(source, column);
    if (new Set(merged).size < 2 || merged.some((v) => !values.includes(v))) fail(`Merge two or more values of ${label}.`);
    if (values.includes(name) && !merged.includes(name)) fail(`${label} already has a value named "${name}".`);
    const merges = Object.assign({}, source.merges);
    merges[column] = (merges[column] || []).filter((m) => !merged.includes(m.name))
      .concat([{ name, values: merged.flatMap((v) => members.get(v) || [v]) }]);
    const next = Object.assign({}, source, { merges });
    const order = (source.valueOrders || {})[column];
    if (order) {
      const kept = order.filter((v) => !merged.includes(v));
      kept.splice(order.findIndex((v) => merged.includes(v)), 0, name);
      next.valueOrders = Object.assign({}, source.valueOrders, { [column]: kept });
    }
    return next;
  }

  // The source with a merged value of a diagram column split back into its members, in its place
  // in a chosen order.
  function withoutMerge(source, column, name) {
    const merge = ((source.merges || {})[column] || []).find((m) => m.name === name);
    if (!merge) fail(`"${name}" is not a merged value of ${columnLabel(source, column)}.`);
    const merges = Object.assign({}, source.merges, { [column]: source.merges[column].filter((m) => m !== merge) });
    if (!merges[column].length) delete merges[column];
    const next = Object.assign({}, source, { merges });
    const order = (source.valueOrders || {})[column];
    if (order) next.valueOrders = Object.assign({}, source.valueOrders, { [column]: order.flatMap((v) => (v === name ? merge.values : [v])) });
    return next;
  }

  // The filtered table with merged values renamed for their merge.
  function recode(table, source) {
    const merges = Object.entries(source.merges || {}).filter(([, list]) => list.length);
    if (!merges.length) return table;
    const to = new Map(merges.map(([c, list]) => [c, new Map(list.flatMap((m) => m.values.map((v) => [v, m.name])))]));
    const index = (name) => {
      const j = table.columns.indexOf(name);
      if (j < 0) fail(`The table has no column "${name}".`);
      return j;
    };
    let rows;
    if (source.spec.format === 'long') {
      const [iStage, iCat] = [index(source.spec.stage), index(source.spec.category)];
      rows = table.rows.map((r) => {
        const map = to.get(r[iStage]);
        return map && map.has(r[iCat]) ? Object.assign(r.slice(), { [iCat]: map.get(r[iCat]) }) : r;
      });
    } else {
      const columns = [...to.keys()].map((c) => [index(c), to.get(c)]);
      rows = table.rows.map((r) => {
        const out = r.slice();
        columns.forEach(([j, map]) => { if (map.has(r[j])) out[j] = map.get(r[j]); });
        return out;
      });
    }
    return { columns: table.columns, rows };
  }

  // The source with a diagram column's values, and so its nodes from the top, in this order.
  function withValueOrder(source, column, values) {
    const current = valuesOf(source, column).values;
    if (values.length !== current.length || current.some((v) => !values.includes(v))) {
      fail(`A new order of ${columnLabel(source, column)} must list each of its values once.`);
    }
    return Object.assign({}, source, { valueOrders: Object.assign({}, source.valueOrders, { [column]: values.slice() }) });
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
    const table = recode(T.filterTable(source.table, source.filters, source.spec, source.stageFilters), source);
    const columns = source.order.filter((c) => source.enabled.includes(c));
    const spec = Object.assign({}, source.spec, source.spec.format === 'wide' ? { columns } : { stages: columns });
    // Missing values all go to one node, which takes the place of the first of them.
    const orders = source.valueOrders || {};
    const setOrders = columns.map((c) => (orders[c] ? orders[c].map((v) => (T.isNA(v) ? T.MISSING : v)) : null));
    const result = T.build(table, spec, Object.assign({}, source.options, { setOrders }));
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

  // Routing belongs to an ordered pair of adjacent columns, not its current numeric stage index.
  function remapStageParams(previous, next, params) {
    const columns = (source) => source.order.filter(c => source.enabled.includes(c));
    const old = columns(previous), current = columns(next);
    const pairs = new Map(old.slice(0, -1).map((c, i) => [JSON.stringify([c, old[i + 1]]), params[i] || {}]));
    return current.slice(0, -1).map((c, i) => Object.assign({}, pairs.get(JSON.stringify([c, current[i + 1]]))));
  }

  let renderedSource;
  function render(host, source, onChange, force = false) {
    if (!force && renderedSource === source) return;
    renderedSource = source;
    host.hidden = !source;
    if (!source) { host.innerHTML = ''; return; }
    const open = new Set([...host.querySelectorAll('[data-filter-column][open]')].map((el) => el.dataset.filterColumn));
    const searches = new Map([...host.querySelectorAll('[data-filter-search]')].map((el) => [el.dataset.filterSearch, el.value]));
    const count = source.summary ? source.summary.rowsFiltered : source.table.rows.length;
    const total = source.table.rows.length;
    const plotted = source.summary ? ` · ${source.summary.total.toLocaleString()} ${esc(source.summary.unit)} plotted` : '';
    const dropped = source.summary && source.summary.rowsDropped ? ` · ${source.summary.rowsDropped.toLocaleString()} incomplete records omitted` : '';
    host.innerHTML = `<div class="file-head">Imported table</div><div class="file-note">${esc(source.name)} · ${count.toLocaleString()} / ${total.toLocaleString()} source rows match${plotted}${dropped}</div>` +
      '<div class="table-control-title">Keep rows with these values</div>' + source.table.columns.map((name) => {
        const kept = source.filters[name];
        return `<details class="table-filter" data-filter-column="${esc(name)}"${open.has(name) ? ' open' : ''}><summary>${esc(name)} ` +
          `<span class="muted">${kept ? `${kept.length} kept` : 'all'}</span></summary><div class="filter-body"></div></details>`;
      }).join('') + '<button type="button" class="mini-btn wide" data-clear-filters>Reset row filters</button>' +
      '<p class="file-note small">The diagram ' + (source.spec.format === 'long' ? 'stages' : 'columns') + ' and their values are in the strip above the plot. ' +
      (source.spec.format === 'wide' ? 'Single-value filters hide their diagram column automatically; expanding the filter restores it. ' : '') +
      'Changes rebuild and fit the diagram, clearing manual node and strand edits.</p>';
    const filtered = Object.keys(source.filters).length || Object.keys(source.stageFilters || {}).length;
    host.querySelector('[data-clear-filters]').disabled = !filtered;
    host.querySelector('[data-clear-filters]').addEventListener('click', () => onChange(withFilters(Object.assign({}, source, { stageFilters: {} }), {})));
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

  SK.tableControls = { create, withFilters, withValue, withValueOrder, withMerge, withoutMerge, withColumn, withMove, valuesOf, columnLabel, displayValue, esc, build, header, render, remapStageParams };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
