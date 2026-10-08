/* "Import table" dialog: preview a long or wide data table, map its columns, and build flows. */
(function () {
  'use strict';
  const SK = window.SK;
  const T = SK.transform;
  const $ = (selector) => document.querySelector(selector);
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const PREVIEW_ROWS = 6;

  const dialog = $('#import-dialog');
  let table = null;
  let tableName = '';
  let spec = null;
  let result = null;
  let typingTimer = null;

  // Column roles for a layout, reusing the automatic guess when it chose that layout.
  function defaultsFor(format) {
    const guessed = T.guess(table);
    if (guessed.format === format) return guessed;
    const info = T.columnInfo(table);
    const names = table.columns;
    const numeric = info.filter((c) => c.numeric).map((c) => c.name);
    const text = info.filter((c) => !c.numeric).map((c) => c.name);
    if (format === 'wide') return { format, columns: text.length >= 2 ? text : names.slice(0, 2), weight: numeric[0] || null, id: null };
    if (format === 'long') return { format, id: names[0], stage: names[1], category: names[2] || names[1], weight: null, stageOrder: 'auto' };
    if (format === 'links') {
      return { format, source: names[0], target: names[1], value: numeric.find((c) => c !== names[0] && c !== names[1]) || null, stage: null };
    }
    return { format };
  }

  function open(text, name) {
    tableName = name || '';
    $('#import-file-name').textContent = name || 'or paste a table below';
    $('#import-text').value = text || '';
    if (!dialog.open) dialog.showModal();
    loadText(text || '');
  }

  function loadText(text) {
    table = null;
    spec = null;
    result = null;
    let error = null;
    if (text.trim()) {
      try {
        table = T.parseTable(text);
        spec = Object.assign({ missing: 'drop' }, T.guess(table));
      } catch (err) {
        error = err.message;
      }
    }
    renderPreview();
    renderMapping();
    if (error) showSummary(error, true);
    else refresh();
  }

  function renderPreview() {
    const host = $('#import-preview');
    if (!table) {
      host.innerHTML = '';
      return;
    }
    const info = T.columnInfo(table);
    const head = info.map((c) => `<th>${esc(c.name)}<span class="badge">${c.numeric ? 'number' : `${c.distinct} values`}</span></th>`).join('');
    const body = table.rows.slice(0, PREVIEW_ROWS).map((r) => `<tr>${r.map((c) => `<td>${esc(c)}</td>`).join('')}</tr>`).join('');
    const more = table.rows.length > PREVIEW_ROWS ? ` (first ${PREVIEW_ROWS} shown)` : '';
    host.innerHTML = `<div class="table-scroll"><table><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table></div>` +
      `<div class="muted small">${table.rows.length} rows${more}</div>`;
  }

  const selectRow = (id, label, options, value) => `<div class="row"><label for="${id}">${label}</label><select id="${id}">` +
    options.map(([v, l]) => `<option value="${esc(v)}"${v === (value || '') ? ' selected' : ''}>${esc(l)}</option>`).join('') +
    '</select></div>';

  function renderMapping() {
    const host = $('#import-mapping');
    const format = $('#import-format');
    format.disabled = !table;
    if (!table) {
      host.innerHTML = '';
      return;
    }
    format.value = spec.format;
    const cols = table.columns.map((c) => [c, c]);
    const numeric = T.columnInfo(table).filter((c) => c.numeric).map((c) => [c.name, c.name]);
    const none = [['', '— none —']];
    let html = '';
    if (spec.format === 'wide') {
      const chosen = spec.columns || [];
      const order = chosen.concat(table.columns.filter((c) => !chosen.includes(c)));
      html += '<div class="row top"><label>Stage columns</label><ol class="stage-picker">' + order.map((c) => {
        const pos = chosen.indexOf(c);
        const arrows = pos >= 0
          ? `<button type="button" data-move="${pos}" data-dir="-1"${pos === 0 ? ' disabled' : ''} title="Earlier stage">↑</button>` +
            `<button type="button" data-move="${pos}" data-dir="1"${pos === chosen.length - 1 ? ' disabled' : ''} title="Later stage">↓</button>`
          : '';
        return `<li class="${pos >= 0 ? 'on' : ''}"><label><input type="checkbox" data-col="${esc(c)}"${pos >= 0 ? ' checked' : ''}> ` +
          `${esc(c)}</label>${arrows}</li>`;
      }).join('') + '</ol></div>';
      html += selectRow('imp-weight', 'Weight', none.concat(numeric), spec.weight) +
        selectRow('imp-id', 'Item id', [['', '— none (trace pathways only) —']].concat(cols), spec.id);
    } else if (spec.format === 'long') {
      html += selectRow('imp-id', 'Item id', cols, spec.id) +
        selectRow('imp-stage', 'Stage', cols, spec.stage) +
        selectRow('imp-category', 'Category', cols, spec.category) +
        selectRow('imp-weight', 'Weight', none.concat(numeric), spec.weight) +
        selectRow('imp-stage-order', 'Stage order', [['auto', 'numeric if possible, else as listed'],
          ['appearance', 'as listed'], ['alphabetical', 'alphabetical']], spec.stageOrder || 'auto');
    } else if (spec.format === 'links') {
      html += selectRow('imp-source', 'Source', cols, spec.source) +
        selectRow('imp-target', 'Target', cols, spec.target) +
        selectRow('imp-value', 'Value', [['', 'count rows']].concat(numeric), spec.value) +
        selectRow('imp-stage', 'Stage', none.concat(cols), spec.stage);
    } else {
      html += '<p class="muted small">The first column holds source labels; every other column is a target and holds its values.</p>';
    }
    host.innerHTML = html;

    const roles = { 'imp-weight': 'weight', 'imp-id': 'id', 'imp-stage': 'stage', 'imp-category': 'category',
      'imp-stage-order': 'stageOrder', 'imp-source': 'source', 'imp-target': 'target', 'imp-value': 'value' };
    Object.entries(roles).forEach(([id, key]) => {
      const el = host.querySelector(`#${id}`);
      if (el) el.addEventListener('change', () => { spec[key] = el.value || null; refresh(); });
    });
    host.querySelectorAll('[data-col]').forEach((box) => box.addEventListener('change', () => {
      const name = box.dataset.col;
      spec.columns = box.checked ? (spec.columns || []).concat([name]) : spec.columns.filter((c) => c !== name);
      renderMapping();
      refresh();
    }));
    host.querySelectorAll('[data-move]').forEach((b) => b.addEventListener('click', () => {
      const i = Number(b.dataset.move);
      const j = i + Number(b.dataset.dir);
      [spec.columns[i], spec.columns[j]] = [spec.columns[j], spec.columns[i]];
      renderMapping();
      refresh();
    }));
  }

  function options() {
    const share = Number($('#import-min-share').value);
    const top = Number($('#import-top-n').value);
    return {
      order: $('#import-order').value,
      minShare: $('#import-min-share').value !== '' && share > 0 ? share / 100 : 0,
      topN: $('#import-top-n').value !== '' && top >= 1 ? Math.round(top) : 0
    };
  }

  function showSummary(text, isError) {
    const el = $('#import-summary');
    el.textContent = text;
    el.classList.toggle('error', !!isError);
    $('#import-apply').disabled = !!isError || !result;
  }

  const fmt = (v) => (Number.isInteger(v) ? v.toLocaleString() : Number(v.toPrecision(5)).toLocaleString());

  function refresh() {
    result = null;
    if (!table) {
      showSummary('Choose or paste a table with a header row.', true);
      return;
    }
    spec.missing = $('#import-missing').value;
    try {
      result = T.build(table, spec, options());
    } catch (err) {
      showSummary(err.message, true);
      return;
    }
    const s = result.summary;
    const shape = result.mode === 'graph'
      ? 'graph (some links skip columns)'
      : `${s.stages} stage${s.stages === 1 ? '' : 's'}`;
    const dropped = s.rowsDropped ? ` · ${s.rowsDropped} incomplete row${s.rowsDropped === 1 ? '' : 's'} dropped` : '';
    showSummary(`${shape} · ${s.nodes} nodes · ${s.strands} strands · ${fmt(s.total)} total flow${dropped}`, false);
  }

  function bind() {
    $('#import-format').addEventListener('change', (e) => {
      const missing = spec ? spec.missing : 'drop';
      spec = Object.assign({ missing }, defaultsFor(e.target.value));
      renderMapping();
      refresh();
    });
    ['#import-missing', '#import-order', '#import-min-share', '#import-top-n'].forEach((sel) => {
      $(sel).addEventListener('change', refresh);
    });
    $('#import-text').addEventListener('input', (e) => {
      clearTimeout(typingTimer);
      typingTimer = setTimeout(() => {
        tableName = '';
        $('#import-file-name').textContent = 'or paste a table below';
        loadText(e.target.value);
      }, 300);
    });
    $('#import-file').addEventListener('change', async (e) => {
      const file = e.target.files[0];
      e.target.value = '';
      if (!file) return;
      const text = await file.text();
      open(text, file.name);
    });
    $('#import-apply').addEventListener('click', () => {
      if (!result) return;
      window.SankeyApp.applyImport(result, { labels: $('#import-labels').value, name: tableName || 'pasted table' });
      dialog.close();
    });
    document.querySelectorAll('[data-open-import]').forEach((b) => b.addEventListener('click', () => open('', '')));
  }

  bind();
  SK.importer = { open };
})();
