/* The column strip above the plot: an imported table's diagram columns as badges over their node
   columns, each shown column's values listed beneath it, and the columns left out in a row above.
   Clicking a badge moves its column between the rows; dragging one sideways reorders the columns
   (and up or down, moves it between the rows); clicking a value drops its rows or keeps them again. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const TC = SK.tableControls;
  const MAX_VALUE_ROWS = 6;
  const DRAG_START = 4;
  const INDENT = 10;
  const STORE = 'polylines-strip';

  // hooks: change(source) applies a new source; refuse(message) explains a refused change;
  // geometry() gives the shown columns in diagram order, { cx, nodes: [{ id, name, color, cx, left,
  // right, top, bottom }] } in client px (null without a drawing); focus(ids)
  // marks those nodes in the plot (null clears). beam: the element drawn from a badge to its nodes.
  function create(host, beam, hooks) {
    host.innerHTML = '<div class="strip-body"><div class="strip-out"><span class="strip-hint">Left out of the diagram</span></div>' +
      '<div class="strip-in"></div><div class="strip-marker" hidden></div></div>' +
      '<button type="button" class="strip-tab" aria-expanded="true"><span class="strip-tab-text">Columns</span><span class="strip-caret" aria-hidden="true">▴</span></button>';
    const outRow = host.querySelector('.strip-out');
    const inRow = host.querySelector('.strip-in');
    const marker = host.querySelector('.strip-marker');
    const tab = host.querySelector('.strip-tab');
    let source = null;
    let sizedTable = null;
    let refocus = null;
    let lit = null;
    let drag = null;
    let suppressClick = false;
    let collapsed = false;
    try { collapsed = localStorage.getItem(STORE) === 'collapsed'; } catch (err) { /* private window */ }

    const badges = () => [...host.querySelectorAll('.strip-badge')];
    const badgeOf = (column) => badges().find((b) => b.dataset.stripColumn === column);
    const shown = () => source.order.filter((c) => source.enabled.includes(c));

    function apply(make) {
      let next;
      try { next = make(); } catch (err) {
        if (!(err instanceof SK.layout.SankeyError)) throw err;
        hooks.refuse(err.message);
        return;
      }
      if (next.order.join('\u0001') === source.order.join('\u0001') && next.enabled.join('\u0001') === source.enabled.join('\u0001') &&
        next.filters === source.filters && next.stageFilters === source.stageFilters) return;
      hooks.change(next);
    }

    // A value's node: by name, or for missing values the node that collects them.
    const nodeOf = (nodes, value) => nodes.find((n) => n.name === value) ||
      (SK.transform.isNA(value) ? nodes.find((n) => n.name === SK.transform.MISSING) : undefined);

    const valuesList = (column) => [...inRow.querySelectorAll('.strip-values')].find((el) => el.dataset.stripValues === column);

    function setCollapsed(on) {
      collapsed = on;
      try { localStorage.setItem(STORE, on ? 'collapsed' : 'open'); } catch (err) { /* private window */ }
      showCollapsed();
    }

    function showCollapsed() {
      host.classList.toggle('collapsed', collapsed);
      tab.setAttribute('aria-expanded', String(!collapsed));
      tab.title = collapsed ? 'Show the diagram columns' : 'Hide the diagram columns';
      tab.querySelector('.strip-caret').textContent = collapsed ? '▾' : '▴';
    }

    // Shown or hidden with the source, as tall as its longest value list (up to MAX_VALUE_ROWS):
    // the plot panel's height depends on it, so this runs before the plot is fitted.
    function size(next) {
      host.hidden = !next;
      if (!next) return;
      if (sizedTable !== next.table) {
        sizedTable = next.table;
        const longest = Math.max(1, ...next.order.map((c) => TC.valuesOf(next, c).values.length));
        host.style.setProperty('--value-rows', Math.min(MAX_VALUE_ROWS, longest));
      }
      showCollapsed();
    }

    function render(next) {
      size(next);
      if (next === source) return;
      source = next;
      unlight();
      badges().forEach((b) => b.remove());
      inRow.querySelectorAll('.strip-values').forEach((el) => el.remove());
      if (!source) return;
      const { esc, displayValue } = TC;
      const included = new Set(source.enabled);
      const locked = included.size <= 2;
      source.order.forEach((c) => {
        const { values, kept } = TC.valuesOf(source, c);
        const label = TC.columnLabel(source, c);
        const on = included.has(c);
        const count = kept.size < values.length ? `<span class="strip-kept">${kept.size}/${values.length}</span>` : '';
        const title = on ? (locked ? `${label}: the diagram needs at least two columns; drag to reorder` : `${label}: click to leave out of the diagram, drag to reorder`)
          : `${label}: click to bring back into the diagram, or drag it to its place`;
        (on ? inRow : outRow).insertAdjacentHTML('beforeend', `<button type="button" class="strip-badge${on && locked ? ' locked' : ''}" data-strip-column="${esc(c)}" ` +
          `aria-pressed="${on}" title="${esc(title)}"><span class="strip-label">${esc(label)}</span>${count}</button>`);
        if (!on) return;
        inRow.insertAdjacentHTML('beforeend', `<div class="strip-values" data-strip-values="${esc(c)}">` + values.map((v, i) =>
          `<button type="button" class="strip-value${kept.has(v) ? '' : ' off'}" data-strip-value="${esc(v)}" data-order="${i}" aria-pressed="${kept.has(v)}" ` +
          `title="${esc(displayValue(v))}: click to ${kept.has(v) ? 'drop' : 'keep'} its rows">${esc(displayValue(v))}</button>`).join('') + '</div>');
      });
      outRow.classList.toggle('empty', !outRow.querySelector('.strip-badge'));
      place();
      if (refocus !== null) {
        const badge = badgeOf(refocus);
        refocus = null;
        if (badge) badge.focus();
      }
    }

    // Shown columns sit over their node columns, each with the room to its nearer neighbour; the
    // others sit between the shown columns around them in the order, pushed apart to stay readable.
    function place() {
      if (!source || host.hidden || collapsed) return;
      const box = host.getBoundingClientRect();
      const width = box.width;
      const cols = shown();
      const geo = hooks.geometry();
      const matched = geo && geo.length === cols.length;
      const xs = cols.map((c, k) => (matched ? geo[k].cx - box.left : (k + 0.5) * width / cols.length));
      const sorted = xs.slice().sort((a, b) => a - b);
      cols.forEach((c, k) => {
        // Each column's room reaches halfway to its neighbours (or to the strip's ends).
        const x = xs[k];
        const i = sorted.indexOf(x);
        const lo = i > 0 ? (sorted[i - 1] + x) / 2 + 3 : 2;
        const hi = i < sorted.length - 1 ? (x + sorted[i + 1]) / 2 - 3 : width - 2;
        const badge = badgeOf(c);
        badge.style.maxWidth = `${Math.max(44, Math.min(240, 2 * Math.min(x - lo, hi - x)))}px`;
        const left = x - badge.offsetWidth / 2;
        badge.style.left = `${left}px`;
        badge.dataset.x = x;
        // The values hang indented under the badge, as wide as the widest needs, within the room.
        const list = valuesList(c);
        list.style.width = 'max-content';
        const w = Math.max(44, Math.min(Math.max(list.offsetWidth, badge.offsetWidth - INDENT), hi - lo));
        list.style.width = `${w}px`;
        list.style.left = `${Math.max(lo, Math.min(left + INDENT, hi - w))}px`;
        placeValues(list, matched ? geo[k] : null);
      });
      // Left-out columns between the shown ones around them in the order (or the row's ends).
      const runs = [];
      let run = { prev: null, items: [] };
      source.order.forEach((c) => {
        const k = cols.indexOf(c);
        if (k < 0) { run.items.push(c); return; }
        run.next = xs[k];
        runs.push(run);
        run = { prev: xs[k], items: [] };
      });
      run.next = null;
      runs.push(run);
      const out = [];
      runs.filter((r) => r.items.length).forEach((r) => {
        const prev = r.prev === null ? Math.min(4, r.next - 60) : r.prev;
        const next = r.next === null ? Math.max(width - 4, r.prev + 60) : r.next;
        r.items.forEach((c, j) => out.push({ badge: badgeOf(c), x: prev + (j + 1) * (next - prev) / (r.items.length + 1) }));
      });
      out.forEach((o) => { o.badge.style.maxWidth = '160px'; o.w = o.badge.offsetWidth; });
      for (let i = 1; i < out.length; i++) {
        const gap = out[i - 1].x + out[i - 1].w / 2 + 4 - (out[i].x - out[i].w / 2);
        if (gap > 0) out[i].x += gap;
      }
      for (let i = out.length - 1; i >= 0; i--) {
        const limit = i === out.length - 1 ? width - 2 : out[i + 1].x - out[i + 1].w / 2 - 4;
        if (out[i].x + out[i].w / 2 > limit) out[i].x = limit - out[i].w / 2;
      }
      out.forEach((o) => {
        o.badge.style.left = `${o.x - o.w / 2}px`;
        o.badge.dataset.x = o.x;
      });
      if (lit) light(lit);
    }

    // Values in the order of their nodes from the top, then those without a node (dropped, absent
    // from the remaining rows, or grouped into Other); kept values take their node's color.
    function placeValues(list, geo) {
      const nodes = geo ? geo.nodes : [];
      const chips = [...list.children].sort((a, b) => a.dataset.order - b.dataset.order);
      const node = new Map(chips.map((chip) => [chip, nodeOf(nodes, chip.dataset.stripValue)]));
      const ordered = chips.map((chip, i) => ({ chip, i, y: node.get(chip) ? node.get(chip).top : Infinity }))
        .sort((a, b) => a.y - b.y || a.i - b.i).map((o) => o.chip);
      if (ordered.some((chip, i) => chip !== list.children[i])) ordered.forEach((chip) => list.appendChild(chip));
      chips.forEach((chip) => {
        const on = chip.getAttribute('aria-pressed') === 'true';
        chip.classList.toggle('absent', on && !node.get(chip));
        if (on && node.get(chip)) chip.style.setProperty('--node', node.get(chip).color);
        else chip.style.removeProperty('--node');
      });
    }

    // The nodes of a hovered badge (its column) or value (its node) glow under a beam from it.
    function light(target) {
      lit = target;
      const cols = shown();
      const k = cols.indexOf(target.column);
      const geo = hooks.geometry();
      const column = k >= 0 && geo && geo.length === cols.length ? geo[k] : null;
      const nodes = !column ? [] : target.value === undefined ? column.nodes : [nodeOf(column.nodes, target.value)].filter(Boolean);
      hooks.focus(nodes.length ? nodes.map((n) => n.id) : null);
      if (!nodes.length) { beam.hidden = true; return; }
      const from = target.element.getBoundingClientRect();
      const frame = beam.parentElement.getBoundingClientRect();
      const left = Math.min(...nodes.map((n) => n.left)) - 3;
      const right = Math.max(...nodes.map((n) => n.right)) + 3;
      const top = host.getBoundingClientRect().bottom;
      const bottom = Math.max(...nodes.map((n) => n.bottom)) + 4;
      if (!(bottom > top)) { beam.hidden = true; return; }
      const x0 = Math.min(from.left, left);
      const x1 = Math.max(from.right, right);
      Object.assign(beam.style, { left: `${x0 - frame.left}px`, top: `${top - frame.top}px`, width: `${x1 - x0}px`, height: `${bottom - top}px`,
        clipPath: `polygon(${from.left - x0}px 0, ${from.right - x0}px 0, ${right - x0}px 100%, ${left - x0}px 100%)` });
      beam.hidden = false;
    }

    function unlight() {
      if (!lit) return;
      lit = null;
      beam.hidden = true;
      hooks.focus(null);
    }

    host.addEventListener('pointerover', (event) => {
      if (drag) return;
      const value = event.target.closest('.strip-value');
      const badge = event.target.closest('.strip-badge');
      if (value) light({ column: value.parentElement.dataset.stripValues, value: value.dataset.stripValue, element: value });
      else if (badge) light({ column: badge.dataset.stripColumn, element: badge });
      else unlight();
    });
    host.addEventListener('pointerleave', () => { if (!drag) unlight(); });

    // Where a dragged badge would go: its index in the order without it (by the positions of the
    // other badges) and its row.
    function dropAt(event, column) {
      const box = host.getBoundingClientRect();
      const x = event.clientX - box.left;
      const others = source.order.filter((c) => c !== column);
      const index = others.filter((c) => Number(badgeOf(c).dataset.x) < x).length;
      const zone = event.clientY < inRow.getBoundingClientRect().top ? 'out' : 'in';
      return { x, index, zone, others };
    }

    function moveDrag(event) {
      if (!drag.moved) {
        if (Math.hypot(event.clientX - drag.x0, event.clientY - drag.y0) < DRAG_START) return;
        drag.moved = true;
        drag.ghost = drag.badge.cloneNode(true);
        drag.ghost.classList.add('strip-ghost');
        drag.ghost.removeAttribute('data-strip-column');
        host.appendChild(drag.ghost);
        drag.badge.classList.add('dragging');
        host.classList.add('dragging');
      }
      const box = host.getBoundingClientRect();
      drag.ghost.style.left = `${event.clientX - box.left - drag.dx}px`;
      drag.ghost.style.top = `${event.clientY - box.top - drag.dy}px`;
      const at = dropAt(event, drag.column);
      outRow.classList.toggle('drop-target', at.zone === 'out');
      const xs = at.others.map((c) => Number(badgeOf(c).dataset.x)).sort((a, b) => a - b);
      const prev = at.index > 0 ? xs[at.index - 1] : xs[0] - 40;
      const next = at.index < xs.length ? xs[at.index] : xs[xs.length - 1] + 40;
      marker.style.left = `${(prev + next) / 2}px`;
      marker.classList.toggle('out', at.zone === 'out');
      marker.hidden = false;
      if (source.enabled.includes(drag.column)) light({ column: drag.column, element: drag.ghost });
    }

    function endDrag(event) {
      window.removeEventListener('pointermove', moveDrag);
      window.removeEventListener('pointerup', endDrag);
      window.removeEventListener('pointercancel', endDrag);
      const d = drag;
      drag = null;
      if (!d.moved) return;
      // The click that may follow the drag (on the badge, or on a parent) is not a click on it.
      suppressClick = true;
      setTimeout(() => { suppressClick = false; });
      d.ghost.remove();
      d.badge.classList.remove('dragging');
      host.classList.remove('dragging');
      outRow.classList.remove('drop-target');
      marker.hidden = true;
      unlight();
      if (event.type === 'pointercancel') return;
      const at = dropAt(event, d.column);
      const include = at.zone === 'in';
      apply(() => {
        const moved = TC.withMove(source, d.column, at.index);
        return include === source.enabled.includes(d.column) ? moved : TC.withColumn(moved, d.column, include);
      });
    }

    host.addEventListener('pointerdown', (event) => {
      const badge = event.target.closest('.strip-badge');
      if (!badge || event.button !== 0) return;
      const box = badge.getBoundingClientRect();
      drag = { column: badge.dataset.stripColumn, badge, x0: event.clientX, y0: event.clientY, dx: event.clientX - box.left, dy: event.clientY - box.top, moved: false };
      window.addEventListener('pointermove', moveDrag);
      window.addEventListener('pointerup', endDrag);
      window.addEventListener('pointercancel', endDrag);
    });

    host.addEventListener('click', (event) => {
      if (event.target.closest('.strip-tab')) { setCollapsed(!collapsed); return; }
      if (suppressClick) { suppressClick = false; return; }
      const value = event.target.closest('.strip-value');
      const badge = event.target.closest('.strip-badge');
      if (value) apply(() => TC.withValue(source, value.parentElement.dataset.stripValues, value.dataset.stripValue));
      else if (badge) apply(() => TC.withColumn(source, badge.dataset.stripColumn, !source.enabled.includes(badge.dataset.stripColumn)));
    });

    host.addEventListener('keydown', (event) => {
      const badge = event.target.closest('.strip-badge');
      if (!badge || !event.altKey || !['ArrowLeft', 'ArrowRight'].includes(event.key)) return;
      event.preventDefault();
      const column = badge.dataset.stripColumn;
      const index = source.order.indexOf(column) + (event.key === 'ArrowLeft' ? -1 : 1);
      if (index < 0 || index >= source.order.length) return;
      refocus = column;
      apply(() => TC.withMove(source, column, index));
      refocus = null;
    });

    return { size, render, place };
  }

  SK.columnStrip = { create };
})(typeof window !== 'undefined' ? window : globalThis);
