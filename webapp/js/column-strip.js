/* The arrange strip above the plot: an imported table's diagram columns as badges over their node
   columns, each shown column's values listed beneath it, and the columns not shown in a row above.
   Clicking a badge moves its column between the rows; dragging one sideways reorders the columns
   (and up or down, moves it between the rows); clicking a value drops its rows or keeps them again,
   and dragging it up or down moves its node within the column. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const TC = SK.tableControls;
  const MAX_VALUE_ROWS = 6;
  const DRAG_START = 4;
  const INDENT = 10;
  const ZONE_GAP = 6;
  const ZONE_PAD = 6;
  const STORE = 'polylines-strip';

  // hooks: change(source) applies a new source; refuse(message) explains a refused change;
  // geometry() gives the shown columns in diagram order, { cx, color, nodes: [{ id, name, color,
  // count, cx, left, right, top, bottom, bar }] } in client px: a node's box including its label,
  // and bar, its drawn body's (null without a drawing);
  // focus(ids) marks those nodes in the plot (null clears). beam: the element drawn from a badge to
  // its nodes.
  function create(host, beam, hooks) {
    host.innerHTML = '<div class="strip-body"><div class="strip-out"><span class="strip-out-label">Not shown:</span>' +
      '<span class="strip-hint">none (click or drag a column here to hide it)</span></div>' +
      '<div class="strip-in"><div class="strip-zones"></div></div><div class="strip-marker" hidden></div></div>' +
      '<div class="strip-value-marker" hidden></div>' +
      '<button type="button" class="strip-tab" aria-expanded="true"><span class="strip-tab-text">Arrange</span><span class="strip-caret" aria-hidden="true">▴</span></button>';
    const outRow = host.querySelector('.strip-out');
    const inRow = host.querySelector('.strip-in');
    const zoneLines = host.querySelector('.strip-zones');
    const marker = host.querySelector('.strip-marker');
    const valueMarker = host.querySelector('.strip-value-marker');
    const tab = host.querySelector('.strip-tab');
    let source = null;
    let sizedTable = null;
    let counts = new Map();
    let listed = new Map(); // per column, the values in the order last shown
    let zones = [];
    let refocus = null;
    let lit = null;
    let drag = null;
    let suppressClick = false;
    let collapsed = false;
    try { collapsed = localStorage.getItem(STORE) === 'collapsed'; } catch (err) { /* private window */ }

    const badges = () => [...host.querySelectorAll('.strip-badge')];
    const badgeOf = (column) => badges().find((b) => b.dataset.stripColumn === column);
    const valuesList = (column) => [...inRow.querySelectorAll('.strip-values')].find((el) => el.dataset.stripValues === column);
    const shown = (s = source) => s.order.filter((c) => s.enabled.includes(c));
    const local = (rect) => {
      const box = host.getBoundingClientRect();
      return { left: rect.left - box.left, right: rect.right - box.left, top: rect.top - box.top, bottom: rect.bottom - box.top };
    };

    function apply(make) {
      let next;
      try { next = make(); } catch (err) {
        if (!(err instanceof SK.layout.SankeyError)) throw err;
        hooks.refuse(err.message);
        return;
      }
      if (next.order.join('\u0001') === source.order.join('\u0001') && next.enabled.join('\u0001') === source.enabled.join('\u0001') &&
        next.filters === source.filters && next.stageFilters === source.stageFilters && next.valueOrders === source.valueOrders) return;
      hooks.change(next);
    }

    // A value's node: by name, or for missing values the node that collects them.
    const nodeOf = (nodes, value) => nodes.find((n) => n.name === value) ||
      (SK.transform.isNA(value) ? nodes.find((n) => n.name === SK.transform.MISSING) : undefined);

    function setCollapsed(on) {
      collapsed = on;
      try { localStorage.setItem(STORE, on ? 'collapsed' : 'open'); } catch (err) { /* private window */ }
      showCollapsed();
    }

    function showCollapsed() {
      host.classList.toggle('collapsed', collapsed);
      tab.setAttribute('aria-expanded', String(!collapsed));
      tab.title = collapsed ? 'Show the diagram columns and their values' : 'Hide the diagram columns and their values';
      tab.querySelector('.strip-caret').textContent = collapsed ? '▾' : '▴';
    }

    // Shown or hidden with the source, as tall as the longest value list of a shown column (up to
    // MAX_VALUE_ROWS): the plot panel's height depends on it, so this runs before the plot is fitted.
    function size(next) {
      host.hidden = !next;
      if (!next) return;
      if (sizedTable !== next.table) {
        sizedTable = next.table;
        counts = new Map();
        listed = new Map();
      }
      const count = (c) => {
        if (!counts.has(c)) counts.set(c, TC.valuesOf(next, c).values.length);
        return counts.get(c);
      };
      host.style.setProperty('--value-rows', Math.min(MAX_VALUE_ROWS, Math.max(1, ...shown(next).map(count))));
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
        const title = on ? (locked ? `${label}: the diagram needs at least two columns; drag to reorder` : `${label}: click to hide, drag to reorder`)
          : `${label}: click to show it again, or drag it to its place`;
        const html = `<button type="button" class="strip-badge${on && locked ? ' locked' : ''}" data-strip-column="${esc(c)}" ` +
          `aria-pressed="${on}" title="${esc(title)}"><span class="strip-label">${esc(label)}</span>${count}</button>`;
        if (on) inRow.insertAdjacentHTML('beforeend', html);
        else outRow.querySelector('.strip-hint').insertAdjacentHTML('beforebegin', html);
        if (!on) return;
        inRow.insertAdjacentHTML('beforeend', `<div class="strip-values" data-strip-values="${esc(c)}">` + values.map((v, i) =>
          `<button type="button" class="strip-value${kept.has(v) ? '' : ' off'}" data-strip-value="${esc(v)}" data-order="${i}" aria-pressed="${kept.has(v)}" ` +
          `title="${esc(displayValue(v))}: click to ${kept.has(v) ? 'drop' : 'keep'} its rows, drag to move its node">` +
          `<span class="strip-chip">${esc(displayValue(v))}</span><span class="strip-count"></span></button>`).join('') + '</div>');
      });
      outRow.classList.toggle('empty', !outRow.querySelector('.strip-badge'));
      place();
      if (refocus !== null) {
        const badge = badgeOf(refocus);
        refocus = null;
        if (badge) badge.focus();
      }
    }

    // Each shown column has a zone over its node column, reaching halfway to its neighbours'
    // centres (the end zones as far outward as inward, within the strip while their column is on
    // it), tinted with its color, dashed at both edges and set apart by white. The badge, in the
    // column's color, sits at the zone's left with the values indented under it, as wide as they
    // need within the zone; scrolled partly off the strip's left end, a zone keeps them in its
    // visible part until its far edge pushes them off.
    function place() {
      if (!source || host.hidden || collapsed) return;
      const box = host.getBoundingClientRect();
      const width = box.width;
      const cols = shown();
      const geo = hooks.geometry();
      const matched = geo && geo.length === cols.length;
      const items = cols.map((c, k) => ({ column: c, x: matched ? geo[k].cx - box.left : (k + 0.5) * width / cols.length,
        geo: matched ? geo[k] : null, badge: badgeOf(c), list: valuesList(c) })).sort((a, b) => a.x - b.x);
      const n = items.length;
      const first = items[0];
      const last = items[n - 1];
      const bounds = [first.x - (n > 1 ? (items[1].x - first.x) / 2 : 80)];
      items.slice(1).forEach((it, i) => bounds.push((items[i].x + it.x) / 2));
      bounds.push(last.x + (n > 1 ? (last.x - items[n - 2].x) / 2 : 80));
      if (first.x >= 0) bounds[0] = Math.max(2, bounds[0]);
      if (last.x <= width) bounds[n] = Math.min(width - 2, bounds[n]);
      zones = items.map((it, i) => ({ column: it.column, x: it.x, from: bounds[i], to: bounds[i + 1],
        left: bounds[i] + ZONE_GAP / 2, right: bounds[i + 1] - ZONE_GAP / 2 }));
      items.forEach((it, i) => {
        const { left, right } = zones[i];
        const inner = right - left - 2 * ZONE_PAD;
        it.badge.style.maxWidth = `${Math.max(44, inner)}px`;
        it.badge.classList.toggle('colored', !!it.geo);
        if (it.geo) it.badge.style.setProperty('--col', it.geo.color);
        placeValues(it.list, it.geo);
        // The widest value in full (unrounded: a rounded-down width cuts it short), with the list's
        // indent and any scroll bar.
        it.list.style.width = 'max-content';
        const want = Math.ceil(it.list.getBoundingClientRect().width) + 1 + (it.list.scrollHeight > it.list.clientHeight + 1 ? 10 : 0);
        const listWidth = Math.max(44, Math.min(want, inner - INDENT));
        const content = Math.max(it.badge.offsetWidth, INDENT + listWidth);
        const x0 = Math.max(left + ZONE_PAD, Math.min(Math.max(left, 2) + ZONE_PAD, right - ZONE_PAD - content));
        it.badge.style.left = `${x0}px`;
        it.list.style.left = `${x0 + INDENT}px`;
        it.list.style.width = `${listWidth}px`;
      });
      // Kept across redraws (scrolling redraws often), and remade when the shown columns change.
      if ([...zoneLines.children].map((el) => el.dataset.stripZone).join('\u0001') !== items.map((it) => it.column).join('\u0001')) {
        zoneLines.innerHTML = items.map((it) => `<div class="strip-zone" data-strip-zone="${TC.esc(it.column)}"></div>`).join('');
      }
      items.forEach((it, i) => {
        const el = zoneLines.children[i];
        el.style.left = `${zones[i].left}px`;
        el.style.width = `${zones[i].right - zones[i].left}px`;
        if (it.geo) el.style.setProperty('--col', it.geo.color);
        else el.style.removeProperty('--col');
      });
      if (lit) light(lit);
    }

    // Values in the column's chosen order (which its nodes follow), else in the order of their nodes
    // from the top, then those without a node (dropped, absent from the remaining rows, or grouped
    // into Other). A value that loses or regains its node keeps the place it was last shown in, the
    // values with nodes taking the other places in node order. Kept values take their node's color.
    function placeValues(list, geo) {
      const column = list.dataset.stripValues;
      const nodes = geo ? geo.nodes : [];
      const chips = [...list.children].sort((a, b) => a.dataset.order - b.dataset.order);
      const node = new Map(chips.map((chip) => [chip, nodeOf(nodes, chip.dataset.stripValue)]));
      const chosen = !!(source.valueOrders || {})[column];
      const byNode = chips.filter((chip) => node.get(chip)).sort((a, b) => node.get(a).top - node.get(b).top);
      const chipOf = new Map(chips.map((chip) => [chip.dataset.stripValue, chip]));
      const before = listed.get(column);
      let ordered = chips;
      if (!chosen && before) {
        const queue = byNode.slice();
        ordered = before.map((v) => (node.get(chipOf.get(v)) ? queue.shift() : chipOf.get(v)));
      } else if (!chosen) {
        ordered = byNode.concat(chips.filter((chip) => !node.get(chip)));
      }
      listed.set(column, ordered.map((chip) => chip.dataset.stripValue));
      if (ordered.some((chip, i) => chip !== list.children[i])) ordered.forEach((chip) => list.appendChild(chip));
      chips.forEach((chip) => {
        const on = chip.getAttribute('aria-pressed') === 'true';
        const n = on ? node.get(chip) : null;
        chip.classList.toggle('absent', on && !n);
        if (n) chip.style.setProperty('--node', n.color);
        else chip.style.removeProperty('--node');
        chip.querySelector('.strip-count').textContent = n ? n.count : '';
      });
    }

    // The nodes of a hovered badge (its column) or value (its node) glow under a beam from the foot
    // of the column's zone (or from a dragged badge or value), which narrows to the nodes' drawn
    // bodies by the top of the first label and runs straight down past the last node.
    function light(target) {
      lit = target;
      const nodesOf = () => {
        const cols = shown();
        const k = cols.indexOf(target.column);
        const geo = hooks.geometry();
        const column = k >= 0 && geo && geo.length === cols.length ? geo[k] : null;
        return !column ? [] : target.value === undefined ? column.nodes : [nodeOf(column.nodes, target.value)].filter(Boolean);
      };
      let nodes = nodesOf();
      hooks.focus(nodes.length ? nodes.map((n) => n.id) : null);
      if (!nodes.length) { beam.hidden = true; return; }
      nodes = nodesOf(); // bold labels are taller and wider
      const box = host.getBoundingClientRect();
      const zone = !target.dragged && zones.find((z) => z.column === target.column);
      const from = zone ? { left: box.left + zone.left, right: box.left + zone.right } : target.element.getBoundingClientRect();
      const frame = beam.parentElement.getBoundingClientRect();
      const left = Math.min(...nodes.map((n) => n.bar.left));
      const right = Math.max(...nodes.map((n) => n.bar.right));
      const top = box.bottom;
      const reach = Math.max(top, Math.min(...nodes.map((n) => n.top)) - 4);
      const bottom = Math.max(...nodes.map((n) => n.bar.bottom)) + 4;
      if (!(bottom > top)) { beam.hidden = true; return; }
      const x0 = Math.min(from.left, left);
      const x1 = Math.max(from.right, right);
      const r = reach - top;
      Object.assign(beam.style, { left: `${x0 - frame.left}px`, top: `${top - frame.top}px`, width: `${x1 - x0}px`, height: `${bottom - top}px`,
        clipPath: `polygon(${from.left - x0}px 0, ${from.right - x0}px 0, ${right - x0}px ${r}px, ${right - x0}px 100%, ` +
          `${left - x0}px 100%, ${left - x0}px ${r}px)` });
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

    // A dragged badge goes to the row under the pointer; among the shown columns (in their zones)
    // to the one slot between the two whose centres are either side of the pointer.
    function slotAt(event, column) {
      const x = event.clientX - host.getBoundingClientRect().left;
      const zone = event.clientY < inRow.getBoundingClientRect().top ? 'out' : 'in';
      const others = zones.filter((z) => z.column !== column);
      return { zone, slot: others.filter((z) => z.x < x).length, others };
    }

    function showSlot(at, column) {
      outRow.classList.toggle('drop-target', at.zone === 'out');
      marker.hidden = at.zone === 'out';
      if (at.zone === 'out') return;
      const home = zones.findIndex((z) => z.column === column);
      // Back at its own place, the column's zone lights up; elsewhere, the line between two zones.
      const stays = home >= 0 && home === at.slot;
      const x = at.slot === 0 ? at.others[0].from : at.slot === at.others.length ? at.others[at.others.length - 1].to : at.others[at.slot - 1].to;
      marker.classList.toggle('home', stays);
      marker.style.left = `${stays ? zones[home].left : x - 1.5}px`;
      marker.style.width = `${stays ? zones[home].right - zones[home].left : 3}px`;
    }

    function startGhost(element) {
      const ghost = element.cloneNode(true);
      ghost.classList.add('strip-ghost');
      ghost.removeAttribute('data-strip-column');
      ghost.removeAttribute('data-strip-value');
      ghost.style.width = `${element.getBoundingClientRect().width}px`;
      host.appendChild(ghost);
      element.classList.add('dragging');
      host.classList.add('dragging');
      return ghost;
    }

    function moveDrag(event) {
      if (!drag.moved) {
        if (Math.hypot(event.clientX - drag.x0, event.clientY - drag.y0) < DRAG_START) return;
        drag.moved = true;
        drag.ghost = startGhost(drag.element);
      }
      const box = host.getBoundingClientRect();
      if (drag.kind === 'value') {
        // Values move only up and down their own list.
        const list = drag.element.parentElement.getBoundingClientRect();
        const y = Math.max(list.top - drag.dy - 6, Math.min(list.bottom - drag.dy + 6, event.clientY - drag.dy));
        drag.ghost.style.left = `${drag.element.getBoundingClientRect().left - box.left}px`;
        drag.ghost.style.top = `${y - box.top}px`;
        const chips = [...drag.element.parentElement.children].filter((c) => c !== drag.element);
        drag.slot = chips.filter((c) => { const r = c.getBoundingClientRect(); return r.top + r.height / 2 < event.clientY; }).length;
        const edge = !chips.length ? list.top : drag.slot === 0 ? chips[0].getBoundingClientRect().top - 1
          : chips[drag.slot - 1].getBoundingClientRect().bottom + 1;
        const chip = local(drag.element.getBoundingClientRect());
        Object.assign(valueMarker.style, { left: `${chip.left}px`, width: `${chip.right - chip.left}px`, top: `${edge - box.top - 1}px` });
        valueMarker.hidden = false;
        light({ column: drag.column, value: drag.element.dataset.stripValue, element: drag.ghost, dragged: true });
        return;
      }
      drag.ghost.style.left = `${event.clientX - box.left - drag.dx}px`;
      drag.ghost.style.top = `${event.clientY - box.top - drag.dy}px`;
      showSlot(slotAt(event, drag.column), drag.column);
      if (source.enabled.includes(drag.column)) light({ column: drag.column, element: drag.ghost, dragged: true });
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
      d.element.classList.remove('dragging');
      host.classList.remove('dragging');
      outRow.classList.remove('drop-target');
      marker.hidden = true;
      valueMarker.hidden = true;
      unlight();
      if (event.type === 'pointercancel') return;
      if (d.kind === 'value') {
        const values = [...d.element.parentElement.children].filter((c) => c !== d.element).map((c) => c.dataset.stripValue);
        values.splice(d.slot, 0, d.element.dataset.stripValue);
        const current = [...d.element.parentElement.children].map((c) => c.dataset.stripValue);
        if (values.join('\u0001') !== current.join('\u0001')) apply(() => TC.withValueOrder(source, d.column, values));
        return;
      }
      const at = slotAt(event, d.column);
      const on = source.enabled.includes(d.column);
      if (at.zone === 'out') {
        if (on) apply(() => TC.withColumn(source, d.column, false));
        return;
      }
      // Just before the shown column it now precedes (after any hidden ones there), or after the last.
      const rest = source.order.filter((c) => c !== d.column);
      const index = at.slot < at.others.length ? rest.indexOf(at.others[at.slot].column) : rest.indexOf(at.others[at.others.length - 1].column) + 1;
      apply(() => {
        const moved = TC.withMove(source, d.column, index);
        return on ? moved : TC.withColumn(moved, d.column, true);
      });
    }

    host.addEventListener('pointerdown', (event) => {
      const element = event.target.closest('.strip-badge, .strip-value');
      if (!element || event.button !== 0) return;
      const box = element.getBoundingClientRect();
      const value = element.classList.contains('strip-value');
      drag = { kind: value ? 'value' : 'column', column: value ? element.parentElement.dataset.stripValues : element.dataset.stripColumn,
        element, x0: event.clientX, y0: event.clientY, dx: event.clientX - box.left, dy: event.clientY - box.top, moved: false };
      window.addEventListener('pointermove', moveDrag);
      window.addEventListener('pointerup', endDrag);
      window.addEventListener('pointercancel', endDrag);
    });

    host.addEventListener('click', (event) => {
      if (event.target.closest('.strip-tab')) { setCollapsed(!collapsed); return; }
      if (suppressClick) { suppressClick = false; return; }
      const value = event.target.closest('.strip-value');
      const badge = event.target.closest('.strip-badge');
      if (value) {
        // The list's order as shown becomes the column's, so a dropped value keeps its place (and so
        // do the nodes) instead of falling to the end with the other values without a node.
        const column = value.parentElement.dataset.stripValues;
        const shownOrder = [...value.parentElement.children].map((c) => c.dataset.stripValue);
        apply(() => TC.withValue(TC.withValueOrder(source, column, shownOrder), column, value.dataset.stripValue));
      }
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

    return { render, place };
  }

  SK.columnStrip = { create };
})(typeof window !== 'undefined' ? window : globalThis);
