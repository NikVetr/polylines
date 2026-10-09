/* Data edits on the app state. mergeNodes() collapses nodes of one layered node set into one. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const fail = (message) => { throw new SK.layout.SankeyError(message); };
  const transpose = (m) => (m.length ? m[0].map((_, j) => m.map((row) => row[j])) : m);
  const sum = (a) => a.reduce((t, v) => t + (Number(v) || 0), 0);

  // Merge nodes `indices` (0-based) of node set `set` (1-based) into one node named `name`, placed
  // where the first of them is in the stack, at their weighted mean height. Returns a new state:
  // flows into and out of the merged nodes are summed; color matrices keep, per merged cell, the
  // color of its largest flow; pathways are re-indexed (identical ones merge, items kept); colors and
  // strand orders keyed by the changed nodes or strands are remapped or dropped.
  function mergeNodes(state, set, indices, name) {
    if (state.mode !== 'layered') fail('Merging nodes needs layered stages.');
    const s = JSON.parse(JSON.stringify(state));
    const mats = s.matrices;
    if (!(set >= 1 && set <= mats.length + 1)) fail(`There is no node set ${set}.`);
    const out = set - 1 < mats.length ? set - 1 : -1; // stage whose rows are this set
    const inc = set - 2; // stage whose columns are this set (-1: none)
    const count = out >= 0 ? mats[out].length : mats[inc][0].length;
    const ids = [...new Set(indices)].sort((a, b) => a - b);
    if (ids.length < 2) fail('Choose at least two nodes to merge.');
    if (ids.some((i) => !(Number.isInteger(i) && i >= 0 && i < count))) fail(`Node set ${set} has no node ${ids.map((i) => i + 1).join(', ')}.`);
    const keep = ids[0];
    const survivors = Array.from({ length: count }, (_, i) => i).filter((i) => i === keep || !ids.includes(i));
    const to = (i) => survivors.indexOf(ids.includes(i) ? keep : i);
    // Node totals before merging, to place the merged node.
    const totals = Array.from({ length: count }, (_, i) =>
      Math.max(out >= 0 ? sum(mats[out][i]) : 0, inc >= 0 ? sum(mats[inc].map((r) => r[i])) : 0));

    // Rows of `rows` for the surviving nodes; the kept node's row combines the merged rows.
    const collapse = (rows, combine) => survivors.map((i) => (i === keep ? combine(ids.map((k) => rows[k]), ids) : rows[i]));
    const addRows = (rows) => rows[0].map((_, j) => sum(rows.map((r) => r[j])));
    // Per cell, the entry (e.g. a color) belonging to the merged node with the largest flow there.
    const pickRows = (flows) => (rows, members) => rows[0].map((_, j) => {
      let best = 0;
      members.forEach((k, m) => { if ((flows[k][j] || 0) > (flows[members[best]][j] || 0)) best = m; });
      return rows[best][j];
    });
    const companions = [s.colorMatrices, s.midMatrices].filter(Boolean);
    if (out >= 0) {
      const flows = mats[out];
      companions.forEach((list) => { if (list[out]) list[out] = collapse(list[out], pickRows(flows)); });
      mats[out] = collapse(flows, addRows);
    }
    if (inc >= 0) {
      const flows = transpose(mats[inc]);
      companions.forEach((list) => { if (list[inc]) list[inc] = transpose(collapse(transpose(list[inc]), pickRows(flows))); });
      mats[inc] = transpose(collapse(flows, addRows));
    }
    const renameNames = (names) => (Array.isArray(names) ? survivors.map((i) => (i === keep ? name : names[i])) : names);
    if (out >= 0 && s.dimnames[out]) s.dimnames[out].rows = renameNames(s.dimnames[out].rows);
    if (inc >= 0 && s.dimnames[inc]) s.dimnames[inc].cols = renameNames(s.dimnames[inc].cols);

    // Palette slots (node positions in loc, unless pinned already) of every node, by key, so the
    // surviving nodes keep their colors once positions shift.
    const slotOf = new Map(s.loc.map((r, id) => {
      const key = `${r.set}:${r.node}`;
      return [r, s.nodeSlots && s.nodeSlots[key] !== undefined ? s.nodeSlots[key] : id];
    }));

    // loc: drop the merged rows, renumber the set, and move the kept node to the weighted mean y.
    const rows = s.loc.filter((r) => Number(r.set) === set);
    const byNode = new Map(rows.map((r) => [Number(r.node) - 1, r]));
    const kept = byNode.get(keep);
    if (kept) {
      const weights = ids.map((i) => totals[i]);
      const members = ids.map((i) => byNode.get(i)).filter(Boolean);
      if (members.length === ids.length && sum(weights) > 0) {
        kept.y = ids.reduce((t, i, m) => t + Number(byNode.get(i).y) * weights[m], 0) / sum(weights);
      }
      if (members.some((r) => r.label)) kept.label = name;
    }
    s.loc = s.loc.filter((r) => Number(r.set) !== set || !ids.includes(Number(r.node) - 1) || Number(r.node) - 1 === keep);
    s.loc.forEach((r) => { if (Number(r.set) === set) r.node = to(Number(r.node) - 1) + 1; });
    s.nodeSlots = Object.fromEntries(s.loc.map((r) => [`${r.set}:${r.node}`, slotOf.get(r)]));

    // Node colors keyed "set:node": the kept node keeps its own, the others' shift down.
    const remapNodes = (obj) => {
      const next = {};
      Object.entries(obj || {}).forEach(([key, v]) => {
        const [ks, kn] = key.split(':').map(Number);
        if (ks !== set) next[key] = v;
        else if (kn - 1 === keep || !ids.includes(kn - 1)) next[`${ks}:${to(kn - 1) + 1}`] = v;
      });
      return next;
    };
    s.nodeColors = remapNodes(s.nodeColors);
    s.mergeColors = remapNodes(s.mergeColors);
    // Strand overrides keyed "stage:i:j": strands of merged nodes are new strands, so theirs go.
    const remapFlows = (obj) => {
      const next = {};
      Object.entries(obj || {}).forEach(([key, v]) => {
        const [st, i, j] = key.split(':').map(Number);
        const k = st === out ? i : st === inc ? j : -1;
        if (k < 0) next[key] = v;
        else if (!ids.includes(k)) next[st === out ? `${st}:${to(i)}:${j}` : `${st}:${i}:${to(j)}`] = v;
      });
      return next;
    };
    s.flowOverrides = remapFlows(s.flowOverrides);
    s.midOverrides = remapFlows(s.midOverrides);
    // Strand orders at this set and its neighbours list strands whose keys change; they reset.
    ['start', 'end'].forEach((side) => {
      Object.keys(s.manualOrder[side] || {}).forEach((key) => {
        if (Math.abs(Number(key.split(':')[0]) - set) <= 1) delete s.manualOrder[side][key];
      });
    });

    if (s.paths) {
      const merged = new Map();
      s.paths.forEach((p) => {
        const nodes = p.nodes.slice();
        nodes[set - 1] = to(nodes[set - 1]);
        const key = nodes.join(',');
        if (!merged.has(key)) merged.set(key, p.members ? { nodes, weight: 0, members: [] } : { nodes, weight: 0 });
        const m = merged.get(key);
        m.weight += p.weight;
        if (p.members) m.members.push(...p.members);
      });
      s.paths = [...merged.values()];
    }
    s.view = null;
    return s;
  }

  SK.edit = { mergeNodes };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
