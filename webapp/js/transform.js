/* Table import: turn long or wide data tables into Sankey flow matrices.
 *
 * Every converter returns { mode, sets (layered) | nodes (graph), matrices, stageNames, rowsUsed,
 * rowsDropped }, which postProcess() orders/groups and finish() turns into matrices + dimnames.
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const fail = (message) => { throw new SK.layout.SankeyError(message); };
  const isNA = (s) => s === null || s === undefined || /^(|na|nan|null)$/i.test(String(s).trim());
  const isNum = (s) => !isNA(s) && Number.isFinite(Number(String(s).trim()));
  const natural = (a, b) => String(a).localeCompare(String(b), undefined, { numeric: true, sensitivity: 'base' });
  const MISSING = '(missing)';
  const SEP = '\u0000';

  const FORMATS = ['wide', 'long', 'links', 'crosstab'];

  function parseTable(text) {
    const rows = SK.csv.parse(text);
    if (rows.length < 2) fail('The table needs a header row and at least one data row.');
    const width = rows[0].length;
    rows.forEach((r, i) => {
      if (r.length !== width) fail(`Table row ${i + 1} has ${r.length} cells, but the header has ${width}.`);
    });
    const header = rows[0].map((c) => c.trim());
    if (header.every((h) => isNum(h))) fail('The table needs a header row that names its columns.');
    const columns = header.map((h, j) => h || `column ${j + 1}`);
    const seen = new Set();
    columns.forEach((c) => {
      if (seen.has(c)) fail(`The table has two columns named "${c}".`);
      seen.add(c);
    });
    return { columns, rows: rows.slice(1).map((r) => r.map((c) => c.trim())) };
  }

  function columnInfo(table) {
    return table.columns.map((name, j) => {
      const values = table.rows.map((r) => r[j]);
      const present = values.filter((v) => !isNA(v));
      return {
        name,
        numeric: present.length > 0 && present.every(isNum),
        distinct: new Set(present).size,
        missing: values.length - present.length
      };
    });
  }

  const WEIGHT_RE = /^(value|values|weight|weights|count|counts|n|flow|amount|size|freq|frequency|total)$/i;
  const STAGE_RE = /^(stage|step|time|wave|visit|year|month|week|day|date|period|phase|round|layer|t)$/i;
  const ID_RE = /^(id|item|subject|person|patient|case|entity|unit|participant|user|respondent|individual|customer|member|sample)s?$|(^|[_\s.])id$|[a-z]Id$/i;

  // A column whose values repeat across stages but never twice within one stage is an item id.
  function pairUniqueColumn(table, info, stage) {
    const s = table.columns.indexOf(stage);
    const uniquePerStage = (j) => {
      const seen = new Set();
      return table.rows.every((r) => {
        if (isNA(r[j]) || isNA(r[s])) return true;
        const key = r[j] + SEP + r[s];
        if (seen.has(key)) return false;
        seen.add(key);
        return true;
      });
    };
    const candidates = info.filter((c, j) => j !== s && c.distinct > 1 && c.distinct < table.rows.length && uniquePerStage(j));
    candidates.sort((a, b) => b.distinct - a.distinct);
    return candidates.length ? candidates[0].name : null;
  }

  // A best guess at the table layout and column roles; the dialog lets users change it.
  function guess(table) {
    const info = columnInfo(table);
    const find = (re, used, numericOnly) => {
      const hit = info.find((c) => re.test(c.name) && !(used || []).includes(c.name) && (!numericOnly || c.numeric));
      return hit ? hit.name : null;
    };
    const source = find(/^(source|from|origin|src)$/i);
    const target = find(/^(target|to|destination|dest|dst)$/i);
    if (source && target) {
      return {
        format: 'links', source, target,
        value: find(WEIGHT_RE, [source, target], true),
        stage: find(/^(stage|step|layer|level|period)$/i, [source, target])
      };
    }
    const stage = find(STAGE_RE);
    // Long data repeats each item across stages, so a usable id column has repeated values.
    const named = stage ? find(ID_RE, [stage]) : null;
    const repeats = named && info.find((c) => c.name === named).distinct < table.rows.length;
    const id = stage ? ((repeats && named) || pairUniqueColumn(table, info, stage)) : null;
    if (id && stage && info.length >= 3) {
      const weight = find(WEIGHT_RE, [id, stage], true);
      const category = (info.find((c) => ![id, stage, weight].includes(c.name) && !c.numeric) ||
        info.find((c) => ![id, stage, weight].includes(c.name)) || {}).name || null;
      return { format: 'long', id, stage, category, weight, stageOrder: 'auto' };
    }
    const text = info.filter((c) => !c.numeric);
    const numeric = info.filter((c) => c.numeric);
    // Crosstabs have one row per category; a long column of unique ids is a wide table instead.
    const first = info[0];
    const firstIdLike = ID_RE.test(first.name) || table.rows.length > 30;
    if (info.length >= 3 && !first.numeric && numeric.length === info.length - 1 &&
        first.distinct === table.rows.length && !firstIdLike) {
      return { format: 'crosstab' };
    }
    const weightCandidates = numeric.filter((c) => !STAGE_RE.test(c.name));
    let weight = find(WEIGHT_RE, [], true);
    if (!weight && text.length >= 2 && weightCandidates.length === 1) weight = weightCandidates[0].name;
    // Ids and other many-valued columns would make one node per row, so they are not stages.
    const idLike = (c) => ID_RE.test(c.name) || (!c.numeric && c.distinct > Math.max(12, table.rows.length / 2));
    const columns = info.filter((c) => c.name !== weight && !idLike(c) && (!c.numeric || c.distinct <= 12)).map((c) => c.name);
    return { format: 'wide', columns, weight };
  }

  // True when column names mark a data table (link list, long table, a named weight column or a
  // leading id column) rather than a numeric flow matrix with a header.
  function looksLikeDataTable(table) {
    const g = guess(table);
    if (g.format === 'links' || g.format === 'long') return true;
    return g.format === 'wide' && ((!!g.weight && WEIGHT_RE.test(g.weight)) || ID_RE.test(table.columns[0]));
  }

  function colIndex(table, name, role) {
    const j = table.columns.indexOf(name);
    if (j < 0) fail(`Choose a ${role} column.`);
    return j;
  }

  function readWeight(row, j, line) {
    if (j < 0) return 1;
    if (!isNum(row[j])) fail(`Row ${line}: "${row[j]}" in the weight column is not a number.`);
    const v = Number(row[j]);
    if (v < 0) fail(`Row ${line}: weights must be non-negative.`);
    return v;
  }

  function stageOrder(values, mode) {
    const unique = [...new Set(values)];
    if (mode === 'alphabetical') return unique.sort(natural);
    if ((mode === 'auto' || mode === undefined) && unique.every(isNum)) return unique.sort((a, b) => Number(a) - Number(b));
    return unique;
  }

  // Wide: one row per item (or per item group, with a weight), one column per stage.
  function fromWide(table, spec) {
    const columns = spec.columns || [];
    if (columns.length < 2) fail('Choose at least two stage columns.');
    const idx = columns.map((c) => colIndex(table, c, 'stage'));
    const w = spec.weight ? colIndex(table, spec.weight, 'weight') : -1;
    const sets = columns.map(() => new Map());
    const counts = columns.slice(1).map(() => new Map());
    // Whole trajectories (one node per stage) are kept so individual pathways can be traced.
    const pathWeights = new Map();
    let used = 0;
    let dropped = 0;
    table.rows.forEach((r, i) => {
      let cats = idx.map((j) => r[j]);
      if (cats.some(isNA)) {
        if (spec.missing !== 'keep') {
          dropped++;
          return;
        }
        cats = cats.map((c) => (isNA(c) ? MISSING : c));
      }
      const v = readWeight(r, w, i + 2);
      cats.forEach((c, k) => { if (!sets[k].has(c)) sets[k].set(c, sets[k].size); });
      for (let k = 0; k < cats.length - 1; k++) {
        const key = cats[k] + SEP + cats[k + 1];
        counts[k].set(key, (counts[k].get(key) || 0) + v);
      }
      const pathKey = cats.map((c, k) => sets[k].get(c)).join(',');
      pathWeights.set(pathKey, (pathWeights.get(pathKey) || 0) + v);
      used++;
    });
    if (!used) fail('No rows remain: every row has a missing value in a chosen stage column.');
    const names = sets.map((m) => [...m.keys()]);
    const matrices = counts.map((m, k) => {
      const mat = names[k].map(() => names[k + 1].map(() => 0));
      m.forEach((v, key) => {
        const [a, b] = key.split(SEP);
        mat[sets[k].get(a)][sets[k + 1].get(b)] += v;
      });
      return mat;
    });
    const stageNames = columns.slice(0, -1).map((c, k) => `${c} → ${columns[k + 1]}`);
    const paths = [...pathWeights].filter(([, weight]) => weight > 0)
      .map(([key, weight]) => ({ nodes: key.split(',').map(Number), weight }));
    return { mode: 'layered', sets: names, matrices, stageNames, paths, rowsUsed: used, rowsDropped: dropped };
  }

  // Long: one row per (item, stage) with the item's category at that stage.
  function fromLong(table, spec) {
    const iId = colIndex(table, spec.id, 'item id');
    const iStage = colIndex(table, spec.stage, 'stage');
    const iCat = colIndex(table, spec.category, 'category');
    if (new Set([iId, iStage, iCat]).size < 3) fail('The item id, stage and category columns must differ.');
    const w = spec.weight ? colIndex(table, spec.weight, 'weight') : -1;
    const items = new Map();
    let dropped = 0;
    table.rows.forEach((r, i) => {
      if (isNA(r[iId]) || isNA(r[iStage])) {
        dropped++;
        return;
      }
      if (!items.has(r[iId])) items.set(r[iId], { cats: new Map(), weight: null });
      const item = items.get(r[iId]);
      if (item.cats.has(r[iStage])) fail(`Item "${r[iId]}" appears twice at stage "${r[iStage]}".`);
      item.cats.set(r[iStage], r[iCat]);
      const v = readWeight(r, w, i + 2);
      if (item.weight !== null && item.weight !== v) fail(`Item "${r[iId]}" has different weights on different rows.`);
      item.weight = v;
    });
    if (!items.size) fail('No rows have both an item id and a stage.');
    const stages = stageOrder(table.rows.filter((r) => !isNA(r[iId]) && !isNA(r[iStage])).map((r) => r[iStage]), spec.stageOrder);
    if (stages.length < 2) fail('The stage column needs at least two distinct stages.');
    const wide = {
      columns: stages.concat(w >= 0 ? ['\u0001weight'] : []),
      rows: [...items.values()].map((item) => stages.map((s) => (item.cats.has(s) ? item.cats.get(s) : ''))
        .concat(w >= 0 ? [String(item.weight)] : []))
    };
    const out = fromWide(wide, { columns: stages, weight: w >= 0 ? '\u0001weight' : null, missing: spec.missing });
    out.rowsDropped += dropped;
    out.itemCount = items.size;
    return out;
  }

  // Links: aggregated or raw edges, with an optional stage column.
  function fromLinks(table, spec) {
    const iS = colIndex(table, spec.source, 'source');
    const iT = colIndex(table, spec.target, 'target');
    if (iS === iT) fail('The source and target columns must differ.');
    const iV = spec.value ? colIndex(table, spec.value, 'value') : -1;
    const iStage = spec.stage ? colIndex(table, spec.stage, 'stage') : -1;
    let dropped = 0;
    const links = [];
    table.rows.forEach((r, i) => {
      if (isNA(r[iS]) || isNA(r[iT]) || (iStage >= 0 && isNA(r[iStage]))) {
        dropped++;
        return;
      }
      links.push({ s: r[iS], t: r[iT], v: readWeight(r, iV, i + 2), stage: iStage >= 0 ? r[iStage] : null });
    });
    if (!links.length) fail('No complete links: every row is missing a source, target or stage.');

    if (iStage >= 0) {
      const stages = stageOrder(links.map((l) => l.stage), 'auto');
      const sets = Array.from({ length: stages.length + 1 }, () => new Map());
      const add = (k, name) => { if (!sets[k].has(name)) sets[k].set(name, sets[k].size); };
      links.forEach((l) => {
        const k = stages.indexOf(l.stage);
        add(k, l.s);
        add(k + 1, l.t);
      });
      const names = sets.map((m) => [...m.keys()]);
      const matrices = stages.map((_, k) => names[k].map(() => names[k + 1].map(() => 0)));
      links.forEach((l) => {
        const k = stages.indexOf(l.stage);
        matrices[k][sets[k].get(l.s)][sets[k + 1].get(l.t)] += l.v;
      });
      const stageNames = stages.map((s) => `${spec.stage} ${s}`);
      return { mode: 'layered', sets: names, matrices, stageNames, rowsUsed: links.length, rowsDropped: dropped };
    }

    const nodes = [];
    const index = new Map();
    links.forEach((l) => [l.s, l.t].forEach((name) => {
      if (!index.has(name)) {
        index.set(name, nodes.length);
        nodes.push(name);
      }
    }));
    links.forEach((l) => { if (l.s === l.t) fail(`"${l.s}" links to itself; self-loops cannot be drawn.`); });
    const n = nodes.length;
    const adj = nodes.map(() => nodes.map(() => 0));
    links.forEach((l) => { adj[index.get(l.s)][index.get(l.t)] += l.v; });
    const depth = longestPathDepth(adj, nodes);
    const layered = links.every((l) => depth[index.get(l.t)] === depth[index.get(l.s)] + 1);
    if (!layered) {
      const order = nodes.map((_, i) => i).sort((a, b) => (depth[a] - depth[b]) || (a - b));
      return {
        mode: 'graph',
        nodes: order.map((i) => nodes[i]),
        matrices: [order.map((i) => order.map((j) => adj[i][j]))],
        stageNames: ['links'],
        rowsUsed: links.length,
        rowsDropped: dropped
      };
    }
    const levels = Math.max(...depth) + 1;
    const sets = Array.from({ length: levels }, () => []);
    nodes.forEach((name, i) => sets[depth[i]].push(name));
    const pos = new Map();
    sets.forEach((set) => set.forEach((name, k) => pos.set(name, k)));
    const matrices = sets.slice(0, -1).map((set, k) => set.map(() => sets[k + 1].map(() => 0)));
    links.forEach((l) => { matrices[depth[index.get(l.s)]][pos.get(l.s)][pos.get(l.t)] += l.v; });
    return {
      mode: 'layered',
      sets,
      matrices,
      stageNames: sets.slice(0, -1).map((_, k) => `step ${k + 1}`),
      rowsUsed: links.length,
      rowsDropped: dropped
    };
  }

  function longestPathDepth(adj, names) {
    const n = adj.length;
    const indeg = adj[0].map((_, j) => adj.filter((r) => r[j] > 0).length);
    const depth = new Array(n).fill(0);
    const queue = indeg.map((d, i) => (d === 0 ? i : -1)).filter((i) => i >= 0);
    let seen = 0;
    while (queue.length) {
      const u = queue.shift();
      seen++;
      adj[u].forEach((v, j) => {
        if (v > 0) {
          depth[j] = Math.max(depth[j], depth[u] + 1);
          if (--indeg[j] === 0) queue.push(j);
        }
      });
    }
    if (seen < n) {
      const stuck = names.filter((_, i) => indeg[i] > 0).slice(0, 4);
      fail(`The links contain a cycle (involving ${stuck.map((s) => `"${s}"`).join(', ')}); Sankey flows must be acyclic.`);
    }
    return depth;
  }

  // Crosstab: first column = source labels, remaining columns = target labels with values.
  function fromCrosstab(table) {
    if (table.columns.length < 2) fail('A crosstab needs a label column and at least one value column.');
    const matrix = table.rows.map((r, i) => r.slice(1).map((cell, j) => {
      if (isNA(cell)) return 0;
      if (!isNum(cell) || Number(cell) < 0) {
        fail(`Row ${i + 2}, column "${table.columns[j + 1]}": "${cell}" is not a non-negative number.`);
      }
      return Number(cell);
    }));
    const rowsNames = table.rows.map((r, i) => (isNA(r[0]) ? `row ${i + 1}` : r[0]));
    if (new Set(rowsNames).size !== rowsNames.length) fail('Crosstab row labels must be unique.');
    return {
      mode: 'layered',
      sets: [rowsNames, table.columns.slice(1)],
      matrices: [matrix],
      stageNames: [`${table.columns[0]} → columns`],
      rowsUsed: table.rows.length,
      rowsDropped: 0
    };
  }

  const isSpecial = (name) => name === MISSING || /^Other( \(grouped\))?$/.test(name);

  // Group small nodes into "Other" (layered only) and order nodes within each set.
  function postProcess(result, options) {
    const opts = options || {};
    if (result.mode === 'graph') {
      if (opts.minShare > 0 || opts.topN > 0) {
        fail('Grouping small nodes into "Other" needs layered stages, but these links skip layers and form a graph.');
      }
      if (opts.order === 'alphabetical' || opts.order === 'size') {
        const m = result.matrices[0];
        const totals = result.nodes.map((_, i) => Math.max(m[i].reduce((a, b) => a + b, 0), m.reduce((a, r) => a + r[i], 0)));
        const order = result.nodes.map((_, i) => i).sort(opts.order === 'size'
          ? (a, b) => totals[b] - totals[a] || a - b
          : (a, b) => natural(result.nodes[a], result.nodes[b]));
        result.nodes = order.map((i) => result.nodes[i]);
        result.matrices = [order.map((i) => order.map((j) => m[i][j]))];
      }
      return result;
    }
    const mats = result.matrices;
    const K = result.sets.length;
    const totals = result.sets.map((set, k) => set.map((_, i) => Math.max(
      k > 0 ? mats[k - 1].reduce((a, r) => a + r[i], 0) : 0,
      k < mats.length ? mats[k][i].reduce((a, b) => a + b, 0) : 0
    )));
    const mapping = [];
    const newSets = [];
    for (let k = 0; k < K; k++) {
      const set = result.sets[k];
      const tot = totals[k];
      const setTotal = tot.reduce((a, b) => a + b, 0);
      const rank = set.map((_, i) => i).sort((a, b) => tot[b] - tot[a] || a - b);
      const small = new Set();
      if (opts.minShare > 0) set.forEach((_, i) => { if (tot[i] < opts.minShare * setTotal) small.add(i); });
      if (opts.topN > 0) rank.slice(opts.topN).forEach((i) => small.add(i));
      const otherName = set.includes('Other') ? 'Other (grouped)' : 'Other';
      const collapse = small.size >= 2;
      const names = [];
      const map = set.map((name, i) => {
        const target = collapse && small.has(i) ? otherName : name;
        let j = names.indexOf(target);
        if (j < 0) {
          names.push(target);
          j = names.length - 1;
        }
        return j;
      });
      const newTot = names.map(() => 0);
      map.forEach((j, i) => { newTot[j] += tot[i]; });
      let order = names.map((_, j) => j);
      if (opts.order === 'alphabetical') {
        order.sort((a, b) => (isSpecial(names[a]) - isSpecial(names[b])) || natural(names[a], names[b]));
      } else if (opts.order === 'size') {
        order.sort((a, b) => (isSpecial(names[a]) - isSpecial(names[b])) || (newTot[b] - newTot[a]) || (a - b));
      } else {
        order.sort((a, b) => (isSpecial(names[a]) - isSpecial(names[b])) || (a - b));
      }
      const position = new Map(order.map((j, p) => [j, p]));
      mapping.push(map.map((j) => position.get(j)));
      newSets.push(order.map((j) => names[j]));
    }
    result.matrices = mats.map((m, k) => {
      const out = newSets[k].map(() => newSets[k + 1].map(() => 0));
      m.forEach((row, i) => row.forEach((v, j) => { out[mapping[k][i]][mapping[k + 1][j]] += v; }));
      return out;
    });
    result.sets = newSets;
    if (result.paths) {
      // Re-index trajectories to the grouped/ordered nodes and merge any that now coincide.
      const merged = new Map();
      result.paths.forEach((p) => {
        const key = p.nodes.map((n, k) => mapping[k][n]).join(',');
        merged.set(key, (merged.get(key) || 0) + p.weight);
      });
      result.paths = [...merged].map(([key, weight]) => ({ nodes: key.split(',').map(Number), weight }));
    }
    return result;
  }

  function summarize(result) {
    const strands = result.matrices.reduce((s, m) => s + m.reduce((a, r) => a + r.filter((v) => v > 0).length, 0), 0);
    const nodes = result.mode === 'graph' ? result.nodes.length : result.sets.reduce((s, set) => s + set.length, 0);
    const total = result.mode === 'graph'
      ? result.matrices[0].reduce((s, r) => s + r.reduce((a, b) => a + b, 0), 0)
      : result.matrices[0].reduce((s, r) => s + r.reduce((a, b) => a + b, 0), 0);
    if (!strands) fail('The chosen columns produce no flows.');
    return { stages: result.matrices.length, nodes, strands, total, rowsUsed: result.rowsUsed, rowsDropped: result.rowsDropped };
  }

  // spec: { format, ...column roles, missing, stageOrder }, options: { order, minShare, topN }.
  function build(table, spec, options) {
    if (!FORMATS.includes(spec.format)) fail(`Unknown table layout "${spec.format}".`);
    const raw = spec.format === 'wide' ? fromWide(table, spec)
      : spec.format === 'long' ? fromLong(table, spec)
        : spec.format === 'links' ? fromLinks(table, spec)
          : fromCrosstab(table);
    const result = postProcess(raw, options);
    // What path weights count, for tooltips: the weight column, else rows (wide) or items (long).
    result.unit = spec.weight || (spec.format === 'long' ? 'items' : 'rows');
    result.dimnames = result.mode === 'graph'
      ? [{ rows: result.nodes, cols: result.nodes }]
      : result.matrices.map((_, k) => ({ rows: result.sets[k], cols: result.sets[k + 1] }));
    result.summary = summarize(result);
    return result;
  }

  SK.transform = { FORMATS, MISSING, parseTable, columnInfo, guess, looksLikeDataTable, build, postProcess };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
