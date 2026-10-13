// Table import converters. Run with: node --test tests/transform.test.js
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const js = (f) => path.join(__dirname, '..', 'js', f);
const SK = require(js('layout.js'));
require(js('color.js'));
require(js('csv.js'));
require(js('transform.js'));
require(js('shapes.js'));
require(js('render.js'));
require(js('table-controls.js'));
const T = SK.transform;

const table = (text) => T.parseTable(text);

test('wide tables count transitions between consecutive stage columns', () => {
  const t = table('diagnosis,treatment,outcome\nA,surgery,good\nA,surgery,poor\nB,drug,good\nA,drug,good\n');
  const r = T.build(t, { format: 'wide', columns: ['diagnosis', 'treatment', 'outcome'] });
  assert.deepStrictEqual(r.dimnames[0], { rows: ['A', 'B'], cols: ['surgery', 'drug'] });
  assert.deepStrictEqual(r.matrices[0], [[2, 1], [0, 1]]);
  assert.deepStrictEqual(r.matrices[1], [[1, 1], [2, 0]]);
  assert.strictEqual(r.summary.total, 4);
});

test('wide tables sum a weight column and handle missing values as asked', () => {
  const t = table('from,to,n\nA,X,10\nA,,5\nB,X,2.5\n');
  const dropped = T.build(t, { format: 'wide', columns: ['from', 'to'], weight: 'n' });
  assert.deepStrictEqual(dropped.matrices[0], [[10], [2.5]]);
  assert.strictEqual(dropped.summary.rowsDropped, 1);
  const kept = T.build(t, { format: 'wide', columns: ['from', 'to'], weight: 'n', missing: 'keep' });
  assert.deepStrictEqual(kept.dimnames[0].cols, ['X', '(missing)']);
  assert.deepStrictEqual(kept.matrices[0], [[10, 5], [2.5, 0]]);
  assert.throws(() => T.build(table('a,b,w\nA,X,ten\n'), { format: 'wide', columns: ['a', 'b'], weight: 'w' }), /not a number/);
});

test('long tables pivot by item with numeric stages in numeric order', () => {
  const t = table('id,visit,status\n1,2,sick\n1,10,well\n2,2,sick\n2,10,sick\n3,2,well\n3,10,well\n');
  const r = T.build(t, { format: 'long', id: 'id', stage: 'visit', category: 'status' });
  assert.deepStrictEqual(r.stageNames, ['visit 2 → visit 10']);
  assert.deepStrictEqual(r.dimnames[0], { rows: ['sick', 'well'], cols: ['well', 'sick'] });
  assert.deepStrictEqual(r.matrices[0], [[1, 1], [1, 0]]);
  assert.throws(() => T.build(table('id,visit,status\n1,1,a\n1,1,b\n'), { format: 'long', id: 'id', stage: 'visit', category: 'status' }),
    /appears twice/);
  assert.throws(() => T.build(table('id,t,s,w\n1,1,a,2\n1,2,b,3\n'), { format: 'long', id: 'id', stage: 't', category: 's', weight: 'w' }),
    /different weights/);
});

test('links with a stage column build one matrix per stage', () => {
  const t = table('stage,source,target,value\n1,A,M,3\n1,B,M,2\n2,M,X,4\n2,M,Y,1\n');
  const r = T.build(t, { format: 'links', source: 'source', target: 'target', value: 'value', stage: 'stage' });
  assert.strictEqual(r.mode, 'layered');
  assert.deepStrictEqual(r.dimnames.map((d) => d.rows), [['A', 'B'], ['M']]);
  assert.deepStrictEqual(r.matrices, [[[3], [2]], [[4, 1]]]);
});

test('links without stages are layered when possible, otherwise a graph, and cycles fail', () => {
  const layered = T.build(table('source,target,value\nA,B,2\nB,C,2\nA,D,1\n'), { format: 'links', source: 'source', target: 'target', value: 'value' });
  assert.strictEqual(layered.mode, 'layered');
  assert.deepStrictEqual(layered.dimnames.map((d) => d.rows), [['A'], ['B', 'D']]);
  const graph = T.build(table('source,target,value\nA,B,2\nB,C,2\nA,C,1\n'), { format: 'links', source: 'source', target: 'target', value: 'value' });
  assert.strictEqual(graph.mode, 'graph');
  assert.deepStrictEqual(graph.dimnames[0].rows, ['A', 'B', 'C']);
  assert.deepStrictEqual(graph.matrices[0], [[0, 2, 1], [0, 0, 2], [0, 0, 0]]);
  assert.throws(() => T.build(table('source,target\nA,B\nB,A\n'), { format: 'links', source: 'source', target: 'target' }), /cycle/);
});

test('crosstabs become a single labeled stage', () => {
  const r = T.build(table('from,X,Y\nA,1,2\nB,0,3\n'), { format: 'crosstab' });
  assert.deepStrictEqual(r.dimnames[0], { rows: ['A', 'B'], cols: ['X', 'Y'] });
  assert.deepStrictEqual(r.matrices[0], [[1, 2], [0, 3]]);
});

test('small nodes are grouped into Other and nodes can be ordered by size', () => {
  const t = table('a,b,n\nbig,X,90\nsmall1,X,4\nsmall2,Y,6\nbig,Y,10\n');
  const r = T.build(t, { format: 'wide', columns: ['a', 'b'], weight: 'n' }, { minShare: 0.1, order: 'size' });
  assert.deepStrictEqual(r.dimnames[0].rows, ['big', 'Other']);
  assert.deepStrictEqual(r.dimnames[0].cols, ['X', 'Y']);
  assert.deepStrictEqual(r.matrices[0], [[90, 10], [4, 6]]);
  const top = T.build(t, { format: 'wide', columns: ['a', 'b'], weight: 'n' }, { topN: 1, order: 'alphabetical' });
  assert.deepStrictEqual(top.dimnames[0].rows, ['big', 'Other']);
  // A single leftover node keeps its own name rather than becoming "Other".
  const one = T.build(t, { format: 'wide', columns: ['a', 'b'], weight: 'n' }, { topN: 2 });
  assert.deepStrictEqual(one.dimnames[0].rows, ['big', 'small1', 'small2']);
});

test('guesses the table layout from column names and types', () => {
  assert.strictEqual(T.guess(table('source,target,value\nA,B,1\n')).format, 'links');
  assert.deepStrictEqual(T.guess(table('id,year,state\n1,2020,a\n1,2021,b\n2,2020,a\n')),
    { format: 'long', id: 'id', stage: 'year', category: 'state', weight: null, stageOrder: 'auto' });
  assert.strictEqual(T.guess(table('from,X,Y\nA,1,2\n')).format, 'crosstab');
  assert.deepStrictEqual(T.guess(table('q1,q2,q3,count\na,b,c,4\n')), { format: 'wide', columns: ['q1', 'q2', 'q3'], weight: 'count', id: null });
  // An id column is recognised by name or by never repeating within a stage.
  const long = 'respondent,wave,answer\nR1,1,yes\nR1,2,no\nR2,1,no\nR2,2,no\nR3,1,yes\nR3,2,yes\n';
  assert.deepStrictEqual(T.guess(table(long)), { format: 'long', id: 'respondent', stage: 'wave', category: 'answer', weight: null, stageOrder: 'auto' });
  assert.strictEqual(T.guess(table(long.replace('respondent', 'who'))).id, 'who');
  // Many-valued id-like columns are not proposed as wide stages, nor stage-named numbers as weights;
  // a named id that is unique per row marks one row per item.
  const rows = Array.from({ length: 20 }, (_, i) => `u${i},${i % 2 ? 'a' : 'b'},${i % 3 ? 'x' : 'y'},${2000 + (i % 2)}`);
  assert.deepStrictEqual(T.guess(table(`user,before,after,year\n${rows.join('\n')}\n`)),
    { format: 'wide', columns: ['before', 'after', 'year'], weight: null, id: 'user' });
  // An id column followed by numeric codes is a wide table, not a crosstab.
  const coded = Array.from({ length: 40 }, (_, i) => `P${i},${i % 3},${(i + 1) % 3},${i % 2}`);
  assert.deepStrictEqual(T.guess(table(`patient,t1,t2,t3\n${coded.join('\n')}\n`)),
    { format: 'wide', columns: ['t1', 't2', 't3'], weight: null, id: 'patient' });
  assert.strictEqual(T.guess(table('id,before,after\na,1,2\nb,2,2\nc,1,1\n')).format, 'wide');
});

test('named data tables are recognised even when every cell is a number', () => {
  assert.strictEqual(T.looksLikeDataTable(table('source,target,value\n1,2,10\n1,3,5\n2,4,7\n')), true);
  assert.strictEqual(T.looksLikeDataTable(table('q1,q2,count\n1,2,4\n2,2,3\n')), true);
  assert.strictEqual(T.looksLikeDataTable(table('"","V1","V2"\n"1",0.1,0.2\n"2",0.3,0.4\n')), false);
});

test('grouping into Other is rejected for graph-shaped links', () => {
  const t = table('source,target,value\nA,B,2\nB,C,2\nA,C,1\n');
  assert.throws(() => T.build(t, { format: 'links', source: 'source', target: 'target', value: 'value' }, { topN: 1 }),
    /needs layered stages/);
});

const cohort = table('patient_id,sex,TR00,TR01,TR02,n\nP1,Female,A,X,Z,8\nP2,Male,A,Y,Z,4\nP3,Female,B,Y,W,6\nP4,Male,B,X,W,2\nP5,Female,A,Y,Z,3\n');
const cohortSpec = { format: 'wide', id: 'patient_id', columns: ['TR00', 'TR01', 'TR02'], weight: 'n' };

test('row filters intersect source columns, retain literal blanks and reject empty subsets', () => {
  assert.deepStrictEqual(T.filterTable(cohort, { sex: ['Female'], TR01: ['Y'] }).rows.map((r) => r[0]), ['P3', 'P5']);
  const blanks = table('a,b\n,X\nNA,Y\nA,Z\n');
  assert.deepStrictEqual(T.filterTable(blanks, { a: [''] }).rows, [['', 'X']]);
  assert.throws(() => T.filterTable(cohort, { sex: [] }), /No rows match/);
  assert.throws(() => T.filterTable(cohort, { sex: ['absent'] }), /absent from the source/);
  assert.throws(() => T.filterTable(cohort, { sex: ['Female'], TR01: [] }), /No rows match/);
  assert.strictEqual(cohort.rows.length, 5);
});

test('table controls rebuild filtered weighted trajectories after hiding and reordering columns', () => {
  const C = SK.tableControls;
  const initial = T.build(cohort, cohortSpec);
  const source = C.create(initial.source, { name: 'cohort.csv', labels: 'above' });
  assert.deepStrictEqual(source.order, ['TR00', 'TR01', 'TR02', 'sex']);
  assert.deepStrictEqual(source.enabled, ['TR00', 'TR01', 'TR02']);
  const changed = Object.assign({}, source, { order: ['TR02', 'TR00', 'TR01', 'sex'], enabled: ['TR00', 'TR02'], filters: { sex: ['Female'] } });
  const { result, source: next } = C.build(changed);
  assert.deepStrictEqual(result.setNames, ['TR02', 'TR00']);
  assert.deepStrictEqual(result.matrices, [[[11, 0], [0, 6]]]);
  assert.strictEqual(result.summary.total, 17);
  assert.strictEqual(next.summary.rowsFiltered, 3);
  assert.strictEqual(result.paths.length, 2);
  assert.deepStrictEqual(result.paths[0].members.map((m) => m.id), ['P1', 'P5']);
  assert.strictEqual(source.table.rows.length, 5);
  assert.strictEqual(C.build(source).result.summary.total, 23);
});

test('long table stages can be disabled and reordered before checking complete trajectories', () => {
  const t = table('id,visit,status,sex\nA,3,C,Female\nA,1,A,Female\nA,2,B,Female\nB,3,D,Male\nB,1,A,Male\nB,2,B,Male\nC,3,C,Female\nC,1,B,Female\n');
  const spec = { format: 'long', id: 'id', stage: 'visit', category: 'status', stageOrder: 'appearance' };
  assert.deepStrictEqual(T.diagramColumns(t, spec), ['3', '1', '2']);
  const source = SK.tableControls.create(T.build(t, spec).source);
  const next = Object.assign({}, source, { order: ['3', '1', '2'], enabled: ['3', '1'], filters: { sex: ['Female'] } });
  const out = SK.tableControls.build(next);
  assert.deepStrictEqual(out.result.setNames, ['visit 3', 'visit 1']);
  assert.strictEqual(out.result.summary.total, 2);
  assert.deepStrictEqual(out.result.matrices, [[[1, 1]]]);
  assert.throws(() => T.build(t, Object.assign({}, spec, { stages: ['3', '3'] })), /only once/);
  assert.throws(() => T.build(t, Object.assign({}, spec, { stages: ['3', 'absent'] })), /no rows after filtering/);
  const duplicateUnused = { columns: t.columns, rows: t.rows.concat([['A', '2', 'unused', 'Female']]) };
  assert.strictEqual(T.build(duplicateUnused, Object.assign({}, spec, { stages: ['3', '1'] })).summary.total, 3);
});

test('stage filters keep the items whose category at a stage is kept, with all their rows', () => {
  const t = table('id,visit,status\nA,1,A\nA,2,B\nB,1,A\nB,2,C\nC,2,B\n');
  const spec = { format: 'long', id: 'id', stage: 'visit', category: 'status' };
  assert.deepStrictEqual(T.stageValues(t, spec, '2'), ['B', 'C']);
  assert.strictEqual(T.stageLabel(spec, '2'), 'visit 2');
  assert.deepStrictEqual(T.filterTable(t, {}, spec, { 2: ['B'] }).rows.map((r) => r[0]), ['A', 'A', 'C']);
  // Items without a row at the filtered stage stay.
  assert.deepStrictEqual(T.filterTable(t, {}, spec, { 1: ['A'] }).rows.map((r) => r[0]), ['A', 'A', 'B', 'B', 'C']);
  assert.throws(() => T.filterTable(t, {}, spec, { 2: ['A'] }), /absent at that stage/);
  assert.throws(() => T.filterTable(cohort, {}, cohortSpec, { TR00: ['A'] }), /need a long table/);
  assert.throws(() => T.filterTable(t, { status: ['C'] }, spec, { 2: ['B'] }), /No rows match/);
});

test('table controls keep or drop single values and columns, and move columns', () => {
  const C = SK.tableControls;
  const source = C.create(T.build(cohort, cohortSpec).source);
  const noA = C.withValue(source, 'TR00', 'A');
  assert.deepStrictEqual(noA.filters, { TR00: ['B'] });
  assert.deepStrictEqual(C.valuesOf(noA, 'TR00'), { values: ['A', 'B'], kept: new Set(['B']) });
  // A single kept value hides its column, as a single-value row filter does.
  assert.deepStrictEqual(noA.enabled, ['TR01', 'TR02']);
  assert.deepStrictEqual(C.withValue(noA, 'TR00', 'A').filters, {});
  assert.throws(() => C.withValue(noA, 'TR00', 'B'), /at least one value of TR00/);
  assert.throws(() => C.withColumn(C.withColumn(source, 'TR01', false), 'TR00', false), /at least two/);
  assert.deepStrictEqual(C.withColumn(source, 'sex', true).enabled, ['TR00', 'TR01', 'TR02', 'sex']);
  assert.deepStrictEqual(C.withMove(source, 'sex', 0).order, ['sex', 'TR00', 'TR01', 'TR02']);
  assert.deepStrictEqual(C.withMove(source, 'TR00', 9).order, ['TR01', 'TR02', 'sex', 'TR00']);
  const long = C.create(T.build(table('id,visit,status\nA,1,A\nA,2,B\nB,1,A\nB,2,C\nC,1,B\nC,2,B\n'),
    { format: 'long', id: 'id', stage: 'visit', category: 'status' }).source);
  const noC = C.withValue(long, '2', 'C');
  assert.deepStrictEqual(noC.stageFilters, { 2: ['B'] });
  assert.deepStrictEqual(noC.filters, {});
  assert.strictEqual(C.build(noC).result.summary.total, 2);
  assert.strictEqual(C.header(noC, 400).lines.flat().map((part) => part.text).join(''), 'status at visit 2 ∈ {B}');
  assert.strictEqual(C.columnLabel(long, '2'), 'visit 2');
});

test('subset captions wrap long values, escape SVG markup and bold column names', () => {
  const source = { filters: { 'sex & group': ['Female <1>', 'A'.repeat(140)] } };
  const header = SK.tableControls.header(source, 200);
  assert.ok(header.lines.length > 1);
  assert.ok(header.markup.includes('font-weight="600">sex &amp; group'));
  assert.ok(header.markup.includes('Female &lt;1&gt;'));
  assert.strictEqual(header.lines.flat().map((p) => p.text).join(''), 'sex & group ∈ {Female <1>, ' + 'A'.repeat(140) + '}');
  assert.strictEqual(SK.tableControls.header(null, 200).height, 0);
});

test('single-value filters hide matching columns and expanding them restores their order', () => {
  const result = T.build(T.parseTable('a,sex,b,c\nA,Female,B,C\nD,Male,E,F\n'),
    { format: 'wide', columns: ['a', 'sex', 'b', 'c'] });
  const original = SK.tableControls.create(result.source);
  const filtered = SK.tableControls.withFilters(original, { sex: ['Female'] });
  assert.deepStrictEqual(filtered.enabled, ['a', 'b', 'c']);
  assert.deepStrictEqual(filtered.autoHidden, ['sex']);
  assert.deepStrictEqual(SK.tableControls.build(filtered).result.setNames, ['a', 'b', 'c']);
  const restored = SK.tableControls.withFilters(filtered, {});
  assert.deepStrictEqual(SK.tableControls.build(restored).result.setNames, ['a', 'sex', 'b', 'c']);
  assert.deepStrictEqual(restored.autoHidden, []);
  // Already-hidden columns stay hidden, and the diagram keeps its minimum two columns.
  const manual = Object.assign({}, original, { enabled: ['a', 'b'] });
  const limited = SK.tableControls.withFilters(manual, { sex: ['Female'], a: ['A'] });
  assert.deepStrictEqual(limited.enabled, ['a', 'b']);
  assert.deepStrictEqual(SK.tableControls.withFilters(limited, {}).enabled, ['a', 'b']);
  assert.deepStrictEqual(original.enabled, ['a', 'sex', 'b', 'c']);
});
