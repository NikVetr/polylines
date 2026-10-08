// Table import converters. Run with: node --test tests/transform.test.js
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const js = (f) => path.join(__dirname, '..', 'js', f);
const SK = require(js('layout.js'));
require(js('color.js'));
require(js('csv.js'));
require(js('transform.js'));
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
  assert.deepStrictEqual(r.stageNames, ['2 → 10']);
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
  assert.deepStrictEqual(T.guess(table('q1,q2,q3,count\na,b,c,4\n')), { format: 'wide', columns: ['q1', 'q2', 'q3'], weight: 'count' });
  // An id column is recognised by name or by never repeating within a stage.
  const long = 'respondent,wave,answer\nR1,1,yes\nR1,2,no\nR2,1,no\nR2,2,no\nR3,1,yes\nR3,2,yes\n';
  assert.deepStrictEqual(T.guess(table(long)), { format: 'long', id: 'respondent', stage: 'wave', category: 'answer', weight: null, stageOrder: 'auto' });
  assert.strictEqual(T.guess(table(long.replace('respondent', 'who'))).id, 'who');
  // Many-valued id-like columns are not proposed as wide stages, nor stage-named numbers as weights.
  const rows = Array.from({ length: 20 }, (_, i) => `u${i},${i % 2 ? 'a' : 'b'},${i % 3 ? 'x' : 'y'},${2000 + (i % 2)}`);
  assert.deepStrictEqual(T.guess(table(`user,before,after,year\n${rows.join('\n')}\n`)),
    { format: 'wide', columns: ['before', 'after', 'year'], weight: null });
  // An id column followed by numeric codes is a wide table, not a crosstab.
  const coded = Array.from({ length: 40 }, (_, i) => `P${i},${i % 3},${(i + 1) % 3},${i % 2}`);
  assert.deepStrictEqual(T.guess(table(`patient,t1,t2,t3\n${coded.join('\n')}\n`)),
    { format: 'wide', columns: ['t1', 't2', 't3'], weight: null });
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
