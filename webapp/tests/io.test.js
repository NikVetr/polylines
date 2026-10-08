// CSV parsing of the files the R functions use. Run with: node --test tests/io.test.js
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const SK = require(path.join(__dirname, '..', 'js', 'layout.js'));
require(path.join(__dirname, '..', 'js', 'color.js'));
require(path.join(__dirname, '..', 'js', 'csv.js'));

test('reads matrices written by R write.csv(), with and without dimnames', () => {
  const plain = SK.csv.parseMatrix('"","V1","V2"\n"1",0.1,0.02\n"2",0.05,0.08\n', 'number', 'plain');
  assert.deepStrictEqual(plain.values, [[0.1, 0.02], [0.05, 0.08]]);
  const named = SK.csv.parseMatrix('"","Out A","Out B"\n"In 1",0.1,0.02\n"In 2",0.05,0.08\n', 'number', 'named');
  assert.deepStrictEqual(named.rowNames, ['In 1', 'In 2']);
  assert.deepStrictEqual(named.colNames, ['Out A', 'Out B']);
  const colors = SK.csv.parseMatrix('"","V1","V2"\n"1","#FF0000","#0000FF80"\n"2",NA,"gray20"\n', 'color', 'cols');
  assert.deepStrictEqual(colors.values, [['#FF0000', '#0000FF80'], [null, '#333333']]);
});

test('headerless matrices keep their first row, including with row names', () => {
  assert.deepStrictEqual(SK.csv.parseMatrix('0.1,0.2\n0.3,0.4\n', 'number', 'm').values, [[0.1, 0.2], [0.3, 0.4]]);
  const rowNamed = SK.csv.parseMatrix('A,1,2\nB,3,4\nC,5,6\n', 'number', 'm');
  assert.deepStrictEqual(rowNamed.rowNames, ['A', 'B', 'C']);
  assert.deepStrictEqual(rowNamed.values, [[1, 2], [3, 4], [5, 6]]);
});

test('a typo or NA in the first data row fails instead of becoming a header', () => {
  assert.throws(() => SK.csv.parseMatrix('1,0.2o\n2,3\n', 'number', 'typo'), /not a number/);
  assert.throws(() => SK.csv.parseMatrix('0.1,NA\n0.2,0.3\n', 'number', 'na'), /not a number/);
  assert.throws(() => SK.csv.parseMatrix('#FF0000,#GG0000\n#00FF00,#0000FF\n', 'color', 'bad'), /not a color/);
});

test('reads loc tables in R data-frame and headerless matrix forms', () => {
  const df = SK.csv.parseLoc('"","set","node","x","y","label"\n"1",1,1,0,0.7,"a"\n"2",1,2,0,0.3,NA\n');
  assert.deepStrictEqual(df, [{ set: 1, node: 1, x: 0, y: 0.7, label: 'a' }, { set: 1, node: 2, x: 0, y: 0.3, label: '' }]);
  const mat = SK.csv.parseLoc('1,1,0,0.7,0.1\n1,2,0,0.3,0.1\n');
  assert.deepStrictEqual(mat[1], { set: 1, node: 2, x: 0, y: 0.3, width: 0.1 });
  assert.throws(() => SK.csv.parseLoc('set,node,x,y\n1,1,0,abc\n'), /not a number/);
});
