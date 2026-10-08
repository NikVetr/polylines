/* CSV reading/writing for the files the R functions use: flow matrices, color matrices and loc. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const fail = (message) => { throw new SK.layout.SankeyError(message); };

  function detectDelimiter(text) {
    const first = text.split(/\r?\n/).find((line) => line.trim() !== '') || '';
    const counts = [',', '\t', ';'].map((d) => [d, first.split(d).length - 1]);
    counts.sort((a, b) => b[1] - a[1]);
    return counts[0][1] > 0 ? counts[0][0] : ',';
  }

  function parse(text) {
    const src = String(text).replace(/^﻿/, '');
    const delim = detectDelimiter(src);
    const rows = [];
    let row = [];
    let field = '';
    let quoted = false;
    for (let i = 0; i < src.length; i++) {
      const ch = src[i];
      if (quoted) {
        if (ch === '"' && src[i + 1] === '"') {
          field += '"';
          i++;
        } else if (ch === '"') {
          quoted = false;
        } else {
          field += ch;
        }
      } else if (ch === '"') {
        quoted = true;
      } else if (ch === delim) {
        row.push(field);
        field = '';
      } else if (ch === '\n' || ch === '\r') {
        if (ch === '\r' && src[i + 1] === '\n') i++;
        row.push(field);
        rows.push(row);
        row = [];
        field = '';
      } else {
        field += ch;
      }
    }
    if (quoted) fail('The CSV file has an unterminated quoted field.');
    if (field !== '' || row.length) {
      row.push(field);
      rows.push(row);
    }
    return rows.filter((r) => r.some((cell) => cell.trim() !== ''));
  }

  const isNumber = (s) => s.trim() !== '' && Number.isFinite(Number(s.trim()));
  const isNA = (s) => /^(|na|nan|null)$/i.test(s.trim());

  // Matrices may carry a header row and/or a row-name column (e.g. from R's write.csv()).
  function parseMatrix(text, kind, label) {
    const rows = parse(text);
    if (!rows.length) fail(`${label} is empty.`);
    const valid = kind === 'color' ? (s) => isNA(s) || SK.color.isColor(s) : isNumber;
    // Names never look like mistyped data (digits, signs, dots, #) or NA, so a typo in the first
    // data row fails loudly instead of turning that row into a header.
    const nameLike = (s) => !valid(s) && !isNA(s) && !/^[-+.\d#]/.test(s.trim());
    let body = rows;
    let colNames = null;
    let rowNames = null;
    const headerFirstBlank = body[0][0].trim() === '';
    const headerCells = body[0].length > 1 ? body[0].slice(1) : body[0];
    if (headerFirstBlank || headerCells.every(nameLike)) {
      colNames = body[0].map((c) => c.trim());
      body = body.slice(1);
    }
    if (!body.length) fail(`${label} has a header but no data rows.`);
    const firstColumnNames = (colNames && headerFirstBlank && colNames.length === body[0].length) ||
      body.every((r) => nameLike(r[0]));
    if (firstColumnNames) {
      rowNames = body.map((r) => r[0].trim());
      body = body.map((r) => r.slice(1));
      if (colNames && colNames.length === body[0].length + 1) colNames = colNames.slice(1);
    }
    const width = body[0].length;
    const values = body.map((r, i) => {
      if (r.length !== width) fail(`${label} row ${i + 1} has ${r.length} values; expected ${width}.`);
      return r.map((cell, j) => {
        if (kind === 'color') {
          if (isNA(cell)) return null;
          if (!SK.color.isColor(cell)) fail(`${label} [${i + 1}, ${j + 1}]: "${cell.trim()}" is not a color.`);
          return SK.color.toHex(SK.color.parse(cell));
        }
        if (!isNumber(cell)) fail(`${label} [${i + 1}, ${j + 1}]: "${cell.trim()}" is not a number.`);
        return Number(cell.trim());
      });
    });
    return { values, rowNames, colNames };
  }

  const LOC_ALIASES = {
    node_width: 'width',
    name: 'label',
    node_body_height: 'height',
    col: 'color',
    node_col: 'color',
    colour: 'color',
    merge_colour: 'merge_col'
  };
  const LOC_NUMERIC = ['set', 'node', 'x', 'y', 'width', 'height', 'cex', 'font'];

  // loc: named columns set, node, x, y [, width, label, cex, font, height, color, merge_col], or
  // headerless numeric columns set, node, x, y [, width [, height]] like the R matrix form.
  function parseLoc(text) {
    const rows = parse(text);
    if (!rows.length) fail('The loc file is empty.');
    const first = rows[0].map((c) => c.trim());
    const hasHeader = first.some((c) => c !== '' && !isNumber(c)) || first[0] === '';
    let names;
    let body;
    if (hasHeader) {
      names = first.map((c) => c.toLowerCase());
      body = rows.slice(1);
      if (names[0] === '') {
        names = names.slice(1);
        body = body.map((r) => r.slice(1));
      }
      names = names.map((n) => LOC_ALIASES[n] || n);
      if (!['set', 'node', 'x', 'y'].every((n) => names.includes(n))) {
        if (names.length < 4) fail('loc needs at least four columns: set, node, x, y.');
        names = ['set', 'node', 'x', 'y', ...names.slice(4)];
      }
    } else {
      body = rows;
      if (rows[0].length < 4 || rows[0].length > 6) {
        fail('A headerless loc file needs 4-6 numeric columns: set, node, x, y [, width [, height]].');
      }
      names = ['set', 'node', 'x', 'y', 'width', 'height'].slice(0, rows[0].length);
    }
    if (!body.length) fail('The loc file has no data rows.');
    return body.map((r, i) => {
      const out = {};
      names.forEach((name, j) => {
        const cell = r[j] === undefined ? '' : r[j].trim();
        if (LOC_NUMERIC.includes(name)) {
          if (isNA(cell)) {
            if (['set', 'node', 'x', 'y'].includes(name)) fail(`loc row ${i + 1}: ${name} is missing.`);
            out[name] = null;
          } else if (!isNumber(cell)) {
            fail(`loc row ${i + 1}: ${name} "${cell}" is not a number.`);
          } else {
            out[name] = Number(cell);
          }
        } else if (name === 'label') {
          out.label = isNA(cell) ? '' : cell;
        } else if (name === 'color' || name === 'merge_col') {
          if (!isNA(cell) && !SK.color.isColor(cell)) fail(`loc row ${i + 1}: "${cell}" is not a color.`);
          out[name] = isNA(cell) ? null : SK.color.toHex(SK.color.parse(cell));
        }
      });
      return out;
    });
  }

  function quote(cell) {
    if (cell === null || cell === undefined) return 'NA';
    const s = String(cell);
    return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
  }

  function format(rows) {
    return `${rows.map((r) => r.map(quote).join(',')).join('\n')}\n`;
  }

  SK.csv = { parse, parseMatrix, parseLoc, format };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
