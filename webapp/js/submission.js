/* Public, JSON-only submissions shared by browser callers and the headless exporter. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const fail = message => { throw new SK.layout.SankeyError(message); };
  const fields = {
    table: ['text', 'spec', 'options', 'labels', 'name', 'style', 'params', 'stageParams', 'thickness'],
    matrices: ['matrices', 'mode', 'loc', 'dimnames', 'stageNames', 'params', 'stageParams', 'style', 'valueScale',
      'colorMatrices', 'midMatrices', 'nodeColors', 'mergeColors', 'paths', 'setNames', 'pathUnit', 'view'],
    project: ['project']
  };

  function create(handlers, settings) {
    return function submit(input) {
      if (!input || typeof input !== 'object' || Array.isArray(input) || !Object.hasOwn(fields, input.type)) {
        fail('A submission needs type "table", "matrices" or "project".');
      }
      Object.keys(input).forEach(key => {
        if (key !== 'type' && !fields[input.type].includes(key)) fail(`Unknown ${input.type} submission field "${key}".`);
      });
      // Callers retain ownership of their inputs; layout and later edits must not mutate them.
      let request;
      try {
        request = JSON.parse(JSON.stringify(input, (key, value) => {
          if (['undefined', 'function', 'symbol', 'bigint'].includes(typeof value) ||
              (typeof value === 'number' && !Number.isFinite(value))) fail('Submission values must be finite JSON data.');
          return value;
        }));
      } catch (err) {
        fail(`Invalid submission: ${err.message}`);
      }
      if (settings && request.type !== 'project') ['style', 'params'].forEach(group => {
        if (request[group] === undefined) return;
        if (!request[group] || typeof request[group] !== 'object' || Array.isArray(request[group])) fail(`${group} must be an object.`);
        Object.keys(request[group]).forEach(key => {
          if (!Object.hasOwn(settings[group], key)) fail(`Unknown ${group} setting "${key}".`);
        });
      });
      if (request.type === 'table') {
        if (typeof request.text !== 'string') fail('A table submission needs CSV or TSV text.');
        if (request.labels !== undefined && !['above', 'inside', 'beside', 'none'].includes(request.labels)) fail('labels must be above, inside, beside or none.');
        if (request.thickness !== undefined && !(Number.isFinite(request.thickness) && request.thickness > 0)) fail('thickness must be a positive number.');
        const table = SK.transform.parseTable(request.text);
        const spec = request.spec || SK.transform.guess(table);
        handlers.table(SK.transform.build(table, spec, request.options), request);
      } else {
        if (request.type === 'matrices') SK.layout.checkMatrices(request.matrices, request.mode);
        handlers[request.type](request.type === 'project' ? request.project : request);
      }
      return handlers.getProject();
    };
  }

  SK.submission = { create };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
