# Polylines Sankey webapp

An interactive, static (no build step) version of the Sankey ribbons in `../R/functions.R`. It reads the same inputs as `sankey_ribbons()` and `sankey_ribbons_graph()`, saved as CSV files, and exports CSVs plus an R script that redraws the result with the R functions.

## Running

Open `index.html` directly in a browser, or serve the folder:

```sh
cd webapp
npm run serve        # python3 -m http.server 4173, then open http://127.0.0.1:4173
```

## Input files

| Role | Format |
| --- | --- |
| Flow matrices | One numeric CSV per stage (rows = source nodes, columns = target nodes); adjacent stages must share dimensions. In graph mode, one square matrix whose rows/columns are the `loc` rows. A header row and row-name column (as written by R's `write.csv()`) are detected and used as node names. |
| Node locations (`loc`) | Columns `set, node, x, y` with optional `width`, `label`, `cex`, `font`, `height`, `color`, `merge_col`. Without a header the file is read like R's numeric `loc` matrix: `set, node, x, y [, width [, height]]`. Blank `width` cells fall back to label-derived or default widths. |
| Strand / mid colors | Color matrices shaped like the flow matrices: hex (`#RRGGBB`, `#RRGGBBAA`), CSS names, R `grayNN`, or `NA`. |
| Project | A `.json` file exported by the app. |

Drop files anywhere or use **Open files…**. Roles are inferred from content and file names (names containing `mid` are mid colors), stage order follows natural file-name order, and the per-role **Load…** buttons force a role. If flow matrices arrive without a matching `loc`, node locations are generated. Invalid input stops rendering and shows the reason in a red banner.

## Interaction

- Hover a strand or node for values and shares of source outflow / target inflow; other strands dim.
- Click to select; the inspector edits strand values and colors (including three-way mid colors), node labels, positions, widths, heights, fonts, and node or trunk colors.
- Drag a node to move it (Shift: vertical only, Alt: horizontal only).
- Drag a strand up or down to reorder it within the stack at its nearer end.
- Undo/redo (Ctrl/⌘+Z, Ctrl/⌘+Shift+Z), fit view (F), deselect (Esc). Sessions autosave to the browser.

All `sankey_ribbons()` layout options are available in the sidebar, including per-stage routing and ordering ("Apply to: stage k"), `adjust_loc`, `adjust_node_height`, boundary routing and compensation, plus graph-mode `require_forward` and `balance`.

## Exports

SVG, PNG (2×), `loc.csv`, a project JSON, and an **R bundle** zip containing the flow matrices, resolved color matrices, `loc.csv` and a `sankey.R` script that calls `sankey_ribbons()`/`sankey_ribbons_graph()` and `plot_sankey_ribbons()` with the current settings. Strand orders set by dragging are written to `stack_order.csv` and passed to R as `stack_order`.

## Code layout

- `js/layout.js`: geometry core ported from the R code (node tables, stacking and ordering, `adjust_loc`, node heights, Bezier and boundary routing). Computed in isotropic plot pixels, so R's `xyr` is always 1.
- `js/shapes.js`: merge trunks, node bodies, interior bridges and border paths.
- `js/render.js`: color resolution and SVG markup.
- `js/app.js`: state, controls, file loading, interactions, inspector, history and exports.
- `js/csv.js`, `js/color.js`, `js/zip.js`, `js/examples.js`: parsing, color math, a store-only zip writer and the bundled examples.

## Tests

```sh
cd webapp
npm install                 # Playwright test runner
npm run test:unit           # geometry parity with R, layout and CSV parsing checks (node:test)
npm run test:e2e            # browser tests (Playwright, Chromium)
```

Regenerate the R reference geometry from the repository root with `Rscript webapp/tests/r_parity_fixtures.R`.

The parity fixtures are produced by the R functions themselves, so `layout.js` is checked against R's strand offsets, centerlines, edges, adjusted node positions and merge lengths.
