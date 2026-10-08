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

## Importing data tables

**Import table…** (also opened automatically for any other table you drop) builds flows from ordinary data:

| Layout | Expected columns | Result |
| --- | --- | --- |
| wide | one row per item or item group, one column per stage, optional weight and item id | transitions counted between consecutive chosen columns |
| long | item id, stage, category, optional weight | pivoted to one row per item, then as wide |
| links | source, target, optional value and stage | one matrix per stage; without a stage column, layers are inferred and links that skip layers produce graph mode |
| crosstab | row labels, then one numeric column per target | a single stage |

The layout and column roles are guessed from names and types, and everything can be changed in the dialog. Other options:
- Missing values: drop the row, or keep it as a "(missing)" node.
- Node order: first appearance, largest first, or alphabetical.
- Group small nodes: merge nodes below a minimum share of their stage, or outside the top N, into "Other".
- Labels: inside node bodies, beside nodes, or none.

Errors (cycles, duplicated item/stage pairs, non-numeric weights) are shown before anything is built.

Imported values stay as counts. A **value scale** (Layout section) turns them into strand thickness and is chosen so node columns sit one x unit apart. Hover details show raw values, and the R bundle multiplies the exported matrices by the same `value_scale`.

Wide and long imports also keep every trajectory (the sequence of nodes an item visits), merged into weighted pathways. With an item id (always present in long tables, optional in wide ones, guessed from names like `patient_id` that are unique per row), each pathway also lists its items, so single items can be traced.

## Interaction

- Hover a strand or node for values and shares of source outflow / target inflow; other strands dim. Hover details appear in the inspector panel (or a corner card in narrow windows), not at the pointer, so they never cover the plot.
- Click to select; the inspector edits strand values and colors (including three-way mid colors), node labels, positions, widths, heights, fonts, and node or trunk colors.
- Drag a node to move it (Shift: vertical only, Alt: horizontal only).
- Drag a strand up or down to reorder it within the stack at its nearer end.
- Dragging a node into a neighbour stops at an invisible wall, or (Canvas & dragging → "pushes them") shoves the neighbour along; a node can never be dropped into an invalid layout.
- **Pathway tracing** (wide and long table imports): hovering a strand or node traces every pathway through it end to end, e.g. which stages and treatments feed Remission, and the inspector breaks the traced items down by node and by strand (counts and shares of the traced total) and lists the largest pathways. Hold Ctrl/⌘ to trace only the substrand under the pointer: one pathway, or one item (e.g. patient P091) when the data has item ids. Each strand is split into substrands, one per pathway and then one per item, ordered by where they arrive at the strand's source node. Traces are colored like the strands and nodes they cross, curve through interior nodes from band edge to band edge, run into end-node bodies, and fade with the node bodies (through white at labeled node centres); the traced nodes dim like the rest, keeping their labels. With **Gather traces in nodes** (on by default), several traced pathways close up into one contiguous bundle through each node they share, in the order they cross it, and fan back out to their own strands at the node edges. A single traced item thinner than 4 px is drawn 4 px wide around its slice. **Substrand lines** (Canvas & dragging) draw the substrand divisions permanently. Tracing switches off (with a warning) if flow values are edited away from the imported data.
- Undo/redo (Ctrl/⌘+Z, Ctrl/⌘+Shift+Z), fit view (F), deselect (Esc). Sessions autosave to the browser.

All `sankey_ribbons()` layout options are available in the sidebar, including per-stage routing and ordering ("Apply to: stage k"), `adjust_loc`, `adjust_node_height`, boundary routing and compensation, plus graph-mode `require_forward` and `balance`.

## Exports

SVG, PNG (2×), `loc.csv`, a project JSON, and an **R bundle** zip containing the flow matrices, resolved color matrices, `loc.csv` and a `sankey.R` script that calls `sankey_ribbons()`/`sankey_ribbons_graph()` and `plot_sankey_ribbons()` with the current settings. Strand orders set by dragging are written to `stack_order.csv` and passed to R as `stack_order`.

## Code layout

- `js/layout.js`: geometry core ported from the R code (node tables, stacking and ordering, `adjust_loc`, node heights, Bezier and boundary routing). Computed in isotropic plot pixels, so R's `xyr` is always 1.
- `js/shapes.js`: merge trunks, node bodies, interior bridges and border paths.
- `js/render.js`: color resolution and SVG markup.
- `js/app.js`: state, controls, file loading, interactions, inspector, history and exports.
- `js/transform.js` and `js/importer.js`: table import converters (wide, long, links, crosstab) and the import dialog.
- `js/paths.js`: substrand and item-slice stacking, pathway trace geometry, hit-testing and substrand lines for item-level data.
- `js/csv.js`, `js/color.js`, `js/zip.js`, `js/examples.js`: parsing, color math, a store-only zip writer and the bundled examples (matrix sets, aggregated patient pathways, a generated cohort of 200 patients with one row each, and long-format survey waves).

## Tests

```sh
cd webapp
npm install                 # Playwright test runner
npm run test:unit           # geometry parity with R, layout, CSV parsing, table import and pathways (node:test)
npm run test:e2e            # browser tests (Playwright, Chromium)
```

Regenerate the R reference geometry from the repository root with `Rscript webapp/tests/r_parity_fixtures.R`.

The parity fixtures are produced by the R functions themselves, so `layout.js` is checked against R's strand offsets, centerlines, edges, adjusted node positions and merge lengths.
