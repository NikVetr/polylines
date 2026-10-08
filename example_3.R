# Clean Sankey ribbon demonstration.
#
# Run this file from the repository root to regenerate
# images/sankey_example.png, images/sankey_adjust_loc_example.png,
# images/sankey_unequal_x_example.png, and
# images/sankey_boundary_route_example.png, and
# images/sankey_settings_grid.png.

source("R/functions.R")

thic_1 <- matrix(
  c(
    0.35, 0.25, 0.05, 0.10,
    0.10, 0.35, 0.20, 0.05,
    0.05, 0.10, 0.30, 0.20
  ),
  nrow = 3,
  byrow = TRUE
)
middle_totals <- colSums(thic_1)
thic_2 <- diag(middle_totals)
thic_3 <- matrix(
  c(
    0.25, 0.15, 0.10,
    0.10, 0.40, 0.20,
    0.05, 0.15, 0.35,
    0.12, 0.06, 0.17
  ),
  nrow = 4,
  byrow = TRUE
)

loc <- rbind(
  cbind(1, 1:3, 0.0, c(3.0, 2.0, 1.0)),
  cbind(2, 1:4, 1.5, c(3.3, 2.4, 1.5, 0.6)),
  cbind(3, 1:4, 2.5, c(3.3, 2.4, 1.5, 0.6)),
  cbind(4, 1:3, 4.0, c(3.0, 2.0, 1.0))
)

node_col <- list(
  c("#245C8A", "#5A7D3A", "#9B4D65"),
  c("#4078A8", "#7C8F47", "#B8758B", "#7A5B9A"),
  c("#4B9AB8", "#9BA64A", "#CB8FA2", "#9070A8"),
  c("#1D7A72", "#8B6F2A", "#8F3D6A")
)
merge_col <- lapply(node_col, grDevices::adjustcolor, alpha.f = 0.92)

edge_col <- list(
  matrix(
    c(
      "#4E88B7", "#699E6D", "#C9869F", "#8D74AA",
      "#5F98C5", "#8DA65C", "#B97991", "#846AA3",
      "#6BA8B8", "#A59D4E", "#CD8BA5", "#A47BB3"
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(
    c(
      "#5FA3C4", NA, NA, NA,
      NA, "#A3AA55", NA, NA,
      NA, NA, "#CE91AA", NA,
      NA, NA, NA, "#9A78B2"
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      "#4C9FA6", "#7EA176", "#B480A1",
      "#6AAE9A", "#B09D4C", "#C6859C",
      "#7EB2B8", "#B7A45C", "#C9548F",
      "#7292B4", "#A78E55", "#A66DB2"
    ),
    nrow = 4,
    byrow = TRUE
  )
)
mid_col <- list(
  matrix(
    c(
      "#F2B84B", NA, "#D75A7C", "#8B5FBF",
      NA, "#C8A64D", "#E28AA3", NA,
      "#56B6A9", NA, "#DB6B92", "#A16FC0"
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(NA_character_, nrow = 4, ncol = 4),
  matrix(
    c(
      "#50B5A8", NA, "#C86A9A",
      NA, "#C9AA4B", "#D27694",
      "#73B9C4", NA, "#DE5D98",
      "#6A91BF", "#B59B58", NA
    ),
    nrow = 4,
    byrow = TRUE
  )
)

grDevices::png("images/sankey_example.png", width = 1400, height = 900, res = 160, bg = "white")
par(mar = c(0.3, 0.3, 0.3, 0.3), xpd = NA)
plot.new()
plot.window(xlim = c(-0.25, 4.25), ylim = c(0.25, 3.65), xaxs = "i", yaxs = "i")

sankey <- sankey_ribbons(list(thic_1, thic_2, thic_3), loc, n = 160, merge_frac = 0.075)
plot_sankey_ribbons(
  sankey,
  col = edge_col,
  node_col = node_col,
  merge_col = merge_col,
  mid_col = mid_col,
  border = NA,
  label_nodes = FALSE,
  gradient_slices = 260,
  gradient_mode = "path",
  raster_fill = TRUE,
  raster_res = 180,
  add = TRUE
)

node_cols_flat <- unlist(node_col, use.names = FALSE)
points(loc[, 3], loc[, 4], pch = 21, bg = "white", col = node_cols_flat, lwd = 2, cex = 1.15)

grDevices::dev.off()

# A second figure demonstrates automatic y-location adjustment in a multi-stage
# Sankey. Several neighboring nodes begin too close together, while others
# already have enough room; the adjusted panel preserves node order and stays
# inside the original y ranges while opening the requested edge-to-edge gap.
adjust_thic_1 <- matrix(
  c(
    0.08, 0.02, 0.00, 0.01, 0.00,
    0.03, 0.06, 0.02, 0.00, 0.00,
    0.00, 0.04, 0.05, 0.02, 0.00,
    0.00, 0.00, 0.03, 0.06, 0.02,
    0.02, 0.00, 0.00, 0.03, 0.06
  ),
  nrow = 5,
  byrow = TRUE
)
adjust_thic_2 <- matrix(
  c(
    0.06, 0.03, 0.01, 0.02,
    0.02, 0.06, 0.03, 0.01,
    0.01, 0.03, 0.05, 0.02,
    0.00, 0.02, 0.06, 0.03,
    0.03, 0.00, 0.02, 0.04
  ),
  nrow = 5,
  byrow = TRUE
)
adjust_thic_2 <- adjust_thic_2 * (colSums(adjust_thic_1) / rowSums(adjust_thic_2))
adjust_thic <- list(adjust_thic_1, adjust_thic_2)
adjust_loc <- rbind(
  cbind(1, 1:5, 0.0, c(0.12, 0.34, 0.365, 0.62, 0.88)),
  cbind(2, 1:5, 1.35, c(0.14, 0.33, 0.355, 0.58, 0.86)),
  cbind(3, 1:4, 2.70, c(0.16, 0.43, 0.455, 0.84))
)
adjust_col <- list(
  matrix(
    c(
      "#2B7FB8CC", "#65A6CACC", "#9CC9DDCC", "#B7A45CCC", "#D19AD6CC",
      "#D45A2ACC", "#F08C4ACC", "#F3B37ACC", "#C9869FCC", "#8D74AACC",
      "#4E9F6ECC", "#89BF7ACC", "#C5D86DCC", "#6BA8B8CC", "#A47BB3CC",
      "#7A5DB0CC", "#A678C9CC", "#D19AD6CC", "#7292B4CC", "#B59B58CC",
      "#4C9FA6CC", "#7EA176CC", "#B480A1CC", "#6AAE9ACC", "#C9548FCC"
    ),
    nrow = 5,
    byrow = TRUE
  ),
  matrix(
    c(
      "#4C9FA6CC", "#7EA176CC", "#B480A1CC", "#C5D86DCC",
      "#6AAE9ACC", "#B09D4CCC", "#C6859CCC", "#F3B37ACC",
      "#7EB2B8CC", "#B7A45CCC", "#C9548FCC", "#D19AD6CC",
      "#7292B4CC", "#A78E55CC", "#A66DB2CC", "#89BF7ACC",
      "#2B7FB8CC", "#65A6CACC", "#9CC9DDCC", "#F08C4ACC"
    ),
    nrow = 5,
    byrow = TRUE
  )
)

draw_adjust_panel <- function(adjust, title) {
  plot.new()
  plot.window(xlim = c(-0.18, 2.88), ylim = c(0.02, 0.98), xaxs = "i", yaxs = "i")
  sankey <- sankey_ribbons(
    adjust_thic,
    adjust_loc,
    n = 150,
    merge_frac = 0.06,
    adjust_loc = adjust,
    min_node_gap = 0.015,
    adjust_loc_expand = FALSE
  )
  plot_sankey_ribbons(
    sankey,
    col = adjust_col,
    border = NA,
    label_nodes = FALSE,
    gradient_slices = 100,
    gradient_mode = "path",
    raster_fill = TRUE,
    raster_res = 140,
    add = TRUE
  )
  original_cols <- c("1" = "#245C8A", "2" = "#8F3D6A", "3" = "#8B6F2A")
  segments(
    adjust_loc[, 3] - 0.04, adjust_loc[, 4],
    adjust_loc[, 3] + 0.04, adjust_loc[, 4],
    col = grDevices::adjustcolor(original_cols[as.character(adjust_loc[, 1])], alpha.f = 0.45),
    lwd = 1.1
  )
  node_cols <- original_cols[as.character(sankey$loc[, 1])]
  points(sankey$loc[, 3], sankey$loc[, 4], pch = 21, bg = "white", col = node_cols, lwd = 1.4, cex = 0.95)
  text(sankey$loc[, 3], sankey$loc[, 4], labels = sankey$loc[, 2], col = node_cols, cex = 0.55, font = 2)
  text(1.35, 0.94, title, font = 2, cex = 0.85)
}

grDevices::png("images/sankey_adjust_loc_example.png", width = 1400, height = 800, res = 160, bg = "white")
par(mfrow = c(1, 2), mar = c(0.3, 0.25, 0.3, 0.25), xpd = NA)
draw_adjust_panel(FALSE, "Original loc")
draw_adjust_panel(TRUE, "Adjusted loc")
grDevices::dev.off()

# A third figure shows that nodes within the same stage do not need to share
# the same x coordinate.
unequal_thic_1 <- matrix(
  c(
    0.18, 0.07, 0.04,
    0.08, 0.16, 0.07,
    0.04, 0.10, 0.14
  ),
  nrow = 3,
  byrow = TRUE
)
unequal_mid_totals <- colSums(unequal_thic_1)
unequal_thic_2 <- matrix(
  c(
    0.18, 0.07, 0.04,
    0.07, 0.16, 0.08,
    0.05, 0.08, 0.15
  ),
  nrow = 3,
  byrow = TRUE
)
unequal_thic_2 <- unequal_thic_2 * (unequal_mid_totals / rowSums(unequal_thic_2))
unequal_loc <- rbind(
  cbind(1, 1:3, c(0.00, 0.22, 0.08), c(0.76, 0.50, 0.24)),
  cbind(2, 1:3, c(1.24, 1.55, 1.36), c(0.78, 0.50, 0.22)),
  cbind(3, 1:3, c(2.88, 2.58, 2.96), c(0.74, 0.50, 0.26))
)
unequal_col <- list(
  matrix(
    c(
      "#2B7FB8CC", "#65A6CACC", "#9CC9DDCC",
      "#D45A2ACC", "#F08C4ACC", "#F3B37ACC",
      "#4E9F6ECC", "#89BF7ACC", "#C5D86DCC"
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(
    c(
      "#4C9FA6CC", "#7EA176CC", "#B480A1CC",
      "#6AAE9ACC", "#B09D4CCC", "#C6859CCC",
      "#7EB2B8CC", "#B7A45CCC", "#C9548FCC"
    ),
    nrow = 3,
    byrow = TRUE
  )
)

grDevices::png("images/sankey_unequal_x_example.png", width = 1400, height = 800, res = 160, bg = "white")
par(mar = c(0.25, 0.25, 0.25, 0.25), xpd = NA)
plot.new()
plot.window(xlim = c(-0.20, 3.18), ylim = c(0.0, 1.0), xaxs = "i", yaxs = "i")
unequal_sankey <- sankey_ribbons(
  list(unequal_thic_1, unequal_thic_2),
  unequal_loc,
  n = 160,
  merge_frac = 0.065,
  adjust_loc = TRUE,
  min_node_gap = 0.012,
  adjust_loc_expand = TRUE
)
plot_sankey_ribbons(
  unequal_sankey,
  col = unequal_col,
  border = NA,
  label_nodes = FALSE,
  gradient_slices = 120,
  gradient_mode = "path",
  raster_fill = TRUE,
  raster_res = 150,
  add = TRUE
)
unequal_node_cols <- c("1" = "#245C8A", "2" = "#8F3D6A", "3" = "#8B6F2A")
points(
  unequal_sankey$loc[, 3],
  unequal_sankey$loc[, 4],
  pch = 21,
  bg = "white",
  col = unequal_node_cols[as.character(unequal_sankey$loc[, 1])],
  lwd = 1.5,
  cex = 1.0
)
grDevices::dev.off()

# Boundary transport is an alternative route for dense fan-in/fan-out layouts.
# It interpolates packed ribbon boundaries directly instead of expanding each
# Bezier centerline independently.
boundary_thic <- matrix(
  c(0.010, 0.060, 0.012, 0.085, 0.014, 0.070, 0.020, 0.105,
    0.018, 0.075, 0.013, 0.055, 0.011, 0.045, 0.010),
  ncol = 1
)
boundary_col <- grDevices::adjustcolor("#5FA3C4", alpha.f = 0.72)

draw_boundary_panel <- function(route, compensation, title) {
  plot.new()
  plot.window(xlim = c(-0.12, 2.95), ylim = c(-0.74, 0.98), xaxs = "i", yaxs = "i")
  xyr <- xyrat()
  widths <- as.numeric(boundary_thic)
  center_spacing <- ((head(widths, -1) + tail(widths, -1)) / 2 + 0.0015) / xyr
  source_y <- 0.86 - c(0, cumsum(center_spacing))
  boundary_loc <- rbind(
    cbind(1, seq_along(widths), 0.0, source_y),
    cbind(2, 1, 2.70, -0.50)
  )
  sankey <- sankey_ribbons(
    boundary_thic,
    boundary_loc,
    n = 220,
    merge_frac = 0.045,
    route = route,
    boundary_group = "target",
    boundary_compensation = compensation,
    xyr = xyr
  )
  plot_sankey_ribbons(
    sankey,
    col = boundary_col,
    border = NA,
    label_nodes = FALSE,
    gradient_slices = 80,
    gradient_mode = "path",
    raster_fill = TRUE,
    raster_res = 160,
    add = TRUE
  )
  points(
    sankey$loc[, 3],
    sankey$loc[, 4],
    pch = 21,
    bg = "white",
    col = c("1" = "#245C8A", "2" = "#8B6F2A")[as.character(sankey$loc[, 1])],
    lwd = 1.3,
    cex = 0.82
  )
  text(mean(par("usr")[1:2]), 0.91, title, font = 2, cex = 0.82)
}

grDevices::png("images/sankey_boundary_route_example.png", width = 1400, height = 800, res = 160, bg = "white")
par(mfrow = c(1, 2), mar = c(0.25, 0.25, 0.25, 0.25), xpd = NA)
draw_boundary_panel("bezier", "none", "Bezier route")
draw_boundary_panel("boundary", "full", "Boundary route")
grDevices::dev.off()

# A settings grid shows how frequently useful non-default arguments change one
# labeled multi-stage Sankey. The example uses 4 input nodes, 2 mediator nodes,
# and 3 output nodes, with unequal x positions inside each layer.
settings_thic_1 <- matrix(
  c(
    0.065, 0.025,
    0.030, 0.085,
    0.075, 0.025,
    0.030, 0.085
  ),
  nrow = 4,
  byrow = TRUE
)
settings_thic_2 <- matrix(
  c(
    0.085, 0.070, 0.045,
    0.035, 0.095, 0.090
  ),
  nrow = 2,
  byrow = TRUE
)
settings_thic_1 <- settings_thic_1 * 1.28
settings_thic_2 <- settings_thic_2 * 1.28
settings_thic <- list(settings_thic_1, settings_thic_2)
settings_loc <- data.frame(
  set = c(rep(1, 4), rep(2, 2), rep(3, 3)),
  node = c(1:4, 1:2, 1:3),
  x = c(-0.06, 0.10, -0.02, 0.16, 1.20, 1.50, 2.90, 2.62, 3.02),
  y = c(0.88, 0.64, 0.38, 0.14, 0.69, 0.31, 0.82, 0.50, 0.18),
  label = c(
    "Clinical A", "Long input B", "Molecular C", "Trait D",
    "Mediator", "Second mediator",
    "Outcome 1", "Longer outcome", "Outcome 3"
  ),
  cex = c(rep(0.70, 4), rep(0.74, 2), rep(0.70, 3)),
  font = rep(2, 9)
)
settings_crowded_loc <- settings_loc
settings_crowded_loc$y <- c(0.78, 0.54, 0.48, 0.22, 0.54, 0.48, 0.76, 0.52, 0.46)
settings_unlabeled_loc <- settings_loc[c("set", "node", "x", "y")]

settings_col <- list(
  matrix(
    c(
      "#2B7FB8BB", "#B7A45CBB",
      "#D45A2ABB", "#C9869FBB",
      "#4E9F6EBB", "#89BF7ABB",
      "#7A5DB0BB", "#D19AD6BB"
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      "#4B9AB8BB", "#9BA64ABB", "#CB8FA2BB",
      "#6AAE9ABB", "#B09D4CBB", "#A66DB2BB"
    ),
    nrow = 2,
    byrow = TRUE
  )
)
settings_node_col <- list(
  c("#245C8A", "#B34F2C", "#4B8B63", "#7A5DB0"),
  c("#4B9AB8", "#9070A8"),
  c("#1D7A72", "#8B6F2A", "#8F3D6A")
)
settings_mid_col <- list(
  matrix(
    c(
      "#F2B84B", NA,
      NA, "#E28AA3",
      "#56B6A9", NA,
      "#8B5FBF", "#DB6B92"
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      "#50B5A8", NA, "#C86A9A",
      NA, "#C9AA4B", "#A16FC0"
    ),
    nrow = 2,
    byrow = TRUE
  )
)

draw_settings_labels <- function(sankey) {
  nodes <- sankey$nodes
  text(
    nodes$x,
    nodes$y,
    labels = nodes$label,
    cex = nodes$cex,
    font = nodes$font,
    col = "#1F2937",
    xpd = NA
  )
}

draw_settings_panel <- function(title, loc = settings_loc, build = list(), plot = list(),
                                show_labels = TRUE) {
  plot.new()
  plot.window(xlim = c(-0.40, 3.30), ylim = c(0.020, 1.000), xaxs = "i", yaxs = "i")
  sankey_args <- modifyList(
    list(
      thic = settings_thic,
      loc = loc,
      n = 120,
      node_label_pad = 0.012,
      node_label_pad_frac = 0.030
    ),
    build
  )
  sankey <- do.call(sankey_ribbons, sankey_args)
  plot_args <- modifyList(
    list(label_nodes = FALSE, node_border_lwd = 0.75, node_border_steps = 32),
    plot
  )
  do.call(plot_sankey_ribbons, c(list(sankey = sankey), plot_args))
  if(isTRUE(show_labels) && "has_label" %in% names(sankey$nodes) && any(sankey$nodes$has_label)) {
    draw_settings_labels(sankey)
  }
  title(main = title, line = 0.20, cex.main = 0.82, font.main = 2)
}

settings_panels <- list(
  list(title = 'settings = "defaults"'),
  list(
    title = 'col = "list"',
    plot = list(col = settings_col)
  ),
  list(
    title = 'gradient_slices = 80',
    plot = list(
      col = settings_col,
      node_col = settings_node_col,
      mid_col = settings_mid_col,
      gradient_slices = 80,
      gradient_mode = "path"
    )
  ),
  list(
    title = 'node_label_pad = 0.05',
    build = list(node_label_pad = 0.050)
  ),
  list(
    title = 'adjust_node_height = "node"',
    build = list(adjust_node_height = "node", node_label_vpad_frac = 0.18)
  ),
  list(
    title = "node_border = NA",
    plot = list(node_border = NA)
  ),
  list(
    title = 'loc = "unlabeled"',
    loc = settings_unlabeled_loc,
    show_labels = FALSE
  ),
  list(
    title = "curvature = 0.15",
    build = list(curvature = 0.15)
  ),
  list(
    title = 'raster_fill = TRUE',
    plot = list(
      col = settings_col,
      gradient_slices = 80,
      raster_fill = TRUE,
      raster_res = 150
    )
  ),
  list(
    title = 'node_border_shape = "line"',
    plot = list(node_border_shape = "line", node_border_fade = "alpha")
  ),
  list(
    title = 'adjust_loc = TRUE',
    loc = settings_crowded_loc,
    build = list(adjust_loc = TRUE, min_node_gap = 0.028, adjust_loc_expand = TRUE)
  ),
  list(
    title = 'border = "gray20"',
    plot = list(border = "gray20", border_lwd = 1.1, border_caps = FALSE, node_border = "gray20")
  )
)

grDevices::png("images/sankey_settings_grid.png", width = 2200, height = 1580, res = 170, bg = "white")
par(mfrow = c(3, 4), mar = c(0.08, 0.14, 1.20, 0.14), xpd = NA)
for(panel in settings_panels) {
  draw_settings_panel(
    title = panel$title,
    loc = if(is.null(panel$loc)) settings_loc else panel$loc,
    build = if(is.null(panel$build)) list() else panel$build,
    plot = if(is.null(panel$plot)) list() else panel$plot,
    show_labels = if(is.null(panel$show_labels)) TRUE else panel$show_labels
  )
}
grDevices::dev.off()
