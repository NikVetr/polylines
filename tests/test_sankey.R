source("R/functions.R")

expect_error <- function(expr, pattern = NULL) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  if(is.null(err)) {
    stop("Expected an error, but expression completed.", call. = FALSE)
  }
  if(!is.null(pattern) && !grepl(pattern, conditionMessage(err))) {
    stop(
      sprintf("Expected error matching %s, got: %s", sQuote(pattern), conditionMessage(err)),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ribbon_bounds <- function(sankey, flow_ind) {
  coords <- sankey$ribbons[[flow_ind]]
  n <- (nrow(coords) - 1) / 2
  if(n != floor(n)) {
    stop("Unexpected ribbon polygon size.", call. = FALSE)
  }
  data.frame(
    x = coords[seq_len(n), "x"],
    upper = coords[seq_len(n), "y"] * sankey$xyr,
    lower = rev(coords[n + seq_len(n), "y"] * sankey$xyr)
  )
}

min_boundary_gap <- function(sankey, inds = seq_len(nrow(sankey$flows))) {
  if(length(inds) <= 1) {
    return(Inf)
  }
  start_visual <- sankey$flows$start_y * sankey$xyr + sankey$flows$start_offset
  ord <- inds[order(start_visual[inds], sankey$flows$from[inds], sankey$flows$to[inds])]
  bounds <- lapply(ord, ribbon_bounds, sankey = sankey)
  min(vapply(seq_len(length(bounds) - 1), function(i) {
    min(bounds[[i + 1]]$lower - bounds[[i]]$upper)
  }, numeric(1)))
}

min_target_boundary_gap <- function(sankey) {
  min(vapply(sort(unique(sankey$flows$to)), function(to) {
    min_boundary_gap(sankey, which(sankey$flows$to == to))
  }, numeric(1)))
}

# Legacy Bezier behavior remains the default.
thic <- matrix(
  c(
    0.10, 0.04,
    0.03, 0.08,
    0.06, 0.02
  ),
  nrow = 3,
  byrow = TRUE
)
loc <- rbind(
  cbind(1, 1:3, 0, c(0.78, 0.50, 0.22)),
  cbind(2, 1:2, 2, c(0.68, 0.30))
)
bezier <- sankey_ribbons(thic, loc, n = 80, xyr = 1)
stopifnot(
  identical(bezier$route, "bezier"),
  length(bezier$ribbons) == sum(thic > 0),
  length(bezier$centerlines) == sum(thic > 0),
  all(vapply(bezier$ribbons, nrow, integer(1)) == 2 * 80 + 1)
)

# Boundary fan-in keeps packed ribbons non-overlapping in the selected group.
dense_thic <- matrix(
  c(0.010, 0.060, 0.012, 0.085, 0.014, 0.070, 0.020, 0.105,
    0.018, 0.075, 0.013, 0.055, 0.011, 0.045, 0.010),
  ncol = 1
)
widths <- as.numeric(dense_thic)
source_y <- 0.86 - c(0, cumsum((head(widths, -1) + tail(widths, -1)) / 2 + 0.0015))
dense_loc <- rbind(
  cbind(1, seq_along(widths), 0, source_y),
  cbind(2, 1, 2.7, -0.50)
)
boundary <- sankey_ribbons(
  dense_thic,
  dense_loc,
  n = 160,
  xyr = 1,
  route = "boundary",
  boundary_group = "target",
  boundary_compensation = "full"
)
stopifnot(
  identical(boundary$route, "boundary"),
  identical(boundary$boundary_group, "target"),
  identical(boundary$boundary_compensation, "full"),
  min_target_boundary_gap(boundary) >= -1e-10
)

dense_loc_width <- cbind(dense_loc, width = c(rep(0.08, length(widths)), 0.24))
boundary_width <- sankey_ribbons(
  dense_thic,
  dense_loc_width,
  n = 160,
  xyr = 1,
  route = "boundary",
  boundary_group = "target",
  boundary_compensation = "full"
)
stopifnot(
  identical(boundary_width$route, "boundary"),
  min_target_boundary_gap(boundary_width) >= -1e-10,
  all(boundary_width$flows$start_x > dense_loc_width[seq_along(widths), 3]),
  all(boundary_width$flows$end_x < dense_loc_width[nrow(dense_loc_width), 3])
)

# Multi-stage route arguments can be supplied stage-by-stage.
mid_totals <- colSums(thic)
thic2 <- diag(mid_totals)
multi_loc <- rbind(
  loc,
  cbind(3, 1:2, 3, c(0.68, 0.30))
)
multi <- sankey_ribbons(
  list(thic, thic2),
  multi_loc,
  n = 60,
  xyr = 1,
  route = list("boundary", "bezier"),
  boundary_group = list("target", "target"),
  boundary_compensation = list("none", "none")
)
stopifnot(
  length(multi$segments) == 2,
  identical(multi$segments[[1]]$route, "boundary"),
  identical(multi$segments[[2]]$route, "bezier")
)

# Boundary route fails loudly when endpoint stacks already overlap.
bad_loc <- rbind(
  cbind(1, 1:2, 0, c(0.00, 0.04)),
  cbind(2, 1, 2, 0.00)
)
bad_thic <- matrix(c(0.10, 0.10), ncol = 1)
expect_error(
  sankey_ribbons(bad_thic, bad_loc, route = "boundary", xyr = 1),
  "non-overlapping endpoint stacks"
)

# Location adjustment still works with boundary routing.
crowded_thic <- matrix(
  c(
    0.08, 0.00,
    0.07, 0.02,
    0.00, 0.08
  ),
  nrow = 3,
  byrow = TRUE
)
crowded_loc <- rbind(
  cbind(1, 1:3, 0, c(0.20, 0.23, 0.26)),
  cbind(2, 1:2, 2, c(0.38, 0.12))
)
adjusted <- sankey_ribbons(
  crowded_thic,
  crowded_loc,
  route = "boundary",
  boundary_group = "target",
  adjust_loc = TRUE,
  min_node_gap = 0.01,
  adjust_loc_expand = TRUE,
  xyr = 1
)
stopifnot(
  identical(adjusted$route, "boundary"),
  all(diff(adjusted$from_nodes$y) > 0),
  min_target_boundary_gap(adjusted) >= -1e-10
)

feasible_loc <- rbind(
  cbind(1, 1:3, 0, c(0.15, 0.50, 0.85)),
  cbind(2, 1:2, 2, c(0.32, 0.72))
)
feasible_adjusted <- sankey_ribbons(
  crowded_thic,
  feasible_loc,
  adjust_loc = TRUE,
  min_node_gap = 0.01,
  adjust_loc_expand = TRUE,
  xyr = 1
)
stopifnot(
  max(abs(feasible_adjusted$loc[, 4] - feasible_loc[, 4])) < 1e-12
)

# Graph adjacency mode allows a node to receive from non-adjacent visual stages.
graph_loc <- rbind(
  c(1, 1, 0.0, 0.70),
  c(1, 2, 0.0, 0.30),
  c(2, 1, 1.4, 0.55),
  c(3, 1, 2.8, 0.62),
  c(3, 2, 2.8, 0.28)
)
graph_thic <- matrix(0, nrow = nrow(graph_loc), ncol = nrow(graph_loc))
graph_thic[1, 3] <- 0.05
graph_thic[2, 3] <- 0.03
graph_thic[3, 4] <- 0.08
graph_thic[1, 4] <- 0.02
graph_thic[2, 5] <- 0.04
graph <- sankey_ribbons_graph(graph_thic, graph_loc, n = 80, xyr = 1)
stopifnot(
  inherits(graph, "sankey_graph"),
  length(graph$ribbons) == sum(graph_thic > 0),
  any(graph$flows$from == 1 & graph$flows$to == 4),
  graph$incoming[[4]] == graph_thic[1, 4] + graph_thic[3, 4],
  graph$outgoing[[3]] == graph_thic[3, 4]
)
graph_trunk <- interior_node_trunk_polygon(graph, graph, node = 3, merge_length = graph$merge_length)
stopifnot(
  !is.null(graph_trunk),
  is.matrix(graph_trunk),
  nrow(graph_trunk) > 4
)

graph_loc_width <- cbind(graph_loc, width = c(0.10, 0.10, 0.22, 0.12, 0.12))
graph_wide <- sankey_ribbons_graph(graph_thic, graph_loc_width, n = 80, xyr = 1)
flow_1_3 <- which(graph_wide$flows$from == 1 & graph_wide$flows$to == 3)
stopifnot(
  abs(graph_wide$flows$start_x[flow_1_3] - (graph_loc_width[1, 3] + graph_loc_width[1, 5] / 2)) < 1e-12,
  abs(graph_wide$flows$end_x[flow_1_3] - (graph_loc_width[3, 3] - graph_loc_width[3, 5] / 2)) < 1e-12
)
graph_wide_trunk <- interior_node_trunk_polygon(graph_wide, graph_wide, node = 3, merge_length = graph_wide$merge_length)
stopifnot(
  min(graph_wide_trunk[, "x"]) <= graph_loc_width[3, 3] - graph_loc_width[3, 5] / 2 + 1e-12,
  max(graph_wide_trunk[, "x"]) >= graph_loc_width[3, 3] + graph_loc_width[3, 5] / 2 - 1e-12
)
graph_source_body <- endpoint_node_body_polygon(
  graph_wide,
  graph_wide$from_nodes[graph_wide$from_nodes$id == 1, ],
  which(graph_wide$flows$from == 1),
  side = "start"
)
graph_sink_body <- endpoint_node_body_polygon(
  graph_wide,
  graph_wide$to_nodes[graph_wide$to_nodes$id == 4, ],
  which(graph_wide$flows$to == 4),
  side = "end"
)
stopifnot(
  min(graph_source_body[, "x"]) <= graph_loc_width[1, 3] - graph_loc_width[1, 5] / 2 + 1e-12,
  max(graph_sink_body[, "x"]) >= graph_loc_width[4, 3] + graph_loc_width[4, 5] / 2 - 1e-12
)

node_border_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(node_border_pdf, width = 4, height = 3)
plot.new()
plot.window(xlim = c(-0.1, 3.0), ylim = c(0.1, 0.8), xaxs = "i", yaxs = "i")
plot_sankey_ribbons(
  graph_wide,
  col = "#5B8CBA66",
  border = NA,
  label_nodes = FALSE,
  node_border = "#111111AA",
  node_border_lty = 2,
  node_border_steps = 8,
  add = TRUE
)
plot_sankey_ribbons(
  graph_wide,
  col = "#5B8CBA66",
  border = NA,
  label_nodes = FALSE,
  node_border = "#111111AA",
  node_border_lty = 2,
  node_border_steps = 8,
  node_border_fade = "alpha",
  node_border_shape = "line",
  add = TRUE
)
plot_sankey_ribbons(
  graph_wide,
  col = "#5B8CBA00",
  border = NA,
  label_nodes = FALSE,
  node_border = "transparent",
  add = TRUE
)
grDevices::dev.off()
unlink(node_border_pdf)

graph_rev <- sankey_ribbons_graph(graph_thic, graph_loc, n = 80, xyr = 1, target_order = "source_y_desc")
flow_1_4 <- which(graph$flows$from == 1 & graph$flows$to == 4)
flow_1_4_rev <- which(graph_rev$flows$from == 1 & graph_rev$flows$to == 4)
stopifnot(
  graph$flows$end_offset[flow_1_4] > 0,
  graph_rev$flows$end_offset[flow_1_4_rev] < 0
)

projected_loc <- cbind(
  c(1, 1, 2),
  c(1, 2, 1),
  c(0.0, 1.8, 3.0),
  c(0.80, 0.60, 0.20)
)
projected_thic <- matrix(0, 3, 3)
projected_thic[1, 3] <- 0.04
projected_thic[2, 3] <- 0.04
source_y_order <- sankey_ribbons_graph(projected_thic, projected_loc, n = 40, xyr = 1)
projected_order <- sankey_ribbons_graph(projected_thic, projected_loc, n = 40, xyr = 1, target_order = "source_projected")
flow_1_3 <- which(source_y_order$flows$from == 1 & source_y_order$flows$to == 3)
flow_1_3_proj <- which(projected_order$flows$from == 1 & projected_order$flows$to == 3)
stopifnot(
  source_y_order$flows$end_offset[flow_1_3] > 0,
  projected_order$flows$end_offset[flow_1_3_proj] < 0
)

bezier_order_loc <- data.frame(
  set = 1:4,
  node = 1:4,
  x = c(1.1659810, 1.4409270, 0.6519892, 3.0),
  y = c(0.4250294, -0.2000113, -0.3492957, 0.2570871),
  width = c(0.16215380, 0.56889698, 0.09735354, 0.0)
)
bezier_order_thic <- matrix(0, 4, 4)
bezier_order_thic[1, 4] <- 0.03472931
bezier_order_thic[2, 4] <- 0.02859826
bezier_order_thic[3, 4] <- 0.03437776
source_y_graph <- sankey_ribbons_graph(bezier_order_thic, bezier_order_loc, n = 80, xyr = 1)
source_bezier_graph <- sankey_ribbons_graph(
  bezier_order_thic,
  bezier_order_loc,
  n = 80,
  xyr = 1,
  target_order = "source_bezier"
)
stopifnot(
  identical(source_y_graph$flows$from[order(source_y_graph$flows$end_offset)], c(3L, 2L, 1L)),
  identical(source_bezier_graph$flows$from[order(source_bezier_graph$flows$end_offset)], c(2L, 3L, 1L))
)
expect_error(
  sankey_ribbons_graph(
    bezier_order_thic,
    bezier_order_loc,
    route = "boundary",
    target_order = "source_bezier",
    xyr = 1
  ),
  "Bezier-sampled endpoint ordering"
)

label_width_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(label_width_pdf, width = 5, height = 3)
plot.new()
plot.window(xlim = c(0, 3), ylim = c(0, 1), xaxs = "i", yaxs = "i")
label_loc <- data.frame(
  set = c(1, 2),
  node = c(1, 1),
  x = c(0.25, 2.75),
  y = c(0.5, 0.5),
  label = c("source label", "target"),
  cex = c(1, 1),
  font = c(2, 2)
)
label_sankey <- sankey_ribbons(matrix(0.08, nrow = 1), label_loc, xyr = 1, node_label_pad = 0.02)
raw_label_width <- graphics::strwidth(label_loc$label[1], cex = label_loc$cex[1], font = label_loc$font[1])
label_shrink <- sankey_ribbons(
  matrix(0.02, nrow = 1),
  label_loc,
  xyr = 1,
  node_label_pad = 0.02,
  adjust_node_height = "label",
  node_label_vpad_frac = 0.10
)
label_expand <- sankey_ribbons(
  matrix(0.02, nrow = 1),
  label_loc,
  xyr = 1,
  node_label_pad = 0.02,
  adjust_node_height = "node",
  node_label_vpad_frac = 0.10
)
mixed_label_loc <- data.frame(
  set = c(1, 1, 2),
  node = c(1, 2, 1),
  x = c(0.25, 0.25, 2.75),
  y = c(0.66, 0.34, 0.50),
  label = c("Short", "Longer source label", "target"),
  cex = c(1, 1, 1),
  font = c(2, 2, 2)
)
mixed_label_thic <- matrix(0, nrow = 3, ncol = 3)
mixed_label_thic[1, 3] <- 0.015
mixed_label_thic[2, 3] <- 0.12
mixed_label_sankey <- sankey_ribbons_graph(
  mixed_label_thic,
  mixed_label_loc,
  xyr = 1,
  node_label_pad = 0.02,
  adjust_node_height = "label",
  balance = "allow"
)
mixed_long <- which(mixed_label_sankey$nodes$label == "Longer source label")
mixed_long_width <- graphics::strwidth(
  mixed_label_sankey$nodes$label[mixed_long],
  cex = mixed_label_sankey$nodes$cex[mixed_long],
  font = mixed_label_sankey$nodes$font[mixed_long]
)
node_buffer_loc <- data.frame(
  set = c(1, 1, 2, 2, 3),
  node = c(1, 2, 1, 2, 1),
  x = c(0.25, 0.25, 1.5, 1.5, 2.75),
  y = c(0.72, 0.28, 0.68, 0.32, 0.50),
  label = c("source 1", "source 2", "Mediator", "Second mediator", "target"),
  cex = rep(1, 5),
  font = rep(2, 5)
)
node_buffer_thic <- matrix(0, nrow = 5, ncol = 5)
node_buffer_thic[1, 3] <- 0.20
node_buffer_thic[2, 4] <- 0.02
node_buffer_thic[3, 5] <- 0.20
node_buffer_thic[4, 5] <- 0.02
node_buffer_sankey <- sankey_ribbons_graph(
  node_buffer_thic,
  node_buffer_loc,
  xyr = 1,
  node_label_pad = 0.02,
  adjust_node_height = "node",
  node_label_vpad_frac = 0.10,
  balance = "allow"
)
mediator <- which(node_buffer_sankey$nodes$label == "Mediator")
second_mediator <- which(node_buffer_sankey$nodes$label == "Second mediator")
target_node <- which(node_buffer_sankey$nodes$label == "target")
target_body <- endpoint_node_body_polygon(
  node_buffer_sankey,
  node_buffer_sankey$to_nodes[node_buffer_sankey$to_nodes$id == target_node, ],
  which(node_buffer_sankey$flows$to == target_node),
  side = "end"
)
target_lower_border <- attr(target_body, "lower_border")
target_upper_border <- attr(target_body, "upper_border")
node_buffer_flow_heights <- pmax(rowSums(node_buffer_thic), colSums(node_buffer_thic))
node_buffer_label_heights <- sankey_text_metric(
  node_buffer_sankey$nodes$label,
  node_buffer_sankey$nodes$cex,
  node_buffer_sankey$nodes$font,
  metric = "height"
)
node_buffer_label_buffers <- (
  node_buffer_sankey$nodes$node_label_body_height - node_buffer_label_heights
) / 2
node_buffer_requested <- 0.10 * max(node_buffer_label_heights)
grDevices::dev.off()
unlink(label_width_pdf)
stopifnot(
  label_sankey$from_nodes$node_width > 0,
  label_sankey$from_nodes$node_width > raw_label_width + 0.04,
  label_sankey$to_nodes$node_width > 0,
  label_sankey$flows$start_x > label_loc$x[1],
  label_sankey$flows$end_x < label_loc$x[2],
  label_shrink$from_nodes$cex < label_loc$cex[1],
  is.na(label_shrink$from_nodes$node_body_height),
  label_expand$from_nodes$cex == label_loc$cex[1],
  label_expand$from_nodes$node_body_height > 0.02,
  all(is.na(mixed_label_sankey$nodes$node_body_height)),
  abs(mixed_label_sankey$nodes$node_label_width[mixed_long] - mixed_long_width) < 1e-8,
  mixed_label_sankey$nodes$node_width[mixed_long] > mixed_long_width + 0.04,
  all(node_buffer_sankey$nodes$node_body_height >= node_buffer_flow_heights - 1e-8),
  all(node_buffer_sankey$nodes$node_label_body_height >=
        node_buffer_label_heights + 2 * node_buffer_requested - 1e-8),
  max(node_buffer_label_buffers) - min(node_buffer_label_buffers) < 1e-8,
  node_buffer_sankey$nodes$node_body_height[mediator] >
    node_buffer_sankey$nodes$node_label_body_height[mediator],
  node_buffer_sankey$nodes$node_body_height[second_mediator] ==
    node_buffer_sankey$nodes$node_label_body_height[second_mediator],
  abs(min(target_lower_border[, "y"]) - min(target_body[, "y"])) < 1e-8,
  abs(max(target_upper_border[, "y"]) - max(target_body[, "y"])) < 1e-8
)

expect_error(
  sankey_ribbons_graph(graph_thic + diag(0.01, nrow(graph_thic)), graph_loc),
  "zero diagonal"
)

backward_thic <- graph_thic
backward_thic[4, 1] <- 0.01
expect_error(
  sankey_ribbons_graph(backward_thic, graph_loc),
  "left-to-right"
)

imbalanced_thic <- graph_thic
imbalanced_thic[3, 4] <- 0.04
expect_error(
  sankey_ribbons_graph(imbalanced_thic, graph_loc, balance = "error"),
  "unequal incoming and outgoing"
)

# Drawing helpers use the stored boundary edges, not normal offsets of the centerline.
boundary_none <- sankey_ribbons(
  dense_thic,
  dense_loc,
  n = 160,
  xyr = 1,
  route = "boundary",
  boundary_group = "target",
  boundary_compensation = "none"
)
for(k in seq_len(nrow(boundary_none$flows))) {
  drawn <- sankey_centerline_edges(boundary_none, k)
  stored <- ribbon_bounds(boundary_none, k)
  stopifnot(
    max(abs(drawn$left[, "y"] - stored$upper)) < 1e-12,
    max(abs(drawn$right[, "y"] - stored$lower)) < 1e-12
  )
}

# Gap-budget compensation runs on multi-strand groups and never overlaps them.
gap_budget <- sankey_ribbons(
  dense_thic,
  dense_loc,
  n = 160,
  xyr = 1,
  route = "boundary",
  boundary_group = "target",
  boundary_compensation = "gap_budget"
)
stopifnot(min_target_boundary_gap(gap_budget) >= -1e-10)

# Right-to-left flows stack exactly like their mirrored left-to-right layout.
mirror_loc <- loc
mirror_loc[, 3] <- 2 - loc[, 3]
mirrored <- sankey_ribbons(thic, mirror_loc, n = 80, xyr = 1)
endpoint_y <- function(sankey, i) vapply(sankey$centerlines, function(cl) cl[i, "y"], numeric(1))
stopifnot(
  max(abs(endpoint_y(mirrored, 1) - endpoint_y(bezier, 1))) < 1e-12,
  max(abs(endpoint_y(mirrored, 80) - endpoint_y(bezier, 80))) < 1e-12,
  all(diff(path_segment_by_x(mirrored$edges[[1]]$left, 0.5, 1.5)[, "x"]) < 0)
)

# Location adjustment meets the requested spacing in range when it fits, and
# otherwise leaves nodes that need no change alone.
crowded_y <- c(0, 0.01, 0.5, 1)
stopifnot(
  max(abs(sankey_adjust_one_node_set(crowded_y, rep(0.1, 4), xyr = 1, allow_expand = FALSE) -
            c(0, 0.1, 0.5, 1))) < 1e-12,
  max(abs(sankey_adjust_one_node_set(crowded_y, rep(0.1, 4), xyr = 1, allow_expand = TRUE) -
            c(-0.045, 0.055, 0.5, 1))) < 1e-12,
  max(abs(sankey_adjust_one_node_set(c(0, 0.02, 0.2), rep(0.2, 3), xyr = 1, allow_expand = FALSE) -
            c(0, 0.1, 0.2))) < 1e-12
)

# Raster fills draw on, and return to, the calling device when others are open.
raster_other <- tempfile(fileext = ".png")
raster_main <- tempfile(fileext = ".png")
grDevices::png(raster_other, width = 200, height = 200)
other_dev <- grDevices::dev.cur()
plot.new()
grDevices::png(raster_main, width = 200, height = 200)
main_dev <- grDevices::dev.cur()
plot.new()
plot.window(xlim = c(-0.2, 2.2), ylim = c(0, 1))
plot_sankey_ribbons(sankey_ribbons(thic, loc), raster_fill = TRUE, label_nodes = FALSE)
stopifnot(grDevices::dev.cur() == main_dev, other_dev %in% grDevices::dev.list())
grDevices::dev.off(main_dev)
grDevices::dev.off(other_dev)
stopifnot(
  any(png::readPNG(raster_main)[, , 1:3] < 0.99),
  all(png::readPNG(raster_other)[, , 1:3] > 0.99)
)
unlink(c(raster_other, raster_main))

render_md5 <- function(draw, width = 300, height = 200) {
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = width, height = height)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-0.2, 3.2), ylim = c(0, 1))
  draw()
  grDevices::dev.off()
  on.exit(unlink(file))
  unname(tools::md5sum(file))
}

# Blank or NA labels leave that node unlabeled, including after re-parsing the table.
partial_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(partial_pdf, width = 5, height = 3)
plot.new()
plot.window(xlim = c(0, 3), ylim = c(0, 1))
partial_loc <- data.frame(
  set = c(1, 1, 2, 2), node = c(1, 2, 1, 2), x = c(0, 0, 2, 2), y = c(0.7, 0.3, 0.7, 0.3),
  label = c("Alpha", NA, "", "Delta")
)
partial_table <- as_sankey_loc_table(partial_loc, node_label_pad = 0.02)
reparsed_table <- as_sankey_loc_table(partial_table, node_label_pad = 0.02)
grDevices::dev.off()
unlink(partial_pdf)
stopifnot(
  identical(partial_table$has_label, c(TRUE, FALSE, FALSE, TRUE)),
  all(partial_table$node_width[2:3] == 0),
  all(partial_table$node_label_width[2:3] == 0),
  all(partial_table$node_width[c(1, 4)] > 0.04),
  identical(reparsed_table$has_label, partial_table$has_label)
)

# add = FALSE opens a window with the aspect ratio the geometry was built for.
aspect_png <- tempfile(fileext = ".png")
grDevices::png(aspect_png, width = 900, height = 300)
aspect_sankey <- sankey_ribbons(list(thic, thic2), multi_loc)
plot_sankey_ribbons(aspect_sankey, add = FALSE, label_nodes = FALSE)
stopifnot(abs(xyrat_local() - aspect_sankey$xyr) < 1e-9)
grDevices::dev.off()
unlink(aspect_png)

# With both supplied, node_col drives strand gradients and merge_col the trunks.
color_multi <- sankey_ribbons(list(thic, thic2), multi_loc, xyr = 1)
color_nodes <- list(c("#AA0000", "#00AA00", "#0000AA"), c("#AAAA00", "#00AAAA"), c("#AA00AA", "#555555"))
color_merges <- lapply(color_nodes, function(x) rep("#222222", length(x)))
merge_only <- render_md5(function() {
  plot_sankey_ribbons(color_multi, merge_col = color_merges, gradient_slices = 40,
                      gradient_mode = "path", label_nodes = FALSE)
})
node_and_merge <- render_md5(function() {
  plot_sankey_ribbons(color_multi, node_col = color_nodes, merge_col = color_merges,
                      gradient_slices = 40, gradient_mode = "path", label_nodes = FALSE)
})
stopifnot(!identical(merge_only, node_and_merge))

# Borders are drawn even when merge zones cover whole strands; multi-stage node sums are drawn.
no_border <- render_md5(function() {
  plot_sankey_ribbons(sankey_ribbons(thic, loc, xyr = 1, merge_frac = 0.6), label_nodes = FALSE)
})
long_merge_border <- render_md5(function() {
  plot_sankey_ribbons(sankey_ribbons(thic, loc, xyr = 1, merge_frac = 0.6), border = "black", label_nodes = FALSE)
})
without_sums <- render_md5(function() plot_sankey_ribbons(color_multi, label_nodes = FALSE))
with_sums <- render_md5(function() plot_sankey_ribbons(color_multi, label_nodes = FALSE, show_node_sums = TRUE))
stopifnot(!identical(no_border, long_merge_border), !identical(without_sums, with_sums))
expect_error(
  draw_multi_stage_interior_trunks(color_multi, node_cols = NULL, merge_cols = NULL, merge_length = 0.1),
  "need `merge_cols` or `node_cols`"
)

# Path gradients color each x slice by the path distance at the slice's x midpoint.
steep <- sankey_ribbons(matrix(0.05), rbind(c(1, 1, 0, 0.95), c(2, 1, 1, 0.05)), xyr = 1, merge_frac = 0)
slice_cols <- character(0)
polygon <- function(x, y, col = NA, ...) slice_cols <<- c(slice_cols, col)
draw_sankey_fill_layer(steep, fill_cols = "#FF0000", gradient_slices = 4, merge_length = 0,
                       gradient_mode = "path", merge_cols = list(start = "#000000", end = "#FFFFFF"))
rm(polygon)
steep_distances <- edge_distances(steep$centerlines[[1]], 1)
expected_t <- vapply((seq_len(4) - 0.5) / 4, function(u) {
  distance_at_x(steep$centerlines[[1]], steep_distances, u) / max(steep_distances)
}, numeric(1))
expected_cols <- vapply(expected_t, function(t) {
  if(t <= 0.5) interpolate_color("#000000", "#FF0000", 2 * t) else interpolate_color("#FF0000", "#FFFFFF", 2 * t - 1)
}, character(1))
stopifnot(identical(slice_cols, expected_cols), abs(expected_t[1] - 0.125) > 0.02)

# Zero-width labeled interior nodes get a solid trunk, not a label-body fade.
beside_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(beside_pdf, width = 5, height = 3)
plot.new()
plot.window(xlim = c(-0.2, 3.2), ylim = c(0, 1))
beside_loc <- data.frame(set = multi_loc[, 1], node = multi_loc[, 2], x = multi_loc[, 3], y = multi_loc[, 4],
                         label = paste("node", seq_len(nrow(multi_loc))), width = 0)
beside <- sankey_ribbons(list(thic, thic2), beside_loc, xyr = 1)
trunk_polygons <- 0
polygon <- function(...) trunk_polygons <<- trunk_polygons + 1
draw_multi_stage_interior_trunks(beside, node_cols = NULL, merge_cols = multi_stage_merge_cols(beside, "#5B8CBA"),
                                 merge_length = beside$merge_length, gradient_slices = 20)
rm(polygon)
grDevices::dev.off()
unlink(beside_pdf)
stopifnot(trunk_polygons == beside$node_counts[2])

# stack_order fixes strand order per node side; unlisted strands stay above listed ones.
stack_rows <- function(s, to) {
  f <- s$flows[s$flows$to == to, ]
  f$from[order(f$end_offset)]
}
manual_single <- sankey_ribbons(thic, loc, xyr = 1,
                                stack_order = data.frame(side = "end", from = c(1, 3), to = 1, rank = c(1, 2)))
manual_multi <- sankey_ribbons(list(thic, thic2), multi_loc, xyr = 1,
                               stack_order = data.frame(side = "start", stage = 1, from = 1, to = 1, rank = 1))
auto_multi <- sankey_ribbons(list(thic, thic2), multi_loc, xyr = 1)
source_stack <- function(s) {
  f <- s$segments[[1]]$flows[s$segments[[1]]$flows$from == 1, ]
  f$to[order(f$start_offset)]
}
manual_graph <- sankey_ribbons_graph(graph_thic, graph_loc, xyr = 1, target_order = "source_bezier",
                                     stack_order = data.frame(side = "end", from = c(3, 1), to = 4, rank = 1:2))
stopifnot(
  identical(stack_rows(manual_single, 1), c(1L, 3L, 2L)),
  identical(stack_rows(sankey_ribbons(thic, loc, xyr = 1), 1), c(3L, 2L, 1L)),
  identical(source_stack(auto_multi), c(2L, 1L)),
  identical(source_stack(manual_multi), c(1L, 2L)),
  identical(manual_multi$segments[[2]]$flows, auto_multi$segments[[2]]$flows),
  identical(stack_rows(manual_graph, 4), c(3L, 1L))
)
expect_error(
  sankey_ribbons(thic, loc, xyr = 1, stack_order = data.frame(side = "end", from = 9, to = 1, rank = 1)),
  "no positive flow"
)
expect_error(
  sankey_ribbons(list(thic, thic2), multi_loc, xyr = 1,
                 stack_order = data.frame(side = "end", from = 1, to = 1, rank = 1)),
  "needs a stage column"
)

cat("sankey tests passed\n")
