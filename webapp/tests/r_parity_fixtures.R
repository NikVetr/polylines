# Generate R reference geometry for the webapp's layout parity tests.
#
# Run from the repository root:
#   Rscript webapp/tests/r_parity_fixtures.R
# Writes webapp/tests/fixtures/r_parity.json. All cases use xyr = 1 so R data
# coordinates equal the webapp's isotropic plot pixels.

source("R/functions.R")

loc_rows <- function(loc) {
  loc <- as.data.frame(loc)
  names(loc)[1:4] <- c("set", "node", "x", "y")
  if(ncol(loc) >= 5 && !"width" %in% names(loc)) names(loc)[5] <- "width"
  lapply(seq_len(nrow(loc)), function(i) as.list(loc[i, intersect(names(loc), c("set", "node", "x", "y", "width"))]))
}

flow_rows <- function(segment, stage) {
  lapply(seq_len(nrow(segment$flows)), function(k) {
    f <- segment$flows[k, ]
    edges <- segment$edges[[k]]
    left_up <- mean(edges$left[, "y"]) >= mean(edges$right[, "y"])
    list(
      stage = stage,
      i = f$from - 1,
      j = f$to - 1,
      start_offset = f$start_offset,
      end_offset = f$end_offset,
      start_x = f$start_x,
      end_x = f$end_x,
      centerline = unname(segment$centerlines[[k]]),
      upper = unname(if(left_up) edges$left else edges$right),
      lower = unname(if(left_up) edges$right else edges$left)
    )
  })
}

run_case <- function(name, thic, loc, params = list(), graph = FALSE) {
  args <- c(list(thic = thic, loc = loc, xyr = 1), params)
  sankey <- do.call(if(graph) sankey_ribbons_graph else sankey_ribbons, args)
  flows <- if(is_sankey_multi(sankey)) {
    do.call(c, lapply(seq_along(sankey$segments), function(s) flow_rows(sankey$segments[[s]], s - 1)))
  } else {
    flow_rows(sankey, 0)
  }
  out_loc <- as.data.frame(sankey$loc)
  matrices <- if(is.list(thic)) thic else list(thic)
  list(
    name = name,
    mode = if(graph) "graph" else "layered",
    matrices = lapply(matrices, function(m) lapply(seq_len(nrow(m)), function(r) I(unname(m[r, ])))),
    loc = loc_rows(loc),
    params = params,
    expected = list(
      flows = flows,
      loc_y = unname(out_loc[[if("y" %in% names(out_loc)) "y" else 4]]),
      merge_length = sankey$merge_length
    )
  )
}

# Shared inputs from example_3.R and tests/test_sankey.R.
thic_1 <- matrix(c(0.35, 0.25, 0.05, 0.10, 0.10, 0.35, 0.20, 0.05, 0.05, 0.10, 0.30, 0.20), 3, byrow = TRUE)
thic_3 <- matrix(c(0.25, 0.15, 0.10, 0.10, 0.40, 0.20, 0.05, 0.15, 0.35, 0.12, 0.06, 0.17), 4, byrow = TRUE)
multi_loc <- rbind(
  cbind(1, 1:3, 0.0, c(3.0, 2.0, 1.0)),
  cbind(2, 1:4, 1.5, c(3.3, 2.4, 1.5, 0.6)),
  cbind(3, 1:4, 2.5, c(3.3, 2.4, 1.5, 0.6)),
  cbind(4, 1:3, 4.0, c(3.0, 2.0, 1.0))
)
multi_thic <- list(thic_1, diag(colSums(thic_1)), thic_3)

adjust_thic_1 <- matrix(c(
  0.08, 0.02, 0.00, 0.01, 0.00, 0.03, 0.06, 0.02, 0.00, 0.00, 0.00, 0.04, 0.05, 0.02, 0.00,
  0.00, 0.00, 0.03, 0.06, 0.02, 0.02, 0.00, 0.00, 0.03, 0.06), 5, byrow = TRUE)
adjust_thic_2 <- matrix(c(
  0.06, 0.03, 0.01, 0.02, 0.02, 0.06, 0.03, 0.01, 0.01, 0.03, 0.05, 0.02,
  0.00, 0.02, 0.06, 0.03, 0.03, 0.00, 0.02, 0.04), 5, byrow = TRUE)
adjust_thic_2 <- adjust_thic_2 * (colSums(adjust_thic_1) / rowSums(adjust_thic_2))
adjust_loc <- rbind(
  cbind(1, 1:5, 0.0, c(0.12, 0.34, 0.365, 0.62, 0.88)),
  cbind(2, 1:5, 1.35, c(0.14, 0.33, 0.355, 0.58, 0.86)),
  cbind(3, 1:4, 2.70, c(0.16, 0.43, 0.455, 0.84))
)

unequal_thic_1 <- matrix(c(0.18, 0.07, 0.04, 0.08, 0.16, 0.07, 0.04, 0.10, 0.14), 3, byrow = TRUE)
unequal_thic_2 <- matrix(c(0.18, 0.07, 0.04, 0.07, 0.16, 0.08, 0.05, 0.08, 0.15), 3, byrow = TRUE)
unequal_thic_2 <- unequal_thic_2 * (colSums(unequal_thic_1) / rowSums(unequal_thic_2))
unequal_loc <- rbind(
  cbind(1, 1:3, c(0.00, 0.22, 0.08), c(0.76, 0.50, 0.24)),
  cbind(2, 1:3, c(1.24, 1.55, 1.36), c(0.78, 0.50, 0.22)),
  cbind(3, 1:3, c(2.88, 2.58, 2.96), c(0.74, 0.50, 0.26))
)

small_thic <- matrix(c(0.10, 0.04, 0.03, 0.08, 0.06, 0.02), 3, byrow = TRUE)
small_loc <- rbind(cbind(1, 1:3, 0, c(0.78, 0.50, 0.22)), cbind(2, 1:2, 2, c(0.68, 0.30)))
mirror_loc <- small_loc
mirror_loc[, 3] <- 2 - small_loc[, 3]

dense_thic <- matrix(c(0.010, 0.060, 0.012, 0.085, 0.014, 0.070, 0.020, 0.105,
                       0.018, 0.075, 0.013, 0.055, 0.011, 0.045, 0.010), ncol = 1)
dense_w <- as.numeric(dense_thic)
dense_y <- 0.86 - c(0, cumsum((head(dense_w, -1) + tail(dense_w, -1)) / 2 + 0.0015))
dense_loc <- rbind(cbind(1, seq_along(dense_w), 0, dense_y), cbind(2, 1, 2.7, -0.50))
fanout_loc <- rbind(cbind(1, 1, 0, -0.50), cbind(2, seq_along(dense_w), 2.7, dense_y))

graph_loc <- rbind(
  c(1, 1, 0.0, 0.70, 0.10),
  c(1, 2, 0.0, 0.30, 0.10),
  c(2, 1, 1.4, 0.55, 0.22),
  c(3, 1, 2.8, 0.62, 0.12),
  c(3, 2, 2.8, 0.28, 0.12)
)
graph_thic <- matrix(0, 5, 5)
graph_thic[1, 3] <- 0.05
graph_thic[2, 3] <- 0.03
graph_thic[3, 4] <- 0.08
graph_thic[1, 4] <- 0.02
graph_thic[2, 5] <- 0.04

projected_loc <- cbind(c(1, 1, 2), c(1, 2, 1), c(0.0, 1.8, 3.0), c(0.80, 0.60, 0.20))
projected_thic <- matrix(0, 3, 3)
projected_thic[1, 3] <- 0.04
projected_thic[2, 3] <- 0.04

bezier_order_loc <- cbind(1:4, 1:4,
                          c(1.1659810, 1.4409270, 0.6519892, 3.0),
                          c(0.4250294, -0.2000113, -0.3492957, 0.2570871),
                          c(0.16215380, 0.56889698, 0.09735354, 0.0))
bezier_order_thic <- matrix(0, 4, 4)
bezier_order_thic[1, 4] <- 0.03472931
bezier_order_thic[2, 4] <- 0.02859826
bezier_order_thic[3, 4] <- 0.03437776

cases <- list(
  run_case("multi_bezier", multi_thic, multi_loc, list(n = 30, merge_frac = 0.075)),
  run_case("multi_stage_routes", list(small_thic, diag(colSums(small_thic))),
           rbind(small_loc, cbind(3, 1:2, 3, c(0.68, 0.30))),
           list(n = 30, route = list("boundary", "bezier"), boundary_group = list("target", "target"))),
  run_case("adjust_expand_false", list(adjust_thic_1, adjust_thic_2), adjust_loc,
           list(n = 20, adjust_loc = TRUE, min_node_gap = 0.015, adjust_loc_expand = FALSE)),
  run_case("adjust_expand_true", list(unequal_thic_1, unequal_thic_2), unequal_loc,
           list(n = 20, adjust_loc = TRUE, min_node_gap = 0.012, adjust_loc_expand = TRUE)),
  run_case("orders_desc_index", small_thic, small_loc,
           list(n = 25, source_order = "target_y_desc", target_order = "source_index")),
  run_case("leftward", small_thic, mirror_loc, list(n = 25)),
  run_case("boundary_none", dense_thic, dense_loc, list(n = 40, route = "boundary", boundary_compensation = "none")),
  run_case("boundary_full", dense_thic, dense_loc, list(n = 40, route = "boundary", boundary_compensation = "full")),
  run_case("boundary_gap_budget", dense_thic, dense_loc,
           list(n = 40, route = "boundary", boundary_compensation = "gap_budget", boundary_ease = 1.6)),
  run_case("boundary_source_fanout", t(dense_thic), fanout_loc,
           list(n = 40, route = "boundary", boundary_group = "source", boundary_compensation = "full")),
  run_case("graph_widths", graph_thic, graph_loc, list(n = 30), graph = TRUE),
  run_case("graph_projected", projected_thic, projected_loc, list(n = 20, target_order = "source_projected"), graph = TRUE),
  run_case("graph_bezier_order", bezier_order_thic, bezier_order_loc, list(n = 40, target_order = "source_bezier"), graph = TRUE),
  run_case("multi_stack_order", multi_thic, multi_loc, list(
    n = 20,
    stack_order = data.frame(side = c("start", "start", "end", "end"), stage = c(1, 1, 3, 3),
                             from = c(2, 2, 1, 4), to = c(1, 3, 2, 2), rank = c(2, 1, 1, 2))
  )),
  run_case("graph_stack_order_bezier", graph_thic, graph_loc, list(
    n = 30,
    target_order = "source_bezier",
    stack_order = data.frame(side = c("end", "end", "start", "start"), from = c(3, 1, 1, 1),
                             to = c(4, 4, 3, 4), rank = c(1, 2, 2, 1))
  ), graph = TRUE)
)

dir.create("webapp/tests/fixtures", showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(cases, "webapp/tests/fixtures/r_parity.json", auto_unbox = TRUE, digits = I(12))
cat(sprintf("wrote %d parity cases\n", length(cases)))
