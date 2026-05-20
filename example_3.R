# Clean Sankey ribbon demonstration.
#
# Run this file from the repository root to regenerate
# images/sankey_example.png.

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
text(loc[, 3], loc[, 4], labels = loc[, 2], col = node_cols_flat, cex = 0.75, font = 2)

grDevices::dev.off()
