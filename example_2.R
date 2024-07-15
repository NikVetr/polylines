library(sf)

#functions
source("~/repos/polylines/R/functions.R")

#### more elaborate examples ####
npts <- 200

svg(file = "~/repos/polylines/images/polylines_example_2.svg", width = 16, height = 10, pointsize = 20)

#initialize plot
plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))

#arrow
x <- seq(0, 2 * pi, length.out = npts)
y <- sin(x)
lwd <- rescale01(0.001 + sqrt((1:npts-1)/npts))
arrowhead_inds <- (npts-floor(npts/10)):npts
lwd[arrowhead_inds] <- rescale01(npts - arrowhead_inds) * max(lwd[arrowhead_inds]) * 1.75

polylines(x, y, lwd = lwd, col = adjustcolor(1, 0.2), complex = F, xpd= NA)

#cool spirals
x <- rescale01(sin(seq(0, 8 * pi, length.out = npts)) * 1:npts) * 5 * xyrat() + 0.5
y <- rescale01(cos(seq(0, 8 * pi, length.out = npts)) * 1:npts) * 5 + 3
lwd <- (rescale01(sin(seq(0, 12 * pi, length.out = npts))) * 0.2 + 0.1) * 
  plogis(1:npts/10 - 5) * plogis(npts:1/2 - 5)

polylines(x, y, lwd = lwd, complex = F, xpd = NA, draw_indiv = F, 
          col = adjustcolor(1, 0.2), border = 1)
          # col = colorRampPalette(c(1,2))(npts-1))
polylines(-x+7, y, lwd = lwd, col = adjustcolor(1, 0.2), complex = F, xpd= NA, border = 1)

#overlapping squiggle
x <- rescale01(sin(seq(0, 8 * pi, length.out = npts))) * 2 * xyrat() +
  rescale01(1:npts) * 6
y <- rescale01(cos(seq(0, 8 * pi, length.out = npts))) * 1.5 + 9
lwd <- (rescale01(max(y) - y) * 0.1 + 0.05) * plogis(1:npts/2 - 5) * plogis(npts:1/2 - 5)

polylines(x, y, lwd = lwd, complex = F, xpd= NA, 
          col = adjustcolor(1, 0.2))

#DNA molecule
draw_DNA(target_center = c(8,4.25), box_dim = c(4,12), strand_thickness = 0.099, rot = 0, 
         extend_straight = "b", chirality_prop = 0.2, amplitude = 1.25, col = 1, 
         dh_bounds = c(-pi/2, 3 * pi / 2), extend_straight_by = pi)

dev.off()
