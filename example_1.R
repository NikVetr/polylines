library(sf)

#functions
source("~/repos/polylines/R/functions.R")

#### demonstration! ####

# create coordinate variables
npts <- 200
x <- seq(0, 2 * pi, length.out = npts)
y <- sin(x)

# Define variable line widths
lwd <- 0.2 + 0.5 * abs(sin(x))^2

# Plot the base plot without lines
plot(x,y, type = "l", 
     xlim = range(x) + diff(range(x))/10 * c(-1,1), 
     ylim = range(y) + diff(range(y))/2 * c(-1,1))

polylines(x, y, lwd = lwd, col = adjustcolor(1, 0.2), complex = F)
# polylines(x, y, lwd = lwd, 
#           col = colorRampPalette(c("blue", "red"))(npts-1), complex = F, 
#           border = NA)

#save to svg
svg(file = "~/repos/polylines/images/polylines_example_1.svg", width = 10, height = 8, pointsize = 20)

plot(x, y, type = "l",
     xlim = range(x) + diff(range(x)) / 10 * c(-1, 1),
     ylim = range(y) + diff(range(y)) / 2 * c(-1, 1))

polylines(x, y, lwd = lwd, col = adjustcolor(1, 0.2), complex = F)

dev.off()

