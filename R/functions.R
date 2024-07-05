#functions
xyrat <- function(){
  prop <- c(diff(par("usr")[1:2]), diff(par("usr")[3:4])) / par("pin")
  prop[1] / prop[2]
}

polylines <- function(x, y = NULL, lwd, col = 1, border = NULL, complex = F, 
                      draw_indiv = F, eps = 1E-9, ...){
  
  if(is.null(y)){
    y <- x[,2]
    x <- x[,1]
  }
  
  # if(any(lwd <= 0))
  npts <- length(x)
  
  #adjust x and y coords if any are identical (messes with later calculations)
  dx <- diff(x)
  if(all(dx==0)){dx <- rep(eps, npts-1)}
  xf <- min(abs(dx)[abs(dx) != 0]) / 1E3
  x <- cumsum(c(x[1], dx + xf))
  
  dy <- diff(y)
  if(all(dy==0)){dy <- rep(eps, npts-1)}
  yf <- min(abs(dy)[abs(dy) != 0]) / 1E3
  y <- cumsum(c(y[1], dy + yf))
  
  # Calculate the current aspect ratio
  xyr <- xyrat()
  
  # calculate slopes of lines
  m <- diff(y) / diff(x)
  m <- c(m, m[length(m)])
  
  # fudge 0 slopes
  # m[m==0] <- min(abs(m[m!=0])) / 1E3
  # don't do this bc it is already handled
  
  #adjust for visual distortion
  mv <- m * xyr^2
  
  #calculate perpendicular slope
  mvi <- -1/mv
  
  #find length of vectors
  npts <- length(x)
  pts <- 1:length(x)
  
  #calculate endpoints of vertices
  endpts <- cbind(x0 = x - lwd / 2, 
                  x1 = x + lwd / 2, 
                  y0 = y - lwd / 2 * mvi, 
                  y1 = y + lwd / 2 * mvi)
  actual_lens <- sqrt((endpts[,"y1"] - endpts[,"y0"])^2 * xyr^2 + 
                        (endpts[,"x1"] - endpts[,"x0"])^2)
  
  #avoid dividing by 0 by adding a smidge
  smallest_nz_len <- min(actual_lens[actual_lens>0])
  actual_lens <- actual_lens + smallest_nz_len/1E3
  
  #calculate adjusted lengths to use
  lwds <- lwd^2 / actual_lens
  
  #identify if we are shifting slope on line horizontally or vertically
  # slope_scale <- exp(mean(log(abs(m))))
  # m_big <- abs(m) > slope_scale
  # swap_x <- sign(mvi)
  # swap_x[m_big] <- 1
  # swap_y <- sign(mvi)
  # swap_y[!m_big] <- 1
  
  #determine direction of curvature
  # rlex <- rle(sign(diff(x)))
  # rley <- rle(sign(diff(y)))
  # dxi <- data.frame(i1 = cumsum(rlex$lengths[-length(rlex$lengths)]),
  #              i2 = cumsum(rlex$lengths[-length(rlex$lengths)]) + 1,
  #              dir = rlex$values[-1])
  # dyi <- data.frame(i1 = cumsum(rley$lengths[-length(rley$lengths)]),
  #              i2 = cumsum(rley$lengths[-length(rley$lengths)]) + 1,
  #              dir = rley$values[-1])
  # 
  # #hm, only need to compensate when x or y goes from + to -
  # # points(x[dxi$i1], y[dxi$i1], col = 2)
  # # points(x[dyi$i1], y[dyi$i1], col = 3)
  # 
  # 
  # #get from the direction of x and y (if incr or decr)
  # if(nrow(dxi) == 0){
  #   sxi <- rep(1, rlex$values)
  # } else {
  #   sxi <- unlist(sapply(1:(nrow(dxi)+1), function(i){
  #     if(i == 1){
  #       return(rep(-dxi$dir[i], dxi$i1[i]))
  #     } else if (i == (nrow(dxi)+1)){
  #       return(rep(-dxi$dir[nrow(dxi)], length(x) - dxi$i2[nrow(dxi)] + 1))
  #     } else {
  #       return(rep(-dxi$dir[i-1], dxi$i1[i] - dxi$i2[i-1] + 1))
  #     }
  #   }))  
  # }
  # 
  # if(nrow(dyi) == 0){
  #   syi <- rep(1, rley$values)
  # } else {
  #   syi <- unlist(sapply(1:(nrow(dyi)+1), function(i){
  #     if(i == 1){
  #       return(rep(-dyi$dir[i], dyi$i1[i]))
  #     } else if (i == (nrow(dyi)+1)){
  #       return(rep(-dyi$dir[nrow(dyi)], length(x) - dyi$i2[nrow(dyi)] + 1))
  #     } else {
  #       return(rep(-dyi$dir[i-1], dyi$i1[i] - dyi$i2[i-1] + 1))
  #     }
  #   }))  
  # }
  
  # swap_x <- sign(m) * sxi
  # swap_y <- sign(m) * sxi
  
  # ...hmm, ok, easier than anticipated, just needed to write down table
  dxi <- sign(diff(x))
  dxi <- c(dxi[1], dxi)
  dyi <- sign(diff(y))
  dyi <- c(dyi[1], dyi)
  
  #worked
  swap_x <- -dyi
  swap_y <- dxi
  
  
  #multiply these to always point slope for *_0 inds to the left of direction of travel
  poly_pts <- data.frame(x0 = x + lwds / 2 * swap_x,
                         x1 = x - lwds / 2 * swap_x,
                         y0 = y + lwds / 2 * abs(mvi) * swap_y,
                         y1 = y - lwds / 2 * abs(mvi) * swap_y
                         )
  
  if(draw_indiv || length(col) == (length(x)-1)){
    for(i in 2:npts){
      poly_coords <- data.frame(x = c(poly_pts$x0[i-1], poly_pts$x0[i], 
                                      poly_pts$x1[i], poly_pts$x1[i-1]),
                                y = c(poly_pts$y0[i-1], poly_pts$y0[i], 
                                      poly_pts$y1[i], poly_pts$y1[i-1]))
      polygon(poly_coords$x, poly_coords$y, 
              border = border, 
              col = ifelse(length(col)==(length(x)-1), col[i-1], col), ...)
    }
    return()
  }
  
  
  #this can be a complex polygon, so we should make it simple
  #by finding the union of the component quadrilaterals 
  if(complex){
    poly_coords <- data.frame(x = c(poly_pts$x0, rev(poly_pts$x1), poly_pts$x0[1]),
                              y = c(poly_pts$y0, rev(poly_pts$y1), poly_pts$y0[1]))
    polygon(poly_coords$x, poly_coords$y, 
            border = border, col = col, ...)
    
  } else {
    
    quads_sf <- lapply(1:(npts-1), function(quad) {
      quad_pts <- poly_pts[quad:(quad+1),]
      quad_pts_big <- cbind(c(quad_pts$x0, rev(quad_pts$x1), quad_pts$x0[1]), 
                            c(quad_pts$y0, rev(quad_pts$y1), quad_pts$y0[1]))
      
      #convert to the st format
      return(sf::st_polygon(list(quad_pts_big))  
      )
    })
    
    #unite and buffer the resulting polygons
    multipolygon <- sf::st_sfc(quads_sf)
    multipolygon <- sf::st_make_valid(multipolygon)
    multipolygon <- sf::st_buffer(multipolygon, dist = 1E-6) #some artefacting from floating point arithmetic
    simple_polygon <- sf::st_union(multipolygon)
    poly_coords_simple <- as.data.frame(as.matrix(simple_polygon[[1]]))
    colnames(poly_coords_simple) <- c("x", "y")
    
    #plot
    polygon(poly_coords_simple$x, poly_coords_simple$y, 
            border = border, col = col, ...)
    
  }
}

rescale01 <- function(x) (x - min(x)) / diff(range(x))

taper_ends <- function(n, p = 0.2, k = 10, ends = "both"){
  end_head <- plogis(rescale01(1:n) * k - k*p)
  if(ends == "head"){return(end_head)}
  if(ends == "tail"){return(rev(end_head))}
  if(ends == "both"){return(end_head * rev(end_head))}
  if(ends == "revboth"){return(1 - end_head * rev(end_head))}
}

linear_interp <- function(x, n) {
  
  target_indices <- seq(1, length(x), length.out = n)
  
  # Get the integer part and the fractional part of the target indices
  lower_indices <- floor(target_indices)
  upper_indices <- ceiling(target_indices)
  
  # Calculate the weights for interpolation
  weights <- target_indices - lower_indices
  
  # Ensure the indices are within the valid range
  lower_indices[lower_indices < 1] <- 1
  upper_indices[upper_indices > length(x)] <- length(x)
  
  # Perform linear interpolation
  y <- (1 - weights) * x[lower_indices] + weights * x[upper_indices]
  
  return(y)
}
