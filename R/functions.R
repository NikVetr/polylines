#functions
xyrat <- function(){
  prop <- c(diff(par("usr")[1:2]), diff(par("usr")[3:4])) / par("pin")
  prop[1] / prop[2]
}

polylines <- function(x, y = NULL, lwd, col = 1, border = NULL, complex = F, 
                      draw_indiv = F, eps = 1E-9, xpd = NA, ...){
  
  if(is.null(y)){
    y <- x[,2]
    x <- x[,1]
  }
  
  # if(any(lwd <= 0))
  npts <- length(x)
  
  #adjust x and y coords if any are identical (messes with later calculations)
  dx <- diff(x)
  if(any(dx==0)){dx[dx==0] <- rep(eps, sum(dx==0))}
  xf <- min(abs(dx)[abs(dx) != 0]) / 1E3
  x <- cumsum(c(x[1], dx + xf))
  
  dy <- diff(y)
  if(any(dy==0)){dy[dy==0] <- rep(eps, sum(dy==0))}
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
              col = ifelse(length(col)==(length(x)-1), col[i-1], col), xpd = NA, ...)
    }
    return()
  }
  
  #this can be a complex polygon, so we should make it simple
  #by finding the union of the component quadrilaterals 
  if(complex){
    poly_coords <- data.frame(x = c(poly_pts$x0, rev(poly_pts$x1), poly_pts$x0[1]),
                              y = c(poly_pts$y0, rev(poly_pts$y1), poly_pts$y0[1]))
    polygon(poly_coords$x, poly_coords$y, 
            border = border, col = col, xpd = NA, ...)
    
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
            border = border, col = col, xpd = NA, ...)
    
  }
}

rescale01 <- function(x) {(x - min(x)) / diff(range(x))}

taper_ends <- function(n, p = 0.2, k = 10, ends = "both"){
  if(n == 0){return(integer(0))}
  if(n == 1){return(1)}
  end_head <- plogis(rescale01(1:n) * k - k*p)
  if(ends == "1" || ends == 1){return(end_head)}
  if(ends == "2" || ends == 2){return(rev(end_head))}
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

dexp_smooth <- function(x, y, r = NA){
  
  if(is.na(r)){
    r <- mean(abs(diff(x)))
    # m <- diff(y) / diff(x)
    # mp <- diff(m)
    # mmp <- log(mean(exp(mp)))
  }
  
  n <- length(x)
  w <- t(sapply(1:(n-1), function(i) 
    c(dexp((x[i] - x[0:(i-1)]), rate = r), 
      dexp(0, rate = r), 
      dexp(-(x[i] - x[(i+1):n]), rate = r)
      )
    ))
  w <- rbind(w, dexp(x[n] - x, rate = r))
  wy <- w %*% t(t(y))
  wy <- wy / apply(w,1,sum)
  
  return(wy)
}

x1 <- sin(seq(0,pi,length.out=100))
x2 <- seq(0,1,length.out=50)

smooth_appendage <- function(x1, x2, r = 0.5, qtexp = 0.99){
  
  #append lines
  xc <- c(x1, x2)
  n1 <- length(x1)
  n2 <- length(x2)
  n <- length(xc)
  
  #extract sublines (where exponential smoothing has an effect)
  n_inds_join <- ceiling(qexp(qtexp, rate = r))
  x1s <- tail(x1, n_inds_join)
  x2s <- head(x2, n_inds_join)
  xcs <- c(x1s, x2s)
  n1s <- length(x1s)
  n2s <- length(x2s)
  ns <- length(xcs)
  
  #pad ends
  x1p <- tail(head(x1, n1-n_inds_join), n_inds_join)
  if(length(x1p) < n_inds_join){ #extend if too short
    x1p <- c(rev(head(x1p,1) - 1:(n_inds_join - length(x1p))*diff(head(x1p, 2))), x1p)
  }
  
  x2p <- tail(head(x2, n2-n_inds_join), n_inds_join)
  if(length(x2p) < n_inds_join){ #extend if too short
    x2p <- c(rev(tail(x2p,1) + 1:(n_inds_join - length(x2p))*diff(tail(x2p, 2))), x2p)
  }
  
  xce <- c(x1p, xcs, x2p)
  
  #perform smoothing and extract at original indices
  t <- 1:length(xce)
  xse <- dexp_smooth(x = t, y = xce, r = r)
  xss <- xse[ns+1:ns]
  
  #insert back into the original sequence
  xs <- xc
  xs[(n1-n1s+1):(n1+n2s)] <- xss
  
  return(xs)
  
}

rotmat_00 <- function(t){r <- t / 360 * 2 * pi; matrix(c(cos(r), -sin(r), sin(r), cos(r)), 2, 2, byrow = T)}

draw_DNA <- function(
    n_strand = 1000,
    dh_bounds = c(-1.5 * pi, 3.5 * pi),
    amplitude = 1,
    phase_shift = pi,
    frequency = 1,
    base_pair_interval = pi / 6, #should really be 5.25 for 
    proportional_distance = 0.2,
    draw_internal_strand = FALSE,
    alternate_sides = FALSE,
    partial_displacement = TRUE,
    chirality_prop = 0.4,
    prop_square_wave = 0.2,
    bp_thickness = 1,
    strand_thickness = 0.1,
    extend_straight = "n",
    extend_straight_by = pi * 2,
    rot = 0,
    target_center = c(0,0),
    box_dim = c(10,10),
    col = 1,
    ...
) {
  
  # initializing the main data frame
  xyr <- xyrat()
  t <- seq(dh_bounds[1], dh_bounds[2], length.out = n_strand)   # Position along the strand for a single twist
  
  strand_df <- data.frame(
    y1 = t,
    y2 = t,
    x1 = amplitude * sin(frequency * t) * xyr,         # First strand x-coordinates
    x2 = amplitude * sin(frequency * t + phase_shift) * xyr,  # Second strand x-coordinates
    z1 = amplitude * sin(frequency * t + pi/2),         # First strand x-coordinates
    z2 = amplitude * sin(frequency * t + phase_shift + pi/2)  # Second strand x-coordinates
  )
  
  #add in straight segment to one end
  
  #calculate straight points
  straight_y <- seq(dh_bounds[2], dh_bounds[2] + 
                      extend_straight_by, by = diff(dh_bounds)/n_strand)
  nstraight <- length(straight_y)
  
  if(extend_straight != "n"){
    strand_df <- rbind(strand_df, 
                       data.frame(
                         y1 = straight_y,
                         y2 = straight_y,
                         x1 = rep(strand_df$x1[n_strand], nstraight),
                         x2 = rep(strand_df$x2[n_strand], nstraight),
                         z1 = rep(strand_df$z1[n_strand], nstraight),
                         z2 = rep(strand_df$z2[n_strand], nstraight)
                       ))
  }
  
  if(extend_straight == "1" || extend_straight == 1){
    strinds <- (n_strand+1):(n_strand+nstraight)
    strand_df$y2[strinds] <- strand_df$y2[n_strand]
  }
  
  if(extend_straight == "2" || extend_straight == 2){
    strinds <- (n_strand+1):(n_strand+nstraight)
    strand_df$y1[strinds] <- strand_df$y1[n_strand]
  }
  
  #compute high level parameters
  bounds <- range(c(strand_df$y1, strand_df$y2))
  maxw_strand <- range(strand_df$x1)
  
  #distort away from perfect sine wave
  # strand_df$x1 <- strand_df$x1 * asinh(abs(strand_df$x1)^-prop_square_wave)
  # strand_df$x2 <- strand_df$x2 * asinh(abs(strand_df$x2)^-prop_square_wave)
  strand_df$x1 <- abs(strand_df$x1)^(1-prop_square_wave) * sign(strand_df$x1) * prop_square_wave + 
    (1-prop_square_wave) * strand_df$x1
  strand_df$x2 <- abs(strand_df$x2)^(1-prop_square_wave) * sign(strand_df$x2) * prop_square_wave + 
    (1-prop_square_wave) * strand_df$x2
  
  #visual width of the strand
  strand_df$lwd1 <- (rescale01(strand_df$z1) * sqrt(strand_thickness))^2 + strand_thickness / 2
  strand_df$lwd2 <- (rescale01(strand_df$z2) * sqrt(strand_thickness))^2 + strand_thickness / 2
  
  # Maximum width between the strands and whitespace to be left over
  max_width <- max(abs(strand_df$x2 - strand_df$x1))
  ws_strand <- proportional_distance * max_width
  
  # Base pair indices
  if(extend_straight == "b" || extend_straight == "n"){
    bp_indices <- sapply(seq(bounds[1], bounds[2], by = base_pair_interval), 
                         function(pos) which.min(abs(strand_df$y1 - pos)))
  } else {
    bp_indices <- sapply(seq(bounds[1], bounds[2], by = base_pair_interval), 
                         function(pos) which.min(abs(t - pos)))
  }
  
  # Creating bp_df
  bp_df <- data.frame(
    y0 = strand_df$y1[bp_indices],
    x0 = strand_df$x1[bp_indices],
    y1 = strand_df$y2[bp_indices],
    x1 = rep(NA, length(bp_indices)),
    w = rep(NA, length(bp_indices)),
    dir = rep(NA, length(bp_indices)),
    lwd0 = strand_df$lwd1[bp_indices],
    lwd1 = strand_df$lwd2[bp_indices]
  )
  
  # Fill in w and dir in bp_df
  bp_df$w <- pmax(abs(strand_df$x2[bp_indices] - strand_df$x1[bp_indices]) - ws_strand, 0)
  bp_df$dir <- ifelse(bp_df$x0 >= 0, -1, 1)  # -1 for left, 1 for right
  
  # Correctly calculate x1 based on w and dir
  bp_df$x1 <- bp_df$x0 + (bp_df$w * bp_df$dir)
  
  # Ensure x1 values do not exceed the strand limits
  bp_df$x1 <- pmin(pmax(bp_df$x1, min(strand_df$x1, strand_df$x2)), max(strand_df$x1, strand_df$x2))
  
  #if alternating sides, displace x values by the ws remainging
  if(alternate_sides){
    bp_df$x0 <- bp_df$x0 + 
      (ws_strand * as.numeric(bp_df$dir==1) - 
         ws_strand * as.numeric(bp_df$dir==-1)) * rep(0:1, length.out = nrow(bp_df)) / 
      ifelse(partial_displacement, 2, 1)
    bp_df$x1 <- bp_df$x1 + 
      (ws_strand * as.numeric(bp_df$dir==1) - 
         ws_strand * as.numeric(bp_df$dir==-1)) * rep(0:1, length.out = nrow(bp_df)) / 
      ifelse(partial_displacement, 2, 1)
  }
  
  #remove 0 width bases
  bp_df <- bp_df[bp_df$w > 1E-6,] 
  
  #adjust for chirality
  strand_df$seen <- abs(strand_df$x1 - strand_df$x2) > chirality_prop
  on_top_1 <- diff(strand_df$x1) > 0
  strand_df$on_top_1 <- c(on_top_1[1], on_top_1)
  strand_df$seen_1 <- strand_df$seen_2 <- T
  strand_df$seen_1[!strand_df$seen & !strand_df$on_top_1] <- F
  strand_df$seen_2[!strand_df$seen & strand_df$on_top_1] <- F
  
  #rotate if requested
  center <- c(x = mean(c(strand_df$x1, strand_df$x2)), 
              y = mean(c(strand_df$y1, strand_df$y2)), 
              z = mean(c(strand_df$z1, strand_df$z2)))
  rotmat <- rotmat_00(rot)
  strand_df[,c("x1", "y1")] <- t(t(as.matrix(t(t(strand_df[,c("x1", "y1")]) - 
                                                 center[c("x", "y")])) %*% rotmat) + target_center)
  strand_df[,c("x2", "y2")] <- t(t(as.matrix(t(t(strand_df[,c("x2", "y2")]) - 
                                                 center[c("x", "y")])) %*% rotmat) + target_center)
  bp_df[,c("x0", "y0")] <- t(t(as.matrix(t(t(bp_df[,c("x0", "y0")]) - 
                                             center[c("x", "y")])) %*% rotmat) + target_center)
  bp_df[,c("x1", "y1")] <- t(t(as.matrix(t(t(bp_df[,c("x1", "y1")]) - 
                                             center[c("x", "y")])) %*% rotmat) + target_center)
  
  
  #rescale to size of bounding box
  min_pts <- c(x = min(c(strand_df$x1, strand_df$x2)), 
               y = min(c(strand_df$y1, strand_df$y2)), 
               z = min(c(strand_df$z1, strand_df$z2)))
  sdf_pts <- strand_df[,c("x1", "y1", "z1", "x2", "y2", "z2")]
  bdf_pts <- bp_df[,c("x0", "y0", "x1", "y1")]
  
  sdf_pts <- t(t(sdf_pts) - c(min_pts, min_pts))
  bdf_pts <- t(t(bdf_pts) - c(min_pts[c("x", "y")], min_pts[c("x", "y")]))
  max_pts <- apply(sdf_pts, 2, max)
  max_pts <- c(x = max(max_pts[c("x1", "x2")]), 
               y = max(max_pts[c("y1", "y2")]), 
               z = max(max_pts[c("z1", "z2")]))
  rescale_factor <- min(box_dim / max_pts[c("x", "y")])
  sdf_pts <- t(t(sdf_pts * rescale_factor))
  bdf_pts <- t(t(bdf_pts * rescale_factor))
  
  #recenter these
  center <- c(x = mean(sdf_pts[,c("x1", "x2")]), 
              y = mean(sdf_pts[,c("y1", "y2")]), 
              z = mean(sdf_pts[,c("z1", "z2")]))
  sdf_pts[,c("x1", "y1", "x2", "y2")] <- t(t(sdf_pts[,c("x1", "y1", "x2", "y2")]) - center[c("x", "y", "x", "y")] + 
                                             c(target_center, target_center))
  bdf_pts[,c("x0", "y0", "x1", "y1")] <- t(t(bdf_pts[,c("x0", "y0", "x1", "y1")]) - center[c("x", "y", "x", "y")] + 
                                             c(target_center, target_center))
  
  
  #and reassign
  strand_df[,c("x1", "y1", "z1", "x2", "y2", "z2")] <- sdf_pts
  bp_df[,c("x0", "y0", "x1", "y1")] <- bdf_pts
  
  
  #find where the strands go underneath each other
  seen_sets_1 <- split(which(strand_df$seen_1), cumsum(!strand_df$seen_1)[strand_df$seen_1])
  seen_sets_2 <- split(which(strand_df$seen_2), cumsum(!strand_df$seen_2)[strand_df$seen_2])
  
  ss1_n <- sapply(seen_sets_1, length)
  ss1_thresh_n <- mean(range(ss1_n))
  ss2_n <- sapply(seen_sets_2, length)
  ss2_thresh_n <- mean(range(ss2_n))
  
  #strand 1
  for(i in seq_along(seen_sets_1)){
    seen_set <- seen_sets_1[[i]]
    sdf <- strand_df[seen_set,]
    polylines(sdf$x1, sdf$y1, lwd = sdf$lwd1, col = col, ...)
  }
  
  #strand 2
  for(i in seq_along(seen_sets_2)){
    seen_set <- seen_sets_2[[i]]
    sdf <- strand_df[seen_set,]
    polylines(sdf$x2, sdf$y2, lwd = sdf$lwd2, col = col, ...)
  }
  
  nbp <- n_strand / nrow(bp_df) 
  for(i in 1:nrow(bp_df)){
    segments(bp_df$x0[i], bp_df$y0[i], bp_df$x1[i], bp_df$y1[i], col = "black")
    xbp <- seq(bp_df$x0[i], bp_df$x1[i], length.out = nbp)
    ybp <- seq(bp_df$y0[i], bp_df$y1[i], length.out = nbp)
    
    #length and direction tell us how wide the nucs are
    lwd <- seq(bp_df$lwd0[i], 
               bp_df$lwd1[i] - (bp_df$lwd1[i] - bp_df$lwd0[i]) * proportional_distance, 
               length.out = nbp)
    lwd <- rescale01(lwd)^2 * diff(range(lwd)) + min(lwd)
    lwd <- lwd * bp_thickness
    polylines(x = xbp, y = ybp, lwd = lwd, col = col, ...)
  }
  
}