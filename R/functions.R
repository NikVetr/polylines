#functions
xyrat <- function(){
  prop <- c(diff(par("usr")[1:2]), diff(par("usr")[3:4])) / par("pin")
  prop[1] / prop[2]
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
    center_with_extension = F,
    rot = 0,
    target_center = c(0,0),
    box_dim = NULL,
    col = 1,
    adjust_xyr = T,
    topleft_xy = NULL,
    forced_box = NULL,
    box_DNA = T,
    straight_extension_real_units = F,
    ...
) {
  
  #if we should adjust the relative sizes by the current plotting region proportions  
  if(adjust_xyr){
    xyr <- xyrat()  
  } else {
    xyr <- 1
  }
  
  # initializing the main data frame
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
                         z1 = seq(strand_df$z1[n_strand], min(strand_df$z1), length.out = nstraight),
                         z2 = seq(strand_df$z2[n_strand], min(strand_df$z2), length.out = nstraight)
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
  center <- c(x = mean(range(c(strand_df$x1, strand_df$x2))), 
              y = mean(range(c(strand_df$y1, strand_df$y2))), 
              z = mean(range(c(strand_df$z1, strand_df$z2))))
  rotmat <- rotmat_00(rot)
  strand_df[,c("x1", "y1")] <- t(t(as.matrix(t(t(strand_df[,c("x1", "y1")]) - 
                                                 center[c("x", "y")])) %*% rotmat) + target_center)
  strand_df[,c("x2", "y2")] <- t(t(as.matrix(t(t(strand_df[,c("x2", "y2")]) - 
                                                 center[c("x", "y")])) %*% rotmat) + target_center)
  bp_df[,c("x0", "y0")] <- t(t(as.matrix(t(t(bp_df[,c("x0", "y0")]) - 
                                             center[c("x", "y")])) %*% rotmat) + target_center)
  bp_df[,c("x1", "y1")] <- t(t(as.matrix(t(t(bp_df[,c("x1", "y1")]) - 
                                             center[c("x", "y")])) %*% rotmat) + target_center)
  
  #rescale to specified size
  min_pts <- c(x = min(c(strand_df$x1, strand_df$x2)), 
               y = min(c(strand_df$y1, strand_df$y2)), 
               z = min(c(strand_df$z1, strand_df$z2)))
  sdf_pts <- strand_df[,c("x1", "y1", "z1", "x2", "y2", "z2")]
  bdf_pts <- bp_df[,c("x0", "y0", "x1", "y1")]
  
  sdf_pts <- t(t(sdf_pts) - c(min_pts, min_pts))
  bdf_pts <- t(t(bdf_pts) - c(min_pts[c("x", "y")], min_pts[c("x", "y")]))
  if(box_DNA){
    max_pts <- apply(sdf_pts[1:n_strand,], 2, max)
  } else {
    max_pts <- apply(sdf_pts, 2, max)
  }
  
  max_pts <- c(x = max(max_pts[c("x1", "x2")]), 
               y = max(max_pts[c("y1", "y2")]), 
               z = max(max_pts[c("z1", "z2")]))
  
  #to the size of the bounding box?
  if(!is.null(box_dim)){
    rescale_factor <- min(box_dim / max_pts[c("x", "y")])
    sdf_pts <- t(t(sdf_pts * rescale_factor))
    bdf_pts <- t(t(bdf_pts * rescale_factor))  
  }
  
  #or rescale the unextended DNA strand to forced_h and forced_w
  if(!is.null(forced_box)){
    rescale_factors <- forced_box / max_pts[c("x", "y")]
    sdf_pts[,c("x1", "x2")] <- t(t(sdf_pts[,c("x1", "x2")] * rescale_factors[1]))
    sdf_pts[,c("y1", "y2")] <- t(t(sdf_pts[,c("y1", "y2")] * rescale_factors[2]))
    bdf_pts[,c("x0", "x1")] <- t(t(bdf_pts[,c("x0", "x1")] * rescale_factors[1]))
    bdf_pts[,c("y0", "y1")] <- t(t(bdf_pts[,c("y0", "y1")] * rescale_factors[2]))
  }
  
  #recenter these
  if(center_with_extension){
    center <- c(x = mean(range(sdf_pts[,c("x1", "x2")])), 
                y = mean(range(sdf_pts[,c("y1", "y2")])), 
                z = mean(range(sdf_pts[,c("z1", "z2")])))
  } else {
    center <- c(x = mean(range(sdf_pts[1:n_strand, c("x1", "x2")])), 
                y = mean(range(sdf_pts[1:n_strand, c("y1", "y2")])), 
                z = mean(range(sdf_pts[1:n_strand, c("z1", "z2")])))  
  }
  
  sdf_pts[,c("x1", "y1", "x2", "y2")] <- t(t(sdf_pts[,c("x1", "y1", "x2", "y2")]) - center[c("x", "y", "x", "y")] + 
                                             c(target_center, target_center))
  bdf_pts[,c("x0", "y0", "x1", "y1")] <- t(t(bdf_pts[,c("x0", "y0", "x1", "y1")]) - center[c("x", "y", "x", "y")] + 
                                             c(target_center, target_center))
  
  #alternatively, specify location using top-left corner
  if(!is.null(topleft_xy)){
    tl_xy <- c(x = min(sdf_pts[,c("x1", "x2")]), 
               y = max(sdf_pts[,c("y1", "y2")]))
    
    sdf_pts[,c("x1", "y1", "x2", "y2")] <- t(t(sdf_pts[,c("x1", "y1", "x2", "y2")]) - tl_xy[c("x", "y", "x", "y")] + 
                                               c(topleft_xy, topleft_xy))
    bdf_pts[,c("x0", "y0", "x1", "y1")] <- t(t(bdf_pts[,c("x0", "y0", "x1", "y1")]) - tl_xy[c("x", "y", "x", "y")] + 
                                               c(topleft_xy, topleft_xy))
  }
  
  #maybe we want the straight part of the DNA to be extended by a real amount?
  if(straight_extension_real_units){
    str_ext <- sdf_pts[strinds, paste0(c("x", "y"), extend_straight)]
    len_straight <- sqrt(sum((head(str_ext, 1) - tail(str_ext, 1))^2))
    len_straight_scale <- len_straight / extend_straight_by
    str_ext_diff <- apply(str_ext, 2, diff)
    new_str_ext <- apply(rbind(str_ext[1,], 
                               str_ext_diff / len_straight_scale), 
                         2, cumsum)
    sdf_pts[strinds, paste0(c("x", "y"), extend_straight)] <- new_str_ext
  }
  
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
    polylines(x = sdf$x1, y = sdf$y1, lwd = sdf$lwd1, col = col, complex = T, ...)
  }
  
  #strand 2
  for(i in seq_along(seen_sets_2)){
    seen_set <- seen_sets_2[[i]]
    sdf <- strand_df[seen_set,]
    polylines(sdf$x2, sdf$y2, lwd = sdf$lwd2, col = col, complex = T, ...)
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
    polylines(x = xbp, y = ybp, lwd = lwd, col = col, complex = T, ...)
  }
  
}



polylines <- function(x, y = NULL, lwd, col = 1, border = 1, complex = F, simple_via_quads = F,
                      draw_indiv = F, eps = 1E-9, xpd = NA, draw_overlap = T, ...){
  
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
              col = ifelse(length(col)==(length(x)-1), col[i-1], col), xpd = xpd, ...)
    }
    return()
  }
  
  #this can be a complex polygon, so we may want to make it simple
  poly_coords <- data.frame(x = c(poly_pts$x0, rev(poly_pts$x1), poly_pts$x0[1]),
                            y = c(poly_pts$y0, rev(poly_pts$y1), poly_pts$y0[1]))
  
  if(complex){
    polygon(poly_coords$x, poly_coords$y, 
            border = border, col = col, xpd = xpd, ...)
    
  } else {
    
    #by finding the union of the component quadrilaterals?
    if(simple_via_quads){
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
              border = border, col = col, xpd = xpd, ...)
      
    } else {
      
      #or just calling the simple function on all the coords together  
      complex_polygon <- sf::st_polygon(list(as.matrix(poly_coords)))
      simple_polygon <- sf::st_make_valid(complex_polygon)
      
      #plot
      polygon(poly_coords$x, poly_coords$y, 
              border = NA, col = col, xpd = xpd, ...)
      
      for(i in 1:length(simple_polygon)){
        poly_coords_simple <-  as.data.frame(as.matrix(simple_polygon[[i]]))
        colnames(poly_coords_simple) <- c("x", "y")
        polygon(poly_coords_simple$x, poly_coords_simple$y, 
                border = border, col = NA, xpd = xpd, ...)
      }
      
    }
    
    if(draw_overlap){
      
      # Convert the coords to a linestring
      linestring <- st_linestring(as.matrix(poly_coords))
       #and intersect with a non-overlapping polygon if it is complex
      if(!st_is_simple(linestring)){
        outer_poly <- sf::st_polygon(list(cbind(c(max(poly_coords$x) + c(1,2,3,1)), c(max(poly_coords$y) + c(1,4,4,1)))))
        linesegs <- lapply(st_difference(linestring, outer_poly), identity)
        nlinesegs <- length(linesegs)
        
        # for(i in 1:nlinesegs){
        #   lseg <- linesegs[[i]]
        #   lenseg <- nrow(lseg)
        #   lines(lseg, col = i, xpd = NA)
        #   text(linesegs[[i]][ceiling(lenseg / 2), 1], linesegs[[i]][ceiling(lenseg / 2), 2], labels = i, col = 1, xpd = NA)
        # }
        
        internal_linesegs <- linesegs[seq(2, nlinesegs, by = 2)]
        nverts <- length(internal_linesegs)
        
        # for(i in 1:nverts){
        #   lseg <- internal_linesegs[[i]]
        #   lenseg <- nrow(lseg)
        #   lines(lseg, col = i, xpd = NA)
        #   text(lseg[ceiling(lenseg / 2), 1], lseg[ceiling(lenseg / 2), 2], labels = i, col = 1, xpd = NA)
        # }
        
        starts <- do.call(rbind, lapply(internal_linesegs, head, 1))
        ends <- do.call(rbind, lapply(internal_linesegs, tail, 1))
        endstarts <- as.matrix(dist(rbind(starts, ends)))[(nverts+1):(2*nverts), 1:nverts] < 1E-9
        adjgraph <- igraph::graph_from_adjacency_matrix(endstarts, mode = "directed")
        V(adjgraph)$name <- V(adjgraph)
        verts <- V(adjgraph)
        paths <- lapply(verts, function(vert) all_simple_paths(adjgraph, from = vert)) #finds all paths in adjacency graph
        
        #finds paths that form cycles
        closed_paths <- lapply(setNames(1:nverts, 1:nverts), function(vi){
          vpaths <- paths[[vi]]
          npaths <- length(vpaths)
          vends <- sapply(vpaths, tail, 1)
          vpaths_continuations <- lapply(vends, function(vie){
            vepaths <- paths[[vie]]
            vepath_ends <- sapply(vepaths, tail, 1)
            vepaths[[which(vepath_ends == vi)]][-1]
          })
          full_paths <- lapply(1:npaths, function(i){
            as.numeric(c(vpaths[[i]], vpaths_continuations[[i]]))
          })
          
          return(unique(full_paths))
        })
        
        #finds the first occurrences of these cycles / closed paths
        #these correspond to the internally bounded polygons from polygon self-intersects
        sorted_cpaths <- lapply(closed_paths, function(cpaths){
          lapply(cpaths, function(cpath) sort(cpath[-1]))
        })
        first_cpath_inds <- which(!duplicated(sorted_cpaths))
        cpaths <- closed_paths[first_cpath_inds]
        ncpaths <- length(cpaths)
        
        #these lines correspond to earlier overlaps on top
        early_lines <- lapply(1:length(cpaths), function(cpi){
          seg_inds <- cpaths[[cpi]][[1]]
          trunc_seg_inds <- seg_inds[-length(seg_inds)]
          target_seg_inds <- trunc_seg_inds[seq(1, length(trunc_seg_inds), by = 2)]
          return(internal_linesegs[target_seg_inds])
        })
        
        #these lines correspond to later occurring overlaps on top
        late_lines <- lapply(1:length(cpaths), function(cpi){
          seg_inds <- cpaths[[cpi]][[1]]
          trunc_seg_inds <- seg_inds[-length(seg_inds)]
          target_seg_inds <- trunc_seg_inds[seq(2, length(trunc_seg_inds), by = 2)]
          return(internal_linesegs[target_seg_inds])
        })
        
        
        for(ipoly in 1:length(early_lines)){
          poly_i <- early_lines[[ipoly]]
          for(iseg in 1:length(poly_i)){
            seg_i <- poly_i[[iseg]]
            lines(seg_i, col = border)
          }
        }
        
      }
      
      
    }
    
  }
}

#### Sankey ribbon plotting ####
# Helpers for bundled Sankey-style ribbons.
#
# Thickness values are interpreted in the same visual units as polylines():
# horizontal-axis user units, corrected for the current plot aspect ratio.

xyrat_local <- function() {
  prop <- c(diff(par("usr")[1:2]), diff(par("usr")[3:4])) / par("pin")
  prop[1] / prop[2]
}

sankey_text_metric <- function(labels, cex, font, metric = c("width", "height")) {
  metric <- match.arg(metric)
  labels <- as.character(labels)
  cex <- rep(cex, length.out = length(labels))
  font <- rep(font, length.out = length(labels))
  vapply(seq_along(labels), function(i) {
    if(identical(metric, "width")) {
      graphics::strwidth(labels[i], cex = cex[i], font = font[i])
    } else {
      graphics::strheight(labels[i], cex = cex[i], font = font[i])
    }
  }, numeric(1))
}

as_sankey_loc_table <- function(loc, default_node_width = 0, node_label_pad = 0,
                                node_label_pad_frac = 0.04) {
  if(!is.numeric(default_node_width) || length(default_node_width) != 1 ||
     !is.finite(default_node_width) || default_node_width < 0) {
    stop("`default_node_width` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.numeric(node_label_pad) || length(node_label_pad) != 1 ||
     !is.finite(node_label_pad) || node_label_pad < 0) {
    stop("`node_label_pad` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.numeric(node_label_pad_frac) || length(node_label_pad_frac) != 1 ||
     !is.finite(node_label_pad_frac) || node_label_pad_frac < 0) {
    stop("`node_label_pad_frac` must be a single non-negative finite number.", call. = FALSE)
  }

  if(is.data.frame(loc)) {
    loc_names <- names(loc)
    core <- if(all(c("set", "node", "x", "y") %in% loc_names)) {
      loc[c("set", "node", "x", "y")]
    } else if(ncol(loc) >= 4) {
      loc[seq_len(4)]
    } else {
      stop("`loc` must have at least four columns.", call. = FALSE)
    }
    out <- data.frame(
      set = as.numeric(core[[1]]),
      node = as.numeric(core[[2]]),
      x = as.numeric(core[[3]]),
      y = as.numeric(core[[4]])
    )

    label_col <- if("label" %in% loc_names) "label" else if("name" %in% loc_names) "name" else NA_character_
    labels <- if(is.na(label_col)) rep(NA_character_, nrow(out)) else as.character(loc[[label_col]])
    # Labels are per row: blank or NA labels leave a node unlabeled. A has_label column comes
    # from re-parsing this function's own output and is kept as is.
    out$has_label <- if("has_label" %in% loc_names) {
      as.logical(loc[["has_label"]])
    } else {
      !is.na(labels) & nzchar(trimws(labels))
    }
    out$label <- ifelse(out$has_label, labels, as.character(out$node))
    out$cex <- if("cex" %in% loc_names) as.numeric(loc[["cex"]]) else rep(1, nrow(out))
    out$font <- if("font" %in% loc_names) as.numeric(loc[["font"]]) else rep(1, nrow(out))
    out$node_label_width <- if(!is.na(label_col)) {
      width <- rep(0, nrow(out))
      width[out$has_label] <- sankey_text_metric(
        out$label[out$has_label], cex = out$cex[out$has_label], font = out$font[out$has_label], metric = "width"
      )
      width
    } else if("node_label_width" %in% loc_names) {
      as.numeric(loc[["node_label_width"]])
    } else {
      rep(0, nrow(out))
    }

    if("width" %in% loc_names) {
      out$node_width <- as.numeric(loc[["width"]])
      out$width_explicit <- TRUE
    } else if("node_width" %in% loc_names) {
      out$node_width <- as.numeric(loc[["node_width"]])
      out$width_explicit <- if("width_explicit" %in% loc_names) {
        as.logical(loc[["width_explicit"]])
      } else {
        TRUE
      }
    } else if(!is.na(label_col)) {
      text_width <- out$node_label_width
      median_width <- stats::median(text_width[out$has_label & text_width > 0], na.rm = TRUE)
      if(!is.finite(median_width)) {
        median_width <- 0
      }
      out$node_width <- ifelse(
        out$has_label,
        text_width + 2 * node_label_pad + 2 * node_label_pad_frac * median_width,
        default_node_width
      )
      out$width_explicit <- FALSE
    } else {
      out$node_width <- rep(default_node_width, nrow(out))
      out$width_explicit <- FALSE
    }
    out$node_body_height <- if("height" %in% loc_names) {
      as.numeric(loc[["height"]])
    } else if("node_body_height" %in% loc_names) {
      as.numeric(loc[["node_body_height"]])
    } else {
      rep(NA_real_, nrow(out))
    }
    out$node_label_body_height <- if("node_label_body_height" %in% loc_names) {
      as.numeric(loc[["node_label_body_height"]])
    } else {
      rep(NA_real_, nrow(out))
    }
  } else {
    if(!is.matrix(loc) || !is.numeric(loc) || ncol(loc) < 4) {
      stop("`loc` must be a numeric matrix with at least four columns or a data frame.", call. = FALSE)
    }
    out <- data.frame(
      set = loc[, 1],
      node = loc[, 2],
      x = loc[, 3],
      y = loc[, 4],
      label = as.character(loc[, 2]),
      has_label = rep(FALSE, nrow(loc)),
      cex = rep(1, nrow(loc)),
      font = rep(1, nrow(loc)),
      node_label_width = rep(0, nrow(loc)),
      node_width = if(ncol(loc) >= 5) loc[, 5] else rep(default_node_width, nrow(loc)),
      width_explicit = ncol(loc) >= 5,
      node_body_height = if(ncol(loc) >= 6) loc[, 6] else rep(NA_real_, nrow(loc)),
      node_label_body_height = rep(NA_real_, nrow(loc))
    )
  }

  if(any(!is.finite(out$set)) || any(!is.finite(out$node)) ||
     any(!is.finite(out$x)) || any(!is.finite(out$y))) {
    stop("The first four `loc` columns must contain finite numeric values.", call. = FALSE)
  }
  if(any(!is.finite(out$node_width)) || any(out$node_width < 0)) {
    stop("Node widths in `loc` must be finite, non-negative values.", call. = FALSE)
  }
  if(any(!is.na(out$node_body_height) & (!is.finite(out$node_body_height) | out$node_body_height < 0))) {
    stop("Node body heights in `loc` must be finite, non-negative values when supplied.", call. = FALSE)
  }
  if(any(!is.na(out$node_label_body_height) & (!is.finite(out$node_label_body_height) | out$node_label_body_height < 0))) {
    stop("Node label body heights in `loc` must be finite, non-negative values when supplied.", call. = FALSE)
  }
  if(any(!is.finite(out$cex)) || any(out$cex <= 0)) {
    stop("`loc` cex values must be positive finite numbers.", call. = FALSE)
  }
  if(any(!is.finite(out$node_label_width)) || any(out$node_label_width < 0)) {
    stop("Node label widths in `loc` must be finite, non-negative values.", call. = FALSE)
  }
  if(any(!is.finite(out$font)) || any(out$font < 1)) {
    stop("`loc` font values must be positive finite numbers.", call. = FALSE)
  }
  out
}

validate_sankey_inputs <- function(thic, loc, from_set = 1, to_set = 2,
                                   default_node_width = 0, node_label_pad = 0,
                                   node_label_pad_frac = 0.04) {
  if(!is.matrix(thic) || !is.numeric(thic)) {
    stop("`thic` must be a numeric matrix.", call. = FALSE)
  }
  if(any(!is.finite(thic)) || any(thic < 0)) {
    stop("`thic` must contain finite, non-negative values.", call. = FALSE)
  }
  loc <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)

  m <- nrow(thic)
  n <- ncol(thic)
  from_loc <- loc[loc$set == from_set, , drop = FALSE]
  to_loc <- loc[loc$set == to_set, , drop = FALSE]

  if(nrow(from_loc) != m) {
    stop("`loc` must contain exactly one row for each `thic` row in `from_set`.", call. = FALSE)
  }
  if(nrow(to_loc) != n) {
    stop("`loc` must contain exactly one row for each `thic` column in `to_set`.", call. = FALSE)
  }
  if(any(duplicated(from_loc$node)) || any(duplicated(to_loc$node))) {
    stop("Node indices in each node set must be unique.", call. = FALSE)
  }
  if(!setequal(from_loc$node, seq_len(m))) {
    stop("From-node indices in `loc` must be 1:nrow(thic).", call. = FALSE)
  }
  if(!setequal(to_loc$node, seq_len(n))) {
    stop("To-node indices in `loc` must be 1:ncol(thic).", call. = FALSE)
  }

  invisible(TRUE)
}

ordered_node_table <- function(loc, node_set, node_count,
                               default_node_width = 0, node_label_pad = 0,
                               node_label_pad_frac = 0.04) {
  loc <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)
  out <- loc[loc$set == node_set, , drop = FALSE]
  out <- out[match(seq_len(node_count), out$node), , drop = FALSE]
  rownames(out) <- NULL
  out
}

node_face_x <- function(start_nodes, end_nodes, start_ind, end_ind) {
  dx <- end_nodes$x[end_ind] - start_nodes$x[start_ind]
  direction <- sign(dx)
  start_x <- start_nodes$x[start_ind] + direction * start_nodes$node_width[start_ind] / 2
  end_x <- end_nodes$x[end_ind] - direction * end_nodes$node_width[end_ind] / 2
  if(any(direction == 0 | direction * (end_x - start_x) <= 0)) {
    stop("Node widths leave no positive horizontal distance between at least one connected node pair.", call. = FALSE)
  }
  list(start = start_x, end = end_x)
}

stack_offsets <- function(widths, order) {
  offsets <- rep(NA_real_, length(widths))
  if(length(order) == 0) {
    return(offsets)
  }

  ordered_widths <- widths[order]
  total_width <- sum(ordered_widths)
  offsets[order] <- cumsum(ordered_widths) - ordered_widths / 2 - total_width / 2
  offsets
}

sankey_source_order_choices <- c(
  "target_y", "target_y_desc", "target_index",
  "target_projected", "target_projected_desc",
  "target_bezier", "target_bezier_desc"
)
sankey_target_order_choices <- c(
  "source_y", "source_y_desc", "source_index",
  "source_projected", "source_projected_desc",
  "source_bezier", "source_bezier_desc"
)

# Opposite-side face x closest to the shared node, for either flow direction.
nearest_face_x <- function(flows, inds, side = c("start", "end")) {
  side <- match.arg(side)
  nearest <- which.min(abs(flows$end_x[inds] - flows$start_x[inds]))
  flows[[paste0(side, "_x")]][inds][nearest]
}

linear_flow_y_at_x <- function(flows, inds, x) {
  start_y <- flows$start_y[inds] + flows$start_offset[inds]
  end_y <- flows$end_y[inds] + flows$end_offset[inds]
  denom <- flows$end_x[inds] - flows$start_x[inds]
  t <- ifelse(abs(denom) <= 1e-12, 0, (x - flows$start_x[inds]) / denom)
  t <- pmin(pmax(t, 0), 1)
  start_y * (1 - t) + end_y * t
}

apply_projected_stack_order <- function(flows, source_order, target_order) {
  if(source_order %in% c("target_projected", "target_projected_desc")) {
    for(i in sort(unique(flows$from))) {
      inds <- which(flows$from == i)
      if(length(inds) < 2) {
        next
      }
      ref_x <- nearest_face_x(flows, inds, "end")
      projected_y <- linear_flow_y_at_x(flows, inds, ref_x)
      ord <- inds[order(projected_y, flows$to[inds])]
      if(identical(source_order, "target_projected_desc")) {
        ord <- rev(ord)
      }
      flows$start_offset[inds] <- stack_offsets(flows$width[inds], match(ord, inds))
    }
  }

  if(target_order %in% c("source_projected", "source_projected_desc")) {
    for(j in sort(unique(flows$to))) {
      inds <- which(flows$to == j)
      if(length(inds) < 2) {
        next
      }
      ref_x <- nearest_face_x(flows, inds, "start")
      projected_y <- linear_flow_y_at_x(flows, inds, ref_x)
      ord <- inds[order(projected_y, flows$from[inds])]
      if(identical(target_order, "source_projected_desc")) {
        ord <- rev(ord)
      }
      flows$end_offset[inds] <- stack_offsets(flows$width[inds], match(ord, inds))
    }
  }

  flows
}

bezier_order_requested <- function(source_order, target_order) {
  source_order %in% c("target_bezier", "target_bezier_desc") ||
    target_order %in% c("source_bezier", "source_bezier_desc")
}

apply_bezier_stack_order <- function(sankey, geometry, source_order, target_order) {
  flows <- sankey$flows

  if(source_order %in% c("target_bezier", "target_bezier_desc")) {
    for(i in sort(unique(flows$from))) {
      inds <- which(flows$from == i)
      if(length(inds) < 2) {
        next
      }
      ref_x <- nearest_face_x(flows, inds, "end")
      sampled_y <- vapply(inds, function(k) {
        point_at_x(geometry$centerlines[[k]], ref_x)[1, "y"]
      }, numeric(1))
      ord <- inds[order(sampled_y, flows$to[inds])]
      if(identical(source_order, "target_bezier_desc")) {
        ord <- rev(ord)
      }
      flows$start_offset[inds] <- stack_offsets(flows$width[inds], match(ord, inds))
    }
  }

  if(target_order %in% c("source_bezier", "source_bezier_desc")) {
    for(j in sort(unique(flows$to))) {
      inds <- which(flows$to == j)
      if(length(inds) < 2) {
        next
      }
      ref_x <- nearest_face_x(flows, inds, "start")
      sampled_y <- vapply(inds, function(k) {
        point_at_x(geometry$centerlines[[k]], ref_x)[1, "y"]
      }, numeric(1))
      ord <- inds[order(sampled_y, flows$from[inds])]
      if(identical(target_order, "source_bezier_desc")) {
        ord <- rev(ord)
      }
      flows$end_offset[inds] <- stack_offsets(flows$width[inds], match(ord, inds))
    }
  }

  sankey$flows <- flows
  sankey
}

# stack_order: NULL or a data frame with columns side ("start" = stack at the
# source node, "end" = stack at the target node), from, to and rank (1 = bottom),
# plus stage (1-based) for list-valued thic. In graph mode from/to are loc rows.
validate_stack_order <- function(stack_order) {
  if(is.null(stack_order)) {
    return(NULL)
  }
  if(!is.data.frame(stack_order) || !all(c("side", "from", "to", "rank") %in% names(stack_order))) {
    stop("`stack_order` must be NULL or a data frame with columns side, from, to and rank.", call. = FALSE)
  }
  stack_order$side <- as.character(stack_order$side)
  if(any(!stack_order$side %in% c("start", "end"))) {
    stop("`stack_order$side` must contain only \"start\" or \"end\".", call. = FALSE)
  }
  numeric_cols <- intersect(c("stage", "from", "to", "rank"), names(stack_order))
  if(any(!vapply(stack_order[numeric_cols], function(x) is.numeric(x) && all(is.finite(x)), logical(1)))) {
    stop("`stack_order` stage, from, to and rank columns must be finite numbers.", call. = FALSE)
  }
  key_cols <- intersect(c("side", "stage", "from", "to"), names(stack_order))
  if(any(duplicated(stack_order[key_cols]))) {
    stop("`stack_order` lists a strand more than once on the same side.", call. = FALSE)
  }
  stack_order
}

stage_stack_order <- function(stack_order, stage, n_stages) {
  if(is.null(stack_order)) {
    return(NULL)
  }
  if(!"stage" %in% names(stack_order)) {
    stop("For list-valued `thic`, `stack_order` needs a stage column.", call. = FALSE)
  }
  if(any(!stack_order$stage %in% seq_len(n_stages))) {
    stop(sprintf("`stack_order$stage` must be between 1 and %s.", n_stages), call. = FALSE)
  }
  stack_order[stack_order$stage == stage, setdiff(names(stack_order), "stage"), drop = FALSE]
}

# Listed strands stack bottom-to-top by rank at their node; unlisted strands at
# that node keep their computed relative order above them.
apply_manual_stack_order <- function(flows, stack_order) {
  if(is.null(stack_order) || nrow(stack_order) == 0) {
    return(flows)
  }
  flow_keys <- paste(flows$from, flows$to)
  order_keys <- paste(stack_order$from, stack_order$to)
  if(any(!order_keys %in% flow_keys)) {
    stop(sprintf(
      "`stack_order` lists strands with no positive flow: %s.",
      paste(unique(order_keys[!order_keys %in% flow_keys]), collapse = ", ")
    ), call. = FALSE)
  }
  for(side in c("start", "end")) {
    rows <- stack_order[stack_order$side == side, , drop = FALSE]
    node_col <- if(identical(side, "start")) "from" else "to"
    offset_col <- paste0(side, "_offset")
    for(node in unique(rows[[node_col]])) {
      inds <- which(flows[[node_col]] == node)
      listed <- rows[rows[[node_col]] == node, , drop = FALSE]
      current <- inds[order(flows[[offset_col]][inds])]
      rank <- listed$rank[match(flow_keys[current], paste(listed$from, listed$to))]
      score <- ifelse(is.na(rank), max(listed$rank) + seq_along(current), rank)
      flows[[offset_col]][inds] <- stack_offsets(flows$width[inds], match(current[order(score)], inds))
    }
  }
  flows
}

build_sankey_flows <- function(thic, loc, from_set = 1, to_set = 2,
                               default_node_width = 0, node_label_pad = 0,
                               node_label_pad_frac = 0.04,
                               source_order = sankey_source_order_choices,
                               target_order = sankey_target_order_choices,
                               stack_order = NULL) {
  source_order <- validate_single_choice(source_order, sankey_source_order_choices, "`source_order`")
  target_order <- validate_single_choice(target_order, sankey_target_order_choices, "`target_order`")
  validate_sankey_inputs(
    thic,
    loc,
    from_set = from_set,
    to_set = to_set,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac
  )

  m <- nrow(thic)
  n <- ncol(thic)
  from_nodes <- ordered_node_table(loc, from_set, m, default_node_width, node_label_pad, node_label_pad_frac)
  to_nodes <- ordered_node_table(loc, to_set, n, default_node_width, node_label_pad, node_label_pad_frac)

  positive <- which(thic > 0, arr.ind = TRUE)
  if(nrow(positive) == 0) {
    stop("`thic` must contain at least one positive value.", call. = FALSE)
  }

  flows <- data.frame(
    from = positive[, "row"],
    to = positive[, "col"],
    width = thic[positive],
    start_offset = NA_real_,
    end_offset = NA_real_
  )

  for(i in seq_len(m)) {
    inds <- which(flows$from == i)
    order <- switch(
      source_order,
      target_y = inds[order(to_nodes$y[flows$to[inds]], flows$to[inds])],
      target_y_desc = inds[order(-to_nodes$y[flows$to[inds]], flows$to[inds])],
      target_index = inds[order(flows$to[inds])],
      target_projected = inds[order(to_nodes$y[flows$to[inds]], flows$to[inds])],
      target_projected_desc = inds[order(-to_nodes$y[flows$to[inds]], flows$to[inds])],
      target_bezier = inds[order(to_nodes$y[flows$to[inds]], flows$to[inds])],
      target_bezier_desc = inds[order(-to_nodes$y[flows$to[inds]], flows$to[inds])]
    )
    flows$start_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }

  for(j in seq_len(n)) {
    inds <- which(flows$to == j)
    order <- switch(
      target_order,
      source_y = inds[order(from_nodes$y[flows$from[inds]], flows$from[inds])],
      source_y_desc = inds[order(-from_nodes$y[flows$from[inds]], flows$from[inds])],
      source_index = inds[order(flows$from[inds])],
      source_projected = inds[order(from_nodes$y[flows$from[inds]], flows$from[inds])],
      source_projected_desc = inds[order(-from_nodes$y[flows$from[inds]], flows$from[inds])],
      source_bezier = inds[order(from_nodes$y[flows$from[inds]], flows$from[inds])],
      source_bezier_desc = inds[order(-from_nodes$y[flows$from[inds]], flows$from[inds])]
    )
    flows$end_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }

  faces <- node_face_x(from_nodes, to_nodes, flows$from, flows$to)
  flows$start_x <- faces$start
  flows$start_node_x <- from_nodes$x[flows$from]
  flows$start_y <- from_nodes$y[flows$from]
  flows$end_x <- faces$end
  flows$end_node_x <- to_nodes$x[flows$to]
  flows$end_y <- to_nodes$y[flows$to]
  flows <- apply_projected_stack_order(flows, source_order, target_order)
  flows <- apply_manual_stack_order(flows, stack_order)

  list(flows = flows, from_nodes = from_nodes, to_nodes = to_nodes)
}

validate_sankey_thic_list <- function(thic) {
  if(!is.list(thic) || length(thic) == 0) {
    stop("List-valued `thic` must contain at least one matrix.", call. = FALSE)
  }
  for(i in seq_along(thic)) {
    if(!is.matrix(thic[[i]]) || !is.numeric(thic[[i]])) {
      stop("Every element of list-valued `thic` must be a numeric matrix.", call. = FALSE)
    }
    if(any(!is.finite(thic[[i]])) || any(thic[[i]] < 0)) {
      stop("All matrices in list-valued `thic` must contain finite, non-negative values.", call. = FALSE)
    }
    if(i > 1 && nrow(thic[[i]]) != ncol(thic[[i - 1]])) {
      stop("For list-valued `thic`, ncol(thic[[i - 1]]) must equal nrow(thic[[i]]).", call. = FALSE)
    }
  }
  invisible(TRUE)
}

sankey_node_widths <- function(thic) {
  if(is.list(thic)) {
    validate_sankey_thic_list(thic)
    node_counts <- c(nrow(thic[[1]]), vapply(thic, ncol, integer(1)))
    out <- vector("list", length(node_counts))
    for(node_set in seq_along(node_counts)) {
      incoming <- if(node_set == 1) {
        rep(0, node_counts[node_set])
      } else {
        colSums(thic[[node_set - 1]])
      }
      outgoing <- if(node_set > length(thic)) {
        rep(0, node_counts[node_set])
      } else {
        rowSums(thic[[node_set]])
      }
      out[[node_set]] <- pmax(incoming, outgoing)
    }
    return(out)
  }

  list(rowSums(thic), colSums(thic))
}

sankey_adjust_one_node_set <- function(y, widths, xyr, min_node_gap = 0,
                                       allow_expand = TRUE) {
  if(length(y) <= 1) {
    return(y)
  }
  if(!is.numeric(min_node_gap) || length(min_node_gap) != 1 ||
     !is.finite(min_node_gap) || min_node_gap < 0) {
    stop("`min_node_gap` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!isTRUE(allow_expand) && !identical(allow_expand, FALSE)) {
    stop("`adjust_loc_expand` must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.finite(xyr) || xyr <= 0) {
    stop("`xyr` must be a positive finite number.", call. = FALSE)
  }

  widths <- as.numeric(widths)
  widths[!is.finite(widths) | widths < 0] <- 0
  ord <- order(y, seq_along(y))
  y_ord <- y[ord]
  width_ord <- widths[ord] / xyr
  gap <- min_node_gap / xyr
  required_spacing <- (width_ord[-length(width_ord)] + width_ord[-1]) / 2 + gap
  current_spacing <- diff(y_ord)
  if(all(current_spacing >= required_spacing - sqrt(.Machine$double.eps))) {
    return(y)
  }
  cumulative_spacing <- c(0, cumsum(required_spacing))
  total_spacing <- cumulative_spacing[length(cumulative_spacing)]
  old_min <- y_ord[1]
  old_max <- y_ord[length(y_ord)]

  if(!isTRUE(allow_expand) && total_spacing > old_max - old_min) {
    # Requested spacing cannot fit in range: pack at proportionally compressed spacing.
    y_adj <- old_min + cumulative_spacing * (old_max - old_min) / total_spacing
  } else {
    fitted_residual <- stats::isoreg(seq_along(y_ord), y_ord - cumulative_spacing)$yf
    if(!isTRUE(allow_expand)) {
      # Bounds are constant across the monotone residuals, so clipping the
      # isotonic fit gives the bounded least-squares solution.
      fitted_residual <- pmin(pmax(fitted_residual, old_min), old_max - total_spacing)
    }
    y_adj <- fitted_residual + cumulative_spacing
  }

  out <- y
  out[ord] <- y_adj
  out
}

sankey_adjust_loc <- function(thic, loc, from_set = 1, to_set = 2,
                              xyr = xyrat_local(), min_node_gap = 0,
                              allow_expand = TRUE,
                              default_node_width = 0, node_label_pad = 0,
                              node_label_pad_frac = 0.04) {
  out_table <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)
  out <- loc
  if(is.list(thic)) {
    widths <- sankey_node_widths(thic)
    node_sets <- seq_along(widths)
  } else {
    validate_sankey_inputs(
      thic,
      loc,
      from_set = from_set,
      to_set = to_set,
      default_node_width = default_node_width,
      node_label_pad = node_label_pad,
      node_label_pad_frac = node_label_pad_frac
    )
    widths <- sankey_node_widths(thic)
    node_sets <- c(from_set, to_set)
  }

  for(i in seq_along(node_sets)) {
    node_set <- node_sets[i]
    rows <- which(out_table$set == node_set)
    if(!length(rows)) {
      next
    }
    node_order <- match(seq_along(widths[[i]]), out_table$node[rows])
    if(anyNA(node_order)) {
      stop("`loc` must contain one row for every node before location adjustment.", call. = FALSE)
    }
    rows <- rows[node_order]
    adjusted_y <- sankey_adjust_one_node_set(
      out_table$y[rows],
      widths[[i]],
      xyr = xyr,
      min_node_gap = min_node_gap,
      allow_expand = allow_expand
    )
    out_table$y[rows] <- adjusted_y
    if(is.data.frame(out)) {
      y_col <- if("y" %in% names(out)) "y" else names(out)[4]
      out[[y_col]][rows] <- adjusted_y
    } else {
      out[rows, 4] <- adjusted_y
    }
  }

  out
}

sankey_loc_flow_heights <- function(thic, loc, from_set = 1, to_set = 2,
                                    graph = FALSE,
                                    default_node_width = 0, node_label_pad = 0,
                                    node_label_pad_frac = 0.04) {
  loc_table <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)
  heights <- rep(0, nrow(loc_table))

  if(isTRUE(graph)) {
    incoming <- colSums(thic)
    outgoing <- rowSums(thic)
    heights <- pmax(incoming, outgoing)
    return(heights)
  }

  if(is.list(thic)) {
    widths <- sankey_node_widths(thic)
    for(node_set in seq_along(widths)) {
      rows <- which(loc_table$set == node_set)
      if(!length(rows)) next
      node_order <- match(seq_along(widths[[node_set]]), loc_table$node[rows])
      if(anyNA(node_order)) {
        stop("`loc` must contain one row for every node before node-height adjustment.", call. = FALSE)
      }
      heights[rows[node_order]] <- widths[[node_set]]
    }
    return(heights)
  }

  from_rows <- which(loc_table$set == from_set)
  to_rows <- which(loc_table$set == to_set)
  heights[from_rows[match(seq_len(nrow(thic)), loc_table$node[from_rows])]] <- rowSums(thic)
  heights[to_rows[match(seq_len(ncol(thic)), loc_table$node[to_rows])]] <- colSums(thic)
  heights
}

adjust_sankey_node_heights <- function(thic, loc, mode = c("none", "label", "node"),
                                       from_set = 1, to_set = 2, graph = FALSE,
                                       xyr = xyrat_local(),
                                       node_label_vpad = 0,
                                       node_label_vpad_frac = 0.25,
                                       default_node_width = 0,
                                       node_label_pad = 0,
                                       node_label_pad_frac = 0.04) {
  mode <- validate_single_choice(mode, c("none", "label", "node"), "`adjust_node_height`")
  if(identical(mode, "none")) {
    return(loc)
  }
  if(!is.numeric(node_label_vpad) || length(node_label_vpad) != 1 ||
     !is.finite(node_label_vpad) || node_label_vpad < 0) {
    stop("`node_label_vpad` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.numeric(node_label_vpad_frac) || length(node_label_vpad_frac) != 1 ||
     !is.finite(node_label_vpad_frac) || node_label_vpad_frac < 0) {
    stop("`node_label_vpad_frac` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.finite(xyr) || xyr <= 0) {
    stop("`xyr` must be a positive finite number.", call. = FALSE)
  }

  loc_table <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)
  label_rows <- which(loc_table$has_label)
  if(length(label_rows) == 0) {
    return(loc)
  }
  flow_heights <- sankey_loc_flow_heights(
    thic,
    loc_table,
    from_set = from_set,
    to_set = to_set,
    graph = graph,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac
  ) / xyr

  label_heights <- sankey_text_metric(loc_table$label, cex = loc_table$cex, font = loc_table$font, metric = "height")
  max_label_height <- max(label_heights[label_rows], na.rm = TRUE)
  if(!is.finite(max_label_height) || max_label_height <= 0) {
    return(loc_table)
  }
  buffer <- node_label_vpad + node_label_vpad_frac * max_label_height

  if(identical(mode, "label")) {
    available <- pmax(flow_heights - 2 * buffer, flow_heights * 0.05)
    scale <- rep(1, nrow(loc_table))
    can_scale <- label_heights > 0 & loc_table$has_label
    scale[can_scale] <- pmin(1, available[can_scale] / label_heights[can_scale])
    loc_table$cex <- loc_table$cex * pmax(scale, 0.05)

    recompute <- loc_table$has_label & !loc_table$width_explicit
    if(any(recompute)) {
      text_width <- sankey_text_metric(loc_table$label, cex = loc_table$cex, font = loc_table$font, metric = "width")
      text_width[!loc_table$has_label] <- 0
      loc_table$node_label_width <- text_width
      median_width <- stats::median(text_width[text_width > 0], na.rm = TRUE)
      if(!is.finite(median_width)) {
        median_width <- 0
      }
      loc_table$node_width[recompute] <- text_width[recompute] +
        2 * node_label_pad + 2 * node_label_pad_frac * median_width
    }
  } else if(identical(mode, "node")) {
    existing_heights <- loc_table$node_body_height
    existing_heights[!is.finite(existing_heights)] <- 0
    base_heights <- pmax(flow_heights, existing_heights)
    label_body_height <- label_heights[label_rows] + 2 * buffer
    loc_table$node_label_body_height[label_rows] <- label_body_height
    loc_table$node_body_height[label_rows] <- pmax(base_heights[label_rows], label_body_height)
  }

  loc_table
}

all_node_table <- function(loc, node_counts, default_node_width = 0, node_label_pad = 0,
                           node_label_pad_frac = 0.04) {
  out <- do.call(rbind, lapply(seq_along(node_counts), function(node_set) {
    ordered_node_table(loc, node_set, node_counts[node_set], default_node_width, node_label_pad, node_label_pad_frac)
  }))
  rownames(out) <- NULL
  out
}

combine_sankey_segments <- function(segments, loc, node_counts, merge_length, xyr,
                                    default_node_width = 0, node_label_pad = 0,
                                    node_label_pad_frac = 0.04) {
  flows <- do.call(rbind, lapply(seq_along(segments), function(i) {
    out <- segments[[i]]$flows
    out$stage <- i
    out$from_set <- i
    out$to_set <- i + 1
    out
  }))
  rownames(flows) <- NULL

  out <- list(
    segments = segments,
    flows = flows,
    nodes = all_node_table(loc, node_counts, default_node_width, node_label_pad, node_label_pad_frac),
    loc = loc,
    node_counts = node_counts,
    merge_length = merge_length,
    xyr = xyr
  )
  class(out) <- c("sankey_multi", "list")
  out
}

apply_visual_normal_offset <- function(x, y, offset, tangent, xyr = xyrat_local()) {
  tangent_visual <- c(tangent[1], tangent[2] * xyr)
  tangent_len <- sqrt(sum(tangent_visual^2))
  if(!is.finite(tangent_len) || tangent_len <= 0) {
    stop("Cannot offset a point with a zero-length tangent.", call. = FALSE)
  }

  normal_visual <- c(-tangent_visual[2], tangent_visual[1]) / tangent_len
  c(x + offset * normal_visual[1], y + offset * normal_visual[2] / xyr)
}

cubic_bezier <- function(p0, p1, p2, p3, n = 100) {
  if(n < 2) {
    stop("`n` must be at least 2.", call. = FALSE)
  }
  t <- seq(0, 1, length.out = n)
  omt <- 1 - t
  cbind(
    x = omt^3 * p0[1] + 3 * omt^2 * t * p1[1] + 3 * omt * t^2 * p2[1] + t^3 * p3[1],
    y = omt^3 * p0[2] + 3 * omt^2 * t * p1[2] + 3 * omt * t^2 * p2[2] + t^3 * p3[2]
  )
}

visual_path_length <- function(coords, xyr = xyrat_local()) {
  if(nrow(coords) < 2) {
    return(0)
  }
  diffs <- coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE]
  sum(sqrt(diffs[, "x"]^2 + (diffs[, "y"] * xyr)^2))
}

centerline_edges <- function(x, y, width, xyr = xyrat_local(), eps = 1e-10,
                             endpoint_normals = c("path", "vertical")) {
  endpoint_normals <- match.arg(endpoint_normals)
  if(length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  if(length(x) < 2) {
    stop("A ribbon centerline must contain at least two points.", call. = FALSE)
  }
  if(length(width) == 1) {
    width <- rep(width, length(x))
  }
  if(length(width) != length(x)) {
    stop("`width` must have length 1 or the same length as `x`.", call. = FALSE)
  }
  if(any(!is.finite(width)) || any(width <= 0)) {
    stop("`width` must contain positive finite values.", call. = FALSE)
  }

  n <- length(x)
  if(n == 2) {
    dx <- rep(x[2] - x[1], 2)
    dy <- rep(y[2] - y[1], 2)
  } else {
    dx <- c(x[2] - x[1], x[3:n] - x[1:(n - 2)], x[n] - x[n - 1])
    dy <- c(y[2] - y[1], y[3:n] - y[1:(n - 2)], y[n] - y[n - 1])
  }
  if(identical(endpoint_normals, "vertical")) {
    direction <- sign(x[n] - x[1])
    if(direction == 0) {
      direction <- sign(dx[n])
    }
    if(direction == 0) {
      stop("Vertical endpoint normals require different start/end x positions.", call. = FALSE)
    }
    dx[c(1, n)] <- direction
    dy[c(1, n)] <- 0
  }
  tangent_visual <- cbind(dx, dy * xyr)
  tangent_len <- sqrt(rowSums(tangent_visual^2))
  if(any(tangent_len <= eps)) {
    stop("Centerline contains repeated or nearly repeated points.", call. = FALSE)
  }

  normal_visual <- cbind(-tangent_visual[, 2], tangent_visual[, 1]) / tangent_len
  normal_data <- cbind(normal_visual[, 1], normal_visual[, 2] / xyr)
  left <- cbind(x, y) + normal_data * (width / 2)
  right <- cbind(x, y) - normal_data * (width / 2)
  colnames(left) <- colnames(right) <- c("x", "y")

  list(left = left, right = right)
}

centerline_to_ribbon <- function(x, y, width, xyr = xyrat_local(), eps = 1e-10,
                                 endpoint_normals = c("path", "vertical")) {
  endpoint_normals <- match.arg(endpoint_normals)
  edges <- centerline_edges(
    x, y, width, xyr = xyr, eps = eps, endpoint_normals = endpoint_normals
  )
  left <- edges$left
  right <- edges$right
  n <- length(x)
  coords <- rbind(left, right[n:1, , drop = FALSE], left[1, ])
  colnames(coords) <- c("x", "y")
  coords
}

# Edges are stored at build time because boundary-routed ribbons are not
# normal-offset expansions of their centerlines.
sankey_centerline_edges <- function(sankey, flow_ind) {
  if(is.null(sankey$edges)) {
    stop("Sankey object has no stored ribbon edges; rebuild it with sankey_ribbons().", call. = FALSE)
  }
  sankey$edges[[flow_ind]]
}

edges_to_ribbon <- function(edges) {
  n <- nrow(edges$left)
  coords <- rbind(edges$left, edges$right[n:1, , drop = FALSE], edges$left[1, ])
  colnames(coords) <- c("x", "y")
  coords
}

validate_single_choice <- function(x, choices, arg) {
  if(!is.character(x)) {
    stop(sprintf("%s must be one of: %s.", arg, paste(choices, collapse = ", ")), call. = FALSE)
  }
  match.arg(x, choices)
}

sankey_stage_param <- function(x, stage, choices = NULL, arg = "`arg`") {
  out <- if(is.list(x)) {
    if(is.null(x[[stage]])) {
      stop(sprintf("%s must provide one value per stage.", arg), call. = FALSE)
    }
    x[[stage]]
  } else {
    x
  }
  if(!is.null(choices)) {
    out <- validate_single_choice(out, choices, arg)
  }
  out
}

boundary_transport_geometry <- function(sankey, n, xyr,
                                        boundary_group = c("target", "source", "stage"),
                                        boundary_compensation = c("none", "full", "gap_budget"),
                                        boundary_ease = 1,
                                        x_tol = 1e-10,
                                        gap_tol = 1e-10) {
  boundary_group <- validate_single_choice(
    boundary_group,
    c("target", "source", "stage"),
    "`boundary_group`"
  )
  boundary_compensation <- validate_single_choice(
    boundary_compensation,
    c("none", "full", "gap_budget"),
    "`boundary_compensation`"
  )
  if(!is.numeric(boundary_ease) || length(boundary_ease) != 1 ||
     !is.finite(boundary_ease) || boundary_ease <= 0) {
    stop("`boundary_ease` must be a single positive finite number.", call. = FALSE)
  }

  flows <- sankey$flows
  ribbons <- vector("list", nrow(flows))
  centerlines <- vector("list", nrow(flows))
  edges <- vector("list", nrow(flows))
  t <- seq(0, 1, length.out = n)
  ease <- t * t * (3 - 2 * t)
  if(boundary_ease != 1) {
    ease <- ease^boundary_ease / (ease^boundary_ease + (1 - ease)^boundary_ease)
    ease[1] <- 0
    ease[length(ease)] <- 1
  }

  group_ids <- switch(
    boundary_group,
    target = sort(unique(flows$to)),
    source = sort(unique(flows$from)),
    stage = "stage"
  )

  for(group_id in group_ids) {
    inds <- switch(
      boundary_group,
      target = which(flows$to == group_id),
      source = which(flows$from == group_id),
      stage = seq_len(nrow(flows))
    )
    start_visual_all <- flows$start_y * xyr + flows$start_offset
    end_visual_all <- flows$end_y * xyr + flows$end_offset
    ord <- switch(
      boundary_group,
      target = inds[order(start_visual_all[inds], flows$from[inds], flows$to[inds])],
      source = inds[order(end_visual_all[inds], flows$to[inds], flows$from[inds])],
      stage = inds[order(start_visual_all[inds], flows$from[inds], flows$to[inds])]
    )

    if(diff(range(flows$start_x[ord])) > x_tol ||
       diff(range(flows$end_x[ord])) > x_tol) {
      stop("Boundary transport requires common x positions within each grouped stage side.", call. = FALSE)
    }

    start_center <- flows$start_y[ord] * xyr + flows$start_offset[ord]
    end_center <- flows$end_y[ord] * xyr + flows$end_offset[ord]
    start_lower <- start_center - flows$width[ord] / 2
    start_upper <- start_center + flows$width[ord] / 2
    end_lower <- end_center - flows$width[ord] / 2
    end_upper <- end_center + flows$width[ord] / 2
    if(length(ord) > 1) {
      start_gap <- tail(start_lower, -1) - head(start_upper, -1)
      end_gap <- tail(end_lower, -1) - head(end_upper, -1)
      if(any(start_gap < -gap_tol) || any(end_gap < -gap_tol)) {
        stop("Boundary transport requires non-overlapping endpoint stacks in the selected grouping.", call. = FALSE)
      }
    }

    x <- flows$start_x[ord[1]] * (1 - t) + flows$end_x[ord[1]] * t
    lower_mat <- vapply(seq_along(ord), function(local_i) {
      start_lower[local_i] * (1 - ease) + end_lower[local_i] * ease
    }, numeric(length(t)))
    upper_mat <- vapply(seq_along(ord), function(local_i) {
      start_upper[local_i] * (1 - ease) + end_upper[local_i] * ease
    }, numeric(length(t)))

    if(!identical(boundary_compensation, "none")) {
      center_mat <- (lower_mat + upper_mat) / 2
      slope_mat <- apply(center_mat, 2, function(center) {
        dy <- diff(center)
        dx <- diff(x)
        slope <- dy / pmax(abs(dx), 1e-12)
        c(slope[1], (head(slope, -1) + tail(slope, -1)) / 2, slope[length(slope)])
      })
      if(is.null(dim(slope_mat))) {
        slope_mat <- matrix(slope_mat, ncol = 1)
      }
      vwidth_mat <- sweep(sqrt(1 + slope_mat^2), 2, flows$width[ord], `*`)
      if(is.null(dim(vwidth_mat))) {
        vwidth_mat <- matrix(vwidth_mat, ncol = 1)
      }
      vwidth_mat[1, ] <- flows$width[ord]
      vwidth_mat[nrow(vwidth_mat), ] <- flows$width[ord]

      if(identical(boundary_compensation, "gap_budget")) {
        base_half <- matrix(rep(flows$width[ord] / 2, each = length(t)), nrow = length(t))
        # Matrix first: pmax() takes dims from its first argument.
        extra_half <- pmax(vwidth_mat / 2 - base_half, 0)
        lambda <- rep(1, length(t))
        if(length(ord) > 1) {
          base_gaps <- lower_mat[, -1, drop = FALSE] - upper_mat[, -ncol(upper_mat), drop = FALSE]
          needed <- extra_half[, -1, drop = FALSE] + extra_half[, -ncol(extra_half), drop = FALSE]
          lambda <- apply(ifelse(needed > 1e-12, base_gaps / needed, Inf), 1, min)
          lambda <- pmax(0, pmin(1, lambda))
        }
        half_mat <- base_half + extra_half * lambda
        lower_mat <- center_mat - half_mat
        upper_mat <- center_mat + half_mat
      } else {
        gap_mat <- if(length(ord) > 1) {
          vapply(seq_len(length(ord) - 1), function(i) {
            start_gap[i] * (1 - ease) + end_gap[i] * ease
          }, numeric(length(t)))
        } else {
          matrix(numeric(), nrow = length(t), ncol = 0)
        }
        lower_comp <- matrix(NA_real_, nrow = length(t), ncol = length(ord))
        upper_comp <- matrix(NA_real_, nrow = length(t), ncol = length(ord))
        for(row_i in seq_along(t)) {
          cursor <- lower_mat[row_i, 1]
          for(local_i in seq_along(ord)) {
            lower_comp[row_i, local_i] <- cursor
            upper_comp[row_i, local_i] <- cursor + vwidth_mat[row_i, local_i]
            cursor <- upper_comp[row_i, local_i]
            if(local_i < length(ord)) {
              cursor <- cursor + gap_mat[row_i, local_i]
            }
          }
          shift <- ((lower_mat[row_i, 1] + upper_mat[row_i, length(ord)]) -
                      (lower_comp[row_i, 1] + upper_comp[row_i, length(ord)])) / 2
          lower_mat[row_i, ] <- lower_comp[row_i, ] + shift
          upper_mat[row_i, ] <- upper_comp[row_i, ] + shift
        }
      }
    }

    for(local_i in seq_along(ord)) {
      k <- ord[local_i]
      lower <- lower_mat[, local_i]
      upper <- upper_mat[, local_i]
      center <- (lower + upper) / 2
      centerlines[[k]] <- cbind(x = x, y = center / xyr)
      edges[[k]] <- list(left = cbind(x = x, y = upper / xyr), right = cbind(x = x, y = lower / xyr))
      ribbons[[k]] <- edges_to_ribbon(edges[[k]])
    }
  }

  names(ribbons) <- sprintf("from_%s_to_%s", flows$from, flows$to)
  list(ribbons = ribbons, centerlines = centerlines, edges = edges)
}

rounded_sankey_ribbon <- function(a, b, width, n = 40, curvature = 0.45, xyr = 1) {
  a <- a * c(1, xyr)
  b <- b * c(1, xyr)
  direction <- sign(b[1] - a[1])
  rise <- if(b[2] < a[2]) -1 else 1
  dx <- abs(b[1] - a[1])
  dy <- abs(b[2] - a[2])
  if(dx <= 0) stop("Constant-width routing needs horizontal space between its endpoints.", call. = FALSE)
  max_radius <- if(dy <= dx) (dx^2 + dy^2) / (4 * max(dy, 1e-12)) else dx / 2
  if(max_radius <= width * 0.55) {
    stop("A bend is too tight for constant-width ribbons. Increase horizontal node spacing or reduce flow widths.", call. = FALSE)
  }
  if(dy < 1e-10) {
    points <- rbind(a, b)
    normals <- cbind(c(0, 0), c(1, 1))
  } else {
    radius <- if(dy <= dx) max_radius else min(dx * 0.49, max(width * 0.55, curvature * dx))
    if(dy <= dx) {
      angle <- 2 * atan2(dy, dx)
    } else {
      lo <- 0
      hi <- pi / 2
      for(i in seq_len(48)) {
        mid <- (lo + hi) / 2
        if(dy * cos(mid) - dx * sin(mid) + 2 * radius * (1 - cos(mid)) > 0) lo <- mid else hi <- mid
      }
      angle <- (lo + hi) / 2
    }
    count <- max(16, ceiling(n / 2), ceiling(angle / 0.04) + 1)
    t <- seq(0, angle, length.out = count)
    back <- rev(t)
    if(dy <= dx) back <- back[-1]
    x <- c(radius * sin(t), dx - radius * sin(back))
    y <- c(radius * (1 - cos(t)), dy - radius * (1 - cos(back)))
    tangent <- c(t, back)
    points <- cbind(a[1] + direction * x, a[2] + rise * y)
    normals <- cbind(-direction * rise * sin(tangent), cos(tangent))
  }
  upper <- points + normals * width / 2
  lower <- points - normals * width / 2
  as_data <- function(points) {
    points[, 2] <- points[, 2] / xyr
    colnames(points) <- c("x", "y")
    points
  }
  list(centerline = as_data(points), edges = if(direction > 0) {
    list(left = as_data(upper), right = as_data(lower))
  } else list(left = as_data(lower), right = as_data(upper)))
}

bezier_sankey_geometry <- function(sankey, n, curvature, xyr) {
  flows <- sankey$flows
  ribbons <- vector("list", nrow(flows))
  centerlines <- vector("list", nrow(flows))
  edges <- vector("list", nrow(flows))

  for(k in seq_len(nrow(flows))) {
    flow <- flows[k, ]
    dx <- flow$end_x - flow$start_x
    if(abs(dx) <= 1e-10) {
      stop("Sankey ribbons require from/to nodes to have different x positions.", call. = FALSE)
    }

    # Stack offsets are vertical (positive = up) whichever way the flow travels.
    p0 <- c(flow$start_x, flow$start_y + flow$start_offset / xyr)
    p3 <- c(flow$end_x, flow$end_y + flow$end_offset / xyr)
    handle <- curvature * dx
    p1 <- c(p0[1] + handle, p0[2])
    p2 <- c(p3[1] - handle, p3[2])
    centerline <- cubic_bezier(p0, p1, p2, p3, n = n)

    centerlines[[k]] <- centerline
    edges[[k]] <- centerline_edges(
      centerline[, "x"], centerline[, "y"], flow$width, xyr = xyr,
      endpoint_normals = "vertical"
    )
    direction <- sign(dx)
    if(any(direction * diff(edges[[k]]$left[, "x"]) < -1e-7) ||
       any(direction * diff(edges[[k]]$right[, "x"]) < -1e-7)) {
      rounded <- rounded_sankey_ribbon(p0, p3, flow$width, n, curvature, xyr)
      centerlines[[k]] <- rounded$centerline
      edges[[k]] <- rounded$edges
    }
    ribbons[[k]] <- edges_to_ribbon(edges[[k]])
  }


  names(ribbons) <- sprintf("from_%s_to_%s", flows$from, flows$to)
  list(ribbons = ribbons, centerlines = centerlines, edges = edges)
}

# Build Sankey ribbon geometry.
#
# thic is either one m-by-n matrix of non-negative flow widths or a list of
# matrices for successive node sets. For a list, ncol(thic[[i]]) must equal
# nrow(thic[[i + 1]]). loc is a matrix/data.frame with columns:
# node set index, node index within that set, x, and y. A fifth numeric matrix
# column, or a data-frame width column, gives horizontal node width. Data-frame
# label and cex columns can derive width from text extents when width is absent.
# The function returns polygon coordinates, centerlines, flow bookkeeping, node
# tables, and the shared merge length used near nodes. Set adjust_loc = TRUE to isotonic-adjust
# node y coordinates so adjacent node bundles have at least min_node_gap
# visual units between their outer edges where possible. If adjust_loc_expand
# is TRUE, each node set may expand outside its original y range when the
# requested spacing does not fit. The default route samples individual Bezier
# centerlines. Set route = "boundary" to interpolate packed ribbon boundaries
# directly; boundary_group controls whether each boundary bundle is grouped by
# destination, source, or the whole stage, and boundary_compensation can expand
# steep ribbons to better preserve apparent normal width.
sankey_ribbons <- function(thic, loc, from_set = 1, to_set = 2, n = 120,
                           curvature = 0.45, merge_frac = 0.05,
                           merge_length = NULL, xyr = xyrat_local(),
                           default_node_width = 0, node_label_pad = 0,
                           node_label_pad_frac = 0.04,
                           adjust_node_height = c("none", "label", "node"),
                           node_label_vpad = 0,
                           node_label_vpad_frac = 0.25,
                           adjust_loc = FALSE, min_node_gap = 0,
                           adjust_loc_expand = TRUE,
                           route = c("bezier", "boundary"),
                           boundary_group = c("target", "source", "stage"),
                           boundary_compensation = c("none", "full", "gap_budget"),
                           boundary_ease = 1,
                           source_order = sankey_source_order_choices,
                           target_order = sankey_target_order_choices,
                           stack_order = NULL) {
  if(!isTRUE(adjust_loc) && !identical(adjust_loc, FALSE)) {
    stop("`adjust_loc` must be TRUE or FALSE.", call. = FALSE)
  }
  stack_order <- validate_stack_order(stack_order)
  adjust_node_height <- validate_single_choice(
    adjust_node_height,
    c("none", "label", "node"),
    "`adjust_node_height`"
  )
  if(is.list(thic)) {
    validate_sankey_thic_list(thic)
    if(!is.null(merge_length) &&
       (!is.numeric(merge_length) || length(merge_length) != 1 ||
        !is.finite(merge_length) || merge_length < 0)) {
      stop("`merge_length` must be NULL or a single non-negative finite number.", call. = FALSE)
    }
    if(isTRUE(adjust_loc)) {
      loc <- sankey_adjust_loc(
        thic,
        loc,
        xyr = xyr,
        min_node_gap = min_node_gap,
        allow_expand = adjust_loc_expand,
        default_node_width = default_node_width,
        node_label_pad = node_label_pad,
        node_label_pad_frac = node_label_pad_frac
      )
    }
    loc <- adjust_sankey_node_heights(
      thic,
      loc,
      mode = adjust_node_height,
      xyr = xyr,
      node_label_vpad = node_label_vpad,
      node_label_vpad_frac = node_label_vpad_frac,
      default_node_width = default_node_width,
      node_label_pad = node_label_pad,
      node_label_pad_frac = node_label_pad_frac
    )
    node_counts <- c(nrow(thic[[1]]), vapply(thic, ncol, integer(1)))
    segments <- lapply(seq_along(thic), function(i) {
      sankey_ribbons(
        thic = thic[[i]],
        loc = loc,
        from_set = i,
        to_set = i + 1,
        n = n,
        curvature = curvature,
        merge_frac = merge_frac,
        merge_length = merge_length,
        xyr = xyr,
        default_node_width = default_node_width,
        node_label_pad = node_label_pad,
        node_label_pad_frac = node_label_pad_frac,
        adjust_node_height = "none",
        node_label_vpad = node_label_vpad,
        node_label_vpad_frac = node_label_vpad_frac,
        adjust_loc = FALSE,
        min_node_gap = min_node_gap,
        adjust_loc_expand = adjust_loc_expand,
        route = sankey_stage_param(route, i, c("bezier", "boundary"), "`route`"),
        boundary_group = sankey_stage_param(boundary_group, i, c("target", "source", "stage"), "`boundary_group`"),
        boundary_compensation = sankey_stage_param(
          boundary_compensation,
          i,
          c("none", "full", "gap_budget"),
          "`boundary_compensation`"
        ),
        boundary_ease = sankey_stage_param(boundary_ease, i, arg = "`boundary_ease`"),
        source_order = sankey_stage_param(source_order, i, sankey_source_order_choices, "`source_order`"),
        target_order = sankey_stage_param(target_order, i, sankey_target_order_choices, "`target_order`"),
        stack_order = stage_stack_order(stack_order, i, length(thic))
      )
    })
    path_lengths <- unlist(lapply(segments, `[[`, "path_lengths"), use.names = FALSE)
    if(is.null(merge_length)) {
      merge_length <- merge_frac * min(path_lengths)
      segments <- lapply(seq_along(thic), function(i) {
        sankey_ribbons(
          thic = thic[[i]],
          loc = loc,
          from_set = i,
          to_set = i + 1,
          n = n,
          curvature = curvature,
          merge_frac = merge_frac,
          merge_length = merge_length,
          xyr = xyr,
          default_node_width = default_node_width,
          node_label_pad = node_label_pad,
          node_label_pad_frac = node_label_pad_frac,
          adjust_node_height = "none",
          node_label_vpad = node_label_vpad,
          node_label_vpad_frac = node_label_vpad_frac,
          adjust_loc = FALSE,
          min_node_gap = min_node_gap,
          adjust_loc_expand = adjust_loc_expand,
          route = sankey_stage_param(route, i, c("bezier", "boundary"), "`route`"),
          boundary_group = sankey_stage_param(boundary_group, i, c("target", "source", "stage"), "`boundary_group`"),
          boundary_compensation = sankey_stage_param(
            boundary_compensation,
            i,
            c("none", "full", "gap_budget"),
            "`boundary_compensation`"
          ),
          boundary_ease = sankey_stage_param(boundary_ease, i, arg = "`boundary_ease`"),
          source_order = sankey_stage_param(source_order, i, sankey_source_order_choices, "`source_order`"),
          target_order = sankey_stage_param(target_order, i, sankey_target_order_choices, "`target_order`"),
          stack_order = stage_stack_order(stack_order, i, length(thic))
        )
      })
    }
    return(combine_sankey_segments(
      segments,
      loc,
      node_counts,
      merge_length,
      xyr,
      default_node_width,
      node_label_pad,
      node_label_pad_frac
    ))
  }

  route <- validate_single_choice(route, c("bezier", "boundary"), "`route`")
  boundary_group <- validate_single_choice(
    boundary_group,
    c("target", "source", "stage"),
    "`boundary_group`"
  )
  boundary_compensation <- validate_single_choice(
    boundary_compensation,
    c("none", "full", "gap_budget"),
    "`boundary_compensation`"
  )
  if(!is.numeric(boundary_ease) || length(boundary_ease) != 1 ||
     !is.finite(boundary_ease) || boundary_ease <= 0) {
    stop("`boundary_ease` must be a single positive finite number.", call. = FALSE)
  }
  source_order <- validate_single_choice(source_order, sankey_source_order_choices, "`source_order`")
  target_order <- validate_single_choice(target_order, sankey_target_order_choices, "`target_order`")
  if(!is.null(stack_order) && "stage" %in% names(stack_order)) {
    stack_order <- stage_stack_order(stack_order, 1, 1)
  }
  if(!is.numeric(merge_frac) || length(merge_frac) != 1 || !is.finite(merge_frac) || merge_frac < 0) {
    stop("`merge_frac` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.null(merge_length) &&
     (!is.numeric(merge_length) || length(merge_length) != 1 ||
      !is.finite(merge_length) || merge_length < 0)) {
    stop("`merge_length` must be NULL or a single non-negative finite number.", call. = FALSE)
  }
  if(isTRUE(adjust_loc)) {
    loc <- sankey_adjust_loc(
      thic,
      loc,
      from_set = from_set,
      to_set = to_set,
      xyr = xyr,
      min_node_gap = min_node_gap,
      allow_expand = adjust_loc_expand,
      default_node_width = default_node_width,
      node_label_pad = node_label_pad,
      node_label_pad_frac = node_label_pad_frac
    )
  }
  loc <- adjust_sankey_node_heights(
    thic,
    loc,
    mode = adjust_node_height,
    from_set = from_set,
    to_set = to_set,
    xyr = xyr,
    node_label_vpad = node_label_vpad,
    node_label_vpad_frac = node_label_vpad_frac,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac
  )

  sankey <- build_sankey_flows(
    thic,
    loc,
    from_set = from_set,
    to_set = to_set,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac,
    source_order = source_order,
    target_order = target_order,
    stack_order = stack_order
  )
  flows <- sankey$flows

  if(identical(route, "boundary")) {
    if(bezier_order_requested(source_order, target_order)) {
      stop("Bezier-sampled endpoint ordering requires `route = \"bezier\"`.", call. = FALSE)
    }
    geometry <- boundary_transport_geometry(
      sankey,
      n = n,
      xyr = xyr,
      boundary_group = boundary_group,
      boundary_compensation = boundary_compensation,
      boundary_ease = boundary_ease
    )
  } else {
    geometry <- bezier_sankey_geometry(sankey, n = n, curvature = curvature, xyr = xyr)
    if(bezier_order_requested(source_order, target_order)) {
      sankey <- apply_bezier_stack_order(sankey, geometry, source_order, target_order)
      sankey$flows <- apply_manual_stack_order(sankey$flows, stack_order)
      flows <- sankey$flows
      geometry <- bezier_sankey_geometry(sankey, n = n, curvature = curvature, xyr = xyr)
    }
  }
  ribbons <- geometry$ribbons
  centerlines <- geometry$centerlines

  names(ribbons) <- sprintf("from_%s_to_%s", flows$from, flows$to)
  path_lengths <- vapply(centerlines, visual_path_length, numeric(1), xyr = xyr)
  if(is.null(merge_length)) {
    merge_length <- merge_frac * min(path_lengths)
  }

  sankey$ribbons <- ribbons
  sankey$centerlines <- centerlines
  sankey$edges <- geometry$edges
  sankey$path_lengths <- path_lengths
  sankey$merge_length <- merge_length
  sankey$xyr <- xyr
  sankey$loc <- loc
  sankey$route <- route
  if(identical(route, "boundary")) {
    sankey$boundary_group <- boundary_group
    sankey$boundary_compensation <- boundary_compensation
    sankey$boundary_ease <- boundary_ease
  }
  sankey
}

validate_sankey_graph_inputs <- function(thic, loc, default_node_width = 0,
                                         node_label_pad = 0,
                                         node_label_pad_frac = 0.04) {
  if(!is.matrix(thic) || !is.numeric(thic)) {
    stop("Graph `thic` must be a numeric square matrix.", call. = FALSE)
  }
  if(nrow(thic) != ncol(thic)) {
    stop("Graph `thic` must be square.", call. = FALSE)
  }
  if(any(!is.finite(thic)) || any(thic < 0)) {
    stop("Graph `thic` must contain finite, non-negative values.", call. = FALSE)
  }
  if(any(diag(thic) > 0)) {
    stop("Graph `thic` must have a zero diagonal.", call. = FALSE)
  }
  loc <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)
  if(nrow(loc) != nrow(thic)) {
    stop("Graph `loc` must contain exactly one row per row/column of `thic`.", call. = FALSE)
  }
  if(any(duplicated(paste(loc$set, loc$node, sep = "\r")))) {
    stop("Graph node set/index pairs in `loc` must be unique.", call. = FALSE)
  }
  invisible(TRUE)
}

build_sankey_graph_flows <- function(thic, loc, require_forward = TRUE,
                                     balance = c("allow", "warn", "error"),
                                     balance_tol = 1e-8,
                                     default_node_width = 0,
                                     node_label_pad = 0,
                                     node_label_pad_frac = 0.04,
                                     source_order = sankey_source_order_choices,
                                     target_order = sankey_target_order_choices,
                                     stack_order = NULL) {
  balance <- validate_single_choice(balance, c("allow", "warn", "error"), "`balance`")
  source_order <- validate_single_choice(source_order, sankey_source_order_choices, "`source_order`")
  target_order <- validate_single_choice(target_order, sankey_target_order_choices, "`target_order`")
  if(!isTRUE(require_forward) && !identical(require_forward, FALSE)) {
    stop("`require_forward` must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.numeric(balance_tol) || length(balance_tol) != 1 ||
     !is.finite(balance_tol) || balance_tol < 0) {
    stop("`balance_tol` must be a single non-negative finite number.", call. = FALSE)
  }
  validate_sankey_graph_inputs(thic, loc, default_node_width, node_label_pad, node_label_pad_frac)
  loc <- as_sankey_loc_table(loc, default_node_width, node_label_pad, node_label_pad_frac)

  nodes <- data.frame(
    id = seq_len(nrow(loc)),
    set = loc$set,
    node = loc$node,
    x = loc$x,
    y = loc$y,
    node_width = loc$node_width,
    label = loc$label,
    has_label = loc$has_label,
    cex = loc$cex,
    font = loc$font,
    node_label_width = loc$node_label_width,
    width_explicit = loc$width_explicit,
    node_body_height = loc$node_body_height,
    node_label_body_height = loc$node_label_body_height
  )
  positive <- which(thic > 0, arr.ind = TRUE)
  if(nrow(positive) == 0) {
    stop("Graph `thic` must contain at least one positive value.", call. = FALSE)
  }

  flows <- data.frame(
    from = positive[, "row"],
    to = positive[, "col"],
    width = thic[positive],
    start_offset = NA_real_,
    end_offset = NA_real_
  )
  faces <- node_face_x(nodes, nodes, flows$from, flows$to)
  flows$start_x <- faces$start
  flows$start_node_x <- nodes$x[flows$from]
  flows$start_y <- nodes$y[flows$from]
  flows$end_x <- faces$end
  flows$end_node_x <- nodes$x[flows$to]
  flows$end_y <- nodes$y[flows$to]

  if(isTRUE(require_forward) && any(nodes$x[flows$to] <= nodes$x[flows$from])) {
    stop("Graph Sankey flows must move left-to-right when `require_forward = TRUE`.", call. = FALSE)
  }

  incoming <- colSums(thic)
  outgoing <- rowSums(thic)
  internal <- incoming > 0 & outgoing > 0
  imbalance <- outgoing - incoming
  bad_balance <- internal & abs(imbalance) > balance_tol
  if(any(bad_balance)) {
    msg <- paste(
      "Graph Sankey has flow-through nodes with unequal incoming and outgoing totals:",
      paste(which(bad_balance), collapse = ", ")
    )
    if(identical(balance, "error")) {
      stop(msg, call. = FALSE)
    }
    if(identical(balance, "warn")) {
      warning(msg, call. = FALSE)
    }
  }

  for(i in seq_len(nrow(thic))) {
    inds <- which(flows$from == i)
    order <- switch(
      source_order,
      target_y = inds[order(nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])],
      target_y_desc = inds[order(-nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])],
      target_index = inds[order(flows$to[inds])],
      target_projected = inds[order(nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])],
      target_projected_desc = inds[order(-nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])],
      target_bezier = inds[order(nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])],
      target_bezier_desc = inds[order(-nodes$y[flows$to[inds]], nodes$x[flows$to[inds]], flows$to[inds])]
    )
    flows$start_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }

  for(j in seq_len(ncol(thic))) {
    inds <- which(flows$to == j)
    order <- switch(
      target_order,
      source_y = inds[order(nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])],
      source_y_desc = inds[order(-nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])],
      source_index = inds[order(flows$from[inds])],
      source_projected = inds[order(nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])],
      source_projected_desc = inds[order(-nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])],
      source_bezier = inds[order(nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])],
      source_bezier_desc = inds[order(-nodes$y[flows$from[inds]], nodes$x[flows$from[inds]], flows$from[inds])]
    )
    flows$end_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }
  flows <- apply_projected_stack_order(flows, source_order, target_order)
  flows <- apply_manual_stack_order(flows, stack_order)

  out <- list(
    flows = flows,
    nodes = nodes,
    from_nodes = nodes[outgoing > 0, c("id", "set", "node", "x", "y", "node_width", "label", "has_label", "cex", "font", "node_label_width", "width_explicit", "node_body_height", "node_label_body_height"), drop = FALSE],
    to_nodes = nodes[incoming > 0, c("id", "set", "node", "x", "y", "node_width", "label", "has_label", "cex", "font", "node_label_width", "width_explicit", "node_body_height", "node_label_body_height"), drop = FALSE],
    incoming = incoming,
    outgoing = outgoing,
    node_width = pmax(incoming, outgoing),
    node_balance = imbalance
  )
  rownames(out$from_nodes) <- NULL
  rownames(out$to_nodes) <- NULL
  out
}

# Build Sankey ribbon geometry from a square adjacency matrix over all nodes.
#
# Rows and columns of `thic` refer to rows of `loc`, so `loc` must contain one
# row per graph node. Positive entries must move left-to-right unless
# require_forward = FALSE. Optional node widths move endpoints to node faces and
# allow flow-through graph nodes to blend incoming and outgoing bundles across a
# horizontal node body. This graph mode is useful when nodes can receive strands
# from non-adjacent visual stages.
sankey_ribbons_graph <- function(thic, loc, n = 120, curvature = 0.45,
                                 merge_frac = 0.05, merge_length = NULL,
                                 xyr = xyrat_local(),
                                 default_node_width = 0, node_label_pad = 0,
                                 node_label_pad_frac = 0.04,
                                 adjust_node_height = c("none", "label", "node"),
                                 node_label_vpad = 0,
                                 node_label_vpad_frac = 0.25,
                                 route = c("bezier", "boundary"),
                                 boundary_group = c("target", "source", "stage"),
                                 boundary_compensation = c("none", "full", "gap_budget"),
                                 boundary_ease = 1,
                                 require_forward = TRUE,
                                 balance = c("allow", "warn", "error"),
                                 balance_tol = 1e-8,
                                 source_order = sankey_source_order_choices,
                                 target_order = sankey_target_order_choices,
                                 stack_order = NULL) {
  route <- validate_single_choice(route, c("bezier", "boundary"), "`route`")
  stack_order <- validate_stack_order(stack_order)
  if(!is.null(stack_order) && "stage" %in% names(stack_order)) {
    stack_order <- stage_stack_order(stack_order, 1, 1)
  }
  if(!is.numeric(merge_frac) || length(merge_frac) != 1 || !is.finite(merge_frac) || merge_frac < 0) {
    stop("`merge_frac` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.null(merge_length) &&
     (!is.numeric(merge_length) || length(merge_length) != 1 ||
      !is.finite(merge_length) || merge_length < 0)) {
    stop("`merge_length` must be NULL or a single non-negative finite number.", call. = FALSE)
  }
  adjust_node_height <- validate_single_choice(
    adjust_node_height,
    c("none", "label", "node"),
    "`adjust_node_height`"
  )
  source_order <- validate_single_choice(source_order, sankey_source_order_choices, "`source_order`")
  target_order <- validate_single_choice(target_order, sankey_target_order_choices, "`target_order`")
  loc <- adjust_sankey_node_heights(
    thic,
    loc,
    mode = adjust_node_height,
    graph = TRUE,
    xyr = xyr,
    node_label_vpad = node_label_vpad,
    node_label_vpad_frac = node_label_vpad_frac,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac
  )

  sankey <- build_sankey_graph_flows(
    thic,
    loc,
    require_forward = require_forward,
    balance = balance,
    balance_tol = balance_tol,
    default_node_width = default_node_width,
    node_label_pad = node_label_pad,
    node_label_pad_frac = node_label_pad_frac,
    source_order = source_order,
    target_order = target_order,
    stack_order = stack_order
  )
  if(identical(route, "boundary")) {
    if(bezier_order_requested(source_order, target_order)) {
      stop("Bezier-sampled endpoint ordering requires `route = \"bezier\"`.", call. = FALSE)
    }
    geometry <- boundary_transport_geometry(
      sankey,
      n = n,
      xyr = xyr,
      boundary_group = boundary_group,
      boundary_compensation = boundary_compensation,
      boundary_ease = boundary_ease
    )
  } else {
    geometry <- bezier_sankey_geometry(sankey, n = n, curvature = curvature, xyr = xyr)
    if(bezier_order_requested(source_order, target_order)) {
      sankey <- apply_bezier_stack_order(sankey, geometry, source_order, target_order)
      sankey$flows <- apply_manual_stack_order(sankey$flows, stack_order)
      geometry <- bezier_sankey_geometry(sankey, n = n, curvature = curvature, xyr = xyr)
    }
  }

  sankey$ribbons <- geometry$ribbons
  sankey$centerlines <- geometry$centerlines
  sankey$edges <- geometry$edges
  sankey$path_lengths <- vapply(sankey$centerlines, visual_path_length, numeric(1), xyr = xyr)
  if(is.null(merge_length)) {
    merge_length <- merge_frac * min(sankey$path_lengths)
  }
  sankey$merge_length <- merge_length
  sankey$xyr <- xyr
  sankey$loc <- loc
  sankey$route <- route
  if(identical(route, "boundary")) {
    sankey$boundary_group <- validate_single_choice(
      boundary_group,
      c("target", "source", "stage"),
      "`boundary_group`"
    )
    sankey$boundary_compensation <- validate_single_choice(
      boundary_compensation,
      c("none", "full", "gap_budget"),
      "`boundary_compensation`"
    )
    sankey$boundary_ease <- boundary_ease
  }
  class(sankey) <- c("sankey_graph", "list")
  sankey
}

sankey_flow_colors <- function(sankey, col, allow_na = FALSE, arg = "`col`") {
  if(is.matrix(col)) {
    if(nrow(col) < max(sankey$flows$from) || ncol(col) < max(sankey$flows$to)) {
      stop(sprintf("%s matrix must have at least as many rows and columns as `thic`.", arg), call. = FALSE)
    }
    out <- col[cbind(sankey$flows$from, sankey$flows$to)]
  } else if(length(col) == 1) {
    out <- rep(col, nrow(sankey$flows))
  } else if(length(col) == nrow(sankey$flows)) {
    out <- col
  } else {
    stop(sprintf("%s must be length 1, one color per positive flow, or a color matrix.", arg), call. = FALSE)
  }

  out <- as.character(out)
  if(!allow_na && anyNA(out)) {
    stop(sprintf("%s cannot contain missing values.", arg), call. = FALSE)
  }
  non_missing <- out[!is.na(out)]
  if(length(non_missing) > 0) {
    invisible(grDevices::col2rgb(non_missing, alpha = TRUE))
  }
  out
}

weighted_average_color <- function(cols, weights) {
  if(length(cols) != length(weights)) {
    stop("Color and weight vectors must have the same length.", call. = FALSE)
  }
  if(anyNA(cols)) {
    stop("Cannot average missing colors.", call. = FALSE)
  }
  rgba <- grDevices::col2rgb(cols, alpha = TRUE) / 255
  avg <- rowSums(t(t(rgba) * weights)) / sum(weights)
  grDevices::rgb(avg[1], avg[2], avg[3], avg[4])
}

node_merge_color <- function(sankey, flow_cols, flow_inds) {
  weighted_average_color(flow_cols[flow_inds], sankey$flows$width[flow_inds])
}

interpolate_color <- function(col0, col1, t) {
  if(!is.finite(t)) {
    stop("Color interpolation position must be finite.", call. = FALSE)
  }
  t <- min(max(t, 0), 1)
  rgba <- grDevices::col2rgb(c(col0, col1), alpha = TRUE) / 255
  out <- rgba[, 1] * (1 - t) + rgba[, 2] * t
  grDevices::rgb(out[1], out[2], out[3], out[4])
}

flow_node_colors <- function(sankey, flow_cols) {
  start_cols <- end_cols <- character(nrow(sankey$flows))
  for(k in seq_len(nrow(sankey$flows))) {
    from_inds <- which(sankey$flows$from == sankey$flows$from[k])
    to_inds <- which(sankey$flows$to == sankey$flows$to[k])
    start_cols[k] <- node_merge_color(sankey, flow_cols, from_inds)
    end_cols[k] <- node_merge_color(sankey, flow_cols, to_inds)
  }
  list(start = start_cols, end = end_cols)
}

validate_merge_cols <- function(sankey, merge_cols) {
  if(is.null(merge_cols)) {
    return(invisible(TRUE))
  }
  if(!is.list(merge_cols) || is.null(merge_cols$start) || is.null(merge_cols$end)) {
    stop("`merge_cols` must be NULL or a list with `start` and `end` color vectors.", call. = FALSE)
  }
  if(length(merge_cols$start) != nrow(sankey$flows) || length(merge_cols$end) != nrow(sankey$flows)) {
    stop("`merge_cols$start` and `merge_cols$end` must have one color per positive flow.", call. = FALSE)
  }
  invisible(grDevices::col2rgb(c(merge_cols$start, merge_cols$end), alpha = TRUE))
  invisible(TRUE)
}

multi_stage_flow_colors <- function(sankey, col) {
  lapply(seq_along(sankey$segments), function(i) {
    sankey_flow_colors(sankey$segments[[i]], stage_arg(col, i))
  })
}

multi_node_merge_colors <- function(sankey, stage_cols) {
  out <- vector("list", length(sankey$node_counts))
  for(node_set in seq_along(sankey$node_counts)) {
    out[[node_set]] <- character(sankey$node_counts[node_set])
    for(node in seq_len(sankey$node_counts[node_set])) {
      cols <- character(0)
      weights <- numeric(0)
      if(node_set > 1) {
        prev <- sankey$segments[[node_set - 1]]
        inds <- which(prev$flows$to == node)
        cols <- c(cols, stage_cols[[node_set - 1]][inds])
        weights <- c(weights, prev$flows$width[inds])
      }
      if(node_set <= length(sankey$segments)) {
        next_segment <- sankey$segments[[node_set]]
        inds <- which(next_segment$flows$from == node)
        cols <- c(cols, stage_cols[[node_set]][inds])
        weights <- c(weights, next_segment$flows$width[inds])
      }
      out[[node_set]][node] <- if(length(cols) == 0) NA_character_ else weighted_average_color(cols, weights)
    }
  }
  out
}

multi_stage_merge_cols <- function(sankey, col) {
  stage_cols <- multi_stage_flow_colors(sankey, col)
  node_cols <- multi_node_merge_colors(sankey, stage_cols)
  lapply(seq_along(sankey$segments), function(i) {
    flows <- sankey$segments[[i]]$flows
    list(
      start = node_cols[[i]][flows$from],
      end = node_cols[[i + 1]][flows$to]
    )
  })
}

merge_cols_from_node_cols <- function(sankey, node_cols) {
  lapply(seq_along(sankey$segments), function(i) {
    flows <- sankey$segments[[i]]$flows
    list(
      start = node_cols[[i]][flows$from],
      end = node_cols[[i + 1]][flows$to]
    )
  })
}

validate_color_vector <- function(x, n, arg) {
  if(!is.character(x) || length(x) != n || anyNA(x)) {
    stop(sprintf("%s must be a character color vector of length %s.", arg, n), call. = FALSE)
  }
  invisible(grDevices::col2rgb(x, alpha = TRUE))
  x
}

single_sankey_node_colors <- function(sankey, node_col, arg = "`node_col`") {
  n_start <- nrow(sankey$from_nodes)
  n_end <- nrow(sankey$to_nodes)

  if(is.list(node_col)) {
    start <- if(!is.null(node_col$start)) node_col$start else node_col$from
    end <- if(!is.null(node_col$end)) node_col$end else node_col$to
    if(is.null(start) && length(node_col) >= 1) {
      start <- node_col[[1]]
    }
    if(is.null(end) && length(node_col) >= 2) {
      end <- node_col[[2]]
    }
    if(is.null(start) || is.null(end)) {
      stop(sprintf("%s must provide start/from and end/to colors.", arg), call. = FALSE)
    }
  } else if(is.character(node_col) && length(node_col) == n_start + n_end) {
    start <- node_col[seq_len(n_start)]
    end <- node_col[n_start + seq_len(n_end)]
  } else {
    stop(
      sprintf("%s must be a list of start/from and end/to colors, or a character vector ordered from nodes then to nodes.", arg),
      call. = FALSE
    )
  }

  list(
    start = validate_color_vector(start, n_start, sprintf("%s start/from", arg)),
    end = validate_color_vector(end, n_end, sprintf("%s end/to", arg))
  )
}

single_merge_cols_from_node_cols <- function(sankey, node_cols) {
  list(
    start = node_cols$start[sankey$flows$from],
    end = node_cols$end[sankey$flows$to]
  )
}

multi_sankey_node_colors <- function(sankey, node_col, arg = "`node_col`") {
  if(!is.list(node_col) || length(node_col) != length(sankey$node_counts)) {
    stop(sprintf("%s must be a list with one character color vector per node set.", arg), call. = FALSE)
  }
  lapply(seq_along(sankey$node_counts), function(i) {
    validate_color_vector(
      node_col[[i]],
      sankey$node_counts[i],
      sprintf("%s[[%s]]", arg, i)
    )
  })
}

colors_have_alpha <- function(cols) {
  cols <- cols[!is.na(cols)]
  if(length(cols) == 0) {
    return(FALSE)
  }
  any(grDevices::col2rgb(cols, alpha = TRUE)["alpha", ] < 255)
}

flatten_alpha_color <- function(cols, bg = NULL) {
  if(is.null(bg) || identical(bg, "transparent")) {
    bg <- "white"
  }
  bg_rgb <- grDevices::col2rgb(bg) / 255
  vapply(cols, function(col) {
    if(is.na(col)) {
      return(NA_character_)
    }
    rgba <- grDevices::col2rgb(col, alpha = TRUE) / 255
    rgb <- rgba[1:3, 1] * rgba[4, 1] + bg_rgb[, 1] * (1 - rgba[4, 1])
    grDevices::rgb(rgb[1], rgb[2], rgb[3], 1)
  }, character(1))
}

fade_alpha_color <- function(col, alpha_frac) {
  if(is.na(col)) {
    return(NA_character_)
  }
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  alpha <- rgba["alpha", 1] / 255 * alpha_frac
  if(!is.finite(alpha) || alpha <= 0) {
    return(NA_character_)
  }
  grDevices::rgb(
    rgba["red", 1] / 255,
    rgba["green", 1] / 255,
    rgba["blue", 1] / 255,
    alpha
  )
}

fade_background_color <- function(col, alpha_frac, bg = par("bg")) {
  faded <- fade_alpha_color(col, alpha_frac)
  if(is.na(faded)) {
    return(NA_character_)
  }
  flatten_alpha_color(faded, bg = bg)
}

graphics_lwd_to_user_width <- function(lwd) {
  diff(par("usr")[1:2]) / par("pin")[1] * lwd / 96
}

opaque_color <- function(col) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(rgba["red", 1] / 255, rgba["green", 1] / 255, rgba["blue", 1] / 255, 1)
}

line_between <- function(p0, p1, n) {
  t <- seq(0, 1, length.out = n)
  out <- cbind(
    x = p0["x"] * (1 - t) + p1["x"] * t,
    y = p0["y"] * (1 - t) + p1["y"] * t
  )
  colnames(out) <- c("x", "y")
  out
}

sample_polyline <- function(coords, n) {
  if(nrow(coords) < 2 || n <= nrow(coords)) {
    return(coords)
  }
  diffs <- coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE]
  lengths <- sqrt(rowSums(diffs^2))
  total <- sum(lengths)
  if(!is.finite(total) || total <= 0) {
    return(coords)
  }
  segment_n <- pmax(2L, ceiling(n * lengths / total))
  out <- coords[1, , drop = FALSE]
  for(i in seq_len(nrow(coords) - 1)) {
    out <- rbind(
      out,
      line_between(coords[i, ], coords[i + 1, ], segment_n[i])[-1, , drop = FALSE]
    )
  }
  colnames(out) <- c("x", "y")
  out
}

draw_faded_polyline <- function(coords, col, lwd, lty, alpha_fun,
                                fade = c("background", "alpha"),
                                shape = c("polyline", "taper", "line"),
                                bg = par("bg")) {
  fade <- match.arg(fade)
  shape <- match.arg(shape)
  if(is.na(col) || nrow(coords) < 2) {
    return(invisible(NULL))
  }
  diffs <- coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE]
  distances <- c(0, cumsum(sqrt(rowSums(diffs^2))))
  total <- max(distances)
  if(total <= 0) {
    return(invisible(NULL))
  }

  if(identical(shape, "taper")) {
    alpha <- vapply(distances / total, alpha_fun, numeric(1))
    max_width <- graphics_lwd_to_user_width(lwd)
    widths <- max_width * pmax(alpha, 0.001)
    poly_col <- if(identical(fade, "background")) {
      flatten_alpha_color(col, bg = bg)
    } else {
      col
    }
    polylines(
      coords[, "x"],
      coords[, "y"],
      lwd = widths,
      col = poly_col,
      border = NA,
      complex = TRUE,
      xpd = NA
    )
    return(invisible(NULL))
  }

  t_mid <- (distances[-length(distances)] + distances[-1]) / (2 * total)
  line_col <- vapply(t_mid, function(t) {
    alpha <- alpha_fun(t)
    if(identical(fade, "background")) {
      fade_background_color(col, alpha, bg = bg)
    } else {
      fade_alpha_color(col, alpha)
    }
  }, character(1))

  if(identical(shape, "polyline")) {
    polylines(
      coords[, "x"],
      coords[, "y"],
      lwd = rep(graphics_lwd_to_user_width(lwd), nrow(coords)),
      col = line_col,
      border = NA,
      complex = TRUE,
      draw_indiv = TRUE,
      xpd = NA
    )
    return(invisible(NULL))
  }

  keep <- !is.na(line_col)
  if(any(keep)) {
    segments(
      coords[-nrow(coords), "x"][keep],
      coords[-nrow(coords), "y"][keep],
      coords[-1, "x"][keep],
      coords[-1, "y"][keep],
      col = line_col[keep],
      lwd = lwd,
      lty = lty,
      lend = "butt",
      xpd = NA
    )
  }
  invisible(NULL)
}

node_body_border_alpha <- function(coords, node = NULL, direction = c("face", "both")) {
  direction <- match.arg(direction)
  if(nrow(coords) < 2) {
    return(function(t) 0)
  }
  x <- coords[, "x"]
  x_rng <- range(x)
  span <- diff(x_rng)
  if(!is.finite(span) || span <= 1e-12) {
    return(function(t) 1)
  }
  center_x <- if(!is.null(node) && "x" %in% names(node) && is.finite(node$x[1])) {
    min(max(node$x[1], x_rng[1]), x_rng[2])
  } else {
    mean(x_rng)
  }
  center_t <- (center_x - x_rng[1]) / span

  if(identical(direction, "both")) {
    return(function(t) {
      pmin(1, pmax(0, abs(t - center_t) / max(center_t, 1 - center_t, 1e-8)))
    })
  }

  function(t) {
    pmin(1, pmax(0, 1 - t))
  }
}

validate_node_border_args <- function(node_border, node_border_lwd,
                                      node_border_lty, node_border_steps) {
  if(is.null(node_border)) {
    return(TRUE)
  }
  if(length(node_border) != 1) {
    stop("`node_border` must be NULL, a single color, or NA.", call. = FALSE)
  }
  if(is.na(node_border)) {
    return(FALSE)
  }
  invisible(grDevices::col2rgb(node_border, alpha = TRUE))
  if(is.na(fade_alpha_color(node_border, 1))) {
    return(FALSE)
  }
  if(!is.numeric(node_border_lwd) || length(node_border_lwd) != 1 ||
     !is.finite(node_border_lwd) || node_border_lwd <= 0) {
    stop("`node_border_lwd` must be a single positive finite number.", call. = FALSE)
  }
  if(length(node_border_lty) != 1) {
    stop("`node_border_lty` must have length one.", call. = FALSE)
  }
  if(!is.numeric(node_border_steps) || length(node_border_steps) != 1 ||
     !is.finite(node_border_steps) || node_border_steps < 2) {
    stop("`node_border_steps` must be a single finite number >= 2.", call. = FALSE)
  }
  TRUE
}

draw_sankey_polygon <- function(coords, col) {
  parts <- attr(coords, "fill_polygons")
  if(is.null(parts)) parts <- list(coords)
  for(part in parts) polygon(part[, "x"], part[, "y"], col = col, border = NA, xpd = NA)
}

draw_x_gradient_polygon <- function(coords, low_col, high_col, steps = 24,
                                    flatten = FALSE, bg = par("bg")) {
  if(is.null(coords) || nrow(coords) < 4) {
    return(invisible(NULL))
  }
  steps <- max(2L, as.integer(round(steps)))
  x_rng <- range(coords[, "x"])
  if(diff(x_rng) <= 1e-12) {
    col <- interpolate_color(low_col, high_col, 0.5)
    if(flatten) {
      col <- flatten_alpha_color(col, bg = bg)
    }
    draw_sankey_polygon(coords, col)
    return(invisible(NULL))
  }

  breaks <- seq(x_rng[1], x_rng[2], length.out = steps + 1)
  for(i in seq_len(steps)) {
    slice <- clip_polygon_x(coords, breaks[i], breaks[i + 1])
    if(is.null(slice)) {
      next
    }
    t <- ((breaks[i] + breaks[i + 1]) / 2 - x_rng[1]) / diff(x_rng)
    col <- interpolate_color(low_col, high_col, t)
    if(flatten) {
      col <- flatten_alpha_color(col, bg = bg)
    }
    draw_sankey_polygon(slice, col)
  }
  invisible(NULL)
}

transparent_color <- function(col) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(rgba["red", 1] / 255, rgba["green", 1] / 255, rgba["blue", 1] / 255, 0)
}

node_has_user_label <- function(node) {
  "has_label" %in% names(node) && isTRUE(as.logical(node$has_label[1]))
}

draw_x_color_stops_polygon <- function(coords, x_stops, cols, steps = 24,
                                       flatten = FALSE, bg = par("bg")) {
  if(is.null(coords) || nrow(coords) < 4) {
    return(invisible(NULL))
  }
  if(length(x_stops) != length(cols) || length(cols) < 2) {
    stop("Color-stop positions and colors must have matching length >= 2.", call. = FALSE)
  }
  ord <- order(x_stops)
  x_stops <- as.numeric(x_stops[ord])
  cols <- cols[ord]
  keep <- c(TRUE, diff(x_stops) > 1e-12)
  x_stops <- x_stops[keep]
  cols <- cols[keep]
  if(length(cols) < 2) {
    col <- cols[1]
    if(flatten) {
      col <- flatten_alpha_color(col, bg = bg)
    }
    draw_sankey_polygon(coords, col)
    return(invisible(NULL))
  }

  for(i in seq_len(length(cols) - 1)) {
    x0 <- x_stops[i]
    x1 <- x_stops[i + 1]
    if(x1 - x0 <= 1e-12) {
      next
    }
    slice <- clip_polygon_x(coords, x0, x1)
    if(is.null(slice)) {
      next
    }
    interval_steps <- max(2L, as.integer(round(steps * (x1 - x0) / (max(x_stops) - min(x_stops)))))
    draw_x_gradient_polygon(
      slice,
      cols[i],
      cols[i + 1],
      steps = interval_steps,
      flatten = flatten,
      bg = bg
    )
  }
  invisible(NULL)
}

default_fill_overlap <- function(draw_border, gradient_slices, merge_length, cols, flatten_trunks = TRUE) {
  # Opaque (flattened) trunks hide the overlap, so translucent strands can safely run under them.
  if(draw_border || (!isTRUE(flatten_trunks) && gradient_slices > 1 && colors_have_alpha(cols))) {
    return(0)
  }
  max(merge_length * 0.02, 1e-4)
}

# band: path distances where the strand leaves its start trunk and enters its end trunk. Colors
# are positioned between them, so a strand shows exactly its node color where it meets the
# opaque trunk instead of stepping to a color part-way along the path.
flow_gradient_color <- function(sankey, flow_cols, node_cols, flow_ind, distance,
                                mid_cols = NULL,
                                merge_length, gradient_mode = c("merge", "path"),
                                band = c(0, sankey$path_lengths[flow_ind])) {
  gradient_mode <- match.arg(gradient_mode)
  thread_col <- if(is.null(mid_cols)) flow_cols[flow_ind] else mid_cols[flow_ind]
  span <- band[2] - band[1]
  if(sankey$path_lengths[flow_ind] <= 0 || span <= 0) {
    return(if(is.na(thread_col)) node_cols$start[flow_ind] else thread_col)
  }
  t <- min(max((distance - band[1]) / span, 0), 1)

  if(gradient_mode == "path") {
    if(is.na(thread_col)) {
      return(interpolate_color(node_cols$start[flow_ind], node_cols$end[flow_ind], t))
    }
    if(t <= 0.5) {
      return(interpolate_color(node_cols$start[flow_ind], thread_col, t * 2))
    }
    return(interpolate_color(thread_col, node_cols$end[flow_ind], (t - 0.5) * 2))
  }

  if(is.na(thread_col)) {
    return(interpolate_color(node_cols$start[flow_ind], node_cols$end[flow_ind], t))
  }
  if(merge_length <= 0) {
    return(thread_col)
  }
  if(distance - band[1] <= merge_length) {
    return(interpolate_color(node_cols$start[flow_ind], thread_col, (distance - band[1]) / merge_length))
  }
  if(band[2] - distance <= merge_length) {
    return(interpolate_color(thread_col, node_cols$end[flow_ind], 1 - (band[2] - distance) / merge_length))
  }
  thread_col
}

draw_edge_segment <- function(edge, centerline, start_skip, end_skip, col, lwd, lty, xyr) {
  diffs <- centerline[-1, , drop = FALSE] - centerline[-nrow(centerline), , drop = FALSE]
  d <- c(0, cumsum(sqrt(diffs[, "x"]^2 + (diffs[, "y"] * xyr)^2)))
  d0 <- start_skip
  d1 <- max(d) - end_skip
  if(d1 <= d0) {
    return(invisible(NULL))
  }

  seg <- path_segment_by_distance(edge, d, d0, d1)
  if(nrow(seg) >= 2) {
    lines(seg[, "x"], seg[, "y"], col = col, lwd = lwd, lty = lty, lend = "butt", xpd = NA)
  }

  invisible(NULL)
}

point_at_distance <- function(coords, distances, target) {
  target <- min(max(target, min(distances)), max(distances))
  if(target %in% distances) {
    return(coords[which(distances == target)[1], , drop = FALSE])
  }

  i <- max(which(distances < target))
  prop <- (target - distances[i]) / (distances[i + 1] - distances[i])
  out <- coords[i, , drop = FALSE] + prop * (coords[i + 1, , drop = FALSE] - coords[i, , drop = FALSE])
  colnames(out) <- colnames(coords)
  out
}

distance_at_x <- function(centerline, distances, target_x) {
  x <- centerline[, "x"]
  target_x <- min(max(target_x, min(x)), max(x))
  if(abs(target_x - x[1]) < 1e-12) {
    return(distances[1])
  }
  if(abs(target_x - x[length(x)]) < 1e-12) {
    return(distances[length(distances)])
  }
  if(target_x %in% x) {
    return(distances[which(x == target_x)[1]])
  }

  if(x[length(x)] >= x[1]) {
    i <- max(which(x < target_x))
  } else {
    i <- max(which(x > target_x))
  }
  prop <- (target_x - x[i]) / (x[i + 1] - x[i])
  distances[i] + prop * (distances[i + 1] - distances[i])
}

point_at_x <- function(coords, target_x) {
  x <- coords[, "x"]
  target_x <- min(max(target_x, min(x)), max(x))
  if(abs(target_x - x[1]) < 1e-12) {
    return(coords[1, , drop = FALSE])
  }
  if(abs(target_x - x[length(x)]) < 1e-12) {
    return(coords[length(x), , drop = FALSE])
  }
  if(target_x %in% x) {
    return(coords[which(x == target_x)[1], , drop = FALSE])
  }

  if(x[length(x)] >= x[1]) {
    i <- max(which(x < target_x))
  } else {
    i <- max(which(x > target_x))
  }
  prop <- (target_x - x[i]) / (x[i + 1] - x[i])
  out <- coords[i, , drop = FALSE] + prop * (coords[i + 1, , drop = FALSE] - coords[i, , drop = FALSE])
  colnames(out) <- colnames(coords)
  out
}

path_segment_by_x <- function(coords, start_x, end_x) {
  increasing <- coords[nrow(coords), "x"] >= coords[1, "x"]
  lo <- min(start_x, end_x)
  hi <- max(start_x, end_x)
  interior <- which(coords[, "x"] > lo & coords[, "x"] < hi)

  if(increasing) {
    out <- rbind(
      point_at_x(coords, lo),
      coords[interior, , drop = FALSE],
      point_at_x(coords, hi)
    )
  } else {
    out <- rbind(
      point_at_x(coords, hi),
      coords[interior, , drop = FALSE],
      point_at_x(coords, lo)
    )
  }
  colnames(out) <- colnames(coords)
  out
}

ribbon_slice_polygon <- function(sankey, flow_ind, start, end) {
  centerline <- sankey$centerlines[[flow_ind]]
  distances <- edge_distances(centerline, sankey$xyr)
  edges <- sankey_centerline_edges(sankey, flow_ind)
  start <- min(max(start, 0), max(distances))
  end <- min(max(end, 0), max(distances))
  if(end <= start) {
    return(NULL)
  }

  left <- path_segment_by_distance(edges$left, distances, start, end)
  right <- path_segment_by_distance(edges$right, distances, start, end)
  coords <- rbind(left, right[nrow(right):1, , drop = FALSE], left[1, , drop = FALSE])
  colnames(coords) <- c("x", "y")
  coords
}

flow_x_cuts <- function(sankey, flow_ind, merge_length, fill_overlap = 0) {
  x0 <- sankey$flows$start_x[flow_ind]
  x1 <- sankey$flows$end_x[flow_ind]
  direction <- sign(x1 - x0)
  if(direction == 0 || merge_length <= 0) {
    return(sort(c(start = x0, end = x1)))
  }

  cut <- max(merge_length - fill_overlap, 0)
  cuts <- c(
    start = x0 + direction * cut,
    end = x1 - direction * cut
  )
  if(direction > 0) cuts else rev(cuts)
}

ribbon_slice_polygon_by_x <- function(sankey, flow_ind, start_x, end_x) {
  edges <- sankey_centerline_edges(sankey, flow_ind)
  left <- path_segment_by_x(edges$left, start_x, end_x)
  right <- path_segment_by_x(edges$right, start_x, end_x)
  if(nrow(left) < 2 || nrow(right) < 2) {
    return(NULL)
  }

  coords <- rbind(left, right[nrow(right):1, , drop = FALSE], left[1, , drop = FALSE])
  colnames(coords) <- c("x", "y")
  coords
}

draw_sankey_fill_layer <- function(sankey, fill_cols, combine = TRUE,
                                   gradient_slices = 1,
                                   merge_length = sankey$merge_length,
                                   fill_overlap = 0,
                                   gradient_mode = c("merge", "path"),
                                   fill_border = NA,
                                   trunk_start = TRUE,
                                   trunk_end = TRUE,
                                   flatten_trunks = TRUE,
                                   trunk_bg = par("bg"),
                                   merge_cols = NULL,
                                   endpoint_cols = NULL,
                                   mid_cols = NULL, ...) {
  gradient_mode <- match.arg(gradient_mode)
  validate_merge_cols(sankey, merge_cols)
  validate_merge_cols(sankey, endpoint_cols)
  if(!is.null(mid_cols) && length(mid_cols) != nrow(sankey$flows)) {
    stop("`mid_cols` must be NULL or one color per positive flow.", call. = FALSE)
  }
  if(!is.numeric(gradient_slices) || length(gradient_slices) != 1 ||
     !is.finite(gradient_slices) || gradient_slices < 1) {
    stop("`gradient_slices` must be a single finite number >= 1.", call. = FALSE)
  }
  gradient_slices <- as.integer(round(gradient_slices))
  # Strand gradients run between node (endpoint) colors; merge_cols stand in when none are given.
  # Trunks use the same colors at their inner edges, so strands meet them without a color step.
  gradient_node_cols <- if(gradient_slices == 1) {
    NULL
  } else if(!is.null(endpoint_cols)) {
    endpoint_cols
  } else if(is.null(merge_cols)) {
    flow_node_colors(sankey, fill_cols)
  } else {
    merge_cols
  }

  if(gradient_slices == 1) {
    if(merge_length <= 0 && combine && length(unique(fill_cols)) == 1 && length(fill_border) == 1) {
      coords <- do.call(rbind, lapply(sankey$ribbons, function(x) rbind(x, c(NA, NA))))
      polygon(coords[, "x"], coords[, "y"], col = fill_cols[1], border = fill_border, xpd = NA, ...)
    } else {
      for(k in seq_along(sankey$ribbons)) {
        if(merge_length > 0 && sankey$path_lengths[k] > 2 * merge_length) {
          xcuts <- flow_x_cuts(sankey, k, merge_length, fill_overlap)
          ribbon <- ribbon_slice_polygon_by_x(sankey, k, xcuts["start"], xcuts["end"])
        } else {
          ribbon <- sankey$ribbons[[k]]
        }
        if(is.null(ribbon)) {
          next
        }
        polygon(
          ribbon[, "x"], ribbon[, "y"],
          col = fill_cols[k],
          border = if(length(fill_border) == 1) fill_border else fill_border[k],
          xpd = NA, ...
        )
      }
    }
  } else {
    for(k in seq_along(sankey$centerlines)) {
      centerline <- sankey$centerlines[[k]]
      distances <- edge_distances(centerline, sankey$xyr)
      band <- c(0, sankey$path_lengths[k])
      if(merge_length > 0 && sankey$path_lengths[k] > 2 * merge_length) {
        xcuts <- flow_x_cuts(sankey, k, merge_length, fill_overlap)
        # The trunks reach merge_length + fill_overlap in from each node face.
        band_x <- flow_x_cuts(sankey, k, merge_length + fill_overlap)
        band <- sort(c(
          distance_at_x(centerline, distances, band_x[["start"]]),
          distance_at_x(centerline, distances, band_x[["end"]])
        ))
      } else {
        xcuts <- sort(c(
          start = sankey$flows$start_x[k],
          end = sankey$flows$end_x[k]
        ))
      }
      breaks_x <- seq(xcuts["start"], xcuts["end"], length.out = gradient_slices + 1)
      if(identical(gradient_mode, "merge") && merge_length > 0 && band[1] > 0) {
        # Merge-mode blends span one merge length past each band edge; give them their own slices.
        dir <- sign(sankey$flows$end_x[k] - sankey$flows$start_x[k])
        zone <- function(x0, toward) seq(x0, x0 + toward * merge_length, length.out = 17)
        extra <- c(zone(band_x[["start"]], dir), zone(band_x[["end"]], -dir))
        breaks_x <- sort(unique(c(breaks_x, extra[extra >= min(xcuts) & extra <= max(xcuts)])))
      }
      for(i in seq_len(length(breaks_x) - 1)) {
        coords <- ribbon_slice_polygon_by_x(sankey, k, breaks_x[i], breaks_x[i + 1])
        if(is.null(coords)) {
          next
        }
        # Slices are evenly spaced in x, so color each by the path distance at its x midpoint.
        mid <- distance_at_x(centerline, distances, mean(breaks_x[i:(i + 1)]))
        polygon(
          coords[, "x"], coords[, "y"],
          col = flow_gradient_color(
            sankey,
            fill_cols,
            gradient_node_cols,
            k,
            mid,
            mid_cols = mid_cols,
            merge_length = merge_length,
            gradient_mode = gradient_mode,
            band = band
          ),
          border = NA,
          xpd = NA,
          ...
        )
      }
    }
  }

  if(merge_length > 0) {
    draw_endpoint_node_bodies(
      sankey,
      flow_cols = fill_cols,
      merge_cols = merge_cols,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      gradient_slices = gradient_slices,
      merge_length = merge_length
    )
    draw_node_trunk_fills(
      sankey,
      flow_cols = fill_cols,
      merge_length = merge_length + fill_overlap,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols,
      endpoint_cols = gradient_node_cols,
      gradient_slices = gradient_slices
    )
    draw_graph_interior_trunks(
      sankey,
      flow_cols = fill_cols,
      merge_cols = merge_cols,
      endpoint_cols = gradient_node_cols,
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      gradient_slices = gradient_slices
    )
  }
}

path_segment_by_distance <- function(coords, distances, start, end) {
  if(end <= start) {
    return(coords[integer(0), , drop = FALSE])
  }

  interior <- which(distances > start & distances < end)
  out <- rbind(
    point_at_distance(coords, distances, start),
    coords[interior, , drop = FALSE],
    point_at_distance(coords, distances, end)
  )
  colnames(out) <- colnames(coords)
  out
}

node_trunk_coords <- function(sankey, node_x, node_y, total_width, direction,
                              outward, merge_length) {
  tangent <- c(direction, 0)
  p0 <- apply_visual_normal_offset(node_x, node_y, -total_width / 2, tangent, xyr = sankey$xyr)
  p1 <- apply_visual_normal_offset(node_x, node_y, total_width / 2, tangent, xyr = sankey$xyr)
  p0_out <- p0 + c(outward * direction * merge_length, 0)
  p1_out <- p1 + c(outward * direction * merge_length, 0)
  coords <- rbind(p0, p0_out, p1_out, p1, p0)
  colnames(coords) <- c("x", "y")
  coords
}

endpoint_edge_segment <- function(sankey, flow_ind, edge_name, side, merge_length) {
  centerline <- sankey$centerlines[[flow_ind]]
  distances <- edge_distances(centerline, sankey$xyr)
  edges <- sankey_centerline_edges(sankey, flow_ind)
  edge <- edges[[edge_name]]
  total <- max(distances)
  dx <- sankey$flows$end_x[flow_ind] - sankey$flows$start_x[flow_ind]
  direction <- sign(dx)
  if(direction == 0) {
    return(edge[integer(0), , drop = FALSE])
  }

  if(side == "start") {
    target_x <- sankey$flows$start_x[flow_ind] + direction * merge_length
    path_segment_by_x(edge, edge[1, "x"], target_x)
  } else if(side == "end") {
    target_x <- sankey$flows$end_x[flow_ind] - direction * merge_length
    path_segment_by_x(edge, target_x, edge[nrow(edge), "x"])
  } else {
    stop("`side` must be 'start' or 'end'.", call. = FALSE)
  }
}

node_trunk_polygon <- function(sankey, flow_inds, side, merge_length) {
  candidates <- endpoint_candidates(sankey, flow_inds, side)
  lower <- candidates[which.min(candidates$y), ]
  upper <- candidates[which.max(candidates$y), ]
  lower_seg <- endpoint_edge_segment(sankey, lower$flow, lower$edge, side, merge_length)
  upper_seg <- endpoint_edge_segment(sankey, upper$flow, upper$edge, side, merge_length)

  if(nrow(lower_seg) < 2 || nrow(upper_seg) < 2) {
    return(NULL)
  }

  coords <- rbind(
    lower_seg,
    upper_seg[nrow(upper_seg):1, , drop = FALSE],
    lower_seg[1, , drop = FALSE]
  )
  colnames(coords) <- c("x", "y")
  # The envelope supplies gradient bounds; only the individual ribbon bands are filled.
  face <- if(side == "start") sankey$flows$start_x[flow_inds[1]] else sankey$flows$end_x[flow_inds[1]]
  direction <- sign(sankey$flows$end_x[flow_inds[1]] - sankey$flows$start_x[flow_inds[1]])
  target <- face + if(side == "start") direction * merge_length else -direction * merge_length
  attr(coords, "fill_polygons") <- Filter(Negate(is.null), lapply(flow_inds, function(k) {
    clip_polygon_x(sankey$ribbons[[k]], face, target)
  }))
  coords
}

node_y_bounds_for_height <- function(node, lower_y, upper_y,
                                     height_col = "node_body_height",
                                     include_flow = TRUE) {
  if(!height_col %in% names(node) ||
     !is.finite(node[[height_col]][1]) ||
     node[[height_col]][1] <= 0) {
    return(c(lower = lower_y, upper = upper_y))
  }
  half_height <- node[[height_col]][1] / 2
  bounds <- c(lower = node$y[1] - half_height, upper = node$y[1] + half_height)
  if(isTRUE(include_flow)) {
    bounds <- c(lower = min(lower_y, bounds["lower"]), upper = max(upper_y, bounds["upper"]))
  }
  bounds
}

node_body_y_bounds <- function(node, lower_y, upper_y) {
  node_y_bounds_for_height(node, lower_y, upper_y, "node_body_height", include_flow = TRUE)
}

node_label_border_y_bounds <- function(node, lower_y, upper_y) {
  node_body_y_bounds(node, lower_y, upper_y)
}

node_label_half_width <- function(node) {
  if(!"node_label_width" %in% names(node) ||
     !is.finite(node$node_label_width[1]) ||
     node$node_label_width[1] <= 0) {
    return(0)
  }
  min(node$node_label_width[1] / 2, node$node_width[1] / 2)
}

node_label_body_half_width <- function(node) {
  text_half <- node_label_half_width(node)
  if(text_half <= 0 ||
     !"node_width" %in% names(node) ||
     !is.finite(node$node_width[1]) ||
     node$node_width[1] <= 0) {
    return(text_half)
  }
  node$node_width[1] / 2
}

interior_node_row <- function(prev_segment, next_segment, node) {
  if(inherits(prev_segment, "sankey_graph") && !is.null(prev_segment$nodes)) {
    out <- prev_segment$nodes[prev_segment$nodes$id == node, , drop = FALSE]
    if(nrow(out) == 1) return(out)
  }
  if(!is.null(prev_segment$to_nodes)) {
    out <- prev_segment$to_nodes[prev_segment$to_nodes$node == node, , drop = FALSE]
    if(nrow(out) == 1) return(out)
  }
  if(!is.null(next_segment$from_nodes)) {
    out <- next_segment$from_nodes[next_segment$from_nodes$node == node, , drop = FALSE]
    if(nrow(out) == 1) return(out)
  }
  NULL
}

node_width_bridge_curve_body <- function(p0, p1, node = NULL, side = c("lower", "upper"),
                                         n = 16, height_col = "node_body_height") {
  side <- match.arg(side)
  if(!is.null(node) &&
     (!height_col %in% names(node) ||
      !is.finite(node[[height_col]][1]) ||
      node[[height_col]][1] <= 0) &&
     !identical(height_col, "node_body_height")) {
    height_col <- "node_body_height"
  }
  if(is.null(node) ||
     !height_col %in% names(node) ||
     !is.finite(node[[height_col]][1]) ||
     node[[height_col]][1] <= 0) {
    return(node_width_bridge_curve(p0, p1, n = n))
  }
  y_mid <- node$y[1] + if(identical(side, "lower")) {
    -node[[height_col]][1] / 2
  } else {
    node[[height_col]][1] / 2
  }
  x0 <- min(p0["x"], p1["x"])
  x1 <- max(p0["x"], p1["x"])
  # Leave horizontal space for the curved shoulders instead of stepping vertically at a face.
  label_half_width <- min(node_label_body_half_width(node), (x1 - x0) / 4)
  left_x <- min(max(node$x[1] - label_half_width, x0), x1)
  right_x <- min(max(node$x[1] + label_half_width, x0), x1)
  p_left <- c(x = left_x, y = y_mid)
  p_right <- c(x = right_x, y = y_mid)
  if(p0["x"] > p1["x"]) {
    tmp <- p_left
    p_left <- p_right
    p_right <- tmp
  }
  n0 <- max(16L, ceiling(n / 3))
  n1 <- max(2L, ceiling(n / 3))
  n2 <- max(16L, n - n0 - n1 + 2L)
  rbind(
    node_width_bridge_curve(p0, p_left, n = n0),
    line_between(p_left, p_right, n = n1)[-1, , drop = FALSE],
    node_width_bridge_curve(p_right, p1, n = n2)[-1, , drop = FALSE]
  )
}

# overlap (x units) pushes the fill polygon's face edge under the merge trunk, so the two
# antialiased edges do not leave a light seam at the node face; borders stay at the face.
endpoint_node_body_polygon <- function(sankey, node, flow_inds, side, overlap = 0) {
  if(is.null(node$node_width) || !is.finite(node$node_width) || node$node_width <= 0) {
    return(NULL)
  }
  candidates <- endpoint_candidates(sankey, flow_inds, side)
  lower <- candidates[which.min(candidates$y), ]
  upper <- candidates[which.max(candidates$y), ]
  directions <- sign(sankey$flows$end_x[flow_inds] - sankey$flows$start_x[flow_inds])
  directions <- unique(directions[directions != 0])
  if(length(directions) != 1) {
    return(NULL)
  }
  direction <- directions[1]
  outer_x <- if(identical(side, "start")) {
    node$x - direction * node$node_width / 2
  } else {
    node$x + direction * node$node_width / 2
  }
  outer_y <- node_body_y_bounds(node, lower$y, upper$y)
  border_y <- node_label_border_y_bounds(node, lower$y, upper$y)
  face_x <- lower$x
  label_half_width <- if("node_body_height" %in% names(node) &&
                         is.finite(node$node_body_height[1]) &&
                         node$node_body_height[1] > 0) {
    min(node_label_body_half_width(node), node$node_width[1] / 4)
  } else {
    node$node_width[1] / 2
  }
  transition_x <- if(face_x > node$x) {
    node$x + label_half_width
  } else {
    node$x - label_half_width
  }
  transition_x <- min(max(transition_x, min(face_x, outer_x)), max(face_x, outer_x))
  fill_face_x <- face_x + sign(face_x - outer_x) * max(overlap, 0)

  fill_lower <- rbind(node_width_bridge_curve(
    c(x = face_x, y = lower$y), c(x = transition_x, y = outer_y["lower"]), n = 16),
    c(x = outer_x, y = outer_y["lower"]))
  fill_upper <- rbind(node_width_bridge_curve(
    c(x = face_x, y = upper$y), c(x = transition_x, y = outer_y["upper"]), n = 16),
    c(x = outer_x, y = outer_y["upper"]))
  coords <- rbind(c(x = fill_face_x, y = lower$y), fill_lower,
                  fill_upper[nrow(fill_upper):1, , drop = FALSE],
                  c(x = fill_face_x, y = upper$y), c(x = fill_face_x, y = lower$y))
  colnames(coords) <- c("x", "y")
  lower_transition <- node_width_bridge_curve(
    c(x = face_x, y = lower$y),
    c(x = transition_x, y = border_y["lower"]),
    n = 16
  )
  upper_transition <- node_width_bridge_curve(
    c(x = face_x, y = upper$y),
    c(x = transition_x, y = border_y["upper"]),
    n = 16
  )
  lower_border <- rbind(
    lower_transition,
    c(x = outer_x, y = border_y["lower"])
  )
  upper_border <- rbind(
    upper_transition,
    c(x = outer_x, y = border_y["upper"])
  )
  colnames(lower_border) <- colnames(upper_border) <- c("x", "y")
  attr(coords, "face_x") <- face_x
  attr(coords, "outer_x") <- outer_x
  attr(coords, "lower_border") <- lower_border
  attr(coords, "upper_border") <- upper_border
  coords
}

node_width_bridge_curve <- function(p0, p1, n = 16) {
  p0 <- as.numeric(p0)
  p1 <- as.numeric(p1)
  if(length(p0) != 2 || length(p1) != 2 || any(!is.finite(c(p0, p1)))) {
    stop("Node bridge endpoints must be finite x/y pairs.", call. = FALSE)
  }
  if(max(abs(p1 - p0)) <= 1e-12) {
    out <- matrix(p0, nrow = 1)
    colnames(out) <- c("x", "y")
    return(out)
  }
  # Match the browser's subpixel cosine sampling (R coordinates are plot units).
  y_per_pixel <- if(grDevices::dev.cur() > 1) abs(diff(par("usr")[3:4])) / (par("pin")[2] * 96) else 1 / 96
  n <- max(n, 2 * ceiling(sqrt(abs(p1[2] - p0[2]) / y_per_pixel * pi^2 / 1.6) / 2) + 1)
  t <- seq(0, 1, length.out = n)
  # Cosine easing gives horizontal tangents at each end without the long
  # straight-looking middle segment caused by sharper logistic/shoulder eases.
  ease <- 0.5 - 0.5 * cos(pi * t)
  out <- cbind(
    x = p0[1] * (1 - t) + p1[1] * t,
    y = p0[2] * (1 - ease) + p1[2] * ease
  )
  colnames(out) <- c("x", "y")
  out
}

draw_endpoint_node_bodies <- function(sankey, flow_cols, merge_cols,
                                      trunk_start = TRUE, trunk_end = TRUE,
                                      flatten_trunks = TRUE,
                                      trunk_bg = par("bg"),
                                      gradient_slices = 24,
                                      merge_length = 0) {
  # Opaque trunks hide a sliver of body pushed under them (about 0.02 inch), removing the seam.
  overlap <- if(isTRUE(flatten_trunks) && merge_length > 0) {
    min(0.02 * diff(par("usr")[1:2]) / par("pin")[1], merge_length)
  } else {
    0
  }
  node_flow_id <- function(nodes, i) {
    if(inherits(sankey, "sankey_graph") && "id" %in% names(nodes)) nodes$id[i] else nodes$node[i]
  }
  is_graph_internal <- function(node_id) {
    inherits(sankey, "sankey_graph") &&
      sankey$incoming[node_id] > 0 && sankey$outgoing[node_id] > 0
  }
  draw_one_side <- function(nodes, node_col, side, merge_side) {
    for(i in seq_len(nrow(nodes))) {
      node_id <- node_flow_id(nodes, i)
      if(is_graph_internal(node_id)) {
        next
      }
      flow_inds <- which(sankey$flows[[node_col]] == node_id)
      if(length(flow_inds) == 0) {
        next
      }
      coords <- endpoint_node_body_polygon(sankey, nodes[i, ], flow_inds, side, overlap = overlap)
      if(is.null(coords)) {
        next
      }
      # The trunk meets the body at the node face in its merge color, so the body continues it.
      body_col <- if(is.null(merge_cols)) {
        node_merge_color(sankey, flow_cols, flow_inds)
      } else {
        merge_cols[[merge_side]][flow_inds[1]]
      }
      if(node_has_user_label(nodes[i, ])) {
        x_rng <- range(coords[, "x"])
        face_x <- attr(coords, "face_x", exact = TRUE)
        face_is_low <- face_x <= mean(x_rng)
        midpoint_col <- transparent_color(body_col)
        draw_x_color_stops_polygon(
          coords,
          c(x_rng[1], nodes$x[i], x_rng[2]),
          if(face_is_low) {
            c(body_col, midpoint_col, midpoint_col)
          } else {
            c(midpoint_col, midpoint_col, body_col)
          },
          steps = max(24L, gradient_slices),
          flatten = flatten_trunks,
          bg = trunk_bg
        )
      } else {
        col <- body_col
        if(flatten_trunks) {
          col <- flatten_alpha_color(col, bg = trunk_bg)
        }
        draw_sankey_polygon(coords, col)
      }
    }
  }

  if(trunk_start) {
    draw_one_side(sankey$from_nodes, "from", side = "start", merge_side = "start")
  }
  if(trunk_end) {
    draw_one_side(sankey$to_nodes, "to", side = "end", merge_side = "end")
  }

  invisible(NULL)
}

draw_endpoint_node_body_borders <- function(sankey, col, flow_cols = NULL,
                                            lwd = 1, lty = 1,
                                            steps = 24,
                                            fade = c("background", "alpha"),
                                            shape = c("polyline", "taper", "line"),
                                            bg = par("bg"),
                                            edge_border = FALSE,
                                            trunk_start = TRUE,
                                            trunk_end = TRUE) {
  fade <- match.arg(fade)
  shape <- match.arg(shape)
  if(!validate_node_border_args(col, lwd, lty, steps)) {
    return(invisible(NULL))
  }
  steps <- as.integer(round(steps))
  node_flow_id <- function(nodes, i) {
    if(inherits(sankey, "sankey_graph") && "id" %in% names(nodes)) nodes$id[i] else nodes$node[i]
  }
  is_graph_internal <- function(node_id) {
    inherits(sankey, "sankey_graph") &&
      sankey$incoming[node_id] > 0 && sankey$outgoing[node_id] > 0
  }
  draw_one_side <- function(nodes, node_col, side) {
    for(i in seq_len(nrow(nodes))) {
      node_id <- node_flow_id(nodes, i)
      if(is_graph_internal(node_id)) {
        next
      }
      flow_inds <- which(sankey$flows[[node_col]] == node_id)
      if(length(flow_inds) == 0) {
        next
      }
      coords <- endpoint_node_body_polygon(sankey, nodes[i, ], flow_inds, side)
      if(is.null(coords)) {
        next
      }
      border_col <- col
      if(is.null(border_col)) {
        if(is.null(flow_cols) || !node_has_user_label(nodes[i, ])) {
          next
        }
        border_col <- opaque_color(node_merge_color(sankey, flow_cols, flow_inds))
      }
      lower <- attr(coords, "lower_border")
      upper <- attr(coords, "upper_border")
      if(is.null(lower) || is.null(upper)) {
        lower <- line_between(coords[2, ], coords[1, ], steps)
        upper <- line_between(coords[3, ], coords[4, ], steps)
      }
      lower <- sample_polyline(lower, steps)
      upper <- sample_polyline(upper, steps)
      if(isTRUE(edge_border)) {
        lower_alpha <- node_body_border_alpha(lower, nodes[i, ], direction = "face")
        upper_alpha <- node_body_border_alpha(upper, nodes[i, ], direction = "face")
      } else {
        lower_alpha <- upper_alpha <- function(t) sin(pi * t)
      }
      draw_faded_polyline(lower, border_col, lwd, lty, lower_alpha,
                          fade = fade, shape = shape, bg = bg)
      draw_faded_polyline(upper, border_col, lwd, lty, upper_alpha,
                          fade = fade, shape = shape, bg = bg)
    }
  }

  if(trunk_start) {
    draw_one_side(sankey$from_nodes, "from", side = "start")
  }
  if(trunk_end) {
    draw_one_side(sankey$to_nodes, "to", side = "end")
  }
  invisible(NULL)
}

draw_node_trunk_fills <- function(sankey, flow_cols, merge_length,
                                  trunk_start = TRUE, trunk_end = TRUE,
                                  flatten_trunks = TRUE,
                                  trunk_bg = par("bg"),
                                  merge_cols = NULL,
                                  endpoint_cols = NULL,
                                  gradient_slices = 24) {
  validate_merge_cols(sankey, merge_cols)
  validate_merge_cols(sankey, endpoint_cols)
  node_flow_id <- function(nodes, i) {
    if(inherits(sankey, "sankey_graph") && "id" %in% names(nodes)) nodes$id[i] else nodes$node[i]
  }
  is_graph_internal <- function(node_id) {
    inherits(sankey, "sankey_graph") &&
      sankey$incoming[node_id] > 0 && sankey$outgoing[node_id] > 0
  }
  draw_one_side <- function(nodes, node_col, side, merge_side) {
    for(i in seq_len(nrow(nodes))) {
      node_id <- node_flow_id(nodes, i)
      if(is_graph_internal(node_id)) {
        next
      }
      flow_inds <- which(sankey$flows[[node_col]] == node_id)
      if(length(flow_inds) == 0) {
        next
      }

      coords <- node_trunk_polygon(sankey, flow_inds, side, merge_length)
      if(is.null(coords)) {
        next
      }
      endpoint_col <- if(is.null(merge_cols)) {
        node_merge_color(sankey, flow_cols, flow_inds)
      } else {
        merge_cols[[merge_side]][flow_inds[1]]
      }
      # With gradients, strands show their endpoint (node) color where they meet the trunk's inner
      # edge, so the trunk blends to that color there; solid strands meet their average color.
      inner_col <- if(!is.null(endpoint_cols) && gradient_slices > 1) {
        endpoint_cols[[merge_side]][flow_inds[1]]
      } else {
        node_merge_color(sankey, flow_cols, flow_inds)
      }
      directions <- sign(sankey$flows$end_x[flow_inds] - sankey$flows$start_x[flow_inds])
      directions <- unique(directions[directions != 0])
      if(length(directions) != 1) {
        next
      }
      endpoint_x <- if(identical(side, "start")) {
        sankey$flows$start_x[flow_inds[1]]
      } else {
        sankey$flows$end_x[flow_inds[1]]
      }
      interior_x <- endpoint_x + if(identical(side, "start")) {
        directions[1] * merge_length
      } else {
        -directions[1] * merge_length
      }
      low_col <- if(endpoint_x < interior_x) endpoint_col else inner_col
      high_col <- if(endpoint_x < interior_x) inner_col else endpoint_col
      if(gradient_slices > 1 || !identical(endpoint_col, inner_col)) {
        draw_x_gradient_polygon(
          coords,
          low_col,
          high_col,
          steps = max(2L, gradient_slices),
          flatten = flatten_trunks,
          bg = trunk_bg
        )
      } else {
        col <- endpoint_col
        if(flatten_trunks) {
          col <- flatten_alpha_color(col, bg = trunk_bg)
        }
        draw_sankey_polygon(coords, col)
      }
    }
  }

  if(trunk_start) {
    draw_one_side(sankey$from_nodes, "from", side = "start", merge_side = "start")
  }
  if(trunk_end) {
    draw_one_side(sankey$to_nodes, "to", side = "end", merge_side = "end")
  }
}

interior_node_trunk_polygon <- function(prev_segment, next_segment, node, merge_length) {
  prev_inds <- which(prev_segment$flows$to == node)
  next_inds <- which(next_segment$flows$from == node)
  if(length(prev_inds) == 0 || length(next_inds) == 0) {
    return(NULL)
  }

  prev_candidates <- endpoint_candidates(prev_segment, prev_inds, side = "end")
  next_candidates <- endpoint_candidates(next_segment, next_inds, side = "start")
  prev_lower <- prev_candidates[which.min(prev_candidates$y), ]
  prev_upper <- prev_candidates[which.max(prev_candidates$y), ]
  next_lower <- next_candidates[which.min(next_candidates$y), ]
  next_upper <- next_candidates[which.max(next_candidates$y), ]

  prev_lower_seg <- endpoint_edge_segment(prev_segment, prev_lower$flow, prev_lower$edge, "end", merge_length)
  prev_upper_seg <- endpoint_edge_segment(prev_segment, prev_upper$flow, prev_upper$edge, "end", merge_length)
  next_lower_seg <- endpoint_edge_segment(next_segment, next_lower$flow, next_lower$edge, "start", merge_length)
  next_upper_seg <- endpoint_edge_segment(next_segment, next_upper$flow, next_upper$edge, "start", merge_length)

  if(any(c(nrow(prev_lower_seg), nrow(prev_upper_seg), nrow(next_lower_seg), nrow(next_upper_seg)) < 2)) {
    return(NULL)
  }

  node_row <- interior_node_row(prev_segment, next_segment, node)
  lower_bridge <- node_width_bridge_curve_body(
    prev_lower_seg[nrow(prev_lower_seg), ],
    next_lower_seg[1, ],
    node = node_row,
    side = "lower"
  )
  upper_bridge <- node_width_bridge_curve_body(
    prev_upper_seg[nrow(prev_upper_seg), ],
    next_upper_seg[1, ],
    node = node_row,
    side = "upper"
  )
  upper_bridge_back <- if(nrow(upper_bridge) > 1) {
    upper_bridge[(nrow(upper_bridge) - 1):1, , drop = FALSE]
  } else {
    upper_bridge[integer(0), , drop = FALSE]
  }

  coords <- rbind(
    prev_lower_seg,
    lower_bridge[-1, , drop = FALSE],
    next_lower_seg[-1, , drop = FALSE],
    next_upper_seg[nrow(next_upper_seg):1, , drop = FALSE],
    upper_bridge_back,
    prev_upper_seg[(nrow(prev_upper_seg) - 1):1, , drop = FALSE],
    prev_lower_seg[1, , drop = FALSE]
  )
  colnames(coords) <- c("x", "y")
  body <- rbind(lower_bridge, upper_bridge[nrow(upper_bridge):1, , drop = FALSE], lower_bridge[1, ])
  attr(coords, "fill_polygons") <- c(list(body),
    attr(node_trunk_polygon(prev_segment, prev_inds, "end", merge_length), "fill_polygons"),
    attr(node_trunk_polygon(next_segment, next_inds, "start", merge_length), "fill_polygons"))
  coords
}

draw_multi_stage_interior_trunks <- function(sankey, node_cols, merge_cols,
                                             merge_length, fill_overlap = 0,
                                             flatten_trunks = TRUE,
                                             trunk_bg = par("bg"),
                                             gradient_slices = 1,
                                             endpoint_cols = NULL) {
  if(length(sankey$segments) < 2 || merge_length <= 0) {
    return(invisible(NULL))
  }
  if(is.null(merge_cols) && is.null(node_cols)) {
    stop("Interior node trunks need `merge_cols` or `node_cols`.", call. = FALSE)
  }
  side_color <- function(cols, stage, end, segment, inds) {
    weighted_average_color(cols[[stage]][[end]][inds], segment$flows$width[inds])
  }

  draw_length <- merge_length + fill_overlap
  for(node_set in 2:length(sankey$segments)) {
    prev_segment <- sankey$segments[[node_set - 1]]
    next_segment <- sankey$segments[[node_set]]
    for(node in seq_len(sankey$node_counts[node_set])) {
      coords <- interior_node_trunk_polygon(prev_segment, next_segment, node, draw_length)
      if(is.null(coords)) {
        next
      }
      node_row <- interior_node_row(prev_segment, next_segment, node)
      prev_inds <- which(prev_segment$flows$to == node)
      next_inds <- which(next_segment$flows$from == node)
      # Trunks use merge colors; node colors only stand in when no merge colors exist.
      left_col <- if(!is.null(merge_cols)) {
        side_color(merge_cols, node_set - 1, "end", prev_segment, prev_inds)
      } else {
        node_cols[[node_set]][node]
      }
      right_col <- if(!is.null(merge_cols)) {
        side_color(merge_cols, node_set, "start", next_segment, next_inds)
      } else {
        node_cols[[node_set]][node]
      }
      middle_col <- transparent_color(weighted_average_color(
        c(left_col, right_col),
        c(sum(prev_segment$flows$width[prev_inds]), sum(next_segment$flows$width[next_inds]))
      ))
      # The fade to a transparent centre is a label body, so it needs a node with width.
      label_body <- !is.null(node_row) && node_has_user_label(node_row) && isTRUE(node_row$node_width[1] > 0)

      # Strands reach their endpoint (node) colors at the band edges; merge colors span the faces.
      in_col <- if(is.null(endpoint_cols)) left_col else side_color(endpoint_cols, node_set - 1, "end", prev_segment, prev_inds)
      out_col <- if(is.null(endpoint_cols)) right_col else side_color(endpoint_cols, node_set, "start", next_segment, next_inds)
      varies <- length(unique(c(in_col, left_col, right_col, out_col))) > 1

      if(gradient_slices > 1 && (varies || label_body)) {
        in_face <- prev_segment$flows$end_x[prev_inds[1]]
        out_face <- next_segment$flows$start_x[next_inds[1]]
        x_rng <- range(coords[, "x"])
        edges <- if(in_face <= out_face) x_rng else rev(x_rng)
        stops <- c(edges[1], in_face, out_face, edges[2])
        cols <- c(in_col, left_col, right_col, out_col)
        if(label_body) {
          stops <- c(stops, node_row$x[1])
          cols <- c(cols, middle_col)
        }
        draw_x_color_stops_polygon(coords, stops, cols, steps = max(24L, gradient_slices),
                                   flatten = flatten_trunks, bg = trunk_bg)
      } else if(label_body) {
        draw_x_color_stops_polygon(
          coords,
          c(min(coords[, "x"]), node_row$x[1], max(coords[, "x"])),
          c(left_col, middle_col, right_col),
          steps = 24L,
          flatten = flatten_trunks,
          bg = trunk_bg
        )
      } else {
        col <- left_col
        if(flatten_trunks) {
          col <- flatten_alpha_color(col, bg = trunk_bg)
        }
        draw_sankey_polygon(coords, col)
      }
    }
  }

  invisible(NULL)
}

draw_multi_stage_interior_node_borders <- function(sankey, col, node_cols = NULL,
                                                   lwd = 1, lty = 1, steps = 24,
                                                   fade = c("background", "alpha"),
                                                   shape = c("polyline", "taper", "line"),
                                                   bg = par("bg"),
                                                   edge_border = FALSE) {
  fade <- match.arg(fade)
  shape <- match.arg(shape)
  if(!validate_node_border_args(col, lwd, lty, steps)) {
    return(invisible(NULL))
  }
  if(length(sankey$node_counts) <= 2) {
    return(invisible(NULL))
  }
  steps <- as.integer(round(steps))
  for(node_set in 2:(length(sankey$node_counts) - 1)) {
    prev_segment <- sankey$segments[[node_set - 1]]
    next_segment <- sankey$segments[[node_set]]
    for(node in seq_len(sankey$node_counts[node_set])) {
      prev_inds <- which(prev_segment$flows$to == node)
      next_inds <- which(next_segment$flows$from == node)
      if(length(prev_inds) == 0 || length(next_inds) == 0) {
        next
      }
      prev_candidates <- endpoint_candidates(prev_segment, prev_inds, "end")
      next_candidates <- endpoint_candidates(next_segment, next_inds, "start")
      node_row <- interior_node_row(prev_segment, next_segment, node)
      border_col <- col
      if(is.null(border_col)) {
        if(is.null(node_cols) || is.null(node_row) || !node_has_user_label(node_row)) {
          next
        }
        border_col <- opaque_color(node_cols[[node_set]][node])
      }
      prev_lower <- prev_candidates[which.min(prev_candidates$y), c("x", "y")]
      prev_upper <- prev_candidates[which.max(prev_candidates$y), c("x", "y")]
      next_lower <- next_candidates[which.min(next_candidates$y), c("x", "y")]
      next_upper <- next_candidates[which.max(next_candidates$y), c("x", "y")]
      lower <- node_width_bridge_curve_body(
        prev_lower, next_lower,
        node = node_row,
        side = "lower",
        n = steps
      )
      upper <- node_width_bridge_curve_body(
        prev_upper, next_upper,
        node = node_row,
        side = "upper",
        n = steps
      )
      if(isTRUE(edge_border)) {
        lower_alpha <- node_body_border_alpha(lower, node_row, direction = "both")
        upper_alpha <- node_body_border_alpha(upper, node_row, direction = "both")
      } else {
        lower_alpha <- upper_alpha <- function(t) sin(pi * t)
      }
      draw_faded_polyline(lower, border_col, lwd, lty, lower_alpha,
                          fade = fade, shape = shape, bg = bg)
      draw_faded_polyline(upper, border_col, lwd, lty, upper_alpha,
                          fade = fade, shape = shape, bg = bg)
    }
  }
  invisible(NULL)
}

clip_polygon_x_one_side <- function(coords, x_cut, keep_above = TRUE) {
  inside <- function(p) if(keep_above) p["x"] >= x_cut else p["x"] <= x_cut
  intersect <- function(p0, p1) {
    if(abs(p1["x"] - p0["x"]) <= 1e-12) {
      return(p1)
    }
    t <- (x_cut - p0["x"]) / (p1["x"] - p0["x"])
    p0 + t * (p1 - p0)
  }

  if(nrow(coords) < 3) {
    return(coords[integer(0), , drop = FALSE])
  }
  if(max(abs(coords[1, ] - coords[nrow(coords), ])) <= 1e-12) {
    coords <- coords[-nrow(coords), , drop = FALSE]
  }

  out <- matrix(numeric(), ncol = 2)
  colnames(out) <- c("x", "y")
  for(i in seq_len(nrow(coords))) {
    current <- coords[i, ]
    previous <- coords[if(i == 1) nrow(coords) else i - 1, ]
    current_inside <- inside(current)
    previous_inside <- inside(previous)
    if(current_inside) {
      if(!previous_inside) {
        out <- rbind(out, intersect(previous, current))
      }
      out <- rbind(out, current)
    } else if(previous_inside) {
      out <- rbind(out, intersect(previous, current))
    }
  }
  if(nrow(out) > 0) {
    out <- rbind(out, out[1, ])
  }
  colnames(out) <- c("x", "y")
  out
}

clip_polygon_x <- function(coords, x0, x1) {
  lo <- min(x0, x1)
  hi <- max(x0, x1)
  out <- clip_polygon_x_one_side(coords, lo, keep_above = TRUE)
  out <- clip_polygon_x_one_side(out, hi, keep_above = FALSE)
  if(nrow(out) < 4) {
    return(NULL)
  }
  parts <- attr(coords, "fill_polygons")
  if(!is.null(parts)) attr(out, "fill_polygons") <- Filter(Negate(is.null), lapply(parts, clip_polygon_x, x0 = x0, x1 = x1))
  out
}

draw_graph_interior_trunks <- function(sankey, flow_cols, merge_cols,
                                       merge_length, fill_overlap = 0,
                                       flatten_trunks = TRUE,
                                       trunk_bg = par("bg"),
                                       gradient_slices = 1,
                                       endpoint_cols = NULL) {
  if(!inherits(sankey, "sankey_graph") || merge_length <= 0) {
    return(invisible(NULL))
  }

  draw_length <- merge_length + fill_overlap
  internal <- which(sankey$incoming > 0 & sankey$outgoing > 0)
  for(node in internal) {
    coords <- interior_node_trunk_polygon(sankey, sankey, node, draw_length)
    if(is.null(coords)) {
      next
    }

    in_inds <- which(sankey$flows$to == node)
    out_inds <- which(sankey$flows$from == node)
    left_face_col <- node_merge_color(sankey, flow_cols, in_inds)
    right_face_col <- node_merge_color(sankey, flow_cols, out_inds)
    left_col <- if(is.null(merge_cols)) {
      left_face_col
    } else {
      weighted_average_color(merge_cols$end[in_inds], sankey$flows$width[in_inds])
    }
    right_col <- if(is.null(merge_cols)) {
      right_face_col
    } else {
      weighted_average_color(merge_cols$start[out_inds], sankey$flows$width[out_inds])
    }

    if(gradient_slices > 1 && diff(range(coords[, "x"])) > 1e-12) {
      # Strands reach their endpoint (node) colors at the band edges; merge colors span the faces.
      in_col <- if(is.null(endpoint_cols)) left_col else weighted_average_color(endpoint_cols$end[in_inds], sankey$flows$width[in_inds])
      out_col <- if(is.null(endpoint_cols)) right_col else weighted_average_color(endpoint_cols$start[out_inds], sankey$flows$width[out_inds])
      in_face <- sankey$flows$end_x[in_inds[1]]
      out_face <- sankey$flows$start_x[out_inds[1]]
      x_rng <- range(coords[, "x"])
      edges <- if(in_face <= out_face) x_rng else rev(x_rng)
      stops <- c(edges[1], in_face, out_face, edges[2])
      cols <- c(in_col, left_col, right_col, out_col)
      node_row <- sankey$nodes[sankey$nodes$id == node, , drop = FALSE]
      # The fade to a transparent centre is a label body, so it needs a node with width.
      if(nrow(node_row) == 1 && node_has_user_label(node_row) && isTRUE(node_row$node_width[1] > 0)) {
        stops <- c(stops, node_row$x[1])
        cols <- c(cols, transparent_color(weighted_average_color(
          c(left_col, right_col),
          c(sum(sankey$flows$width[in_inds]), sum(sankey$flows$width[out_inds]))
        )))
      }
      draw_x_color_stops_polygon(coords, stops, cols, steps = max(24L, gradient_slices),
                                 flatten = flatten_trunks, bg = trunk_bg)
      next
    }

    cols <- c(left_col, right_col)
    weights <- c(sum(sankey$flows$width[in_inds]), sum(sankey$flows$width[out_inds]))
    col <- weighted_average_color(cols, weights)
    if(flatten_trunks) {
      col <- flatten_alpha_color(col, bg = trunk_bg)
    }
    draw_sankey_polygon(coords, col)
  }

  invisible(NULL)
}

draw_graph_interior_node_borders <- function(sankey, col, flow_cols = NULL,
                                             lwd = 1, lty = 1, steps = 24,
                                             fade = c("background", "alpha"),
                                             shape = c("polyline", "taper", "line"),
                                             bg = par("bg"),
                                             edge_border = FALSE) {
  fade <- match.arg(fade)
  shape <- match.arg(shape)
  if(!inherits(sankey, "sankey_graph") ||
     !validate_node_border_args(col, lwd, lty, steps)) {
    return(invisible(NULL))
  }
  steps <- as.integer(round(steps))
  internal <- which(sankey$incoming > 0 & sankey$outgoing > 0)
  for(node in internal) {
    in_inds <- which(sankey$flows$to == node)
    out_inds <- which(sankey$flows$from == node)
    if(length(in_inds) == 0 || length(out_inds) == 0) {
      next
    }
    in_candidates <- endpoint_candidates(sankey, in_inds, "end")
    out_candidates <- endpoint_candidates(sankey, out_inds, "start")
    in_lower <- in_candidates[which.min(in_candidates$y), c("x", "y")]
    in_upper <- in_candidates[which.max(in_candidates$y), c("x", "y")]
    out_lower <- out_candidates[which.min(out_candidates$y), c("x", "y")]
    out_upper <- out_candidates[which.max(out_candidates$y), c("x", "y")]
    node_row <- interior_node_row(sankey, sankey, node)
    border_col <- col
    if(is.null(border_col)) {
      if(is.null(flow_cols) || is.null(node_row) || !node_has_user_label(node_row)) {
        next
      }
      all_inds <- c(in_inds, out_inds)
      border_col <- opaque_color(node_merge_color(sankey, flow_cols, all_inds))
    }
    lower <- node_width_bridge_curve_body(
      in_lower, out_lower,
      node = node_row,
      side = "lower",
      n = steps
    )
    upper <- node_width_bridge_curve_body(
      in_upper, out_upper,
      node = node_row,
      side = "upper",
      n = steps
    )
    if(isTRUE(edge_border)) {
      lower_alpha <- node_body_border_alpha(lower, node_row, direction = "both")
      upper_alpha <- node_body_border_alpha(upper, node_row, direction = "both")
    } else {
      lower_alpha <- upper_alpha <- function(t) sin(pi * t)
    }
    draw_faded_polyline(lower, border_col, lwd, lty, lower_alpha,
                        fade = fade, shape = shape, bg = bg)
    draw_faded_polyline(upper, border_col, lwd, lty, upper_alpha,
                        fade = fade, shape = shape, bg = bg)
  }
  invisible(NULL)
}

edge_distances <- function(centerline, xyr) {
  diffs <- centerline[-1, , drop = FALSE] - centerline[-nrow(centerline), , drop = FALSE]
  c(0, cumsum(sqrt(diffs[, "x"]^2 + (diffs[, "y"] * xyr)^2)))
}

draw_endpoint_edge <- function(sankey, flow_ind, edge_name, side, merge_length, col, lwd, lty) {
  seg <- endpoint_edge_segment(sankey, flow_ind, edge_name, side, merge_length)

  if(nrow(seg) >= 2) {
    lines(seg[, "x"], seg[, "y"], col = col, lwd = lwd, lty = lty, lend = "butt", xpd = NA)
  }
  invisible(seg)
}

endpoint_candidates <- function(sankey, flow_inds, side) {
  out <- do.call(rbind, lapply(flow_inds, function(k) {
    centerline <- sankey$centerlines[[k]]
    edges <- sankey_centerline_edges(sankey, k)
    endpoint <- if(side == "start") 1 else nrow(centerline)
    rbind(
      data.frame(flow = k, edge = "left", x = edges$left[endpoint, "x"], y = edges$left[endpoint, "y"]),
      data.frame(flow = k, edge = "right", x = edges$right[endpoint, "x"], y = edges$right[endpoint, "y"])
    )
  }))
  rownames(out) <- NULL
  out
}

draw_node_outer_borders <- function(sankey, col, lwd, lty, merge_length,
                                    cap_start = TRUE, cap_end = TRUE) {
  node_flow_id <- function(nodes, i) {
    if(inherits(sankey, "sankey_graph") && "id" %in% names(nodes)) nodes$id[i] else nodes$node[i]
  }
  draw_one_side <- function(nodes, node_col, side, cap) {
    for(i in seq_len(nrow(nodes))) {
      flow_inds <- which(sankey$flows[[node_col]] == node_flow_id(nodes, i))
      if(length(flow_inds) == 0) {
        next
      }

      candidates <- endpoint_candidates(sankey, flow_inds, side)
      lower <- candidates[which.min(candidates$y), ]
      upper <- candidates[which.max(candidates$y), ]
      draw_endpoint_edge(sankey, lower$flow, lower$edge, side, merge_length, col, lwd, lty)
      draw_endpoint_edge(sankey, upper$flow, upper$edge, side, merge_length, col, lwd, lty)
      if(cap) {
        lines(c(lower$x, upper$x), c(lower$y, upper$y), col = col, lwd = lwd, lty = lty,
              lend = "butt", xpd = NA)
      }
    }
  }

  draw_one_side(sankey$from_nodes, "from", side = "start", cap = cap_start)
  draw_one_side(sankey$to_nodes, "to", side = "end", cap = cap_end)
}

draw_sankey_borders <- function(sankey, border, lwd = 1, lty = 1,
                                merge_length = sankey$merge_length,
                                cap_start = TRUE, cap_end = TRUE) {
  if(is.na(border)) {
    return(invisible(NULL))
  }

  for(k in seq_along(sankey$centerlines)) {
    centerline <- sankey$centerlines[[k]]
    edges <- sankey_centerline_edges(sankey, k)
    # Strands too short for both merge zones are filled whole, so border them whole.
    skip <- if(sankey$path_lengths[k] > 2 * merge_length) merge_length else 0
    draw_edge_segment(edges$left, centerline, skip, skip, border, lwd, lty, sankey$xyr)
    draw_edge_segment(edges$right, centerline, skip, skip, border, lwd, lty, sankey$xyr)
  }
  if(merge_length > 0) {
    draw_node_outer_borders(
      sankey,
      border,
      lwd,
      lty,
      merge_length,
      cap_start = cap_start,
      cap_end = cap_end
    )
  }

  invisible(NULL)
}

draw_node_sum_segments <- function(sankey, col = "#111111", lwd = 2,
                                   offset = NULL, adjust_xyr = TRUE) {
  if(is.null(offset)) {
    offset <- diff(range(c(sankey$from_nodes$x, sankey$to_nodes$x))) * 0.06
  }
  y_scale <- if(adjust_xyr) sankey$xyr else 1

  from_totals <- tapply(sankey$flows$width, sankey$flows$from, sum)
  to_totals <- tapply(sankey$flows$width, sankey$flows$to, sum)

  for(i in seq_len(nrow(sankey$from_nodes))) {
    node <- as.character(if(inherits(sankey, "sankey_graph") && "id" %in% names(sankey$from_nodes)) {
      sankey$from_nodes$id[i]
    } else {
      sankey$from_nodes$node[i]
    })
    total <- unname(from_totals[node])
    if(is.na(total)) {
      total <- 0
    }
    x <- sankey$from_nodes$x[i] - offset
    y <- sankey$from_nodes$y[i]
    segments(x, y - total / (2 * y_scale), x, y + total / (2 * y_scale),
             col = col, lwd = lwd, xpd = NA)
  }

  for(i in seq_len(nrow(sankey$to_nodes))) {
    node <- as.character(if(inherits(sankey, "sankey_graph") && "id" %in% names(sankey$to_nodes)) {
      sankey$to_nodes$id[i]
    } else {
      sankey$to_nodes$node[i]
    })
    total <- unname(to_totals[node])
    if(is.na(total)) {
      total <- 0
    }
    x <- sankey$to_nodes$x[i] + offset
    y <- sankey$to_nodes$y[i]
    segments(x, y - total / (2 * y_scale), x, y + total / (2 * y_scale),
             col = col, lwd = lwd, xpd = NA)
  }

  invisible(NULL)
}

plotting_device_extent <- function() {
  usr <- par("usr")
  plt <- par("plt")
  usr + c(
    diff(usr[1:2]) / diff(plt[1:2]) * (c(0, 1) - plt[1:2]),
    diff(usr[3:4]) / diff(plt[3:4]) * (c(0, 1) - plt[3:4])
  )
}

render_raster_fill <- function(draw_fill, raster_res = 96, interpolate = TRUE) {
  if(!requireNamespace("png", quietly = TRUE)) {
    stop("Package `png` is required for rasterized Sankey fills.", call. = FALSE)
  }

  gr_usr <- plotting_device_extent()
  tmp <- tempfile(fileext = ".png")
  main_dev <- grDevices::dev.cur()

  png_args <- list(
    filename = tmp,
    width = par("din")[1],
    height = par("din")[2],
    units = "in",
    res = raster_res,
    bg = "transparent"
  )
  if(capabilities("cairo")) {
    png_args$type <- "cairo"
  }
  do.call(grDevices::png, png_args)
  raster_dev <- grDevices::dev.cur()
  # dev.off() activates the next open device, which need not be the caller's.
  on.exit({
    if(raster_dev %in% grDevices::dev.list()) {
      grDevices::dev.off(raster_dev)
    }
    grDevices::dev.set(main_dev)
    unlink(tmp)
  }, add = TRUE)
  par(mar = c(0, 0, 0, 0), xpd = NA)
  plot.new()
  plot.window(xlim = gr_usr[1:2], ylim = gr_usr[3:4], xaxs = "i", yaxs = "i")
  draw_fill()
  grDevices::dev.off(raster_dev)
  grDevices::dev.set(main_dev)

  rasterImage(
    png::readPNG(tmp),
    gr_usr[1], gr_usr[3], gr_usr[2], gr_usr[4],
    interpolate = interpolate,
    xpd = NA
  )

  invisible(NULL)
}

draw_sankey_fill_raster <- function(sankey, fill_cols, combine, gradient_slices,
                                    merge_length, fill_overlap, gradient_mode,
                                    raster_res = 96,
                                    interpolate = TRUE,
                                    trunk_start = TRUE,
                                    trunk_end = TRUE,
                                    flatten_trunks = TRUE,
                                    trunk_bg = par("bg"),
                                    merge_cols = NULL,
                                    endpoint_cols = NULL,
                                    mid_cols = NULL, ...) {
  # Read the background from the calling device, not the temporary raster device.
  force(trunk_bg)
  render_raster_fill(function() {
    draw_sankey_fill_layer(
      sankey,
      fill_cols = fill_cols,
      combine = combine,
      gradient_slices = gradient_slices,
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      gradient_mode = gradient_mode,
      fill_border = NA,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols,
      endpoint_cols = endpoint_cols,
      mid_cols = mid_cols,
      ...
    )
  }, raster_res = raster_res, interpolate = interpolate)
}

draw_sankey_multi_fill_layer <- function(sankey, col, combine, gradient_slices,
                                         merge_length, fill_overlap, gradient_mode,
                                         merge_cols, node_cols,
                                         endpoint_cols = NULL,
                                         mid_col = NULL,
                                         flatten_trunks = TRUE,
                                         trunk_bg = par("bg"), ...) {
  multi_gradient_slices <- max(vapply(seq_along(sankey$segments), function(i) {
    as.integer(round(stage_arg(gradient_slices, i)))
  }, integer(1)))
  for(i in seq_along(sankey$segments)) {
    fill_cols <- sankey_flow_colors(sankey$segments[[i]], stage_arg(col, i))
    mid_cols <- if(is.null(mid_col)) {
      NULL
    } else {
      sankey_flow_colors(
        sankey$segments[[i]],
        stage_arg(mid_col, i),
        allow_na = TRUE,
        arg = "`mid_col`"
      )
    }
    draw_sankey_fill_layer(
      sankey$segments[[i]],
      fill_cols = fill_cols,
      combine = combine,
      gradient_slices = stage_arg(gradient_slices, i),
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      gradient_mode = gradient_mode,
      fill_border = NA,
      trunk_start = i == 1,
      trunk_end = i == length(sankey$segments),
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols[[i]],
      endpoint_cols = endpoint_cols[[i]],
      mid_cols = mid_cols,
      ...
    )
  }

  draw_multi_stage_interior_trunks(
    sankey,
    node_cols = node_cols,
    merge_cols = merge_cols,
    merge_length = merge_length,
    fill_overlap = fill_overlap,
    flatten_trunks = flatten_trunks,
    trunk_bg = trunk_bg,
    gradient_slices = multi_gradient_slices,
    endpoint_cols = endpoint_cols
  )
}

draw_sankey_multi_fill_raster <- function(sankey, col, combine, gradient_slices,
                                          merge_length, fill_overlap, gradient_mode,
                                          merge_cols, node_cols,
                                          endpoint_cols = NULL,
                                          mid_col = NULL,
                                          raster_res = 96,
                                          interpolate = TRUE,
                                          flatten_trunks = TRUE,
                                          trunk_bg = par("bg"), ...) {
  force(trunk_bg)
  render_raster_fill(function() {
    draw_sankey_multi_fill_layer(
      sankey,
      col = col,
      combine = combine,
      gradient_slices = gradient_slices,
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      gradient_mode = gradient_mode,
      merge_cols = merge_cols,
      node_cols = node_cols,
      endpoint_cols = endpoint_cols,
      mid_col = mid_col,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      ...
    )
  }, raster_res = raster_res, interpolate = interpolate)
}

# Opens a plot window with the aspect ratio the geometry was built for
# (asp = xyr), so strand widths and stack offsets stay visually correct.
open_sankey_window <- function(sankey) {
  xs <- c(sankey$flows$start_x, sankey$flows$end_x)
  ys <- c(sankey$flows$start_y, sankey$flows$end_y)
  pad_x <- diff(range(xs)) * 0.08
  pad_y <- max(sum(sankey$flows$width) / sankey$xyr, diff(range(ys))) * 0.25
  plot.new()
  plot.window(
    xlim = range(xs) + c(-pad_x, pad_x),
    ylim = range(ys) + c(-pad_y, pad_y),
    asp = sankey$xyr
  )
}

is_sankey_multi <- function(sankey) {
  inherits(sankey, "sankey_multi") || !is.null(sankey$segments)
}

stage_arg <- function(x, stage) {
  if(is.list(x) && !is.null(x[[stage]])) {
    return(x[[stage]])
  }
  x
}

plot_sankey_multi <- function(sankey, col = "#5B8CBAAA", border = NA,
                              add = TRUE, label_nodes = TRUE,
                              from_col = "#B3261EFF", to_col = "#146C2EFF",
                              combine = TRUE, border_lwd = 1,
                              border_lty = 1,
                              border_caps = TRUE,
                              node_border = NULL,
                              node_border_lwd = 1,
                              node_border_lty = 1,
                              node_border_steps = 24,
                              node_border_fade = c("background", "alpha"),
                              node_border_shape = c("polyline", "taper", "line"),
                              node_col = NULL,
                              merge_col = NULL,
                              merge_cols = NULL,
                              mid_col = NULL,
                              flatten_trunks = TRUE,
                              trunk_bg = par("bg"),
                              node_border_bg = trunk_bg,
                              merge_length = sankey$merge_length,
                              gradient_slices = 1,
                              gradient_mode = c("merge", "path"),
                              fill_overlap = NULL,
                              raster_fill = FALSE,
                              raster_res = 96,
                              interp_raster = TRUE,
                              show_node_sums = FALSE,
                              node_sum_col = "#111111",
                              node_sum_lwd = 2,
                              node_sum_offset = NULL, ...) {
  gradient_mode <- match.arg(gradient_mode)

  if(!add) {
    open_sankey_window(sankey)
  }

  node_cols <- NULL
  endpoint_cols <- NULL
  if(!is.null(node_col)) {
    node_cols <- multi_sankey_node_colors(sankey, node_col, arg = "`node_col`")
  }

  if(!is.null(merge_col)) {
    merge_node_cols <- multi_sankey_node_colors(sankey, merge_col, arg = "`merge_col`")
    merge_cols <- merge_cols_from_node_cols(sankey, merge_node_cols)
    if(is.null(node_cols)) {
      node_cols <- merge_node_cols
    } else {
      # With both supplied, node_col sets strand gradient endpoints and merge_col the trunks.
      endpoint_cols <- merge_cols_from_node_cols(sankey, node_cols)
    }
  } else if(!is.null(node_cols)) {
    merge_cols <- merge_cols_from_node_cols(sankey, node_cols)
  } else if(is.null(merge_cols)) {
    stage_cols <- multi_stage_flow_colors(sankey, col)
    node_cols <- multi_node_merge_colors(sankey, stage_cols)
    merge_cols <- merge_cols_from_node_cols(sankey, node_cols)
  } else if(!is.list(merge_cols) || length(merge_cols) != length(sankey$segments)) {
    stop("For multi-stage Sankey objects, `merge_cols` must be NULL or a list with one entry per stage.", call. = FALSE)
  }

  stage_inds <- seq_along(sankey$segments)
  all_raster <- all(vapply(stage_inds, function(i) isTRUE(stage_arg(raster_fill, i)), logical(1)))
  stage_has_border <- vapply(stage_inds, function(i) {
    b <- stage_arg(border, i)
    length(b) == 1 && !is.na(b)
  }, logical(1))
  all_stages_have_border <- all(stage_has_border)

  if(all_raster) {
    visible_border <- any(stage_has_border)
    max_gradient_slices <- max(vapply(stage_inds, function(i) {
      as.integer(round(stage_arg(gradient_slices, i)))
    }, integer(1)))
    alpha_cols <- c(
      unlist(lapply(stage_inds, function(i) sankey_flow_colors(sankey$segments[[i]], stage_arg(col, i)))),
      unlist(lapply(merge_cols, function(x) c(x$start, x$end))),
      unlist(lapply(endpoint_cols, function(x) c(x$start, x$end)))
    )
    if(!is.null(mid_col)) {
      alpha_cols <- c(
        alpha_cols,
        unlist(lapply(stage_inds, function(i) {
          sankey_flow_colors(sankey$segments[[i]], stage_arg(mid_col, i), allow_na = TRUE, arg = "`mid_col`")
        }))
      )
    }
    multi_fill_overlap <- if(is.null(fill_overlap)) {
      default_fill_overlap(visible_border, max_gradient_slices, merge_length, alpha_cols, flatten_trunks)
    } else if(is.list(fill_overlap)) {
      stage_arg(fill_overlap, 1)
    } else {
      fill_overlap
    }
    if(!is.numeric(multi_fill_overlap) || length(multi_fill_overlap) != 1 ||
       !is.finite(multi_fill_overlap) || multi_fill_overlap < 0) {
      stop("`fill_overlap` must be NULL or a single non-negative finite number.", call. = FALSE)
    }

    draw_sankey_multi_fill_raster(
      sankey,
      col = col,
      combine = combine,
      gradient_slices = gradient_slices,
      merge_length = merge_length,
      fill_overlap = multi_fill_overlap,
      gradient_mode = gradient_mode,
      merge_cols = merge_cols,
      node_cols = node_cols,
      endpoint_cols = endpoint_cols,
      mid_col = mid_col,
      raster_res = stage_arg(raster_res, 1),
      interpolate = stage_arg(interp_raster, 1),
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      ...
    )

    for(i in stage_inds) {
      draw_endpoint_node_body_borders(
        sankey$segments[[i]],
        col = stage_arg(node_border, i),
        flow_cols = sankey_flow_colors(sankey$segments[[i]], stage_arg(col, i)),
        lwd = stage_arg(node_border_lwd, i),
        lty = stage_arg(node_border_lty, i),
        steps = stage_arg(node_border_steps, i),
        fade = stage_arg(node_border_fade, i),
        shape = stage_arg(node_border_shape, i),
        bg = stage_arg(node_border_bg, i),
        edge_border = stage_has_border[i],
        trunk_start = i == 1,
        trunk_end = i == length(sankey$segments)
      )
    }
    draw_multi_stage_interior_node_borders(
      sankey,
      col = stage_arg(node_border, 1),
      node_cols = node_cols,
      lwd = stage_arg(node_border_lwd, 1),
      lty = stage_arg(node_border_lty, 1),
      steps = stage_arg(node_border_steps, 1),
      fade = stage_arg(node_border_fade, 1),
      shape = stage_arg(node_border_shape, 1),
      bg = stage_arg(node_border_bg, 1),
      edge_border = all_stages_have_border
    )

    for(i in stage_inds) {
      b <- stage_arg(border, i)
      if(length(b) == 1 && !is.na(b)) {
        draw_sankey_borders(
          sankey$segments[[i]],
          border = b,
          lwd = stage_arg(border_lwd, i),
          lty = stage_arg(border_lty, i),
          merge_length = merge_length,
          cap_start = isTRUE(border_caps) && i == 1,
          cap_end = isTRUE(border_caps) && i == length(sankey$segments)
        )
      }
    }
  } else {
    for(i in stage_inds) {
      cap_start <- isTRUE(border_caps) && i == 1
      cap_end <- isTRUE(border_caps) && i == length(sankey$segments)
      trunk_start <- i == 1
      trunk_end <- i == length(sankey$segments)
      plot_sankey_ribbons(
        sankey$segments[[i]],
        col = stage_arg(col, i),
        border = stage_arg(border, i),
        add = TRUE,
        label_nodes = FALSE,
        from_col = from_col,
        to_col = to_col,
        combine = combine,
        border_lwd = stage_arg(border_lwd, i),
        border_lty = stage_arg(border_lty, i),
        border_caps = c(start = cap_start, end = cap_end),
        node_border = stage_arg(node_border, i),
        node_border_lwd = stage_arg(node_border_lwd, i),
        node_border_lty = stage_arg(node_border_lty, i),
        node_border_steps = stage_arg(node_border_steps, i),
        node_border_fade = stage_arg(node_border_fade, i),
        node_border_shape = stage_arg(node_border_shape, i),
        node_border_bg = stage_arg(node_border_bg, i),
        .edge_border = stage_has_border[i],
        trunk_start = trunk_start,
        trunk_end = trunk_end,
        merge_cols = merge_cols[[i]],
        endpoint_cols = endpoint_cols[[i]],
        mid_col = stage_arg(mid_col, i),
        flatten_trunks = flatten_trunks,
        trunk_bg = trunk_bg,
        merge_length = merge_length,
        gradient_slices = stage_arg(gradient_slices, i),
        gradient_mode = gradient_mode,
        fill_overlap = stage_arg(fill_overlap, i),
        raster_fill = stage_arg(raster_fill, i),
        raster_res = stage_arg(raster_res, i),
        interp_raster = stage_arg(interp_raster, i),
        show_node_sums = FALSE,
        ...
      )
    }

    draw_multi_stage_interior_trunks(
      sankey,
      node_cols = node_cols,
      merge_cols = merge_cols,
      merge_length = merge_length,
      fill_overlap = if(is.null(fill_overlap)) 0 else stage_arg(fill_overlap, 1),
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      gradient_slices = max(vapply(stage_inds, function(i) {
        as.integer(round(stage_arg(gradient_slices, i)))
      }, integer(1))),
      endpoint_cols = endpoint_cols
    )
    draw_multi_stage_interior_node_borders(
      sankey,
      col = stage_arg(node_border, 1),
      node_cols = node_cols,
      lwd = stage_arg(node_border_lwd, 1),
      lty = stage_arg(node_border_lty, 1),
      steps = stage_arg(node_border_steps, 1),
      fade = stage_arg(node_border_fade, 1),
      shape = stage_arg(node_border_shape, 1),
      bg = stage_arg(node_border_bg, 1),
      edge_border = all_stages_have_border
    )
  }

  if(show_node_sums) {
    offset <- if(is.null(node_sum_offset)) diff(range(sankey$nodes$x)) * 0.06 else node_sum_offset
    for(segment in sankey$segments) {
      draw_node_sum_segments(segment, col = node_sum_col, lwd = node_sum_lwd, offset = offset)
    }
  }

  if(label_nodes) {
    first_set <- min(sankey$nodes$set)
    last_set <- max(sankey$nodes$set)
    for(i in seq_len(nrow(sankey$nodes))) {
      node <- sankey$nodes[i, ]
      node_col <- if(node$set == first_set) from_col else if(node$set == last_set) to_col else "black"
      points(node$x, node$y, pch = 21, bg = "white", col = node_col)
      text(
        node$x, node$y,
        labels = if("label" %in% names(node)) node$label else node$node,
        col = node_col,
        pos = if(node$set == first_set) 2 else if(node$set == last_set) 4 else 3,
        xpd = NA
      )
    }
  }

  invisible(sankey)
}

# Draw Sankey ribbons returned by sankey_ribbons().
#
# col controls strand colors and can be a scalar, a vector with one value per
# positive flow, a matrix matching thic, or a list of matrices for multi-stage
# thic. By default, node/merge colors are inferred as weighted averages of the
# adjacent strand colors. node_col can override those colors per node, while
# merge_col can override the merged trunk colors specifically; both accept
# list(start/from = ..., end/to = ...) for two-stage plots or a list with one
# color vector per node set for multi-stage plots. mid_col optionally supplies
# per-strand middle colors for three-way gradients; NA entries retain a direct
# endpoint-to-endpoint gradient. gradient_slices controls gradient resolution.
# raster_fill = TRUE rasterizes only the fill layer before overlaying borders
# and labels, which is useful for smoother and lighter gradient output.
plot_sankey_ribbons <- function(sankey, col = "#5B8CBAAA", border = NA,
                                add = TRUE, label_nodes = TRUE,
                                from_col = "#B3261EFF", to_col = "#146C2EFF",
                                combine = TRUE, border_lwd = 1,
                                border_lty = 1,
                                border_caps = TRUE,
                                node_border = NULL,
                                node_border_lwd = 1,
                                node_border_lty = 1,
                                node_border_steps = 24,
                                node_border_fade = c("background", "alpha"),
                                node_border_shape = c("polyline", "taper", "line"),
                                trunk_start = TRUE,
                                trunk_end = TRUE,
                                node_col = NULL,
                                merge_col = NULL,
                                merge_cols = NULL,
                                endpoint_cols = NULL,
                                mid_col = NULL,
                                flatten_trunks = TRUE,
                                trunk_bg = par("bg"),
                                node_border_bg = trunk_bg,
                                merge_length = sankey$merge_length,
                                gradient_slices = 1,
                                gradient_mode = c("merge", "path"),
                                fill_overlap = NULL,
                                raster_fill = FALSE,
                                raster_res = 96,
                                interp_raster = TRUE,
                                show_node_sums = FALSE,
                                node_sum_col = "#111111",
                                node_sum_lwd = 2,
                                node_sum_offset = NULL,
                                .edge_border = NULL, ...) {
  if(is_sankey_multi(sankey)) {
    return(plot_sankey_multi(
      sankey,
      col = col,
      border = border,
      add = add,
      label_nodes = label_nodes,
      from_col = from_col,
      to_col = to_col,
      combine = combine,
      border_lwd = border_lwd,
      border_lty = border_lty,
      border_caps = border_caps,
      node_border = node_border,
      node_border_lwd = node_border_lwd,
      node_border_lty = node_border_lty,
      node_border_steps = node_border_steps,
      node_border_fade = node_border_fade,
      node_border_shape = node_border_shape,
      node_border_bg = node_border_bg,
      node_col = node_col,
      merge_col = merge_col,
      merge_cols = merge_cols,
      mid_col = mid_col,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_length = merge_length,
      gradient_slices = gradient_slices,
      gradient_mode = gradient_mode,
      fill_overlap = fill_overlap,
      raster_fill = raster_fill,
      raster_res = raster_res,
      interp_raster = interp_raster,
      show_node_sums = show_node_sums,
      node_sum_col = node_sum_col,
      node_sum_lwd = node_sum_lwd,
      node_sum_offset = node_sum_offset,
      ...
    ))
  }

  if(!add) {
    open_sankey_window(sankey)
  }

  draw_border <- length(border) == 1 && !is.na(border)
  node_edge_border <- if(is.null(.edge_border)) draw_border else isTRUE(.edge_border)
  fill_border <- if(draw_border) NA else border
  fill_cols <- sankey_flow_colors(sankey, col)
  if(!is.null(node_col)) {
    node_cols <- single_sankey_node_colors(sankey, node_col, arg = "`node_col`")
    merge_cols <- single_merge_cols_from_node_cols(sankey, node_cols)
  }
  if(!is.null(merge_col)) {
    # With both supplied, node_col sets strand gradient endpoints and merge_col the trunks.
    if(!is.null(node_col)) {
      endpoint_cols <- merge_cols
    }
    merge_node_cols <- single_sankey_node_colors(sankey, merge_col, arg = "`merge_col`")
    merge_cols <- single_merge_cols_from_node_cols(sankey, merge_node_cols)
  }
  mid_cols <- if(is.null(mid_col)) {
    NULL
  } else {
    sankey_flow_colors(sankey, mid_col, allow_na = TRUE, arg = "`mid_col`")
  }
  gradient_mode <- match.arg(gradient_mode)
  if(is.null(fill_overlap)) {
    alpha_cols <- c(fill_cols)
    if(!is.null(merge_cols)) {
      alpha_cols <- c(alpha_cols, merge_cols$start, merge_cols$end)
    }
    if(!is.null(endpoint_cols)) {
      alpha_cols <- c(alpha_cols, endpoint_cols$start, endpoint_cols$end)
    }
    if(!is.null(mid_cols)) {
      alpha_cols <- c(alpha_cols, mid_cols)
    }
    fill_overlap <- default_fill_overlap(draw_border, gradient_slices, merge_length, alpha_cols, flatten_trunks)
  }
  if(!is.numeric(fill_overlap) || length(fill_overlap) != 1 ||
     !is.finite(fill_overlap) || fill_overlap < 0) {
    stop("`fill_overlap` must be NULL or a single non-negative finite number.", call. = FALSE)
  }

  if(raster_fill) {
    draw_sankey_fill_raster(
      sankey,
      fill_cols = fill_cols,
      combine = combine,
      gradient_slices = gradient_slices,
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      gradient_mode = gradient_mode,
      raster_res = raster_res,
      interpolate = interp_raster,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols,
      endpoint_cols = endpoint_cols,
      mid_cols = mid_cols,
      ...
    )
  } else {
    draw_sankey_fill_layer(
      sankey,
      fill_cols = fill_cols,
      combine = combine,
      gradient_slices = gradient_slices,
      merge_length = merge_length,
      fill_overlap = fill_overlap,
      gradient_mode = gradient_mode,
      fill_border = fill_border,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols,
      endpoint_cols = endpoint_cols,
      mid_cols = mid_cols,
      ...
    )
  }

  draw_endpoint_node_body_borders(
    sankey,
    col = node_border,
    flow_cols = fill_cols,
    lwd = node_border_lwd,
    lty = node_border_lty,
    steps = node_border_steps,
    fade = node_border_fade,
    shape = node_border_shape,
    bg = node_border_bg,
    edge_border = node_edge_border,
    trunk_start = trunk_start,
    trunk_end = trunk_end
  )
  draw_graph_interior_node_borders(
    sankey,
    col = node_border,
    flow_cols = fill_cols,
    lwd = node_border_lwd,
    lty = node_border_lty,
    steps = node_border_steps,
    fade = node_border_fade,
    shape = node_border_shape,
    bg = node_border_bg,
    edge_border = node_edge_border
  )

  if(draw_border) {
    if(length(border_caps) == 1) {
      cap_start <- cap_end <- isTRUE(border_caps)
    } else {
      cap_start <- isTRUE(border_caps[["start"]])
      cap_end <- isTRUE(border_caps[["end"]])
    }
    draw_sankey_borders(
      sankey,
      border = border,
      lwd = border_lwd,
      lty = border_lty,
      merge_length = merge_length,
      cap_start = cap_start,
      cap_end = cap_end
    )
  }

  if(show_node_sums) {
    draw_node_sum_segments(
      sankey,
      col = node_sum_col,
      lwd = node_sum_lwd,
      offset = node_sum_offset
    )
  }

  if(label_nodes) {
    if(inherits(sankey, "sankey_graph")) {
      node_col <- ifelse(sankey$nodes$x <= stats::median(sankey$nodes$x), from_col, to_col)
      points(sankey$nodes$x, sankey$nodes$y, pch = 21, bg = "white", col = node_col)
      labels <- if("label" %in% names(sankey$nodes)) sankey$nodes$label else sankey$nodes$node
      text(
        sankey$nodes$x,
        sankey$nodes$y,
        labels = labels,
        col = node_col,
        pos = ifelse(sankey$nodes$x <= stats::median(sankey$nodes$x), 2, 4),
        xpd = NA
      )
    } else {
      points(sankey$from_nodes$x, sankey$from_nodes$y, pch = 21, bg = "white", col = from_col)
      points(sankey$to_nodes$x, sankey$to_nodes$y, pch = 21, bg = "white", col = to_col)
      text(
        sankey$from_nodes$x, sankey$from_nodes$y,
        labels = if("label" %in% names(sankey$from_nodes)) sankey$from_nodes$label else sankey$from_nodes$node,
        col = from_col, pos = 2, xpd = NA
      )
      text(
        sankey$to_nodes$x, sankey$to_nodes$y,
        labels = if("label" %in% names(sankey$to_nodes)) sankey$to_nodes$label else sankey$to_nodes$node,
        col = to_col, pos = 4, xpd = NA
      )
    }
  }

  invisible(sankey)
}

# Check that adjacent endpoint strands touch without gaps or overlap.
check_sankey_spacing <- function(sankey, tolerance = 1e-9) {
  if(is_sankey_multi(sankey)) {
    checks <- lapply(sankey$segments, check_sankey_spacing, tolerance = tolerance)
    max_error <- max(vapply(checks, `[[`, numeric(1), "max_error"))
    if(max_error > tolerance) {
      stop(sprintf("Endpoint spacing check failed; max error was %g.", max_error), call. = FALSE)
    }
    return(list(segments = checks, max_error = max_error))
  }

  check_one_side <- function(flows, node_col, offset_col) {
    split_inds <- split(seq_len(nrow(flows)), flows[[node_col]])
    out <- lapply(names(split_inds), function(node) {
      inds <- split_inds[[node]]
      if(length(inds) < 2) {
        return(data.frame(node = as.integer(node), max_abs_error = 0))
      }
      ord <- inds[order(flows[[offset_col]][inds])]
      observed <- diff(flows[[offset_col]][ord])
      expected <- (flows$width[head(ord, -1)] + flows$width[tail(ord, -1)]) / 2
      data.frame(node = as.integer(node), max_abs_error = max(abs(observed - expected)))
    })
    do.call(rbind, out)
  }

  start <- check_one_side(sankey$flows, "from", "start_offset")
  end <- check_one_side(sankey$flows, "to", "end_offset")
  max_error <- max(c(start$max_abs_error, end$max_abs_error))
  if(max_error > tolerance) {
    stop(sprintf("Endpoint spacing check failed; max error was %g.", max_error), call. = FALSE)
  }

  list(start = start, end = end, max_error = max_error)
}

# Convert Sankey ribbon polygons to an sf geometry collection.
sankey_as_sf <- function(sankey, union = FALSE, make_valid = TRUE) {
  if(!requireNamespace("sf", quietly = TRUE)) {
    stop("Package `sf` is required for `sankey_as_sf()`.", call. = FALSE)
  }
  if(is_sankey_multi(sankey)) {
    out <- do.call(c, lapply(sankey$segments, sankey_as_sf, union = FALSE, make_valid = make_valid))
    if(union) {
      out <- sf::st_union(out)
    }
    return(out)
  }
  polygons <- lapply(sankey$ribbons, function(coords) sf::st_polygon(list(as.matrix(coords))))
  out <- sf::st_sfc(polygons)
  if(make_valid) {
    out <- sf::st_make_valid(out)
  }
  if(union) {
    out <- sf::st_union(out)
  }
  out
}
