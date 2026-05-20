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

validate_sankey_inputs <- function(thic, loc, from_set = 1, to_set = 2) {
  if(!is.matrix(thic) || !is.numeric(thic)) {
    stop("`thic` must be a numeric matrix.", call. = FALSE)
  }
  if(any(!is.finite(thic)) || any(thic < 0)) {
    stop("`thic` must contain finite, non-negative values.", call. = FALSE)
  }
  if(!is.matrix(loc) || !is.numeric(loc) || ncol(loc) != 4) {
    stop("`loc` must be a numeric matrix with four columns.", call. = FALSE)
  }
  if(any(!is.finite(loc))) {
    stop("`loc` must contain only finite values.", call. = FALSE)
  }

  m <- nrow(thic)
  n <- ncol(thic)
  from_loc <- loc[loc[, 1] == from_set, , drop = FALSE]
  to_loc <- loc[loc[, 1] == to_set, , drop = FALSE]

  if(nrow(from_loc) != m) {
    stop("`loc` must contain exactly one row for each `thic` row in `from_set`.", call. = FALSE)
  }
  if(nrow(to_loc) != n) {
    stop("`loc` must contain exactly one row for each `thic` column in `to_set`.", call. = FALSE)
  }
  if(any(duplicated(from_loc[, 2])) || any(duplicated(to_loc[, 2]))) {
    stop("Node indices in each node set must be unique.", call. = FALSE)
  }
  if(!setequal(from_loc[, 2], seq_len(m))) {
    stop("From-node indices in `loc` must be 1:nrow(thic).", call. = FALSE)
  }
  if(!setequal(to_loc[, 2], seq_len(n))) {
    stop("To-node indices in `loc` must be 1:ncol(thic).", call. = FALSE)
  }

  invisible(TRUE)
}

ordered_node_table <- function(loc, node_set, node_count) {
  out <- loc[loc[, 1] == node_set, , drop = FALSE]
  out <- out[match(seq_len(node_count), out[, 2]), , drop = FALSE]
  data.frame(
    set = out[, 1],
    node = out[, 2],
    x = out[, 3],
    y = out[, 4]
  )
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

build_sankey_flows <- function(thic, loc, from_set = 1, to_set = 2) {
  validate_sankey_inputs(thic, loc, from_set = from_set, to_set = to_set)

  m <- nrow(thic)
  n <- ncol(thic)
  from_nodes <- ordered_node_table(loc, from_set, m)
  to_nodes <- ordered_node_table(loc, to_set, n)

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
    order <- inds[order(to_nodes$y[flows$to[inds]], flows$to[inds])]
    flows$start_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }

  for(j in seq_len(n)) {
    inds <- which(flows$to == j)
    order <- inds[order(from_nodes$y[flows$from[inds]], flows$from[inds])]
    flows$end_offset[inds] <- stack_offsets(flows$width[inds], match(order, inds))
  }

  flows$start_x <- from_nodes$x[flows$from]
  flows$start_y <- from_nodes$y[flows$from]
  flows$end_x <- to_nodes$x[flows$to]
  flows$end_y <- to_nodes$y[flows$to]

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

all_node_table <- function(loc, node_counts) {
  out <- do.call(rbind, lapply(seq_along(node_counts), function(node_set) {
    ordered_node_table(loc, node_set, node_counts[node_set])
  }))
  rownames(out) <- NULL
  out
}

combine_sankey_segments <- function(segments, loc, node_counts, merge_length, xyr) {
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
    nodes = all_node_table(loc, node_counts),
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

centerline_edges <- function(x, y, width, xyr = xyrat_local(), eps = 1e-10) {
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

centerline_to_ribbon <- function(x, y, width, xyr = xyrat_local(), eps = 1e-10) {
  edges <- centerline_edges(x, y, width, xyr = xyr, eps = eps)
  left <- edges$left
  right <- edges$right
  n <- length(x)
  coords <- rbind(left, right[n:1, , drop = FALSE], left[1, ])
  colnames(coords) <- c("x", "y")
  coords
}

# Build Sankey ribbon geometry.
#
# thic is either one m-by-n matrix of non-negative flow widths or a list of
# matrices for successive node sets. For a list, ncol(thic[[i]]) must equal
# nrow(thic[[i + 1]]). loc is a matrix/data.frame with columns:
# node set index, node index within that set, x, and y. The function returns
# polygon coordinates, centerlines, flow bookkeeping, node tables, and the
# shared merge length used near nodes.
sankey_ribbons <- function(thic, loc, from_set = 1, to_set = 2, n = 120,
                           curvature = 0.45, merge_frac = 0.05,
                           merge_length = NULL, xyr = xyrat_local()) {
  if(is.list(thic)) {
    validate_sankey_thic_list(thic)
    if(!is.null(merge_length) &&
       (!is.numeric(merge_length) || length(merge_length) != 1 ||
        !is.finite(merge_length) || merge_length < 0)) {
      stop("`merge_length` must be NULL or a single non-negative finite number.", call. = FALSE)
    }
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
        xyr = xyr
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
          xyr = xyr
        )
      })
    }
    return(combine_sankey_segments(segments, loc, node_counts, merge_length, xyr))
  }

  if(!is.numeric(merge_frac) || length(merge_frac) != 1 || !is.finite(merge_frac) || merge_frac < 0) {
    stop("`merge_frac` must be a single non-negative finite number.", call. = FALSE)
  }
  if(!is.null(merge_length) &&
     (!is.numeric(merge_length) || length(merge_length) != 1 ||
      !is.finite(merge_length) || merge_length < 0)) {
    stop("`merge_length` must be NULL or a single non-negative finite number.", call. = FALSE)
  }

  sankey <- build_sankey_flows(thic, loc, from_set = from_set, to_set = to_set)
  flows <- sankey$flows

  ribbons <- vector("list", nrow(flows))
  centerlines <- vector("list", nrow(flows))

  for(k in seq_len(nrow(flows))) {
    flow <- flows[k, ]
    dx <- flow$end_x - flow$start_x
    if(abs(dx) <= 1e-10) {
      stop("Prototype currently requires from/to nodes to have different x positions.", call. = FALSE)
    }

    start_tangent <- c(sign(dx), 0)
    end_tangent <- c(sign(dx), 0)
    p0 <- apply_visual_normal_offset(
      flow$start_x, flow$start_y, flow$start_offset, start_tangent, xyr = xyr
    )
    p3 <- apply_visual_normal_offset(
      flow$end_x, flow$end_y, flow$end_offset, end_tangent, xyr = xyr
    )
    handle <- curvature * dx
    p1 <- c(p0[1] + handle, p0[2])
    p2 <- c(p3[1] - handle, p3[2])
    centerline <- cubic_bezier(p0, p1, p2, p3, n = n)

    centerlines[[k]] <- centerline
    ribbons[[k]] <- centerline_to_ribbon(
      centerline[, "x"], centerline[, "y"], flow$width, xyr = xyr
    )
  }

  names(ribbons) <- sprintf("from_%s_to_%s", flows$from, flows$to)
  path_lengths <- vapply(centerlines, visual_path_length, numeric(1), xyr = xyr)
  if(is.null(merge_length)) {
    merge_length <- merge_frac * min(path_lengths)
  }

  sankey$ribbons <- ribbons
  sankey$centerlines <- centerlines
  sankey$path_lengths <- path_lengths
  sankey$merge_length <- merge_length
  sankey$xyr <- xyr
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

default_fill_overlap <- function(draw_border, gradient_slices, merge_length, cols) {
  if(draw_border || (gradient_slices > 1 && colors_have_alpha(cols))) {
    return(0)
  }
  max(merge_length * 0.02, 1e-4)
}

flow_gradient_color <- function(sankey, flow_cols, node_cols, flow_ind, distance,
                                mid_cols = NULL,
                                merge_length, gradient_mode = c("merge", "path")) {
  gradient_mode <- match.arg(gradient_mode)
  path_length <- sankey$path_lengths[flow_ind]
  thread_col <- if(is.null(mid_cols)) flow_cols[flow_ind] else mid_cols[flow_ind]
  if(path_length <= 0) {
    return(if(is.na(thread_col)) node_cols$start[flow_ind] else thread_col)
  }

  if(gradient_mode == "path") {
    t <- min(max(distance / path_length, 0), 1)
    if(is.na(thread_col)) {
      return(interpolate_color(node_cols$start[flow_ind], node_cols$end[flow_ind], t))
    }
    if(t <= 0.5) {
      return(interpolate_color(node_cols$start[flow_ind], thread_col, t * 2))
    }
    return(interpolate_color(thread_col, node_cols$end[flow_ind], (t - 0.5) * 2))
  }

  if(is.na(thread_col)) {
    return(interpolate_color(node_cols$start[flow_ind], node_cols$end[flow_ind], distance / path_length))
  }
  if(merge_length <= 0) {
    return(thread_col)
  }
  if(distance <= merge_length) {
    return(interpolate_color(node_cols$start[flow_ind], thread_col, distance / merge_length))
  }
  if(distance >= path_length - merge_length) {
    return(interpolate_color(
      thread_col,
      node_cols$end[flow_ind],
      (distance - (path_length - merge_length)) / merge_length
    ))
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
    lines(seg[, "x"], seg[, "y"], col = col, lwd = lwd, lty = lty, xpd = NA)
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
      coords[rev(interior), , drop = FALSE],
      point_at_x(coords, lo)
    )
  }
  colnames(out) <- colnames(coords)
  out
}

ribbon_slice_polygon <- function(sankey, flow_ind, start, end) {
  centerline <- sankey$centerlines[[flow_ind]]
  distances <- edge_distances(centerline, sankey$xyr)
  edges <- centerline_edges(
    centerline[, "x"], centerline[, "y"], sankey$flows$width[flow_ind], xyr = sankey$xyr
  )
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
  centerline <- sankey$centerlines[[flow_ind]]
  edges <- centerline_edges(
    centerline[, "x"], centerline[, "y"], sankey$flows$width[flow_ind], xyr = sankey$xyr
  )
  left <- path_segment_by_x(edges$left, start_x, end_x)
  right <- path_segment_by_x(edges$right, start_x, end_x)
  if(nrow(left) < 2 || nrow(right) < 2) {
    return(NULL)
  }

  coords <- rbind(left, right[nrow(right):1, , drop = FALSE], left[1, , drop = FALSE])
  colnames(coords) <- c("x", "y")
  coords
}

merge_cut_distances <- function(sankey, flow_ind, merge_length, fill_overlap = 0) {
  centerline <- sankey$centerlines[[flow_ind]]
  distances <- edge_distances(centerline, sankey$xyr)
  path_length <- max(distances)
  if(merge_length <= 0 || path_length <= 0) {
    return(c(start = 0, end = path_length))
  }

  dx <- sankey$flows$end_x[flow_ind] - sankey$flows$start_x[flow_ind]
  direction <- sign(dx)
  if(direction == 0) {
    return(c(start = 0, end = path_length))
  }

  cut <- max(merge_length - fill_overlap, 0)
  start_x <- sankey$flows$start_x[flow_ind] + direction * cut
  end_x <- sankey$flows$end_x[flow_ind] - direction * cut
  d0 <- distance_at_x(centerline, distances, start_x)
  d1 <- distance_at_x(centerline, distances, end_x)
  sort(c(start = d0, end = d1))
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
                                   mid_cols = NULL, ...) {
  gradient_mode <- match.arg(gradient_mode)
  validate_merge_cols(sankey, merge_cols)
  if(!is.null(mid_cols) && length(mid_cols) != nrow(sankey$flows)) {
    stop("`mid_cols` must be NULL or one color per positive flow.", call. = FALSE)
  }
  if(!is.numeric(gradient_slices) || length(gradient_slices) != 1 ||
     !is.finite(gradient_slices) || gradient_slices < 1) {
    stop("`gradient_slices` must be a single finite number >= 1.", call. = FALSE)
  }
  gradient_slices <- as.integer(round(gradient_slices))

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
    node_cols <- if(is.null(merge_cols)) flow_node_colors(sankey, fill_cols) else merge_cols
    for(k in seq_along(sankey$centerlines)) {
      if(merge_length > 0 && sankey$path_lengths[k] > 2 * merge_length) {
        xcuts <- flow_x_cuts(sankey, k, merge_length, fill_overlap)
        breaks_x <- seq(xcuts["start"], xcuts["end"], length.out = gradient_slices + 1)
        distance_cuts <- merge_cut_distances(sankey, k, merge_length, fill_overlap)
      } else {
        xcuts <- sort(c(
          start = sankey$flows$start_x[k],
          end = sankey$flows$end_x[k]
        ))
        breaks_x <- seq(xcuts["start"], xcuts["end"], length.out = gradient_slices + 1)
        distance_cuts <- c(start = 0, end = sankey$path_lengths[k])
      }
      d0 <- distance_cuts["start"]
      d1 <- distance_cuts["end"]
      breaks <- seq(d0, d1, length.out = gradient_slices + 1)
      for(i in seq_len(gradient_slices)) {
        coords <- ribbon_slice_polygon_by_x(sankey, k, breaks_x[i], breaks_x[i + 1])
        if(is.null(coords)) {
          next
        }
        mid <- mean(breaks[i:(i + 1)])
        polygon(
          coords[, "x"], coords[, "y"],
          col = flow_gradient_color(
            sankey,
            fill_cols,
            node_cols,
            k,
            mid,
            mid_cols = mid_cols,
            merge_length = merge_length,
            gradient_mode = gradient_mode
          ),
          border = NA,
          xpd = NA,
          ...
        )
      }
    }
  }

  if(merge_length > 0) {
    draw_node_trunk_fills(
      sankey,
      flow_cols = fill_cols,
      merge_length = merge_length + fill_overlap,
      trunk_start = trunk_start,
      trunk_end = trunk_end,
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      merge_cols = merge_cols
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
  edges <- centerline_edges(
    centerline[, "x"], centerline[, "y"], sankey$flows$width[flow_ind], xyr = sankey$xyr
  )
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
  coords
}

draw_node_trunk_fills <- function(sankey, flow_cols, merge_length,
                                  trunk_start = TRUE, trunk_end = TRUE,
                                  flatten_trunks = TRUE,
                                  trunk_bg = par("bg"),
                                  merge_cols = NULL) {
  validate_merge_cols(sankey, merge_cols)
  draw_one_side <- function(nodes, node_col, side, merge_side) {
    for(i in seq_len(nrow(nodes))) {
      flow_inds <- which(sankey$flows[[node_col]] == nodes$node[i])
      if(length(flow_inds) == 0) {
        next
      }

      coords <- node_trunk_polygon(sankey, flow_inds, side, merge_length)
      if(is.null(coords)) {
        next
      }
      col <- if(is.null(merge_cols)) {
        node_merge_color(sankey, flow_cols, flow_inds)
      } else {
        merge_cols[[merge_side]][flow_inds[1]]
      }
      if(flatten_trunks) {
        col <- flatten_alpha_color(col, bg = trunk_bg)
      }
      polygon(coords[, "x"], coords[, "y"], col = col, border = NA, xpd = NA)
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

  coords <- rbind(
    prev_lower_seg,
    next_lower_seg[-1, , drop = FALSE],
    next_upper_seg[nrow(next_upper_seg):1, , drop = FALSE],
    prev_upper_seg[(nrow(prev_upper_seg) - 1):1, , drop = FALSE],
    prev_lower_seg[1, , drop = FALSE]
  )
  colnames(coords) <- c("x", "y")
  coords
}

draw_multi_stage_interior_trunks <- function(sankey, node_cols, merge_cols,
                                             merge_length, fill_overlap = 0,
                                             flatten_trunks = TRUE,
                                             trunk_bg = par("bg")) {
  if(length(sankey$segments) < 2 || merge_length <= 0) {
    return(invisible(NULL))
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

      col <- if(!is.null(node_cols)) {
        node_cols[[node_set]][node]
      } else {
        flow_inds <- which(prev_segment$flows$to == node)
        merge_cols[[node_set - 1]]$end[flow_inds[1]]
      }
      if(flatten_trunks) {
        col <- flatten_alpha_color(col, bg = trunk_bg)
      }
      polygon(coords[, "x"], coords[, "y"], col = col, border = NA, xpd = NA)
    }
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
    lines(seg[, "x"], seg[, "y"], col = col, lwd = lwd, lty = lty, xpd = NA)
  }
  invisible(seg)
}

endpoint_candidates <- function(sankey, flow_inds, side) {
  out <- do.call(rbind, lapply(flow_inds, function(k) {
    centerline <- sankey$centerlines[[k]]
    edges <- centerline_edges(
      centerline[, "x"], centerline[, "y"], sankey$flows$width[k], xyr = sankey$xyr
    )
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
  draw_one_side <- function(nodes, node_col, side, cap) {
    for(i in seq_len(nrow(nodes))) {
      flow_inds <- which(sankey$flows[[node_col]] == nodes$node[i])
      if(length(flow_inds) == 0) {
        next
      }

      candidates <- endpoint_candidates(sankey, flow_inds, side)
      lower <- candidates[which.min(candidates$y), ]
      upper <- candidates[which.max(candidates$y), ]
      draw_endpoint_edge(sankey, lower$flow, lower$edge, side, merge_length, col, lwd, lty)
      draw_endpoint_edge(sankey, upper$flow, upper$edge, side, merge_length, col, lwd, lty)
      if(cap) {
        lines(c(lower$x, upper$x), c(lower$y, upper$y), col = col, lwd = lwd, lty = lty, xpd = NA)
      }
    }
  }

  draw_one_side(sankey$from_nodes, "from", side = "start", cap = cap_start)
  draw_one_side(sankey$to_nodes, "to", side = "end", cap = cap_end)
}

draw_sankey_borders <- function(sankey, border, lwd = 1, lty = 1,
                                merge_length = sankey$merge_length,
                                cap_start = TRUE, cap_end = TRUE) {
  if(is.na(border) || merge_length >= min(sankey$path_lengths) / 2) {
    return(invisible(NULL))
  }

  for(k in seq_along(sankey$centerlines)) {
    centerline <- sankey$centerlines[[k]]
    edges <- centerline_edges(
      centerline[, "x"], centerline[, "y"], sankey$flows$width[k], xyr = sankey$xyr
    )
    draw_edge_segment(edges$left, centerline, merge_length, merge_length, border, lwd, lty, sankey$xyr)
    draw_edge_segment(edges$right, centerline, merge_length, merge_length, border, lwd, lty, sankey$xyr)
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
    node <- as.character(sankey$from_nodes$node[i])
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
    node <- as.character(sankey$to_nodes$node[i])
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

draw_sankey_fill_raster <- function(sankey, fill_cols, combine, gradient_slices,
                                    merge_length, fill_overlap, gradient_mode,
                                    raster_res = 96,
                                    interpolate = TRUE,
                                    trunk_start = TRUE,
                                    trunk_end = TRUE,
                                    flatten_trunks = TRUE,
                                    trunk_bg = par("bg"),
                                    merge_cols = NULL,
                                    mid_cols = NULL, ...) {
  if(!requireNamespace("png", quietly = TRUE)) {
    stop("Package `png` is required for rasterized Sankey fills.", call. = FALSE)
  }

  gr_usr <- plotting_device_extent()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
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
  on.exit({
    if(grDevices::dev.cur() != main_dev) {
      grDevices::dev.off()
    }
  }, add = TRUE)
  par(mar = c(0, 0, 0, 0), xpd = NA)
  plot.new()
  plot.window(xlim = gr_usr[1:2], ylim = gr_usr[3:4], xaxs = "i", yaxs = "i")
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
    mid_cols = mid_cols,
    ...
  )
  grDevices::dev.off()

  rasterImage(
    png::readPNG(tmp),
    gr_usr[1], gr_usr[3], gr_usr[2], gr_usr[4],
    interpolate = interpolate,
    xpd = NA
  )

  invisible(NULL)
}

draw_sankey_multi_fill_layer <- function(sankey, col, combine, gradient_slices,
                                         merge_length, fill_overlap, gradient_mode,
                                         merge_cols, node_cols,
                                         mid_col = NULL,
                                         flatten_trunks = TRUE,
                                         trunk_bg = par("bg"), ...) {
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
    trunk_bg = trunk_bg
  )
}

draw_sankey_multi_fill_raster <- function(sankey, col, combine, gradient_slices,
                                          merge_length, fill_overlap, gradient_mode,
                                          merge_cols, node_cols,
                                          mid_col = NULL,
                                          raster_res = 96,
                                          interpolate = TRUE,
                                          flatten_trunks = TRUE,
                                          trunk_bg = par("bg"), ...) {
  if(!requireNamespace("png", quietly = TRUE)) {
    stop("Package `png` is required for rasterized Sankey fills.", call. = FALSE)
  }

  gr_usr <- plotting_device_extent()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
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
  on.exit({
    if(grDevices::dev.cur() != main_dev) {
      grDevices::dev.off()
    }
  }, add = TRUE)
  par(mar = c(0, 0, 0, 0), xpd = NA)
  plot.new()
  plot.window(xlim = gr_usr[1:2], ylim = gr_usr[3:4], xaxs = "i", yaxs = "i")
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
    mid_col = mid_col,
    flatten_trunks = flatten_trunks,
    trunk_bg = trunk_bg,
    ...
  )
  grDevices::dev.off()

  rasterImage(
    png::readPNG(tmp),
    gr_usr[1], gr_usr[3], gr_usr[2], gr_usr[4],
    interpolate = interpolate,
    xpd = NA
  )

  invisible(NULL)
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
                              node_col = NULL,
                              merge_col = NULL,
                              merge_cols = NULL,
                              mid_col = NULL,
                              flatten_trunks = TRUE,
                              trunk_bg = par("bg"),
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
    xs <- c(sankey$flows$start_x, sankey$flows$end_x)
    ys <- c(sankey$flows$start_y, sankey$flows$end_y)
    pad_x <- diff(range(xs)) * 0.08
    pad_y <- max(sum(sankey$flows$width), diff(range(ys))) * 0.25
    plot.new()
    plot.window(
      xlim = range(xs) + c(-pad_x, pad_x),
      ylim = range(ys) + c(-pad_y, pad_y),
      asp = NA
    )
  }

  node_cols <- NULL
  if(!is.null(node_col)) {
    node_cols <- multi_sankey_node_colors(sankey, node_col, arg = "`node_col`")
  }

  if(!is.null(merge_col)) {
    merge_node_cols <- multi_sankey_node_colors(sankey, merge_col, arg = "`merge_col`")
    merge_cols <- merge_cols_from_node_cols(sankey, merge_node_cols)
    node_cols <- merge_node_cols
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

  if(all_raster) {
    visible_border <- any(vapply(stage_inds, function(i) {
      b <- stage_arg(border, i)
      length(b) == 1 && !is.na(b)
    }, logical(1)))
    max_gradient_slices <- max(vapply(stage_inds, function(i) {
      as.integer(round(stage_arg(gradient_slices, i)))
    }, integer(1)))
    alpha_cols <- c(
      unlist(lapply(stage_inds, function(i) sankey_flow_colors(sankey$segments[[i]], stage_arg(col, i)))),
      unlist(lapply(merge_cols, function(x) c(x$start, x$end)))
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
      default_fill_overlap(visible_border, max_gradient_slices, merge_length, alpha_cols)
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
      mid_col = mid_col,
      raster_res = stage_arg(raster_res, 1),
      interpolate = stage_arg(interp_raster, 1),
      flatten_trunks = flatten_trunks,
      trunk_bg = trunk_bg,
      ...
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
        trunk_start = trunk_start,
        trunk_end = trunk_end,
        merge_cols = merge_cols[[i]],
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
      trunk_bg = trunk_bg
    )
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
        labels = node$node,
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
                                trunk_start = TRUE,
                                trunk_end = TRUE,
                                node_col = NULL,
                                merge_col = NULL,
                                merge_cols = NULL,
                                mid_col = NULL,
                                flatten_trunks = TRUE,
                                trunk_bg = par("bg"),
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
    xs <- c(sankey$flows$start_x, sankey$flows$end_x)
    ys <- c(sankey$flows$start_y, sankey$flows$end_y)
    pad_x <- diff(range(xs)) * 0.08
    pad_y <- max(sum(sankey$flows$width), diff(range(ys))) * 0.25
    plot.new()
    plot.window(
      xlim = range(xs) + c(-pad_x, pad_x),
      ylim = range(ys) + c(-pad_y, pad_y),
      asp = NA
    )
  }

  draw_border <- length(border) == 1 && !is.na(border)
  fill_border <- if(draw_border) NA else border
  fill_cols <- sankey_flow_colors(sankey, col)
  if(!is.null(node_col)) {
    node_cols <- single_sankey_node_colors(sankey, node_col, arg = "`node_col`")
    merge_cols <- single_merge_cols_from_node_cols(sankey, node_cols)
  }
  if(!is.null(merge_col)) {
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
    if(!is.null(mid_cols)) {
      alpha_cols <- c(alpha_cols, mid_cols)
    }
    fill_overlap <- default_fill_overlap(draw_border, gradient_slices, merge_length, alpha_cols)
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
      mid_cols = mid_cols,
      ...
    )
  }

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
    points(sankey$from_nodes$x, sankey$from_nodes$y, pch = 21, bg = "white", col = from_col)
    points(sankey$to_nodes$x, sankey$to_nodes$y, pch = 21, bg = "white", col = to_col)
    text(
      sankey$from_nodes$x, sankey$from_nodes$y,
      labels = sankey$from_nodes$node, col = from_col, pos = 2, xpd = NA
    )
    text(
      sankey$to_nodes$x, sankey$to_nodes$y,
      labels = sankey$to_nodes$node, col = to_col, pos = 4, xpd = NA
    )
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
