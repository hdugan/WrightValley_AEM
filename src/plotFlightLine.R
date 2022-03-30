#dvdpLocation is UTMX, UTMY, depth. ex) c(416673, 1391082, 85.73)

plotFlightLine <- function(lineNo, aemDF, dvdpLocation = NULL, dvdpLocation2 = NULL) {
  # Derive depths of each layer 
  depths = aemDF |> filter(LINE_NO == lineNo) |> 
    dplyr::select(DEP_TOP_1:DEP_TOP_30) |> 
    pivot_longer(cols = DEP_TOP_1:DEP_TOP_30, names_to = "bin", values_to = "depths") |> 
    mutate(bin = parse_number(bin)) |> 
    group_by(bin) |> 
    summarise_all(first)

  # Get depth of DOI
  doi = aemDF |> filter(LINE_NO == lineNo) |>
    dplyr::select(UTMX, UTMY, DOI_CONSERVATIVE)

  # Get distance between points
  flightDist = aemDF |> filter(LINE_NO == lineNo) |>
    dplyr::select(UTMX, UTMY) |>
    mutate(row = row_number())
  # Make all lines plot west to east
  WE = -1
  if(flightDist |> slice(1) |> pull(UTMX) > flightDist |> slice(n()) |> pull(UTMX)) {
    flightDist = flightDist |> map_df(rev)
    WE = 1 # This comes into play later for blackout boxes
  }

  flightDist = flightDist |>
    mutate(across(.fns = lag, .names = '{col}_next')) |>
    rowwise() |>
    mutate(dist = pointDistance(c(UTMX, UTMY),c(UTMX_next, UTMY_next), method = 'Euclidean', lonlat = F)[1]) |>
    mutate(dist = if_else(is.na(dist), 0, dist)) |>
    ungroup() |>
    mutate(distcum = cumsum(dist)) |>
    arrange(row)

  # add in thickness and DOIs and flightDist
  a = aemDF |> filter(LINE_NO == lineNo) |>
    dplyr::select(UTMX, UTMY, ELEVATION, RHO_I_1:RHO_I_30) |>
    pivot_longer(cols = RHO_I_1:RHO_I_30, names_to = "bin", values_to = "rho") |>
    mutate(bin = parse_number(bin)) |>
    left_join(depths) |>
    left_join(doi) |>
    left_join(flightDist) |>
    mutate(depth.elev = round(ELEVATION - depths), doi.elev = round(ELEVATION - DOI_CONSERVATIVE))
  
  
  # No 30 bin for 2011 flight lines
  if (any(is.na(a |> filter(bin ==30) |> pull(rho)))) {
    a = a |> filter(bin != 30)
  }
  
  mindoi = min(a$depth.elev)
  
  # b = data.frame(new.depths = seq(min(a$depth.elev), max(a$depth.elev), by = 1))
  b = expand_grid(distcum = unique(a$distcum), depth.elev = seq(min(a$depth.elev, na.rm = T), max(a$depth.elev, na.rm = T), by = 1)) |>
    left_join(a) |>
    group_by(distcum) %>%
    mutate(rhoInterp = na.approx(rho, na.rm=FALSE, method = 'linear')) |>
    mutate(rhoInterp = if_else(rhoInterp < 1, 1, rhoInterp)) |>
    mutate(rhoInterp = if_else(rhoInterp > 1e4, 1e4, rhoInterp)) |>
    mutate(rhoBackground = if_else(!is.na(rhoInterp), 1e4, NA_real_)) |>
    ungroup()
  
  # Get bathymetry
  # test extracting depths over line
  testline = a |> filter(bin == 1) |> dplyr::select(UTMX,UTMY)
  coordinates(testline) <- ~UTMX+UTMY
  raster::crs(testline) = raster::crs(DEM3)
  testline.depths <- raster::extract(DEM3,             # raster layer
                                     testline,   # SPDF with centroids for buffer
                                     method = 'simple',
                                     buffer = 5,     # buffer size, units depend on CRS
                                     fun = median,         # what to value to extract
                                     df = FALSE)         # return a dataframe?
  
  testline.df = a |> filter(bin == 1) |>
    dplyr::select(UTMX,UTMY, ELEVATION, distcum) |>
    mutate(bath.depth = testline.depths) |>
    mutate(bathy = ELEVATION - bath.depth)
  
  # Extract DVDP borehole location for plot 
  if (!is.null(dvdpLocation)) {
  dvdpLine = a |> filter(bin == 1) |> 
    dplyr::select(UTMX,UTMY, ELEVATION, distcum) |> 
    mutate(UTMXd = dvdpLocation[1], UTMYd = dvdpLocation[2]) |> 
    mutate(UTMxDIFF = abs(UTMX - UTMXd), UTMyDIFF = abs(UTMY - UTMYd)) |> 
    filter(UTMxDIFF == min(UTMxDIFF))
  }
  
  if (!is.null(dvdpLocation2)) {
  dvdpLine2 = a |> filter(bin == 1) |> 
    dplyr::select(UTMX,UTMY, ELEVATION, distcum) |> 
    mutate(UTMXd = dvdpLocation2[1], UTMYd = dvdpLocation2[2]) |> 
    mutate(UTMxDIFF = abs(UTMX - UTMXd), UTMyDIFF = abs(UTMY - UTMYd)) |> 
    filter(UTMxDIFF == min(UTMxDIFF))
  }
  
  # CONTINUOUS SCALE - Interpolated rho
  # Get black out zones
  rr = b |>
    filter(bin == 1) |>
    mutate(index = row_number()) |>
    mutate(test = dist>100) %>%
    filter(test == TRUE) |>
    pull(index)
  
  blackout = b |>
    filter(bin == 1) |>
    mutate(index = row_number()) |>
    filter(index %in% rr | index %in% (rr+WE))
  
  # Plot resistivity
  options(scipen = 1)
  mybreaks = seq(0,4)
  mylabels = 10^mybreaks
  
  # Dataframe for DOI polygon
  b.ribbon = b |>
    filter(!is.na(DOI_CONSERVATIVE)) |>
    filter(bin == 30) |>
    mutate(doi.elev = if_else(doi.elev < depth.elev, depth.elev, doi.elev))
  
  #Plots using contours
  p2 = ggplot(b) +
    geom_contour_fill(aes(x = distcum, y = depth.elev, z = log10(rhoBackground))) + # Interpolation can leave gaps... this fills in with max limit of 1e4
    geom_contour_fill(bins = 20, aes(x = distcum, y = depth.elev, z = log10(rhoInterp))) +
    scale_fill_gradientn(name = 'rho (Ω-m)', colours = rev(met.brewer("Hiroshige", n=100)),
                         limits = log10(c(1, 1e4)),
                         breaks = mybreaks, labels = mylabels,
                         oob = scales::squish) +
    geom_ribbon(data = b.ribbon,
                aes(x = distcum, ymax = doi.elev, ymin = mindoi), alpha = 0.8) + # DOI ribbon
    geom_path(data = testline.df, aes(x = distcum, y = bathy)) + # lake bathymetry
    xlab('Distance W → E (m)') + ylab('Elevation (m)') +
    theme_minimal(base_size = 8)
  
  if (!is.null(dvdpLocation)) {
    p2 = p2 + geom_linerange(data = dvdpLine, aes(x = distcum, ymin = ELEVATION - dvdpLocation[3], ymax = ELEVATION), color = 'black', size = 1) # DVDP borehole 
  }
  if (!is.null(dvdpLocation2)) {
    p2 = p2 + geom_linerange(data = dvdpLine2, aes(x = distcum, ymin = ELEVATION - dvdpLocation2[3], ymax = ELEVATION), color = 'black', size = 1) # DVDP borehole 
  }
  if(nrow(blackout) == 2) {
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[1], xmax = distcum[2], ymin = mindoi, ymax = ELEVATION[1])) # blackout
  }
  if(nrow(blackout) > 2) {
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[1], xmax = distcum[2], ymin = mindoi, ymax = ELEVATION[1])) # blackout
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[3], xmax = distcum[4], ymin = mindoi, ymax = ELEVATION[3])) # blackout
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[5], xmax = distcum[6], ymin = mindoi, ymax = ELEVATION[5])) # blackout
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[7], xmax = distcum[8], ymin = mindoi, ymax = ELEVATION[7])) # blackout
    p2 = p2 + geom_rect(data = blackout, aes(xmin = distcum[9], xmax = distcum[10], ymin = mindoi, ymax = ELEVATION[9])) # blackout
  }
  
  return(p2)
}
