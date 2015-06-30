# Helper function to convert Matlab numeric times to R POSIXct timestamps
mt2rt = function(x, timez = "UTC") {
  days = x - 719529   # 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  as.POSIXct(secs, origin = '1970-01-01', tz = timez)
}

# helper function to extract a numeric YYYMMDD from a filepath
date_from_fpath = function(x){
  m = tail(unlist(strsplit(x, "/")), 1)
  m = unlist(regmatches(x, gregexpr("[[:digit:]]+", x) ))
  as.Date(m[which(nchar(m) == 8)], format = "%Y%m%d")
}

# helper function to extract water level gauge sites from a file path
site_from_fpath = function(x){
  m = tolower(tail(unlist(strsplit(x, "/")), 1))
  if(grepl("sheephouse|shp", m))
    "sheephouse creek"
  else if (grepl("mouth|mou", m))
    "river mouth"
  else if (grepl("jenner", m))
    "jenner"
  else if (grepl("mos|moscow", m))
    "moscow bridge"  
  else
    "unknown"
}

#' Combine Water Level Records
#'
#' Combine water level data files into a dataframe.
#'
#' @param wll A list of dataframes of water level data
#' @param timecol The column name containing matlab numeric datetime. Matlab
#'    times are assumed to be in "UTC" timezone.
#' @param depthcol The column name containing depths.
#' @param sitecol The column name to contain site data.
#'
#' @export
merge_rrewll = function(wll, timecol = "mtime", depthcol = "depth", 
  sitecol = "site"){
  # extract and merge dataframes of mtime and depth
  for(n in names(wll))
    wll[[n]][sitecol] = site_from_fpath(n)
  d = do.call(rbind, lapply(wll, function(x) x[c(sitecol, timecol, depthcol)]))
  rownames(d) = NULL
  d[timecol] = mt2rt(d[[timecol]])
  d
}

#' Combine CTD Transects
#
#' Combine CTD transect data files into a dataframe.
#'
#' @param ctd A list of dataframes of CTD transect data
#' @param dates Vector of dates of each transect listed in \code{ctd}. If
#'   \code{NULL}, will search for numeric YYYYMMDD in \code{names(ctd)}.
#' @param timecol The column name to be created containing the transect dates. 
#'
#' @export
merge_rrectd = function(ctd, dates = NULL, timecol = "date"){
  if(is.null(dates))
    dates = as.Date(sapply(names(ctd), date_from_fpath), origin = "1970-01-01")
  for(i in seq(length(ctd)))
    ctd[[i]][timecol] = dates[[i]]
  d = do.call(rbind, ctd)
  rownames(d) = NULL
  d
}

#' Add Cast Depth
#'
#' Add cast depths to CTD transect data.
#'
#' @param d A dataframe of CTD transect data.
#' @param depthcol The column name to assign depth data to.
#' @param elevcol The column name containing elevation.
#' @param surfelevcol The column name containing water surface elevation.
#' @return The dataframe \code{d} with an additional column \code{depthcol}.
#'
#' @details Depth is calculated as \code{depthcol = surfelevcol - elevcol}.
#'
#' @export
depth_from_elev = function(d, depthcol = "depth", elevcol = "elev", 
  surfelevcol = "surfelev"){
  d[depthcol] = d[[surfelevcol]] - d[[elevcol]] 
  d
}
