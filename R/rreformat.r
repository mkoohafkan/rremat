# Helper function to convert Matlab numeric times to R POSIXct timestamps
mt2rt = function(x, timez = "UTC") {
  days = x - 719529   # 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  as.POSIXct(secs, origin = '1970-01-01', tz = timez)
}

# helper function to extract a numeric YYYMMDD from a filepath
date_from_fpath = function(x){
  m = unlist(regmatches(x, gregexpr("[[:digit:]]+", x) ))
  as.Date(m[which(nchar(m) == 8)], format = "%Y%m%d")
}

# helper function to extract the ID from from a CTD filepath
# index is first alpha after YYYYMMDD and  before "_"
id_from_fpath = function(x){
  m = tail(unlist(strsplit(x, "/")), 1)
  m = head(unlist(strsplit(m, "_")), 1)
  gsub("[[:digit:]]", "", m)
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
  else if (grepl("heron", m))
    "heron rookery"
  else
    "unknown"
}

# helper function to get gauge locations from file path
gauge_from_fpath = function(x){
  m = unlist(strsplit(tail(unlist(strsplit(x, c("/"))), 1), ".csv"))  
  if(grepl("russianriver", m))
    "russian river"
  else if (grepl("austincreek", m))
    "austin creek"
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
#' @param elevcol The column name containing elevations.
#' @param tempcol The column name containing temperatures.
#' @param sitecol The column name to contain site data.
#'
#' @seealso read_rrewll
#'
#' @export
merge_rrewll = function(wll, timecol = "mtime", depthcol = "depth", 
  elevcol = "elev", tempcol = "temp", sitecol = "site"){
  for(n in names(wll)){
    # get date from filename
    wll[[n]][sitecol] = site_from_fpath(n)
    # add empty temperature column if missing
    if(!(tempcol %in% names(wll[[n]])))
      wll[[n]][tempcol] = NA
    # add empty elevation column if missing
    if(!(elevcol %in% names(wll[[n]])))
      wll[[n]][elevcol] = NA      
  }
  # merge dataframes
  d = do.call(rbind, lapply(wll, function(x) 
    x[c(sitecol, timecol, depthcol, elevcol, tempcol)]))
  rownames(d) = NULL
  # convert matlab time to POSIX time
  d[timecol] = mt2rt(d[[timecol]])
  d
}

#' Combine CTD Transects
#
#' Combine CTD transect data files into a dataframe.
#'
#' @param ctd A list of dataframes of CTD transect data.
#' @param dates Vector of dates of each transect listed in \code{ctd}. If
#'   \code{NULL}, will search for numeric YYYYMMDD in \code{names(ctd)}.
#' @param timecol The column name to be created containing the transect dates. 
#'
#' @seealso read_rrectd read_rrectdgrid
#'
#' @export
merge_rrectd = function(ctd, dates = NULL, ids = NULL, timecol = "date", 
  idcol = "id"){
  # get date from filepath
  if(is.null(dates))
    dates = as.Date(sapply(names(ctd), date_from_fpath), origin = "1970-01-01")
  if(is.null(ids))
    ids = sapply(names(ctd), id_from_fpath)
  for(i in seq(length(ctd))){
    ctd[[i]][timecol] = dates[[i]]
    ctd[[i]][idcol] = ids[[i]]
  }
  d = do.call(rbind, ctd)
  rownames(d) = NULL
  d
}

#' Combine Tide Records
#'
#' Combine data files produced by \code{download_tides}.
#'
#' @param tides A list of dataframes of tide data.
#' @param datetimecol The column name containing timestamps.
#' @param heightcol The column name containing water levels.
#' @param sigmacol The column name containing sigma values.
#' @return A single dataframe of tide data.
#'
#' @seealso download_tides
#'
#' @export
merge_tides = function(tides, datetimecol = "Date.Time", 
  heightcol = "Water.Level", sigmacol = "Sigma"){
  d = vector("list", length = length(tides))
  for(i in seq(length(tides)))
    d[[i]] = data.frame(datetime = as.POSIXct(tides[[i]][[datetimecol]], 
      tz = "UTC"), height = tides[[i]][[heightcol]], 
      sigma = tides[[i]][[sigmacol]])
  do.call(rbind.data.frame, d)
}

#' Combine Streamflow Records
#'
#' Combine streamflow records obtained from the USGS.
#'
#' @param sf A list of dataframes of streamflow data.
#' @param datetimecol Column name containing timestamps.
#' @param flowcol Column name containing discharge.
#' @return A single dataframe of streamflow data.
#'
#' @export
merge_streamflow = function(sf, datetimecol = "datetime", 
  flowcol = "discharge"){
  sites = sapply(names(sf), gauge_from_fpath)
  d = vector("list", length = length(sf))
  for(i in seq(length(sf)))
    d[[i]] = data.frame(gauge = sites[[i]], 
      datetime = as.POSIXct(sf[[i]][[datetimecol]], tz = "US/Pacific"),
      flow = sf[[i]][[flowcol]])
  do.call(rbind.data.frame, d)
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
#' @seealso read_rrectd read_rrectdgrid merge_rrectd
#' 
#' @export
depth_from_elev = function(d, depthcol = "depth", elevcol = "elev", 
  surfelevcol = "surfelev"){
  d[depthcol] = d[[surfelevcol]] - d[[elevcol]] 
  d
}

#' Correct Transect Distance Markers
#'
#' Correct transect distance markers in CTD transect data.
#'
#' @param d A dataframe of CTD transect data.
#' @param distcol The column name containing distance data to correct.
#' @return The dataframe \code{d} with corrected values in column 
#'   \code{distcol}.
#'
#' @details Distances are corrected and converted from km to m by comparing GPS 
#'   coordinates and transect distance markers to RRE bathymetry. Distance 
#'   corrections are shown below:
#'   \itemize{
#'     \item 0.3 => 400
#'     \item (1.1, 1.2) => 1100
#'     \item 1.5 => 4800 (typo)
#'     \item (2.4, 2.5) => 2600
#'     \item (3.1, 3.2, 3.5) => 3800
#'     \item 4.0 => 4300
#'     \item 4.6 => 4800
#'     \item 5.3 => 5700
#'     \item 6.4 => 6900
#'     \item (7.3, 7.4) => 7900
#'     \item 8.7 => 9300
#'     \item 9.5 => 10100
#'     \item (10.0, 10.1) => 10800
#'   }
#'
#' @seealso read_rrectd read_rrectdgrid merge_rrectd
#'
#' @export
correct_dist = function(d, distcol = "dist"){  
  riverdist = data.frame(dist =  c(0.3, 1.1, 1.2, 1.5, 2.4, 2.5, 3.1, 3.2, 3.5, 
    4.0, 4.6, 5.3, 6.4, 7.3, 7.4, 8.7, 9.5, 10.0, 10.1), distcor = c(400, 1100,
    1100, 4800, 2600, 2600, 3800, 3800, 3800, 4300, 4800, 5700, 6900, 7900, 
    7900, 9300, 10100, 10800, 10800))
    
  d[distcol] = sapply(d[[distcol]], function(x) 
    riverdist$distcor[which.min(abs(x - riverdist$dist))])    
  d
}

#' Add Volume
#'
#' Add water volume contained in depth and distance zones.
#'
#' @param d Dataframe containing physical data, e.g. \code{data(grids)}.
#' @param v Dataframe containing volume lookup information, e.g. 
#'   \code{data(volumes)}. Assumes first 3 columns are x, y, and z resolution 
#'   respectively.
#' @param zcol Column name containing vertical markers, e.g. \code{elev}.
#' @param xcol Column name containing longitudinal markers, e.g. \code{dist}.
#' @return The dataframe \code{d} with the additional column \code{volume}.
#'
#' @importFrom dplyr inner_join
#' @export
add_volume = function(d, v, r, zcol = "elev", xcol = "dist"){
  retnames = names(d)
  cellvol = prod(r[1:3])
  f = inner_join(d, v, by = c(xcol, zcol))
  f['count'] = as.integer(f$count)
  f['volume'] = f$count*cellvol
  f[c(retnames, 'volume')]
}
