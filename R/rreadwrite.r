# function to get the most common value in a vector
commonval <- function(x){
  tr = as.data.frame(unclass(rle(sort(x))))[,2:1]
  tr$values[tr$lengths == max(tr$lengths)][1]
}

#' Read Water Level Data 
#'
#' Read water level data from Largier Lab Matlab file
#'
#' @param fpath Path to input data.
#' @param quiet Suppress messages during data processing.
#' @return A dataframe with named columns. Additional elements (e.g. note) are
#'   added as attributes.
#'
#' @details Largier Lab Matlab data is typically formatted as a Matlab  
#'   structure with multiple equal-length data columns and an optional note.
#'
#' @importFrom R.matlab readMat
#' @export
read_rrewll = function(fpath, quiet = FALSE){
  if(!quiet)
    message("Reading '", fpath, "'")  
  # read the file
  mobj = readMat(fpath)
  mobjn = names(mobj)
  if(!quiet)
    message("Found variables: ", paste(mobjn, collapse = ", "))
  ret = vector("list", length = length(mobjn))
  names(ret) = mobjn
  # loop through variables in file
  for(n in mobjn){
    if(!quiet)
      message("Extracting ", n, "...")
    obj = mobj[[n]][,1,1]
    it = sapply(obj, length)
    if(!quiet)
      message("...contains entries: ", paste(names(it), collapse = ", "))    
    # coerce to dataframe
    dlength = commonval(it)
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add attributes
    if(all(names(it) %in% dcols)){
      if(!quiet)
        message("...no additional attributes")
    } else{
    acols = names(obj[!(names(obj) %in% dcols)])
      if(!quiet)
        message("...attributes are: ", paste(acols, collapse = ", "))
      for(acol in acols)
        attr(dobj, acol) <- obj[[acol]]
    }
    ret[[n]] = dobj
  }
  attr(ret, "sourcefile") = fpath
  if(!quiet){
    message("Output list contains: ", paste("'", names(ret), "'", 
      sep = "", collapse = ", "))
    message("Attribute 'sourcefile' is ", '"', attr(ret, "sourcefile"), '"')
    message("DONE")
  }
  return(ret)  
}  

#' Read CTD Data 
#'
#' Read CTD transect data from Largier Lab Matlab file.
#'
#' @param fpath Path to input data.
#' @param quiet Suppress messages during data processing.
#' @param distcol Column name containing distance upstream for each cast 
#'   location.
#' @param surfdistcol Column name containing water surface elevation for each 
#'   cast location.
#' @return A dataframe with named columns. Additional elements (e.g. note) are
#'   added as attributes.
#'
#' @details Largier Lab Matlab data is typically formatted as a Matlab  
#'   structure with multiple equal-length data columns and two columns  
#'   'surfdist' and 'surfelev' with surface information for each cast location.  
#'   'Surfdist' and 'surfelev' are used to populate an expanded 'surfelev' 
#'   column corresponding to data column 'elev'.
#'
#' @importFrom R.matlab readMat
#' @export
read_rrectd = function(fpath, quiet = FALSE, distcol = "dist", 
  surfdistcol = "surfdist"){
  if(!quiet)
  message("Reading '", fpath, "'")
  mobj = readMat(fpath)
  mobjn = names(mobj)
  if(!quiet)
    message("Found variables: ", paste(mobjn, collapse = ", "))
  ret = vector("list", length = length(mobjn))
  names(ret) = mobjn
  # loop through variables in file
  for(n in mobjn){
    if(!quiet)
      message("Extracting ", n, "...")
    obj = mobj[[n]][,1,1]
    it = sapply(obj, length)
    if(!quiet)
      message("...contains entries: ", paste(names(it), collapse = ", "))    
    # coerce to dataframe
    dlength = commonval(it)
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add location data
    llength = it[[surfdistcol]]
    lcols = names(it[it == llength])
    if(!quiet)
      message("...location columns are: ", paste(lcols, collapse = ", "))      
    lobj = lapply(obj[lcols], as.vector)
    dobj[lcols[lcols != surfdistcol]] = NA
    if(!quiet)
      message("...pairing: ", paste(distcol, surfdistcol, sep = " <=> "))      
    for(i in seq(length(lobj[[surfdistcol]]))){
      for(lcol in lcols[lcols != surfdistcol])
        dobj[which(dobj[[distcol]] == lobj[[surfdistcol]][i]), 
             lcol] = lobj[[lcol]][i]      
    }
    # add attributes
    if(all(names(it) %in% c(dcols, lcols))){
      if(!quiet)
        message("...no additional attributes")
    } else{
      acols = names(it)[!(names(it) %in% c(dcols, lcols))]
      if(!quiet)
        message("...attributes are: ", paste(acols, collapse = ", "))
      for(acol in acols)
        attr(dobj, acol) <- obj[[acol]]
    }
    ret[[n]] = dobj
  }
  attr(ret, "sourcefile") = fpath
  if(!quiet){
    message("Output list contains: ", paste("'", names(ret), "'", 
      sep = "", collapse = ", "))
    message("Attribute 'sourcefile' is ", '"', attr(ret, "sourcefile"), '"')
    message("DONE")
  }
  return(ret)  
}

#' Read interpolated CTD grid 
#'
#' Read interpolated grid of CTD data from Largier Lab Matlab file.
#'
#' @param fpath Path to input data.
#' @param quiet Suppress messages during data processing.
#' @return A dataframe with named columns. Additional elements (e.g. note) are
#'   added as attributes.
#'
#' @details Largier Lab Matlab data is typically formatted as a Matlab  
#'   structure with multiple equal-size matrices.
#'
#' @importFrom R.matlab readMat
#' @export
read_rrectdgrid = function(fpath, quiet = FALSE){
  if(!quiet)
  message("Reading '", fpath, "'")
  mobj = readMat(fpath)
  mobjn = names(mobj)
  if(!quiet)
    message("Found variables: ", paste(mobjn, collapse = ", "))
  ret = vector("list", length = length(mobjn))
  names(ret) = mobjn
  # loop through variables in file
  for(n in mobjn){
    if(!quiet)
      message("Extracting ", n, "...")
    obj = mobj[[n]][,1,1]
    it = sapply(obj, length)
    if(!quiet)
      message("...contains entries: ", paste(names(it), collapse = ", "))    
    # coerce to dataframe
    dlength = max(unique(it))
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add attributes
    if(all(names(it) %in% dcols)){
      if(!quiet)
        message("...no additional attributes")
    } else{
      acols = names(it)[!(names(it) %in% dcols)]
      if(!quiet)
        message("...attributes are: ", paste(acols, collapse = ", "))
      for(acol in acols)
        attr(dobj, acol) <- obj[[acol]]
    }
    ret[[n]] = dobj
  }
  attr(ret, "sourcefile") = fpath
  if(!quiet){
    message("Output list contains: ", paste("'", names(ret), "'", 
      sep = "", collapse = ", "))
    message("Attribute 'sourcefile' is ", '"', attr(ret, "sourcefile"), '"')
    message("DONE")
  }
  return(ret)  
}

#' Download NOAA tide data
#' 
#' Download data from NOAA Tides & Currents web API.
#'
#' @param f Destination file.
#' @param begin_date Character or integer start date of date request in format
#'   YYYYMMDD.
#' @param end_date Character or integer end date of date request in format
#'   YYYYMMDD. must be no more than 365 days later than \code{start_date}.
#' @param product Data product, default is hourly ocean water height.
#' @param station Data station to download data from. Default is Point Reyes 
#'   Buoy. 
#' @param datum Vertical datum that data product is references to. Default
#'   is mean lower low water (MLLW).
#' @param units units used for data product. Default is metric units.
#' @param time_zone Time zone used in data product. Default is GMT.
#' @param ... Other arguments to pass to \code{download.file()}.
#' @return The destination file path.
#' 
#' @details The NOAA Tides & Currents web API limits data requests to 365 days
#'   or less.
#'
#' @export 
download_tides = function(f = tempfile(), begin_date, end_date, 
  product = "hourly_height", station = "9415020", datum = "MLLW", 
  units = "metric", time_zone = "GMT", ...){
  if(as.Date(paste(end_date), format = "%Y%m%d") - 
    as.Date(paste(begin_date), format = "%Y%m%d") > 365)
    stop("'begin_date' and 'end_date' must be less than 365 days apart.")
  noaaurl = paste0("http://tidesandcurrents.noaa.gov/api/datagetter?product=",
    product, "&application=NOS.COOPS.TAC.WL&station=", station, "&begin_date=",
    begin_date, "&end_date=", end_date, "&datum=", datum, "&units=", units,
    "&time_zone=", time_zone,"&format=csv")
  download.file(noaaurl, f, ...)
  return(f)
}

#' Download USGS streamflow data
#' 
#' Download data from USGS web API.
#'
#' @param f Destination file.
#' @param begin_date Character start date of date request in format
#'   "YYYY-MM-DD".
#' @param end_date Character end date of date request in format
#'   "YYYY-MM-DD".
#' @param product Data product, default is streamflow.
#' @param station Data station to download data from. Default is Guerneville.
#'   Austin Creek station is second element of default value.   
#' @param ... Other arguments to pass to \code{download.file()}.
#' @return The destination file path.
#' 
#' @details The NOAA Tides & Currents web API limits data requests to 365 days
#'   or less.
#'
#' @export 
download_streamflow = function(f = tempfile(), begin_date, end_date, 
  product = "00060", station = c("11467000","11467200"), ...){
  usgsurl = paste0("http://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_", product, 
    "=on&format=rdb&site_no=", station[1], "&period=&begin_date=", begin_date, 
    "&end_date=", end_date)
  download.file(usgsurl, f, ...)
  return(f)
}

#' Read Bathymetry Tables
#'
#' Read and combine the bathymetry tables.
#'
#' @param files A list of files containing bathymetry data. Each file is 
#'   expected to have columns \code{OID}, \code{dist}, \code{count}, 
#'   \code{area}, \code{sum}, and \code{bedelev}.
#' @return A data frame with columns, \code{bedelev}, \code{dist}, \code{area}, 
#'   \code{count}, and \code{sum}.
#'
#' @export
read_bathymetry_tables = function(files){
  d = do.call(rbind.data.frame, lapply(files, read.csv))
  names(d) = c("OID", "dist", "count", "area", "sum", "bedelev")
  d[c("bedelev", "dist", "area", "count", "sum")]
}
