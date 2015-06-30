#' Estuary Closures
#' 
#' Estuary closures identified from water level and photographic data. 
#'   Structured as:
#' \enumerate{
#'   \item \code{id} (arbitary) closure id
#'   \item \code{initiation} date of closure initiation
#'   \item \code{breach} date of mouth breach
#' }
#' @docType data
#' @keywords datasets
#' @name closures
#' @usage data(closures)
#' @format A data frame with 3 variables
NULL

#' CTD Cast Data
#' 
#' CTD cast data collected by the Largier Lab and processed using 
#'   \code{rremat}. Structured as:
#' \enumerate{
#'   \item \code{date} cast date
#'   \item \code{dist} longitudinal distance from mouth (km)
#'   \item \code{surfelev} water surface elevation (m NAVD29)
#'   \item \code{elev} elevation (m NAVD29)
#'   \item \code{depth} cast depth (m)
#'   \item \code{ta} temperature (degrees C)
#'   \item \code{sa} salinity (PSU)
#'   \item \code{da}       
#'   \item \code{oa} dissolved oxygen (mg/L)        
#'   \item \code{sat} dissolved oxygen percent saturation (%)
#'   \item \code{fl} fluorescence     
#'   \item \code{bt}
#'   \item \code{par}
#'   \item \code{ph} pH
#' }
#' @docType data
#' @keywords datasets
#' @name ctd
#' @usage data(ctd)
#' @format A data frame with 14 variables
NULL

#' Water Level Data
#' 
#' Pressure gauge water Level data collected by the Largier Lab and processed 
#'   using \code{rremat}. Structured as:
#' \itemize{
#'   \item \code{site} gauge location
#'   \item \code{mtime} timestamp (UTC)
#'   \item \code{depth} water depth (m)
#' }
#' @docType data
#' @keywords datasets
#' @name wll
#' @usage data(wll)
#' @format A data frame with 3 variables
NULL

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
#' @import R.matlab
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
    dlength = max(unique(it))
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add attributes
    acols = names(obj[!(names(obj) %in% dcols)])
    if(!quiet)
      message("...attributes are: ", paste(acols, collapse = ", "))
    for(acol in acols)
      attr(dobj, acol) <- obj[[acol]]
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
#' @import R.matlab
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
    dlength = max(unique(it))
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
        dobj[which(dobj[[distcol]] == lobj[[surfdistcol]][i]), lcol] = lobj[[lcol]][i]      
    }
    # add attributes
    if(all(names(it) %in% c(dcols, lcols)))
      if(!quiet)
        message("...no additional attributes")
    else{
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
