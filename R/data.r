#' CTD Transect Meta Data
#'
#' Meta data for CTD transects. Structured as:
#' \itemize{
#'   \item \code{start} timestamp of transect commencement
#'   \item \code{end} timestamp of transect completion
#'   \item \code{note} additional note on e.g. mouth state
#' }
#' @details Transect meta data was collected from the Russian River Estuary 
#'   Circulation and Water Quality Data Reports (2011, 2012, 2013, 2014) 
#'   submitted to the Sonoma County Water Agency.
#' @docType data
#' @keywords datasets
#' @name ctdmeta
#' @usage data(ctdmeta)
#' @format A data frame with 3 variables
"ctdmeta"


#' CTD Cast Location Data
#'
#' CTD Cast locations. Structured as:
#' \itemize{
#'   \item \code{station} CTD cast location number
#'   \item \code{name} CTD cast location name
#'   \item \code{latitude} CTD cast location latitude
#'   \item \code{longitude} CTD cast location longitude
#'   \item \code{dist} CTD cast location longitudinal distance from mouth (m)
#' }
#' @details CTD cast location data was collected from the Russian River Estuary 
#'   Circulation and Water Quality Data Reports (2011, 2012, 2013, 2014) 
#'   submitted to the Sonoma County Water Agency.
#' @docType data
#' @keywords datasets
#' @name ctdlocations
#' @usage data(ctdlocations)
#' @format A data frame with 5 variables
"ctdlocations"

#' Interpolated CTD Grids
#' 
#' Interpolated CTD grids constructed using natural neighbor interpolation in 
#' Matlab. Structured as:
#' \itemize{
#'   \item \code{sa} salinity (PSU)
#'   \item \code{ta} temperature (degrees C)
#'   \item \code{oa} dissolved oxygen (mg/L)        
#'   \item \code{dist} longitudinal distance from mouth (m)
#'   \item \code{elev} elevation (m NAVD29)
#'   \item \code{date} cast date
#' }
#' @docType data
#' @keywords datasets
#' @name grids
#' @usage data(grids)
#' @format A data frame with 6 variables
#' @seealso read_rrectdgrid merge_rrectd depth_from_elev
"grids"

#' CTD Cast Data
#' 
#' CTD cast data collected by the Largier Lab and processed using 
#' \code{rremat}. Structured as:
#' \enumerate{
#'   \item \code{date} cast date
#'   \item \code{dist} longitudinal distance from mouth (m)
#'   \item \code{surfelev} water surface elevation (m NAVD29)
#'   \item \code{elev} elevation (m NAVD29)
#'   \item \code{depth} cast depth (m)
#'   \item \code{ta} temperature (degrees C)
#'   \item \code{sa} salinity (PSU)
#'   \item \code{da}       
#'   \item \code{oa} dissolved oxygen (mg/L)        
#'   \item \code{sat} dissolved oxygen percent saturation (%)
#'   \item \code{fl} fluorescence (ug/L))
#'   \item \code{bt} beam transmission (%)
#'   \item \code{par} photosynthetically-active radiation (umol^-1 m^-2)
#'   \item \code{ph} pH
#' }
#' @docType data
#' @keywords datasets
#' @name ctd
#' @usage data(ctd)
#' @format A data frame with 14 variables
#' @seealso read_rrectd merge_rrectd depth_from_elev correct_dist
"ctd"

#' Water Level Data
#' 
#' Pressure gauge water Level data collected by the Largier Lab and processed 
#' using \code{rremat}. Structured as:
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
#' @seealso read_rrewll merge_rrewll
"wll"

#' Volume Lookup Table
#' 
#' Volume lookup table constructed from RRE bathymetry. Structured as:
#' \itemize{
#'   \item \code{dist} distance along the thalweg marking the edge of each zone
#'     furthest from the mouth, e.g. \code{dist = 100} identifies the region
#'     spanning 0 meters and 100 meters along the thalweg.
#'   \item \code{elev} elevation marking the the top of the depth increment, 
#'     i.e. \code{elev = 0.1} identifies the region spanning 0 and 0.1 meters 
#'     elevation.
#'   \item \code{count} the total number of cells contained within each zone 
#'     and vertical increment.
#' }
#' @details The volume lookup table was generated from the RRE bathymetry DEM 
#'   and zone delineations based on a manual trace of the river thaleweg. The 
#'   volume cells are defined as having a length of 0.3048 meters (1 foot),
#'   width of 0.3048 meters, and height of 0.1 meters (cell volume of 
#'   0.009290304 cubic meters). Zones are 
#'   delineated in 100-meter increments along the thalweg, and each zone is 
#'   divided in the vertical into 0.1-meter increments from a minimum elevation 
#'   of -15.8 meters to a maximum elevation of 2.6 meters. The lookup table 
#'   lists the count of volume elements in each vertical increment in each 
#'   zone, i.e. each row lists the number of elements between river distances 
#'   \code{d} and \code{d - 100} and between depths \code{z} and 
#'   \code{z - 0.1}. 
#' @docType data
#' @keywords datasets
#' @name volumes
#' @usage data(volumes)
#' @format A data frame with 3 variables
"volumes"

#' Estuary Closures
#' 
#' Estuary closures identified from water level and photographic data. 
#' Structured as:
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
"closures"
