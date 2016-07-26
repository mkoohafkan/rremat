#' Continuous Habitat Suitability Function Generator
#'
#' Generate a continuous habitat suitability function given parameter values 
#' and scores.
#'
#' @param value Vector of parameter values, or a dataframe containing 
#'   parameters values in the first column and scores in the second column. 
#'   \code{value} must be in ascending order.
#' @param score Vector of scores assigned to parameter values specified in 
#'   \code{values}.
#' @param scoreleft the value to be returned when input values are less than 
#'   \code{min(value)}.
#' @param scoreright the value to be returned when input values are greater 
#'   than \code{max(value)}.
#' @param spline.expand Add curvature to the habitat suitability curve by 
#'   expanding the resolution of \code{(value, score)} and fitting a spline 
#'   using \code{spline()}. If \code{spline.expand = 1}, the habitat 
#'   suitability curve is generated as a linear interpolation between 
#'   \code{(value, score)} pairs. For integer values of 
#'   \code{spline.expand > 1}, the lengths of \code{value} and \code{score} are
#'   increased to \code{spline.expand*length(value)}.
#' @param ... Additional arguments passed to \code{spline()}.
#' @return A function representing the habitat suitability curve
#'
#' @details When \code{spline.expand > 1}, an interpolating spline is first
#'   fit to the data. In order to ensure that no scores exceed the maximum 
#'   score included in \code{score}, two separate splines are fit to data below
#'   and above the maximum value of \code{score}. The interpolated data are 
#'   then approximated again using a linear interpolation to enforce the 
#'   extrapolation rules specified by \code{scoreleft} and \code{scoreright}.
#'
#' @examples
#' make_hsc(c(5, 10, 15, 20, 25), c(0, 0.7, 1, 0.75, 0), spline.expand = 1)
#' make_hsc(c(5, 10, 15, 20, 25), c(0, 0.7, 1, 0.75, 0), spline.expand = 100)
#'
#' make_hsc(c(5, 10, 15), c(0, 0.7, 1), scoreright = 1, spline.expand = 1)
#' make_hsc(c(5, 10, 15), c(0, 0.7, 1), scoreright = 1, spline.expand = 100)
#'
#' @export
make_hsc = function(value, score, scoreleft = 0, scoreright = 0, 
  spline.expand = 1L, ...){
  if(missing(score)){
    score = value[,2]
    value = value[,1]
  }
  if(!identical(sort(value), value))
    stop("Value must be strictly increasing")
  value2 = seq(min(value), max(value), length.out = spline.expand*length(value))
  value2 = unique(sort(c(value2, value)))
  whichval = value[which.max(score)]
  valfirst = value[value <= whichval]
  vallast = value[value >= whichval]  
  val2first = value2[value2 <= whichval]
  val2last = value2[value2 >= whichval]
  scorefirst = score[seq(which.max(score))]
  scorelast = score[seq(which.max(score), length(score))]
  splinefirst = spline(valfirst, scorefirst, xout = val2first, ...)$y
  splinelast = spline(vallast, scorelast, xout = val2last, ...)$y
  score2 = c(splinefirst, tail(splinelast, -1))  
  approxfun(value2, score2, method = "linear", yleft = scoreleft, 
    yright = scoreright)
}

#' Discrete Habitat Suitability Function Generator
#'
#' Generate a discrete habitat suitability function given parameter windows and 
#' scores.
#'
#' @param expr List of expressions evaluating the value of a parameter \code{x}. 
#'   All expressions should be mutually exclusive. See examples for how to 
#'   construct the expressions.
#' @param score Vector of scores assigned when a given expression evaluates as
#'   \code{TRUE}.
#' @return A function representing the habitat suitability index.
#'
#' @examples
#' windows = list(
#'   optimal = expression(x >= 14 & x <= 18),
#'   suitable = expression(x < 14 | (x >= 18 & x < 21)),
#'   stressful = expression(x >= 21 & x <= 25),
#'   unsuitable = expression(x > 25)
#' )
#' scores = c(3, 2, 1, 0)
#'
#' make_hsd(windows, scores)
#' make_hsd(windows, names(windows))
#'
#' @export
make_hsd = function(expr, score){
  function(x) score[unlist(lapply(expr, eval, envir = environment()))]  
}  

#' Habitat Suitability Index Generator
#'
#' Generate a habitat suitability index function based on a set of habitat 
#' suitability curves and an aggregation rule.
#'
#' @param hsc A list of habitat suitability curve functions.
#' @param ag.fun A function used for aggregating the outputs of the habitat 
#'   suitability curves.
#' @return A function that calculates the Habitat Suitability Index given a 
#'   vector or dataframe of habitat suitability parameters corresponding (in 
#'   order) to the habitat suitability curves contained in \code{hsc}.
#'
#' @examples
#' f1 = make_hsc(c(5, 10, 15, 20, 25), c(0, 0.7, 1, 0.75, 0))
#' f2 = make_hsc(c(0, 5, 10), c(1, 0.5, 0), scoreleft = 1)
#' make_hsi(list(f1, f2), ag.fun = mean)
#' make_hsi(list(f1, f2), ag.fun = min)
#'
#' @export
make_hsi = function(hsc = list(), ag.fun = mean){
  function(x) 
    ag.fun(mapply(do.call, hsc, lapply(x, list)))
}

#' Join volume information
#' 
#' Attach water volume information from estuary bathymetry to gridded data.
#'
#' @param d Dataframe containing data to have volume information attached to.
#' @param volumes Dataframe containing cell counts. If \code{cellvol} is 
#'   missing, must also containing the attribute "resolution" which is a 
#'   dataframe or list with elements \code{xres}, \code{yres}, and \code{zres}.
#' @param cellvol Numeric volume of cells desribed by \code{volumes}.
#' @param joincols Columns to use when joining \code{volumes} to \code{d}.
#' @param countcol Column name containing cell count data in \code{volumes}.
#' @param volcol The name of the column containing volume data to be added to
#'   \code{d}.
#' @return The dataframe \code{d} with the additional column \code{volcol}.
#'
#' @importFrom dplyr inner_join
#' @export
join_volume = function(d, volumes, cellvol, joincols = c("elev", "dist", "wse"), 
  countcols = c("count", "count.littoral", "count.limnetic", 
    "count.sublimnetic", "count.epibenthic", "count.profundal"), 
  volcols = c("volume.total", "volume.littoral", "volume.limnetic", 
    "volume.sublimnetic", "volume.epibenthic", "volume.profundal")){
  if(missing(volumes))
    data(volumes, envir = environment())
  if(missing(cellvol))
    cellvol = prod(attr(volumes, "resolution")[c("xres", "yres", "zres")])    
  ind = d
  d[, joincols] = as.character(unlist(d[, joincols]))
  volumes[, joincols] = as.character(unlist(volumes[, joincols]))
  f = left_join(d, volumes, by = joincols)
  f[, joincols] = as.numeric(unlist(f[, joincols]))
  for(i in seq_along(volcols))
    ind[volcols[i]] = f[[countcols[i]]]*cellvol
  ind
}

#' Join water-surface elevation information
#' 
#' Attach maximum water-surface elevation recorded in CTD transects to gridded 
#'   data.
#' 
#' @param d Dataframe containing data to have water surface elevation attached 
#'   to.
#' @param ctd Dataframe containing ctd transects matching grids in \code{d}. 
#' @param joincols Columns to use when joining \code{ctd} to \code{d}.
#' @param wsein The name of the column in \code{ctd} containing water-surface 
#'   elevation data.
#' @param wseout The name of the column containing water-surface elevation data
#'   to be added to \code{d}.
#' @return The dataframe \code{d} with the additional column \code{wseout}.
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize
#' @export
join_wse = function(d, ctd, joincols = c("date", "id"), 
  wsein = "elev", wseout = "wse"){
  if(missing(ctd))
    data(ctd, envir = environment())
  ind = d 
  ctd["wsein"] = ctd[[wsein]]
  wl = summarize(group_by_(ctd, .dots = joincols), wse = max(wsein))
  wse = left_join(d, wl, by = joincols)
  ind[wseout] = wse$wse
  ind
}

#' Summarize by Strata
#'
#' Group and summarize a dataframe by specific "strata" or groupings.
#'
#' @param d The dataframe to be summarized.
#' @param stratcols Vector or list of column names to group by.
#' @param summexpr Named vector or list of expressions to summarize the data.
#' @return A dataframe containing grouped and summarized data.
#'
#' @details This function is basically a wrapper for the \code{summarize} and
#'   \code{group_by} functions provided by \code{dplyr}.
#'
#' @importFrom dplyr summarize_
#' @importFrom dplyr group_by_
#' @export
summarize_by_strata = function(d, stratcols, summexpr){
  as.data.frame(summarize_(group_by_(d, .dots = stratcols), 
    .dots = summexpr))  
}
