#' Habitat Suitability Curve Generator
#'
#' Generate a habitat suitability curve given parameter values and scores.
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

