#' Mixing Layer Depth
#'
#' Identification of the mixing layer depth (e.g. thermocline, pycnocline) 
#'   using a modified version of the Maximum Angle Method.
#'
#' @param z Vector of depths, or 2-column dataframe containing depths in first 
#'   column and variable of interest (e.g. temperature, salinity) in second
#'   column.
#' @param x Vector containing variable of interest (e.g. temperature, salinity)
#'   of same length as \code{z}.
#' @param np Number of points to use in linear regressions (see 'details'). 
#' @param min.range The minimum range in the value of \code{x} in order
#'   for a mixing layer to be detectable.
#' @param max.z Maximum value of \code{z} to use when searching for mixing 
#'   layer depth.
#' @param return.index If TRUE, return the index of \code{z} corresponding to
#'   the mixing layer depth. Otherwise, return the depth.
#' @return either the mixing layer depth or the element index of z associated 
#'   with said depth, depending on the value of \code{return.type}.
#'
#' @details The Maximum Angle Method is modified to better handle the
#'   identification of the mixing layer depth when the surface layer is 
#'   poorly-mixed and to provide some flexibility in the linear regression, but
#'   may be less stable as a result. The modifications force the dual linear 
#'   regressions to share the last/first data point and allow the user to 
#'   specify the number of points used in the regressions.
#'
#' @references Chu, Peter C., and Chenwu Fan. "Maximum angle method for 
#'   determining mixed layer depth from seaglider data." Journal of 
#'   oceanography 67.2 (2011): 219-230.
#'
#' @examples
#' data(ctd)
#' zt = subset(ctd, date == date[1] & dist == dist[1])[c("depth", "ta")] 
#' mld(zt)
#' mld(zt, return.index = TRUE)
#' 
#' #' @seealso \code{\link{blt}}
#' @export
mld = function(z, x = NULL, np = 2, min.range = 0, max.z = NULL, 
  return.index = FALSE){
  if(is.null(x)){
    x = z[, 2]
    z = z[, 1]
  }
  if(!is.null(max.z)){
    x = x[z <= max.z]
    z = z[z <= max.z]
  }
  if(np > floor(0.5*length(z)))
    stop("Argument 'np' is too large.")
  minx = min(x)
  maxx = max(x)
  rangex = maxx - minx
  # return NA if range of variable is too small
  if(rangex < min.range)
    return(NA)
  minz = z[min(which(x == min(x)))]
  maxz = z[max(which(x == max(x)))]
  rangez = maxz - minz
  if(minz != min(z) | maxz != max(z))
    warning("Data is noisy")
  m = np - 1
  j = np - 1
  K = length(z)
  tantheta = 0
  clinek = 1
  for(k in seq(j + 1, K - m)){
#   Gtop = lm(x ~ z, data.frame(z = z[seq(k - j, k)], 
#     x = x[seq(k - j, k)]))$coefficients[[2]]
#   Gbot = lm(x ~ z, data.frame(z = z[seq(k, k + m)], 
#     x = x[seq(k, k + m)]))$coefficients[[2]]
    # slope of line z ~ x is defined as cor(x, z)*sd(x)/sd(z)
    topmask = seq(k - j, k)
    botmask = seq(k, k + m)
    Gtop = cor(z[topmask], x[topmask])*sd(x[topmask])/sd(z[topmask])
    Gbot = cor(z[botmask], x[botmask])*sd(x[botmask])/sd(z[botmask])
    newtantheta = abs((Gbot - Gtop)/(1 + Gbot*Gtop))
    if(!is.na(newtantheta) & newtantheta > tantheta){
      clinek = k
      tantheta = newtantheta
    }  
  }
  if(return.index)
    clinek
  else
    z[clinek]
}

#' Barrier Layer Thickness
#'
#' Calculate the thickness of the barrier layer.
#'
#' @param z Vector of depths, or 3-column dataframe containing depths in first 
#'   column, temperatures in the second column and salinities in the third 
#'   column.
#' @param t Vector of temperatures of same length as \code{z}.
#' @param s Vector of salinities of same length as \code{z}.
#' @param ... Other arguments passed to \code{mld}. Note that 
#'   \code{return.index = FALSE} is fixed.
#' @return The thickness of the barrier layer.
##'
#' @details The 'Barrier Layer' is the region between the pycnocline and 
#'   thermocline. This function identifies the thermocline and pycnocline 
#'   depths using \code{mld} and calculates the thickness of the barrier as the 
#'   difference the two depths.
#'
#' @references Chu, Peter C., and Chenwu Fan. "Maximum angle method for 
#'   determining mixed layer depth from seaglider data." Journal of 
#'   oceanography 67.2 (2011): 219-230.
#'
#' @seealso \code{\link{mld}} 
#' @export
blt = function(z, t, s, ...){
  if(is.missing(t) | is.missing(s)){
    s = z[, 3]
    t = z[, 2]
    z = z[,1]
  }
  thermocline = mld(z, t, return.index = FALSE, ...)
  pycnocline = mld(z, s, return.index = FALSE, ...)
  abs(thermocline - pycnocline)
}

#' Stratify by Depth
#'
#' Stratify depths into zones using specified midpoints.
#'
#' @param z Vector of depths
#' @param midpoints Vector of midpoints or cut points to stratify \code{z} by.
#'   \code{min(z)} and \code{max(z)} are automatically included.
#' @param ... Other arguments to pass to \code{cut()}.
#'
#' @details This function is basically a wrapper for \code{cut()}, but requires
#'   the cuts be directly specified and that \code{include.lowest = TRUE}. It
#'   Every element of \code{z} is therefore guaranteed to be included in one
#'   interval.
stratify_depth = function(z, midpoints, ...){
  minz = min(z)
  maxz = max(z)
  if(any(midpoints >= maxz) | any(midpoints <= minz))
    stop("Midpoints must be within range (min(z), max(z))")
  cut(z, unique(c(minz, sort(midpoints), maxz)), include.lowest = TRUE, ...)  
}

