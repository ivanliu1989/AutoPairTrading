#' Hurst Exponent Test
#'
#' Effectively this returns a value between 0 and 1 that tells you whether a time-series is trending or mean-reverting.
#' The closer the value is to 0.5 means the more "random" the time-series has behaved historically.
#' Values below 0.5 imply the time-series is mean-reverting, and above 0.5 imply trending.
#' The closer the value is to 0 implies greater levels of mean-reversion.
#'
#' @param y Vector to be tested for a unit root.
#' @param type Test type, either "none", "drift" or "trend".
#' @param lags Number of lags for endogenous variable to be included.
#'
#' @return A \code{list} of ADF test results
#'
#' @examples
#' data(Raotbl3)
#' attach(Raotbl3)
#' lc.df <- AugmentedDickeyFullerTest(y=lc, lags=3, type='trend')
#' summary(lc.df)
#'
#' @import urca
#' @export
HurstExponentTest <- function(y, type, lags){
  lc.df <- ur.df(y=y, lags=lags, type=type)
  return(lc.df)
}
