#' Half-Life Mean-Reversion
#'
#' Computed from an Ornstein–Uhlenbeck process. This is the theoretically computed time, based on a historical window of data,
#' that it will take for the spread to mean-revert half of its distance after having diverged from the mean of the spread.
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
HalfLifeMeanReversion <- function(y, type, lags){
  lc.df <- ur.df(y=y, lags=lags, type=type)
  return(lc.df)
}
