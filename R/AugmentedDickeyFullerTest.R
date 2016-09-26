#' Augmented Dickey Fuller Test
#'
#' ADF test is a unit-root test for determining whether the spread is cointegrated
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
AugmentedDickeyFullerTest <- function(y, type, lags){
  lc.df <- ur.df(y=y, lags=lags, type=type)
  return(lc.df)
}
