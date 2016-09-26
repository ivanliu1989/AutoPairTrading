#' Johansen Cointegration Test
#'
#' Conducts the Johansen procedure on a given data set.
#' The "trace" or "eigen" statistics are reported and the matrix of eigenvectors as well as the loading matrix.
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
JohansenCointegrationTest <- function(y, type, lags){
  library(urca)
  lc.df <- ur.df(y=y, lags=lags, type=type)
  return(lc.df)
}
