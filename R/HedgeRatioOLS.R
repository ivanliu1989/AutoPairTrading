#' Hedge Ratio OLS
#'
#' Calculate Hedge Ratio by using OLS
#'
#' @param y long series
#' @param x short series
#' @param add_const keep constant (risk free profits) or not
#'
#' @return A \code{list} of hedge ratio (beta, risk) and alpha (risk free profits)
#'
#' @examples
#' data(Raotbl3)
#' attach(Raotbl3)
#' lc.df <- AugmentedDickeyFullerTest(y=lc, lags=3, type='trend')
#' summary(lc.df)
#'
#' @export
HedgeRatioOLS <- function(y, x, add_const = TRUE){
  if(add_const){
    ols.fit <- lm(y~x+1)
  }else{
    ols.fit <- lm(y~x-1)
  }
  alpha = coef(ols.fit)["(Intercept)"][[1]]
  beta = coef(ols.fit)["x"][[1]]
  return(list(alpha = alpha,
              beta = beta))
}
