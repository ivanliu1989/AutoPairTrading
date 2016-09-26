#' Zscore
#'
#' Normalize time series
#'
#' @param t a time-series to be normalized
#'
#' @return A normalized time-series
#'
#' @examples
#' library(quantmod)
#' getFX("AUD/USD")
#' zscores(AUDUSD)
#'
#' @export
zscores <- function(t){
  tz <- (t-mean(t))/sd(t)
  return(tz)
}

#' Fill Missing Data
#'
#' Fill missing data according to the last value
#'
#' @param v a vector of values
#'
#' @return A \code{vector} of filled values
#'
#' @examples
#' v = c(1, rep(NA, 10), 2)
#' fillMissingData(v)
#'
#' @export
fillMissingData <- function(v){
  for(i in 1:length(v)){
    v[i]<-ifelse(is.na(v[i]), v[i-1], v[i])
  }
  return(v)
}
