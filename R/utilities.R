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

#' Zscore based Moving Average
#'
#' Using moving averages to compute the z-score of the ratio at each given time.
#' This will tell us how extreme the ratio is and whether it's a good idea to enter a position at this time.
#'
#' @param t a time-series to be normalized
#' @param long days of long-term moving averages
#' @param short days of short-term moving averages
#'
#' @return A normalized time-series
#'
#' @examples
#' library(quantmod)
#' getFX("AUD/USD")
#' zscores.ma(AUDUSD, 60, 10)
#'
#' @export
zscores.ma <- function(t, long = 60, short = 10){
  tz <- (SMA(t, short) - SMA(t, long)) / rollapply(t, long, sd)
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
