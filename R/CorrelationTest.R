#' Correlation Tests
#'
#' Tests if two series are correlated.
#'
#' @param x, y numeric vectors of data values.
#' @param method a character string naming which test should be applied. ("pearson","kendall","spearman")
#'
#' @return A \code{list} of test results and diagram
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' cor = CorrelationTest(AUDUSD, CADUSD)
#' cor
#'
#' @import fBasics
#' @export
CorrelationTest <- function(x, y, method = "pearson"){
  library(fBasics)
  library(PerformanceAnalytics)
  cor <- correlationTest(x, y, method)
  plot <- chart.Correlation(merge(x, y))
  return(list(cor=cor, plot=plot))
}


