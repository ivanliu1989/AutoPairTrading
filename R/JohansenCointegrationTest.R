#' Johansen Cointegration Test
#'
#' Conducts the Johansen procedure on a given data set.
#' The "trace" or "eigen" statistics are reported and the matrix of eigenvectors as well as the loading matrix.
#'
#' @param x Data matrix to be investigated for cointegration.
#' @param type The test to be conducted, either ‘eigen’ or ‘trace’.
#' @param ecdet Character, ‘none’ for no intercept in cointegration, ‘const’ for constant term in cointegration and ‘trend’ for trend variable in cointegration.
#' @param K The lag order of the series (levels) in the VAR.
#'
#' @return A \code{list} of Johansen Cointegration test results
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' res <- JohansenCointegrationTest(merge(AUDUSD, CADUSD), type = "trace", ecdet = "none", K = 2)
#' res
#'
#' @import urca
#' @export
JohansenCointegrationTest <- function(x, type = "trace", ecdet = "none", K = 2){
  library(urca)
  johansen.test <- summary(ca.jo(x, type=type, ecdet=ecdet, K=K))
  r.1 <- johansen.test@teststat[1]
  p.value = johansen.test@cval[1,]
  signif <- ifelse(r.1 >= p.value["1pct"], 1, ifelse(r.1 >= p.value["5pct"], 5, ifelse(r.1 >= p.value["10pct"], 10, 99)))

  res = list(jc.test = johansen.test,
             r.1 = r.1,
             p.value = p.value,
             signif = signif)
  return(res)
}


#' Search Cointegrated Pairs
#'
#' Conducts the Johansen procedure and ADF test on all combination of pairs to find potential integrated pairs
#'
#' @param data Data matrix including all serires
#'
#' @seealso \link{JohansenCointegrationTest}
#'
#' @import urca
#' @export
findCointegratedPairs <- function(data){
  # pvalue_jc = matrix()
  # pvalue_adf = matrix()
  # keys = index(data)
  # pairs = matrix()
  for(i in 1:n){
    for(j in (i+1):n){
      S1 = data[,i]
      S2 = data[,j]
      jc.test <- JohansenCointegrationTest(data[,c(i,j)])
      adf.test <- AugmentedDickeyFullerTest(S1/S2)
      if(jc.test$signif <= 5 | adf.test$signif <= 5){
        cat("\nFind potential cointegrated pairs!")
        cat(paste0("\nPairs: ", names(data)[i], " vs ", names(data)[j]))
        cat(paste0("\njc.test: ", jc.test$signif, " | adf.test:", adf.test$signif))
      }
    }
  }
}


visualizeCointegration <- function(y,x){
  ratios <- y/x
  ratios.z <- zscores(ratios)
  ratios.z
  chart.TimeSeries()
}
