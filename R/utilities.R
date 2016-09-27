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

#' Compute the Holdings Percentages
#'
#' Compute the Holdings Percentages based on long/short shares and prices
#'
#' @param y.shares y shares
#' @param x.shares x shares
#' @param y.price y price
#' @param x.price x price
#'
#' @return A \code{vector} of percentages of x and y to be targeted
#'
#' @export
computeHoldingsPct <- function(y.shares, x.shares, y.price, x.price){
  y.dollars = y.shares * y.price
  x.dollars = x.shares * x.price
  notional.dollars = abs(y.dollars) + abs(x.dollars)
  y.target.pct = y.dollars / notional.dollars
  x.target.pct = x.dollars / notional.dollars
  return(c(y.target.pct, x.target.pct))
}


#' Make Orders based on Portfolio Percent
#'
#' Make orders based on portfolio percentages
#'
#' @param initial.capital total capital to invest
#' @param capital current capital in hand
#' @param holdings current holdings in dollar values for symbol
#' @param prices current prices of symbol
#' @param shares percent of total portfolio to invest
#'
#' @return A \code{list} of updated values for current status of the symbol
#'
#' @export
make_order_pct <- function(initial.capital, capital, holdings, prices, shares){
  initial.capital = as.numeric(initial.capital)
  capital = as.numeric(capital)
  holdings = as.numeric(holdings)
  prices = as.numeric(prices)
  shares = as.numeric(shares)

  order_dollars = initial.capital * shares
  order_num = round(order_dollars / prices / 100) * 100
  # 1. update capital
  capital = capital - prices * order_num
  # 2. update holding
  holdings = holdings + prices * order_num
  # 3. shares
  shares = shares + order_num

  res = list(
    capital = capital,
    holdings = holdings,
    prices = prices,
    shares = shares
  )
  return(res)
}


#' Create a Summary Sheet for Backtesting
#'
#' Create a Summary Sheet for Backtesting to record all trading activities across the time
#'
#' @param y y series
#' @param x x series
#' @param initial.capital initial capital
#'
#' @return A \code{data.frame}
#'
#' @export
createSummarySheet <- function(y, x, initial.capital){
  dt.summary <- merge(y,x)
  colnames(dt.summary) <- c("y.prices", "x.prices")
  dt.summary$in_short = FALSE
  dt.summary$in_long = FALSE
  dt.summary$capital = initial.capital
  dt.summary$y.holdings = 0
  dt.summary$x.holdings = 0
  dt.summary$y.shares = 0
  dt.summary$x.shares = 0
  dt.summary$hedgeRatio = 0
  dt.summary$long.pos = 0
  dt.summary$short.pos = 0

  return(dt.summary)
}
