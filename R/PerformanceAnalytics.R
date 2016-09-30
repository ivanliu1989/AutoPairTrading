#' Get a basic report of trading strategy
#'
#' Get a basic report of trading strategy
#'
#' @param dt.summary trading activity sheet returned from \code{BackTesting}
#'
#' @return A \code{list} of basic trading details
#'
#' @seealso \link{BackTesting}
#'
#' @export
performanceReport <- function(dt.summary){
  # - entry/exit points
  # - average holding time
  # - num of trades in the period
  # - average return per trade
  # - distribution of returns per trade
  # - distribution of days holding
  ret <- dt.summary[dt.summary$in_short | dt.summary$in_long,]
  ret <- ROC(ret$real.capital, n = 1, type = "discrete", na.pad = 0)
  APR <- round(prod(1+ret)**(252/length(ret)) - 1, 5)
  Sharpe <- round(sqrt(252)*mean(ret)/sd(ret), 5)

  trade.summary <- dt.summary[!dt.summary$trade == "NO Trade",]
  table(trade.summary$trade)
  benchmark.ret = mean(c((trade.summary$y.prices[length(trade.summary$y.prices)] - trade.summary$y.prices[1]) / trade.summary$y.prices[1],
  (trade.summary$x.prices[length(trade.summary$x.prices)] - trade.summary$x.prices[1]) / trade.summary$x.prices[1])) - 0.02

  # short
  trade.short <- trade.summary[!trade.summary$trade == "Go Long",]
  trade.short <- setDataTable(trade.short)
  trade.short[, exit.capital := shift(real.capital, n = 1, fill = 0, type = "lead")]
  trade.short[, returns := (exit.capital-real.capital)/real.capital]
  trade.short[, trade.days := shift(Dates, n = 1, fill = 0, type = "lead") - Dates]
  trade.short <- trade.short[!trade == "Exit"]

  # long
  trade.long <- trade.summary[!trade.summary$trade == "Go Short",]
  trade.long <- setDataTable(trade.long)
  trade.long[, exit.capital := shift(real.capital, n = 1, fill = 0, type = "lead")]
  trade.long[, returns := (exit.capital-real.capital)/real.capital]
  trade.long[, trade.days := shift(Dates, n = 1, fill = 0, type = "lead") - Dates]
  trade.long <- trade.long[!trade == "Exit"]

  # All trades
  trade.details <- rbind(trade.long[,.(Dates, y.prices, x.prices, y.shares, x.shares, hedgeRatio, brokerage, trade, real.capital, returns, trade.days)],
                         trade.short[,.(Dates, y.prices, x.prices, y.shares, x.shares, hedgeRatio, brokerage, trade, real.capital, returns, trade.days)])
  setorder(trade.details, Dates)

  avg.holdingdays <- mean(trade.details$trade.days)
  avg.returns <- mean(trade.details$returns[-length(trade.details$returns)])
  tot.returns <- prod(trade.details$returns[-length(trade.details$returns)]+1)-1
  tot.tradingdays <- round(sum(trade.details$trade.days[-length(trade.details$trade.days)]) * 252/365)

  res <- list(
    trade.summary = dt.summary,
    trade.details = trade.details,
    stats.summary = list(avg.holdingdays = avg.holdingdays,
    avg.returns = avg.returns,
    tot.returns = tot.returns,
    tot.tradingdays = tot.tradingdays,
    APR = APR,
    Sharpe = Sharpe,
    Alpha = NULL,
    Beta = NULL,
    Volatility = NULL,
    Max.Drawdown = NULL,
    benchmark.ret = benchmark.ret)
  )
  return(res)
}
