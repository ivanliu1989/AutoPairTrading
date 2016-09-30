#' Generate a pdf report and email users
#'
#' Generate a pdf report and email users
#'
#' @param performanceReport an object returned from \code{performanceReport}
#' @param to a list of email addresses of report receivers
#'
#' @seealso \link{performanceReport}
#'
#' @export
#' @import ggplot2
#' @import mailR
performanceEvaluationEmail <- function(performanceReport, to){
  library(ggplot2)

  first.trade <- as.Date(performanceReport$trade.details$Dates[1])
  full.trades <- performanceReport$trade.summary[rownames(performanceReport$trade.summary)>=first.trade,]

  # 0. Basic Reports


  # 1. Performance
  R <- ROC(full.trades[, c("y.prices", "x.prices", "real.capital")])
  charts.PerformanceSummary(R)

  # 2. Divergence
  trade.points <- full.trades[!full.trades$trade == "NO Trade",]
  chart.TimeSeries(full.trades[,c("y.prices", "x.prices")], event.lines = rownames(trade.points),
                   event.labels = trade.points$trade)
  zc <- zscores(getPriceRatio(full.trades$y.prices, full.trades$x.prices, FALSE))
  pratio <- data.frame(zscor = zc, low = -1, high = 1)
  rownames(pratio) <- rownames(full.trades)
  chart.TimeSeries(pratio, event.lines = rownames(trade.points),
                   event.labels = trade.points$trade)

  # 3. Hedge Ratio, alpha, beta
  risk.trades <- full.trades[, c("y.prices", "x.prices", "alpha", "beta")]
  chart.Bar(risk.trades[,c("alpha", "beta")])

  # 4. Scatter plot - holding days and returns
  ret.hold <- ggplot(performanceReport$trade.details, aes(x = as.numeric(trade.days), y = as.numeric(returns))) +
    geom_point(aes(color = trade, size = as.numeric(returns))) +
    geom_smooth() +
    geom_text(aes(label = Dates))

  # 5. Returns density
  ret.density <- ggplot(performanceReport$trade.details) +
    geom_density(aes(x = returns))

  # 6. Email report
  library(mailR)
  from = "ivan@growingdata.com.au"
  subject = "test"
  msg = "test"

  send.mail(from = "ivan@growingdata.com.au",
            to = to,
            subject = subject,
            body = msg,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ivan@growingdata.com.au", passwd = "Kalmanfilter123", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
}








