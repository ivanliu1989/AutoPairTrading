#' Get a Template Run of AutoPairTrading Package
#'
#' Get a Template Run of AutoPairTrading Package
#'
#' @examples
#' getUserTemplate()
#'
#' @export
getUserTemplate <- function(){
  sink("getUserTemplate.R")
  cat(
"
# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(TTR)


# 1. Get Y, X and Price.Ratio ---------------------------------------------
pair <- na.omit(merge(AUDUSD, CADUSD))
tscale <- \"2014-01-01/2016-10-01\"
pair <- pair[tscale]
y = pair[,1]
x = pair[,2]
price.ratio <- getPriceRatio(y, x, FALSE)


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = \"trace\", ecdet = \"none\", K = 2); cat(paste0(\"P-value: \", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0(\"Half-Life: \", half.life))


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life); cat(paste0(\"Hurse Exponent: \", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Zscore ---------------------------------------------------------------
zc <- zscores(price.ratio)
zc.ma <- zscores.ma(price.ratio, 60, 10)


# 9. Bollinger Bands ------------------------------------------------------
BBbands <- BollingerBands(price.ratio, half.life, 2)


# 10. Strategy creation ---------------------------------------------------
mr <- zc.ma
# Momentum indicators
momRSI.y <- momentum.RSI(y, round(half.life/2))
momRSI.x <- momentum.RSI(x, round(half.life/2))
momRSI <- abs(zscores(na.omit(momRSI.y - momRSI.x))) # RSI Variance
momMACD.y <- momentum.MACD(y)
momMACD.x <- momentum.MACD(x)
momMACD <- abs(zscores(na.omit(momMACD.y$longshort - momMACD.x$longshort))) # MACD Variance
momCrossover.y <- momentum.Crossover(y)
momCrossover.x <- momentum.Crossover(x)
momCrossover <- abs(zscores(na.omit((momCrossover.y$hamming.Dist * momCrossover.y$spearman * momCrossover.y$thickness) -
(momCrossover.x$hamming.Dist * momCrossover.x$spearman * momCrossover.x$thickness)))) # Crossover Variance
# Aggregate strategies
strategies <- na.omit(merge(mr, momRSI, momMACD, momCrossover))
strategies$momentum <- (strategies[, 2] + strategies[, 3] + strategies[, 4])/3

chart.TimeSeries(cbind(strategies, -1, 1), legend.loc = \"topleft\")
chart.TimeSeries(strategies[, c(1,5)], legend.loc = \"topleft\")

strategies$final <- strategies[,1] * 0.6 + sign(strategies[,1]) * strategies$momentum * 0.4
# Strategies with momentum
strategies <- na.omit(merge(y, x, strategies$final))

# 11. Back Testing --------------------------------------------------------
# Strategies of simple mean reversion
# strategies <- na.omit(merge(y, x, zc.ma))
context <- InitializeContext(strategies$y, strategies$x, capital = 1e5, window = 20, lookback = 250, brokerage = 0.001, stoploss = 0.1)
dt.summary <- BackTesting(strategies[,1], strategies[,2], context, strategies[,3], rep(-1, nrow(strategies)),
rep(1, nrow(strategies)), rep(0, nrow(strategies)))


# 12. Performance Analytics -----------------------------------------------
basic.report <- performanceReport(dt.summary)
msg = c(\"Simple Mean Reversion (Normal Zscore)\",
\"Simple Mean Reversion (Moving Average Zscore)\",
\"Momentum & Mean Reversion Ensemble (40 to 60)\")
performanceEvaluationEmail(basic.report, c(\"ivan.liuyanfeng@gmail.com\"), message = msg[1])


# 13. Searching Good Integrated Pairs -------------------------------------
data(\"sp500\")
datasets <- sp500[,1:100]
searchCointegratedPairs(datasets, path = \"GoodIntegratedPairs.pdf\",
                        to = c(\"ivan.liuyanfeng@gmail.com\", \"ivan@growingdata.com.au\"),
                        testPeriod = 63, trainPeriod = 252)

")

  sink()
  file.edit("getUserTemplate.R")
}



