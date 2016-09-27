#' Get a Template Run of AutoPairTrading Package
#'
#' Get a Template Run of AutoPairTrading Package
#'
#' @examples
#' getUserTemplate()
#'
#' @export
getUserTemplate <- function(){
  cat(
"
# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(quantmod)
getFX(\"AUD/USD\")
getFX(\"CAD/USD\")


# 1. Get Y, X and Price.Ratio ---------------------------------------------
y = AUDUSD
x = CADUSD
price.ratio <- y/x


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = \"drift\", lags = 1)
adf.x <- AugmentedDickeyFullerTest(x, type = \"drift\", lags = 1)
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = \"drift\", lags = 1)
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = \"drift\", lags = 1)


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = \"drift\", lags = 1)
jc.test <- JohansenCointegrationTest(merge(y,x), type = \"trace\", ecdet = \"none\", K = 2)


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life)


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)],
x[(length(x)-half.life):length(x)])


# 8. Zscore ---------------------------------------------------------------
zc <- zscores(price.ratio)
zc.ma <- zscores.ma(price.ratio, 60, 10)


# 9. Bollinger Bands ------------------------------------------------------
BBbands <- BollingerBands(price.ratio, half.life, 2)


# 10. Strategy creation ---------------------------------------------------
# zc.ma
# zc


# 11. Back Testing --------------------------------------------------------
context <- InitializeContext(y, x, capital = 1e5, window = 20, lookback = 250, brokerage = 0.02, stoploss = 0.1)
dt.summary <- BackTesting(y, x, context, zc.ma, 1, -1)
dt.summary <- BackTesting(y, x, context, zc, 1, -1)
plot(dt.summary$real.capital)


# 12. WIP -----------------------------------------------------------------
# 1. Walkforward testing
# 2. Dynamic HalfLife
# 3. KalmanFilter as an alternative to Hedge Ratio update
# 4. Implement more complicated Strategy Creation
#    4.1 Interest Rates
#    4.2 Commodity
#    4.3 Partner countries
#    4.4 VIR
#    4.5 Sentiment
#    4.6 Machine learning scoring
#    4.7 etc.
# 5. Dashboard of Strategy Performance
# 6. Detailed Statistical Report
# 7. Implement Stop-Loss
# 8. Implement Multi-entry & Multi-exit
# 9. Replace Close-price with Bid/Buy from IB API
")
}



