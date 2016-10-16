

# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(TTR)
library(data.table)

# 1. Get Y, X and Price.Ratio ---------------------------------------------
tscale <- "2014-01-01/2016-10-01"
y = AUDUSD[tscale]
x = CADUSD[tscale]
price.ratio <- getPriceRatio(y, x, FALSE)
names(price.ratio) = "price.ratio"


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = "drift", lags = 1); cat(paste0("P-value: ", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = "drift", lags = 1); cat(paste0("P-value: ", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = "drift", lags = 1); cat(paste0("P-value: ", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = "trace", ecdet = "none", K = 2); cat(paste0("P-value: ", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0("Half-Life: ", half.life))


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life); cat(paste0("Hurse Exponent: ", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Preparing the Universe data ------------------------------------------
head(SampleUniverse)


# 9. Back Testing ---------------------------------------------------------
context <- InitializeContext(SampleUniverse$AUD.USD, SampleUniverse$CAD.USD, capital = 1e6, window = 20,
lookback = 252, brokerage = 0.001, stoploss = 0.1, half.life = half.life)
dt.summary <- BackTestingRealTime(context, SampleUniverse, nEval = 350)
# dt.summary <- BackTestingRealTimeBenchmark(context, SampleUniverse, nEval = 350)


# 10. Performance Analytics -----------------------------------------------
basic.report <- performanceReport(dt.summary)
performanceEvaluationEmail(basic.report, c("ivan.liuyanfeng@gmail.com"), message = "Machine Learning and Mean Reversion - Real Time")


# 11. Searching Good Integrated Pairs -------------------------------------
data("sp500")
datasets <- sp500
searchCointegratedPairs(datasets, path = "GoodIntegratedPairs.pdf",
to = c("ivan.liuyanfeng@gmail.com", "ivan@growingdata.com.au"),
testPeriod = 63, trainPeriod = 252)
