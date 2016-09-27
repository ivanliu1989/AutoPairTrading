rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(quantmod)

getFX("AUD/USD")
getFX("CAD/USD")

# 1. Unit Test - InitializeContext() --------------------------------------
context <- InitializeContext(AUDUSD, CADUSD, capital = 1e5, window = 20, lookback = 250, brokerage = 0.02, stoploss = 0.1)
expect_equal(length(context), 8)


# 2. Unit Test - CorrelationTest() ----------------------------------------
cor = CorrelationTest(AUDUSD, CADUSD)
expect_equivalent(cor$cor@test$estimate, 0.7735753)


# 3. Unit Test - AugmentedDickeyFullerTest() ------------------------------
adf.test <- AugmentedDickeyFullerTest(AUDUSD, type = "drift", lags = 1)
expect_equivalent(adf.test$signif, 5)
expect_equal(is.numeric(adf.test$z.lag.1), TRUE)
adf.test.coint <- AugmentedDickeyFullerTest(AUDUSD/CADUSD, type = "drift", lags = 1)
expect_equivalent(adf.test$signif, 99)


# 4. Unit Test - JohansenCointegrationTest() ------------------------------
jc.test <- JohansenCointegrationTest(merge(AUDUSD, CADUSD), type = "trace", ecdet = "none", K = 2)
expect_equivalent(jc.test$signif, 10)


# 5. Unit Test - HalfLifeMeanReversion() ----------------------------------
half.life <- HalfLifeMeanReversion(AUDUSD/CADUSD)
expect_equal(half.life$half.life.round, 68)


# 6. Unit Test - HedgeRatioOLS() ------------------------------------------
hedgeRatio <- HedgeRatioOLS(AUDUSD[(length(AUDUSD)-half.life$half.life.round):length(AUDUSD)],
                            CADUSD[(length(AUDUSD)-half.life$half.life.round):length(AUDUSD)])
expect_equal(names(hedgeRatio), c("alpha","beta"))
hedgeRatio <- HedgeRatioOLS(AUDUSD, CADUSD, FALSE)


# 7. Unit Test - HurstExponentTest() --------------------------------------
hurst <- HurstExponentTest(AUDUSD/CADUSD, half.life$half.life.round)
expect_equal(length(hurst$hurstKY), length(AUDUSD))


# 8. Unit Test - Zscore price ratio ---------------------------------------
zc <- zscores(AUDUSD/CADUSD)
zc.ma <- zscores.ma(AUDUSD/CADUSD, 60, 10)
par(mfcol = c(2,1))
chart.TimeSeries(merge(zc,zc.ma, 1,-1))
chart.TimeSeries(merge(AUDUSD, CADUSD))


# 9. Unit Test - Bollinger Bands ------------------------------------------
BBbands = BollingerBands(AUDUSD/CADUSD, half.life$half.life.round, 2)
expect_equal(class(BBbands), "data.frame")


# 10. Unit Test - Strategy ------------------------------------------------
y = AUDUSD
x = CADUSD
price.ratio <- y/x

# 1. Correlation test
cor.test <- CorrelationTest(y, x)
# 2. Stationary tests - ADF
adf.y <- AugmentedDickeyFullerTest(y, type = "drift", lags = 1)
adf.x <- AugmentedDickeyFullerTest(x, type = "drift", lags = 1)
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = "drift", lags = 1)
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = "drift", lags = 1)
# 3. Price Ratio Cointegration tests
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = "drift", lags = 1)
jc.test <- JohansenCointegrationTest(merge(y,x), type = "trace", ecdet = "none", K = 2)
# 4. Half-life tests to determine windows length
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round
# 5. Price Ratio Hurse Exponent tests
hurst.test <- HurstExponentTest(price.ratio, half.life)
# 6. Hedge Ratio Calculation
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)],
                            x[(length(x)-half.life):length(x)])
# 7. Zscore
zc <- zscores(price.ratio)
zc.ma <- zscores.ma(price.ratio, 60, 10)
# 8. Bollinger Bands
BBbands <- BollingerBands(price.ratio, half.life, 2)

# 9. Entry/Exit
context <- InitializeContext(y, x, capital = 1e5, window = 20, lookback = 250, brokerage = 0.02, stoploss = 0.1)
dt.summary <- BackTesting(y, x, context, zc.ma, 1, -1)
dt.summary <- BackTesting(y, x, context, zc, 1, -1)

plot(dt.summary$real.capital)
