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
hedgeRatio <- HedgeRatioOLS(AUDUSD, CADUSD)
expect_equal(names(hedgeRatio), c("alpha","beta"))
hedgeRatio <- HedgeRatioOLS(AUDUSD, CADUSD, FALSE)
