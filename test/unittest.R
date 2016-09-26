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
expect_equivalent(adf.test$signif, "<5")
expect_equal(is.numeric(adf.test$z.lag.1), TRUE)


# 4. Unit Test - JohansenCointegrationTest() ------------------------------


