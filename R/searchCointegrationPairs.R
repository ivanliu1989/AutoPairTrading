#' Search Cointegrated Pairs
#'
#' Conduct the Johansen procedure and ADF test on all combination of pairs to find potential integrated pairs
#' Conduct the out of sample tests on all pairs to test the consistencies of cointegrations
#'
#' @param dataset Data matrix including all serires
#' @param path the path of the pdf report to be saved
#' @param to a list of email addressed to receive the reports
#' @param testPeriod number of points used to test cointegrations
#' @param trainPeriod number of points used to train cointegrations
#'
#' @seealso \link{JohansenCointegrationTest}
#' @seealso \link{AugmentedDickeyFullerTest}
#'
#' @examples
#' data("sp500")
#' datasets <- sp500[,sample(1:500, 100)]
#' searchCointegratedPairs(datasets, path = "GoodIntegratedPairs.pdf",
#' to = c("ivan.liuyanfeng@gmail.com"),testPeriod = 63, trainPeriod = 252)
#'
#' @import urca
#' @import tseries
#' @import mailR
#' @import xtable
#' @export
searchCointegratedPairs <- function(dataset, path = "GoodIntegratedPairs.pdf", to = c("ivan.liuyanfeng@gmail.com"),
                                    testPeriod = 63, trainPeriod = 252){
  library(tseries)
  stocks <- names(dataset)
  nrStocks <- length(stocks)
  totalPeriod <- testPeriod + trainPeriod
  nDays <- nrow(dataset)
  testDates <- (nDays-testPeriod):nDays
  learningDates <- (nDays - testPeriod - trainPeriod):(nDays - testPeriod)

  learning_ds <- dataset[learningDates,]
  test_ds <- dataset[testDates,]

  # 1. Calculating Cointegrations for all Pairs -----------------------------
  # prepare variables
  ht_jo <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
  ht_adf <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
  p_ratio <- list()

  # Test cointegrations of all pairs
  for (j in 1:(nrStocks-1)) {
    for (i in (j+1):nrStocks) {

      cat("Testing cointegrations of ", j, " - ", i, "\n")
      tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
      if (length(tmp_ds) == 0)
      {
        ht_jo[j, i] <- NA
        ht_adf[j, i] <- NA
        next
      }

      p_ratio <- getPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)

      # The ht object contains the p-value from the ADF test.
      # The p-value is the probability that the spread is NOT
      # mean-reverting.  Hence, a small p-value means it is very
      # improbable that the spread is NOT mean-reverting
      adf.p <- try(AugmentedDickeyFullerTest(na.omit(coredata(p_ratio)))$signif[[1]])
      if (isTRUE(class(adf.p) == "try-error"))
      {
        ht_adf[j, i] <- NA
        next
      }
      ht_adf[j, i] <- adf.p

      # Johansen test same as above
      jc.p <- try(JohansenCointegrationTest(na.omit(merge(tmp_ds[,2],tmp_ds[,1])))$signif[[1]])
      if (isTRUE(class(jc.p) == "try-error"))
      {
        ht_jo[j, i] <- NA
        next
      }
      ht_jo[j, i] <- jc.p
    }
  }


  # 2. Select well performed ------------------------------------------------
  zscore <- 0;
  rscore <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)
  pairSummary <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)

  idx <- 1;

  # lets evaluate the spreads
  for (j in 1:(nrStocks-1)) {
    for (i in (j+1):nrStocks) {

      # if no data, skip
      if (is.na(ht_jo[j, i]) | is.na(ht_adf[j, i])) next

      # is spread stationary (i.e. pair is co-integrated)
      # p-value is the smaller the better
      if (ht_jo[j, i] <= 5 & ht_adf[j, i] <= 5) {

        tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
        if (length(tmp_ds) == 0) next

        p_ratio <- getPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)

        # calculate z-score
        zscore <- sum(abs(scale(p_ratio)))/length(p_ratio)
        rscore[idx, 3] <- sd(p_ratio)
        rscore[idx, 4] <- zscore
        rscore[idx, 5] <- mean(p_ratio)
        rscore[idx, 1] <- j
        rscore[idx, 2] <- i

        pairSummary[idx, ] = fivenum(coredata(p_ratio))[1:5]
        idx <- idx + 1
      }
    }

    cat("Calculating ", j, "\n")
  }

  # clean up na rows
  rscore <- na.remove(rscore)
  pairSummary <- na.remove(pairSummary)

  # 3. Visualise good pairs -------------------------------------------------
  cat(paste0("Found ", length(rscore[,1]), " good pairs!"))

  if (length(rscore[,1]) == 0) { stop("No good pair found!") }


  pdf(path, useDingbats=FALSE, width=8, height=9)
  for (pos in 1:length(rscore[,1])) {
    j <- rscore[pos, 1]
    i <- rscore[pos, 2]
    name_j <- stocks[j]
    name_i <- stocks[i]
    cat(paste0("\nPair: ", name_j, " to ", name_i, " | Score: Johansen-", ht_jo[j,i], " & ADF-", ht_adf[j,i]))

    # training set
    tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
    if (length(tmp_ds) == 0) next
    l_pr <- getPriceRatio(tmp_ds[,2],tmp_ds[,1], log = T)
    l_pr <- zscores(l_pr)
    l_ds_j <- indexation(tmp_ds[,1])
    l_ds_i <- indexation(tmp_ds[,2])

    # testing set
    tmp_ds <- na.omit(cbind(test_ds[,j], test_ds[,i]))
    if (length(tmp_ds) == 0) next
    t_pr <- getPriceRatio(tmp_ds[,2],tmp_ds[,1], log = T)
    t_pr <- zscores(t_pr)

    lb = -1
    ub = 1

    par(mfrow=c(3,1))
    par(mar = c(2, 4, 4, 2))
    chart.TimeSeries(merge(l_ds_j, l_ds_i), ylab = "Prices", colorset = bluefocus,
                     legend.loc = "topleft", xaxis = TRUE, main = paste0("Price Index - ", name_j, " to ", name_i))
    l_dt <- cbind(l_pr, 1, -1); colnames(l_dt) <- c("Price Ratio", "Upper Band", "Low Band")
    chart.TimeSeries(l_dt,xaxis = TRUE, ylab = "Price Ratio", colorset = bluefocus,
                     legend.loc = "topleft", main = "Train Price Ratio")
    t_dt <- cbind(t_pr, 1, -1); colnames(t_dt) <- c("Price Ratio", "Upper Band", "Low Band")
    par(mar = c(3, 4, 4, 2))
    chart.TimeSeries(t_dt,xaxis = TRUE, ylab = "Price Ratio", colorset = bluefocus,
                     legend.loc = "topleft", main = "Test Price Ratio")

    # cmd <- readline()
    # if (cmd == 'c') break
  }
  dev.off()

  # 4. Email ----------------------------------------------------------------
  library(mailR)
  library(xtable)
  report.dt <- cbind(rscore, pairSummary)
  report.dt[,1] <- stocks[as.numeric(report.dt[,1])]
  report.dt[,2] <- stocks[as.numeric(report.dt[,2])]
  colnames(report.dt) <- c("Symbol Y", "Symbol X", "Std", "Zscore", "Mean", "Min", "Lower-hinge", "Median", "Upper-hinge", "Max")
  from = "ivan@growingdata.com.au"
  subject = "AutoPairTrading - Search Cointegration Report"
  msg = paste0("<h3>AutoPairTrading Cointegration Search Notification - ", Sys.Date(), "</h3>",
               paste0("<h4>Found ", length(rscore[,1]), " good pairs!</h4>"),
               paste0("<p>Searched ", nrStocks, " unique series and ", round(nrStocks^2/2), " pairs in total.</p>"),
               paste0("<p>Following ", length(rscore[,1]), " have been found as good pairs in terms of their Johansen & ADF scores.</p>"),
               print(xtable(report.dt), type = "html"),
               "<br>",
               "<p>For more details, please see attached <b>GoodIntegratedPairs.pdf</b></p>"
  )
  tryCatch({
    send.mail(from = "ivan@growingdata.com.au",
              to = to,
              subject = subject,
              body = msg,
              html = TRUE,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ivan@growingdata.com.au", passwd = "Kalmanfilter123", ssl = TRUE),
              authenticate = TRUE,
              attach.files = path,
              send = TRUE)
  }, finally = {
    unlink(path, recursive = T, force = T)
    cat("Reports sent! Please check your email box!")
  })
}


