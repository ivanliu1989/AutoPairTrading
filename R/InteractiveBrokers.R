#' Download FX Historical Data
#'
#' Makes a request to the Interactive Brokers Trader Workstation (TWS), and returns an xts object containing the results of the request if successful.
#'
#' @param tws connection to current TWS account, if NULL, a new connection will be created within the function
#' @param duration time span the request will cover
#' @param barsize bar size to retrieve
#' @param Cur1 symbol of the first currency
#' @param Cur2 symbol of the second currency
#'
#' @details
#' Legal barSize settings are technically '1 secs','5 secs','15 secs','30 mins','1 min','2 mins', '3 mins','5 mins','15 mins', '30 mins','1 hour','1 day', '1 week','1 month','3 months', and '1 year'. They must be specified exactly and there is no guarantee from the API that all will work for all securities or durations.
#' The duration string must be of the form 'n S' where the last character may be any one of 'S' (seconds), 'D' (days), 'W' (weeks), 'M' (months), and 'Y' (year). At present the limit for years is 1.
#'
#' @return A \code{list} of Bid, Ask and Bid_Ask prices
#'
#' @examples
#' library(IBrokers)
#' tws <- twsConnect(clientId = 999)
#' USDCAD <- reqHistoryFX(tws, "10 Y", "1 day", "USD", "CAD")
#' USDAUD <- reqHistoryFX(tws, "10 Y", "1 day", "AUD", "USD")
#'
#' @import IBrokers
#' @export
reqHistoryFX <- function(tws = NULL, duration = "10 Y", barsize = "1 day", Cur1 = "USD", Cur2 = "CAD"){
  library(IBrokers)
  if(is.null(tws)){
    tws <- twsConnect(clientId = 999)
  }

  ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract

  BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                              duration = duration, useRTH = "1", whatToShow='MIDPOINT')

  BID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                           duration = duration, useRTH = "1", whatToShow='BID')

  ASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                           duration = duration, useRTH = "1", whatToShow='ASK')

  CleanData <- merge(BIDASK[,4], BID[,4], ASK[, 4])
  colnames(CleanData) <- c("Close.price", "Bid.price", "Ask.price")

  res <- list(BIDASK = BIDASK,
              BID = BID,
              ASK = ASK,
              CleanData = CleanData)

  if(is.null(tws)){
    twsDisconnect(tws)
  }

  return(res)
}
