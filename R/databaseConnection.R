#' SQL Server Connection
#'
#' Connect to Sql Server databases
#'
#' @return A \code{conn} of database
#'
#' @examples
#' conn <- sqlServerConnect()
#' sqlquery1 <- "select capiq_symbol_ticker, capiq_symbol_exchange, pricing_date, price_close, volume from insites_metrics_dev.meta.equity_price where pricing_date between GETDATE() - 500 and GETDATE()"
#' sqlquery2 <- "select distinct capiq_symbol_ticker, capiq_symbol_exchange from insites_metrics_dev.meta.equity_price where pricing_date between GETDATE() - 500 and GETDATE()"
#' res <- sqlFetch(conn, sqlquery1)
#'
#' @export
#' @import
#' RODBC
sqlServerConnect <- function(){
  library(RODBC)
  conn <- odbcConnect("growingdata", "terence","6Da7ad72!")
  return(conn)
}


#' Quandl API Connection
#'
#' Connect to Quandl API
#'
#' @return NULL
#'
#' @examples
#' QuandlConnect()
#' mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL")
#'
#' @export
#' @import
#' Quandl
QuandlConnect <- function(){
  library(Quandl)
  Quandl.api_key("H83fxGxUfi-GBx1JyDx8")
}
