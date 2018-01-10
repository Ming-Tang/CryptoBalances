library(jsonlite)

import_Etherscan <- function(addr) {
  addr <- tolower(addr)
  url <- paste0("http://api.etherscan.io/api?module=account&action=txlist&sort=desc&address=", addr)
  
  Etherscan <- data.table(fromJSON(url)$result)
  datetimes <- from_unix_timestamp(Etherscan$timeStamp)
  
  Etherscan[, Sign := ifelse(to == addr, 1L, -1L)]
  Etherscan[, dBal := as.numeric(value) * 1e-18] # as.bigq(Etherscan$value)/as.bigq(1e18)
  Etherscan[, Date := as.Date(datetimes)][, DateTime := datetimes]
  setorder(Etherscan, -DateTime)
  Etherscan <- add_buy_sell(Etherscan, "ETH")
  Etherscan <- Etherscan[, .(Date, DateTime, BuyCur, Buy, SellCur, Sell,
                             Exchange="ETH Wallet", From=from, To=to, Transaction=hash)]
  Etherscan
}