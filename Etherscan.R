import_Etherscan <- function(addr) {
  addr <- tolower(addr)
  url <- paste0("http://api.etherscan.io/api?module=account&action=txlist&sort=desc&address=", addr)
  
  result <- fromJSON(url)$result
  Etherscan <- data.table(result)
  datetimes <- from_unix_timestamp(Etherscan$timeStamp)
  
  Etherscan[, Sign := ifelse(to == addr, 1L, -1L)]
  #Etherscan[, dBal := as.numeric(value) * 1e-18] # as.bigq(Etherscan$value)/as.bigq(1e18)
  deno <- paste0("/1", strrep("0", 18))
  C <- as.character
  Etherscan[, dBal := C(as.bigq(paste0(C(value), deno)))]
  Etherscan[, DateTime := datetimes]
  Etherscan <- add_buy_sell(Etherscan, "ETH")
  Etherscan <- Etherscan[, .(DateTime, BuyCur, Buy, SellCur, Sell=negate(Sell),
                             Exchange="ETH Wallet", From=from, To=to, Transaction=hash)]
  
  TxFees <- data.table(result)[, .(
    DateTime=from_unix_timestamp(timeStamp),
    BuyCur="ETH", Buy="0",
    SellCur="ETH",  Sell=paste0(C(as.bigq(gasPrice)*as.bigq(gasUsed)), deno),
    Exchange="ETH Wallet",  From=from, To=to,  Transaction=hash, Group="TxFee")]
  
  Etherscan <- rbind(Etherscan, TxFees[From==addr], fill=TRUE)
  Etherscan[, Date := as.Date(DateTime)] 
  
  setorder(Etherscan, -DateTime)
  Etherscan
}
