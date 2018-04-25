import_Binance_TradeHistory <- function(file) {
  Binance.TH <- data.table(read_excel(
    file, 
    col_types = c("text", "text", "text", "text", "text", "text", "text", "text")))
  
  names(Binance.TH)[names(Binance.TH) == "Date(UTC)"] <- "Date" 
  
  Binance.TH[, DateTime := as.POSIXct(Date, tz="UTC")][, Date := as.Date(DateTime)]
  Binance.TH[, Market := gsub("(BTC|ETH|BNB|USDT)$", "/\\1", Market)]
  Binance.TH[, Type := ifelse(Type=="BUY", "Buy", "Sell")]
  Binance.TH[, Id := as.character(rev(1L:.N))]
  Binance.TH <- fill_buy_sell(Binance.TH)
  Binance.TH[Type=="Buy", `:=`(Buy=Amount, Sell=Total)]
  Binance.TH[Type=="Sell", `:=`(Buy=Total, Sell=Amount)]
  
  Binance.TH <- rbind(
    Binance.TH[, .(DateTime, Date, BuyCur=`Fee Coin`, Buy="0", SellCur=`Fee Coin`, Sell=Fee, Id, Exchange="Binance", Group="TradeFee", Market, Type)],
    Binance.TH[, .(DateTime, Date, BuyCur=BuyCur, Buy=Buy, SellCur=SellCur, Sell, Id, Exchange="Binance", Group="Trade", Market, Type)])
  setorder(Binance.TH, -DateTime, Group)
  Binance.TH
}

import_Binance_DepositHistory <- function(file) {
  Binance.DH <- data.table(read_excel(
    file,
    col_types=c("text", "text", "text", "text", "text", "text")))
  Binance.DH[, DateTime := as.POSIXct(Date, tz="UTC")][, Date := as.Date(DateTime)]
  
  if (nrow(Binance.DH) == 0) { return(data.table()) }
  
  Binance.DH <- Binance.DH[, .(
    Date, DateTime,
    BuyCur=Coin, Buy=Amount, 
    SellCur=Coin, Sell="0", 
    Transaction=TXID,
    Exchange="Binance", Group="Deposit")]
  Binance.DH
}

import_Binance_WithdrawHistory <- function(file) {
  Binance.WH <- data.table(read_excel(
    file,
    col_types=c("text", "text", "text", "text", "text", "text")))
  Binance.WH[, DateTime := as.POSIXct(Date, tz="UTC")][, Date := as.Date(DateTime)]
  
  if (nrow(Binance.WH) == 0) { return(data.table()) }
  
  Binance.WH <- Binance.WH[, .(
    Date, DateTime,
    BuyCur=Coin, Buy="0", 
    SellCur=Coin, Sell=Amount, 
    Transaction=TXID,
    Exchange="Binance", Group="Withdrawal")]
  Binance.WH
}
