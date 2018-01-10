import_Poloniex_TradeHistory <- function(file) {
  Poloniex.TH <- data.table(read_csv(
    file,
    col_types=cols(
      Amount = col_character(), 
      `Base Total Less Fee` = col_character(), 
      `Order Number` = col_character(), 
      Price = col_character(),
      `Quote Total Less Fee` = col_character(), 
      Total = col_character(),
      Date=col_datetime("%Y-%m-%d %H:%M:%S"))))
  
  Poloniex.TH[, `:=`(Q=`Quote Total Less Fee`, B=`Base Total Less Fee`)]
  Poloniex.TH[, `:=`(Date= as.Date(Date), DateTime=Date)]
  setorder(Poloniex.TH, -DateTime)
  
  Poloniex.TH <- fill_buy_sell(Poloniex.TH)
  Poloniex.TH[Type=="Buy", `:=`(Buy=Q, Sell=negate(B))]
  Poloniex.TH[Type=="Sell", `:=`(Buy=B, Sell=negate(Q))]
  Poloniex.TH <- Poloniex.TH[, .(Date, DateTime, BuyCur, Buy, SellCur, Sell, Rate=Price, Exchange="Poloniex", Group="Trade")]
  Poloniex.TH
}

import_Poloniex_DepositHistory <- function(file) {
  Poloniex.DH <- data.table(
    read_csv(file, col_types = cols(Date = col_datetime(format = "%F %T"), Amount=col_character())))
  Poloniex.DH <- Poloniex.DH[, .(
    Date=as.Date(Date), DateTime=Date,
    BuyCur=Currency, Buy=Amount, 
    SellCur=Currency, Sell="0", 
    Exchange="Poloniex", Group="Deposit", From=Address)]
  Poloniex.DH
}

import_Poloniex_WithdrawHistory <- function(file) {
  Poloniex.WH <- data.table(
    read_csv(file, col_types = cols(Date = col_datetime(format = "%F %T"), Amount=col_character())))
  Poloniex.WH <- Poloniex.WH[, .(
    Date=as.Date(Date), DateTime=Date, 
    BuyCur=Currency, Buy="0", 
    SellCur=Currency, Sell=Amount, 
    Exchange="Poloniex", Group="Withdrawal", From=Address)]
  Poloniex.WH
}
