Cryptopia_date_format <- "%d/%m/%Y %I:%M:%S %p"

import_Cryptopia_TradeHistory <- function(file) {
  Cryptopia.TH <- data.table(read_csv(
    file, 
    col_types = cols(
      Amount = col_character(), 
      Fee = col_character(),
      Rate = col_character(), 
      Timestamp = col_datetime(format=Cryptopia_date_format),
      Total = col_character())))
  
  if (nrow(Cryptopia.TH) == 0) { return(data.table()) }
  
  Cryptopia.TH[, `:=`(Date = as.Date(Timestamp), DateTime = Timestamp)]
  Cryptopia.TH <- fill_buy_sell(Cryptopia.TH)
  Cryptopia.TH[Type=="Buy", `:=`(Buy=Amount, Sell=Total)]
  Cryptopia.TH[Type=="Sell", `:=`(Buy=Total, Sell=Amount)]
  Cryptopia.TH <- Cryptopia.TH[, .(Date, DateTime, BuyCur, Buy, SellCur, Sell, Rate, Exchange="Cryptopia", Group="Trade")]
  Cryptopia.TH 
} 

import_Cryptopia_DepositHistory <- function(file) {
  Cryptopia.DH <- data.table(read_csv(
    file, 
    col_types = cols(
      Amount = col_character(),
      Timestamp=col_datetime(format=Cryptopia_date_format))))
  
  if (nrow(Cryptopia.DH) == 0) { return(data.table()) }
  
  Cryptopia.DH <- Cryptopia.DH[, .(
      Date=as.Date(Timestamp), DateTime=Timestamp, 
      BuyCur=Currency, Buy=Amount, 
      SellCur=Currency, Sell="0", 
      Transaction=Transaction,
      Exchange="Cryptopia", Group="Deposit")]
  
  Cryptopia.DH
}

import_Cryptopia_WithdrawHistory <- function(file) {
  Cryptopia.WH <- data.table(read_csv(
    file, 
    col_types = cols(
      Amount = col_character(),
      Timestamp=col_datetime(format=Cryptopia_date_format))))
  
  if (nrow(Cryptopia.WH) == 0) { return(data.table()) }
  
  Cryptopia.WH <- Cryptopia.WH[, .(
      Date=as.Date(Timestamp), DateTime=Timestamp, 
      BuyCur=Currency, Buy="0", 
      SellCur=Currency, Sell=Amount, 
      Transaction=TransactionId, To=Address,
      Exchange="Cryptopia", Group="Withdrawal")]
  
  Cryptopia.WH
}
