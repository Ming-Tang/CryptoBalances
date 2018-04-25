import_Coinbase_TransactionsReport <- function(file) {
  Coinbase <- data.table(read_csv(
    file, 
    col_types = cols_only(
      Amount = col_character(), 
      `Bitcoin Hash (visit https://www.coinbase.com/tx/[HASH] in your browser for more info)` = col_character(), 
      Currency = col_character(), Notes = col_guess(), 
      Timestamp = col_character(), To = col_guess(), 
      `Transfer ID` = col_character(), 
      `Transfer Total` = col_guess(), `Transfer Total Currency` = col_guess()), 
    skip = 3))
  
  Coinbase[, DateTime := as.POSIXct(Timestamp, tz="UTC")][, Date := as.Date(DateTime)]
  
  Coinbase_Withdraws <- Coinbase[is.na(`Transfer ID`) & is_neg(Amount), ]
  Coinbase_Purchases <- Coinbase[!is.na(`Transfer ID`), ]
  
  Coinbase_Withdraws <- Coinbase_Withdraws[, .(
    Date, DateTime, BuyCur=`Currency`, Buy="0", SellCur=`Currency`, Sell=negate(Amount), 
    Group="Transfer", Exchange="Coinbase", To=To,
    Transaction=`Bitcoin Hash (visit https://www.coinbase.com/tx/[HASH] in your browser for more info)`)]
  Coinbase_Purchases <- Coinbase_Purchases[, .(
    Date, DateTime, BuyCur=Currency, Buy=Amount,
    SellCur=`Transfer Total Currency`, Sell=`Transfer Total`, Group="Trade", Exchange="Coinbase")]
  
  Coinbase <- rbind(Coinbase_Withdraws, Coinbase_Purchases, fill=TRUE)[order(-DateTime)]
  Coinbase
}
