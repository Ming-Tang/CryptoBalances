
import_Electrum <- function(file, cur="BTC") {
  ElectrumHistory <- data.table(read_csv(
    file, 
    na="unconfirmed",
    col_types=cols(timestamp=col_datetime(format="%F %T"), value=col_character())))
  
  ElectrumHistory <- ElectrumHistory[, .(
    Date=as.Date(timestamp),
    DateTime=timestamp,
    BuyCur=cur,
    Buy=ifelse(!is_neg(value), value, "0"),
    SellCur=cur,
    Sell=ifelse(!is_neg(value), "0", negate(value)),
    Exchange="Electrum Wallet")]
  ElectrumHistory
}
