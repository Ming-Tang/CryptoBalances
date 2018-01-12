
from_unix_timestamp <- function(xs) {as.POSIXct(as.integer(xs), tz="UTC", origin="1970-01-01") }

add_buy_sell <- function(DT, cur="BTC") {
  DT[, `:=`(BuyCur=cur, Buy="0", SellCur=cur, Sell="0")]
  DT[Sign == 1L, Buy := dBal]
  DT[Sign == -1L, Sell := negate(dBal)]
  #if (nrow(DT[Sign %notin% c(1L, -1L)])) {
  #  warning("Invalid sign values detected.")
  #}
  return(DT)
}

split_cur_pair <- function(pair) {
  spl <- strsplit(pair, "/")
  list(CurA=unlist(lapply(spl, FUN=function(x) x[1])), 
       CurB=unlist(lapply(spl, FUN=function(x) x[2])))
}

fill_buy_sell <- function(DT) {
  pairs <- split_cur_pair(DT$Market)
  DT[, `:=`(BuyCur=ifelse(Type=="Buy", pairs$CurA, pairs$CurB),
            SellCur= ifelse(Type=="Buy", pairs$CurB, pairs$CurA))]
  return(DT)
}

to_delta <- function(DT) {
  DT1 <- copy(DT)[, TId := 1L:.N]
  
  DT.Buys <- copy(DT1)[, `:=`(TOrd=TId*10L, Cur=BuyCur, dBal=Buy, P=1L)]
  DT.Sells <- copy(DT1)[, `:=`(TOrd=TId*10L + 1L, Cur=SellCur, dBal=negate(Sell), P=-1L)]
  
  #DT.Buys[c("BuyCur", "SellCur", "Buy", "Sell") := NULL]
  #DT.Sells[c("BuyCur", "SellCur", "Buy", "Sell") := NULL]
  
  DT.D <- rbind(DT.Buys, DT.Sells)
  setorder(DT.D, TOrd)
  return(DT.D)
}

# Math for numbers stored in strings

negate <- function(x) {
  ifelse(startsWith(x, "-"), substring(x, 2), ifelse(startsWith(x, "+"), paste0("-", substring(x, 2)), paste0("-", x))) 
}

is_zero <- function(x) { as.numeric(x) == 0 }
is_pos <- function(x) { !startsWith(x, "-") & !is_zero(x) }
is_neg <- function(x) { startsWith(x, "-") }

library(gmp)

# Convert decimal strings to rational numbers
to_bigqs <- function(nums) {
  nums <- sub("^\\+", "", nums)
  ids <- regexpr("\\.", nums)
  ids <- ifelse(ids == -1, 0L, nchar(nums) - ids)
  ns <- sub("^(-?)0+(?!$)", "\\1", sub("\\.", "", nums), perl=TRUE)
  ds <- paste0("1", strrep("0", ids))
  #print(list(nums, ids, ns, ds))
  as.bigq(ns, ds)
}

bsum <- function(xs) sum(to_bigqs(xs))
bcumsum <- function(xs) cumsum(to_bigqs(xs))

to_cn <- function(xs) as.character(as.numeric(xs))

if (FALSE) {
  nums <- c("0", "0.0", "0.000", "-0", "12.34", "44.543103", "-111.3456003", "0.2", "-5", "-0.4445", ".0032", "-.00543")
  #nums <- c("-111.3456003")
  
  res1 <- sum(to_bigqs(nums))
  res2 <- sum(as.numeric(nums))
  list(as.character(as.numeric(res1)), res2)
}