
calc_ACB <- function(Rate, dBal) {
  # cBal: current balance
  # dACB: change in adjusted cost basis 
  # cACB: current adjusted cost basis
  # cAvC: current average cost
  
  Q <- to_bigqs
  Ch <- as.character
  NQ <- function(x) Ch(as.numeric(Q(x)))
  
  DBals <- data.table(Rate=Rate, dBal=dBal)
  DBals[, `:=`(cBal="0", dACB="0", cACB="0", cAvC="0")]
  
  cACB_ <- as.bigq(0)
  cBal_ <- as.bigq(0)
  div <- function(x, y) if (is.na(y) || y == 0) as.bigq(NA) else x/y
  for (i in 1L:nrow(DBals)) {
    row <- DBals[i]
    rate <- Q(row$Rate)
    dBal_ = Q(row$dBal)
    if (dBal_ > 0) {
      dBuy <- dBal_ * rate
      cACB_ <- cACB_ + dBuy
      cBal_ <- cBal_ + dBal_
      cAvC_ <- div(cACB_, cBal_)
      DBals[i, `:=`(cBal=Ch(cBal_), dACB=Ch(dBuy), cACB=Ch(cACB_), cAvC=Ch(cAvC_))]
    } else if (dBal_ == 0) {
      DBals[i, `:=`(cBal=Ch(cBal_), dACB=Ch(0), cACB=Ch(cACB_), cAvC=Ch(cAvC_))]
    } else {
      dSell <- dBal_ * div(cACB_, cBal_)
      cACB_ <- cACB_ + dSell
      cBal_ <- cBal_ + dBal_
      cAvC_ <- div(cACB_, cBal_)
      DBals[i, `:=`(cBal=Ch(cBal_), dACB=Ch(dSell), cACB=Ch(cACB_), cAvC=Ch(cAvC_))]
    }
    #print(list(row, cACB_))
  }
  
  DBals[, `:=`(cBal.N=NQ(cBal), dBal.N=NQ(dBal), Rate.N=NQ(Rate), dACB.N=NQ(dACB), cACB.N=NQ(cACB), cAvC.N=NQ(cAvC))]
  DBals
}

# Usage (for one currency)
DBals <- Combined.D[Cur=="SC"][, c(.SD, calc_ACB(RateUSD, dBal))]
setorder(DBals, DateTime)
View(DBals[, .(P, DateTime, Cur, RateBTC=Rate, RateUSD, cBal.N, dBal.N, Rate.N, dACB.N, cACB.N, cAvC.N)])

# Usage (for all currencies)
# Combined.D[, c(.SD, calc_ACB(RateUSD, dBal)), by=Cur]
# View(Combined.D[, .SD[.N], by=Cur][, RateUSD := NULL][
#         , c(.SD, RateUSD=usd_rate(Cur)), by=Cur
#       ][cBal.N != 0, .(Cur, cBal.N, RateBTC, RateUSD, cACB.N, 
#                       cAvC.N, Ratio= round(as.numeric(RateUSD) / as.numeric(cAvC.N),4) )])