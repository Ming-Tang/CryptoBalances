curs <- names(Rates)
dates <- Rates$BTC$USD$DateTime
setorder(Combined.D, DateTime, -TOrd)
CBals.D <- Combined.D[
  , rbind(.SD, data.table(DateTime=dates, dBal="0", TOrd=0), fill=TRUE)
  , by="Cur"]

setorder(CBals.D, DateTime, -TOrd)
CBals.D <- CBals.D[
  , .(DateTime=DateTime,
      TOrd,
      dBal,
      RateSat,
      RateUSD,
      RateBTC,
      cBal=as.character(bcumsum(dBal)),
      cBal.N=as.numeric(bcumsum(dBal))),
  , by="Cur"]
setkey(CBals.D, Cur, DateTime)

#CBals.D <- Rates.USD[Rates.BTC[CBals.D, roll=-Inf], roll=-Inf]
#CBals.D[Cur=="BTC", RateSat := 1e8]
#CBals.D[Cur=="USD", RateUSD := 1]

CBals.D[, Date := as.Date(DateTime)]

CBals.D[, dBal.N := as.numeric(to_bigqs(dBal))]
CBals.D[, cBal.BTC := cBal.N*as.numeric(RateSat)*1e-8]
CBals.D[, cBal.USD := cBal.N*as.numeric(RateUSD)]

setorder(CBals.D, DateTime)
DailyBals <- CBals.D[
  , .SD[, .(cBal.USD=max(cBal.USD), cBal.BTC=max(cBal.BTC)), by="Cur"]
       [, .(cBal.USD=sum(cBal.USD), cBal.BTC=sum(cBal.BTC))]
  , by="Date"]
