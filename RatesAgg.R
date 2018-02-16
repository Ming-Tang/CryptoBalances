Rates.BTC <- list()
for (n in names(Rates)) {
  Rates.BTC[[n]] <- Rates[[n]]$BTC[
    , .(Cur=as.character(n),
        DateTime.c=as.character(DateTime),
        RateSat=as.integer(as.numeric(Close)*1e8),
        RateBTC=as.numeric(Close))
    ]
}

Rates.BTC <- rbindlist(Rates.BTC)
Rates.BTC[, DateTime := as.POSIXct(DateTime.c)]
Rates.BTC[, DateTime.c := NULL]

Rates.USD <- list()
for (n in names(Rates)) {
  Rates.USD[[n]] <- Rates[[n]]$USD[
    , .(Cur=as.character(n), DateTime.c=as.character(DateTime), RateUSD=as.numeric(Close))]
}

Rates.USD <- rbindlist(Rates.USD)
Rates.USD[, DateTime := as.POSIXct(DateTime.c)]
Rates.USD[, DateTime.c := NULL]

d0 <- min(Rates.BTC$DateTime)
Rates.BTC[Cur=="BTC", DateTime := d0]
Rates.USD[Cur=="USD", DateTime := d0]

setkey(Rates.BTC, Cur, DateTime)
setkey(Rates.USD, Cur, DateTime)

# Usage:
# add_rates(Combined.D[Cur=="NEO"], Rates.USD[Cur=="NEO"], "NEO")
add_rates <- function(DT, Rates, cur) {
  C <- DT[order(-DateTime)]
  R <- Rates[Cur==cur]
  setkey(C, DateTime)
  setkey(R, DateTime)
  RC <- R[C, roll=-Inf]
  RC[, Cur := NULL]
  return(RC)
}
