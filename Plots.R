library(ggplot2)

setorder(Combined.D, DateTime)
Combined.D[, K := as.character(DateTime)]
DTs <- data.table(DateTime=unique(Combined.D$DateTime))
DTs[, K := as.character(DateTime)]
Bals <- Combined.D[
  , merge(.SD[, .(K, Cur, dBal)],
          DTs, all.y=TRUE, by="K" 
          )[ , dBal := ifelse(is.na(dBal), "0", dBal)], by=Cur]

Bals <- Bals[, .(DateTime, dBal, cBal = as.numeric(bcumsum(dBal))), by=Cur]
#Bals[, cBal := cumsum(as.numeric(dBal))]

qplot(DateTime, cBal, data=Bals[cBal>=0], geom="line", col=Cur) +
  facet_wrap(~Cur, scales = "free_y")
