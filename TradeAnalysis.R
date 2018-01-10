cur <- "BTC"

Trades <- Combined[Group=="Trade"]

TradeHist <- rbind(
  Trades[SellCur==cur][, .(Cur=BuyCur, DateTime, Rate, dBal=Sell, Type="Buy")],
  Trades[BuyCur==cur][, .(Cur=SellCur, DateTime, Rate, dBal=Buy, Type="Sell")])
TradeHist[, `:=`(Rate=as.numeric(Rate), dBal=as.numeric(dBal))]
setorder(TradeHist, Cur, DateTime)

TradeSells <- Trades[SellCur==cur, .(dTrade=as.numeric(bsum(Buy)), dBal=as.numeric(-bsum(Sell))), by=c("BuyCur", "SellCur")][, Cur := BuyCur][order(Cur)]
TradeBuys <- Trades[BuyCur==cur, .(dTrade=as.numeric(-bsum(Sell)), dBal=as.numeric(bsum(Buy))), by=c("BuyCur", "SellCur")][, Cur := SellCur][order(Cur)]
TradeAnalysis <- TradeSells[TradeBuys, on="Cur"][, .(
  Cur,
  Sold=-i.dTrade, Bought=dTrade,
  SoldAt=i.dBal, BoughtAt=-dBal,
  dTrade=i.dTrade+dTrade, 
  dBal=i.dBal+dBal)
]
TradeAnalysis[, `:=`(
  AvgSellSat = as.integer(1e8 * as.numeric(SoldAt) / Sold),
  AvgBuySat = as.integer(1e8 * as.numeric(BoughtAt) / Bought))]
TradeAnalysis[, Ratio := AvgSellSat / AvgBuySat]
TradeAnalysis[, Closed := dTrade == 0]
TradeAnalysis[dBal < 0, BreakEvenSat := as.integer(1e8 * -dBal / (Bought - Sold))]
setorder(TradeAnalysis, -dBal)
#print(paste("Trade Analysis against", cur))
TradeAnalysis

if (FALSE) {
  EMC2Crash <- TradeHist[Cur=="EMC2" & DateTime >= "2017-12-19 11:38" & DateTime <= "2017-12-19 17:00"]
  qplot(DateTime, Rate, col=Type, data=EMC2Crash, size=dBal) + facet_wrap(~Cur, scales = "free") +
    scale_color_manual(values=c(Buy="#00FF00", Sell="#FF0000")) +  scale_size_area(max_size=5) +
    geom_text(aes(label=strrep("=", as.integer(200*dBal))), col="black", size=3) +
    geom_line(aes(DateTime, Rate), data=EMC2Crash, size=0.1, inherit.aes=FALSE)
}