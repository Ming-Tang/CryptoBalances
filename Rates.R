
get_rates <- function(curA, curB) {
  path <- paste0("./rates/", curA, "_", curB, ".json")
  if (file.exists(path)) {
    resp <- fromJSON(path)
  } else {
    url <- paste0("https://min-api.cryptocompare.com/data/histohour?fsym=", curA, "&tsym=", curB, "&e=CCCAGG&limit=10000")
    resp <- fromJSON(url)
    write_json(resp, path, digits=32)
  }
  if (resp$Response != "Success") {
    NULL
  } else {
    Data <- resp$Data
    if (length(Data) == 0) { NULL }
    else {
      res <- data.table(
        DateTime=as.POSIXct(Data$time, origin="1970-01-01", tz="UTC"),
        Close=Data$close, High=Data$high, Low=Data$low, Open=Data$open)
      
      setorder(res, DateTime)
      res
    }
  }
}


curAs <- c(sort(unique(Combined.D$Cur)), "XLM")
curBs <- c("USD", "BTC")

Rates <- list()
Rates.N <- list()
for (curA in curAs) {
  for (curB in curBs) {
    #print(sprintf("%s/%s", curA, curB))
    if (curA == curB) {
      Rates[[curA]][[curB]] <- data.table(DateTime=NA, Close="1", High="1", Low="1", Open="1")
      Rates.N[[curA]][[curB]] <- data.table(DateTime=NA, Close=1, High=1, Low=1, Open=1)
    } else {
      p <- get_rates(curA, curB) 
      if (!is.null(p))  {
        Rates[[curA]][[curB]] <- p[DateTime >= "2017-01-01"]
        Rates.N[[curA]][[curB]] <- p[DateTime >= "2017-01-01"][
          , .(DateTime, Close=as.numeric(Close), High=as.numeric(High), Low=as.numeric(Low), Open=as.numeric(Open))]
      }
    }
  }
}

Rates$STR <- Rates$XLM
Rates.N$STR <- Rates.N$XLM
