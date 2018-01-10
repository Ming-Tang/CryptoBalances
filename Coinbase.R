CoinbaseTransactions <- read_csv(
  "~/Downloads/CoinbaseTransactions.csv",
  skip = 4,
  col_types = cols_only(
    Amount = col_guess(),
    Balance = col_guess(), Currency = col_guess(),
    Notes = col_guess(), Timestamp = col_guess(),
    To = col_guess(), `Transfer Fee` = col_guess(),
    `Transfer Fee Currency` = col_guess(),
    `Transfer Total` = col_guess(),
    `Transfer Total Currency` = col_guess()))
View(CoinbaseTransactions)
