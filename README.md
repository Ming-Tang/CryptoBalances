Cryptocurrency Balance Tracker
==============================

Setup
-----
  
Get RStudio and install packages `data.table, gmp readr, readxl, ggplot2, jsonlite`.

Create `import.R`, which will contain code to import data.

Run document `Report.Rmd`.

Supported Data Sources
======================

 - Etherscan API
 - Electrum wallet history in CSV
 - Poloniex: trades, deposit, withdraw in CSV
 - Cryptopia: trades, deposit, withdraw in CSV
 - Binance: trades, deposit, withdraw in CSV
 
Cryptocurrency exchanges rates are provided by CryptoCompare API.
The rates are cached in the `rates/` folder.

Data Format
===========

Each row is a transaction, which consists of a `Buy` and `Sell` pair.
Buy represents increase in balance and Sell represents decrease in balance.

 - `Date`: Date (year-month-day) of the transaction
 - `DateTime`: Date and time of the transaction
 - `BuyCur`: Currency of the Buy half of the transaction
 - `Buy`: Change of balance for buy
 - `SellCur`: Currency of the Sell half of the transaction
 - `Sell`: Change of balance for sell
 - `Exchange`: Exchange (or wallet) of the transaction
 - `Group`: Category of the transaction (such as `"Trade"`)
 - `Transaction`: Txid of wallet (non-trade) transactions
 - `From`, `To`: Addresses for wallet transactions
 - `Id`: Used to group two rows into a same transaction, i.e. trading fee and trade in Binance

Number Representation
---------------------

All cryptocurrency balances (and changes in balances) are stored as strings, in order to be summed up exactly.
The strings must be valid inputs for GMP `as.bigq` function. Decimal values are not supported by `as.bigq`.
The `to_bigqs` function converts strings to GMP rational numbers.

The `bsum`, `bcumsum` functions compute the sum and cumulative sums for balances.

Delta Form
----------

The `to_delta` function converts the dataset into a form where each row specifies a change in 
balance in one currency. For example, a buy/sell pair in a trade will be split into two rows.

 - `Cur`: The currency for the change in balance
 - `dBal`: Change in balance for the specified currency
 - `TId`: Used to group two rows into same the transaction in original form

Known Issues
============

 - Ethereum token balances in wallets are not retrieved.
 - Bitcoin Cash automatically issued by exchanges are not taken into account.
 - Transaction fees are only listed for Etherscan transactions.

Sample `import.R`
=================

```{r}
Etherscan <- import_Etherscan("0x<ADDRESS_HERE>")

Electrum_BTC <- import_Electrum("~/electrum-history.csv", "BTC")

Poloniex.TH <- import_Poloniex_TradeHistory("tradeHistory-2.csv")
Poloniex.DH <- import_Poloniex_DepositHistory("depositHistory-3.csv")
Poloniex.WH <- import_Poloniex_WithdrawHistory("withdrawalHistory.csv")

Cryptopia.TH <- import_Cryptopia_TradeHistory("Trade_History-6.csv")
Cryptopia.DH <- import_Cryptopia_DepositHistory("Deposit_History.csv")
Cryptopia.WH <- import_Cryptopia_WithdrawHistory("Withdraw_History.csv")

# Must change file extension from .csv to .xslx
Binance.TH <- import_Binance_TradeHistory("TradeHistory-2.xlsx")
Binance.DH <- import_Binance_DepositHistory("DepositHistory-6.xlsx")
Binance.WH <- import_Binance_WithdrawHistory("WithdrawalHistory-3.xlsx")

# Trade history in Buy/Sell pair form
Combined <- rbindlist(list(
  Cryptopia.TH, Cryptopia.DH, Cryptopia.WH, 
  Poloniex.TH, Poloniex.DH, Poloniex.WH,
  Binance.TH, Binance.DH, Binance.WH,
  Etherscan, Electrum_BTC), fill=TRUE)

```