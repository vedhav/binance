# binance

R interface to the `Binance` REST API endpoints for Spot, Futures, and Options.
`binance` is an interface for the Binance REST APIs in R using the `httr2` package and `tidyverse` styling.

It supports connecting and sending requests to:

1. Binance Spot Exchange
2. Binance USDT-M Futures Exchange
3. Binance Coin-M Futures Exchange
4. Binance European Options Exchange


## Installation

```r
pak::pak("vedhav/binance")
```

## Usage
  
```r
library(binance)
spot <- binance_spot()
futures_usdt_m <- binance_futures_usdt_m()
futures_coin_m <- binance_futures_coin_m()
options <- binance_options()

spot$test_connectivity()
#> SUCCESS [2024-03-04 07:48:25] Connection is successful.
futures_usdt_m$test_connectivity()
#> SUCCESS [2024-03-04 07:48:27] Connection is successful.
futures_coin_m$test_connectivity()
#> SUCCESS [2024-03-04 07:48:29] Connection is successful.
options$test_connectivity()
#> SUCCESS [2024-03-04 07:48:31] Connection is successful.
```
