#' Binance Futures Constructor
#'
#' Class that encapsulates the common API methods for both USDT-M and Coin-M futures.
#'
#' @keywords internal
BinanceFutures <- R6::R6Class( # nolint
  "BinanceFutures",
  inherit = Binance,
  public = list(
    #' Initialize
    #'
    #' Initialize a `BinanceFutures` object.
    #'
    #' @param profile (`string`) the name of the configuration to read the API configurations.
    initialize = function(profile) {
      super$initialize(profile)
    },
    #' Compressed/Aggregate Trades List
    #'
    #' Get compressed, aggregate market trades.
    #' Market trades that fill in 100ms with the same price and the same taking side will have the quantity aggregated.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param from_id (`numeric`) trade id to fetch from.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1000`.
    #'
    #' @return (`tibble`) a tibble with the aggregate trades data.
    aggregate_trades = function(symbol, from_id = NULL, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        symbol = symbol, fromId = from_id, startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$aggregate_trades, params = params)
      resp |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        dplyr::rename(
          aggregate_trade_id = "a", price = "p", quantity = "q",
          first_trade_id = "f", last_trade_id = "l", timestamp = "T",
          is_buyer_maker = "m", is_best_match = "m"
        )
    },
    #' Continuous Contract Kline/Candlestick Data
    #'
    #' Kline/candlestick bars for a specific contract type.
    #' Klines are uniquely identified by their open time.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param contract_type (`character`) the contract type.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1500`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    continuous_kline = function(pair, contract_type, interval, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        pair = pair, contractType = contract_type, interval = interval,
        startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$continuous_kline, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[1:11]
    },
    #' Index Price Kline/Candlestick Data
    #'
    #' Kline/candlestick bars for a specific contract type.
    #' Klines are uniquely identified by their open time.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1500`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    index_price_klines = function(pair, interval, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        pair = pair, interval = interval, startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$index_price_klines, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[, 1:7]
    },
    #' Mark Price Kline/Candlestick Data
    #'
    #' Kline/candlestick bars for a specific contract type.
    #' Klines are uniquely identified by their open time.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1500`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    mark_price_klines = function(symbol, interval, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        symbol = symbol, interval = interval, startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$mark_price_klines, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[, 1:7]
    },
    #' Premium index Kline Data
    #'
    #' Kline/candlestick bars for a specific contract type.
    #' Klines are uniquely identified by their open time.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1500`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    premium_index_klines = function(symbol, interval, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        symbol = symbol, interval = interval, startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$premium_index_klines, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[, 1:7]
    },
    #' Symbol Order Book Ticker
    #'
    #' Best price/qty on the order book for a symbol or symbols.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the price ticker data.
    order_book_ticker = function(symbol = NULL) {
      params <- private$clean_params(symbol = symbol)
      private$get_request(private$endpoints$ticker_book, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Open Interest
    #'
    #' Get present open interest of a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the open interest data.
    open_interest = function(symbol) {
      params <- list(symbol = symbol)
      private$get_request(private$endpoints$open_interest, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Quarterly Contract Settlement Price
    #'
    #' Get the quarterly settlement price for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #'
    #' @return (`tibble`) a tibble with the settlement price data.
    quarterly_settlement_price = function(pair) {
      params <- list(pair = pair)
      private$get_request(private$endpoints$delivery_price, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Open Interest Statistics
    #'
    #' Get open interest statistics for a specific pair.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the open interest statistics.
    index_constituents = function(symbol) {
      params <- list(symbol = symbol)
      resp <- private$get_request(private$endpoints$constituents, params = params) |>
        httr2::resp_body_json()
      purrr::map_df(resp$constituents, ~.x) |>
        dplyr::mutate(index_symbol = resp$symbol, time = resp$time, .before = exchange)
    },
    #' Basis
    #'
    #' Get basis data for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param contract_type (`character`) the contract type.
    #' Ppossible values: "CURRENT_QUARTER", "NEXT_QUARTER", "PERPETUAL"
    #' @param period (`character`) the period for the basis.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the basis in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the basis in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the basis data.
    basis = function(pair, contract_type, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, contractType = contract_type, period = period,
        limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$basis, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    }
  ),
  private = list()
)

#' Binance USDT-M Futures Class
#'
#' Class that implements the API methods for the Binance USDT-M Futures API.
#' @keywords internal
BinanceFuturesUSDTM <- R6::R6Class( # nolint
  "BinanceFuturesUSDTM",
  inherit = BinanceFutures,
  public = list(
    #' Initialize
    #'
    #' Initialize a `BinanceFuturesUSDTM` object.
    #'
    #' @param testnet (`logical`) whether to use the testnet. Default is `FALSE`.
    initialize = function(testnet = FALSE) {
      profile <- "futures_usdt_m"
      if (testnet) {
        profile <- paste0(profile, "_test")
      }
      super$initialize(profile)
    },
    #' Mark Price
    #'
    #' Mark Price and Funding Rate
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the mark price and funding rate data.
    mark_price = function(symbol = NULL) {
      params <- private$clean_params(symbol = symbol)
      private$get_request(private$endpoints$premium_index, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Get Funding Rate History
    #'
    #' Get funding rate history. Rate Limit 500/5min/IP
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `100`; max is `1000`.
    #'
    #' @return (`tibble`) a tibble with the funding rate history data.
    funding_rate_history = function(symbol = NULL, start_time = NULL, end_time = NULL, limit = 100) {
      params <- private$clean_params(
        symbol = symbol, startTime = start_time, endTime = end_time, limit = limit
      )
      private$get_request(private$endpoints$funding_rate, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Get Funding Rate Info
    #'
    #' Query funding rate info for symbols that had FundingRateCap/ FundingRateFloor / fundingIntervalHours adjustment
    #'
    #' @return (`tibble`) a tibble with the funding rate info.
    funding_info = function() {
      private$get_request(private$endpoints$funding_info) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        dplyr::select(-disclaimer)
    },
    #' Symbol Price Ticker V2
    #'
    #' Latest price for a symbol or symbols.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the price ticker data.
    price_ticker_v2 = function(symbol = NULL) {
      params <- private$clean_params(symbol = symbol)
      private$get_request(private$endpoints$ticker_price_v2, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Open Interest Statistics
    #'
    #' Get open interest statistics for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param period (`character`) the period for the open interest statistics.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the open interest statistics in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the open interest statistics in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the open interest statistics.
    open_interest_stats = function(symbol, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        symbol = symbol, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$oi_stats, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Top Trader Long/Short Ratio (Accounts)
    #'
    #' Get top trader long/short ratio (accounts) for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (accounts) data.
    top_ls_ratio_accounts = function(symbol, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        symbol = symbol, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$top_ls_ratio_accounts, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Top Trader Long/Short Ratio (Positions)
    #'
    #' Get top trader long/short ratio (positions) for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (positions) data.
    top_ls_ratio_positions = function(symbol, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        symbol = symbol, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$top_ls_ratio_positions, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Long/Short Ratio
    #'
    #' Get long/short ratio for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param period (`character`) the period for the long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the long/short ratio data.
    ls_ratio = function(symbol, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        symbol = symbol, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$ls_ratio, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Taker Buy/Sell Volume
    #'
    #' Get taker buy/sell volume for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param period (`character`) the period for the taker buy/sell volume.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the taker buy/sell volume in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the taker buy/sell volume in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the taker buy/sell volume data.
    taker_buy_sell_volume = function(symbol, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        symbol = symbol, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$taker_buy_sell_volume, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Historical BLVT NAV Kline/Candlestick
    #'
    #' The BLVT NAV system is based on Binance Futures.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1500`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    bltv_kline = function(symbol, interval, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        symbol = symbol, interval = interval, startTime = start_time, endTime = end_time, limit = limit
      )
      resp <- private$get_request(private$endpoints$bltv_kline, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$bltv_kline_colnames)
      resp |>
        dplyr::select(!dplyr::matches("ignore"))
    },
    #' Composite Index Symbol Information
    #'
    #' Get composite index symbol information.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the composite index symbol information.
    composite_index_info = function(symbol = NULL) {
      params <- private$clean_params(symbol = symbol)
      resp <- private$get_request(private$endpoints$index_info, params = params) |>
        httr2::resp_body_json()
      if (is.null(names(resp))) {
        resp |>
          purrr::map_df(list_to_tibble) |>
          janitor::clean_names()
      } else {
        resp |>
          tibble::as_tibble() |>
          janitor::clean_names()
      }
    },
    #' Multi-Assets Mode Asset Index
    #'
    #' Get Asset index for Multi-Assets mode.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the asset index data.
    multi_asset_index = function(symbol = NULL) {
      params <- private$clean_params(symbol = NULL)
      resp <- private$get_request(private$endpoints$asset_index, params = params) |>
        httr2::resp_body_json()
      if (is.null(names(resp))) {
        resp |>
          purrr::map_df(list_to_tibble) |>
          janitor::clean_names()
      } else {
        resp |>
          tibble::as_tibble() |>
          janitor::clean_names()
      }
    }
  ),
  private = list(
    bltv_kline_colnames = c(
      "open_time", "open", "high", "low", "close", "real_leverage", "close_time",
      "ignore_1", "num_of_updates", "ignore_2", "ignore_3", "ignore_4"
    )
  )
)

#' Binance USDT-M Constructor
#'
#' Constructor to create the Binance USDT-M Futures API object.
#' @export
binance_futures_usdt_m <- BinanceFuturesUSDTM$new

#' Binance Coin-M Futures Class
#'
#' Class that implements the API methods for the Binance Coin-M Futures API.
#' @keywords internal
BinanceFuturesCoinM <- R6::R6Class( # nolint
  "BinanceFuturesCoinM",
  inherit = BinanceFutures,
  public = list(
    #' Initialize
    #'
    #' Initialize a `BinanceFuturesCoinM` object.
    #'
    #' @param testnet (`logical`) whether to use the testnet. Default is `FALSE`.
    initialize = function(testnet = FALSE) {
      profile <- "futures_coin_m"
      if (testnet) {
        profile <- paste0(profile, "_test")
      }
      super$initialize(profile)
    },
    #' Old Trades Lookup
    #'
    #' Get older market historical trades.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1000`.
    #' @param from_id (`numeric`) trade id to fetch from.
    old_trade_lookup = function(symbol, limit = 500, from_id = NULL) {
      # TODO: Need auth for some reason!
      params <- private$clean_params(symbol = symbol, limit = limit, fromId = from_id)
      private$get_request(endpoint = private$endpoints$historical_trades, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' 24hr Ticker Price Change Statistics
    #'
    #' 24 hour rolling window price change statistics.
    #' Careful when accessing this with no symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the 24 hour ticker price change statistics.
    ticker_stats_24hr = function(symbol) {
      params <- list(symbol = symbol)
      private$get_request(endpoint = private$endpoints$ticker_24hr, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Index Price and Mark Price
    #'
    #' Get the index price and mark price for a specific symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param pair (`character`) the trading pair.
    #'
    #' @return (`tibble`) a tibble with the index and mark price data.
    index_and_mark_price = function(symbol = NULL, pair = NULL) {
      if (is.null(symbol) && is.null(pair)) {
        rlang::abort("Either `symbol` or `pair` can be provided. Or both can be `NULL` to get all the data.")
      }
      params <- private$clean_params(symbol = symbol, pair = pair)
      private$get_request(private$endpoints$premium_index, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Get Funding Rate Info
    #'
    #' Query funding rate info for symbols that had FundingRateCap/ FundingRateFloor / fundingIntervalHours adjustment
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `100`; max is `1000`.
    #'
    #' @return (`tibble`) a tibble with the funding rate info.
    funding_rate = function(symbol, start_time = NULL, end_time = NULL, limit = 100) {
      params <- private$clean_params(symbol = symbol, startTime = start_time, endTime = end_time, limit = limit)
      private$get_request(private$endpoints$funding_rate, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Open Interest Statistics
    #'
    #' Get open interest statistics for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param contract_type (`character`) the contract type.
    #' Possible values: "CURRENT_QUARTER", "NEXT_QUARTER", "PERPETUAL"
    #' @param period (`character`) the period for the open interest statistics.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the open interest statistics in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the open interest statistics in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the open interest statistics.
    open_interest_stats = function(pair, contract_type, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, contractType = contract_type, period = period,
        limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$oi_stats, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Top Trader Long/Short Ratio (Accounts)
    #'
    #' Get top trader long/short ratio (accounts) for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (accounts) data.
    top_ls_ratio_accounts = function(pair, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$top_ls_ratio_accounts, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Top Trader Long/Short Ratio (Positions)
    #'
    #' Get top trader long/short ratio (positions) for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (accounts) data.
    top_ls_ratio_positions = function(pair, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$top_ls_ratio_positions, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Long/Short Ratio
    #'
    #' Get long/short ratio for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (accounts) data.
    ls_ratio = function(pair, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, period = period, limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$ls_ratio, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Taker Buy/Sell Volume
    #'
    #' Get taker buy/sell volume for a specific pair.
    #'
    #' @param pair (`character`) the trading pair.
    #' @param period (`character`) the period for the top trader long/short ratio.
    #' Possible values: "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d"
    #' @param contract_type (`character`) the contract type.
    #' Possible values: "CURRENT_QUARTER", "NEXT_QUARTER", "PERPETUAL"
    #' @param limit (`integer`) the number of results to return. Default is `30`; max is `500`.
    #' @param start_time (`numeric`) the start time for the top trader long/short ratio in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the top trader long/short ratio in epoch milliseconds.
    #'
    #' @return (`tibble`) a tibble with the top trader long/short ratio (accounts) data.
    taker_buy_sell_volume = function(pair, contract_type, period, limit = 30, start_time = NULL, end_time = NULL) {
      params <- private$clean_params(
        pair = pair, contractType = contract_type, period = period,
        limit = limit, startTime = start_time, endTime = end_time
      )
      private$get_request(private$endpoints$taker_buy_sell_volume, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    }
  ),
  private = list()
)

#' Binance Coin-M Constructor
#'
#' Constructor to create the Binance Coin-M Futures API object.
#' @export
binance_futures_coin_m <- BinanceFuturesCoinM$new
