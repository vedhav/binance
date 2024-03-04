#' Binance Spot Class
#'
#' Class that implements the API methods for the Binance Spot API.
#' @keywords internal
BinanceSpot <- R6::R6Class( # nolint
  "BinanceSpot",
  inherit = Binance,
  public = list(
    #' Initialize
    #'
    #' Initialize a `BinanceSpot` object.
    #'
    #' @param testnet (`logical`) whether to use the testnet. Default is `FALSE`.
    initialize = function(testnet = FALSE) {
      profile <- "spot"
      if (testnet) {
        profile <- paste0(profile, "_test")
      }
      super$initialize(profile)
    },
    #' Compressed/Aggregate Trades List
    #'
    #' Get compressed, aggregate trades.
    #' Trades that fill at the time, from the same order, with the same price will have the quantity aggregated.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param from_id (`numeric`) trade id to fetch from. Default is `NULL`.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1000`.
    #'
    #' @return (`tibble`) a tibble with the aggregate trades data.
    aggregate_trades_list = function(symbol, from_id = NULL, start_time = NULL, end_time = NULL, limit = 500) {
      params <- private$clean_params(
        symbol = symbol, from_id = from_id,
        start_time = start_time, end_time = end_time
      )
      private$get_request(endpoint = private$endpoints$aggregate_trades, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        dplyr::rename(
          aggregate_trade_id = "a", price = "p", quantity = "q",
          first_trade_id = "f", last_trade_id = "l", timestamp = "T",
          is_buyer_maker = "m", is_best_match = "M"
        )
    },
    #' UIKlines
    #'
    #' The request is similar to klines having the same parameters and response.
    #' `uiKlines`` return modified kline data, optimized for presentation of candlestick charts.
    #'
    #' @param symbol (`character`) the trading symbol.
    #' @param interval (`character`) the interval for the klines.
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param time_zone (`character`) the time zone. Default is "0" (UTC).
    #' @param limit (`integer`) the number of results to return. Default is `500`; max is `1000`.
    #'
    #' @return (`tibble`) a tibble with the kline data.
    get_ui_kline_data = function(symbol, interval, start_time = NULL, end_time = NULL, time_zone = "0", limit = 500) {
      params <- private$clean_params(
        symbol = symbol, interval = interval, startTime = start_time,
        endTime = end_time, limit = limit
      )
      resp <- private$get_request(endpoint = private$endpoints$ui_klines, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[1:11]
    },
    #' Current Average Price
    #'
    #' Current average price for a symbol.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the average price data.
    get_current_avg_price = function(symbol) {
      params <- list(symbol = symbol)
      private$get_request(endpoint = private$endpoints$avg_price, params) |>
        httr2::resp_body_json() |>
        tibble::as_tibble() |>
        janitor::clean_names()
    },
    #' 24hr Ticker Price Change Statistics
    #'
    #' 24 hour rolling window price change statistics. Careful when accessing this with no symbol.
    #'
    #' @param symbols (`character`) the trading symbols.
    #'
    #' @return (`tibble`) a tibble with the price change statistics.
    trading_day_ticker = function(symbols) {
      params <- list(symbols = private$format_multiple_inputs(symbols))
      private$get_request(endpoint = private$endpoints$ticker_trading_day, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Symbol Price Ticker
    #'
    #' Best price/qty on the order book for a symbol or symbols.
    #'
    #' @param symbols (`character`) the trading symbols.
    #'
    #' @return (`tibble`) a tibble with the price ticker data.
    order_book_ticker = function(symbols) {
      params <- list(symbols = private$format_multiple_inputs(symbols))
      private$get_request(endpoint = private$endpoints$ticker_book, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Rolling window price change statistics
    #'
    #' Note: This method is different from the `ticker_stats_24hr` method.
    #' The window used to compute statistics will be no more than 59999ms from the requested windowSize.
    #' openTime for this method always starts on a minute, while the closeTime is the current time of the request.
    #' As such, the effective window will be up to 59999ms wider than windowSize.
    #' E.g. If the closeTime is 1641287867099 (January 04, 2022 09:17:47:099 UTC),
    #' and the windowSize is 1d. the openTime will be: 1641201420000 (January 3, 2022, 09:17:00 UTC)
    #'
    #' @param symbols (`character`) the trading symbols.
    #' @param window_size (`character`) the window size for the statistics. Default is "1d".
    #' Supported windowSize values:
    #'     1m,2m....59m for minutes
    #'     1h, 2h....23h - for hours
    #'     1d...7d - for days
    #' Units cannot be combined (e.g. 1d2h is not allowed).
    #' @param type (`character`) the type of statistics. Default is "FULL"; Supported values are "FULL" or "MINI".
    price_change_stats = function(symbols, window_size = "1d", type = "FULL") {
      params <- list(symbols = private$format_multiple_inputs(symbols), windowSize = window_size, type = type)
      private$get_request(endpoint = private$endpoints$ticker, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    }
  ),
  private = list()
)

#' Binance Spot Constructor
#'
#' Constructor to create the Binance Spot API object.
#' @export
binance_spot <- BinanceSpot$new
