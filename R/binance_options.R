#' Binance Options Class
#'
#' Class that implements the API methods for the Binance Options API.
#' @keywords internal
BinanceOptions <- R6::R6Class( # nolint
  "BinanceOptions",
  inherit = Binance,
  public = list(
    #' Initialize
    #'
    #' Initialize a `BinanceOptions` object.
    #'
    #' @param testnet (`logical`) whether to use the testnet. Default is `FALSE`.
    initialize = function(testnet = FALSE) {
      profile <- "options"
      if (testnet) {
        profile <- paste0(profile, "_test")
      }
      super$initialize(profile)
    },
    #' Exchange Information
    #'
    #' Current exchange trading rules and symbol information.
    #'
    #' @return (`list`) a list with `option_contract`, `option_asset`, and `option_symbol` data.
    get_exchange_info = function() {
      resp <- private$get_request(endpoint = private$endpoints$exchange_info) |>
        httr2::resp_body_json()
      options <- purrr::map(c("optionContracts", "optionAssets", "optionSymbols"), function(x) {
        purrr::map_df(
          resp[[x]],
          list_to_tibble
        ) |>
          janitor::clean_names()
      })
      names(options) <- c("option_contract", "option_asset", "option_symbol")
      options$option_symbol$expiry_date <- as.POSIXct(options$option_symbol$expiry_date / 1000)
      options
    },
    #' Symbol Price Ticker
    #' Get spot index price for option underlying.
    #'
    #' @param underlying (`character`) the underlying symbol.
    #'
    #' @return (`tibble`) a tibble with the index price data.
    price_ticker = function(underlying) {
      params <- list(underlying = underlying)
      private$get_request(endpoint = private$endpoints$index, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Option Mark Price
    #'
    #' Option mark price and greek info.
    #'
    #' @param symbol (`character`) the trading symbol.
    #'
    #' @return (`tibble`) a tibble with the mark price and option greeks data.
    mark_price = function(symbol = NULL) {
      params <- private$clean_params(symbol = symbol)
      private$get_request(endpoint = private$endpoints$mark, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Historical Exercise Records
    #'
    #' Get historical exercise records.
    #' REALISTIC_VALUE_STRICKEN -> Exercised.
    #' EXTRINSIC_VALUE_EXPIRED -> Expired OTM.
    #'
    #' @param underlying (`character`) the underlying symbol.
    #' @param start_time (`numeric`) the start time for the exercise records in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the exercise records in epoch milliseconds.
    #' @param limit (`integer`) the number of results to return. Default is `100`; max is `100`.
    #'
    #' @return (`tibble`) a tibble with the exercise records.
    excerise_records = function(underlying = NULL, start_time = NULL, end_time = NULL, limit = 100) {
      params <- private$clean_params(
        underlying = underlying, start_time = start_time,
        end_time = end_time, limit = limit
      )
      private$get_request(endpoint = private$endpoints$excerise_history, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    },
    #' Open interest
    #'
    #' Get open interest for specific underlying asset on specific expiration date.
    #'
    #' @param underlying_asset (`character`) the underlying asset.
    #' @param expiration (`character`) the expiration date.
    #'
    #' @return (`tibble`) a tibble with the open interest data.
    open_interest = function(underlying_asset, expiration) {
      params <- private$clean_params(
        underlyingAsset = underlying_asset, expiration = expiration
      )
      private$get_request(endpoint = private$endpoints$open_interest, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    }
  ),
  private = list(
    kline_colnames = c(
      "open", "high", "low", "close", "volume",
      "amount", "interval", "trade_count", "taker_volume",
      "taker_amount", "open_time", "close_time"
    )
  )
)

#' Binance European Options Constructor
#'
#' Constructor to create the Binance European Options API object.
#' @export
binance_options <- BinanceOptions$new
