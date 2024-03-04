#' `binance`: Use the Binance REST APIs in R
#'
#' This package provides an interface to the `Binance` REST API endpoints
#' for spot, futures, and options markets.
#'
#' To learn mode about the package, visit the [project website](https://vedhav.github.io/binance/)
#'
#' @keywords internal
"_PACKAGE"

#' Binance Constructor
#'
#' Class that encapsulates the core API methods to communicate with the Binance API.
#'
#' @keywords internal
Binance <- R6::R6Class( # nolint
  "Binance",
  public = list(
    #' Initialize
    #'
    #' Initialize a `Binance` object.
    #'
    #' @param profile (`string`) the name of the configuration to read the API configurations.
    initialize = function(profile) {
      private$profile <- profile
      private$base_url <- private$get_config("base_url")
      private$endpoints <- private$get_config("endpoints")
    },
    #' Print method
    #'
    #' Print high level information about the object.
    #'
    #' @param ... additional arguments to `print`
    print = function(...) {
      print(paste0("Docs: ", private$get_config("docs")), ...)
    },
    #' Setup API Key and Secret
    #'
    #' Providing the object with the Binance API key and Secret to perform authenticated requests.
    #'
    #' @param api_key (`string`) the Binance API key.
    #' Will be taken from the environment variable `BINANCE_API_KEY` if not provided.
    #' @param api_secret (`string`) the Binance API secret.
    #' Will be taken from the environment variable `BINANCE_API_SECRET` if not provided.
    sign_in = function(api_key = Sys.getenv("BINANCE_API_KEY"), api_secret = Sys.getenv("BINANCE_API_SECRET")) {
      private$api_key <- api_key
      private$api_secret <- api_secret
    },
    #' Test Connectivity
    #'
    #' Test connectivity to the Rest API by logging the connection status.
    #'
    #' @return a `httr2` response object.
    test_connectivity = function() {
      resp <- private$get_request(endpoint = private$endpoints$ping)
      if (!httr2::resp_is_error(resp)) {
        logger::log_success("Connection is successful.")
      } else {
        logger::log_error("Connection is unsuccessful.")
      }
      invisible(resp)
    },
    #' Check Server Time
    #'
    #' Test connectivity to the Rest API and get the current server time.
    #'
    #' @return a `POSIXct` object with the server time.
    check_server_time = function() {
      resp <- private$get_request(endpoint = private$endpoints$time)
      as.POSIXct(httr2::resp_body_json(resp)$serverTime / 1000)
    },
    #' Exchange Information
    #'
    #' Current exchange trading rules and symbol information
    #'
    #' @return a `tibble` with the exchange information.
    get_exchange_info = function() {
      resp <- private$get_request(endpoint = private$endpoints$exchange_info) |>
        httr2::resp_body_json()
      purrr::map_df(
        resp$symbols,
        list_to_tibble
      ) |>
        janitor::clean_names()
    },
    #' Order Book
    #'
    #' Get the order book for a given symbol.
    #'
    #' @param symbol (`string`) the trading symbol.
    #' @param limit (`integer`) the number of orders to return. Default 100; max 5000.
    #' If limit > 5000, then the response will truncate to 5000.
    #'
    #' @return a `list` with the last update id, bids, and asks.
    order_book = function(symbol, limit = 100) {
      params <- list(symbol = symbol, limit = limit)
      resp <- private$get_request(endpoint = private$endpoints$depth, params) |>
        httr2::resp_body_json()
      list(
        last_update_id = resp$lastUpdateId,
        bids = tibble::tibble(
          price = as.numeric(unlist(lapply(resp$bids, `[[`, 1))),
          quantity = as.numeric(unlist(lapply(resp$bids, `[[`, 2)))
        ),
        asks = tibble::tibble(
          price = as.numeric(unlist(lapply(resp$asks, `[[`, 1))),
          quantity = as.numeric(unlist(lapply(resp$asks, `[[`, 2)))
        )
      )
    },
    #' Recent Trades List
    #'
    #' Get recent trades for a given symbol.
    #'
    #' @param symbol (`string`) the trading symbol.
    #'
    #' @param limit (`integer`) the number of trades to return. Default 500; max 1000.
    recent_trades_list = function(symbol, limit = 500) {
      params <- list(symbol = symbol, limit = limit)
      private$get_request(endpoint = private$endpoints$trades, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Old Trade Lookup
    #'
    #' Get older market trades for a given symbol.
    #'
    #' @param symbol (`string`) the trading symbol.
    #' @param limit (`integer`) the number of trades to return. Default 500; max 1000.
    #' @param from_id (`integer`) trade id to fetch from. Default gets most recent trades.
    old_trade_lookup = function(symbol, limit = 500, from_id = NULL) {
      params <- private$clean_params(symbol = symbol, limit = limit, fromId = from_id)
      private$get_request(endpoint = private$endpoints$historical_trades, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Kline/Candlestick Data
    #'
    #' Kline/candlestick bars for a symbol. Klines are uniquely identified by their open time.
    #'
    #' @param symbol (`string`) the trading symbol.
    #' @param interval (`string`) the interval for the kline/candlestick bars.
    #' Valid values: `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`8h`,`12h`,`1d`,`3d`,`1w`, `1M`
    #' Where: s-> seconds; m -> minutes; h -> hours; d -> days; w -> weeks; M -> months
    #' @param start_time (`numeric`) the start time for the klines in epoch milliseconds.
    #' @param end_time (`numeric`) the end time for the klines in epoch milliseconds.
    #' @param time_zone (`string`) the time zone to use for the start and end time. Default is "0" (UTC).
    #' @param limit (`integer`) the number of klines to return. Default 500; max 1000.
    #'
    #' @return a `tibble` with the kline data.
    kline = function(symbol, interval, start_time = NULL, end_time = NULL, time_zone = "0", limit = 500) {
      params <- private$clean_params(
        symbol = symbol, interval = interval, startTime = start_time,
        endTime = end_time, limit = limit
      )
      resp <- private$get_request(endpoint = private$endpoints$klines, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(tibble::as_tibble, .name_repair = ~ private$kline_colnames)
      resp[1:11]
    },
    #' 24hr Ticker Price Change Statistics
    #'
    #' 24 hour rolling window price change statistics. Careful when accessing this with no symbol.
    #'
    #' @param symbols (`character`) the trading symbols of interest.
    #'
    #' @return a `tibble` with the price change statistics.
    ticker_stats_24hr = function(symbols) {
      params <- list(symbols = private$format_multiple_inputs(symbols))
      private$get_request(endpoint = private$endpoints$ticker_24hr, params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x) |>
        janitor::clean_names()
    },
    #' Symbol Price Ticker
    #'
    #' Latest price for a symbol or symbols.
    #'
    #' @param symbols (`character`) the trading symbols of interest.
    #'
    #' @return a `tibble` with the price ticker.
    price_ticker = function(symbols) {
      params <- list(symbols = private$format_multiple_inputs(symbols))
      private$get_request(endpoint = private$endpoints$ticker_price, params = params) |>
        httr2::resp_body_json() |>
        purrr::map_df(~.x)
    }
  ),
  private = list(
    profile = "default",
    endpoints = list(),
    base_url = NULL,
    api_key = NULL,
    api_secret = NULL,
    lock_requests = FALSE,
    lock_until = NULL,
    kline_colnames = c(
      "open_time", "open", "high", "low", "close", "volume",
      "close_time", "quote_asset_volume", "num_trades",
      "taker_base_volume", "taker_quote_volume", "ignore"
    ),
    clean_params = function(...) {
      params <- list(...)
      Filter(Negate(is.null), params)
    },
    get_signed_params = function(params = list()) {
      params$timestamp <- timestamp()
      params$signature <- digest::hmac(
        key = "API_KEY",
        object = paste(
          names(params),
          unlist(params),
          sep = "=",
          collapse = "&"
        ),
        algo = "sha256"
      )
      params
    },
    check_for_locks = function() {
      if (private$lock_requests) {
        if (private$lock_until < Sys.time()) {
          private$lock_requests <- FALSE
        } else {
          lock_until_secs <- difftime(Sys.time(), private$lock_until, units = "secs") |>
            round() |>
            as.character()
          rlang::abort(sprintf("Too many requests, API requests locked for %s seconds", lock_until_secs))
        }
      }
    },
    get_request = function(endpoint, params = list(), sign = FALSE) {
      private$check_for_locks()
      if (sign) {
        params <- private$get_signed_params(params)
      }
      query <- paste0(private$base_url, endpoint) |>
        httr2::request() |>
        httr2::req_url_query(!!!params)
      tryCatch(
        resp <- httr2::req_perform(query),
        error = function(err) {
          if (httr2::resp_status(err$resp) == 400) {
            error_body <- err$resp |>
              httr2::resp_body_json()
            logger::log_error("[Error code: {error_body$code}] {error_body$msg}")
          }
          private$log_additional_error_info(err)
          rlang::abort(err$message)
        }
      )
      resp
    },
    get_config = function(key) {
      config::get(
        value = key,
        config = private$profile,
        file = system.file("config.yml", package = "binance")
      )
    },
    format_multiple_inputs = function(inputs) {
      if (is.null(inputs)) {
        return(NULL)
      }
      inputs <- paste0('"', inputs, '"') |>
        paste(collapse = ",")
      paste0("[", inputs, "]")
    },
    get_used_weight = function(resp) {
      httr2::resp_headers(resp)[["x-mbx-used-weight"]] |> as.numeric()
    },
    log_additional_error_info = function(err) {
      error_class <- class(err)[1]
      if (error_class == "httr2_http_403") {
        logger::log_error("The WAF Limit (Web Application Firewall) has been violated.")
      } else if (error_class == "httr2_http_409") {
        logger::log_error("cancelReplace order partially succeeds.")
      } else if (error_class == "httr2_http_429") {
        logger::log_error("Request rate limit is broken. Will lock API access for a minute.")
        private$lock_requests <- TRUE
        private$lock_until <- Sys.time() + 60
      } else if (error_class == "httr2_http_418") {
        logger::log_error("IP has been auto-banned for sending requests after 429.")
      }
    }
  )
)
