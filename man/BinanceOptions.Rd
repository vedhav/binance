% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_options.R
\name{BinanceOptions}
\alias{BinanceOptions}
\title{Binance Options Class}
\description{
Class that implements the API methods for the Binance Options API.
}
\keyword{Initialize}
\keyword{`BinanceOptions`}
\keyword{a}
\keyword{internal}
\keyword{object.}
\section{Super class}{
\code{\link[binance:Binance]{binance::Binance}} -> \code{BinanceOptions}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BinanceOptions-new}{\code{BinanceOptions$new()}}
\item \href{#method-BinanceOptions-get_exchange_info}{\code{BinanceOptions$get_exchange_info()}}
\item \href{#method-BinanceOptions-price_ticker}{\code{BinanceOptions$price_ticker()}}
\item \href{#method-BinanceOptions-mark_price}{\code{BinanceOptions$mark_price()}}
\item \href{#method-BinanceOptions-excerise_records}{\code{BinanceOptions$excerise_records()}}
\item \href{#method-BinanceOptions-open_interest}{\code{BinanceOptions$open_interest()}}
\item \href{#method-BinanceOptions-clone}{\code{BinanceOptions$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="check_server_time"><a href='../../binance/html/Binance.html#method-Binance-check_server_time'><code>binance::Binance$check_server_time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="kline"><a href='../../binance/html/Binance.html#method-Binance-kline'><code>binance::Binance$kline()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="old_trade_lookup"><a href='../../binance/html/Binance.html#method-Binance-old_trade_lookup'><code>binance::Binance$old_trade_lookup()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="order_book"><a href='../../binance/html/Binance.html#method-Binance-order_book'><code>binance::Binance$order_book()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="print"><a href='../../binance/html/Binance.html#method-Binance-print'><code>binance::Binance$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="recent_trades_list"><a href='../../binance/html/Binance.html#method-Binance-recent_trades_list'><code>binance::Binance$recent_trades_list()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="sign_in"><a href='../../binance/html/Binance.html#method-Binance-sign_in'><code>binance::Binance$sign_in()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="test_connectivity"><a href='../../binance/html/Binance.html#method-Binance-test_connectivity'><code>binance::Binance$test_connectivity()</code></a></span></li>
<li><span class="pkg-link" data-pkg="binance" data-topic="Binance" data-id="ticker_stats_24hr"><a href='../../binance/html/Binance.html#method-Binance-ticker_stats_24hr'><code>binance::Binance$ticker_stats_24hr()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-new"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$new(testnet = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{testnet}}{(\code{logical}) whether to use the testnet. Default is \code{FALSE}.
Exchange Information

Current exchange trading rules and symbol information.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-get_exchange_info"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-get_exchange_info}{}}}
\subsection{Method \code{get_exchange_info()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$get_exchange_info()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(\code{list}) a list with \code{option_contract}, \code{option_asset}, and \code{option_symbol} data.
Symbol Price Ticker
Get spot index price for option underlying.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-price_ticker"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-price_ticker}{}}}
\subsection{Method \code{price_ticker()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$price_ticker(underlying)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{underlying}}{(\code{character}) the underlying symbol.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{tibble}) a tibble with the index price data.
Option Mark Price

Option mark price and greek info.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-mark_price"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-mark_price}{}}}
\subsection{Method \code{mark_price()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$mark_price(symbol = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{symbol}}{(\code{character}) the trading symbol.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{tibble}) a tibble with the mark price and option greeks data.
Historical Exercise Records

Get historical exercise records.
REALISTIC_VALUE_STRICKEN -> Exercised.
EXTRINSIC_VALUE_EXPIRED -> Expired OTM.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-excerise_records"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-excerise_records}{}}}
\subsection{Method \code{excerise_records()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$excerise_records(
  underlying = NULL,
  start_time = NULL,
  end_time = NULL,
  limit = 100
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{underlying}}{(\code{character}) the underlying symbol.}

\item{\code{start_time}}{(\code{numeric}) the start time for the exercise records in epoch milliseconds.}

\item{\code{end_time}}{(\code{numeric}) the end time for the exercise records in epoch milliseconds.}

\item{\code{limit}}{(\code{integer}) the number of results to return. Default is \code{100}; max is \code{100}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{tibble}) a tibble with the exercise records.
Open interest

Get open interest for specific underlying asset on specific expiration date.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-open_interest"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-open_interest}{}}}
\subsection{Method \code{open_interest()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$open_interest(underlying_asset, expiration)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{underlying_asset}}{(\code{character}) the underlying asset.}

\item{\code{expiration}}{(\code{character}) the expiration date.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{tibble}) a tibble with the open interest data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BinanceOptions-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BinanceOptions-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BinanceOptions$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
