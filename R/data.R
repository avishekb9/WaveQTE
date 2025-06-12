#' Historical Global Market Indices Closing Prices
#'
#' A dataset containing daily closing prices for 10 global market indices
#' from 2015-05-22 onwards.
#'
#' @name market_indices
#' @format A dataframe with the following columns:
#' \describe{
#'   \item{Date}{Date in YYYY-MM-DD format}
#'   \item{GSPC}{S&P 500 Index (USA)}
#'   \item{N225}{Nikkei 225 (Japan)}
#'   \item{FTSE}{FTSE 100 (UK)}
#'   \item{FCHI}{CAC 40 (France)}
#'   \item{GDAXI}{DAX (Germany)}
#'   \item{SSE}{Shanghai Composite (China)}
#'   \item{BSE}{BSE SENSEX (India)}
#'   \item{HSI}{Hang Seng Index (Hong Kong)}
#'   \item{TWII}{Taiwan Weighted Index (Taiwan)}
#'   \item{MOEX}{MOEX Russia Index (Russia)}
#' }
#' @source Global market data
#' @examples
#' \dontrun{
#' # Load the data directly from GitHub
#' data_url <- "https://raw.githubusercontent.com/avishekb9/WaveQTE/main/market_indices.csv"
#' market_data <- read.csv(data_url)
#' head(market_data)
#' }
NULL
