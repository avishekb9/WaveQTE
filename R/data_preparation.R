#' Data Preparation Functions
#' @name data_preparation
#' @description Functions for data import and processing
NULL

#' Get Stock Market Data
#'
#' @param symbols Character vector of stock symbols
#' @param start_date Start date for data retrieval (YYYY-MM-DD)
#' @param end_date End date for data retrieval (YYYY-MM-DD)
#' @return An xts object containing log returns
#' @import quantmod xts zoo
#' @export
#' @examples
#' \dontrun{
#' data <- get_stock_data(c("AAPL", "MSFT"), "2020-01-01", "2020-12-31")
#' }
get_stock_data <- function(symbols, start_date = "2007-01-01", end_date = "2023-12-31") {
  message("Fetching data for ", length(symbols), " symbols")

  returns_list <- lapply(symbols, function(sym) {
    tryCatch({
      message("Processing: ", sym)
      prices <- quantmod::getSymbols(sym, from = start_date,
                                     to = end_date, auto.assign = FALSE)
      prices_adj <- quantmod::Ad(prices)
      prices_adj <- zoo::na.approx(prices_adj, na.rm = FALSE)
      prices_adj <- zoo::na.locf(prices_adj)
      prices_adj <- zoo::na.locf(prices_adj, fromLast = TRUE)
      returns <- diff(log(prices_adj))[-1]
      returns
    }, error = function(e) {
      message("Error with symbol ", sym, ": ", e$message)
      return(NULL)
    })
  })

  returns_list <- returns_list[!sapply(returns_list, is.null)]
  if (length(returns_list) == 0) {
    stop("No valid data retrieved for any symbols")
  }

  returns <- do.call(merge, returns_list)
  colnames(returns) <- symbols[1:ncol(returns)]
  return(returns)
}

#' Process Returns Data
#'
#' @param returns xts object containing return data
#' @return Processed xts object
#' @importFrom stats complete.cases sd
#' @export
#' @examples
#' \dontrun{
#' data <- get_stock_data("AAPL")
#' processed_data <- process_returns(data)
#' }
process_returns <- function(returns) {
  z_scores <- abs(scale(returns))
  returns[z_scores > 5] <- NA
  returns <- na.approx(returns, na.rm = FALSE)
  returns <- na.locf(returns)
  returns <- na.locf(returns, fromLast = TRUE)
  return(returns)
}

#' Calculate Summary Statistics
#'
#' @param returns xts object containing return data
#' @return List of statistics
#' @importFrom stats cor quantile var
#' @importFrom moments skewness kurtosis
#' @importFrom tseries jarque.bera.test adf.test
#' @export
#' @examples
#' \dontrun{
#' data <- get_stock_data("AAPL")
#' stats <- calculate_summary_stats(data)
#' }
calculate_summary_stats <- function(returns) {
  basic_stats <- data.frame(
    Mean = colMeans(returns),
    SD = apply(returns, 2, sd),
    Skewness = apply(returns, 2, skewness),
    Kurtosis = apply(returns, 2, kurtosis),
    JB_test = apply(returns, 2, function(x) jarque.bera.test(x)$p.value),
    ADF_test = apply(returns, 2, function(x) adf.test(x)$p.value)
  )

  risk_stats <- data.frame(
    VaR_95 = apply(returns, 2, quantile, probs = 0.05),
    ES_95 = apply(returns, 2, function(x)
      mean(x[x < quantile(x, 0.05)])),
    MaxDrawdown = apply(returns, 2, function(x)
      max(cummax(x) - x))
  )

  list(
    basic = basic_stats,
    risk = risk_stats,
    correlation = cor(returns)
  )
}

#' Check Data Quality
#'
#' @param market_data List of market data
#' @param crisis_periods List of crisis period dates (optional)
#' @return NULL (prints diagnostics)
#' @importFrom stats window
#' @importFrom tseries adf.test
#' @export
#' @examples
#' \dontrun{
#' data <- list(developed = get_stock_data("AAPL"))
#' crisis_periods <- list(covid = c("2020-01-01", "2020-12-31"))
#' check_data_quality(data, crisis_periods)
#' }
check_data_quality <- function(market_data, crisis_periods = NULL) {
  for(market_name in names(market_data)) {
    cat("\nChecking", market_name, "market data:")
    data <- market_data[[market_name]]

    cat("\n  Observations:", nrow(data))
    cat("\n  Assets:", ncol(data))

    if (!is.null(crisis_periods)) {
      for(crisis in names(crisis_periods)) {
        crisis_data <- window(data,
                              start = as.Date(crisis_periods[[crisis]][1]),
                              end = as.Date(crisis_periods[[crisis]][2]))
        cat("\n  ", crisis, "period observations:", nrow(crisis_data))
      }
    }

    missing <- colSums(is.na(data))
    if(any(missing > 0)) {
      cat("\n  Warning: Missing values detected:")
      print(missing[missing > 0])
    }

    adf_tests <- apply(data, 2, function(x) adf.test(x)$p.value)
    cat("\n  Non-stationary series:",
        sum(adf_tests > 0.05), "out of", ncol(data))
  }
}
