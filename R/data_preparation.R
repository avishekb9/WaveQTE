#' Data Preparation Functions
#' @name data_preparation
#' @description Functions for data import and processing
NULL

#' Get Stock Market Data
#'
#' @param symbols Character vector of stock symbols (only used when fetching from Yahoo Finance)
#' @param start_date Start date for data retrieval (YYYY-MM-DD)
#' @param end_date End date for data retrieval (YYYY-MM-DD)
#' @param data_source Source of data. Options:
#'        "yahoo" (default): Fetch from Yahoo Finance
#'        "builtin": Use built-in market indices dataset
#'        "file": Load from a local CSV file (specify file_path)
#'        "custom": Use a pre-loaded data frame (specify custom_data)
#' @param file_path Path to a local CSV file when data_source="file"
#' @param custom_data A data frame when data_source="custom"
#' @param date_col Name of the date column (default: "Date")
#' @param indices Character vector of columns to retrieve.
#'        If NULL (default), returns all available columns.
#' @param return_prices Logical, whether to return prices instead of returns (default: FALSE)
#' @return An xts object containing log returns or prices
#' @import quantmod xts zoo
#' @importFrom utils read.csv
#' @export
#' @examples
#' # Method 1: Using built-in dataset
#' data <- get_stock_data(data_source = "builtin")
#' head(data)
#'
#' # Method 2: Using built-in dataset with selected indices
#' developed_markets <- get_stock_data(data_source = "builtin", 
#'                                   indices = c("GSPC", "N225", "FTSE"))
#' head(developed_markets)
#'
#' # Method 3: Using a local CSV file
#' \dontrun{
#'   data <- get_stock_data(data_source = "file", 
#'                         file_path = "path/to/your/data.csv")
#'   head(data)
#' }
#'
#' # Method 4: Using a pre-loaded custom data frame
#' \dontrun{
#'   my_data <- read.csv("my_stocks.csv")
#'   data <- get_stock_data(data_source = "custom", 
#'                         custom_data = my_data,
#'                         date_col = "MyDateColumn")
#'   head(data)
#' }
#'
#' # Method 5: Fetch data from Yahoo Finance (requires internet)
#' \dontrun{
#'   data <- get_stock_data(c("AAPL", "MSFT"), 
#'                         start_date = "2020-01-01", 
#'                         end_date = "2020-12-31")
#'   head(data)
#' }
get_stock_data <- function(symbols = NULL, 
                          start_date = "2007-01-01", 
                          end_date = "2023-12-31",
                          data_source = "yahoo", 
                          file_path = NULL,
                          custom_data = NULL,
                          date_col = "Date",
                          indices = NULL, 
                          return_prices = FALSE) {
  
  # Handle different data sources
  if (data_source == "builtin") {
    # URL to the raw CSV file on GitHub
    data_url <- "https://raw.githubusercontent.com/avishekb9/WaveQTE/master/market_indices.csv"
    # Or if you uploaded to data folder:
    # data_url <- "https://raw.githubusercontent.com/avishekb9/WaveQTE/master/data/market_indices.csv"
    
    # Download and read the data
    market_data <- tryCatch({
      read.csv(data_url)
    }, error = function(e) {
      stop("Could not download built-in dataset. Please check your internet connection.")
    })
    
  } else if (data_source == "file") {
    # Check if file path is provided
    if (is.null(file_path)) {
      stop("file_path must be provided when data_source='file'")
    }
    
    # Read the data from local file
    market_data <- tryCatch({
      read.csv(file_path)
    }, error = function(e) {
      stop("Could not read file: ", e$message)
    })
    
  } else if (data_source == "custom") {
    # Check if custom data is provided
    if (is.null(custom_data) || !is.data.frame(custom_data)) {
      stop("custom_data must be a valid data frame when data_source='custom'")
    }
    
    # Use the provided data
    market_data <- custom_data
    
  } else if (data_source == "yahoo") {
    # Check if symbols are provided
    if (is.null(symbols) || length(symbols) == 0) {
      stop("symbols must be provided when data_source='yahoo'")
    }
    
    # Use original implementation for Yahoo Finance
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
    
  } else {
    stop("Invalid data_source. Must be one of: 'yahoo', 'builtin', 'file', or 'custom'")
  }
  
  # For built-in, file, or custom data sources:
  
  # Check if date column exists
  if (!date_col %in% colnames(market_data)) {
    stop("Date column '", date_col, "' not found in the data")
  }
  
  # Convert the date column to proper Date format if it's not already
  if (!inherits(market_data[[date_col]], "Date")) {
    market_data[[date_col]] <- as.Date(market_data[[date_col]])
  }
  
  # Convert to xts format
  prices <- xts(market_data[, !colnames(market_data) %in% date_col], 
               order.by = market_data[[date_col]])
  
  # Calculate returns if needed
  if (!return_prices) {
    result <- diff(log(prices))[-1]
  } else {
    result <- prices
  }
  
  # Filter by indices/columns if specified
  if (!is.null(indices)) {
    if (!all(indices %in% colnames(result))) {
      stop("Some requested indices/columns not available in dataset. ",
           "Available columns: ", paste(colnames(result), collapse = ", "))
    }
    result <- result[, indices]
  }
  
  # Filter by date if specified
  result <- window(result, 
                  start = as.Date(start_date),
                  end = as.Date(end_date))
  
  return(result)
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
#' \dontrun{
#' # Using built-in dataset
#' data <- get_stock_data(data_source = "builtin")
#' processed_data <- process_returns(data)
#' head(processed_data)
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
