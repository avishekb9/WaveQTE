#' Financial Contagion Analysis Functions
#' @name contagion_analysis
#' @description Functions for analyzing financial contagion and market spillovers
NULL

#' Calculate Contagion Index
#'
#' @param returns_dm Matrix of developed market returns
#' @param returns_em Matrix of emerging market returns
#' @param tau Quantile level (default: 0.05)
#' @param n.levels Number of wavelet levels (default: 4)
#' @return Array containing contagion measures
#' @importFrom stats window
#' @export
#' @examples
#' \donttest{
#' dm_returns <- get_stock_data(c("SPY", "EWU"))
#' em_returns <- get_stock_data(c("EWZ", "FXI"))
#' contagion <- calculate_contagion_index(dm_returns, em_returns)
#' }
calculate_contagion_index <- function(returns_dm, returns_em, tau = 0.05, n.levels = 4) {
  if (nrow(returns_dm) != nrow(returns_em)) {
    stop("Return series must have the same length")
  }

  n_dm <- ncol(returns_dm)
  n_em <- ncol(returns_em)

  contagion_matrix <- array(0, dim = c(n_em, n_dm, n.levels))

  for(j in 1:n.levels) {
    for(i in 1:n_em) {
      for(k in 1:n_dm) {
        wave_em <- wavelet_decompose(returns_em[,i], n.levels)
        wave_dm <- wavelet_decompose(returns_dm[,k], n.levels)

        contagion_matrix[i,k,j] <- calculate_qte(
          wave_em$details[[j]],
          wave_dm$details[[j]],
          tau = tau
        )
      }
    }
  }

  return(contagion_matrix)
}

#' Calculate Dynamic Contagion Index
#'
#' @param returns_dm Matrix of developed market returns
#' @param returns_em Matrix of emerging market returns
#' @param window_size Rolling window size (default: 252)
#' @param tau Quantile level (default: 0.05)
#' @return Matrix of dynamic contagion indices
#' @export
#' @examples
#' \donttest{
#' dm_returns <- get_stock_data(c("SPY", "EWU"))
#' em_returns <- get_stock_data(c("EWZ", "FXI"))
#' dci <- calculate_dynamic_contagion(dm_returns, em_returns)
#' }
calculate_dynamic_contagion <- function(returns_dm, returns_em, window_size = 252, tau = 0.05) {
  if (nrow(returns_dm) != nrow(returns_em)) {
    stop("Return series must have the same length")
  }

  n <- nrow(returns_dm)
  n_rolls <- n - window_size + 1

  dci <- matrix(0, n_rolls, 4)  # 4 scales
  colnames(dci) <- paste0("Scale_", 1:4)

  for(t in 1:n_rolls) {
    dm_window <- returns_dm[t:(t+window_size-1),]
    em_window <- returns_em[t:(t+window_size-1),]

    ci <- calculate_contagion_index(dm_window, em_window, tau)
    dci[t,] <- apply(ci, 3, mean)
  }

  return(dci)
}

#' Calculate Cross-Market Correlation
#'
#' @param returns_dm Developed market returns
#' @param returns_em Emerging market returns
#' @param window_size Rolling window size (default: 252)
#' @return Array of rolling correlations
#' @importFrom stats cor
#' @export
#' @examples
#' \donttest{
#' dm_returns <- get_stock_data(c("SPY", "EWU"))
#' em_returns <- get_stock_data(c("EWZ", "FXI"))
#' correlations <- calculate_cross_market_correlation(dm_returns, em_returns)
#' }
calculate_cross_market_correlation <- function(returns_dm, returns_em, window_size = 252) {
  if (nrow(returns_dm) != nrow(returns_em)) {
    stop("Return series must have the same length")
  }

  all_returns <- cbind(returns_dm, returns_em)
  n <- nrow(all_returns)
  n_rolls <- n - window_size + 1
  n_total <- ncol(all_returns)

  roll_cor <- array(0, dim = c(n_total, n_total, n_rolls))
  dimnames(roll_cor) <- list(
    colnames(all_returns),
    colnames(all_returns),
    NULL
  )

  for(t in 1:n_rolls) {
    window_data <- all_returns[t:(t+window_size-1),]
    roll_cor[,,t] <- cor(window_data)
  }

  return(roll_cor)
}

#' Identify Crisis Periods
#'
#' @param returns Return data matrix
#' @param threshold Threshold for crisis identification (default: -2)
#' @return Logical vector indicating crisis periods
#' @importFrom stats sd
#' @export
#' @examples
#' \donttest{
#' data <- get_stock_data(c("SPY", "EWZ"))
#' crisis_periods <- identify_crisis_periods(data)
#' }
identify_crisis_periods <- function(returns, threshold = -2) {
  # Calculate standardized returns
  std_returns <- scale(returns)

  # Identify crisis periods (extreme negative returns)
  crisis_periods <- apply(std_returns, 1, function(x) any(x < threshold))

  return(crisis_periods)
}

#' Analyze Crisis vs Non-Crisis Periods
#'
#' @param returns_dm Developed market returns
#' @param returns_em Emerging market returns
#' @param crisis_periods Logical vector indicating crisis periods
#' @return List containing contagion measures for crisis and normal periods
#' @export
#' @examples
#' \donttest{
#' dm_returns <- get_stock_data(c("SPY", "EWU"))
#' em_returns <- get_stock_data(c("EWZ", "FXI"))
#' crisis_periods <- identify_crisis_periods(cbind(dm_returns, em_returns))
#' analysis <- crisis_analysis(dm_returns, em_returns, crisis_periods)
#' }
crisis_analysis <- function(returns_dm, returns_em, crisis_periods) {
  if (nrow(returns_dm) != nrow(returns_em) ||
      length(crisis_periods) != nrow(returns_dm)) {
    stop("Input dimensions must match")
  }

  crisis_dm <- returns_dm[crisis_periods,]
  crisis_em <- returns_em[crisis_periods,]
  normal_dm <- returns_dm[!crisis_periods,]
  normal_em <- returns_em[!crisis_periods,]

  crisis_contagion <- calculate_contagion_index(crisis_dm, crisis_em)
  normal_contagion <- calculate_contagion_index(normal_dm, normal_em)

  return(list(
    crisis = crisis_contagion,
    normal = normal_contagion,
    crisis_dates = which(crisis_periods)
  ))
}
