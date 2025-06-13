#' Quantile Transfer Entropy Functions
#'
#' @name quantile_transfer_entropy
#' @description Functions for calculating Quantile Transfer Entropy
#' @importFrom stats quantile residuals sd
#' @importFrom quantreg rq
NULL

#' Calculate Quantile Transfer Entropy
#'
#' @param x Source time series
#' @param y Target time series
#' @param tau Quantile level (default: 0.5)
#' @param k Lag order (default: 1)
#' @return Numeric QTE value or NA if calculation fails
#' @export
#' @examples
#' \donttest{
#' x <- rnorm(100)
#' y <- 0.5 * x + rnorm(100, sd = 0.1)
#' qte <- calculate_qte(x, y, tau = 0.5)
#' }
calculate_qte <- function(x, y, tau = 0.5, k = 1) {
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Input series must be numeric")
  }

  if (length(x) != length(y)) {
    stop("Input series must have the same length")
  }

  if (tau <= 0 || tau >= 1) {
    stop("Quantile level tau must be between 0 and 1")
  }

  n <- length(x)
  if (n <= k) {
    warning("Series too short for specified lag")
    return(NA_real_)
  }

  # Create lagged variables
  x_t <- x[(k+1):n]
  x_lag <- x[1:(n-k)]
  y_lag <- y[1:(n-k)]

  # Check for sufficient observations
  if (length(x_t) < 3) {
    warning("Series too short for meaningful QTE calculation")
    return(NA_real_)
  }

  tryCatch({
    # Fit quantile regression models with more robust error handling
    model1 <- suppressWarnings(rq(x_t ~ x_lag, tau = tau))
    model2 <- suppressWarnings(rq(x_t ~ x_lag + y_lag, tau = tau))

    # Check if models were fit successfully
    if (is.null(model1) || is.null(model2)) {
      return(NA_real_)
    }

    # Get residuals
    resid1 <- residuals(model1)
    resid2 <- residuals(model2)

    # Check residuals
    if (any(is.na(resid1)) || any(is.na(resid2)) ||
        all(resid1 == 0) || all(resid2 == 0)) {
      return(NA_real_)
    }

    # Calculate QTE
    qte <- log(sum(abs(resid1))) - log(sum(abs(resid2)))

    # Sanity check on result
    if (!is.finite(qte)) {
      return(NA_real_)
    }

    return(qte)

  }, error = function(e) {
    warning("Error in QTE calculation: ", e$message)
    return(NA_real_)
  })
}

#' Calculate Multiscale QTE
#'
#' @param wave_x Wavelet decomposition for source series
#' @param wave_y Wavelet decomposition for target series
#' @param tau_levels Vector of quantile levels
#' @return Matrix of QTE values for each scale and quantile
#' @export
#' @examples
#' \donttest{
#' # Generate test data
#' x <- rnorm(256)
#' y <- 0.5 * x + rnorm(256, sd = 0.1)
#'
#' # Perform wavelet decomposition
#' wave_x <- wavelet_decompose(x)
#' wave_y <- wavelet_decompose(y)
#'
#' # Calculate multiscale QTE
#' qte_results <- calculate_multiscale_qte(wave_x, wave_y, c(0.1, 0.5, 0.9))
#' }
calculate_multiscale_qte <- function(wave_x, wave_y, tau_levels) {
  # Input validation
  if (!identical(names(wave_x$details), names(wave_y$details))) {
    stop("Wavelet decompositions must have matching scales")
  }

  if (any(tau_levels <= 0) || any(tau_levels >= 1)) {
    stop("All quantile levels must be between 0 and 1")
  }

  n_scales <- length(wave_x$details)

  # Initialize results matrix
  qte_results <- matrix(NA_real_,
                        nrow = length(tau_levels),
                        ncol = n_scales,
                        dimnames = list(
                          paste0("tau=", tau_levels),
                          names(wave_x$details)
                        ))

  # Calculate QTE for each scale and quantile level
  for(i in seq_along(tau_levels)) {
    for(j in 1:n_scales) {
      qte_results[i,j] <- calculate_qte(
        wave_x$details[[j]],
        wave_y$details[[j]],
        tau = tau_levels[i]
      )
    }
  }

  return(qte_results)
}

#' Bootstrap QTE Confidence Intervals
#'
#' @param x Source time series
#' @param y Target time series
#' @param tau Quantile level
#' @param B Number of bootstrap replications
#' @param block_size Block size for block bootstrap
#' @param conf_level Confidence level for intervals
#' @return List containing bootstrap samples and confidence intervals
#' @export
#' @examples
#' \donttest{
#' x <- rnorm(100)
#' y <- 0.5 * x + rnorm(100, sd = 0.1)
#' boot_results <- bootstrap_qte(x, y, tau = 0.5, B = 1000)
#' }
bootstrap_qte <- function(x, y, tau = 0.5, B = 1000, block_size = 20,
                          conf_level = 0.95) {
  # Input validation
  if (length(x) != length(y)) {
    stop("Input series must have the same length")
  }

  if (block_size >= length(x)) {
    stop("Block size must be smaller than series length")
  }

  if (B < 100) {
    warning("Number of bootstrap replications might be too small")
  }

  n <- length(x)
  qte_boot <- numeric(B)
  alpha <- 1 - conf_level

  # Perform block bootstrap
  for(b in 1:B) {
    # Generate blocks
    n_blocks <- ceiling(n/block_size)
    blocks <- sample(1:(n-block_size+1), n_blocks, replace = TRUE)

    # Create bootstrap sample
    indices <- unlist(lapply(blocks, function(i) i:(i+block_size-1)))
    indices <- indices[1:n]  # Trim to original length

    # Calculate QTE for bootstrap sample
    qte_boot[b] <- calculate_qte(x[indices], y[indices], tau = tau)
  }

  # Remove NA values if any
  qte_boot <- qte_boot[!is.na(qte_boot)]

  if (length(qte_boot) < B * 0.5) {
    warning("More than 50% of bootstrap replications failed")
  }

  # Calculate confidence intervals
  ci <- quantile(qte_boot, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)

  # Add names to confidence intervals
  names(ci) <- paste0(100 * c(alpha/2, 1-alpha/2), "%")

  return(list(
    boot_samples = qte_boot,
    ci = ci,
    mean = mean(qte_boot, na.rm = TRUE),
    sd = sd(qte_boot, na.rm = TRUE),
    n_valid = sum(!is.na(qte_boot))
  ))
}

#' Rolling Window QTE Analysis
#'
#' @param x Source time series
#' @param y Target time series
#' @param window_size Window size for rolling analysis
#' @param tau Quantile level
#' @param step_size Step size for rolling window (default: 1)
#' @return Vector of QTE values for each window
#' @export
#' @examples
#' \donttest{
#' x <- rnorm(500)
#' y <- 0.5 * x + rnorm(500, sd = 0.1)
#' rolling_qte <- calculate_rolling_qte(x, y, window_size = 100)
#' }
calculate_rolling_qte <- function(x, y, window_size, tau = 0.5, step_size = 1) {
  # Input validation
  if (length(x) != length(y)) {
    stop("Input series must have the same length")
  }

  if (window_size >= length(x)) {
    stop("Window size must be smaller than series length")
  }

  n <- length(x)
  n_windows <- floor((n - window_size)/step_size) + 1
  qte_values <- numeric(n_windows)

  for(i in 1:n_windows) {
    idx_start <- (i-1)*step_size + 1
    idx_end <- idx_start + window_size - 1

    qte_values[i] <- calculate_qte(
      x[idx_start:idx_end],
      y[idx_start:idx_end],
      tau = tau
    )
  }

  return(qte_values)
}

#' Test QTE Significance
#'
#' @param x Source time series
#' @param y Target time series
#' @param tau Quantile level
#' @param n_permutations Number of permutations for test
#' @return List containing test statistics and p-value
#' @export
#' @examples
#' \donttest{
#' x <- rnorm(100)
#' y <- 0.5 * x + rnorm(100, sd = 0.1)
#' test_result <- test_qte_significance(x, y)
#' }
test_qte_significance <- function(x, y, tau = 0.5, n_permutations = 1000) {
  # Calculate observed QTE
  observed_qte <- calculate_qte(x, y, tau = tau)

  # Permutation test
  perm_qte <- numeric(n_permutations)
  n <- length(x)

  for(i in 1:n_permutations) {
    # Permute y series
    y_perm <- y[sample(n, n)]
    perm_qte[i] <- calculate_qte(x, y_perm, tau = tau)
  }

  # Calculate p-value
  p_value <- mean(abs(perm_qte) >= abs(observed_qte), na.rm = TRUE)

  return(list(
    observed = observed_qte,
    permutation_values = perm_qte,
    p_value = p_value,
    significant = p_value < 0.05
  ))
}

# Register global variables to avoid R CMD check notes
utils::globalVariables(c("QTE", "Quantile"))
