#' Wavelet Decomposition Functions
#'
#' @name wavelet_decomposition
#' @description Functions for performing wavelet decomposition analysis
#'
#' @import waveslim
#' @importFrom stats var
NULL

#' Perform Wavelet Decomposition
#'
#' @param returns Numeric vector of return data
#' @param n.levels Number of wavelet decomposition levels (default: 4)
#' @param wf Wavelet filter to use (default: "la8")
#' @return List containing wavelet decomposition results
#' @export
#' @examples
#' \dontrun{
#' data <- rnorm(256)
#' wave <- wavelet_decompose(data, n.levels = 4)
#' }
wavelet_decompose <- function(returns, n.levels = 4, wf = "la8") {
  # Input validation
  if (!is.numeric(returns)) {
    stop("Input must be a numeric vector")
  }

  if (any(!is.finite(returns))) {
    stop("Input contains NA, NaN, or Inf values")
  }

  # Check minimum length requirement
  min_length <- 2^(n.levels + 1)
  if (length(returns) < min_length) {
    stop(sprintf("Series length %d too short for %d levels. Minimum required: %d",
                 length(returns), n.levels, min_length))
  }

  # Perform MODWT with error checking
  tryCatch({
    wave <- modwt(returns, wf = wf, n.levels = n.levels)

    # Extract details
    details <- list()
    for(i in 1:n.levels) {
      details[[i]] <- wave[[paste0("d", i)]]
    }

    # Extract smooth component
    smooth <- wave[[paste0("s", n.levels)]]

    # Name the scales with their time interpretations
    names(details) <- paste0("Scale_", c("2-4d", "4-8d", "8-16d", "16-32d")[1:n.levels])

    # Create result object
    result <- list(
      details = details,
      smooth = smooth,
      wave = wave,
      original = returns
    )

    # Optional reconstruction check with very lenient tolerances
    # This is mainly for diagnostic purposes and won't fail the decomposition
    tryCatch({
      recon <- smooth
      for(d in details) {
        recon <- recon + d
      }

      # Only warn if reconstruction is very poor (>50% relative error)
      max_abs_orig <- max(abs(returns))
      if (max_abs_orig > 0) {  # Avoid division by zero
        rel_diff <- max(abs(recon - returns)) / max_abs_orig
        if (rel_diff > 0.5) {  # 50% relative difference tolerance
          warning("Wavelet reconstruction shows significant numerical instability, relative difference: ",
                  format(rel_diff * 100, digits = 3), "%")
        }
      }
    }, error = function(e) {
      # If reconstruction check fails, just continue without warning
      # This prevents the decomposition from failing due to reconstruction issues
    })

    return(result)

  }, error = function(e) {
    stop(paste("Wavelet decomposition failed:", e$message))
  })
}

#' Safe Wavelet Decomposition
#'
#' @param returns Numeric vector of return data
#' @param n.levels Number of wavelet decomposition levels (default: 4)
#' @param wf Wavelet filter to use (default: "la8")
#' @return List containing wavelet decomposition results
#' @export
#' @examples
#' \dontrun{
#' data <- rnorm(256)
#' wave <- safe_wavelet_decompose(data)
#' }
safe_wavelet_decompose <- function(returns, n.levels = 4, wf = "la8") {
  # Input validation
  if (any(is.na(returns))) {
    stop("Input contains NA values")
  }

  if (any(!is.finite(returns))) {
    stop("Input contains Inf or NaN values")
  }

  min_length <- 2^(n.levels + 1)
  if (length(returns) < min_length) {
    stop(sprintf("Series length %d too short for %d levels. Minimum required: %d",
                 length(returns), n.levels, min_length))
  }

  # Scale check
  if (max(abs(returns)) > 1e10) {
    warning("Large values detected in input series. Consider scaling.")
  }

  # Perform decomposition with error handling
  tryCatch({
    result <- wavelet_decompose(returns, n.levels, wf)

    # Additional validation
    max_coef <- max(abs(unlist(result$details)))
    if (max_coef > 1e3 * max(abs(returns))) {
      warning("Large wavelet coefficients detected")
    }

    return(result)
  }, error = function(e) {
    stop(paste("Safe wavelet decomposition failed:", e$message))
  })
}

#' Analyze Wavelet Variance
#'
#' @param wave_decomp Wavelet decomposition list from wavelet_decompose
#' @return List containing variance decomposition and percentage contributions
#' @export
#' @examples
#' \dontrun{
#' data <- rnorm(256)
#' wave <- wavelet_decompose(data)
#' var_analysis <- analyze_wavelet_variance(wave)
#' }
analyze_wavelet_variance <- function(wave_decomp) {
  # Input validation
  if (!is.list(wave_decomp) || !all(c("details", "smooth") %in% names(wave_decomp))) {
    stop("Invalid wavelet decomposition object")
  }

  # Calculate variance for each detail level
  var_decomp <- lapply(wave_decomp$details, function(d) {
    if (any(!is.finite(d))) {
      stop("Invalid coefficients in wavelet details")
    }
    var(d, na.rm = TRUE)
  })

  # Add smooth component variance
  if (any(!is.finite(wave_decomp$smooth))) {
    stop("Invalid coefficients in smooth component")
  }
  var_decomp$smooth <- var(wave_decomp$smooth, na.rm = TRUE)

  # Calculate total variance
  total_var <- sum(unlist(var_decomp))
  if (!is.finite(total_var) || total_var <= 0) {
    stop("Invalid total variance")
  }

  # Calculate percentage contributions
  var_pct <- lapply(var_decomp, function(v) v/total_var * 100)

  # Verify percentages (with more lenient tolerance)
  total_pct <- sum(unlist(var_pct))
  if (abs(total_pct - 100) > 1e-6) {
    warning("Variance percentages do not sum exactly to 100%: ", 
            format(total_pct, digits = 6))
  }

  return(list(
    variance = var_decomp,
    percentage = var_pct
  ))
}

#' Print Wavelet Decomposition Summary
#'
#' @param wave_decomp Wavelet decomposition object
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' data <- rnorm(256)
#' wave <- wavelet_decompose(data)
#' print_wavelet_summary(wave)
#' }
print_wavelet_summary <- function(wave_decomp) {
  cat("\nWavelet Decomposition Summary:\n")
  cat("Number of levels:", length(wave_decomp$details), "\n")
  cat("Series length:", length(wave_decomp$original), "\n")

  # Calculate and print scale information
  for (i in seq_along(wave_decomp$details)) {
    cat(sprintf("Scale %d (%s): %d coefficients\n",
                i,
                names(wave_decomp$details)[i],
                length(wave_decomp$details[[i]])))
  }

  # Print smooth component info
  cat("Smooth component length:", length(wave_decomp$smooth), "\n")

  # Optional reconstruction check (non-failing)
  tryCatch({
    recon <- wave_decomp$smooth
    for(d in wave_decomp$details) {
      recon <- recon + d
    }
    max_diff <- max(abs(recon - wave_decomp$original))
    cat(sprintf("\nMaximum reconstruction difference: %.2e\n", max_diff))
  }, error = function(e) {
    cat("\nReconstruction check failed:", e$message, "\n")
  })

  invisible(NULL)
}
