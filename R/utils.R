#' Utility Functions
#' @name utils
#' @description Helper functions for the package
NULL

#' Save Analysis Results
#'
#' @param results List of analysis results
#' @param file_path File path for saving
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' save_results(list(qte = qte_results), "analysis_results.RData")
#' }
save_results <- function(results, file_path) {
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path), recursive = TRUE)
  }

  save(results, file = file_path)
  invisible(NULL)
}

#' Load Analysis Results
#'
#' @param file_path Path to saved results
#' @return List of analysis results
#' @export
#' @examples
#' \dontrun{
#' results <- load_results("analysis_results.RData")
#' }
load_results <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Results file not found")
  }

  results <- NULL
  load(file_path, envir = environment())
  results
}

#' Print Summary Statistics
#'
#' @param stats Summary statistics object
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' stats <- calculate_summary_stats(returns)
#' print_summary_stats(stats)
#' }
print_summary_stats <- function(stats) {
  cat("\nBasic Statistics:\n")
  print(round(stats$basic, 4))

  cat("\nRisk Measures:\n")
  print(round(stats$risk, 4))

  cat("\nCorrelation Matrix:\n")
  print(round(stats$correlation, 4))

  invisible(NULL)
}

#' Check Package Dependencies
#'
#' @return Character vector of missing packages
#' @export
#' @examples
#' missing_deps <- check_dependencies()
check_dependencies <- function() {
  required_packages <- c(
    "quantmod", "waveslim", "quantreg", "igraph",
    "ggplot2", "reshape2", "viridis", "xts", "zoo",
    "moments", "tseries"
  )

  missing_packages <- required_packages[
    !sapply(required_packages, requireNamespace, quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    warning("Missing required packages: ",
            paste(missing_packages, collapse = ", "))
  }

  invisible(missing_packages)
}
