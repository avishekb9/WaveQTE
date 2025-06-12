#' WaveQTE: Wavelet-based Quantile Transfer Entropy Analysis
#'
#' @description
#' Implements Wavelet-based Quantile Transfer Entropy analysis for financial time series.
#' Provides tools for analyzing market interconnectedness, financial contagion,
#' and risk spillover effects across different time scales and market conditions.
#'
#' @section Main Functions:
#' \itemize{
#'   \item Data Preparation:
#'     \code{\link{get_stock_data}},
#'     \code{\link{process_returns}},
#'     \code{\link{calculate_summary_stats}}
#'   \item Wavelet Analysis:
#'     \code{\link{wavelet_decompose}},
#'     \code{\link{safe_wavelet_decompose}},
#'     \code{\link{analyze_wavelet_variance}}
#'   \item Quantile Transfer Entropy:
#'     \code{\link{calculate_qte}},
#'     \code{\link{calculate_multiscale_qte}},
#'     \code{\link{bootstrap_qte}}
#'   \item Network Analysis:
#'     \code{\link{create_qte_network}},
#'     \code{\link{calculate_network_metrics}},
#'     \code{\link{create_multiscale_networks}}
#'   \item Visualization:
#'     \code{\link{plot_enhanced_network}},
#'     \code{\link{plot_qte_heatmap}},
#'     \code{\link{plot_market_strength}}
#' }
#'
#' @section Data Requirements:
#' The package works with financial time series data. The main input should be:
#' \itemize{
#'   \item Return series (preferably daily)
#'   \item Clean data without missing values
#'   \item Sufficient length for wavelet decomposition
#' }
#'
#' @section Usage Example:
#' \preformatted{
#' library(WaveQTE)
#'
#' # Get data
#' data <- get_stock_data(c("AAPL", "MSFT"), "2019-01-01", "2023-12-31")
#'
#' # Wavelet decomposition
#' wave <- lapply(1:ncol(data), function(i) wavelet_decompose(data[,i]))
#'
#' # Calculate QTE
#' qte <- calculate_multiscale_qte(wave[[1]], wave[[2]], c(0.1, 0.5, 0.9))
#'
#' # Create network
#' net <- create_qte_network(data, wave, scale = 1)
#'
#' # Visualize
#' plot_enhanced_network(net, scale = 1, tau = 0.5)
#' }
#'
#' @references
#' Add relevant papers and references here.
#'
#' @keywords internal
"_PACKAGE"
