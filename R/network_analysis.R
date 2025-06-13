#' Network Analysis Functions
#' @name network_analysis
#' @description Functions for network construction and analysis
NULL

#' Create QTE Network
#'
#' @param returns Return data matrix
#' @param wave_decomp List of wavelet decompositions
#' @param scale Scale level to analyze
#' @param tau Quantile level (default: 0.5)
#' @return List containing network objects and QTE matrix
#' @import igraph
#' @export
#' @examples
#' \donttest{
#' data <- matrix(rnorm(300), ncol = 3)
#' wave <- lapply(1:ncol(data), function(i) wavelet_decompose(data[,i]))
#' net <- create_qte_network(data, wave, scale = 1)
#' }
create_qte_network <- function(returns, wave_decomp, scale, tau = 0.5) {
  n_assets <- ncol(returns)
  if (length(wave_decomp) != n_assets) {
    stop("Number of wavelet decompositions must match number of assets")
  }

  # Create QTE matrix
  qte_matrix <- matrix(0, n_assets, n_assets)
  colnames(qte_matrix) <- rownames(qte_matrix) <- colnames(returns)

  for(i in 1:n_assets) {
    for(j in 1:n_assets) {
      if(i != j) {
        qte_matrix[i,j] <- calculate_qte(
          wave_decomp[[i]]$details[[scale]],
          wave_decomp[[j]]$details[[scale]],
          tau = tau
        )
      }
    }
  }

  # Create networks
  g_abs <- graph_from_adjacency_matrix(
    abs(qte_matrix),
    mode = "directed",
    weighted = TRUE,
    diag = FALSE
  )

  g_signed <- graph_from_adjacency_matrix(
    qte_matrix,
    mode = "directed",
    weighted = TRUE,
    diag = FALSE
  )

  # Add attributes
  V(g_signed)$name <- colnames(returns)
  V(g_signed)$size <- strength(g_abs, mode = "total")
  E(g_signed)$width <- abs(E(g_signed)$weight)

  list(
    network_abs = g_abs,
    network_signed = g_signed,
    qte_matrix = qte_matrix
  )
}

#' Calculate Network Metrics
#'
#' @param network Network object from create_qte_network
#' @return List of network metrics
#' @export
#' @examples
#' \donttest{
#' data <- matrix(rnorm(300), ncol = 3)
#' wave <- lapply(1:ncol(data), function(i) wavelet_decompose(data[,i]))
#' net <- create_qte_network(data, wave, scale = 1)
#' metrics <- calculate_network_metrics(net)
#' }
calculate_network_metrics <- function(network) {
  g_abs <- network$network_abs
  g_signed <- network$network_signed

  # Use absolute weights for metrics
  E(g_abs)$weight <- abs(E(g_abs)$weight)

  metrics <- list(
    density = edge_density(g_abs),
    reciprocity = reciprocity(g_abs),
    transitivity = transitivity(g_abs, type = "global"),

    degree = degree(g_abs, mode = "total"),
    betweenness = betweenness(g_abs),
    closeness = closeness(g_abs),
    eigenvector = eigen_centrality(g_abs)$vector,

    positive_density = mean(E(g_signed)$weight > 0),
    negative_density = mean(E(g_signed)$weight < 0),

    communities = cluster_louvain(as_undirected(g_abs))
  )

  return(metrics)
}

#' Create Multiscale Networks
#'
#' @param returns Return data matrix
#' @param wave_decomp Wavelet decomposition list
#' @param params List with n_levels and tau_levels
#' @return List of networks and metrics
#' @export
#' @examples
#' \donttest{
#' data <- matrix(rnorm(300), ncol = 3)
#' wave <- lapply(1:ncol(data), function(i) wavelet_decompose(data[,i]))
#' params <- list(n_levels = 4, tau_levels = c(0.1, 0.5, 0.9))
#' networks <- create_multiscale_networks(data, wave, params)
#' }
create_multiscale_networks <- function(returns, wave_decomp, params) {
  networks <- list()
  metrics <- list()

  for(scale in 1:params$n_levels) {
    networks[[scale]] <- list()
    metrics[[scale]] <- list()

    for(tau in params$tau_levels) {
      net_result <- create_qte_network(
        returns,
        wave_decomp,
        scale,
        tau
      )

      networks[[scale]][[as.character(tau)]] <- net_result
      metrics[[scale]][[as.character(tau)]] <- calculate_network_metrics(net_result)
    }
  }

  return(list(
    networks = networks,
    metrics = metrics
  ))
}

# Register global variables to avoid R CMD check notes
utils::globalVariables(c("weight", "size", "market_type", "name"))
