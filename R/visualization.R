#' Network Visualization Functions
#'
#' @name visualization
#' @description Functions for visualizing network analysis results
#'
#' @import ggplot2
#' @import viridis
#' @import igraph
NULL

#' Plot Enhanced Network
#'
#' @param network Network object
#' @param scale Scale level
#' @param tau Quantile level
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' net <- create_qte_network(returns, wave_decomp, scale = 1)
#' plot_enhanced_network(net, scale = 1, tau = 0.5)
#' }
plot_enhanced_network <- function(network, scale, tau) {
  # Register global variables to avoid R CMD check notes
  from <- to <- width <- weight <- name <- size <- market_type <- NULL

  g_signed <- network$network_signed
  g_layout <- g_signed
  E(g_layout)$weight <- abs(E(g_signed)$weight)

  layout <- layout_with_fr(g_layout) * 0.7

  # Create data frames for plotting
  edges_df <- data.frame(
    from = ends(g_signed, E(g_signed))[,1],
    to = ends(g_signed, E(g_signed))[,2],
    weight = E(g_signed)$weight,
    width = E(g_signed)$width
  )

  nodes_df <- data.frame(
    name = V(g_signed)$name,
    size = V(g_signed)$size,
    market_type = V(g_signed)$market_type
  )

  ggplot() +
    geom_segment(data = edges_df,
                 aes(x = from, y = to,
                     alpha = abs(weight),
                     size = width,
                     color = weight)) +
    geom_point(data = nodes_df,
               aes(x = name, y = size,
                   size = size,
                   color = market_type)) +
    scale_color_brewer(palette = "Set1") +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_alpha_continuous(range = c(0.2, 0.8)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    labs(title = paste("Network Structure - Scale", scale, "Tau", tau),
         x = "Node",
         y = "Size",
         color = "Market Type",
         size = "Connection Strength",
         alpha = "Weight")
}

#' Plot QTE Heatmap
#'
#' @param qte_matrix QTE results matrix
#' @param title Plot title
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' qte_results <- calculate_multiscale_qte(wave_x, wave_y, tau_levels)
#' plot_qte_heatmap(qte_results)
#' }
plot_qte_heatmap <- function(qte_matrix, title = "Multiscale QTE Analysis") {
  # Register global variables
  Scale <- Quantile <- QTE <- NULL

  df <- reshape2::melt(qte_matrix)
  colnames(df) <- c("Quantile", "Scale", "QTE")

  ggplot(df, aes(x = Scale, y = Quantile, fill = QTE)) +
    geom_tile() +
    scale_fill_viridis() +
    theme_minimal() +
    labs(title = title,
         x = "Wavelet Scale",
         y = "Quantile Level",
         fill = "QTE Value") +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Plot Rolling QTE
#'
#' @param rolling_qte Rolling QTE results
#' @param window_size Window size used
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' rolling_results <- calculate_rolling_qte(x, y, window_size = 100)
#' plot_rolling_qte(rolling_results, window_size = 100)
#' }
plot_rolling_qte <- function(rolling_qte, window_size) {
  # Register global variables
  Time <- value <- variable <- NULL

  df <- data.frame(
    Time = 1:nrow(rolling_qte),
    rolling_qte
  )
  df_long <- reshape2::melt(df, id.vars = "Time")

  ggplot(df_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Rolling Multiscale QTE",
         x = "Time",
         y = "QTE Value",
         color = "Scale") +
    theme(plot.title = element_text(hjust = 0.5))
}

#' Plot Market Strength
#'
#' @param network Network object
#' @param scale Scale level
#' @param tau Quantile level
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' net <- create_qte_network(returns, wave_decomp, scale = 1)
#' plot_market_strength(net, scale = 1, tau = 0.5)
#' }
plot_market_strength <- function(network, scale, tau) {
  # Register global variables
  Market <- Internal_Strength <- Clustering <- NULL

  metrics <- calculate_network_metrics(network)

  strength_df <- data.frame(
    Market = names(metrics$degree),
    Internal_Strength = as.numeric(metrics$degree),
    Clustering = as.numeric(metrics$transitivity)
  )

  ggplot(strength_df, aes(x = Market, y = Internal_Strength, fill = Clustering)) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = paste("Market Strength - Scale", scale, "Tau", tau),
      x = "Market",
      y = "Internal Strength",
      fill = "Clustering\nCoefficient"
    )
}

#' Save Plot to File
#'
#' @param plot ggplot object
#' @param filename Output filename
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in dots per inch
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' p <- plot_qte_heatmap(qte_results)
#' save_plot(p, "heatmap.png")
#' }
save_plot <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  invisible(NULL)
}

# Register all global variables
utils::globalVariables(c(
  "from", "to", "width", "weight", "name", "size", "market_type",
  "Scale", "Quantile", "QTE",
  "Time", "value", "variable",
  "Market", "Internal_Strength", "Clustering"
))
