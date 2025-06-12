#' Network Visualization Functions
#'
#' @name visualization
#' @description Functions for visualizing network analysis results
#'
#' @import ggplot2
#' @import viridis
#' @import igraph
#' @importFrom stats runif
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
  # Input validation
  if (!is.list(network) || !all(c("network_signed", "network_abs") %in% names(network))) {
    stop("Invalid network object")
  }
  
  g_signed <- network$network_signed
  g_abs <- network$network_abs
  
  # Check if network has vertices
  if (vcount(g_signed) == 0) {
    stop("Network has no vertices")
  }
  
  # Check if network has edges
  if (ecount(g_signed) == 0) {
    # Create a simple node-only plot if no edges
    nodes_df <- data.frame(
      name = V(g_signed)$name,
      x = runif(vcount(g_signed)),
      y = runif(vcount(g_signed)),
      size = ifelse(is.null(V(g_signed)$size), 1, abs(V(g_signed)$size)),
      market_type = ifelse(is.null(V(g_signed)$market_type), "unknown", as.character(V(g_signed)$market_type))
    )
    
    p <- ggplot(nodes_df, aes(x = x, y = y)) +
      geom_point(aes(size = size, color = market_type), alpha = 0.7) +
      geom_text(aes(label = name), vjust = -1.5, size = 3) +
      scale_color_brewer(palette = "Set1") +
      scale_size_continuous(range = c(3, 8)) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
      ) +
      labs(title = paste("Network Structure - Scale", scale, "Tau", tau, "(No Edges)"),
           color = "Market Type",
           size = "Node Size") +
      coord_fixed()
    
    return(p)
  }
  
  # Calculate layout for nodes with edges
  tryCatch({
    layout_coords <- layout_with_fr(g_abs)
  }, error = function(e) {
    # Fallback to simple layout
    layout_coords <- matrix(runif(vcount(g_signed) * 2), ncol = 2)
  })
  
  # Ensure layout has correct dimensions
  if (nrow(layout_coords) != vcount(g_signed)) {
    layout_coords <- matrix(runif(vcount(g_signed) * 2), ncol = 2)
  }
  
  # Create nodes data frame
  nodes_df <- data.frame(
    name = V(g_signed)$name,
    x = layout_coords[, 1],
    y = layout_coords[, 2],
    size = ifelse(is.null(V(g_signed)$size), 1, abs(V(g_signed)$size)),
    market_type = ifelse(is.null(V(g_signed)$market_type), "unknown", as.character(V(g_signed)$market_type))
  )
  
  # Create edges data frame
  edge_list <- as_edgelist(g_signed, names = FALSE)
  edges_df <- data.frame(
    from_x = layout_coords[edge_list[, 1], 1],
    from_y = layout_coords[edge_list[, 1], 2],
    to_x = layout_coords[edge_list[, 2], 1],
    to_y = layout_coords[edge_list[, 2], 2],
    weight = E(g_signed)$weight,
    width = abs(E(g_signed)$weight)
  )
  
  # Create plot
  p <- ggplot() +
    # Add edges
    geom_segment(data = edges_df,
                aes(x = from_x, y = from_y,
                    xend = to_x, yend = to_y,
                    alpha = abs(weight),
                    linewidth = width,
                    color = weight)) +
    # Add nodes
    geom_point(data = nodes_df,
              aes(x = x, y = y,
                  size = size,
                  fill = market_type),
              shape = 21, color = "black", alpha = 0.7) +
    # Add labels
    geom_text(data = nodes_df,
             aes(x = x, y = y, label = name),
             size = 3, vjust = -1.5, hjust = 0.5) +
    # Scales
    scale_fill_brewer(palette = "Set1") +
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    scale_size_continuous(range = c(3, 8)) +
    scale_linewidth_continuous(range = c(0.2, 2)) +
    scale_alpha_continuous(range = c(0.3, 0.8)) +
    # Theme
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = paste("Network Structure - Scale", scale, "Tau", tau),
         fill = "Market Type",
         color = "Edge Weight",
         size = "Node Size",
         linewidth = "Edge Width",
         alpha = "Edge Strength") +
    coord_fixed()
  
  return(p)
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
  # Input validation
  if (!is.matrix(qte_matrix) && !is.data.frame(qte_matrix)) {
    stop("Input must be a matrix or data frame")
  }
  
  # Convert to data frame for plotting
  df <- reshape2::melt(as.matrix(qte_matrix))
  colnames(df) <- c("Quantile", "Scale", "QTE")
  
  # Create plot
  ggplot(df, aes(x = Scale, y = Quantile, fill = QTE)) +
    geom_tile() +
    scale_fill_viridis_c() +
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
  # Input validation
  if (!is.matrix(rolling_qte) && !is.vector(rolling_qte)) {
    stop("rolling_qte must be a matrix or vector")
  }
  
  # Convert to data frame
  if (is.vector(rolling_qte)) {
    df <- data.frame(
      Time = 1:length(rolling_qte),
      QTE = rolling_qte,
      Series = "QTE"
    )
  } else {
    df <- data.frame(
      Time = 1:nrow(rolling_qte),
      rolling_qte
    )
    df_long <- reshape2::melt(df, id.vars = "Time")
  }
  
  # Create plot
  if (is.vector(rolling_qte)) {
    p <- ggplot(df, aes(x = Time, y = QTE)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Rolling QTE",
           x = "Time",
           y = "QTE Value")
  } else {
    p <- ggplot(df_long, aes(x = Time, y = value, color = variable)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Rolling Multiscale QTE",
           x = "Time",
           y = "QTE Value",
           color = "Scale")
  }
  
  p + theme(plot.title = element_text(hjust = 0.5))
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
  # Calculate network metrics
  tryCatch({
    metrics <- calculate_network_metrics(network)
  }, error = function(e) {
    stop("Failed to calculate network metrics: ", e$message)
  })
  
  # Prepare data for plotting
  if (is.null(metrics$degree) || length(metrics$degree) == 0) {
    stop("No degree metrics available")
  }
  
  strength_df <- data.frame(
    Market = names(metrics$degree),
    Internal_Strength = as.numeric(metrics$degree),
    Clustering = ifelse(is.null(metrics$transitivity) || is.na(metrics$transitivity), 
                       0, metrics$transitivity)
  )
  
  # Remove any rows with missing data
  strength_df <- strength_df[complete.cases(strength_df), ]
  
  if (nrow(strength_df) == 0) {
    stop("No valid data for market strength plot")
  }
  
  # Create plot
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

#' Plot Spillover Heatmap
#'
#' @param spillover_matrix Matrix of spillover values
#' @param title Plot title
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' spillover_plot <- plot_spillover_heatmap(spillover_matrix)
#' }
plot_spillover_heatmap <- function(spillover_matrix, title = "Spillover Analysis") {
  # Input validation
  if (!is.matrix(spillover_matrix)) {
    stop("Input must be a matrix")
  }
  
  # Convert to data frame
  df <- reshape2::melt(spillover_matrix)
  colnames(df) <- c("From", "To", "Spillover")
  
  # Create plot
  ggplot(df, aes(x = From, y = To, fill = Spillover)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = title,
      x = "From Market",
      y = "To Market",
      fill = "Spillover\nIntensity"
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
  tryCatch({
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
    cat("Plot saved to:", filename, "\n")
  }, error = function(e) {
    stop("Failed to save plot: ", e$message)
  })
  
  invisible(NULL)
}

# Register all global variables to avoid R CMD check notes
utils::globalVariables(c(
  "from_x", "from_y", "to_x", "to_y", "width", "weight", "name", "size", "market_type",
  "Scale", "Quantile", "QTE", "x", "y", "fill",
  "Time", "value", "variable",
  "Market", "Internal_Strength", "Clustering",
  "From", "To", "Spillover"
))
