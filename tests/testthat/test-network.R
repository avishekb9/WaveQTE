test_that("create_qte_network works correctly", {
  set.seed(123)
  n_assets <- 3
  n_obs <- 256
  returns <- matrix(rnorm(n_obs * n_assets), ncol = n_assets)
  colnames(returns) <- c("A", "B", "C")

  wave_decomp <- lapply(1:n_assets, function(i) {
    wavelet_decompose(returns[,i])
  })

  # Test network creation
  net <- create_qte_network(returns, wave_decomp, scale = 1)

  # Check structure
  expect_type(net, "list")
  expect_named(net, c("network_abs", "network_signed", "qte_matrix"))

  # Check matrix dimensions
  expect_equal(dim(net$qte_matrix), c(n_assets, n_assets))

  # Check network properties
  expect_s3_class(net$network_signed, "igraph")
  expect_equal(vcount(net$network_signed), n_assets)

  # Test error handling
  expect_error(create_qte_network(returns[,-1], wave_decomp, scale = 1))
})

test_that("calculate_network_metrics gives valid results", {
  set.seed(123)
  n_assets <- 3
  qte_matrix <- matrix(runif(n_assets^2), n_assets, n_assets)
  diag(qte_matrix) <- 0
  colnames(qte_matrix) <- rownames(qte_matrix) <- letters[1:n_assets]

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

  network <- list(
    network_abs = g_abs,
    network_signed = g_signed,
    qte_matrix = qte_matrix
  )

  metrics <- calculate_network_metrics(network)

  # Check metrics structure
  expect_type(metrics, "list")
  expect_true(all(c("density", "reciprocity", "transitivity",
                    "degree", "betweenness", "closeness",
                    "eigenvector") %in% names(metrics)))

  # Check metric values
  expect_true(metrics$density >= 0 && metrics$density <= 1)
  expect_true(metrics$reciprocity >= 0 && metrics$reciprocity <= 1)
  expect_false(is.na(metrics$transitivity))
})

test_that("create_multiscale_networks handles all scales", {
  set.seed(123)
  n_assets <- 3
  n_obs <- 256
  returns <- matrix(rnorm(n_obs * n_assets), ncol = n_assets)
  colnames(returns) <- c("A", "B", "C")

  wave_decomp <- lapply(1:n_assets, function(i) {
    wavelet_decompose(returns[,i])
  })

  params <- list(
    n_levels = 4,
    tau_levels = c(0.1, 0.5, 0.9)
  )

  # Test multiscale network creation
  networks <- create_multiscale_networks(returns, wave_decomp, params)

  # Check structure
  expect_type(networks, "list")
  expect_named(networks, c("networks", "metrics"))
  expect_length(networks$networks, params$n_levels)

  # Check each scale and tau
  for(scale in 1:params$n_levels) {
    for(tau in as.character(params$tau_levels)) {
      net <- networks$networks[[scale]][[tau]]
      expect_false(is.null(net))
      expect_equal(dim(net$qte_matrix), c(n_assets, n_assets))

      metrics <- networks$metrics[[scale]][[tau]]
      expect_false(is.null(metrics))
      expect_true("density" %in% names(metrics))
    }
  }
})
