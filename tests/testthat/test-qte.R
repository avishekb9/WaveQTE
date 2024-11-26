test_that("calculate_qte functions correctly", {
  set.seed(123)
  x <- rnorm(100)
  y <- 0.5 * x + rnorm(100, sd = 0.1)  # Create dependent series

  # Test basic functionality
  qte <- calculate_qte(x, y)
  expect_type(qte, "double")
  expect_false(is.na(qte))

  # Test different quantile levels
  qte_low <- calculate_qte(x, y, tau = 0.1)
  qte_high <- calculate_qte(x, y, tau = 0.9)
  expect_false(identical(qte_low, qte_high))

  # Test length checks
  expect_error(calculate_qte(x, y[1:50]))

  # Test short series handling with proper data
  x_short <- c(1, 2)
  y_short <- c(2, 4)
  expect_warning(result <- calculate_qte(x_short, y_short))
  expect_true(is.na(result))
})

test_that("calculate_multiscale_qte works correctly", {
  set.seed(123)
  x <- rnorm(256)
  y <- 0.5 * x + rnorm(256, sd = 0.1)

  wave_x <- wavelet_decompose(x)
  wave_y <- wavelet_decompose(y)

  tau_levels <- c(0.1, 0.5, 0.9)
  qte_results <- calculate_multiscale_qte(wave_x, wave_y, tau_levels)

  # Check dimensions
  expect_equal(dim(qte_results), c(length(tau_levels), length(wave_x$details)))

  # Check row and column names
  expect_equal(rownames(qte_results), paste0("tau=", tau_levels))
  expect_equal(colnames(qte_results), names(wave_x$details))

  # Check values are finite
  expect_true(all(!is.infinite(qte_results)))
})

test_that("bootstrap_qte works correctly", {
  set.seed(123)
  x <- rnorm(100)
  y <- 0.5 * x + rnorm(100, sd = 0.1)

  # Test with default parameters
  boot_result <- bootstrap_qte(x, y, B = 100)

  # Check structure
  expect_type(boot_result, "list")
  expect_named(boot_result, c("boot_samples", "ci", "mean", "sd", "n_valid"))

  # Check bootstrap samples
  expect_length(boot_result$boot_samples, 100)

  # Check confidence intervals
  expect_length(boot_result$ci, 2)
  expect_true(boot_result$ci[1] < boot_result$ci[2])

  # Check additional statistics
  expect_type(boot_result$mean, "double")
  expect_type(boot_result$sd, "double")
  expect_type(boot_result$n_valid, "integer")

  # Test error handling
  expect_error(bootstrap_qte(x, y[1:50]))
})
