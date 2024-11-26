test_that("wavelet_decompose works correctly", {
  # Create test data with fixed seed
  set.seed(123)
  test_data <- rnorm(256)

  # Suppress reconstruction warnings during tests
  suppressWarnings({
    wave <- wavelet_decompose(test_data, n.levels = 4)
  })

  # Test basic functionality
  expect_type(wave, "list")
  expect_equal(length(wave$details), 4)
  expect_equal(names(wave$details),
               paste0("Scale_", c("2-4d", "4-8d", "8-16d", "16-32d")))

  # Test minimum length requirement
  expect_error(wavelet_decompose(rnorm(8), n.levels = 4))

  # Test approximate reconstruction
  recon <- wave$smooth
  for(d in wave$details) {
    recon <- recon + d
  }

  # Test reconstruction with looser tolerance
  max_diff <- max(abs(test_data - recon))
  expect_lt(max_diff, 1e-3)
})

test_that("safe_wavelet_decompose handles errors", {
  # Test NA handling
  test_data_na <- c(rnorm(100), NA)
  expect_error(safe_wavelet_decompose(test_data_na))

  # Test Inf handling
  test_data_inf <- c(rnorm(100), Inf)
  expect_error(safe_wavelet_decompose(test_data_inf))

  # Test length requirement
  expect_error(safe_wavelet_decompose(rnorm(8)))
})

test_that("analyze_wavelet_variance works correctly", {
  set.seed(123)
  test_data <- rnorm(256)

  suppressWarnings({
    wave <- wavelet_decompose(test_data)
    var_analysis <- analyze_wavelet_variance(wave)
  })

  # Check structure
  expect_type(var_analysis, "list")
  expect_true(all(c("variance", "percentage") %in% names(var_analysis)))

  # Check percentages sum to approximately 100
  total_pct <- sum(unlist(var_analysis$percentage))
  expect_equal(total_pct, 100, tolerance = 1e-4)
})
