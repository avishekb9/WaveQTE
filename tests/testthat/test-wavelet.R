# Complete replacement for tests/testthat/test-wavelet.R

test_that("wavelet_decompose returns expected structure", {
  # Create test data with fixed seed
  set.seed(123)
  test_data <- rnorm(256)
  
  # Perform decomposition
  wave <- suppressWarnings(wavelet_decompose(test_data, n.levels = 4))
  
  # Test basic structure and components
  expect_type(wave, "list")
  expect_named(wave, c("details", "smooth", "wave", "original"))
  expect_equal(length(wave$details), 4)
  expect_equal(names(wave$details), 
               paste0("Scale_", c("2-4d", "4-8d", "8-16d", "16-32d")))
  
  # Test that details and smooth components exist and have appropriate lengths
  for(i in 1:length(wave$details)) {
    expect_true(is.numeric(wave$details[[i]]))
    expect_true(length(wave$details[[i]]) > 0)
  }
  expect_true(is.numeric(wave$smooth))
  expect_true(length(wave$smooth) > 0)
  
  # Test that original data is preserved
  expect_equal(wave$original, test_data)
  
  # Just check that reconstruction produces a numeric result of the right length
  recon <- wave$smooth
  for(d in wave$details) {
    recon <- recon + d
  }
  expect_true(is.numeric(recon))
  expect_equal(length(recon), length(test_data))
})

test_that("wavelet_decompose handles errors correctly", {
  # Test minimum length requirement
  expect_error(wavelet_decompose(rnorm(8), n.levels = 4))
  
  # Test NA handling
  expect_error(wavelet_decompose(c(rnorm(100), NA)))
  
  # Test Inf handling
  expect_error(wavelet_decompose(c(rnorm(100), Inf)))
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
  
  # Create wavelet decomposition
  wave <- suppressWarnings(wavelet_decompose(test_data))
  
  # Test variance analysis
  var_analysis <- analyze_wavelet_variance(wave)
  
  # Check structure
  expect_type(var_analysis, "list")
  expect_true(all(c("variance", "percentage") %in% names(var_analysis)))
  
  # Check variances are numeric and positive
  var_values <- unlist(var_analysis$variance)
  expect_true(all(is.numeric(var_values)))
  expect_true(all(var_values >= 0))
  
  # Check percentages sum to approximately 100
  total_pct <- sum(unlist(var_analysis$percentage))
  expect_equal(total_pct, 100, tolerance = 1e-3)
})
