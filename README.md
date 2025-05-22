# WaveQTE

Wavelet-based Quantile Transfer Entropy Analysis for Financial Time Series

[![R](https://img.shields.io/badge/R-%3E%3D3.5.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

WaveQTE is an R package that implements Wavelet-based Quantile Transfer Entropy analysis for financial time series. It provides comprehensive tools for analyzing market interconnectedness, financial contagion, and risk spillover effects across different time scales and market conditions.

## Key Features

- **Multi-source Data Support**: Built-in financial dataset, Yahoo Finance integration, and custom data import
- **Wavelet Decomposition**: MODWT-based analysis across multiple time scales
- **Quantile Transfer Entropy**: Robust information flow measurement across quantiles
- **Network Analysis**: QTE-based network construction and visualization
- **Financial Contagion**: Crisis period analysis and cross-market spillover detection
- **Comprehensive Visualization**: Heatmaps, network plots, and time series analysis

## Installation

```r
# Install dependencies
install.packages(c("devtools", "quantmod", "waveslim", "quantreg", "igraph", 
                   "ggplot2", "reshape2", "viridis", "xts", "zoo", "moments", "tseries"))

# Install WaveQTE from GitHub
devtools::install_github("avishekb9/WaveQTE")


```

# Quick Start
## Basic Workflow
```r
library(WaveQTE)
```

## Method 1: Use built-in financial dataset (recommended for testing)
```r
data <- get_stock_data(data_source = "builtin", 
                       indices = c("GSPC", "N225", "FTSE"))
head(data)
```
## Method 2: Fetch live data from Yahoo Finance
```r
data <- get_stock_data(c("AAPL", "MSFT", "JPM"), start_date = "2020-01-01", end_date = "2023-12-31")
```
## Process the data
```r
processed_data <- process_returns(data)
summary_stats <- calculate_summary_stats(processed_data)
```
## Wavelet Analysis
```r
# Perform wavelet decomposition
wave_decomp <- lapply(1:ncol(processed_data), function(i) {
  wavelet_decompose(as.numeric(processed_data[1:256, i]), n.levels = 4)
})

# Analyze variance contributions
var_analysis <- lapply(wave_decomp, analyze_wavelet_variance)
print_wavelet_summary(wave_decomp[[1]])
```
## Quantile Transfer Entropy
```r
# Calculate multiscale QTE
tau_levels <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qte_results <- calculate_multiscale_qte(
  wave_decomp[[1]], 
  wave_decomp[[2]], 
  tau_levels
)

# Visualize results
plot_qte_heatmap(qte_results, title = "Multiscale QTE Analysis")
```
## Network Analysis
```r
# Create QTE-based network
network <- create_qte_network(processed_data[1:256, ], 
                             wave_decomp, 
                             scale = 1, 
                             tau = 0.5)

# Calculate network metrics
metrics <- calculate_network_metrics(network)
print(paste("Network density:", round(metrics$density, 3)))

# Visualize network
plot_enhanced_network(network, scale = 1, tau = 0.5)
```
## Financial Contagion Analysis
```r
# Analyze contagion between market groups
dm_data <- get_stock_data(data_source = "builtin", indices = c("GSPC", "N225", "FTSE"))
em_data <- get_stock_data(data_source = "builtin", indices = c("SSE", "BSE", "HSI"))

# Calculate contagion measures
contagion_results <- calculate_contagion_index(dm_data[1:256, ], 
                                              em_data[1:256, ])

# Identify crisis periods
crisis_periods <- identify_crisis_periods(cbind(dm_data, em_data))
```
# Data Sources
## The package supports multiple data sources:

1. **Built-in Dataset**: 10 global market indices from 2015-2023
2. **Yahoo Finance**: Live data fetching via quantmod
3. **Custom Data**: Import your own CSV files or data frames
```r
# Built-in data (recommended for examples)
data1 <- get_stock_data(data_source = "builtin")

# Yahoo Finance data
data2 <- get_stock_data(c("AAPL", "GOOGL"), "2020-01-01", "2023-12-31")

# Custom CSV file
data3 <- get_stock_data(data_source = "file", file_path = "my_data.csv")

# Custom data frame
data4 <- get_stock_data(data_source = "custom", custom_data = my_df)
```
# Documentation
**Vignettes**: Comprehensive tutorials are available:
```r
# Basic package usage
vignette("basic-usage", package = "WaveQTE")

# Financial contagion analysis
vignette("contagion-analysis", package = "WaveQTE")

# Advanced features and customization
vignette("advanced-features", package = "WaveQTE")
```
## Function Documentation
```r
# Core functions
?wavelet_decompose
?calculate_qte
?create_qte_network
?plot_enhanced_network

# Data functions
?get_stock_data
?process_returns
?calculate_summary_stats

# Contagion analysis
?calculate_contagion_index
?identify_crisis_periods
```
## Package Structure
```r
WaveQTE/
├── R/
│   ├── data_preparation.R      # Data import and processing
│   ├── wavelet_decomposition.R # Wavelet analysis functions
│   ├── quantile_transfer_entropy.R # QTE calculations
│   ├── network_analysis.R      # Network construction
│   ├── contagion_analysis.R    # Financial contagion tools
│   ├── visualization.R         # Plotting functions
│   └── utils.R                # Utility functions
├── man/                       # Documentation files
├── tests/                     # Unit tests
├── vignettes/                 # Tutorials and examples
└── inst/                      # Additional package files
```
## Example Applications

**Market Integration Analysis**: Measure information flow between financial markets
**Crisis Contagion**: Analyze how financial crises spread across markets
**Risk Management**: Identify spillover patterns for portfolio optimization
**Regulatory Policy**: Understand systemic risk and market interconnectedness
**Academic Research**: Empirical finance and econometrics studies

## Requirements
R >= 3.5.0
Dependencies: quantmod, waveslim, quantreg, igraph, ggplot2, and others (see DESCRIPTION)

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request. See CONTRIBUTING.md for guidelines.
## License
This project is licensed under the MIT License - see the LICENSE file for details.
## Citation
If you use this package in your research, please cite:
```r
@misc{WaveQTE2024,
  author = {Avishek Bhandari},
  title = {WaveQTE: Wavelet-based Quantile Transfer Entropy Analysis},
  year = {2024},
  publisher = {GitHub},
  journal = {GitHub repository},
  url = {https://github.com/avishekb9/WaveQTE}
}
```
## Contact
**Email**: bavisek@gmail.com

## Acknowledgments

1. Built with R and the excellent R ecosystem
2. Wavelet analysis powered by the waveslim package
3. Network analysis using igraph
4. Visualization with ggplot2


