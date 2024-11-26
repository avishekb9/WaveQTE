# WaveQTE

Wavelet-based Quantile Transfer Entropy Analysis for Financial Time Series

## Overview

WaveQTE is an R package that implements Wavelet-based Quantile Transfer Entropy analysis for financial time series. It provides tools for analyzing market interconnectedness, financial contagion, and risk spillover effects across different time scales and market conditions.

## Features

- Data preparation and cleaning
- Wavelet decomposition analysis
- Quantile Transfer Entropy calculation
- Network analysis and visualization
- Financial contagion analysis
- Cross-market correlation analysis
- Bootstrap analysis for confidence intervals

## Installation

```R
# Install dependencies
install.packages(c("devtools", "quantmod", "waveslim", "quantreg", "igraph"))

# Install WaveQTE
devtools::install_github("username/WaveQTE")
```

## Quick Start

```R
library(WaveQTE)

# Get data
symbols <- c("AAPL", "MSFT", "JPM")
data <- get_stock_data(symbols, "2019-01-01", "2023-12-31")

# Perform wavelet decomposition
wave <- lapply(1:ncol(data), function(i) {
  wavelet_decompose(data[,i])
})

# Calculate QTE
qte_results <- calculate_multiscale_qte(
  wave[[1]], wave[[2]], 
  tau_levels = c(0.1, 0.5, 0.9)
)

# Create and visualize network
net <- create_qte_network(data, wave, scale = 1, tau = 0.5)
plot_enhanced_network(net, scale = 1, tau = 0.5)
```

## Documentation

See the package vignettes for detailed examples:

```R
vignette("basic-usage", package = "WaveQTE")
vignette("contagion-analysis", package = "WaveQTE")
vignette("advanced-features", package = "WaveQTE")
```

## Requirements

- R >= 3.5.0
- Required packages: See DESCRIPTION file

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use this package in your research, please cite:

```
@misc{WaveQTE,
  author = {Your Name},
  title = {WaveQTE: Wavelet-based Quantile Transfer Entropy Analysis},
  year = {2024},
  publisher = {GitHub},
  journal = {GitHub repository},
  url = {https://github.com/username/WaveQTE}
}
```

## Contact

- Create an issue on GitHub
- Email: your.email@example.com