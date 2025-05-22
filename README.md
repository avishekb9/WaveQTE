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
