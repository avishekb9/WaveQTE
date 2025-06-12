# WaveQTE <img src="man/figures/logo.png" align="right" height="139" />

> **Wavelet-based Quantile Transfer Entropy Analysis for Financial Time Series**

*Advanced tools for analyzing market interconnectedness, financial contagion, and risk spillover effects across different time scales and market conditions.*

[![R](https://img.shields.io/badge/R-%3E%3D3.5.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/WaveQTE)](https://cran.r-project.org/package=WaveQTE)
[![Downloads](https://cranlogs.r-pkg.org/badges/WaveQTE)](https://cran.r-project.org/package=WaveQTE)

## 🚀 Why WaveQTE?

WaveQTE bridges the gap between theoretical finance and practical analysis by combining **wavelet decomposition** with **quantile transfer entropy** to reveal hidden patterns in financial markets. Whether you're a researcher, practitioner, or policymaker, WaveQTE provides the tools you need to understand:

- 📊 **Market Interconnectedness** - How markets influence each other across time scales
- 🌊 **Financial Contagion** - How crises spread between markets and regions  
- 📈 **Risk Spillovers** - Directional risk transmission patterns
- 🔗 **Network Dynamics** - Complex relationships in financial systems

## ✨ Key Features

<div class="feature-grid">

### 📁 **Multi-source Data Support**
- Built-in financial dataset with 10 global market indices
- Yahoo Finance integration for real-time data
- Custom data import capabilities
- Automated data quality checks

### 〰️ **Wavelet Decomposition** 
- MODWT-based analysis across multiple time scales
- Robust handling of non-stationary financial data
- Scale-specific analysis of market dynamics
- Advanced variance decomposition

### 📊 **Quantile Transfer Entropy**
- Information flow measurement across quantiles
- Tail-risk and extreme event analysis
- Directional spillover detection
- Bootstrap significance testing

### 🕸️ **Network Analysis**
- QTE-based network construction
- Comprehensive network metrics
- Community detection algorithms
- Multi-scale network visualization

### 🌊 **Financial Contagion**
- Crisis period identification
- Cross-market correlation analysis
- Dynamic contagion measurement
- Regional spillover patterns

### 📈 **Rich Visualizations**
- Interactive network plots
- Publication-ready heatmaps
- Time-varying analysis charts
- Customizable themes and layouts

</div>

## 📦 Installation

<div class="install-box">

```r
# Install from GitHub (recommended)
if (!require("devtools")) install.packages("devtools")
devtools::install_github("avishekb9/WaveQTE")

# Load the package
library(WaveQTE)
```

</div>

**Dependencies:** WaveQTE automatically installs required packages including `quantmod`, `waveslim`, `quantreg`, `igraph`, `ggplot2`, and others.

## 🚀 Quick Start

### 1️⃣ **Load Data**
```r
# Use built-in dataset (recommended for testing)
data <- get_stock_data(data_source = "builtin", 
                       indices = c("GSPC", "N225", "FTSE"))

# Or fetch live data from Yahoo Finance
# data <- get_stock_data(c("AAPL", "MSFT", "JPM"), 
#                        start_date = "2020-01-01", 
#                        end_date = "2023-12-31")
```

### 2️⃣ **Process Returns**
```r
processed_data <- process_returns(data)
summary_stats <- calculate_summary_stats(processed_data)
print_summary_stats(summary_stats)
```

### 3️⃣ **Wavelet Decomposition**
```r
# Decompose time series into multiple scales
wave_decomp <- lapply(1:ncol(processed_data), function(i) {
  wavelet_decompose(as.numeric(processed_data[1:256, i]), n.levels = 4)
})

# Analyze variance contributions across scales
var_analysis <- lapply(wave_decomp, analyze_wavelet_variance)
print_wavelet_summary(wave_decomp[[1]])
```

### 4️⃣ **Quantile Transfer Entropy**
```r
# Calculate multiscale QTE
tau_levels <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qte_results <- calculate_multiscale_qte(
  wave_decomp[[1]], 
  wave_decomp[[2]], 
  tau_levels
)

# Calculate multiscale QTE across quantiles
tau_levels <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qte_results <- calculate_multiscale_qte(
  wave_decomp[[1]], wave_decomp[[2]], tau_levels
)

# Create beautiful heatmap visualization
plot_qte_heatmap(qte_results, title = "Multiscale QTE Analysis")
```

### 5️⃣ **Network Analysis**
```r
# Build QTE-based financial network
network <- create_qte_network(processed_data[1:256, ], 
                             wave_decomp, scale = 1, tau = 0.5)

# Compute comprehensive network metrics
metrics <- calculate_network_metrics(network)
print(paste("Network density:", round(metrics$density, 3)))

# Create interactive network visualization
plot_enhanced_network(network, scale = 1, tau = 0.5)
```

### 6️⃣ **Financial Contagion Analysis**
```r
# Analyze contagion between developed and emerging markets
dm_data <- get_stock_data(data_source = "builtin", 
                          indices = c("GSPC", "N225", "FTSE"))
em_data <- get_stock_data(data_source = "builtin", 
                          indices = c("SSE", "BSE", "HSI"))

# Measure contagion effects
contagion_results <- calculate_contagion_index(dm_data[1:256, ], 
                                              em_data[1:256, ])

# Identify crisis periods automatically
crisis_periods <- identify_crisis_periods(cbind(dm_data, em_data))
```
## 📚 Learning Resources

### 📖 **Comprehensive Vignettes**
- **[Basic Usage](articles/basic-usage.html)** - Get started with core functionality
- **[Advanced Features](articles/advanced-features.html)** - Explore sophisticated analysis techniques  
- **[Contagion Analysis](articles/contagion-analysis.html)** - Specialized crisis and spillover analysis

### 📊 **Data Sources**
WaveQTE supports multiple data sources for maximum flexibility:

| Source | Description | Use Case |
|--------|-------------|----------|
| **Built-in** | 10 global market indices (2015-2023) | Testing and examples |
| **Yahoo Finance** | Real-time market data via quantmod | Live analysis |
| **Custom CSV** | Your own data files | Specialized datasets |
| **Data Frames** | Pre-loaded R objects | Custom workflows |

```r
# Examples of different data sources
builtin_data <- get_stock_data(data_source = "builtin")
yahoo_data <- get_stock_data(c("AAPL", "GOOGL"), "2020-01-01", "2023-12-31")
custom_data <- get_stock_data(data_source = "file", file_path = "my_data.csv")
```

## 🎯 Use Cases

<div class="use-cases">

### 🏛️ **Academic Research**
- Publish-ready analysis for finance journals
- Robust statistical methods with bootstrap testing
- Comprehensive documentation and references

### 🏢 **Risk Management**
- Real-time spillover monitoring
- Crisis early warning systems
- Portfolio diversification analysis

### 📊 **Policy Analysis**
- Systemic risk assessment
- Financial stability monitoring
- Cross-border contagion analysis

### 💼 **Investment Strategy**
- Market interconnectedness insights
- Timing and allocation decisions
- Risk-adjusted portfolio construction

</div>

## 🏗️ Package Architecture

```
WaveQTE/
├── 📁 R/                          # Core functionality
│   ├── 🔧 data_preparation.R       # Data import & processing
│   ├── 〰️ wavelet_decomposition.R # Multi-scale analysis
│   ├── 📊 quantile_transfer_entropy.R # QTE calculations
│   ├── 🕸️ network_analysis.R       # Network construction
│   ├── 🌊 contagion_analysis.R     # Crisis analysis
│   ├── 📈 visualization.R          # Rich visualizations
│   └── 🛠️ utils.R                 # Helper functions
├── 📚 vignettes/                   # Comprehensive tutorials
├── 🧪 tests/                       # Extensive test suite
└── 📖 man/                         # Complete documentation
```
## 🌟 Real-World Applications

<div class="applications">

| Application | Description | Benefit |
|-------------|-------------|---------|
| **🏛️ Academic Research** | Publish-ready analysis for finance journals | Rigorous methodology with statistical testing |
| **🏢 Risk Management** | Real-time spillover monitoring | Early warning systems for portfolio protection |
| **📊 Policy Analysis** | Systemic risk assessment | Evidence-based financial stability decisions |
| **💼 Investment Strategy** | Market interconnectedness insights | Enhanced portfolio diversification |
| **🌍 Crisis Analysis** | Cross-border contagion detection | Understanding global financial linkages |

</div>

## 📈 Performance & Quality

- ✅ **109 passing tests** with comprehensive coverage
- ✅ **CRAN-compliant** package structure
- ✅ **Optimized algorithms** for large datasets
- ✅ **Bootstrap validation** for statistical robustness
- ✅ **Extensive documentation** with real-world examples

## 🤝 Contributing

We welcome contributions! Whether you're fixing bugs, adding features, or improving documentation:

1. 🍴 Fork the repository
2. 🌱 Create a feature branch
3. ✨ Make your changes
4. 🧪 Run tests (`devtools::test()`)
5. 📝 Submit a pull request

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 📚 Citation

If you use WaveQTE in your research, please cite:

```bibtex
@misc{WaveQTE2024,
  author = {Avishek Bhandari},
  title = {WaveQTE: Wavelet-based Quantile Transfer Entropy Analysis for Financial Time Series},
  year = {2024},
  publisher = {GitHub},
  url = {https://github.com/avishekb9/WaveQTE},
  note = {R package version 0.1.0}
}
```

## 💬 Support & Community

- 🐛 **Issues**: [GitHub Issues](https://github.com/avishekb9/WaveQTE/issues)
- 📧 **Contact**: bavisek@gmail.com
- 📖 **Documentation**: [Package Website](https://avishekb9.github.io/WaveQTE)

## 🙏 Acknowledgments

WaveQTE is built on the shoulders of giants:

- **R Core Team** - The R statistical computing environment
- **waveslim** package - Wavelet analysis functionality  
- **igraph** package - Network analysis capabilities
- **ggplot2** package - Beautiful and flexible visualizations
- **quantmod** package - Financial data integration

---

<div align="center">

**⭐ Star this repository if WaveQTE helps your research! ⭐**

[Get Started](articles/basic-usage.html) • [Documentation](reference/index.html) • [Examples](articles/) • [GitHub](https://github.com/avishekb9/WaveQTE)

</div>


