# Contributing to WaveQTE

Thank you for your interest in contributing to WaveQTE! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## How to Contribute

1. Fork the repository
2. Create a new branch for your feature
3. Make your changes
4. Run tests and checks
5. Submit a pull request

## Development Setup

1. Install R (>= 3.5.0)
2. Install required packages:
```R
install.packages(c("devtools", "roxygen2", "testthat"))
```
3. Clone your fork:
```bash
git clone https://github.com/your-username/WaveQTE.git
```
4. Install package dependencies:
```R
devtools::install_deps()
```

## Code Guidelines

* Follow the existing code style
* Add roxygen2 documentation for new functions
* Include examples in function documentation
* Add unit tests for new functionality
* Use meaningful variable names
* Add error handling where appropriate

## Testing

Before submitting a pull request:

```R
devtools::document()
devtools::test()
devtools::check()
```

## Documentation

* Update relevant documentation files
* Add/update function documentation
* Update vignettes if needed
* Update NEWS.md for user-visible changes

## Submitting Changes

1. Push your changes to your fork
2. Submit a pull request
3. Describe your changes
4. Reference any relevant issues

## Questions?

Feel free to open an issue for questions or suggestions.