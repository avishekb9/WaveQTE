% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile_transfer_entropy.R
\name{test_qte_significance}
\alias{test_qte_significance}
\title{Test QTE Significance}
\usage{
test_qte_significance(x, y, tau = 0.5, n_permutations = 1000)
}
\arguments{
\item{x}{Source time series}

\item{y}{Target time series}

\item{tau}{Quantile level}

\item{n_permutations}{Number of permutations for test}
}
\value{
List containing test statistics and p-value
}
\description{
Test QTE Significance
}
\examples{
\donttest{
x <- rnorm(100)
y <- 0.5 * x + rnorm(100, sd = 0.1)
test_result <- test_qte_significance(x, y)
}
}
