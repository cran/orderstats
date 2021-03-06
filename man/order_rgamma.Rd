% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_stats.R
\name{order_rgamma}
\alias{order_rgamma}
\title{Gets random order statistics from a gamma distribution}
\usage{
order_rgamma(draw_size, shape, scale, k, n)
}
\arguments{
\item{draw_size}{- The size of the output sample}

\item{shape}{- The shape parameter in the gamma distribution}

\item{scale}{- The scale parameter in the gamma distribution}

\item{k}{- The Kth smallest value from a sample}

\item{n}{- The size of the sample to compute the order statistic from}
}
\value{
A vector of random order statistic variables from a gamma distribution
}
\description{
Gets random order statistics from a gamma distribution
}
\examples{
order_rgamma(10, 20, 2, 100, 10000)
}

