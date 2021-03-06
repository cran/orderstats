% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_stats.R
\name{order_rexp}
\alias{order_rexp}
\title{Gets random order statistics from an exponential distribution}
\usage{
order_rexp(draw_size, rate, k, n)
}
\arguments{
\item{draw_size}{- The size of the output sample}

\item{rate}{- The shape parameter in the exponential distribution}

\item{k}{- The Kth smallest value from a sample}

\item{n}{- The size of the sample to compute the order statistic from}
}
\value{
A vector of random order statistic variables from an exponential distribution
}
\description{
Gets random order statistics from an exponential distribution
}
\examples{
order_rexp(10, 0.005, 100, 10000)
}

