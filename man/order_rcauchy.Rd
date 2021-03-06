% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_stats.R
\name{order_rcauchy}
\alias{order_rcauchy}
\title{Gets random order statistics from a cauchy distribution}
\usage{
order_rcauchy(draw_size = 1, location = 0, scale = 1, k = 1, n = 1)
}
\arguments{
\item{draw_size}{- The size of the output sample}

\item{location}{- The location parameter in the cauchy distribution}

\item{scale}{- The scale parameter in the cauchy distribution}

\item{k}{- The Kth smallest value from a sample}

\item{n}{- The size of the sample to compute the order statistic from}
}
\value{
A vector of random order statistic variables from a cauchy distribution
}
\description{
Gets random order statistics from a cauchy distribution
}
\examples{
order_rcauchy(10, 0, 1, 100, 10000)
}

