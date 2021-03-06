% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_stats.R
\name{order_probs}
\alias{order_probs}
\title{Gets order statistics from a 0-1 uniform distribution}
\usage{
order_probs(draw_size, k, n)
}
\arguments{
\item{draw_size}{- The size of the output sample}

\item{k}{- The Kth smallest value from a sample}

\item{n}{- The size the sample to compute the order statistic from}
}
\value{
A vector of random order statistic variables from a 0-1 uniform distribution
}
\description{
Gets order statistics from a 0-1 uniform distribution
}
\references{
Gentle, James E. (2009), Computational Statistics, Springer, p. 63, ISBN 9780387981444
}

