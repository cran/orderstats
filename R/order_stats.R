# This package uses the beta distributin to calculate uniform order statistics
# then it puts the vector of probabilities in an inverse CDF, which gives us a
# vector of random order statistics efficiently

#' Gets order statistics from a 0-1 uniform distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param k - The Kth smallest value from a sample
#' @param n - The size the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a 0-1 uniform distribution
#' @references
#'  Gentle, James E. (2009), Computational Statistics, Springer, p. 63, ISBN 9780387981444
#' @export
order_probs <- function(draw_size, k, n) {
  return(rbeta(draw_size, k, n + 1 - k))
}

#' Gets random order statistics from a normal distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param mean - The mean of the normal distribution
#' @param sd - The standard deviation of the normal distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a normal distribution
#' @examples
#' order_rnorm(10, 0, 1, 100, 10000)
#' @export
order_rnorm <- function (draw_size = 1, mean = 0, sd = 1, k = 1, n =1) {
  orders <- order_probs(draw_size, k, n)
  return(qnorm(orders, mean, sd))
}

#' Gets random order statistics from a chi-square distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param df - The degrees of the chi-square distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a chi-square distribution
#' @examples
#' order_rchisq(10, 1, 100, 10000)
#' @export
order_rchisq <- function (draw_size, df, k, n) {
  orders <- order_probs(draw_size, k, n)
  return(qchisq(orders, df))
}

#' Gets random order statistics from a cauchy distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param location - The location parameter in the cauchy distribution
#' @param scale - The scale parameter in the cauchy distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a cauchy distribution
#' @examples
#' order_rcauchy(10, 0, 1, 100, 10000)
#' @export
order_rcauchy <- function (draw_size = 1, location = 0, scale = 1, k = 1, n = 1) {
  orders <- order_probs(draw_size, k, n)
  return(qcauchy(orders, location, scale))
}

#' Gets random order statistics from a logistic distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param location - The location parameter in the logistic distribution
#' @param scale - The scale parameter in the logistic distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a logistic distribution
#' @examples
#' order_rlogis(10, 0, 1, 100, 10000)
#' @export
order_rlogis <- function (draw_size, location, scale, k, n) {
  orders <- order_probs(draw_size, k, n)
  return(qlogis(orders, location = location, scale = scale))
}

#' Gets random order statistics from a gamma distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param shape - The shape parameter in the gamma distribution
#' @param scale - The scale parameter in the gamma distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from a gamma distribution
#' @examples
#' order_rgamma(10, 20, 2, 100, 10000)
#' @export
order_rgamma <- function (draw_size, shape, scale, k, n) {
  orders <- order_probs(draw_size, k, n)
  return(qgamma(orders, shape = shape, scale = scale))
}

#' Gets random order statistics from an exponential distribution
#' @import stats
#' @param draw_size - The size of the output sample
#' @param rate - The shape parameter in the exponential distribution
#' @param k - The Kth smallest value from a sample
#' @param n - The size of the sample to compute the order statistic from
#' @return A vector of random order statistic variables from an exponential distribution
#' @examples
#' order_rexp(10, 0.005, 100, 10000)
#' @export
order_rexp <- function (draw_size, rate, k, n) {
  orders <- order_probs(draw_size, k, n)
  return(qexp(orders, rate = rate))
}

