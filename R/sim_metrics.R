## Metrics (% bias etc) go here
# See https://cran.r-project.org/web/packages/rsimsum/vignettes/A-introduction.html

#' Compute the parameter bias of a vector estimated parameters versus the ground-truth value of that parameter
#'
#' @param true_param_value numeric. Value of the ground-truth parameter.
#' @param simulated_param_values k-length numeric vector. k >= 1 and holds the parameter values of the estimated parameters.
#'
#' @return average bias of simulated values versus the ground-truth value
#'
#' @details This function computes the percentage bias by using the signed mean difference.
#'
#' @export
bias <- function(true_param_value, simulated_param_values) {
  mean(simulated_param_values - true_param_value)
}

#' MCMC Standard Error of the bias value
#'
#' @param

#' Compute the emperical SE
#'
#' @param x numeric vector. Simulated parameter estimates
#'
#' @return numeric scalar. Emperical standard error.
#'
#' @export
emperical_SE <- function(x) {
  sqrt((1/(length(x) - 1)) * sum((x - mean(x))^2))
}

#' Compute the MCMC SE
#'
#' @param x numeric vector. Simulated parameter estimates.
#'
#' @return numeric scalar. MCMC standard error.
#'
#' @export
MCMC_SE <- function(x) {
  emperical_SE(x) / sqrt(2*(length(x) - 1))
}

#' Compute coverage
#'
#' @param CI list. Each element contains the upper and lower values of the 95% CI of an interation.
#' @param true_param_value numeric scalar. True value of the parameter.
#'
#' @return numeric scalar. Probability that the 95% CI contains the true value.
#'
#' @export
coverage <- function(x) {

}
