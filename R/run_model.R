## Functions used to run the model are placed here

#' Run an mHMM on a simulated sleep dataset
#'
#' @param
#'
#' @return
#'
run_mHMM <- function(mcmc_iterations = 2000, mcmc_burn_in = 1000) {
  # Model properties
  mprop = list(
    "m" = 3,
    "n_dep" = 3
  )

  # Mcmc options
  mcmcOpts <- list(
    "J"=mcmc_iterations,
    "burn_in"=mcmc_burn_in
  )
}
