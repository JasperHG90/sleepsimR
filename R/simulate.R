## Functions used to simulate data are placed here

#' Simulate a sleep dataset for use in an mHMM model
#'
#' @param n number of subjects
#' @param n_t number of observations for each subject
#' @param zeta between-subject variance of the emission distributions
#' @param Q between-subject variance of the transition probability matrix
#' @param seed
#'
#' @return list containing states and emission distribution observed data. See \link[mHMMbayes]{sim_mHMM}.
#'
#' @importFrom mHMMbayes sim_mHMM
#'
#' @export
#'
#' @details
simulate_dataset <- function(n, n_t, zeta, Q, seed) {
  # Retrieve parameter values from options
  # (set in zzz.R)
  gamma <- getOption("gamma_bar")
  emiss <- getOption("emission_bar")
  m <- nrow(gamma)
  n_dep <- length(emiss_dist)
  # Set seed
  set.seed(seed)
  # Simulate dataset
  data_simulated <- mHMMbayes::sim_mHMM(
    # Number of observations for each person
    n_t = n_t,
    # Number of persons
    n = n,
    # Type of emission distributions
    data_distr = "continuous",
    # Number of states
    m = m,
    n_dep = ,n_dep,
    start_state = 1,
    gamma = gamma,
    emiss_distr = emiss,
    var_gamma = Q,
    var_emiss = zeta
  )
  # Return
  return(data_simulated)
}
