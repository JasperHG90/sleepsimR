## Functions used to simulate data are placed here

#' Simulate a sleep dataset for use in an mHMM model
#'
#' @param n int. number of subjects.
#' @param n_t int. number of observations for each subject.
#' @param zeta float. between-subject variance of the emission distributions.
#' @param Q float. between-subject variance of the transition probability matrix.
#' @param seed int. random seed used to generate the dataset.
#'
#' @return list containing states and emission distribution observed data. See \link[mHMMbayes]{sim_mHMM}.
#'
#' @importFrom mHMMbayes sim_mHMM
#'
#' @export
simulate_dataset <- function(n, n_t, zeta, Q, seed) {
  # Retrieve parameter values from options
  # (set in zzz.R)
  gamma <- getOption("sleepsimR_simulate")[["gamma_bar"]]
  emiss <- getOption("sleepsimR_simulate")[["emission_bar"]]
  m <- nrow(gamma)
  n_dep <- length(emiss)
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
    # Number of emission distributions
    n_dep = n_dep,
    # Start state (Awake)
    start_state = 1,
    # Transition probabilities
    gamma = gamma,
    # Emission distribution means + var
    emiss_distr = emiss,
    # Between-subject variance for TPM
    var_gamma = Q,
    # Between-subject variance for emission distributions
    var_emiss = rep(zeta, m)
  )
  # Return
  return(data_simulated)
}
