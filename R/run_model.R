## Functions used to run the model are placed here

#' Run an mHMM on a simulated sleep dataset
#'
#' @param data Matrix. data set used to run the mHMM. See s_data parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param start_values List. start values for relevant parameters. See start_val parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param model_seed Int. Random seed that is set before running the model.
#' @param mcmc_iterations Int. number of iterations for the MCMC algorithm. Defaults to 1000. See mcmc parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param mcmc_burn_in Int. number of burn-in samples for the MCMC algorithm. Defaults to 500. See mcmc parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param show_progress Boolean. Should progress of MCMC algorithm be displayed? Defaults to TRUE.
#'
#' @return An mHMM_cont object containing posterior distributions for each of the parameters.
#'
#' @importFrom mHMMbayes mHMM_cont
#' @importFrom assertthat are_equal
#'
#' @export
run_mHMM <- function(data, start_values, model_seed, mcmc_iterations = 2000, mcmc_burn_in = 1000, show_progress = TRUE,
                     hyperprior_means = NULL) {
  # Model properties
  mprop = list(
    "m" = nrow(getOption("sleepsimR_simulate")[["gamma_bar"]]),
    "n_dep" = 3
  )
  # Mcmc options
  mcmcOpts <- list(
    "J"=mcmc_iterations,
    "burn_in"=mcmc_burn_in
  )
  # Mean hyperprior
  if(is.null(hyperprior_means)) {
    hyp_means <- list(
      # Depvar 1
      matrix(c(0,0,0), nrow=1, ncol=mprop$m),
      # Depvar 2
      matrix(c(0,0,0), nrow=1, ncol=mprop$m),
      # Depvar 3
      matrix(c(0,0,0), nrow=1, ncol=mprop$m)
    )
  } else {
    assertthat::are_equal(length(hyperprior_means), mprop$n_dep)
    hyp_means <- vector("list", mprop$n_dep)
    for(i in seq_along(hyperprior_means)) {
      assertthat::assert_that(length(hyperprior_means[[i]]) == mprop$m,
                              msg=paste0("Hyperprior on the group-level means of variable ",
                                         i, " is not of length ", mprop$m))
      hyp_means[[i]] <- matrix(
        hyperprior_means[[i]], nrow=1, ncol=mprop$m
      )
    }
  }
  # Set hyper-prior values
  hyper_priors <- list(
    # Hyperprior on intercepts of dependent variables
    emiss_mu0 = hyp_means,
    # Hyperprior on the number of subjects in each state
    # Hypothetical subjects --> c(1,1,1)
    emiss_K0 = list(1,1,1),
    # Degrees of freedom on the emission dist. means
    # Hypothetical degrees of freedom
    emiss_nu = list(1,1,1),
    # Variances between subjects
    # Prior on hyperparameter between-subject variance
    # Hypothetical variances between hypothetical subjects
    emiss_V = list(
      # Depvar1
      c(.1,.1,.1),
      # Depvar2
      c(.1,.1,.1),
      # Depvar3
      c(.1,.1,.1)
    ),
    # shape values. Fixed variances of normal emission distributions
    # SUbject-fixed normal emission distribution shape/scale parameters
    # This is a regular inverse gamma
    emiss_a0 = list(
      # Depvar1
      c(.01,.01,.01),
      # Depvar2
      c(.01,.01,.01),
      # Depvar3
      c(.01,.01,.01)
    ),
    # Hyperprior on scale values of inverse gamma
    emiss_b0 = list(
      # Depvar1
      c(.01,.01,.01),
      # Depvar2
      c(.01,.01,.01),
      # Depvar3
      c(.01,.01,.01)
    )
  )
  # Set seed
  set.seed(model_seed)
  # Run model
  mod <- mHMMbayes::mHMM_cont(as.matrix(data),
                              gen=mprop,
                              start_val=start_values,
                              mcmc = mcmcOpts,
                              emiss_hyp_prior = hyper_priors,
                              show_progress=show_progress)
  # Return model
  return(mod)
}
