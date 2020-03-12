## Functions used to run the model are placed here

#' Run an mHMM on a simulated sleep dataset
#'
#' @param data Matrix. data set used to run the mHMM. See s_data parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param start_values List (must be unnamed). start values for relevant parameters. See start_val parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param hyperprior_means Numeric vector. Contains the hyperprior value for the between-subject distribution means. See \link[mHMMbayes]{mHMM_cont}.
#' @param model_seed Int. Random seed that is set before running the model.
#' @param mcmc_iterations Int. number of iterations for the MCMC algorithm. Defaults to 1000. See mcmc parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param mcmc_burn_in Int. number of burn-in samples for the MCMC algorithm. Defaults to 500. See mcmc parameter in \link[mHMMbayes]{mHMM_cont}.
#' @param show_progress Boolean. Should progress of MCMC algorithm be displayed? Defaults to TRUE.
#' @param order_data Boolean. Should hyperpriors and start values be sorted from lowest to highest? This is required to record label switching. See \link[mHMMbayes]{mHMM_cont}.
#'
#' @return An mHMM_cont object containing posterior distributions for each of the parameters.
#'
#' @importFrom mHMMbayes mHMM_cont
#'
#' @export
run_mHMM <- function(data, start_values, hyperprior_means, model_seed, mcmc_iterations = 2000, mcmc_burn_in = 1000, show_progress = TRUE,
                     order_data = TRUE) {
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
  # Order!
  state_orders <- vector("list", mprop$n_dep)
  names(state_orders) <- colnames(tdf[,-1])
  # Checks on Mean hyperprior
  if(!(length(hyperprior_means) == mprop$n_dep)) {
    stop(paste0("User must pass exactly as many hyperprior means as there are dependent variables (", mprop$n_dep, ")"))
  }
  hyp_means <- vector("list", mprop$n_dep)
  for(i in seq_along(hyperprior_means)) {
    if(!(length(hyperprior_means[[i]]) == mprop$m)) {
      stop(paste0("Hyperprior on the group-level means of variable ",
                  i, " is not of length ", mprop$m))
    }
    hyp_means[[i]] <- matrix(
      ifelse(order_data, sort(hyperprior_means[[i]]), hyperprior_means[[i]]), nrow=1, ncol=mprop$m
    )
  }
  # Sort the start values by size
  for(idx in seq_along(start_values)) {
    # First is gamma
    if(idx == 1) {
      next
    }
    if(order_data) {
      # Save order (so I can do the same to ground-truth values)
      state_orders[[idx-1]] <- sort.list(start_values[[idx]][,1])
      start_values[[idx]] <- start_values[[idx]][sort.list(start_values[[idx]][,1]),]
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
  # Add state orders
  mod$state_orders <- state_orders
  # Return model
  return(mod)
}
