context("Simulate a dataset and run an mHMM")

test_that("Can simulate a dataset and run the mHMM", {
  # Load sample data
  sd <- read.csv("tests/testthat/data/scen_sample.csv.gz", stringsAsFactors = FALSE)
  sd <- sd[,-1]
  # Select a scenario
  scen <- sd[1,]
  # Set #iterations low
  scen$n_t <- 10
  scen$n <- 3
  # Simulate dataset
  d <- simulate_dataset(scen$n, scen$n_t, scen$zeta, scen$Q, scen$dsim_seed)
  # Expect dims
  expect_equal(length(d), 2)
  expect_equal(dim(d[[1]]), c(4000,2))
  expect_equal(dim(d[[2]]), c(4000,4))
  # To data frame
  tdf <- data.frame(
    id = d$obs[,1],
    EEG_mean_beta = d$obs[,2],
    EOG_median_theta = d$obs[,3],
    EOG_min_beta = d$obs[,4]
  )
  # Make hyperprior values
  hyp_priors <- list(
    as.vector(tapply(tdf[,-1]$EEG_mean_beta, states, mean)),
    as.vector(tapply(tdf[,-1]$EOG_median_theta, states, mean)),
    as.vector(tapply(tdf[,-1]$EOG_min_beta, states, mean))
  )
  # Read from json
  gammasv <- jsonlite::fromJSON(scen$start_gamma, simplifyVector = FALSE)
  emisssv <- jsonlite::fromJSON(scen$start_emiss, simplifyVector = FALSE)
  # Reshape start values
  m <- sqrt(length(gammasv[[1]]))
  start_values <- list(
    matrix(unlist(gammasv), nrow=m, ncol=m,
           byrow = TRUE),
    matrix(unlist(emisssv$EEG_Fpz_Cz_mean_beta),
           ncol=2, byrow = TRUE),
    matrix(unlist(emisssv$EOG_median_theta),
           ncol=2, byrow = TRUE),
    matrix(unlist(emisssv$EOG_min_beta),
           ncol=2, byrow=TRUE)
  )
  # Run model
  mod <- sleepsimR::run_mHMM(tdf, start_values = start_values, hyperprior_means = hyp_priors,
                             model_seed = scen$model_seed,mcmc_iterations=10, mcmc_burn_in = 1,
                             show_progress = FALSE)
  # Expect objects
  expect_equal(names(mod), c('input','PD_subj','gamma_int_subj','gamma_int_bar','gamma_cov_bar',
                             'emiss_cov_bar','gamma_prob_bar','emiss_mu_bar','gamma_naccept',
                             'emiss_varmu_bar','emiss_var_bar','label_switch','state_orders'))
})
