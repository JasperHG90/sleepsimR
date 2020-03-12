#' Generate parameter settings for each simulation iteration
#'
#' @param seed random seed to use for generating parameter settings for each iteration. Defaults to 3547912, which is the seed I use for my thesis.
#'
#' @return data frame with 36.000 rows and 11 columns
#' \describe{
#'    \item{n}{int. number of subjects}
#'    \item{n_t}{int. number of observed data points for each subject}
#'    \item{zeta}{float. between-subject variance for the means of the emission distributions}
#'    \item{Q}{float. between-subject variance for the intercepts of the transition-probability matrix}
#'    \item{scenario_id}{string .unique id of the simulation scenario}
#'    \item{rank}{int. iteration number of the simulation scenario}
#'    \item{iteration_id}{string. unique id of the iteration}
#'    \item{dsim_seed}{int. random seed used to generate the data}
#'    \item{model_seed}{int. random seed used to run the mHMM}
#'    \item{start_gamma}{json. initial values for the between-subject transition probability matrix}
#'    \item{start_emiss}{json. intiial values for each of the 3 emission distributions}
#' }
#'
#' @details In my simulation study, I test 250 iterations for each of 144 different scenarios. Hence, this function
#'          returns a data frame with 36.000 rows - one for each iteration - that contains specific information
#'          used to generate and model the data
#'
#' @export
generate_scenarios <- function(seed= 3547912) {
  # Check dependencies
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package \"digest\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("assertthat", quietly = TRUE)) {
    stop("Package \"assertthat\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Set seed
  set.seed(seed)
  # Number of subjects
  n <- c(10, 20, 40, 80)
  # Number of timesteps for each subject
  n_t <- c(400, 800, 1600, 3200)
  # Variance of the between-subject component distributions
  zeta <- c(0.5, 1, 2)
  # Variance of the between-subject transition probabilities
  Q <- c(0.1, 0.2, 0.4)
  # Create data
  scenarios <- expand.grid(n, n_t, zeta, Q)
  # Column names
  colnames(scenarios) <- c("n", "n_t", "zeta", "Q")
  # Create unique id for each scenario
  scenarios$scenario_id <- vapply(1:nrow(scenarios), function(x) digest::digest(paste(scenarios[x,],
                                                                              collapse="_"),
                                                                              algo = "md5"),
                                  "string")
  # Create iteration for each scenario
  scenarios <- lapply(1:nrow(scenarios), function(x) {
    lst <- lapply(1:250, function(y) {
      io <- scenarios[x,]
      io$rank <- y
      io$iteration_id <- digest::digest(paste(io, collapse="_"), algo="md5")
      # Add dummy variable whether or not to save all model data (+-5%)
      io$save_model <- ifelse(runif(nrow(io)) <= 0.05, TRUE, FALSE)
      io
    })
    do.call(rbind.data.frame, lst)
  })
  # Bind
  scenarios <- do.call(rbind.data.frame, scenarios)
  # Make seeds for (1) data simulations and (2) model execution
  seeds <- sample.int(10000000, nrow(scenarios) * 2, replace=FALSE)
  scenarios$dsim_seed <- seeds[1:36000]
  scenarios$model_seed <- seeds[36001:72000]
  # Start values for each dataset
  # Gamma
  ## TPM gamma
  diag_value <- runif(36000, 0.5, 0.8)
  assertthat::are_equal(length(unique(diag_value)), nrow(scenarios))
  start_gamma <- lapply(diag_value, function(x) {
    gam <- diag(x, 3)
    gam[lower.tri(gam) | upper.tri(gam)] <- (1-x) / 2
    # Assert row sums equal to one
    assertthat::are_equal(rowSums(gam), c(1,1,1))
    # Return json array with flat vector
    return(jsonlite::toJSON(list("tpm" = as.vector(gam)),
                            pretty = TRUE,
                            flatten = TRUE))
  })
  ## Emission distributions
  # For the 3 continuous emission distributions
  start_emiss <- lapply(1:36000, function(x) {
    jsonlite::toJSON(list(
      #EEG_Fpz_Cz_max_gamma
      EEG_Fpz_Cz_mean_beta = c( -.38 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                                0 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                                1.13 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1)),
      # EOG_median_theta
      EOG_median_theta = c( .5 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                            -.6 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                            .4 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1)),
      # EOG_min_beta
      EOG_min_beta = c( 1 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                        -.9 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1),
                        -.1 + runif(1, -.2, .2), 0.2 + runif(1, -.1,.1))
    ), pretty = TRUE, flatten = TRUE)
  })
  # Add to data
  scenarios$start_gamma <- as.character(start_gamma)
  scenarios$start_emiss <- as.character(start_emiss)
  # Return
  return(scenarios)
}
