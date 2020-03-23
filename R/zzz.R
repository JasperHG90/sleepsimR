## Load on startup

.onLoad <- function(libname = find.package("sleepsimR"), pkgname="sleepsimR") {
  # Set API host is not already in environment
  if(Sys.getenv("SLEEPSIMR_MASTER_HOST") == "") {
    Sys.setenv("SLEEPSIMR_MASTER_HOST" = "http://localhost:5007")
  }
  # Hard-code options in the package
  options(
    sleepsimR_simulate = list(
      # List of estimated (first element) and set residual variances
      "kappa" <- list(
          list(
            "EEG_Fpz_Cz_beta" = c(0.64, 0.87, 0.81),
            "EOG_median_beta" = c(0.134, 0.28, 0.22),
            "EOG_min_theta" = c(0.37, 0.35, 0.52)
          ),
          list(
            "EEG_Fpz_Cz_beta" = c(0.11, 0.16, 0.15),
            "EOG_median_beta" = c(0.124, 0.15, 0.13),
            "EOG_min_theta" = c(0.16, 0.12, 0.14)
          )
      ),
      # Between-subject TPM
      "gamma_bar" = matrix(
        c(0.984, 0.003, 0.013,
          0.007, 0.959, 0.034,
          0.012, 0.021, 0.967),
        nrow = 3, ncol = 3, byrow = TRUE
      ),
      # emission distributions
      "emission_bar" = list(
        # EEG_Fpz_Cz_mean_beta
        matrix(c(
          -0.36, 0.11, # 1
          -0.6, 0.16, # 2
          0.7, 0.15   # 3
        ), nrow=3, ncol=2,
        byrow = TRUE),
        # EOG_median_theta
        matrix(c(
          1.01, 0.23, # 1
          -1.31, 0.18, # 2
          -0.24, 0.19  # 3
        ), nrow=3, ncol=2,
        byrow=TRUE),
        # EOG_min_theta
        matrix(c(
          0.75, 0.16,  # 1
          -1.31, 0.12, # 2
          0.005, 0.14  # 3
        ), nrow=3, ncol=2,
        byrow=TRUE)
      )
    ),
    # Unique ID for this session
    "sleepsimR_uid" = uuid::UUIDgenerate()
  )
}
