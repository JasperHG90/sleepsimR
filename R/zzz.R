## Load on startup

.onLoad <- function(libname = find.package("sleepsimR"), pkgname="sleepsimR") {
  # Set API host is not already in environment
  if(Sys.getenv("SLEEPSIMR_MASTER_HOST") == "") {
    Sys.setenv("SLEEPSIMR_MASTER_HOST" = "http://localhost:5007")
  }
  # Hard-code options in the package
  options(
    sleepsimR_simulate = list(
      # Sleep states
      "m" = 3,
      # Emission distribution type
      "distr" = "continuous",
      # Number of dependent variables
      "n_dep" = 3,
      # Between-subject TPM
      "gamma_bar" = matrix(
        c(0.983611984, 0.002889722, 0.01349829,
          0.006792402, 0.959200913, 0.03400669,
          0.012392675, 0.021041540, 0.96656579),
        nrow = 3, ncol = 3, byrow = TRUE
      ),
      # Between-subject emission distributions
      "emission_bar" = list(
        "EEG_Fpz_Cz_mean_beta" = c(-.3641326, -0.5949267, 0.6950765),
        "EOG_median_theta" = c(1.0143346, -1.3078620, -0.2425523),
        "EOG_min_beta" = c(0.745672484, -1.310224312, 0.004942798)
      ),
      # Which state to start in when simulating
      "start_state" = 1
    ),
    # Unique ID for this session
    sleepsimR_uid = uuid::UUIDgenerate()
  )
}
