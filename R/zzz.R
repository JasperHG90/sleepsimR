## Load on startup

.onLoad <- function(libname = find.package("sleepsimR"), pkgname="sleepsimR") {
  # Set API host is not already in environment
  if(Sys.getenv("SLEEPSIMR_MASTER_HOST") == "") {
    Sys.setenv("SLEEPSIMR_MASTER_HOST" = "http://localhost:5007")
  }
  # Hard-code options in the package
  options(
    sleepsimR_simulate = list(
      "m" = 3,                # Sleep states
      "distr" = "continuous", # Simulate a continuous distribution
      "n_dep" = 2            # Number of dependent variables
    ),
    # Unique ID for this session
    sleepsimR_uid = uuid::UUIDgenerate()
  )
}
