## Load on startup

.onLoad <- function(libname = find.package("sleepsimR"), pkgname="sleepsimR") {
  # Assign unique id
  if(Sys.getenv("SLEEPSIMR_UID") == "") {
    Sys.setenv("SLEEPSIMR_UID" = uuid::UUIDgenerate())
  }
  # Set API host is not already in environment
  if(Sys.getenv("SLEEPSIMR_MASTER_HOST" == "")) {
    Sys.setenv("SLEEPSIMR_MASTER_HOST" = "http://localhost:5007")
  }
}
