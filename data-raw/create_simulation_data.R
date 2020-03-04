## Create dataset for simulations
##
## This file takes the settings for each parameter that will be varied in the simulation study
##  and creates the parameters for each scenario in full factorial design. It also generates
##  start values and seeds for each scenario

library(digest)
# Seed
set.seed(3547912)

## Settings

# Number of subjects
n <- c(10, 20, 40, 80)
# Number of timesteps for each subject
n_t <- c(400,800, 1600, 3200)
# Variance of the between-subject component distributions
zeta <- c(0.5, 1, 2)
# Variance of the between-subject transition probabilities
Q <- c(0.1, 0.2, 0.4)
# Create data
scenarios <- expand.grid(n, n_t, zeta, Q)
# Column names
colnames(scenarios) <- c("n", "n_t", "zeta", "Q")
# Create seeds for each scenario
scenarios$seed <- sample.int(1000000, nrow(scenarios), replace=FALSE)
# Create unique id for each scenario
scenarios$scenario_id <- vapply(1:nrow(scenarios), function(x) digest(paste(scenarios[x,], collapse="_"), algo = "md5"), "string")
# Create iteration for each scenario
scenarios <- lapply(1:nrow(scenarios), function(x) {
    lst <- lapply(1:250, function(y) {
      io <- scenarios[x,]
      io$rank <- y
      io$iteration_id <- digest(paste(io, collapse="_"), algo="md5")
      io
    })
    do.call(rbind.data.frame, lst)
})
# Bind
scenarios <- do.call(rbind.data.frame, scenarios)

# Start values for each dataset

# ..
# ..
# ..
# ..
