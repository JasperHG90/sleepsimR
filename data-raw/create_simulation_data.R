## Create dataset for simulations
##
## This file takes the settings for each parameter that will be varied in the simulation study
##  and creates the parameters for each scenario in full factorial design. It also generates
##  start values and seeds for each scenario

# Seed
set.seed(3547912)

## Settings

# Number of subjects
n <- c(10, 20, 40, 80)
# Number of timesteps for each subject
n_t <- c(400,800, 1600, 3200)
# Variance of the between-subject component distributions
zeta <- c(0.5, 1, 2)
# Varuance of the between-subject transition probabilities
Q <- c(0.1, 0.2, 0.4)

# Create data
scenarios <- expand.grid(n, n_t, zeta, Q)

# Create seeds for each scenario
seeds <- sample.int(1000000, nrow(scenarios), replace=FALSE)

# Start values for each dataset

# ..
# ..
# ..
# ..
