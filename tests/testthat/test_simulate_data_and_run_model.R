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

})
