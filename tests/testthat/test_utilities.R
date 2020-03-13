context("Test utility functions (MAP values, posterior plots etc)")

test_that("Can get MAP estimates", {
  # Load model
  mod <- readRDS("tests/testthat/data/mod.rds")
  # Get MAP estimates
  me <- MAP(mod)
})
