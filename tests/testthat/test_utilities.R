context("Test utility functions (MAP values, posterior plots etc)")

test_that("Can get MAP estimates", {
  # Load model
  mod <- readRDS("data/mod.rds")
  # Expect class
  expect_s3_class(mod, "mHMM_cont")
  # Get MAP estimates
  me <- MAP(mod)
  # Assert length
  expect_length(me, 9)
  # Of each important variable, check values
  gamma_int <- me$gamma_int_bar
  emiss <- list(emiss_mu, emiss_var, emiss_varmu)
  # Expect names and length
  expect_length(gamma_int, 2)
  expect_named(gamma_int, c("mean", "SE"))
  expect_length(gamma_int$mean, 6)
  expect_length(gamma_int$SE, 6)
  # For each in emiss, check
  for(var_idx in seq_along(emiss)) {
    var <- emiss[[var_idx]]
    for(idx in seq_along(var)) {
      el <- var[[idx]]
      expect_length(el, 2)
      expect_named(el, c("mean", "SE"))
      expect_length(el$mean, 3)
      expect_length(el$SE, 3)
    }
  }
})

test_that("Can retrieve credible intervals", {
  mod <- readRDS("data/mod.rds")
  # Burn
  mod <- burn(mod)
  ci <- credible_interval(mod$gamma_int_bar)
  # Expect 2 rows (2.5% and 97.5%)
  expect_equal(nrow(ci), 2)
  # Expect 6 columns (6 intercepts)
  expect_equal(ncol(ci), 6)
})

test_that("Can plot posterior distributions", {
  # Load model
  mod <- readRDS("data/mod.rds")
  for(idx in c(1,2,3)) {
    op <- plot_posterior(mod, var=idx)
    expect_s3_class(op, "ggplot")
  }
})

test_that("Can plot trace plot", {
  # Load model
  mod <- readRDS("data/mod.rds")
  for(idx in c(1,2,3)) {
    op <- trace_plot(mod, var=idx)
    expect_s3_class(op, "ggplot")
  }
})
