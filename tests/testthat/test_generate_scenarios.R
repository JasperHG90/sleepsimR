context("Test that generated scenarios are equal to the scenarios I used in my thesis")

test_that("Can generate simulation scenarios and they are equal to the scenarios in the program <sleepsimR-api>", {
  # Load sample data
  sd <- read.csv("data/scen_sample.csv.gz", stringsAsFactors = FALSE)
  sd <- sd[,-1]
  # Generate scenarios
  scen <- generate_scenarios()
  # Subset same 100 rows as above
  set.seed(445566)
  scen_subs <- scen[sample(nrow(scen)),][1:100,]
  # Expect equal rows
  expect_equal(nrow(scen_subs), nrow(sd))
  # Check equality of all values
  eq_vect <- rep(FALSE, nrow(scen_subs))
  for(idx in seq_along(eq_vect)) {
    eq_vect[idx] <- all(sd[idx,] == scen_subs[idx,])
  }
  # Expect all rows to be the same
  expect_equal(sum(eq_vect), 100)
})
