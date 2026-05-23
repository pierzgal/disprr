# Regression test: shut-out parties (parties that win 0 seats in every
# district of an election) must still contribute |0 - v_k| to the aggregate
# disproportionality indexes. A shiny-app sibling of this code briefly
# computed seat counts with table() instead of tabulate(), which dropped
# zero-seat parties from the per-district output and silently under-counted
# LHI / GHI / SLI at high party / district counts. This test guards against
# that pattern returning to the package.

test_that("simulate_E indexes sum over ALL parties, not just winners", {
  # High np / nd combination forces several parties to be shut out.
  e <- simulate_E(
    seed = 1000, dist = "lnorm",
    np = 12, nd = 12, ne = 5,
    mean = 12, sd = 1.5, rate = 1 / 25000, max = 500000,
    TS = 100, formula = "dh", formula_dist = "hh",
    threshold = 0, threshold_country = 0
  )
  se  <- e$Seat_Excess
  pkg <- e$Disproportionality_per_elec

  # The scenario must actually produce shut-out parties, or the test is
  # vacuous.
  shutout_counts <- vapply(split(se, se$ElectionID), function(d) {
    sum(d$Seats == 0)
  }, integer(1))
  expect_true(any(shutout_counts > 0),
              info = "Probe scenario failed to produce any shut-out party.")

  # Recompute LHI directly from the full Seat_Excess (every party included)
  # and verify the package's value agrees.
  full_LHI <- vapply(split(se, se$ElectionID), function(d) {
    0.5 * sum(abs(d$SeatShare - d$VoteShare))
  }, numeric(1))

  expect_equal(as.numeric(pkg$LHI), unname(full_LHI), tolerance = 5e-3)
})
