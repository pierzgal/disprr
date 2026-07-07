# Regression tests: shut-out parties (parties that win 0 seats in every
# district of an election) must be RETAINED in Seat_Excess and contribute
# their full |0 - v_k| terms to the aggregate disproportionality indexes.
#
# Why this needs an explicit test: the property is implicit in
# divisorMethods() using tabulate(winners, nbins = length(parties)), which
# always emits a length-np vector with zeros. A refactor to table(winners)
# — which only contains parties that won at least one seat — would silently
# drop shut-out parties from every downstream merge and under-count
# LHI / GHI / SLI / meanRSE2. The shiny-app sibling of this code had exactly
# that bug (fixed there in v0.10.6): mean LHI under-counted by up to ~38%
# in high-district-count configurations. These tests make any such refactor
# fail loudly.
#
# All data below is SIMULATED (fixed seeds for reproducibility).

# Shared fixture: nd = 10 districts of ~10 seats each gives d'Hondt an
# effective per-district threshold of roughly 1/11, so the smallest of the
# 8 log-normal parties win no seats anywhere. Verified to produce 14
# shut-out party-elections under this seed.
sim_shut <- simulate_E(
  seed = 1000, dist = "lnorm",
  np = 8, nd = 10, ne = 10,
  mean = 12, sd = 1.5, max = 500000,
  TS = 100, formula = "dh", formula_dist = "hh"
)

test_that("all np parties appear in Seat_Excess for every election", {
  se <- sim_shut$Seat_Excess
  expect_equal(nrow(se), 8 * 10)
  expect_true(all(table(se$ElectionID) == 8),
              info = "every election must have exactly one row per party")
})

test_that("the fixture genuinely produces shut-out parties", {
  # Guards the tests below against becoming vacuous if the sampling or
  # apportionment internals ever change what this seed generates.
  se <- sim_shut$Seat_Excess
  shut <- se[se$Seats == 0 & se$Votes > 0, ]
  expect_gt(nrow(shut), 0)
})

test_that("a fully shut-out party has SeatShare 0 and RSE2_i = -1", {
  se <- sim_shut$Seat_Excess
  shut <- se[se$Seats == 0 & se$Votes > 0, ]
  expect_true(all(shut$SeatShare == 0))
  # RSE2_i = (0 - ideal share) / ideal share = -1: zero seats against a
  # positive ideal share is exactly 100% under-representation.
  expect_true(all(shut$RSE2_i == -1))
})

test_that("LHI matches a direct recomputation over ALL parties", {
  # Recomputed from the raw Seats and Votes columns (exact integers), so a
  # dropped shut-out party could not hide behind rounding.
  se <- sim_shut$Seat_Excess
  lhi_re <- vapply(split(se, se$ElectionID), function(d) {
    0.5 * sum(abs(d$Seats / 100 - d$Votes / sum(d$Votes)))
  }, numeric(1))
  lhi_re <- lhi_re[gtools::mixedorder(names(lhi_re))]
  expect_equal(as.numeric(sim_shut$Disproportionality_per_elec$LHI),
               unname(lhi_re), tolerance = 1e-3)
})

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
