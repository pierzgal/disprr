# Tests for the simplex-based vote-distribution options:
#   - taagepera_allik() expected-share vector
#   - sampleElectionData(dist = "dirichlet" / "uniform_simplex")
#
# All numeric expectations below are properties of the formula or of the
# distribution itself; no party-share numbers are hand-inserted, R computes
# both sides of every comparison.


# ---- taagepera_allik() ----

test_that("taagepera_allik returns a valid share vector for l = 2..10", {
  for (l in 2:10) {
    mu <- taagepera_allik(l)
    expect_length(mu, l)
    expect_equal(sum(mu), 1, tolerance = 1e-12,
                 info = paste("Sum != 1 at l =", l))
    expect_true(all(mu > 0),
                info = paste("Non-positive share at l =", l))
    expect_true(all(diff(mu) <= 0),
                info = paste("Not descending at l =", l))
  }
})

test_that("taagepera_allik mu[1] is approximately 1/sqrt(l)", {
  # The recursion uses mu[1] = 1/sqrt(l) exactly; the final mu / sum(mu)
  # normalisation makes only a floating-point-level adjustment.
  for (l in c(2, 3, 5, 7, 10)) {
    expect_equal(taagepera_allik(l)[1], 1 / sqrt(l), tolerance = 1e-10)
  }
})

test_that("taagepera_allik errors for l < 2 or non-numeric input", {
  expect_error(taagepera_allik(1), ">= 2")
  expect_error(taagepera_allik(0), ">= 2")
  expect_error(taagepera_allik("five"), "single integer")
  expect_error(taagepera_allik(c(3, 5)), "single integer")
})


# ---- sampleElectionData(dist = "dirichlet") ----

test_that("dirichlet path returns the documented sample structure", {
  s <- sampleElectionData(
    seed = 1, dist = "dirichlet",
    np = 4, nd = 2, ne = 5,
    phi = 20, votes_per_district = 10000,
    TS = 10, formula_dist = "hh"
  )
  expect_named(s, c("Votes_Dist_Party", "Seats_Dist", "Votes_Share_Party",
                    "Votes_Total_Dist", "Votes_Total_Party", "Votes_Total",
                    "Params"))
  expect_equal(dim(s$Votes_Dist_Party), c(4, 2, 5))
  expect_equal(s$Params, c(5, 2, 4, 10))
})

test_that("dirichlet vote shares sum to about 1 after floor()", {
  # Each district's draw is shares * votes_per_district, then floor.
  # The total in a district is therefore at most votes_per_district and at
  # most np units below it (one unit lost per party at worst).
  votes_per_district <- 10000
  np <- 5
  s <- sampleElectionData(
    seed = 7, dist = "dirichlet",
    np = np, nd = 3, ne = 4,
    phi = 25, votes_per_district = votes_per_district,
    TS = 12, formula_dist = "hh"
  )
  for (j in seq_len(4)) {
    for (i in seq_len(3)) {
      tot <- sum(s$Votes_Dist_Party[, i, j])
      expect_true(tot <= votes_per_district)
      expect_true(tot >= votes_per_district - np)
    }
  }
})

test_that("dirichlet draws are sorted ascending within each district", {
  s <- sampleElectionData(
    seed = 3, dist = "dirichlet",
    np = 6, nd = 2, ne = 3,
    phi = 30, votes_per_district = 50000,
    TS = 15, formula_dist = "hh"
  )
  for (j in seq_len(3)) {
    for (i in seq_len(2)) {
      v <- s$Votes_Dist_Party[, i, j]
      expect_true(all(diff(v) >= 0))
    }
  }
})


# ---- sampleElectionData(dist = "uniform_simplex") ----

test_that("uniform_simplex path returns the documented sample structure", {
  s <- sampleElectionData(
    seed = 2, dist = "uniform_simplex",
    np = 4, nd = 1, ne = 5,
    votes_per_district = 10000,
    TS = 10, formula_dist = "hh"
  )
  expect_equal(dim(s$Votes_Dist_Party), c(4, 1, 5))
})


# ---- Input validation ----

test_that("dirichlet rejects missing votes_per_district or phi", {
  expect_error(
    sampleElectionData(seed = 1, dist = "dirichlet",
                       np = 4, nd = 1, ne = 2,
                       votes_per_district = NULL,
                       TS = 10, formula_dist = "hh"),
    "votes_per_district"
  )
  expect_error(
    sampleElectionData(seed = 1, dist = "dirichlet",
                       np = 4, nd = 1, ne = 2,
                       phi = -1, votes_per_district = 10000,
                       TS = 10, formula_dist = "hh"),
    "phi"
  )
})

test_that("count-based dists still require 'max'", {
  expect_error(
    sampleElectionData(seed = 1, dist = "lnorm",
                       np = 4, nd = 1, ne = 2,
                       mean = 10, sd = 1.2,
                       TS = 10, formula_dist = "hh"),
    "'max' must be supplied"
  )
})


# ---- End-to-end through simulate_E ----

test_that("simulate_E runs with dist = 'dirichlet'", {
  e <- simulate_E(
    seed = 1, dist = "dirichlet",
    np = 4, nd = 1, ne = 10,
    mean = 10, sd = 1.2, rate = 1/25000, max = 100000,
    phi = 20, votes_per_district = 10000,
    TS = 10, formula = "dh", formula_dist = "hh",
    threshold = 0, threshold_country = 0
  )
  expect_named(e, c("Seat_Excess", "Apportionment",
                    "Disproportionality_per_elec", "Summary"))
  expect_equal(nrow(e$Disproportionality_per_elec), 10)
})

test_that("simulate_E runs with dist = 'uniform_simplex'", {
  e <- simulate_E(
    seed = 1, dist = "uniform_simplex",
    np = 4, nd = 1, ne = 10,
    mean = 10, sd = 1.2, rate = 1/25000, max = 100000,
    phi = 20, votes_per_district = 10000,
    TS = 10, formula = "sl", formula_dist = "hh",
    threshold = 0, threshold_country = 0
  )
  expect_equal(nrow(e$Disproportionality_per_elec), 10)
})
