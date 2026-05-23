# Regression test: simulate_E() must return Seat_Excess and
# Disproportionality_per_elec with rows in NATURAL election order
# (e1, e2, ..., e50), not the lexicographic order that aggregate / merge /
# split produce on string keys (e1, e10, e11, ..., e5, e50, e6, ..., e9).
# Numerical correctness is unaffected; this is purely a row-order guarantee
# that users depend on when printing or iterating over these tables.
# Matches the v0.10.6 fix in the disprr shiny app sibling.

test_that("simulate_E returns rows in natural election order", {
  e <- simulate_E(
    seed = 1, dist = "lnorm",
    np = 4, nd = 1, ne = 50,
    mean = 12, sd = 1.5, rate = 1 / 25000, max = 500000,
    TS = 50, formula = "dh", formula_dist = "hh",
    threshold = 0, threshold_country = 0
  )
  natural <- paste0("e", 1:50)

  expect_identical(
    as.character(e$Disproportionality_per_elec$ElectionID),
    natural
  )
  expect_identical(
    unique(as.character(e$Seat_Excess$ElectionID)),
    natural
  )
})
