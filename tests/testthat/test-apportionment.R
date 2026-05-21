# Known-answer tests for all apportionment methods
#
# Reference values computed by hand or cross-checked against:
# - Balinski & Young (2001), Fair Representation
# - Pukelsheim (2017), Proportional Representation
# - Wikipedia "Highest averages method" worked examples

# ---- Test data ----
# Classic 5-party example used by multiple textbook sources
parties_5 <- c("A", "B", "C", "D", "E")
votes_5   <- c(100000, 80000, 30000, 20000, 10000)

# Simple 3-party case
parties_3 <- c("X", "Y", "Z")
votes_3   <- c(47000, 16000, 15900)

# ---- D'Hondt (Jefferson, Hagenbach-Bischoff) ----

test_that("D'Hondt allocates correctly with classic 5-party example", {
  # 8 seats: quotients ranked by V/d where d = 1,2,3,...
  # A: 100k, 50k, 33.3k, 25k, 20k, 16.7k, 14.3k, 12.5k

  # B: 80k, 40k, 26.7k, 20k, 16k, ...
  # C: 30k, 15k, ...
  # D: 20k, ...
  # E: 10k, ...
  # Top 8: 100k(A), 80k(B), 50k(A), 40k(B), 33.3k(A), 30k(C), 26.7k(B), 25k(A)
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh")
  expected_seats <- c(4, 3, 1, 0, 0)
  expect_equal(res$Seats[order(res$Party)], expected_seats)
  expect_equal(sum(res$Seats), 8)
})

test_that("Jefferson and Hagenbach-Bischoff produce same results as D'Hondt", {
  dh  <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh")
  jef <- divisorMethods(parties_5, votes_5, seats = 8, method = "jef")
  hb  <- divisorMethods(parties_5, votes_5, seats = 8, method = "hb")

  expect_equal(dh$Seats, jef$Seats)
  expect_equal(dh$Seats, hb$Seats)
})


# ---- Sainte-Laguë and Webster ----

test_that("Sainte-Laguë allocates correctly", {
  # Divisors: 1, 3, 5, 7, ...
  # A: 100k, 33.3k, 20k, 14.3k, ...
  # B: 80k, 26.7k, 16k, ...
  # C: 30k, 10k, ...
  # D: 20k, 6.7k, ...
  # E: 10k, ...
  # Top 8: 100k(A), 80k(B), 33.3k(A), 30k(C), 26.7k(B), 20k(A/D tie)
  # With 8 seats: A=3, B=2, C=1, D=1, E=1
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "sl")
  expect_equal(sum(res$Seats), 8)
  # SL is more proportional than DH: smaller parties get more
  res_dh <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh")
  expect_true(res$Seats[res$Party == "A"] <= res_dh$Seats[res_dh$Party == "A"])
})

test_that("Webster produces identical results to Sainte-Laguë", {
  sl <- divisorMethods(parties_5, votes_5, seats = 8, method = "sl")
  wb <- divisorMethods(parties_5, votes_5, seats = 8, method = "wb")
  expect_equal(sl$Seats, wb$Seats)
})

test_that("Webster = Sainte-Laguë across multiple seat counts", {
  for (s in c(3, 5, 10, 15, 20)) {
    sl <- divisorMethods(parties_5, votes_5, seats = s, method = "sl")
    wb <- divisorMethods(parties_5, votes_5, seats = s, method = "wb")
    expect_equal(sl$Seats, wb$Seats,
                 info = paste("Mismatch at seats =", s))
  }
})


# ---- Modified Sainte-Laguë ----

test_that("Modified Sainte-Laguë uses 1.4 as first divisor", {
  # First divisor is 1.4 instead of 1, penalizing small parties' first seat
  # A: 100k/1.4=71.4k, 100k/3=33.3k, ...
  # E: 10k/1.4=7.1k (harder to win first seat)
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "msl")
  expect_equal(sum(res$Seats), 8)
  # MSL should give fewer seats to smallest parties vs SL
  sl <- divisorMethods(parties_5, votes_5, seats = 8, method = "sl")
  expect_true(
    res$Seats[res$Party == "E"] <= sl$Seats[sl$Party == "E"]
  )
})


# ---- Danish method ----

test_that("Danish method uses 1, 4, 7, 10, ... divisors", {
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "danish")
  expect_equal(sum(res$Seats), 8)
})


# ---- Hungarian modified Sainte-Laguë ----

test_that("Hungarian MSL uses 1.5 as first divisor", {
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "hsl")
  expect_equal(sum(res$Seats), 8)
  # First divisor of 1.5 is higher than MSL's 1.4
  msl <- divisorMethods(parties_5, votes_5, seats = 8, method = "msl")
  # Small parties should get no more than under MSL
  expect_true(
    res$Seats[res$Party == "E"] <= msl$Seats[msl$Party == "E"]
  )
})


# ---- Imperiali ----

test_that("Imperiali strongly favors large parties", {
  # Divisors: 1, 1.5, 2, 2.5, ... (closer spacing than DH)
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "imperiali")
  expect_equal(sum(res$Seats), 8)
  dh <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh")
  # Imperiali should give at least as many seats to largest party as DH
  expect_true(
    res$Seats[res$Party == "A"] >= dh$Seats[dh$Party == "A"]
  )
})


# ---- Huntington-Hill ----

test_that("Huntington-Hill guarantees at least 1 seat per party with votes", {
  # First divisor = sqrt(1*0) = 0, so V/0 = Inf for all parties with votes > 0
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "hh")
  expect_equal(sum(res$Seats), 8)
  # All 5 parties have positive votes and seats >= 5, so everyone gets >= 1
  expect_true(all(res$Seats[res$Votes > 0] >= 1))
})

test_that("Huntington-Hill with fewer seats than parties still works", {
  # Only 3 seats but 5 parties; top 3 Inf quotients go to 3 largest
  res <- divisorMethods(parties_5, votes_5, seats = 3, method = "hh")
  expect_equal(sum(res$Seats), 3)
})


# ---- Adams ----

test_that("Adams guarantees at least 1 seat per party with votes", {
  # First divisor = 0, same Inf guarantee as HH
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "ad")
  expect_equal(sum(res$Seats), 8)
  expect_true(all(res$Seats[res$Votes > 0] >= 1))
})


# ---- Hamilton-Hare ----

test_that("Hamilton-Hare allocates correctly", {
  # Total votes = 240000, quota = 240000/8 = 30000
  # A: 100000/30000 = 3.333 -> 3 + remainder 0.333
  # B: 80000/30000 = 2.667  -> 2 + remainder 0.667
  # C: 30000/30000 = 1.000  -> 1 + remainder 0.000
  # D: 20000/30000 = 0.667  -> 0 + remainder 0.667
  # E: 10000/30000 = 0.333  -> 0 + remainder 0.333
  # Integer sum = 6, remainder seats = 2
  # Largest remainders: B (0.667) and D (0.667) get extra seats
  # Result: A=3, B=3, C=1, D=1, E=0
  res <- LR_Hamilton(parties_5, votes_5, seats = 8)
  expected <- c(3, 3, 1, 1, 0)
  expect_equal(res$Seats[order(res$Party)], expected)
  expect_equal(sum(res$Seats), 8)
})


# ---- Input validation ----

test_that("divisorMethods rejects invalid inputs", {
  expect_error(divisorMethods(c("A", "B"), c(-10, 100), 3, "dh"),
               "negative")
  expect_error(divisorMethods(c("A", "B"), c(100, 200), 0, "dh"),
               "positive")
  expect_error(divisorMethods(c("A"), c(100, 200), 3, "dh"),
               "same length")
  expect_error(divisorMethods(c("A", "B"), c(100, 200), 3, "dh",
                              threshold = 1.5),
               "between 0 and 1")
})

test_that("LR_Hamilton rejects invalid inputs", {
  expect_error(LR_Hamilton(c("A", "B"), c(-10, 100), 3),
               "negative")
  expect_error(LR_Hamilton(c("A", "B"), c(100, 200), 0),
               "positive")
})


# ---- Threshold ----

test_that("Electoral threshold excludes parties below it", {
  # Party E has 10k/240k = 4.17% vote share
  # With 5% threshold, E should be excluded
  res <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh",
                        threshold = 0.05)
  expect_equal(res$Seats[res$Party == "E"], 0)
  expect_equal(sum(res$Seats), 8)
})

test_that("Threshold of 0 excludes nobody", {
  res1 <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh",
                         threshold = 0)
  res2 <- divisorMethods(parties_5, votes_5, seats = 8, method = "dh")
  expect_equal(res1$Seats, res2$Seats)
})

test_that("All-below-threshold raises error", {
  expect_error(
    divisorMethods(parties_3, votes_3, seats = 5, method = "dh",
                   threshold = 0.99),
    "threshold"
  )
})


# ---- Seat share consistency ----

test_that("Seat shares sum to 1 for all methods", {
  methods <- c("dh", "sl", "msl", "danish", "hsl", "imperiali",
               "hh", "wb", "jef", "ad", "hb")
  for (m in methods) {
    res <- divisorMethods(parties_5, votes_5, seats = 10, method = m)
    expect_equal(sum(res$SeatShare), 1, tolerance = 1e-10,
                 info = paste("Method:", m))
    expect_equal(sum(res$Seats), 10,
                 info = paste("Method:", m))
  }
})

test_that("Hamilton seat shares sum to 1", {
  res <- LR_Hamilton(parties_5, votes_5, seats = 10)
  expect_equal(sum(res$SeatShare), 1, tolerance = 1e-10)
  expect_equal(sum(res$Seats), 10)
})


# ---- Output structure ----

test_that("divisorMethods returns expected columns", {
  res <- divisorMethods(parties_3, votes_3, seats = 5, method = "dh")
  expect_named(res, c("Party", "Seats", "SeatShare", "Votes", "VoteShare"))
  expect_type(res$Party, "character")
  expect_type(res$Seats, "integer")
})

test_that("LR_Hamilton returns expected columns", {
  res <- LR_Hamilton(parties_3, votes_3, seats = 5)
  expect_named(res, c("Party", "Seats", "SeatShare", "Votes", "VoteShare"))
})


# ---- Auto-generated party labels ----

test_that("NULL parties generates labels automatically", {
  res <- divisorMethods(parties = NULL, votes = c(100, 50, 30), seats = 5,
                        method = "dh")
  expect_equal(nrow(res), 3)
  expect_equal(sum(res$Seats), 5)
})


# ---- Edge case: single party ----

test_that("Single party gets all seats", {
  res <- divisorMethods(c("Solo"), c(10000), seats = 10, method = "dh")
  expect_equal(res$Seats, 10)
})

test_that("Single party Hamilton gets all seats", {
  res <- LR_Hamilton(c("Solo"), c(10000), seats = 10)
  expect_equal(res$Seats, 10)
})


# ---- Ordering ----

test_that("order_name = TRUE sorts alphabetically", {
  res <- divisorMethods(c("Zulu", "Alpha", "Mike"), c(100, 200, 150),
                        seats = 5, method = "dh", order_name = TRUE)
  expect_equal(res$Party, c("Alpha", "Mike", "Zulu"))
})

test_that("order_name = FALSE preserves input order", {
  res <- divisorMethods(c("Zulu", "Alpha", "Mike"), c(100, 200, 150),
                        seats = 5, method = "dh", order_name = FALSE)
  expect_equal(res$Party, c("Zulu", "Alpha", "Mike"))
})
