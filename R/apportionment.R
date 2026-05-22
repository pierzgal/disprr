#' Divisor Methods for Seat Apportionment
#'
#' Allocate seats to parties using a variety of divisor methods.
#'
#' @param parties A character vector of party labels, same length as \code{votes}.
#'   If \code{NULL}, 3-letter identifiers are generated automatically.
#' @param votes A numeric vector of non-negative votes received by each party.
#' @param seats A positive integer: the number of seats to apportion.
#' @param method A character string selecting the divisor method. Available methods:
#'   \describe{
#'     \item{\code{"dh"}}{D'Hondt (divisors: 1, 2, 3, \ldots). Equivalent to
#'       Jefferson (\code{"jef"}) and Hagenbach-Bischoff (\code{"hb"}).}
#'     \item{\code{"sl"}}{Sainte-Laguë (divisors: 1, 3, 5, \ldots).
#'       Mathematically equivalent to Webster (\code{"wb"}), which uses
#'       divisors 0.5, 1.5, 2.5, \ldots (a constant rescaling that does not
#'       affect the ranking of quotients).}
#'     \item{\code{"msl"}}{Modified Sainte-Laguë (first divisor 1.4,
#'       then 3, 5, 7, \ldots). Used in Scandinavian countries.}
#'     \item{\code{"danish"}}{Danish modified Sainte-Laguë (divisors:
#'       1, 4, 7, 10, \ldots).}
#'     \item{\code{"hsl"}}{Hungarian modified Sainte-Laguë (first divisor
#'       1.5, then 3, 5, 7, \ldots).}
#'     \item{\code{"imperiali"}}{Imperiali divisor method (divisors: 1, 1.5, 2,
#'       2.5, \ldots). Not to be confused with the Imperiali quota (a largest
#'       remainder method).}
#'     \item{\code{"hh"}}{Huntington-Hill / Equal Proportions (divisors:
#'       \eqn{\sqrt{n(n-1)}} for \eqn{n = 1, 2, \ldots}). Used for U.S. House
#'       apportionment. The first divisor is 0, guaranteeing at least 1 seat to
#'       every party with positive votes.}
#'     \item{\code{"wb"}}{Webster / Major Fractions (divisors: 0.5, 1.5, 2.5,
#'       \ldots). Produces identical results to Sainte-Laguë (\code{"sl"}).}
#'     \item{\code{"jef"}}{Jefferson / Greatest Divisors (divisors: 1, 2, 3,
#'       \ldots). Identical to D'Hondt (\code{"dh"}).}
#'     \item{\code{"ad"}}{Adams / Smallest Divisors (divisors: 0, 1, 2, 3,
#'       \ldots). Most favorable to small parties. The first divisor is 0,
#'       guaranteeing at least 1 seat to every party with positive votes.}
#'     \item{\code{"hb"}}{Hagenbach-Bischoff (divisors: 1, 2, 3, \ldots).
#'       Identical to D'Hondt (\code{"dh"}).}
#'   }
#' @param threshold A numeric value in \code{[0, 1]}. Parties whose vote share
#'   is strictly below \code{threshold} are excluded. Default is 0 (no threshold).
#' @param order_name Logical. If \code{TRUE} (default), output rows are sorted
#'   alphabetically by party name; if \code{FALSE}, by original input order.
#'
#' @details
#' When two or more quotients are exactly equal (a tie for the last seat),
#' the seat is awarded to the party with the larger vote total. This matters
#' chiefly for Huntington-Hill (\code{"hh"}) and Adams (\code{"ad"}), whose
#' first divisor is 0: every party with positive votes then has an infinite
#' first quotient, and the vote-based tie-break ensures that, when seats are
#' scarcer than parties, they go to the largest parties rather than to
#' whichever party happened to appear first in the input.
#'
#' @return A \code{data.frame} with columns:
#'   \code{Party}, \code{Seats}, \code{SeatShare}, \code{Votes}, \code{VoteShare}.
#'
#' @examples
#' divisorMethods(
#'   parties = c("A", "B", "C"),
#'   votes = c(100000, 80000, 30000),
#'   seats = 8,
#'   method = "dh"
#' )
#'
#' @export
divisorMethods <- function(parties = NULL,
                           votes = NULL,
                           seats = NULL,
                           method = c("dh", "sl", "msl", "danish", "hsl",
                                      "hh", "imperiali", "wb", "jef", "ad", "hb"),
                           threshold = 0,
                           order_name = TRUE) {
  method <- match.arg(method)

  ## --- Input validation ---
  if (is.null(votes) || length(votes) == 0L)
    stop("'votes' must be a non-empty numeric vector.")
  if (any(votes < 0))
    stop("'votes' must not contain negative values.")
  if (is.null(seats) || seats < 1L)
    stop("'seats' must be a positive integer.")
  seats <- as.integer(seats)

  if (is.null(parties)) {
    parties <- if.parties.null(length(votes))
  }
  if (length(parties) != length(votes))
    stop("'parties' and 'votes' must have the same length.")
  if (threshold < 0 || threshold > 1)
    stop("'threshold' must be between 0 and 1.")

  ## --- Apply threshold ---
  vote_shares <- votes / sum(votes)
  eligible <- vote_shares >= threshold
  working_votes <- ifelse(eligible, votes, 0)

  if (sum(working_votes) == 0)
    stop("All parties are below the electoral threshold; no allocation possible.")

  ## --- Build divisor vector ---
  divisor_vec <- .build_divisors(method, seats)

  ## --- Compute quotients and allocate ---
  quotient_mat <- outer(working_votes, divisor_vec, "/")
  party_idx <- rep(seq_along(parties), times = seats)
  all_quotients <- as.vector(quotient_mat)

  ## Rank quotients; ties (notably the infinite first quotients of "hh"/"ad")
  ## are broken in favour of the party with more votes.
  tie_break <- working_votes[party_idx]
  winners <- party_idx[
    order(all_quotients, tie_break, decreasing = TRUE)
  ][seq_len(seats)]
  seat_counts <- tabulate(winners, nbins = length(parties))

  ## --- Assemble output ---
  total_seats <- sum(seat_counts)
  output <- data.frame(
    Party = parties,
    Seats = seat_counts,
    SeatShare = if (total_seats > 0) seat_counts / total_seats else 0,
    Votes = votes,
    VoteShare = vote_shares,
    stringsAsFactors = FALSE
  )

  if (order_name) {
    output <- output[order(output$Party), ]
  }

  rownames(output) <- NULL
  output
}


#' Hamilton-Hare Largest Remainder Method
#'
#' Allocate seats to parties using the Hamilton-Hare largest remainder method.
#'
#' @inheritParams divisorMethods
#'
#' @return A \code{data.frame} with columns:
#'   \code{Party}, \code{Seats}, \code{SeatShare}, \code{Votes}, \code{VoteShare}.
#'
#' @examples
#' LR_Hamilton(
#'   parties = c("A", "B", "C"),
#'   votes = c(100000, 80000, 30000),
#'   seats = 8
#' )
#'
#' @export
LR_Hamilton <- function(parties = NULL,
                        votes = NULL,
                        seats = NULL,
                        threshold = 0,
                        order_name = TRUE) {
  ## --- Input validation ---
  if (is.null(votes) || length(votes) == 0L)
    stop("'votes' must be a non-empty numeric vector.")
  if (any(votes < 0))
    stop("'votes' must not contain negative values.")
  if (is.null(seats) || seats < 1L)
    stop("'seats' must be a positive integer.")
  seats <- as.integer(seats)

  if (is.null(parties)) {
    parties <- if.parties.null(length(votes))
  }
  if (length(parties) != length(votes))
    stop("'parties' and 'votes' must have the same length.")
  if (threshold < 0 || threshold > 1)
    stop("'threshold' must be between 0 and 1.")

  ## --- Apply threshold ---
  vote_shares <- votes / sum(votes)
  eligible <- vote_shares >= threshold
  working_votes <- ifelse(eligible, votes, 0)
  total_working <- sum(working_votes)

  if (total_working == 0)
    stop("All parties are below the electoral threshold; no allocation possible.")

  ## --- Hamilton allocation ---
  quotas <- working_votes / total_working * seats
  integer_parts <- as.integer(floor(quotas))
  remainders <- quotas - integer_parts
  seats_remaining <- seats - sum(integer_parts)

  if (seats_remaining > 0L) {
    extra <- order(remainders, decreasing = TRUE)[seq_len(seats_remaining)]
    integer_parts[extra] <- integer_parts[extra] + 1L
  }

  if (sum(integer_parts) != seats)
    stop("Allocation error: total seats assigned does not equal requested seats.")

  ## --- Assemble output ---
  total_seats <- sum(integer_parts)
  output <- data.frame(
    Party = parties,
    Seats = integer_parts,
    SeatShare = if (total_seats > 0) integer_parts / total_seats else 0,
    Votes = votes,
    VoteShare = vote_shares,
    stringsAsFactors = FALSE
  )

  if (order_name) {
    output <- output[order(output$Party), ]
  }

  rownames(output) <- NULL
  output
}


# ---- Internal helpers for divisor construction ----

#' Build the divisor vector for a given method
#' @keywords internal
.build_divisors <- function(method, seats) {
  switch(
    method,
    dh = , jef = , hb = {
      seq(from = 1, by = 1, length.out = seats)
    },
    sl = {
      seq(from = 1, by = 2, length.out = seats)
    },
    wb = {
      seq(from = 0.5, by = 1, length.out = seats)
    },
    msl = {
      c(1.4, seq(from = 3, by = 2, length.out = seats - 1L))
    },
    danish = {
      c(1, seq(from = 4, by = 3, length.out = seats - 1L))
    },
    hsl = {
      c(1.5, seq(from = 3, by = 2, length.out = seats - 1L))
    },
    imperiali = {
      c(1, seq(from = 1.5, by = 0.5, length.out = seats - 1L))
    },
    hh = {
      n <- seq_len(seats)
      sqrt(n * (n - 1))
    },
    ad = {
      c(0, seq(from = 1, by = 1, length.out = seats - 1L))
    }
  )
}
