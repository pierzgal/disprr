#' Divisor Methods
#'
#' Allocate seats to parties using a variaty of divisor methods.
#' @param parties A character vector for party labels in the same order as in the numerical vector of votes for parties. If NULL, 3-letter identifiers (e.g. 'XYZ') will be assigned.
#' @param votes A numeric vector of votes received by each party.
#' @param seats The number of seats to be apportioned.
#' @param method A character name for the method to be used (e.g. 'dh' = d'Hondt).
#' The following apportionment methods are available:
#' "dh" - d'Hondt method,
#' "sl" - Sainte-Lague method,
#' "msl" - Modified Sainte-Lague method,
#' "danish" - Danish modified Sainte-Lague method,
#' "hsl" - Hungarian modified Sainte-Lague method,
#' "imperiali" - The Italian Imperiali (not to be confused with the Imperiali Quota, which is a Largest remainder method),
#' "hh" - Huntington-Hill method,
#' "wb" - Webster's method,
#' "jef" - Jefferson's method,
#' "ad" - Adams's method,
#' "hb" - Hagenbach-Bischoff method.
#' @param threshold A numeric value between [0, 1]. Default is set to 0.
#' @param order_name A logical value. Sort rows of a dataframe with results by party name (alphabetically).
#'
#' @return A data.frame of length parties containing apportioned seats.
#' @examples
#' (...)
#' @export

divisorMethods <-
  function(parties = NULL,
           votes = NULL,
           seats = NULL,
           method = c("dh",
                      "sl",
                      "msl",
                      "danish",
                      "hsl",
                      "hh",
                      "imperiali",
                      "wb",
                      "jef",
                      "ad",
                      "hb"),
           threshold = 0,
           order_name = TRUE,
           ...) {
    .ratio <- votes / sum(votes)
    .votes <- ifelse(.ratio < threshold, 0, votes)

    if (sum(.votes) == 0)
      stop("Allocation cannot be done - all parties vote totals below electoral threshold.")

    if (length(parties) != length(votes))
      stop("Allocation error.")

    # Methods

    switch(
      method,
      dh = {
        #d'Hondt
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("d'Hondt")
      },
      sl = {
        #Sainte-Lague
        divisor.vec <- seq(from = 1,
                           by = 2,
                           length.out = seats)
        method.name <- c("Sainte-Lagu\u00EB")
      },
      msl = {
        #Modified Sainte-Lague
        divisor.vec <-
          c(1.4, seq(
            from = 3,
            by = 2,
            length.out = seats - 1
          ))
        method.name <- c("Modified Sainte-Lagu\u00EB")
      },
      danish = {
        #Danish
        divisor.vec <-
          c(1, seq(
            from = 4,
            by = 3,
            length.out = seats - 1
          ))
        method.name <- c("Danish Sainte-Lagu\u00EB")
      },
      hsl = {
        #Hungarian
        divisor.vec <-
          c(1.5, seq(
            from = 3,
            by = 2,
            length.out = seats - 1
          ))
        method.name <- c("Hungarian Sainte-Lagu\u00EB")
      },
      imperiali = {
        #Imperiali
        divisor.vec <-
          c(1, seq(
            from = 1.5,
            by = .5,
            length.out = seats - 1
          ))
        method.name <- c("Imperiali")
      },
      hh = {
        #Huntington-Hill Equal Proportions Method
        divisor.vec0 <- seq(from = 1,
                            by = 1,
                            length.out = seats)
        divisor.vec <- sqrt(divisor.vec0 * (divisor.vec0 - 1))
        method.name <- c("Hungtinton-Hill")
      },
      wb = {
        #Webster Major Fractions Method
        divisor.vec0 <- seq(from = 1,
                            by = 2,
                            length.out = seats)
        divisor.vec <- (divisor.vec0 + (divisor.vec0 - 1)) / 2
        method.name <- c("Webster")
      },
      jef = {
        #Jefferson Greatest Divisors or Hagenbach-Bischoff Method
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("Jefferson")
      },
      ad = {
        #Adam's Method Smallest Devisors
        divisor.vec <-
          c(0, seq(
            from = 1,
            by = 1,
            length.out = seats - 1
          ))
        method.name <- c("Adam's Method")
      },
      hb = {
        #Hagenbach-Bischoff Method
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("Hagenbach-Bischoff")
      }
    )

    .temp <- data.frame(parties = rep(parties, each = seats),
                        scores = as.vector(sapply(.votes, function(x)
                          x / divisor.vec)))

    out <- with(.temp, (parties[order(-scores)][1:seats]))

    TABLE <- table(out, useNA = c("no", "ifany", "always"))

    ptab <- base::prop.table(TABLE)
    names(TABLE)[is.na(names(TABLE))] <- "<NA>"


    percentages = TRUE

    output <- data.frame(
      class = names(TABLE),
      Freq = as.vector(TABLE[]),
      Prop = if (percentages == TRUE) {
        signif(as.vector(ptab[]), 3)
      } else {
        signif(as.vector(ptab[]), 3)
      }
    )

    names(output) <- c("Party", "Seats", "SeatShare")

    ## order by party name

    if (order_name == TRUE) {
      result <-
        data.frame(
          Party = parties,
          Votes = votes,
          VoteShare = votes / sum(votes)
        )

      output <- dplyr::left_join(output, result, by = "Party")
      return(output)

    }

    else {
      result <-
        data.frame(
          Party = parties,
          Votes = votes,
          VoteShare = votes / sum(votes)
        )

      output <- dplyr::left_join(result, output, by = "Party")

      # Reorder by column name
      output <-
        output[c("Party", "Seats", "SeatShare", "Votes", "VoteShare")]

      return(output)

    }
  }


#' Hamilton-Hare Largest Remainder Method
#'
#' Allocate seats to parties using Hamilton-Hare largest remainder apportionment method.
#' @param parties A character vector for party labels in the same order as in the numerical vector of votes for parties. If NULL, 3-letter identifiers (e.g. 'XYZ') will be assigned.
#' @param votes A numeric vector of votes received by each party.
#' @param seats The number of seats to be apportioned.
#' @param threshold A numeric value between [0, 1]. Default is set to 0.
#' @param order_name A logical value. Sort rows of a dataframe with results by party name (alphabetically).
#'
#' @return A data.frame of length parties containing apportioned seats.
#' @examples
#' (...)
#' @export

LR_Hamilton <- function(parties = NULL,
                        votes = NULL,
                        seats = NULL,
                        threshold = 0,
                        order_name = TRUE,
                        ...) {
  .ratio <- votes / sum(votes)
  .votes <- ifelse(.ratio < threshold, 0, votes)

  if (sum(.votes) == 0)
    stop("Allocation cannot be done - all parties vote totals below electoral threshold.")

  output <- data.frame(
    parties = parties,
    scores = .votes / sum(.votes) * seats,
    perc = signif(.votes / sum(.votes), 3)
  )

  integer <- with(output, floor(scores))

  fraction <- with(output, scores - integer)

  remainder <- seats - sum(integer)

  output[, 2] <- integer

  extra <-
    utils::head(order(fraction, decreasing = TRUE), remainder)

  output$scores[extra] <- (output$scores[extra] + 1)

  if (sum(output$scores) != seats)
    stop("Allocation error.")

  names(output) <- c("Party", "Seats", "SeatShare")


  if (order_name == TRUE) {
    output <- dplyr::arrange(output, Party)

    result <-
      data.frame(
        Party = parties,
        Votes = votes,
        VoteShare = votes / sum(votes)
      )

    output <- dplyr::left_join(output, result, by = "Party")
    return(output)

  }

  else {
    result <-
      data.frame(
        Party = parties,
        Votes = votes,
        VoteShare = votes / sum(votes)
      )

    output <- dplyr::left_join(result, output, by = "Party")

    # Reorder by column name
    output <-
      output[c("Party", "Seats", "SeatShare", "Votes", "VoteShare")]
    return(output)

  }
}
