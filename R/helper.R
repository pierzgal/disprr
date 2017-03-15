#' Threshold at a Country Level
#'
#' A helper function that allows to apply an electoral threshold at a country level.
#' @return The function returns modified sample of election data. The number of votes of parties with fractions of votes not greater than a specified 'threshold' are set to 0, before an apportionment method is applied.
#' @keywords internal

.countryThreshold <-
  function (np, ne, nd, threshold_country, sample) {
    if (nd > 1) {
      rsum = list()
      tsum = list()
      rel_rsum = list()

      for (i in seq(1, ne, by = 1)) {
        rsum[[i]] <- apply(sample[[1]][, , i], 1, sum)
        tsum[[i]] <- sum(sample[[1]][, , i])

        rel_rsum[[i]] <- rsum[[i]] / tsum[[i]]
      }


      for (j in seq(1, ne, by = 1)) {
        for (i in seq(1, np, by = 1)) {
          ifelse(rel_rsum[[j]][i] <= threshold_country, sample[[1]][i, , j] <-
                   0, sample[[1]][i, , j])
        }
      }

      out <- sample

    }
    else {
      for (i in seq(1, ne, by = 1)) {
        rsum[[i]] <- sample[[1]][, , i]
        tsum[[i]] <- sum(sample[[1]][, , i])

        rel_rsum[[i]] <- rsum[[i]] / tsum[[i]]
      }

      for (j in seq(1, ne, by = 1)) {
        for (i in seq(1, np, by = 1)) {
          ifelse(rel_rsum[[j]][i] <= threshold_country, sample[[1]][i, , j] <-
                   0, sample[[1]][i, , j])
        }
      }

    }

    out <- sample

  }


#' Compute 'Ideal' Shares of Seats
#'
#' A helper function that computes so-called 'ideal' shares of seats at a country level. If # of districts = 1, a country level = a district level.
#' #' @param np A numeric value for the number of parties.
#' @param ne A numeric value for the number of elections.
#' @param nd A numeric value for the number of electoral districts.
#' @param np A numeric value for the number of parties.
#' @param sample A list generated using 'sampleElectionData' function.
#'
#' @return A data frame with 'ideal' shares of seats for parties at a country level.
#' @keywords internal

.seatsIdeal <- function (ne, nd, np, sample) {
  result <- vector("list", ne)
  out1 <- list()
  out2 <- list()

  for (j in seq(1, ne, by = 1)) {
    out1[[j]] = sample[[3]][[j]] * sum(sample[[2]][[j]])
  } # SeatShareIdeal

  for (j in seq(1, ne, by = 1)) {
    out2[[j]] =  sample[[3]][[j]] * 100
  }

  result <- list(SeatTotalIdeal = out1, SeatShareIdeal = out2)
  result


  result2 = vector("list", ne)

  for (i in seq(1, ne, by = 1)) {
    result2[[i]] <-
      data.frame(
        elec = paste("e", i, sep = ""),
        SeatTotalIdeal = round(result$SeatTotalIdeal[[i]], 4),
        SeatShareIdeal = round(result$SeatShareIdeal[[i]], 4)
      )
  }


  for (i in seq(1, ne, by = 1)) {
    result2[[i]] <- dplyr::mutate(result2[[i]], Party = if.parties.null(np))
    result2[[i]] <- dplyr::arrange(result2[[i]], desc(SeatShareIdeal))
  }

  result2
  result2 <- dplyr::bind_rows(result2)
  out <- result2

  return(out)

}


#' Country-level Election Outcomes under Proportional Representation
#'
#' The function allocates seats to parties using a variaty of divisor methods at a district-level, then it combines results over all district and returns a data frame of country-level election outcomes.
#' @param sample A list generated using 'sampleElectionData' function.
#' @param formula A character name for the apportionment method to be used (e.g. 'dh' = d'Hondt).
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
#' @param threshold_country A numeric value between [0, 1] for a threshold at a country level. Default is set to 0.
#'
#' @return A data frame containing country-level election outcomes, including the apportionment of seats.
#' @keywords internal

.ProportionalRepresentation <-
  function (sample,
            formula,
            threshold = 0,
            threshold_country = 0) {
    ne <- sample$Params[1]
    nd <- sample$Params[2]
    np <- sample$Params[3]

    out <- vector("list", ne)
    x <- vector("list", ne)
    e <- vector("list", ne)


    ############ TRY-CATCH

    # (...)

    ############

    {
      no_seats_dist <- unlist(sample$Seats_Dist)

      if (0 %in% no_seats_dist)  {
        stop("There are districts with 0 seats.")
      }

      else {
        ### .countryThreshold at a country level enabled

        if ((threshold_country > 0) & (threshold == 0))  {
          sample_mod <-
            .countryThreshold(np, ne, nd, threshold_country, sample) # apply a threshold at a country level - mod sample

          if (formula == "hamilton")

          {
            for (j in seq(1, ne, by = 1)) {
              for (i in seq(1, nd, by = 1)) {
                out[[j]][[i]] <-
                  LR_Hamilton(
                    parties = if.parties.null(np),
                    votes = sample_mod[[1]][, i, j],
                    seats = sample_mod[[2]][[j]][i],
                    threshold = 0,
                    order_name = T
                  )
              }
            }

            out

            for (i in seq(1, ne, by = 1)) {
              for (j in  seq(1, nd, by = 1)) {
                x[[i]][[j]] <- dplyr::mutate(
                  out[[i]][[j]],
                  id = paste("e", i, "d", j, sep = ""),
                  elec = paste("e", i, sep = ""),
                  dist = paste("d", j, sep = ""),
                  distTS = sample[[2]][[i]][j]
                )
                e[[i]][[j]] <- dplyr::select(x[[i]][[j]],
                                             Party,
                                             Seats,
                                             SeatShare,
                                             Votes,
                                             VoteShare,
                                             id,
                                             elec,
                                             dist,
                                             distTS)

              }
            }

          }

          else {
            for (j in seq(1, ne, by = 1)) {
              for (i in seq(1, nd, by = 1)) {
                out[[j]][[i]] <-
                  divisorMethods(
                    parties = if.parties.null(np),
                    votes = sample_mod[[1]][, i, j],
                    seats = sample_mod[[2]][[j]][i],
                    method = formula,
                    threshold = 0,
                    order_name = T
                  )
              }
            }

            out

            for (i in seq(1, ne, by = 1)) {
              for (j in  seq(1, nd, by = 1)) {
                x[[i]][[j]] <- dplyr::mutate(
                  out[[i]][[j]],
                  id = paste("e", i, "d", j, sep = ""),
                  elec = paste("e", i, sep = ""),
                  dist = paste("d", j, sep = ""),
                  distTS = sample[[2]][[i]][j]
                )
                e[[i]][[j]] <- dplyr::select(x[[i]][[j]],
                                             Party,
                                             Seats,
                                             SeatShare,
                                             Votes,
                                             VoteShare,
                                             id,
                                             elec,
                                             dist,
                                             distTS)

              }
            }

          }


          out <- dplyr::bind_rows(lapply(e,  FUN = bind_rows))
          out

        }

        ### .countryThreshold at a country level disabled
        else {
          if (formula == "hamilton")

          {
            for (j in seq(1, ne, by = 1)) {
              for (i in seq(1, nd, by = 1)) {
                out[[j]][[i]] <-
                  LR_Hamilton(
                    parties = if.parties.null(np),
                    votes = sample[[1]][, i, j],
                    seats = sample[[2]][[j]][i],
                    threshold = threshold,
                    order_name = T
                  )
              }
            }

            out

            for (i in seq(1, ne, by = 1)) {
              for (j in  seq(1, nd, by = 1)) {
                x[[i]][[j]] <- dplyr::mutate(
                  out[[i]][[j]],
                  id = paste("e", i, "d", j, sep = ""),
                  elec = paste("e", i, sep = ""),
                  dist = paste("d", j, sep = ""),
                  distTS = sample[[2]][[i]][j]
                )
                e[[i]][[j]] <- select(x[[i]][[j]],
                                      Party,
                                      Seats,
                                      SeatShare,
                                      Votes,
                                      VoteShare,
                                      id,
                                      elec,
                                      dist,
                                      distTS)

              }
            }

          }

          else {
            for (j in seq(1, ne, by = 1)) {
              for (i in seq(1, nd, by = 1)) {
                out[[j]][[i]] <-
                  divisorMethods(
                    parties = if.parties.null(np),
                    votes = sample[[1]][, i, j],
                    seats = sample[[2]][[j]][i],
                    method = formula,
                    threshold = threshold,
                    order_name = T
                  )
              }
            }

            out

            for (i in seq(1, ne, by = 1)) {
              for (j in  seq(1, nd, by = 1)) {
                x[[i]][[j]] <- dplyr::mutate(
                  out[[i]][[j]],
                  id = paste("e", i, "d", j, sep = ""),
                  elec = paste("e", i, sep = ""),
                  dist = paste("d", j, sep = ""),
                  distTS = sample[[2]][[i]][j]
                )
                e[[i]][[j]] <- select(x[[i]][[j]],
                                      Party,
                                      Seats,
                                      SeatShare,
                                      Votes,
                                      VoteShare,
                                      id,
                                      elec,
                                      dist,
                                      distTS)

              }
            }

          }


          ### Result
          out <- dplyr::bind_rows(lapply(e,  FUN = dplyr::bind_rows))
          out

        }

      }

    } # Stop

  }
