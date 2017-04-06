#' If Parties NULL
#'
#' Create 3-letter identifiers for parties if 'parties' parameter in 'divisorMethods' or 'LR.Hamilton' functions is NULL.
#' @return 3-letter identifiers for parties.
#' @examples
#' (...)
#' @export

if.parties.null <- function(x) {
  set.seed(1)
  parties <- replicate(x,
                       paste(sample(LETTERS, 3,
                                    replace = TRUE), collapse = ""))
  parties
}

#' Sample Election Data
#'
#' Return a pseudorandom sample (from a truncated log-normal distribution) of n election results using a variaty of paremeters such as: the number of parties (np), the total number of seats (TS) or the number of electoral districts.
#' @param seed A scalar value (a random seed) used to initialize a pseudorandom number generator.
#' @param dist A character value for a probability distribution function used to sample data. 'Uniform' (default) = 'uniform', 'Exponential' = 'exp' and 'Log-Normal' = 'lnorm' distributions are available.
#' @param np A numeric value for the number of parties.
#' @param nd A numeric value for the number of electoral districts.
#' @param ne A numeric value for the number of elections.
#' @param mean A numeric value for the location parameter (a log-normal distribution).
#' @param sd A numeric value for the scale parameter (a log-normal distribution).
#' @param rate A numeric value for the rate parameter (an exponential distribution).
#' @param max A numeric value for the upper limit of the domain for a truncated log-normal distribution used to sample votes for parties at the constituency level.
#' @param TS A numeric value for the total number of seats apportioned among electoral district (if nd (the number of district) > 1), using a selected electoral formula.
#' @param formula_dist A character value that specifies the apportionment method used to divide seats ('TS') among electoral districts.
#'
#' @return A list containing:
#' Votes_Dist_Party,
#' Seats_Dist,
#' Votes_Share_Party,
#' Votes_Total_Dist,
#' Votes_Total_Party,
#' Votes_Total.
#' @examples
#' (...)
#' @export

sampleElectionData <-
  function (seed,
            dist = "uniform",
            np,
            nd,
            ne,
            mean = NULL,
            sd = NULL,
            rate = NULL,
            max,
            TS,
            formula_dist,
            ...) {
    set.seed(seed)

    x = array(dim = c(np, nd, ne))
    csum = list()
    rsum = list()
    tsum = list()
    seats_dist = list()
    votes_share = list()

    switch (dist,
            uniform = {
              for (j in seq(1, ne, by = 1)) {
                for (i in seq(1, nd, by = 1)) {
                  x[, i, j] <- sort(floor(runif(
                    np, min = 0, max = max
                  )))
                }

              }

            },
            lnorm = {
              for (j in seq(1, ne, by = 1)) {
                for (i in seq(1, nd, by = 1)) {
                  x[, i, j] <-
                    sort(floor(
                      truncdist::rtrunc(
                        np,
                        spec = "lnorm",
                        a = 0,
                        b = max,
                        meanlog = mean,
                        sdlog = sd
                      )
                    ))
                }

              }

            },
            exp = {
              for (j in seq(1, ne, by = 1)) {
                for (i in seq(1, nd, by = 1)) {
                  x[, i, j] <-
                    sort(floor(
                      truncdist::rtrunc(
                        np,
                        spec = "exp",
                        a = 0,
                        b = max,
                        rate = rate
                      )
                    ))
                }

              }

            })


    if (nd > 1) {
      for (i in seq(1, ne, by = 1)) {
        csum[[i]] <- apply(x[, , i], 2, sum)
      }

      for (i in seq(1, ne, by = 1)) {
        rsum[[i]] <- apply(x[, , i], 1, sum)
      }

      for (i in seq(1, ne, by = 1)) {
        tsum[[i]] <- sum(x[, , i])
      }

    }

    else {
      for (i in seq(1, ne, by = 1)) {
        csum[[i]] <- sum(x[, , i])
      }

      for (i in seq(1, ne, by = 1)) {
        rsum[[i]] <- x[, , i]
      }

      for (i in seq(1, ne, by = 1)) {
        tsum[[i]] <- sum(x[, , i])
      }

    }

    # Use apportionment functions with order_name = FALSE
    switch (
      formula_dist,
      hamilton = {
        for (i in seq(1, ne, by = 1)) {
          seats_dist[[i]] <-
            Hamilton3(
              parties = if.parties.null(nd),
              votes = 1 * csum[[i]],
              seats = TS,
              order_name = F
            )[[2]]
        }
      },
      ad = {
        for (i in seq(1, ne, by = 1)) {
          seats_dist[[i]] <-
            divisorMethods(
              parties = if.parties.null(nd),
              votes = 1 * csum[[i]],
              seats = TS,
              method = "ad",
              order_name = F
            )[[2]]
        }
      },
      dh = {
        for (i in seq(1, ne, by = 1)) {
          seats_dist[[i]] <-
            divisorMethods(
              parties = if.parties.null(nd),
              votes = 1 * csum[[i]],
              seats = TS,
              method = "dh",
              order_name = F
            )[[2]]
        }
      },
      hh = {
        for (i in seq(1, ne, by = 1)) {
          seats_dist[[i]] <-
            divisorMethods(
              parties = if.parties.null(nd),
              votes = 1 * csum[[i]],
              seats = TS,
              method = "hh",
              order_name = F
            )[[2]]
        }
      }
    )

    for (i in seq(1, ne, by = 1)) {
      votes_share[[i]] <- rsum[[i]] / tsum[[i]]
    }

    sample <-
      list(
        Votes_Dist_Party = x,
        Seats_Dist = seats_dist,
        Votes_Share_Party = votes_share,
        Votes_Total_Dist = csum,
        Votes_Total_Party = rsum,
        Votes_Total = tsum,
        Params = c(ne, nd, np, TS)
      )

    out <- sample

  }

# --------------------


#' Simulate Elections under Proportional Representation
#'
#' The function simulates election results and computes a variaty of disproportionality measures.
#'
#' @return A list containg the following data objects:
#' Seat_Excess,
#' Apportionment,
#' Disproportionality_per_elec,
#' Summary.
#' @examples
#' (...)
#' @export

simulate_E <-
  function (seed,
            dist = "lnorm",
            np,
            nd,
            ne,
            mean,
            sd,
            rate,
            max,
            TS,
            formula,
            formula_dist,
            threshold,
            threshold_country,
            ...) {
    set.seed(seed)

    sample <-
      sampleElectionData(seed, dist, np, nd, ne, mean, sd, rate, max, TS, formula_dist)


    # Seat apportionment per district
    # Return list (Party Seats SeatShare Votes VoteShare id elec dist distTS)

    apportionment <-
      .ProportionalRepresentation(sample, formula, threshold, threshold_country)

    apportionment$Party <- as.character(apportionment$Party)

    # Determine number of votes by election
    # Return df (VoteShareTotalParty VotesTotalParty elec Party)

    {
      vote_share <- vector("list", ne)

      for (i in seq(1, ne, by = 1)) {
        vote_share[[i]] = data.frame(
          VoteShareTotalParty = round(sample$Votes_Share_Party[[i]] * 100, 4),
          VotesTotalParty = as.integer(sample$Votes_Total_Party[[i]]),
          elec = paste("e", i, sep = "")
        )
      }

      out <- vote_share
      out


      for (i in seq(1, ne, by = 1)) {
        out[[i]] <- dplyr::mutate(out[[i]], Party = if.parties.null(np))
        out[[i]] <-
          dplyr::arrange(out[[i]], desc(VoteShareTotalParty))
      }

      vote_share <- data.table::rbindlist(out)
    }

    apportionment <-
      dplyr::left_join(apportionment, vote_share, by = c("Party", "elec"))


    ## Group districts in elections

    apportionment_sum1 <-
      dplyr::group_by(apportionment, elec, Party)
    apportionment_sum2 <-
      dplyr::summarise(
        apportionment_sum1,
        seats = sum(Seats),
        seat_perc = sum(Seats) / TS * 100,
        distTS = distTS[1],
        TS = TS[1]
      )
    apportionment_sum3 <-
      dplyr::left_join(apportionment_sum2, vote_share, by = c("Party", "elec"))


    ## Compute 'ideal' shares of seats
    seats_ideal <- .seatsIdeal(ne, nd, np, sample)

    ## Merge
    merged <-
      dplyr::left_join(apportionment_sum3, seats_ideal, by = c("Party", "elec"))


    ## Seat excesses
    seat_excess <-
      dplyr::mutate(
        merged,
        Party = as.factor(Party),
        #        SE1_i _2 = signif(seats - VoteShareTotalParty / 100 * distTS, 2),
        SE1_i = signif(seats - VoteShareTotalParty / 100 * TS, 2),
        SE2_i = signif(seat_perc / 100 - VoteShareTotalParty / 100, 2),
        SE2_i_pp = signif(seat_perc - SeatShareIdeal, 2),
        SeatShare = signif(seat_perc / 100, 2),
        VoteShare = signif(VoteShareTotalParty / 100, 2),
        StandardQuota = (SeatShareIdeal / 100) * TS,
        RSE2_i = signif((seat_perc - SeatShareIdeal) / SeatShareIdeal, 2)
      )

    seat_excess <-
      dplyr::select(
        seat_excess,
        PartyID = Party,
        ElectionID = elec,
        Seats = seats,
        SeatShare,
        Votes = VotesTotalParty,
        VoteShare,
        SQ = StandardQuota,
        SE1_i,
        SE2_i,
        SE2_i_pp,
        RSE2_i
      )
    seat_excess <-
      dplyr::mutate(seat_excess, Seats = as.integer(Seats))


    ### Disproportionality indexes ###
    disp <- dplyr::group_by(seat_excess, ElectionID)
    disp <-
      dplyr::summarise(
        disp,
        meanRSE2 = signif(sum(abs(RSE2_i)) / np, digits = 2),
        LHI = signif(1 / 2 * sum(abs(
          SeatShare - VoteShare
        )), digits = 2),
        GHI = signif(sqrt(1 / 2 * sum((SeatShare - VoteShare) ^ 2
        )), digits = 2),
        SLI = signif(sum((
          SeatShare - VoteShare
        ) ^ 2 / (VoteShare)), digits = 2),
        ENPP = signif(1 / sum((SeatShare) ^ 2), digits = 2)
      )

    summary <-
      psych::describe(
        disp[2:5],
        quant = c(.10, .25, .75, .90),
        IQR = TRUE,
        skew = FALSE
      )
    ### Disproportionality indexes ###


    ## Seat biases - by Party

    apportionment_sum3 <-
      dplyr::group_by(apportionment, distTS, Party)
    esb <-
      dplyr::summarise(apportionment_sum3,
                       SB2_i = mean((SeatShare / 100 - VoteShare / 100)),
                       VotesParty_DM = sum(Votes))


    # Only for country-wide districts
    if (nd == 1) {
      apportionment_sum3a <- dplyr::group_by(apportionment, Party)
      esb.3 <-
        dplyr::summarise(apportionment_sum3a,
                         SB2_i = mean((SeatShare / 100 - VoteShare / 100)),
                         VotesParty_dist = sum(Votes))

    }

    else {
      esb.3 = NULL
    }
    ###

    ### Results

    out <-
      list(
        Seat_Excess = seat_excess,
        Apportionment = apportionment,
        Disproportionality_per_elec = disp,
        Summary = summary
      )
    #      list(seat_excess, apportionment, disp, esb, esb.2, esb.3, summary)

    return(out)

  }

# --------------------

#' Per-party Disproportionality Measures for Varying District Sizes and Numbers of Parties
#'
#' The function computes per-party disproportionality measures using simulated election data with varying district sizes and numbers of parties. Also, the function plots the relationships betweenon, on the one hand, values of a variaty of disproportionality measures and, on the other hand, district sizes or numbers of parties.
#' @export

simulate_Disp <-
  function(seed = 1000,
           dist = "lnorm",
           np,
           nd = 1,
           ne,
           mean = 11,
           sd = 1.8,
           rate = 1 / 15000,
           max = 50000,
           formula,
           formula_dist = "hh",
           threshold = 0,
           threshold_country = 0,
           minTS = 3,
           maxTS = 20,
           ...) {
    # Declare vars
    sb_bw <- list()
    sim <- list()

    # Data
    if (nd >= minTS) {
      stop(
        "The total number of districts ('nd') needs to be less than the minimal total number of seats ('minTS')."
      )
    }

    else {
      for (i in seq(2, maxTS, by = 2)) {
        sim[[i]] <-
          simulate_E(
            seed,
            dist,
            np,
            nd,
            ne,
            mean,
            sd,
            rate,
            max,
            TS = minTS + i - 2,
            formula,
            formula_dist,
            threshold,
            threshold_country
          )

        sb_bw[[i]] <-
          dplyr::mutate(
            sim[[i]][[2]],
            method = formula,
            TS = minTS + i - 2,
            #            SE1_i = SeatShare / 100 * distTS - VoteShare / 100 * distTS,
            SE1_i = Seats - VoteShare / 100 * distTS,
            SE2_i = SeatShare / 100 - VoteShare / 100
          )

      }
      sb_bw <- dplyr::bind_rows(sb_bw) # return data frame


      ese <- dplyr::group_by(sb_bw, TS, Party)
      ese <-
        dplyr::summarise(ese,
                         SB1_i = mean(Seats - VoteShare / 100 * distTS),
                         V = sum(Votes))

      ese2 <- dplyr::group_by(sb_bw, TS, Party)
      ese2 <-
        dplyr::summarise(ese2,
                         SB2_i = mean(SeatShare / 100 - VoteShare / 100),
                         V = sum(Votes))
      ese_mean <- dplyr::group_by(sb_bw, TS, Party)
      ese_mean <-
        dplyr::summarise(ese_mean,
                         SB1_i = mean(Seats - VoteShare / 100 * distTS),
                         V = sum(Votes))
      ese_mean <- dplyr::group_by(ese_mean, Party)
      ese_mean <-
        dplyr::summarise(ese_mean, ESB1 = mean(SB1_i), TV = sum(V))

      # Return list

      bias_data <-
        list(
          sb_bw = sb_bw,
          ese = ese,
          ese2 = ese2,
          ese_mean = ese_mean
        )

      return(bias_data)

    }

  }

# ----

#' Plot Disproportionality Measures Related to District Sizes and Numbers of Parties
#'
#' The function plots the relationships between, on the one hand, values of a variaty of disproportionality measures and, on the other hand, district sizes or numbers of parties.
#' @export

plot_Disp <-
  function(bias_data, tse = c(0, 5 / 12, -1 / 12, -4 / 12), ...)
  {
    # Plots
    sb_bw_plot1 <-
      ggplot2::ggplot(data = bias_data$sb_bw) + ggplot2::geom_boxplot(ggplot2::aes(
        x = Party,
        y = (SeatShare / 100 * distTS - VoteShare / 100 * distTS),
        fill = factor(TS)
      ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + ggplot2::ylab("Seat Excess") + viridis::scale_fill_viridis(discrete = TRUE,
                                                                       name = "DM",
                                                                       option = "D", end = 0.8) + ggplot2::geom_hline(yintercept = tse) + ggplot2::theme_classic() + ggplot2::theme(
                                                                         panel.grid.major = ggplot2::element_line(size = .3, color = "red"),
                                                                         #increase size of axis lines
                                                                         axis.line = ggplot2::element_line(size =
                                                                                                             .3, color = "black"),
                                                                         axis.ticks = ggplot2::element_line(size =
                                                                                                              0.35, color = "black"),
                                                                         #increase the font size
                                                                         text = ggplot2::element_text(size =
                                                                                                        12)
                                                                       )


    sb_bw_plot2 <-
      ggplot2::ggplot(data = bias_data$sb_bw) + ggplot2::geom_boxplot(ggplot2::aes(
        x = Party,
        y = (Seats - VoteShare / 100 * distTS),
        fill = factor(TS)
      ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + ggplot2::ylab("SE_i1(M)") + viridis::scale_fill_viridis(discrete = TRUE,
                                                                    name = "M",
                                                                    option = "D", end = 0.8) + ggplot2::geom_hline(yintercept = tse) + ggplot2::theme_classic() + ggplot2::theme(
                                                                      panel.grid.major = ggplot2::element_line(size = .3, color = "red"),
                                                                      #increase size of axis lines
                                                                      axis.line = ggplot2::element_line(size =
                                                                                                          .3, color = "black"),
                                                                      axis.ticks = ggplot2::element_line(size =
                                                                                                           .3, color = "black"),
                                                                      #increase the font size
                                                                      text = ggplot2::element_text(size =
                                                                                                     12)
                                                                    )

    sb_bw_plot3 <-
      ggplot2::ggplot(data = bias_data$sb_bw) + ggplot2::geom_boxplot(ggplot2::aes(
        x = Party,
        y = (SeatShare / 100 - VoteShare / 100),
        fill = factor(TS)
      ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + ggplot2::ylab("SE_i2(M)") + viridis::scale_fill_viridis(discrete = TRUE,
                                                                    name = "M",
                                                                    option = "D", end = 0.8) + ggplot2::geom_hline(yintercept = c(0)) + ggplot2::theme_classic() + ggplot2::theme(
                                                                      panel.grid.major = ggplot2::element_line(size = .3, color = "red"),
                                                                      #increase size of axis lines
                                                                      axis.line = ggplot2::element_line(size =
                                                                                                          .3, color = "black"),
                                                                      axis.ticks = ggplot2::element_line(size =
                                                                                                           .3, color = "black"),
                                                                      #increase the font size
                                                                      text = ggplot2::element_text(size =
                                                                                                     12)
                                                                    )

    ese_plot <-
      ggplot2::ggplot(data = bias_data$ese) + ggplot2::geom_point(
        ggplot2::aes(
          x = Party,
          y = SB1_i,
          colour = factor(TS)
        ),
        size = 4,
        alpha = 1 / 2
      ) + ggplot2::ylab("B_i1(M)") + ggplot2::facet_grid( ~ V) + viridis::scale_color_viridis(name =
                                                                                                "M", discrete = TRUE) + ggplot2::theme_classic() + ggplot2::geom_hline(yintercept = tse)

    ese_plot2 <-
      ggplot2::ggplot(data = bias_data$ese2) + ggplot2::geom_point(
        ggplot2::aes(
          x = Party,
          y = SB2_i,
          colour = factor(TS)
        ),
        size = 4,
        alpha = 1 / 2
      ) + ggplot2::ylab("B_i2(M)") + ggplot2::facet_grid( ~ V) + viridis::scale_color_viridis(name =
                                                                                                "M", discrete = TRUE) + ggplot2::theme_classic() + ggplot2::geom_hline(yintercept = c(0))

    ese_mean_plot <-
      ggplot2::ggplot(data = bias_data$ese_mean) + ggplot2::geom_point(
        ggplot2::aes(
          x = Party,
          y = ESB1,
          colour = factor(TV)
        ),
        size = 4,
        alpha = 1 / 2
      ) + ggplot2::ylab("B_i1(M)") + ggplot2::theme_classic() + ggplot2::geom_hline(yintercept = tse) + viridis::scale_color_viridis(name =
                                                                                                                                       "TV", discrete = TRUE)
    # Return list

    sb <-
      list(sb_bw_plot2,
           sb_bw_plot3,
           ese_plot,
           ese_plot2,
           ese_mean_plot)

    return(sb)
  }


# ----


#' Aggregate-Level Disproportionality Measures
#'
#' The function computes aggregate-level measures of disproportionality. It also models and plots relationships between values of aggregate-level disproportionality measures and district sizes.
#' @export

Disp2 <- function(seed = 1000,
                       np = 3,
                       nd = 1,
                       ne = 100,
                       dist = "lnorm",
                       minTS = 3,
                       lim = 16,
                       jump = 1,
                       threshold = 0,
                       threshold_country = 0,
                       ...)

{
  dsl <- list()
  dmsl <- list()
  ddh <- list()
  dhh <- list()
  dad <- list()
  dhamilton <- list()

  seatbias <- list()
  out <- list()


  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "dh",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    ddh[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "DH",
                    DM = minTS + i - 2,
                    NP = np)

  }

  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "sl",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    dsl[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "SL",
                    DM = minTS + i - 2,
                    NP = np)

  }


  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "msl",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    dmsl[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "MSL",
                    DM = minTS + i - 2,
                    NP = np)

  }


  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "hamilton",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    dhamilton[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "H",
                    DM = minTS + i - 2,
                    NP = np)

  }

  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "hh",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    dhh[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "HH",
                    DM = minTS + i - 2,
                    NP = np)

  }

  for (i in seq(2, lim, by = jump)) {
    seatbias[[i]] <-
      simulate_E(
        seed = seed,
        np = np,
        nd = nd,
        ne = ne,
        dist = dist,
        rate = 1 / 15000,
        mean = 11,
        sd = 1.8,
        max = 50000,
        TS = minTS + i - 2,
        formula = "ad",
        formula_dist = "hh",
        threshold = threshold,
        threshold_country = threshold_country
      )

    dad[[i]] <-
      dplyr::mutate(seatbias[[i]][[3]],
                    method = "A",
                    DM = minTS + i - 2,
                    NP = np)

  }

  lghi_dh <- dplyr::bind_rows(ddh)
  lghi_sl <- dplyr::bind_rows(dsl)
  lghi_msl <- dplyr::bind_rows(dmsl)
  lghi_hh <- dplyr::bind_rows(dhh)
  lghi_ad <- dplyr::bind_rows(dad)
  lghi_hamilton <- dplyr::bind_rows(dhamilton)

  # ----

  # Models
  ## DH

#  model_dh <- glm( GHI ~ DM, family = gaussian(link = "log"), data = dplyr::filter(lghi_dh, method == "DH") )

  model_dh <- nls( GHI ~ C*exp(alpha*DM), start = list(C = 0.5, alpha = -0.1), data = dplyr::filter(lghi_dh, method == "DH") )

  lghi_dh = dplyr::mutate(lghi_dh, GHI_predicted = predict(model_dh) )

  # ----

  ## SL

#  model_sl <- lm( I(log(GHI)) ~ DM, data = dplyr::filter(lghi_sl, method == "SL") )

  model_sl <- nls( GHI ~ C*exp(alpha*DM), start = list(C = 0.5, alpha = -0.1), data = dplyr::filter(lghi_sl, method == "SL") )

  lghi_sl = dplyr::mutate(lghi_sl, GHI_predicted = predict(model_sl) )

  # ----

  ## MSL

  model_msl <- nls( GHI ~ C*exp(alpha*DM), start = list(C = 0.5, alpha = -0.1), data = dplyr::filter(lghi_msl, method == "MSL") )

  lghi_msl = dplyr::mutate(lghi_msl, GHI_predicted = predict(model_msl) )

  # ----

  ## H

  model_h <- nls( GHI ~ C*exp(alpha*DM), start = list(C = 0.5, alpha = -0.1), data = dplyr::filter(lghi_hamilton, method == "H") )

  lghi_hamilton = dplyr::mutate(lghi_hamilton, GHI_predicted = predict(model_h) )

  # ----


#  lghi_all <-
#    dplyr::bind_rows(lghi_dh, lghi_sl, lghi_msl, lghi_hh, lghi_ad, lghi_hamilton)

  lghi_all <-
    dplyr::bind_rows(lghi_dh, lghi_sl, lghi_msl, lghi_hamilton)

  # Plots

  plot_GHI <-
    ggplot2::ggplot(data = lghi_all) + ggplot2::geom_boxplot(ggplot2::aes(
      x = factor(DM),
      y = GHI,
      fill = factor(method)
    ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + viridis::scale_fill_viridis(option = "C",
                                     discrete = TRUE,
                                     begin = 0.4) +  ggplot2::xlab("DM") + ggplot2::ylab("GHI") + ggplot2::labs(fill = "Method") + ggplot2::geom_hline(yintercept = c(0.1), size = 0.35, linetype = "longdash", colour = "blue" ) + ggplot2::theme_classic() + ggplot2::theme(
                                       panel.grid.major = ggplot2::element_line(size = 0.35, color = "red"),
                                       #increase size of axis lines
                                       axis.line = ggplot2::element_line(size =
                                                                           0.35, color = "black"),
                                       axis.ticks = ggplot2::element_line(size =
                                                                           0.35, color = "black"),
                                       #Adjust legend position to maximize space, use a vector of proportion
                                       #across the plot and up the plot where you want the legend.
                                       #You can also use "left", "right", "top", "bottom", for legends on t
                                       #he side of the plot
                                       legend.position = c(.85, .7),
                                       #increase the font size
                                       text = ggplot2::element_text(size =
                                                                      12)
                                     )

  plot_LHI <-
    ggplot2::ggplot(data = lghi_all) + ggplot2::geom_boxplot(ggplot2::aes(
      x = factor(DM),
      y = LHI,
      fill = factor(method)
    ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + viridis::scale_fill_viridis(option = "D",
                                     discrete = TRUE,
                                     begin = 0.4) +  ggplot2::xlab("DM") + ggplot2::ylab("LHI") + ggplot2::labs(fill = "Method") + ggplot2::geom_hline(yintercept = c(0, 0.1), size = 0.35, linetype = "longdash", colour = "blue") + ggplot2::theme_classic() + ggplot2::theme(
                                       panel.grid.major = ggplot2::element_line(size = 0.35, color = "red"),
                                       #increase size of axis lines
                                       axis.line = ggplot2::element_line(size =
                                                                           0.35, color = "black"),
                                       axis.ticks = ggplot2::element_line(size =
                                                                            0.35, color = "black"),
                                       #Adjust legend position to maximize space, use a vector of proportion
                                       #across the plot and up the plot where you want the legend.
                                       #You can also use "left", "right", "top", "bottom", for legends on t
                                       #he side of the plot
                                       legend.position = c(.85, .7),
                                       #increase the font size
                                       text = ggplot2::element_text(size =
                                                                      12)
                                     )

  # ----

  # scatter_GHI <-
  #   ggplot2::ggplot(data = dplyr::filter(lghi_all, method %in% c("DH", "SL", "MSL", "H") )) + ggplot2::geom_jitter(ggplot2::aes(
  #     x = DM,
  #     y = GHI,
  #     color = factor(method)
  #   ), size = 1.5, alpha = 0.5, width = 0.2) + viridis::scale_color_viridis(option = "D", discrete = TRUE) +  ggplot2::xlab("DM") + ggplot2::ylab("GHI") + ggplot2::labs(color = "Method") + ggplot2::geom_hline(yintercept = c(0.1), size = 0.35, linetype = "longdash", colour = "blue") + ggplot2::theme_classic() + ggplot2::theme(
  #     #increase size of axis lines
  #     axis.line = ggplot2::element_line(size =
  #                                         0.35, color = "black"),
  #     axis.ticks = ggplot2::element_line(size =
  #                                          0.35, color = "black"),
  #     #Adjust legend position to maximize space, use a vector of proportion
  #     #across the plot and up the plot where you want the legend.
  #     #You can also use "left", "right", "top", "bottom", for legends on t
  #     #he side of the plot
  #     legend.position = c(.85, .7),
  #     #increase the font size
  #     text = ggplot2::element_text(size =
  #                                    12)
  #   ) + ggplot2::geom_line( ggplot2::aes(x = DM, y = GHI_predicted, color = factor(method)), size=1, alpha = 0.8)

  # ----

  scatter_GHI <-
    ggplot2::ggplot(data = dplyr::filter(lghi_all, method %in% c("DH", "SL", "MSL", "H") )) + ggplot2::geom_jitter(ggplot2::aes(
      x = DM,
      y = GHI
), size = 1.5, alpha = 0.5, width = 0.2) + ggplot2::facet_grid( . ~ method ) + ggplot2::xlab("DM") + ggplot2::ylab("GHI") + ggplot2::labs(color = "Method") + ggplot2::geom_hline(yintercept = c(0.7), size = 0.35, linetype = "longdash", colour = "blue") + ggplot2::theme_classic() + ggplot2::theme(
      #increase size of axis lines
      axis.line = ggplot2::element_line(size =
                                          0.35, color = "black"),
      axis.ticks = ggplot2::element_line(size =
                                           0.35, color = "black"),
      #Adjust legend position to maximize space, use a vector of proportion
      #across the plot and up the plot where you want the legend.
      #You can also use "left", "right", "top", "bottom", for legends on t
      #he side of the plot
      legend.position = c(.85, .7),
      #increase the font size
      text = ggplot2::element_text(size =
                                     12)
    ) + ggplot2::geom_line( ggplot2::aes(x = DM, y = GHI_predicted), size=1, alpha = 0.8)

  # ----

  # ggplot2::geom_smooth(ggplot2::aes(
  # x = DM,
  # y = GHI,
  # color = factor(method)
  # ), lwd = 0.25, method = "glm", family = gaussian(link = 'log'))

  # ----

  plot_SLI <-
    ggplot2::ggplot(data = lghi_all) + ggplot2::geom_boxplot(ggplot2::aes(
      x = factor(DM),
      y = SLI,
      fill = factor(method)
    ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + viridis::scale_fill_viridis(option = "D",
                                                                                   discrete = TRUE,
                                                                                   begin = 0.4) +  ggplot2::xlab("DM") + ggplot2::ylab("SLI") + ggplot2::labs(fill = "Method") + ggplot2::geom_hline(yintercept = c(0, 0.1), size = 0.35, linetype = "longdash", colour = "blue") + ggplot2::theme_classic() + ggplot2::theme(
                                                                                     panel.grid.major = ggplot2::element_line(size = 0.35, color = "red"),
                                                                                     #increase size of axis lines
                                                                                     axis.line = ggplot2::element_line(size =
                                                                                                                         0.35, color = "black"),
                                                                                     axis.ticks = ggplot2::element_line(size =
                                                                                                                          0.35, color = "black"),
                                                                                     #Adjust legend position to maximize space, use a vector of proportion
                                                                                     #across the plot and up the plot where you want the legend.
                                                                                     #You can also use "left", "right", "top", "bottom", for legends on t
                                                                                     #he side of the plot
                                                                                     legend.position = c(.85, .7),
                                                                                     #increase the font size
                                                                                     text = ggplot2::element_text(size =
                                                                                                                    12)
                                                                                   ) + ylim(0, 1)

  plot_ENPP <-
    ggplot2::ggplot(data = lghi_all) + ggplot2::geom_boxplot(ggplot2::aes(
      x = factor(DM),
      y = ENPP,
      fill = factor(method)
    ), lwd = 0.25, fatten = 0.4, outlier.size = 0.6) + viridis::scale_fill_viridis(option = "D",
                                     discrete = TRUE,
                                     begin = 0.4) +  ggplot2::xlab("DM") + ggplot2::ylab("ENPP") + ggplot2::labs(fill = "Method") + ggplot2::geom_hline(yintercept = c(2), size = 0.35, linetype = "longdash", colour = "blue") + ggplot2::theme_classic() + ggplot2::theme(
                                       panel.grid.major = ggplot2::element_line(size = 0.35, color = "red"),
                                       #increase size of axis lines
                                       axis.line = ggplot2::element_line(size =
                                                                           0.35, color = "black"),
                                       axis.ticks = ggplot2::element_line(size =
                                                                            0.35, color = "black"),
                                       #Adjust legend position to maximize space, use a vector of proportion
                                       #across the plot and up the plot where you want the legend.
                                       #You can also use "left", "right", "top", "bottom", for legends on t
                                       #he side of the plot
                                       legend.position = c(.85, .7),
                                       #increase the font size
                                       text = ggplot2::element_text(size =
                                                                      12)
                                     )

  out <-
    list(summary = lghi_all, plot_GHI = plot_GHI, scatter_GHI = scatter_GHI, Model_DH = model_dh, Model_SL = model_sl, Model_MSL = model_msl, Model_Hamilton = model_h)

  return(out)

}
