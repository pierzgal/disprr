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
                      rtrunc(
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
                    sort(floor(rtrunc(
                      np,
                      spec = "exp",
                      a = 0,
                      b = max,
                      rate = rate
                    )))
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

    # Apportionment functions with sorting disabled

    switch (
      formula_dist,
      hamilton = {
        for (i in seq(1, ne, by = 1)) {
          seats_dist[[i]] <-
            Hamilton3(
              parties = .if.parties.null(nd),
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
              parties = .if.parties.null(nd),
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
              parties = .if.parties.null(nd),
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
              parties = .if.parties.null(nd),
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
        Votes_Total = tsum
      )



    out <- sample

  }


# --------------------


#' Simulate Elections under Proportional Representation
#'
#' The function simulate election results and compute a variaty of disproportionality measures.
#'
#' @return A list containg the following data objects:
#' seat_excess,
#' apportionment,
#' disp,
#' summary.
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
            threshold_country) {
    set.seed(seed)

    sample <-
      sampleElectionData(seed, dist, np, nd, ne, mean, sd, rate, max, TS, formula_dist)


    # Seat apportionment per district
    # Return list (Party Seats SeatShare Votes VoteShare id elec dist distTS)

    apportionment <-
      .ProportionalRepresentation(sample, formula, threshold, threshold_country)


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
        out[[i]] <- mutate(out[[i]], Party = .if.parties.null(np))
        out[[i]] <- arrange(out[[i]], desc(VoteShareTotalParty))
      }

      vote_share <- bind_rows(out)
      }

    apportionment <-
      left_join(apportionment, vote_share, by = c("Party", "elec"))


    ## Group districts in elections

    apportionment_sum1 <- group_by(apportionment, elec, Party)
    apportionment_sum2 <-
      dplyr::summarise(
        apportionment_sum1,
        seats = sum(Seats),
        seat_perc = sum(Seats) / TS * 100,
        TS = TS[1]
      )

    apportionment_sum <-
      left_join(apportionment_sum2, vote_share, by = c("Party", "elec"))


    ## Compute 'ideal' shares of seats
    seats_ideal <- .seatsIdeal(ne, nd, np, sample)

    ## Merge
    merged <-
      left_join(apportionment_sum, seats_ideal, by = c("Party", "elec"))


    ## Seat excesses
    seat_excess <-
      mutate(
        merged,
        Party = as.factor(Party),
        excess = round(seat_perc / 100 - SeatShareIdeal / 100, 6),
        SeatShare = round(seat_perc / 100, 4),
        VoteShare = round(VoteShareTotalParty / 100, 4),
        StandardQuota = (SeatShareIdeal / 100) * TS,
        RSE2_i = round((seat_perc - SeatShareIdeal) / SeatShareIdeal, 4)
      )

    # Remark :  RSE_i = DEV0

    seat_excess <-
      select(
        seat_excess,
        PartyID = Party,
        ElectionID = elec,
        Seats = seats,
        SeatShare,
        Votes = VotesTotalParty,
        VoteShare,
        SQ = StandardQuota,
        SE2_i = excess,
        RSE2_i
      )
    seat_excess <- mutate(seat_excess, Seats = as.integer(Seats))


    ### Disproportionality indexes ###
    disp <- group_by(seat_excess, ElectionID)
    disp <-
      dplyr::summarise(
        disp,
        meanRSE2 = sum(abs(RSE2_i)) / np,
        LHI = 1 / 2 * sum(abs(SeatShare - VoteShare)),
        GHI = sqrt(1 / 2 * sum((
          SeatShare - VoteShare
        ) ^ 2)),
        SLI = sum((SeatShare - VoteShare) ^ 2 / (VoteShare))
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

    apportionment_sum3 <- group_by(apportionment, distTS, Party)
    esb <-
      dplyr::summarise(apportionment_sum3,
                       E = mean((SeatShare / 100 - VoteShare / 100)),
                       VotesParty_DM = sum(Votes))

    apportionment_sum3.2 <-
      group_by(apportionment, elec, distTS, Party)
    esb.2 <-
      dplyr::summarise(apportionment_sum3.2,
                       E = mean((SeatShare / 100 - VoteShare / 100)),
                       VotesParty_dist = sum(Votes))


    # Only for country-wide districts
    if (nd == 1) {
      apportionment_sum3.3 <- group_by(apportionment, Party)
      esb.3 <-
        dplyr::summarise(apportionment_sum3.3,
                         E = mean((SeatShare / 100 - VoteShare / 100)),
                         VotesParty_dist = sum(Votes))

    }

    else {
      esb.3 = NULL
    }
    ###

    ### Results

    out <-
      list(seat_excess, apportionment, disp, esb, esb.2, esb.3, summary)

    return(out)

  }

# --------------------

#' Per-party Disproportionality Measures for Varying District Sizes and Numbers of Parties
#'
#' The function computes per-party disproportionality measures using simulated election data with varying district sizes and numbers of parties. Also, the function plots the relationships betweenon, on the one hand, values of a variaty of disproportionality measures and, on the other hand, district sizes or numbers of parties.
#' @export

simulate_Disp <-
  function(seed = 1000,
           dist = "uniform",
           np,
           nd = 1,
           ne,
           mean = 12.5,
           sd = 1.5,
           rate = 0.00001,
           max = 500000,
           formula,
           formula_dist = "hh",
           threshold = 0,
           threshold_country = 0,
           minTS = 6,
           steps = 20,
           ...) {
    # Declare vars
    sb_bw <- list()
    sim <- list()
    
    # Data
    if (nd >= minTS) {
      stop("The total number of districts ('nd') needs to be less than the minimal total number of seats ('minTS').")
    }
    
    else {
      for (i in seq(1, steps, by = 1)) {
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
            TS = minTS + i * 6 - 6,
            formula,
            formula_dist,
            threshold,
            threshold_country
          )
        
        sb_bw[[i]] <-
          mutate(
            sim[[i]][[2]],
            method = "dh",
            TS = minTS + i * 6 - 6,
            SE1 = SeatShare / 100 * distTS - VoteShare / 100 * distTS,
            SE2 = Seats - VoteShare / 100 * distTS,
            TSB = (3 * VoteShare / 100 - 1) / 2
          )
        
      }
      sb_bw <- bind_rows(sb_bw) # return data frame
      
      
      ese <- group_by(sb_bw, TS, Party)
      ese <-
        summarise(ese,
                  SB_i = mean(Seats - VoteShare / 100 * distTS),
                  V = sum(Votes))
      
      ese2 <- group_by(sb_bw, TS, Party)
      ese2 <-
        summarise(ese2,
                  SB2_i = mean(SeatShare / 100 - VoteShare / 100),
                  V = sum(Votes))
      ese_mean <- group_by(sb_bw, TS, Party)
      ese_mean <-
        summarise(ese_mean,
                  SB_i = mean(Seats - VoteShare / 100 * distTS),
                  V = sum(Votes))
      ese_mean <- group_by(ese_mean, Party)
      ese_mean <- summarise(ese_mean, ESB = mean(SB_i), TV = sum(V))
      
      # Return list
      
      bias_data <-
        list(sb_bw = sb_bw,
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
#' The function plots the relationships betweenon, on the one hand, values of a variaty of disproportionality measures and, on the other hand, district sizes or numbers of parties.
#' @export

plot_Disp <-
  function(bias_data, tse = c(0, 5 / 12, -1 / 12, -4 / 12), ...)
  {
    
    # Plots
    sb_bw_plot1 <-
      ggplot(data = bias_data$sb_bw) + geom_boxplot(aes(
        x = Party,
        y = (SeatShare / 100 * distTS - VoteShare / 100 * distTS),
        colour = factor(TS)
      )) + ylab("Seat Excess") + scale_color_viridis(discrete = TRUE,
                                                     name = "DM",
                                                     option = "D") + geom_hline(yintercept = tse) + theme_classic()
    
    
    sb_bw_plot2 <-
      ggplot(data = bias_data$sb_bw) + geom_boxplot(aes(
        x = Party,
        y = (Seats - VoteShare / 100 * distTS),
        colour = factor(TS)
      )) + ylab("SE_i1(M)") + scale_color_viridis(discrete = TRUE,
                                                  name = "M",
                                                  option = "D") + geom_hline(yintercept = tse) + theme_classic()
    
    sb_bw_plot3 <-
      ggplot(data = bias_data$sb_bw) + geom_boxplot(aes(
        x = Party,
        y = (SeatShare / 100 - VoteShare / 100),
        colour = factor(TS)
      )) + ylab("SE_i2(M)") + scale_color_viridis(discrete = TRUE,
                                                  name = "M",
                                                  option = "D") + geom_hline(yintercept = c(0)) + theme_classic()
    
    ese_plot <-
      ggplot(data = bias_data$ese) + geom_point(aes(
        x = Party,
        y = SB_i,
        colour = factor(TS)
      ),
      size = 4,
      alpha = 1 / 2) + ylab("B_i1(M)") + facet_grid( ~ V) + scale_color_viridis(name =
                                                                                  "M", discrete = TRUE) + theme_classic() + geom_hline(yintercept = tse)
    
    ese_plot2 <-
      ggplot(data = bias_data$ese2) + geom_point(aes(
        x = Party,
        y = SB2_i,
        colour = factor(TS)
      ),
      size = 4,
      alpha = 1 / 2) + ylab("B_i2(M)") + facet_grid( ~ V) + scale_color_viridis(name =
                                                                                  "M", discrete = TRUE) + theme_classic() + geom_hline(yintercept = c(0))
    
    ese_mean_plot <-
      ggplot(data = bias_data$ese_mean) + geom_point(aes(
        x = Party,
        y = ESB,
        colour = factor(TV)
      ),
      size = 4,
      alpha = 1 / 2) + ylab("B_i1(M)") + theme_classic() + geom_hline(yintercept = tse) + scale_color_viridis(name =
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
