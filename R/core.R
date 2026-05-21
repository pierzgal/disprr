#' Sample Election Data
#'
#' Generate pseudo-random election results from a specified probability
#' distribution.
#'
#' @param seed Integer: random seed for reproducibility.
#' @param dist Character: probability distribution for vote sampling.
#'   One of \code{"uniform"} (default), \code{"lnorm"}, or \code{"exp"}.
#' @param np Integer: number of parties.
#' @param nd Integer: number of electoral districts.
#' @param ne Integer: number of elections.
#' @param mean Numeric: location parameter for log-normal distribution.
#' @param sd Numeric: scale parameter for log-normal distribution.
#' @param rate Numeric: rate parameter for exponential distribution.
#' @param max Numeric: upper bound for truncated distributions or for
#'   \code{runif}.
#' @param TS Integer: total number of seats to apportion among districts.
#' @param formula_dist Character: method used to divide \code{TS} seats among
#'   districts. One of \code{"hamilton"}, \code{"ad"}, \code{"dh"}, or
#'   \code{"hh"}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{Votes_Dist_Party}{3D array \code{[party, district, election]}.}
#'     \item{Seats_Dist}{List of per-election seat allocations to districts.}
#'     \item{Votes_Share_Party}{List of per-election party vote shares.}
#'     \item{Votes_Total_Dist}{List of per-election district vote totals.}
#'     \item{Votes_Total_Party}{List of per-election party vote totals.}
#'     \item{Votes_Total}{List of per-election grand totals.}
#'     \item{Params}{Vector: \code{c(ne, nd, np, TS)}.}
#'   }
#'
#' @export
sampleElectionData <- function(seed = 0,
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
  dist <- match.arg(dist, c("uniform", "lnorm", "exp"))

  x <- array(dim = c(np, nd, ne))

  for (j in seq_len(ne)) {
    for (i in seq_len(nd)) {
      x[, i, j] <- sort(floor(switch(
        dist,
        uniform = runif(np, min = 0, max = max),
        lnorm   = truncdist::rtrunc(np, spec = "lnorm", a = 0, b = max,
                                     meanlog = mean, sdlog = sd),
        exp     = truncdist::rtrunc(np, spec = "exp", a = 0, b = max,
                                     rate = rate)
      )))
    }
  }

  csum <- vector("list", ne)
  rsum <- vector("list", ne)
  tsum <- vector("list", ne)

  for (i in seq_len(ne)) {
    if (nd > 1L) {
      csum[[i]] <- apply(x[, , i], 2, sum)
      rsum[[i]] <- apply(x[, , i], 1, sum)
    } else {
      csum[[i]] <- sum(x[, , i])
      rsum[[i]] <- x[, , i]
    }
    tsum[[i]] <- sum(x[, , i])
  }

  district_labels <- if.parties.null(nd)
  seats_dist <- vector("list", ne)

  for (i in seq_len(ne)) {
    if (formula_dist == "hamilton") {
      seats_dist[[i]] <- LR_Hamilton(
        parties = district_labels,
        votes = csum[[i]],
        seats = TS,
        order_name = FALSE
      )$Seats
    } else {
      seats_dist[[i]] <- divisorMethods(
        parties = district_labels,
        votes = csum[[i]],
        seats = TS,
        method = formula_dist,
        order_name = FALSE
      )$Seats
    }
  }

  votes_share <- lapply(seq_len(ne), \(i) rsum[[i]] / tsum[[i]])

  list(
    Votes_Dist_Party  = x,
    Seats_Dist        = seats_dist,
    Votes_Share_Party = votes_share,
    Votes_Total_Dist  = csum,
    Votes_Total_Party = rsum,
    Votes_Total       = tsum,
    Params            = c(ne, nd, np, TS)
  )
}


# ----


#' Simulate Elections under Proportional Representation
#'
#' Simulates election results and computes per-party and aggregate
#' disproportionality measures.
#'
#' @param seed Integer: random seed.
#' @param dist Character: probability distribution (\code{"uniform"},
#'   \code{"lnorm"}, or \code{"exp"}).
#' @param np,nd,ne Integer: number of parties, districts, elections.
#' @param mean,sd Numeric: log-normal parameters.
#' @param rate Numeric: exponential rate parameter.
#' @param max Numeric: upper bound for truncated distribution.
#' @param TS Integer: total seats.
#' @param formula Character: apportionment method (e.g., \code{"dh"}, \code{"sl"},
#'   \code{"hamilton"}).
#' @param formula_dist Character: method for distributing seats among districts.
#' @param threshold Numeric: district-level threshold.
#' @param threshold_country Numeric: country-level threshold.
#'
#' @return A list with components:
#'   \describe{
#'     \item{Seat_Excess}{Per-party, per-election seat excess measures.}
#'     \item{Apportionment}{Full district-level apportionment data.}
#'     \item{Disproportionality_per_elec}{Aggregate indexes per election.}
#'     \item{Summary}{Descriptive statistics via \code{psych::describe}.}
#'   }
#'
#' @export
simulate_E <- function(seed,
                       dist = "lnorm",
                       np, nd, ne,
                       mean, sd, rate, max,
                       TS,
                       formula,
                       formula_dist,
                       threshold,
                       threshold_country,
                       ...) {
  set.seed(seed)

  sample <- sampleElectionData(
    seed = seed, dist = dist, np = np, nd = nd, ne = ne,
    mean = mean, sd = sd, rate = rate, max = max,
    TS = TS, formula_dist = formula_dist
  )

  apportionment <- .ProportionalRepresentation(
    sample = sample, formula = formula,
    threshold = threshold, threshold_country = threshold_country
  )
  apportionment$Party <- as.character(apportionment$Party)

  ## Vote shares at country level
  vote_share_list <- vector("list", ne)
  for (i in seq_len(ne)) {
    vote_share_list[[i]] <- data.frame(
      VoteShareTotalParty = round(sample$Votes_Share_Party[[i]], 4),
      VotesTotalParty = as.integer(sample$Votes_Total_Party[[i]]),
      elec = paste0("e", i),
      Party = if.parties.null(np),
      stringsAsFactors = FALSE
    )
    vote_share_list[[i]] <- vote_share_list[[i]][
      order(-vote_share_list[[i]]$VoteShareTotalParty), ]
  }
  vote_share <- do.call(rbind, vote_share_list)

  apportionment <- merge(apportionment, vote_share, by = c("Party", "elec"),
                         all.x = TRUE)

  ## Aggregate seats across districts within each election
  agg <- stats::aggregate(
    Seats ~ elec + Party,
    data = apportionment,
    FUN = sum
  )
  names(agg)[3] <- "seats"
  agg$seat_perc <- agg$seats / TS

  distTS_first <- stats::aggregate(
    distTS ~ elec + Party,
    data = apportionment,
    FUN = \(x) x[1]
  )
  agg <- merge(agg, distTS_first, by = c("elec", "Party"), all.x = TRUE)
  agg$TS <- TS
  agg <- merge(agg, vote_share, by = c("Party", "elec"), all.x = TRUE)

  ## Ideal seat shares
  seats_ideal <- .seatsIdeal(ne, nd, np, sample)
  merged <- merge(agg, seats_ideal, by = c("Party", "elec"), all.x = TRUE)

  ## Seat excesses
  seat_excess <- data.frame(
    PartyID    = merged$Party,
    ElectionID = merged$elec,
    Seats      = as.integer(merged$seats),
    SeatShare  = signif(merged$seat_perc, 3),
    Votes      = merged$VotesTotalParty,
    VoteShare  = signif(merged$VoteShareTotalParty, 3),
    SQ         = merged$SeatShareIdeal * TS,
    SE1_i      = signif(merged$seats - merged$VoteShareTotalParty * TS, 3),
    SE2_i      = signif(merged$seat_perc - merged$VoteShareTotalParty, 3),
    SE2_i_pp   = signif(merged$seat_perc - merged$SeatShareIdeal, 3),
    RSE2_i     = signif(
      (merged$seat_perc - merged$SeatShareIdeal) / merged$SeatShareIdeal, 3
    ),
    stringsAsFactors = FALSE
  )

  ## Aggregate disproportionality indexes per election
  disp_metrics <- lapply(split(seat_excess, seat_excess$ElectionID), \(df) {
    data.frame(
      ElectionID = df$ElectionID[1],
      meanRSE2 = signif(sum(abs(df$RSE2_i)) / np, 3),
      LHI  = signif(0.5 * sum(abs(df$SeatShare - df$VoteShare)), 4),
      GHI  = signif(sqrt(0.5 * sum((df$SeatShare - df$VoteShare)^2)), 3),
      SLI  = signif(sum((df$SeatShare - df$VoteShare)^2 / df$VoteShare), 4),
      ENPP = signif(1 / sum(df$SeatShare^2), 3),
      NPP  = sum(df$SeatShare > 0),
      stringsAsFactors = FALSE
    )
  })
  disp <- do.call(rbind, disp_metrics)
  rownames(disp) <- NULL

  summary_stats <- psych::describe(
    disp[, c("meanRSE2", "LHI", "GHI", "SLI")],
    quant = c(0.10, 0.25, 0.75, 0.90),
    IQR = TRUE,
    skew = FALSE
  )

  list(
    Seat_Excess              = seat_excess,
    Apportionment            = apportionment,
    Disproportionality_per_elec = disp,
    Summary                  = summary_stats
  )
}


# ----


#' Per-party Disproportionality across District Sizes
#'
#' Computes per-party disproportionality measures using simulated election data
#' across a range of total seat counts.
#'
#' @param seed,dist,np,nd,ne,rate,mean,sd,max Parameters passed to
#'   \code{simulate_E}.
#' @param formula Character: apportionment method.
#' @param formula_dist Character: method for inter-district seat allocation.
#' @param threshold,threshold_country Numeric: electoral thresholds.
#' @param minTS,maxTS,jump Integer: range and step for total seats.
#'
#' @return A list with components:
#'   \describe{
#'     \item{sb_bw}{Full per-district data across all seat sizes.}
#'     \item{ese}{Mean seat bias SB1 by party and seat size.}
#'     \item{ese2}{Mean seat bias SB2 by party and seat size.}
#'     \item{ese_mean}{Grand mean bias ESB1 by party.}
#'     \item{sim}{List of simulation results per seat size.}
#'   }
#'
#' @export
simulate_Disp <- function(seed = 0,
                          dist = "lnorm",
                          np,
                          nd = 1,
                          ne,
                          rate = 1 / 25000,
                          mean = 10,
                          sd = 1.2,
                          max = 100000,
                          formula,
                          formula_dist = "hh",
                          threshold = 0,
                          threshold_country = 0,
                          minTS = 3,
                          maxTS = 20,
                          jump = 2,
                          ...) {
  if (nd >= minTS)
    stop("'nd' must be less than 'minTS'.")

  ts_seq <- seq(from = minTS, to = maxTS, by = jump)
  sb_bw_list <- vector("list", length(ts_seq))
  sim_list <- vector("list", length(ts_seq))

  for (k in seq_along(ts_seq)) {
    i <- ts_seq[k]
    sim_list[[k]] <- simulate_E(
      seed = seed, dist = dist, np = np, nd = nd, ne = ne,
      mean = mean, sd = sd, rate = rate, max = max,
      TS = i, formula = formula, formula_dist = formula_dist,
      threshold = threshold, threshold_country = threshold_country
    )

    apport <- sim_list[[k]]$Apportionment
    apport$method <- formula
    apport$TS <- i
    apport$SE1_i <- apport$Seats - apport$VoteShare * apport$distTS
    apport$SE2_i <- apport$SeatShare - apport$VoteShare
    sb_bw_list[[k]] <- apport
  }

  sb_bw <- do.call(rbind, sb_bw_list)

  ## Aggregate seat totals per election-party-TS
  sb_bw_agg <- stats::aggregate(
    Seats ~ TS + elec + Party,
    data = sb_bw,
    FUN = sum
  )
  names(sb_bw_agg)[4] <- "SeatTotal"

  sb_bw_agg <- merge(sb_bw_agg,
    unique(sb_bw[, c("TS", "elec", "Party", "VoteShareTotalParty")]),
    by = c("TS", "elec", "Party"), all.x = TRUE
  )
  sb_bw_agg$SeatShareTotal <- sb_bw_agg$SeatTotal / sb_bw_agg$TS
  sb_bw_agg$SE2T_i <- sb_bw_agg$SeatShareTotal - sb_bw_agg$VoteShareTotalParty

  sb_bw <- merge(sb_bw, sb_bw_agg, by = c("Party", "elec", "TS"), all.x = TRUE)

  ## Expected seat biases
  sb_bw$SB1_i_raw <- sb_bw$Seats - sb_bw$VoteShare * sb_bw$distTS
  sb_bw$SB2_i_raw <- sb_bw$SeatShare - sb_bw$VoteShare

  ese_agg <- do.call(rbind, lapply(
    split(sb_bw, interaction(sb_bw$TS, sb_bw$Party, drop = TRUE)),
    \(df) data.frame(
      TS = df$TS[1], Party = df$Party[1],
      SB1_i = mean(df$SB1_i_raw), V = mean(df$Votes),
      stringsAsFactors = FALSE
    )
  ))
  rownames(ese_agg) <- NULL

  ese2_agg <- do.call(rbind, lapply(
    split(sb_bw, interaction(sb_bw$TS, sb_bw$Party, drop = TRUE)),
    \(df) data.frame(
      TS = df$TS[1], Party = df$Party[1],
      SB2_i = mean(df$SB2_i_raw), V = mean(df$Votes),
      stringsAsFactors = FALSE
    )
  ))
  rownames(ese2_agg) <- NULL

  ese_mean_grand <- do.call(rbind, lapply(
    split(ese_agg, ese_agg$Party),
    \(df) data.frame(
      Party = df$Party[1],
      ESB1 = mean(df$SB1_i), TV = sum(df$V),
      stringsAsFactors = FALSE
    )
  ))
  rownames(ese_mean_grand) <- NULL

  list(
    sb_bw    = sb_bw,
    ese      = ese_agg,
    ese2     = ese2_agg,
    ese_mean = ese_mean_grand,
    sim      = sim_list
  )
}


# ----


#' Plot Per-party Disproportionality Measures
#'
#' Produces diagnostic plots for per-party disproportionality across district
#' sizes.
#'
#' @param bias_data A list returned by \code{simulate_Disp}.
#' @param tse Numeric vector: horizontal reference lines for seat excess plots.
#'
#' @return A list of \code{ggplot} objects.
#'
#' @export
plot_Disp <- function(bias_data,
                      tse = c(0, 5/12, -1/12, -4/12),
                      ...) {
  sb <- bias_data$sb_bw

  base_theme <- ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "red"),
      axis.line  = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "black"),
      text = ggplot2::element_text(size = 12)
    )

  viridis_fill <- viridis::scale_fill_viridis(
    discrete = TRUE, name = "DM", option = "D", begin = 0.5
  )

  box_params <- list(lwd = 0.25, fatten = 0.4, outlier.size = 0.3)

  ## Plot 1: SE1_i by SeatShare at district level
  p1 <- ggplot2::ggplot(sb) +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = Party, y = SeatShare * distTS - VoteShare * distTS,
                   fill = factor(TS)),
      lwd = box_params$lwd, fatten = box_params$fatten,
      outlier.size = box_params$outlier.size
    ) +
    ggplot2::ylab("SE1_i(DM)") +
    viridis_fill +
    ggplot2::geom_hline(yintercept = tse) +
    base_theme

  ## Plot 2: SE1_i by raw Seats
  p2 <- ggplot2::ggplot(sb) +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = Party, y = Seats - VoteShare * distTS,
                   fill = factor(TS)),
      lwd = box_params$lwd, fatten = box_params$fatten,
      outlier.size = box_params$outlier.size
    ) +
    ggplot2::ylab("SE1_i(DM)") +
    viridis_fill +
    ggplot2::geom_hline(yintercept = tse) +
    base_theme

  ## Plot 3: SE2_i faceted by party
  viridis_fill2 <- viridis::scale_fill_viridis(
    discrete = TRUE, name = "DM", option = "D", begin = 0.6
  )

  p3 <- ggplot2::ggplot(sb) +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = "", y = SeatShare - VoteShare, fill = factor(TS)),
      lwd = box_params$lwd, fatten = box_params$fatten,
      outlier.size = box_params$outlier.size
    ) +
    ggplot2::facet_wrap(~Party) +
    ggplot2::ylab("SE2_i(DM)") +
    viridis_fill2 +
    ggplot2::geom_hline(yintercept = c(0, 0.1), colour = "blue") +
    base_theme +
    ggplot2::stat_summary(
      ggplot2::aes(x = "", y = SeatShare - VoteShare, fill = factor(TS)),
      fun = "mean", shape = 3, geom = "point", size = 2,
      position = ggplot2::position_dodge(width = 0.75), color = "black"
    )

  ## Plot 4: SB1_i bias
  p4 <- ggplot2::ggplot(bias_data$ese) +
    ggplot2::geom_point(
      ggplot2::aes(x = Party, y = SB1_i, colour = factor(TS)),
      size = 4, alpha = 0.5
    ) +
    ggplot2::ylab("B_i1(DM)") +
    ggplot2::facet_grid(~V) +
    viridis::scale_color_viridis(name = "DM", discrete = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = tse)

  ## Plot 5: SB2_i bias
  p5 <- ggplot2::ggplot(bias_data$ese2) +
    ggplot2::geom_point(
      ggplot2::aes(x = Party, y = SB2_i, colour = factor(TS)),
      size = 4, alpha = 0.5
    ) +
    ggplot2::ylab("B_i2(DM)") +
    ggplot2::facet_grid(~V) +
    viridis::scale_color_viridis(name = "DM", discrete = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = 0)

  ## Plot 6: ESB1 grand mean
  p6 <- ggplot2::ggplot(bias_data$ese_mean) +
    ggplot2::geom_point(
      ggplot2::aes(x = Party, y = ESB1, colour = factor(TV)),
      size = 4, alpha = 0.5
    ) +
    ggplot2::ylab("B_i1(DM)") +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = tse) +
    viridis::scale_color_viridis(name = "TV", discrete = TRUE)

  list(p2, p3, p4, p5, p6)
}


# ----


#' Aggregate-Level Disproportionality Measures
#'
#' Runs simulations across multiple apportionment methods and seat counts,
#' computing aggregate disproportionality indexes (GHI, LHI, SLI, ENPP, NPP).
#' Optionally fits \eqn{GHI \sim C \cdot e^{\alpha \cdot DM}} models.
#'
#' @param seed,np,nd,ne,dist,rate,mean,sd,max Parameters passed to
#'   \code{simulate_E}.
#' @param minTS,maxTS,jump Integer: range and step for total seats.
#' @param threshold,threshold_country Numeric: electoral thresholds.
#' @param start_C,start_alpha Numeric: NLS starting values.
#' @param model Logical: if \code{TRUE}, fit exponential decay models.
#' @param methods Character vector: method codes to simulate. Defaults to
#'   \code{c("dh", "sl", "msl", "hamilton", "hh", "ad", "imperiali", "danish")}.
#' @param formula_dist Character: method for inter-district seat allocation.
#'
#' @return A list with \code{summary} (combined disproportionality data),
#'   per-method model objects (if \code{model = TRUE}), and per-method
#'   raw simulation results.
#'
#' @export
Disp2 <- function(seed = 0,
                  np = 3,
                  nd = 1,
                  ne = 100,
                  dist = "lnorm",
                  rate = 1 / 25000,
                  mean = 10,
                  sd = 1.2,
                  max = 100000,
                  minTS = 3,
                  maxTS = 20,
                  jump = 1,
                  threshold = 0,
                  threshold_country = 0,
                  start_C = 0.2,
                  start_alpha = -0.2,
                  model = TRUE,
                  methods = c("dh", "sl", "msl", "hamilton",
                              "hh", "ad", "imperiali", "danish"),
                  formula_dist = "hh",
                  ...) {

  method_labels <- c(
    dh = "DH", sl = "SL", msl = "MSL", hamilton = "H",
    hh = "HH", ad = "A", imperiali = "Imperiali", danish = "Danish"
  )

  ts_seq <- seq(from = minTS, to = maxTS, by = jump)

  all_disp   <- vector("list", length(methods))
  all_raw    <- vector("list", length(methods))
  all_models <- list()

  for (m_idx in seq_along(methods)) {
    meth <- methods[m_idx]
    label <- method_labels[[meth]]

    raw_list  <- vector("list", length(ts_seq))
    disp_list <- vector("list", length(ts_seq))

    for (k in seq_along(ts_seq)) {
      i <- ts_seq[k]
      raw_list[[k]] <- simulate_E(
        seed = seed, np = np, nd = nd, ne = ne,
        dist = dist, rate = rate, mean = mean, sd = sd, max = max,
        TS = i, formula = meth, formula_dist = formula_dist,
        threshold = threshold, threshold_country = threshold_country
      )

      disp_df <- raw_list[[k]]$Disproportionality_per_elec
      disp_df$method <- label
      disp_df$DM <- i
      disp_df$NP <- np
      disp_list[[k]] <- disp_df
    }

    lghi <- do.call(rbind, disp_list)

    if (model) {
      nls_fit <- tryCatch(
        stats::nls(
          GHI ~ C * exp(alpha * DM),
          start = list(C = start_C, alpha = start_alpha),
          data = lghi
        ),
        error = \(e) {
          warning(sprintf("NLS fit failed for method '%s': %s", label, e$message))
          NULL
        }
      )

      if (!is.null(nls_fit)) {
        lghi$GHI_predicted <- stats::predict(nls_fit)
        all_models[[paste0("Model_", label)]] <- nls_fit
      }
    }

    all_disp[[m_idx]] <- lghi
    all_raw[[m_idx]] <- raw_list
    names(all_raw)[m_idx] <- label
  }

  lghi_all <- do.call(rbind, all_disp)
  lghi_all <- lghi_all[gtools::mixedorder(lghi_all$ElectionID), ]
  rownames(lghi_all) <- NULL

  out <- c(list(summary = lghi_all), all_models, all_raw)
  out
}


# ----


#' Plot Aggregate-Level Disproportionality Measures
#'
#' Produces faceted plots of GHI, NPP, and ENPP across district magnitudes
#' and apportionment methods.
#'
#' @param data A list returned by \code{Disp2}.
#' @param methods Character vector: method labels to include in the plot
#'   (must match labels in the data).
#' @param vlines Numeric vector: x-axis positions for vertical reference lines.
#'   Default is \code{NULL} (no vertical lines). Values refer to factor levels
#'   on the DM axis.
#'
#' @return A list of \code{ggplot} objects: \code{plot_GHI}, \code{plot_NPP},
#'   \code{plot_ENPP}.
#'
#' @export
plot_Disp2 <- function(data = NULL,
                       methods = c("DH", "SL", "H", "Imperiali"),
                       vlines = NULL,
                       ...) {
  lghi_all <- data[["summary"]]
  lghi_all <- lghi_all[lghi_all$method %in% methods, ]

  base_theme <- ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.1, color = "red"),
      axis.line  = ggplot2::element_line(linewidth = 0.35, color = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, color = "black"),
      text = ggplot2::element_text(size = 12)
    )

  hlines_ghi <- ggplot2::geom_hline(
    yintercept = c(0.1, 0.05),
    linewidth = 0.35, linetype = "longdash", colour = "blue"
  )

  vlines_layer <- if (!is.null(vlines)) {
    ggplot2::geom_vline(
      xintercept = vlines,
      linewidth = 0.45, linetype = "longdash", colour = "green"
    )
  }

  ## GHI plot
  has_predicted <- "GHI_predicted" %in% names(lghi_all)

  plot_GHI <- ggplot2::ggplot(lghi_all) +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = as.factor(DM), y = GHI),
      lwd = 0.25, fatten = 0.4, outlier.size = 0.6
    ) +
    ggplot2::facet_wrap(~method) +
    ggplot2::xlab("DM") +
    ggplot2::ylab("GHI") +
    hlines_ghi +
    base_theme

  if (has_predicted) {
    plot_GHI <- plot_GHI +
      ggplot2::geom_line(
        ggplot2::aes(x = as.factor(DM), y = GHI_predicted, group = 1),
        linewidth = 0.35, colour = "blue"
      )
  }

  if (!is.null(vlines_layer)) {
    plot_GHI <- plot_GHI + vlines_layer
  }

  ## NPP plot
  plot_NPP <- ggplot2::ggplot(lghi_all) +
    ggplot2::geom_count(
      ggplot2::aes(x = DM, y = NPP),
      colour = "red", alpha = 0.7
    ) +
    ggplot2::facet_wrap(~method) +
    ggplot2::xlab("DM") + ggplot2::ylab("NPP") +
    base_theme

  ## ENPP plot
  plot_ENPP <- ggplot2::ggplot(lghi_all) +
    ggplot2::geom_count(
      ggplot2::aes(x = DM, y = ENPP),
      colour = "red", alpha = 0.8, shape = 1
    ) +
    ggplot2::facet_wrap(~method) +
    ggplot2::xlab("DM") + ggplot2::ylab("ENPP") +
    base_theme

  list(plot_GHI = plot_GHI, plot_NPP = plot_NPP, plot_ENPP = plot_ENPP)
}
