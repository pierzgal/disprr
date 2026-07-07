# Column names used inside ggplot2::aes() calls in plot_Disp() / plot_Disp2().
# aes() evaluates them in the data frame at plot time (non-standard
# evaluation), so R CMD check cannot see a binding and would flag each one
# as an undefined global variable without this declaration.
utils::globalVariables(c(
  "Party", "Seats", "VoteShare", "distTS", "TS", "SeatShare",
  "SB1_i", "SB2_i", "ESB1", "TV",
  "DM", "GHI", "GHI_predicted", "NPP", "ENPP"
))
