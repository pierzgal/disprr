# disprr 0.2.0

## New features

* Added a "Getting started with disprr" vignette (`vignette("disprr")`): a
  user guide covering seat apportionment, election simulation, and the
  disproportionality indexes.

## Bug fixes

* **Huntington-Hill / Adams tie-breaking**: these methods use a first divisor
  of 0, giving every party with positive votes an infinite first quotient.
  Ties among those infinite quotients are now broken in favour of the party
  with more votes. Previously they were broken by input order, so when seats
  were scarcer than parties the seats could go to arbitrary (or, inside the
  simulation, the smallest) parties.
* **`Disp2()` no longer aborts** when the NLS fit converges for some methods
  but fails for others. The `GHI_predicted` column is now always present
  (filled with `NA` on failure), keeping the per-method results
  `rbind`-compatible.
* `Disp2()` now accepts all eleven divisor-method codes (previously `"wb"`,
  `"jef"`, and `"hb"` triggered a "subscript out of bounds" error); an
  unknown code now raises an informative error.
* `simulate_Disp()` no longer produces `.x` / `.y` suffixed duplicate
  columns in its `sb_bw` output.
* `LR_Hamilton()$Seats` is now an integer vector, matching
  `divisorMethods()$Seats`.

## Methodological changes

* Aggregate disproportionality indexes (Gallagher, Loosemore-Hanby,
  Sainte-Laguë, ENPP) are now computed from full-precision vote and seat
  shares instead of values pre-rounded to three significant figures, so
  rounding error is no longer propagated into the indexes.
* `RSE2_i`, `meanRSE2`, and the Sainte-Laguë index now guard against
  division by zero for parties with a zero vote (or zero ideal) share;
  `RSE2_i` is reported as `NA` in that case rather than `Inf`/`NaN`.

## Other changes

* The country-level threshold (`.countryThreshold`) now excludes parties
  *strictly below* the threshold, matching the district-level convention in
  `divisorMethods()` and `LR_Hamilton()`. A party exactly at the threshold
  is retained.
* Removed unused `...` arguments from all exported functions, resolving the
  "Undocumented arguments" `R CMD check` warning.
* Removed dead code: the unused `p1` plot in `plot_Disp()`, the unused
  `distTS` aggregation in `simulate_E()`, and the unused `.method_label()`
  helper.


# disprr 0.1.0

## Major changes

* **Architecture overhaul**: eliminated `dplyr` and `data.table` dependencies;
  all internal code now uses base R (`merge`, `split`/`lapply`,
  `do.call(rbind, ...)`).
* **Input validation**: `divisorMethods()` and `LR_Hamilton()` now check for
  negative votes, invalid thresholds, mismatched vector lengths, and zero-seat
  requests, with informative error messages.
* **Massive deduplication**:
    - `.ProportionalRepresentation()`: collapsed 4 near-identical code blocks
      into a single flow (~230 lines reduced to ~50).
    - `Disp2()`: collapsed 8 copy-pasted loops (~400 lines) into a single
      parameterized loop (~50 lines). Added a `methods` parameter so users
      can choose which apportionment methods to simulate.
* **`Disp2()` new `methods` parameter**: users can now select a subset of
  methods to compare (default: all 8). Previously all methods were always run.
* **`plot_Disp2()` new `vlines` parameter**: replaces hardcoded vertical
  reference lines; set to `NULL` by default.
* **NLS robustness**: `Disp2()` wraps NLS model fitting in `tryCatch`, so
  convergence failures issue a warning instead of stopping the simulation.

## Bug fixes

* Fixed `.countryThreshold()` using `ifelse()` with assignment as side-effect,
  which could silently fail under certain conditions.
* Fixed `if.parties.null()` polluting the global random seed; it now saves and
  restores `.Random.seed` on exit.
* Fixed typo "Hungtinton-Hill" in method name string.

## Documentation

* Comprehensive roxygen2 documentation for all exported and internal functions.
* Documented mathematical equivalences between methods: Jefferson = D'Hondt =
  Hagenbach-Bischoff; Webster = Sainte-Lague.
* Working `@examples` sections for `divisorMethods()` and `LR_Hamilton()`.

## Testing

* Added a `testthat` test suite (70 tests) covering:
    - Known-answer verification for all 11 divisor methods and Hamilton-Hare.
    - Mathematical equivalence tests (jef = dh = hb, wb = sl).
    - Input validation and error handling.
    - Electoral threshold behavior.
    - Edge cases (single party, ties, zero-vote parties).

## Deprecated ggplot2 usage

* Replaced `fun.y` with `fun` in `stat_summary()`.
* Replaced `size` with `linewidth` in line-based geoms.

## Dependencies

* Removed: `dplyr`, `data.table`, `ggthemes`.
* Minimum R version raised to 4.1.0 (from 3.1.0).


# disprr 0.0.4

* Initial development version (research code).
