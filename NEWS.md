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
