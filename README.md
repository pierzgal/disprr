# disprr

Simulate proportional representation election results and compute
disproportionality measures.

## Overview

**disprr** provides tools for:

- **Seat apportionment** using 11 divisor methods (D'Hondt, Sainte-Lague,
  Webster, Huntington-Hill, Adams, and others) plus the Hamilton-Hare largest
  remainder method.
- **Election simulation** with configurable vote distributions -- independent
  counts (uniform, log-normal, exponential) or compositional Dirichlet models:
  calibrated via the Taagepera-Allik rule (realistic party systems) or uniform
  on the simplex (the Pukelsheim benchmark) -- plus multi-district scenarios
  and electoral thresholds.
- **Disproportionality analysis** including per-party seat excess measures and
  aggregate indexes (Gallagher, Loosemore-Hanby, Sainte-Lague, ENPP).
- **Visualization** of disproportionality across district magnitudes and
  apportionment methods.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("pierzgal/disprr")
```

To include the user-guide vignette, use `remotes` instead:

```r
# install.packages("remotes")
remotes::install_github("pierzgal/disprr", build_vignettes = TRUE)
vignette("disprr")   # open the user guide
```

## Quick start

### Apportion seats

```r
library(disprr)

# D'Hondt method
divisorMethods(
  parties = c("A", "B", "C", "D"),
  votes   = c(100000, 80000, 30000, 20000),
  seats   = 10,
  method  = "dh"
)

# Sainte-Lague method
divisorMethods(
  parties = c("A", "B", "C", "D"),
  votes   = c(100000, 80000, 30000, 20000),
  seats   = 10,
  method  = "sl"
)

# Hamilton-Hare largest remainder
LR_Hamilton(
  parties = c("A", "B", "C", "D"),
  votes   = c(100000, 80000, 30000, 20000),
  seats   = 10
)
```

### Available apportionment methods

| Code         | Method                    | Notes                                |
|:-------------|:--------------------------|:-------------------------------------|
| `"dh"`       | D'Hondt                   | = Jefferson (`"jef"`) = H-B (`"hb"`) |
| `"sl"`       | Sainte-Lague              | = Webster (`"wb"`)                   |
| `"msl"`      | Modified Sainte-Lague     | First divisor 1.4                    |
| `"danish"`   | Danish Sainte-Lague       | Divisors: 1, 4, 7, 10, ...          |
| `"hsl"`      | Hungarian Sainte-Lague    | First divisor 1.5                    |
| `"imperiali"`| Imperiali (divisor)       | Not the Imperiali quota              |
| `"hh"`       | Huntington-Hill           | U.S. House apportionment             |
| `"ad"`       | Adams                     | Smallest divisors                    |
| `"wb"`       | Webster                   | Equivalent to Sainte-Lague           |
| `"jef"`      | Jefferson                 | Equivalent to D'Hondt                |
| `"hb"`       | Hagenbach-Bischoff        | Equivalent to D'Hondt                |
| `"hamilton"` | Hamilton-Hare (LR)        | Via `LR_Hamilton()`                  |

### Run a simulation

```r
# Simulate 100 elections with 5 parties, comparing methods
result <- Disp2(
  seed = 42,
  np   = 5,
  ne   = 100,
  minTS = 3,
  maxTS = 20,
  methods = c("dh", "sl", "hamilton", "hh")
)

# Plot Gallagher index across district magnitudes
plots <- plot_Disp2(data = result)
plots$plot_GHI
```

### Vote-distribution models

```r
# Expected vote shares for a 5-party system (Taagepera-Allik rule)
taagepera_allik(5)

# Simulate with a realistic (calibrated Dirichlet) party system
result_dir <- Disp2(
  seed = 42, np = 5, ne = 100,
  dist = "dirichlet", phi = 20, votes_per_district = 100000,
  methods = c("dh", "sl", "hamilton", "hh")
)

# Or with the uniform-on-simplex benchmark
result_us <- Disp2(
  seed = 42, np = 5, ne = 100,
  dist = "uniform_simplex", votes_per_district = 100000,
  methods = c("dh", "sl", "hamilton", "hh")
)
```

## Testing

Run the test suite:

```r
devtools::test()
```

## Dependencies

- **ggplot2** (>= 3.4.0) -- visualization
- **gtools** (>= 3.5.0) -- mixed sorting
- **psych** (>= 2.0.0) -- descriptive statistics
- **truncdist** (>= 1.0-2) -- truncated distributions for simulation
- **viridis** (>= 0.6.0) -- color palettes

## Citation

If you use this package in research, please cite:

```
Pierzgalski, M. (2026). disprr: Simulate Proportional Representation
Election Results and Compute Disproportionality Measures. R package
version 0.3.1. https://github.com/pierzgal/disprr
```

## License

GPL (>= 3)
