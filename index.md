---
layout: default
---

## Overview

**disprr** is an R package for simulating proportional representation elections
and measuring electoral disproportionality. It implements 12 apportionment
methods and provides a full simulation-to-analysis pipeline.

### Apportionment methods

| Code | Method | Family |
|:-----|:-------|:-------|
| `"dh"` | D'Hondt | Divisor (= Jefferson, Hagenbach-Bischoff) |
| `"sl"` | Sainte-Lague | Divisor (= Webster) |
| `"msl"` | Modified Sainte-Lague | Divisor (first divisor 1.4) |
| `"danish"` | Danish Sainte-Lague | Divisor (1, 4, 7, 10, ...) |
| `"hsl"` | Hungarian Sainte-Lague | Divisor (first divisor 1.5) |
| `"imperiali"` | Imperiali | Divisor (1, 1.5, 2, 2.5, ...) |
| `"hh"` | Huntington-Hill | Divisor (geometric mean) |
| `"ad"` | Adams | Divisor (smallest divisors) |
| `"hamilton"` | Hamilton-Hare | Largest remainder |

### Disproportionality indexes

- **GHI** -- Gallagher index (least squares)
- **LHI** -- Loosemore-Hanby index
- **SLI** -- Sainte-Lague index
- **ENPP** -- Effective number of parliamentary parties
- Per-party seat excess and bias measures

### Vote-distribution models

- **Independent log-normal** (also uniform, exponential) -- legacy count draws
- **Dirichlet, calibrated** -- Taagepera-Allik mean shares (realistic party systems)
- **Uniform on simplex** -- symmetric Dirichlet (Pukelsheim benchmark)

## Installation

```r
# install.packages("pak")
pak::pak("pierzgal/disprr")

# with the user-guide vignette:
remotes::install_github("pierzgal/disprr", build_vignettes = TRUE)
```

## Quick start

```r
library(disprr)

# Apportion 10 seats using D'Hondt
divisorMethods(
  parties = c("A", "B", "C", "D"),
  votes   = c(100000, 80000, 30000, 20000),
  seats   = 10,
  method  = "dh"
)

# Compare methods across district magnitudes
result <- Disp2(
  seed = 42, np = 5, ne = 100,
  minTS = 3, maxTS = 20,
  methods = c("dh", "sl", "hamilton", "hh")
)
plots <- plot_Disp2(data = result)
plots$plot_GHI
```

## Author

**Michal Pierzgalski** (University of Lodz)

## License

GPL (>= 3)
