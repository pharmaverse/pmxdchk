# pmxdchk

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pharmaverse/pmxdchk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pharmaverse/pmxdchk/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`pmxdchk` is an open-source R package (part of the [pharmaverse](https://pharmaverse.org/)) for checking the quality of NONMEM-style and ADPPK pharmacometric datasets before modeling. It provides:

- **41 data checks** covering dataset structure, time, dosing, observations, covariates, compartments, occasions, and pooled studies. Each check states the rule it applies and what to do when it flags.
- **A Shiny app** to load a dataset, look at it, work through the findings with the affected records and subject profiles next to each one, record a decision per finding, and export the results.
- **A headless entry point**, `run_nmpk_checks()`, that runs the same checks from a script.

## Try it in your browser

**<https://pharmaverse.github.io/pmxdchk/>**

No installation needed. The demo runs entirely in your browser (WebAssembly): data you load are not uploaded to any server. The first load takes about a minute. Click *Example with issues*, *Confirm mapping*, then *Run checks*.

## Installation

```r
# Install the development version from GitHub
# install.packages("pak")
pak::pak("pharmaverse/pmxdchk")
```

## Usage

### In the app

```r
library(pmxdchk)
run_app()
```

1. **Data** — upload a delimited text file (`.csv`, `.txt`, `.dat`) or load a built-in example, confirm the variable mapping and the inferred study type, and run the checks. CDISC ADPPK names such as `USUBJIDN`, `AFRLT`, and `WTBL` are mapped to NONMEM names automatically.
2. **Overview** — counts, concentration-time profiles of all subjects, dose levels, and missing values.
3. **Findings** — the checks to review, most severe first. Each shows its rule, the affected records, the profile of each affected subject, and what to do. Decisions and comments are exported with the findings as CSV.
4. **Profiles** — any subject's profile and records, with flagged records marked.
5. **Check library** — every check and its rule.

The app runs locally: your data stay on your machine.

### From a script

```r
library(pmxdchk)

findings <- run_nmpk_checks(
  adppk_corrupted,
  mapping = c(ID = "USUBJIDN", TIME = "AFRLT", NTIME = "NFRLT", WT = "WTBL"),
  study_type = c("All", "Multiple Dose")
)
findings[findings$status == "flag", c("check_id", "severity", "message")]

# The rule and guidance of every check
check_catalogue()
```

Two example datasets are included: `adppk_example` (from [`pharmaverseadam`](https://pharmaverse.github.io/pharmaverseadam/)) and `adppk_corrupted`, the same data with known issues injected.

## Documentation

- [`docs/check_reference.html`](docs/check_reference.html) — what each check flags, generated from the package (`dev/make_check_reference.R`).
- [`NEWS.md`](NEWS.md) — changes.

## Not yet available

- PDF or HTML check reports.
- Reading SAS transport (`.xpt`) files.
- Reconciliation against source ADaM / SDTM datasets (checklist items `CORE-SRC-*`).

## Development

```r
devtools::load_all()
devtools::test()
```

The package is a [golem](https://thinkr-open.github.io/golem/) application. Each check domain has a `R/checks_<domain>.R` file of pure functions with matching tests in `tests/testthat/`; checks are registered, with their rule and guidance, in `R/check_registry.R`. The Shiny modules are in `R/mod_*.R`.

`dev/make_shinylive.R` builds a browser-only copy of the app (WebAssembly, no server) for static hosting; the `shinylive` workflow publishes it to GitHub Pages.

## License

[Apache License 2.0](LICENSE.md)
