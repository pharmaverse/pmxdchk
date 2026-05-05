# pmxdchk <img src="inst/app/www/favicon.ico" align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pharmaverse/pmxdchk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pharmaverse/pmxdchk/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`pmxdchk` is an open-source R package (part of the [pharmaverse](https://pharmaverse.org/)) that provides an interactive Shiny application for:

- **Data visualization** — individual and population-level PK concentration-time profiles for NONMEM and ADPPK datasets
- **Data quality checking** — comprehensive, configurable checks for common pharmacometric data issues
- **PDF reporting** — exportable check reports with user-selectable checks

## Installation

```r
# Install the development version from GitHub
# install.packages("pak")
pak::pak("pharmaverse/pmxdchk")
```

## Usage

```r
library(pmxdchk)

# Launch the Shiny app
run_app()
```

The app supports file upload of:
- **ADPPK** datasets (CDISC ADaM format, `.xpt` / `.csv`)
- **NONMEM-style** datasets (`.csv`)

A built-in example dataset (`adppk_example`, sourced from [`pharmaverseadam`](https://pharmaverse.github.io/pharmaverseadam/)) is available to explore the app without uploading data.

## Features

### Data Upload & Preview
Upload your dataset or load the built-in example. Preview columns, types, and basic summaries before running checks.

### Visualization
Interactive PK concentration-time plots including individual profiles and population-level overlays.

### Data Checks
Configurable checks organized by domain. Select which checks to run and review flagged issues in an interactive table. Check domains are under active development — see [Issues](https://github.com/pharmaverse/pmxdchk/issues) for the planned check list.

### PDF Report
Generate a PDF report of selected check results, suitable for inclusion in study documentation.

## Project Structure

```
pmxdchk/
├── R/
│   ├── app_config.R               # golem app configuration
│   ├── app_ui.R                   # Top-level UI
│   ├── app_server.R               # Top-level server
│   ├── run_app.R                  # Exported run_app() launcher
│   ├── mod_data_upload.R          # Module: data upload & preview
│   ├── mod_visualization.R        # Module: PK plots
│   ├── mod_report.R               # Module: PDF export
│   ├── mod_check_<domain>.R       # Module UI/server per check domain
│   ├── checks_<domain>.R          # Standalone check functions per domain
│   └── utils.R                    # Shared utilities
├── inst/
│   ├── app/www/                   # Static Shiny assets
│   └── report_templates/
│       └── check_report.Rmd       # PDF report template
├── tests/testthat/                # Unit tests for check functions
├── vignettes/                     # User-facing documentation
├── data/                          # Built-in example datasets
├── data-raw/                      # Scripts to generate example datasets
├── documents/                     # Project reference documents
└── .github/workflows/             # CI/CD pipelines
```

## pharmaverse

`pmxdchk` is part of the [pharmaverse](https://pharmaverse.org/), a collection of R packages for clinical reporting. It follows pharmaverse conventions for package structure, documentation, and testing.

## License

[Apache License 2.0](LICENSE)

## Code of Conduct

Please note that the `pmxdchk` project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
