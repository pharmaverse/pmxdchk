# Small constructed NMPK datasets for unit tests. Independent of the bundled
# example data so checks can be tested without pharmaverseadam. Each builder
# represents a different kind of dataset so the suite covers every check.

# Two subjects, one dose + three observations each, no issues.
make_clean_nmpk <- function() {
  tibble::tibble(
    ID = rep(1:2, each = 4),
    TIME = rep(c(0, 1, 2, 4), 2),
    EVID = rep(c(1, 0, 0, 0), 2),
    MDV = rep(c(1, 0, 0, 0), 2),
    DV = c(0, 10, 8, 5, 0, 12, 9, 6),
    AMT = rep(c(100, 0, 0, 0), 2)
  )
}

# Clean dataset with one duplicated key (ID/TIME/EVID).
make_duplicate_nmpk <- function() {
  dplyr::bind_rows(make_clean_nmpk(), make_clean_nmpk()[2, ])
}

# Clean multiple-dose dataset: ADDL/II/SS/OCC present, nominal time present,
# constant covariates. Serves as the base for most dirty fixtures.
make_multidose_nmpk <- function() {
  obs_times <- c(0.5, 1, 2, 4, 8)
  build_subject <- function(id) {
    dose <- tibble::tibble(
      ID = id, TIME = 0, NTIME = 0, EVID = 1, MDV = 1, DV = 0, AMT = 100,
      ADDL = 3, II = 24, SS = 0, OCC = 1,
      SEX = id %% 2, AGE = 40 + id, WT = 70 + id
    )
    obs <- tibble::tibble(
      ID = id, TIME = obs_times, NTIME = obs_times, EVID = 0, MDV = 0,
      DV = round(10 * exp(-0.15 * obs_times), 3), AMT = 0,
      ADDL = NA_real_, II = NA_real_, SS = NA_real_, OCC = 1,
      SEX = id %% 2, AGE = 40 + id, WT = 70 + id
    )
    dplyr::bind_rows(dose, obs)
  }
  dplyr::bind_rows(lapply(1:6, build_subject))
}

# IV dataset: adds RATE/DUR to the multiple-dose base.
make_iv_nmpk <- function() {
  d <- make_multidose_nmpk()
  d$RATE <- ifelse(d$EVID == 1, 10, 0)
  d$DUR <- ifelse(d$EVID == 1, 10, NA_real_)
  d
}

# Multi-analyte dataset: duplicates observations across two DVID groups.
make_multianalyte_nmpk <- function() {
  d <- make_multidose_nmpk()
  d$DVID <- ifelse(d$EVID == 0, 1, NA_real_)
  obs2 <- d[d$EVID == 0, ]
  obs2$DVID <- 2
  obs2$DV <- obs2$DV * 0.5
  dplyr::bind_rows(d, obs2)
}

# Integrated dataset: two studies via STUDYID.
make_integrated_nmpk <- function() {
  d <- make_multidose_nmpk()
  d$STUDYID <- ifelse(d$ID <= 3, "S1", "S2")
  d
}

# Single-dose dataset with continuous covariates for covariate checks.
make_cov_nmpk <- function() {
  tibble::tibble(
    ID = 1:6,
    TIME = 0,
    EVID = 1,
    MDV = 1,
    DV = 0,
    AMT = 100,
    AGE = c(40, 42, 38, 45, 41, 39)
  )
}
