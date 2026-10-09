## Generate sample CSVs for trying the app: a clean one and one with issues
## injected across many domains so flags and profile highlighting are visible.
## adppk uses CDISC ADPPK names (USUBJIDN, AFRLT, NFRLT, WTBL, PARAMN, ...);
## the app's variable mapping resolves these to canonical names.

d <- pharmaverseadam::adppk
readr::write_csv(d, "dev/adppk_sample.csv")

bad <- d
ids <- head(unique(bad$USUBJIDN), 8)
dose_rows <- which(bad$EVID %in% c(1, 4))
obs_rows <- function(id) which(bad$EVID == 0 & bad$USUBJIDN == id)

# STRUCT-003: invalid EVID
bad$EVID[obs_rows(ids[1])[1]] <- 9

# DOSE-001: zero dose amount on a dose record
bad$AMT[dose_rows[1]] <- 0

# DOSE-002: extreme dose amount (likely unit error)
bad$AMT[dose_rows[2]] <- bad$AMT[dose_rows[2]] * 1000

# TIME-001: decreasing time within a subject (swap two obs times)
o1 <- obs_rows(ids[2])
if (length(o1) >= 2) bad$AFRLT[o1[1:2]] <- bad$AFRLT[o1[2:1]]

# TIME-003: negative time on a predose-like record
o2 <- obs_rows(ids[3])
if (length(o2) >= 1) bad$AFRLT[o2[1]] <- -1

# OBS-004: high predose concentration
if (length(o2) >= 1) bad$DV[o2[1]] <- max(bad$DV, na.rm = TRUE)

# OBS-005: extreme concentration outlier
o3 <- which(bad$EVID == 0 & bad$DV > 0)[1]
bad$DV[o3] <- bad$DV[o3] * 1e4

# OBS-001: MDV=0 but DV missing
o4 <- obs_rows(ids[4])[1]
if (!is.na(o4)) {
  bad$MDV[o4] <- 0
  bad$DV[o4] <- NA
}

# COV-001: inconsistent fixed covariate (SEX) within a subject
s5 <- which(bad$USUBJIDN == ids[5])
if (length(s5) >= 2) bad$SEX[s5[2]] <- setdiff(c("M", "F"), bad$SEX[s5[1]])[1]

# COV-004 / COV-005: implausible baseline weight
bad$WTBL[bad$USUBJIDN == ids[6]] <- -50

# DOSE-003: subject with observations but no dose
bad <- bad[!(bad$USUBJIDN == ids[7] & bad$EVID %in% c(1, 4)), ]

# STRUCT-005: duplicate a record
bad <- rbind(bad, bad[obs_rows(ids[8])[1], ])

# MR-001: exclusion flag without a reason
bad$EXCLFL <- ""
bad$EXCLFL[obs_rows(ids[1])[2]] <- "Y"

readr::write_csv(bad, "dev/adppk_corrupted_sample.csv")
cat("wrote dev/adppk_sample.csv and dev/adppk_corrupted_sample.csv\n")
