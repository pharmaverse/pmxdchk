## Build the intentionally corrupted example dataset shipped in data/.
##
## Clean data alone cannot demonstrate the checks, so this derives a variant of
## the full adppk dataset with known issues injected across many domains. Each
## injection is annotated with the check it is meant to trigger. adppk uses
## CDISC ADPPK names (USUBJIDN ~ ID, AFRLT ~ TIME, NFRLT ~ NTIME, WTBL ~ WT,
## PARAMN ~ DVID); the app's variable mapping resolves these to canonical names.

adppk_corrupted <- pharmaverseadam::adppk

ids <- head(unique(adppk_corrupted$USUBJIDN), 8)
dose_rows <- which(adppk_corrupted$EVID %in% c(1, 4))
obs_rows <- function(id) which(adppk_corrupted$EVID == 0 & adppk_corrupted$USUBJIDN == id)

# CORE-STRUCT-003: invalid EVID value.
adppk_corrupted$EVID[obs_rows(ids[1])[1]] <- 9

# CORE-DOSE-001: zero dose amount on a dose record.
adppk_corrupted$AMT[dose_rows[1]] <- 0

# CORE-DOSE-002: extreme dose amount (likely unit error).
adppk_corrupted$AMT[dose_rows[2]] <- adppk_corrupted$AMT[dose_rows[2]] * 1000

# CORE-TIME-001: decreasing time within a subject (swap two observation times).
o1 <- obs_rows(ids[2])
if (length(o1) >= 2) adppk_corrupted$AFRLT[o1[1:2]] <- adppk_corrupted$AFRLT[o1[2:1]]

# CORE-OBS-004: unusually high predose concentration.
o2 <- obs_rows(ids[3])
if (length(o2) >= 1) adppk_corrupted$DV[o2[1]] <- max(adppk_corrupted$DV, na.rm = TRUE)

# CORE-OBS-005: extreme concentration outlier.
o3 <- which(adppk_corrupted$EVID == 0 & adppk_corrupted$DV > 0)[1]
adppk_corrupted$DV[o3] <- adppk_corrupted$DV[o3] * 1e4

# CORE-OBS-001: MDV = 0 but DV missing.
o4 <- obs_rows(ids[4])[1]
if (!is.na(o4)) {
  adppk_corrupted$MDV[o4] <- 0
  adppk_corrupted$DV[o4] <- NA
}

# CORE-COV-001: inconsistent fixed covariate (SEX) within a subject.
s5 <- which(adppk_corrupted$USUBJIDN == ids[5])
if (length(s5) >= 2) {
  adppk_corrupted$SEX[s5[2]] <- setdiff(c("M", "F"), adppk_corrupted$SEX[s5[1]])[1]
}

# CORE-COV-004 / CORE-COV-005: implausible baseline weight.
adppk_corrupted$WTBL[adppk_corrupted$USUBJIDN == ids[6]] <- -50

# CORE-DOSE-003: subject with observations but no dose.
adppk_corrupted <- adppk_corrupted[
  !(adppk_corrupted$USUBJIDN == ids[7] & adppk_corrupted$EVID %in% c(1, 4)),
]

# CORE-STRUCT-005: duplicate a record.
adppk_corrupted <- rbind(adppk_corrupted, adppk_corrupted[obs_rows(ids[8])[1], ])

# CORE-MR-001: exclusion flag without a reason.
adppk_corrupted$EXCLFL <- ""
adppk_corrupted$EXCLFL[obs_rows(ids[1])[2]] <- "Y"

usethis::use_data(adppk_corrupted, overwrite = TRUE)
