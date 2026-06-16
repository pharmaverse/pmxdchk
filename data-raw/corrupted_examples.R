## Script to build intentionally corrupted example datasets.
##
## Clean data alone cannot demonstrate the checks, so this derives a small set
## of subjects from adppk_example and injects known issues for demos, the
## vignette, and integration tests. Each issue is documented inline with the
## check it is meant to trigger.
##
## adppk uses CDISC ADPPK names; the canonical NONMEM mapping is roughly
## ID = USUBJIDN, TIME = AFRLT, plus EVID, MDV, DV, AMT, CMT as-is.

adppk_example <- pharmaverseadam::adppk

# Keep it small: the first few subjects only.
keep_ids <- head(unique(adppk_example$USUBJIDN), 5)
adppk_corrupted <- adppk_example[adppk_example$USUBJIDN %in% keep_ids, ]
adppk_corrupted <- adppk_corrupted[order(adppk_corrupted$USUBJIDN, adppk_corrupted$AFRLT), ]

# CORE-STRUCT-005: duplicate an existing record (duplicate key).
adppk_corrupted <- rbind(adppk_corrupted, adppk_corrupted[2, ])

# CORE-TIME-001: make TIME decrease within the first subject by swapping two
# adjacent observation times.
obs_rows <- which(adppk_corrupted$EVID == 0 & adppk_corrupted$USUBJIDN == keep_ids[1])
if (length(obs_rows) >= 2) {
  i <- obs_rows[1]
  j <- obs_rows[2]
  adppk_corrupted$AFRLT[c(i, j)] <- adppk_corrupted$AFRLT[c(j, i)]
}

# CORE-COV-005: implausible covariate (negative baseline weight).
adppk_corrupted$WTBL[adppk_corrupted$USUBJIDN == keep_ids[2]] <- -50

# CORE-OBS-003: BLQ in the middle of a profile (zero an interior observation).
obs_rows2 <- which(adppk_corrupted$EVID == 0 & adppk_corrupted$USUBJIDN == keep_ids[3])
if (length(obs_rows2) >= 3) {
  mid <- obs_rows2[ceiling(length(obs_rows2) / 2)]
  adppk_corrupted$DV[mid] <- 0
}

usethis::use_data(adppk_corrupted, overwrite = TRUE)
