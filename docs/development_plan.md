# pmxdchk Development Plan

Source of truth for the check catalogue: `docs/NMPK_QC_Checklist.xlsx`
(sheets: *Public Core Checks*, *Enhanced AI Checks*, *Study Type Map*, *Check Mapping*).

This plan covers the **Public Core** tier only. The **Enhanced AI-Assisted**
tier (24 `ENH-*` checks requiring protocol / data-specification parsing) is
explicitly **out of scope** for now and will be planned separately.

**Guiding goal: this is a *review* tool, not just a *check* tool.** Value comes
less from how many checks run and more from how fast a pharmacometrician can
understand, locate, and triage issues in a NONMEM dataset. The checklist
remains the catalogue of *what* we detect; this plan defines *how a human works
with the results*.

## 1. Scope decisions

| Decision | Choice |
|---|---|
| MVP scope | A complete **review loop** end-to-end (see §7), not a count of checks |
| Variable mapping | Auto-guess + manual confirmation; auto-guess backed by a limited dictionary of ADPPK / CDISC standard variable names |
| File formats | CSV only for now (`.xpt`/`haven` deferred) |
| Enhanced AI tier | Deferred — not in this plan |
| Checklist Excel | Kept as-is as the catalogue; workflow/presentation decisions live here, not in the spreadsheet |

## 2. Design principles

1. **Review-first.** Every flag must be shown *in context* — never a bare bad
   row. The reviewer drills summary → subject → records → plot.
2. **False positives are the enemy.** The checklist itself repeatedly says
   "review flag, not error" / "scientist review" / "do not auto-label". A tool
   that cries wolf gets abandoned. Hence: clear pass/flag/skip states,
   configurable thresholds, and per-finding triage (accept / reject / comment).
3. **Standardized check-result object.** Every check returns one S3 structure;
   UI and report only consume that structure.
4. **Headless-capable.** The engine runs without Shiny. A single exported
   `run_nmpk_checks(data, mapping, thresholds)` returns a tidy findings tibble,
   so users can run checks in their own scripts or CI, not only in the app.
5. **Variable mapping layer.** Canonical NONMEM/ADPPK names ↔ user columns; the
   checklist assumes "after variable mapping" throughout.
6. **Study-type inference first.** `CORE-STUDYTYPE-001` runs before everything
   and drives default check selection.

## 3. Product surfaces

The app is organized around what a reviewer *does*, not 1:1 around the 47
checks. Several checks aggregate into a surface rather than getting their own
tab.

### 3.1 Overview dashboard (landing page)
First thing shown after upload + mapping. Answers "what is this dataset?" at a
glance: subject / record / dose / observation counts, EVID×MDV×CMT cross-tab,
BLQ %, missingness %, DV log-distribution, dose distribution, inferred study
type. Surfaces `CORE-STRUCT-004` and `CORE-STRUCT-006` here instead of as buried
checks. This single page covers most of the initial "get oriented" need.

### 3.2 Individual profile browser (the core review surface)
Per-subject view — pharmacometricians think per subject. Elevates
`CORE-MR-003` from one check to a primary tab:
- Concentration–time plot (log/linear toggle) + dose markers + the subject's
  event table.
- Flags overlaid on plot and table (which record was flagged by which check).
- Prev/next navigation, with an option to step only through flagged subjects.
- Optional ADDL/II expansion view to inspect the implied full dosing sequence.

### 3.3 Findings + triage
The list of all findings (one row per finding). For each: severity, check, the
flagged records/subjects, link into the profile browser. Per-finding triage
state — **accept / reject (false positive) / comment** — persisted for the
session and exportable. The output of a review session is a *triaged issue
list* that can be handed to data programmers. (This concept is not in the
checklist; it is the single biggest practicality win for "review".)

### 3.4 Reports & export
- **CSV export** of flagged records and the triaged findings list — the daily
  hand-off to programmers.
- **PDF report** (`inst/report_templates/check_report.Rmd`) for archiving:
  sectioned by domain, ordered by severity, with flagged-record tables.
- **Audit trail** embedded in both: checks run, thresholds used, package
  version, timestamp, source file name (supports the checklist's ICH E9/E6
  traceability references).

### 3.5 Settings
Thresholds and severity filters as a first-class panel: outlier cutoffs,
minimum quantifiable record count N, which severities to include in the report.
Conservative defaults; everything passed as parameters into check functions
(never hard-coded).

## 4. Check domains → file mapping

Each domain follows the `checks_<domain>.R` (pure functions) +
`mod_check_<domain>.R` (Shiny module) pairing from `CLAUDE.md`. Note that some
domains feed a product surface (§3) rather than a standalone module — e.g.
STRUCT-004/006 feed the Overview, MR-003 feeds the Profile browser.

| Domain (`<domain>`) | Check IDs | n | Study type | Phase |
|---|---|---|---|---|
| `studytype` | CORE-STUDYTYPE-001 | 1 | All (first) | 1 |
| `structure` | CORE-STRUCT-001..007 | 7 | All | 2 |
| `time` | CORE-TIME-001..004 | 4 | All | 2 |
| `dosing` | CORE-DOSE-001..007 | 7 | All / Multiple / IV | 2 / 3 |
| `observations` | CORE-OBS-001..006 | 6 | All / Single / Multiple | 2 |
| `covariates` | CORE-COV-001..006 | 6 | All | 2 |
| `readiness` | CORE-MR-001..003 | 3 | All | 2 |
| `occasion` | CORE-OCC-001 | 1 | Multiple / Integrated | 3 |
| `compartment` | CORE-CMP-001..003 | 3 | Multi-analyte | 3 |
| `integrated` | CORE-INT-001..003 | 3 | Integrated | 3 |
| `source` | CORE-SRC-000..005 | 6 | Needs source ADaM/SDTM | 4 |

`structure` is the dependency root: `CORE-STRUCT-001` (core variable presence),
`-002` (numeric parsability), `-003` (EVID validity) are prerequisites for most
other checks. They run first and short-circuit: if a prerequisite fails,
downstream checks are marked `skip`.

## 5. Check engine contract (build first)

`R/check_result.R` — standardized result object:

```r
new_check_result(
  check_id, title, domain, severity,   # metadata
  status,        # "pass" | "flag" | "skip" | "error"
  n_flagged,     # number of flagged records / subjects
  summary_table = NULL,    # tibble  -> output type "summary_table"
  flagged_records = NULL,  # tibble  -> "flagged_records"
  subject_list = NULL,     # tibble/character -> "subject_list"
  plot_data = NULL,        # tibble  -> "plot_data" (for the profile browser)
  message                  # one-line human-readable conclusion
)
```

`R/check_registry.R` — one metadata entry per check, taken directly from the
checklist columns:

```r
register_check(
  id          = "CORE-STRUCT-005",
  fn          = check_struct_duplicate_records,
  min_vars    = c("ID", "TIME", "EVID"),
  study_types = "All",
  severity    = "High",
  requires    = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001"),
  skip_if     = ...   # skip condition predicate
)
```

`R/run_checks.R` — runner: topologically sort by `requires` → evaluate skip on
mapped variables → execute (passing `thresholds`) → aggregate. Adding a check
becomes "write one pure function + register one row"; no UI change required.

**Headless entry point** — exported, the same path the app uses:

```r
run_nmpk_checks(data, mapping = NULL, thresholds = default_thresholds())
#> returns a tidy findings tibble: one row per finding
#>   check_id | domain | severity | status | subject | n_flagged | message
```

The rich `check_result` objects are also returned (as a list attribute) for the
app; the flat tibble is what scripts, exports, and tests filter on.

## 6. Variable mapping

- Canonical names: NONMEM/ADPPK core set — `ID, TIME, EVID, MDV, DV, AMT, CMT,
  RATE, DUR, II, ADDL, SS, DVID, OCC/DOSNO/PERIOD, STUDYID`, plus common
  covariates (`AGE, WT/BW, BMI, SEX, RACE, ...`).
- Auto-guess: match user columns (case-insensitive) against a built-in
  dictionary of ADPPK / CDISC standard variable names and common aliases
  (`USUBJID`→`ID`, `TAFD`/`NTLD`→time variables, etc.). Limited lookup, no
  fuzzy ML.
- **Frictionless path**: most NMPK datasets already use NONMEM-standard names.
  When all canonical variables auto-resolve, show a confirmation summary with a
  one-click "looks right, continue" — do not force manual remapping.
- Manual confirmation: a mapping UI (canonical name → dropdown of user columns)
  pre-filled with guesses; the user overrides only what is wrong.
- The confirmed mapping is a reactive passed to the runner; all checks operate
  on mapped canonical names.

## 7. Phased roadmap

### Phase 0 — Infrastructure
- `DESCRIPTION`: add real imports (`dplyr, tidyr, purrr, rlang, DT, ggplot2,
  readr`); set a real author.
- Implement the engine (§5): result object, registry, runner, the headless
  `run_nmpk_checks()`, and `default_thresholds()`.
- Run `data-raw/adppk_example.R` to build the bundled example dataset, **plus a
  few intentionally corrupted variants** (known TIME disorder, duplicate
  records, implausible covariate, BLQ-in-middle) for demos, tests, and the
  vignette — clean data alone can't demonstrate value.
- Done when: a dummy check runs through `run_nmpk_checks()` against the example
  data and returns a standard findings tibble.

### Phase 1 — Data ingestion + variable mapping
- `mod_data_upload`: CSV upload, preview (DT), variable mapping UI (§6) with the
  frictionless path.
- `checks_studytype.R` + `mod_check_studytype`: `CORE-STUDYTYPE-001` —
  inference per *Study Type Map* + user confirmation.

### Phase 2 — MVP review loop ← first end-to-end target
Not "22 checks" but a *usable review loop*. A small set of high-value checks
plus the surfaces that make them reviewable:

- **Overview dashboard** (§3.1) — STRUCT-004/006 + dataset summary.
- **Individual profile browser** (§3.2) — MR-003.
- **High-value checks**: STRUCT-001..006, TIME-001, DOSE-001, OBS-001, OBS-005
  (outliers), COV-004 (continuous outliers).
- **Findings + triage** (§3.3) with **CSV export**.
- **Settings** (§3.5) for the thresholds those checks use.

Loop: upload → map → infer type → Overview → run checks → browse flagged
subjects → triage → export. Each `checks_*.R` ships with
`tests/testthat/test-checks_*.R` exercising pass / flag / skip on the corrupted
fixtures. **Get this in front of a real reviewer before adding breadth.**

### Phase 3 — Breadth: remaining core + conditional checks
Fill in the rest of `structure`/`time`/`dosing`/`observations`/`covariates`,
then type-specific domains: `occasion` (OCC-001), `compartment`
(CMP-001..003), `integrated` (INT-001..003), `dosing` 005/006/007. Default
selection gated by inferred study type.

### Phase 4 — Source traceability
`source` (SRC-001..005): second dataset upload (ADSL/ADPC/ADEX...) and
reconciliation. Depends on Phase 1 SRC-000 role detection.

### Phase 5 — PDF report polish
`mod_report` + `check_report.Rmd`: user selects checks to include → render PDF
with the audit trail (§3.4). (Interactive triage and CSV export already exist
from Phase 2; this phase is archival reporting.)

### Phase 6 — Enhanced AI tier (out of current scope)
All `ENH-*` checks require protocol/spec parsing. Keep as a separate
`checks_enh_*.R` set plus a document-parsing adapter, fully decoupled from
Public Core.

## 8. Cross-cutting: performance, testing, CI

- **Performance**: integrated/pooled datasets can be millions of rows. Avoid
  row-wise logic in checks; paginate/sample DT rendering; never render a full
  table to the browser.
- **Testing**: every exported `checks_*.R` function has a `test-checks_*.R`
  (per `CLAUDE.md`), driven by the corrupted fixtures; coverage target > 80%.
  Pure functions tested without Shiny; key UI paths use `shinytest2`.
- **CI**: populate `.github/workflows/` (currently empty): R-CMD-check,
  test-coverage, lint, pkgdown.
