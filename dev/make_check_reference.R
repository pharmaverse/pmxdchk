## Builds docs/check_reference.html from the check registry, so the reference
## always states the rules the package actually applies. Re-run after changing a
## check: source("dev/make_check_reference.R") from the package root.

devtools::load_all(quiet = TRUE)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

domains <- c(
  studytype = "Study type",
  structure = "Dataset structure",
  time = "Time",
  dosing = "Dosing",
  observations = "Observations",
  covariates = "Covariates",
  compartment = "Compartment and analyte",
  occasion = "Occasion",
  integrated = "Pooled studies",
  readiness = "Modeling readiness"
)

registry <- check_registry()
defaults <- default_thresholds()

check_row <- function(spec) {
  types <- setdiff(spec$study_types, "All")
  runs <- if ("All" %in% spec$study_types) "Always" else {
    paste(types, collapse = ", ")
  }
  needs <- if (length(spec$min_vars) > 0) {
    paste0("<br><span class=\"muted\">Needs ",
           esc(paste(spec$min_vars, collapse = ", ")), "</span>")
  } else {
    ""
  }
  sprintf(
    paste0(
      "<tr><td><strong>%s</strong><br><code>%s</code></td>",
      "<td><span class=\"sev %s\">%s</span></td>",
      "<td>%s<p class=\"todo\"><span class=\"muted\">What to do:</span> %s</p></td>",
      "<td>%s%s</td></tr>"
    ),
    esc(spec$title), spec$id, tolower(spec$severity), spec$severity,
    esc(spec$rule), esc(spec$guidance), runs, needs
  )
}

sections <- vapply(names(domains), function(domain) {
  specs <- Filter(function(s) s$domain == domain, registry)
  specs <- specs[order(names(specs))]
  paste0(
    "<h2>", domains[[domain]], "</h2>\n<table>\n<thead><tr>",
    "<th>Check</th><th>Severity</th><th>What it flags</th><th>Runs</th>",
    "</tr></thead>\n<tbody>\n",
    paste(vapply(specs, check_row, character(1)), collapse = "\n"),
    "\n</tbody>\n</table>"
  )
}, character(1))

html <- c(
  "<!doctype html>",
  "<html lang=\"en\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>pmxdchk check reference</title>",
  "<style>",
  ":root { --fg: #1c2024; --muted: #60656b; --line: #d9dde1; --bg: #fff;",
  "  --head: #f4f6f8; }",
  "@media (prefers-color-scheme: dark) { :root { --fg: #e6e8ea;",
  "  --muted: #9aa1a8; --line: #3a4046; --bg: #16191c; --head: #20252a; } }",
  "body { font: 15px/1.5 system-ui, sans-serif; color: var(--fg);",
  "  background: var(--bg); max-width: 1100px; margin: 0 auto;",
  "  padding: 24px 20px 60px; }",
  "h1 { font-size: 26px; margin: 0 0 4px; }",
  "h2 { font-size: 18px; margin: 32px 0 8px; }",
  "table { border-collapse: collapse; width: 100%; }",
  "th, td { text-align: left; vertical-align: top; padding: 8px 10px;",
  "  border-bottom: 1px solid var(--line); }",
  "th { background: var(--head); font-size: 13px; }",
  "td:first-child { width: 24%; } td:nth-child(2) { width: 9%; }",
  "td:last-child { width: 15%; }",
  "code { font-size: 12px; color: var(--muted); }",
  ".muted { color: var(--muted); } .todo { margin: 6px 0 0; }",
  ".sev { font-size: 12px; font-weight: 600; padding: 1px 7px;",
  "  border-radius: 4px; color: #fff; white-space: nowrap; }",
  ".critical { background: #c62828; } .high { background: #b26a00; }",
  ".medium { background: #5f6b76; }",
  "ul { margin: 6px 0; padding-left: 20px; }",
  ".wrap { overflow-x: auto; }",
  "@media print { body { max-width: none; } tr { break-inside: avoid; } }",
  "</style>",
  "</head>",
  "<body>",
  "<h1>pmxdchk check reference</h1>",
  sprintf(
    "<p class=\"muted\">%d checks, generated %s from pmxdchk %s.</p>",
    length(registry), format(Sys.Date()),
    utils::packageDescription("pmxdchk", fields = "Version")
  ),
  "<h2>Terms</h2>",
  "<ul>",
  "<li><strong>Dose record</strong>: EVID 1 or 4.",
  " <strong>Observation</strong>: EVID 0.",
  " <strong>Quantifiable observation</strong>: an observation with MDV = 0",
  " and a non-missing DV.</li>",
  "<li><strong>Analyte or compartment group</strong>: observations are",
  " compared only with others that share the same DVID and CMT, when those",
  " take more than one value among observations.</li>",
  "<li><strong>Result</strong>: a check flags, passes, or is skipped. It is",
  " skipped when a variable it needs is not mapped, when it does not apply to",
  " the confirmed study type, or when a check it depends on was skipped.</li>",
  "<li><strong>Robust |z|</strong>: distance from the median divided by the",
  " median absolute deviation (MAD, scaled to a standard deviation).</li>",
  "</ul>",
  "<h2>Thresholds</h2>",
  "<div class=\"wrap\"><table>",
  "<thead><tr><th>Threshold</th><th>Default</th><th>Used by</th></tr></thead>",
  "<tbody>",
  sprintf(
    "<tr><td>Outlier cutoff (robust |z|)</td><td>%s</td><td>%s</td></tr>",
    defaults$outlier_nmad, "CORE-TIME-004, CORE-DOSE-002, CORE-OBS-005"
  ),
  sprintf(
    "<tr><td>Covariate boxplot fence (x IQR)</td><td>%s</td><td>%s</td></tr>",
    defaults$outlier_iqr_k, "CORE-COV-004, CORE-COV-006"
  ),
  sprintf(
    "<tr><td>Predose cutoff (%% of Cmax)</td><td>%s</td><td>%s</td></tr>",
    100 * defaults$predose_cmax_frac, "CORE-OBS-004"
  ),
  sprintf(
    "<tr><td>Minimum quantifiable records</td><td>%s</td><td>%s</td></tr>",
    defaults$min_quantifiable_n, "CORE-OBS-006"
  ),
  "</tbody></table></div>",
  paste0("<div class=\"wrap\">", sections, "</div>"),
  "<h2>Not implemented</h2>",
  "<p>The source-data reconciliation checks of the checklist",
  " (CORE-SRC-000 to CORE-SRC-005) are not implemented: the package reads",
  " only the NMPK dataset.</p>",
  "</body>",
  "</html>"
)

writeLines(html, "docs/check_reference.html", useBytes = TRUE)
