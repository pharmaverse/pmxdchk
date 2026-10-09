#' Application theme
#'
#' A neutral, compact Bootstrap 5 theme: system fonts, a zinc grey scale,
#' bordered cards, and pill badges. The extra rules only use Bootstrap CSS
#' variables, so they follow the light / dark mode switch. They live here and
#' not in `inst/app/www` so that the Shinylive build, which has no package
#' resources, gets the same look.
#'
#' @return A [bslib::bs_theme()] object.
#' @noRd
app_theme <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#2563eb",
    secondary = "#71717a",
    success = "#16a34a",
    info = "#0284c7",
    warning = "#d97706",
    danger = "#dc2626",
    font_scale = 0.875,
    base_font = bslib::font_collection(
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue",
      "Arial", "sans-serif"
    ),
    "body-color" = "#18181b",
    "border-color" = "#e4e4e7",
    "border-radius" = "0.5rem",
    "border-radius-lg" = "0.75rem",
    "headings-font-weight" = 600
  )
  bslib::bs_add_rules(theme, app_css())
}

#' Extra style rules of the application
#'
#' @return A character scalar of CSS.
#' @noRd
app_css <- function() {
  r"(
body { -webkit-font-smoothing: antialiased; }

/* Navigation */
.navbar { border-bottom: 1px solid var(--bs-border-color); }
.navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
.navbar .nav-link {
  border-radius: 0.375rem;
  padding: 0.35rem 0.75rem !important;
  font-weight: 500;
  color: var(--bs-secondary-color);
}
.navbar .nav-link.active {
  background: var(--bs-secondary-bg);
  color: var(--bs-emphasis-color);
}
.navbar .nav-link .badge { margin-left: 0.35rem; }

/* Dataset and run status, pinned under the navigation */
.app-status {
  position: sticky;
  top: 0;
  z-index: 1020;
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.75rem;
  padding: 0.5rem 1rem;
  margin: 0 calc(-0.5 * var(--bs-gutter-x)) 1rem;
  background: var(--bs-tertiary-bg);
  border-bottom: 1px solid var(--bs-border-color);
}

/* Cards */
.card { box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04); }
.card-header {
  background: transparent;
  border-bottom: 0;
  padding-top: 1rem;
  padding-bottom: 0;
  font-size: 1rem;
  font-weight: 600;
}
.card-footer { background: transparent; }
.step-num {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.5rem;
  height: 1.5rem;
  margin-right: 0.5rem;
  border-radius: 50%;
  background: var(--bs-emphasis-color);
  color: var(--bs-body-bg);
  font-size: 0.75rem;
}
.sticky-pane { position: sticky; top: 4rem; }

/* Key figures */
.stat {
  padding: 0.85rem 1rem;
  border: 1px solid var(--bs-border-color);
  border-radius: var(--bs-border-radius-lg);
  background: var(--bs-body-bg);
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
}
.stat-label {
  display: flex;
  justify-content: space-between;
  color: var(--bs-secondary-color);
  font-size: 0.8rem;
  font-weight: 500;
}
.stat-value {
  font-size: 1.6rem;
  font-weight: 600;
  letter-spacing: -0.02em;
  font-variant-numeric: tabular-nums;
}

/* Forms */
.control-label, .form-label { font-size: 0.85rem; font-weight: 500; }
.help-block { color: var(--bs-secondary-color); font-size: 0.85rem; }
.shiny-input-container { margin-bottom: 0.75rem; }
.shiny-input-container:has(> .shiny-file-input-progress) { margin-bottom: 0; }
.shiny-input-container .radio-inline,
.shiny-input-container .checkbox-inline { margin-right: 1rem; }
.shiny-input-container input[type="checkbox"],
.shiny-input-container input[type="radio"] {
  border-color: var(--bs-secondary-color);
}
.toolbar {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  gap: 0.5rem 1.25rem;
}
.toolbar > * { margin-bottom: 0 !important; }
.accordion {
  --bs-accordion-btn-padding-y: 0.6rem;
  --bs-accordion-active-bg: transparent;
  --bs-accordion-active-color: var(--bs-emphasis-color);
  --bs-accordion-btn-focus-box-shadow: none;
}
.accordion-button { font-weight: 500; }

/* Notices and badges */
.alert {
  --bs-alert-padding-y: 0.6rem;
  --bs-alert-padding-x: 0.85rem;
  --bs-alert-margin-bottom: 0.75rem;
  display: flex;
  align-items: baseline;
  gap: 0.6rem;
}
.alert-secondary {
  --bs-alert-bg: var(--bs-tertiary-bg);
  --bs-alert-border-color: var(--bs-border-color);
  --bs-alert-color: var(--bs-secondary-color);
}
.badge { font-weight: 500; }
.detail-label {
  margin-bottom: 0.15rem;
  color: var(--bs-secondary-color);
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}
.decision {
  padding: 0.75rem 1rem 0.25rem;
  margin-bottom: 1rem;
  border: 1px solid var(--bs-border-color);
  border-radius: var(--bs-border-radius);
  background: var(--bs-tertiary-bg);
}

/* Tables */
table.dataTable, table.shiny-table { font-variant-numeric: tabular-nums; }
table.dataTable thead th, table.shiny-table thead th {
  border-bottom: 1px solid var(--bs-border-color) !important;
  color: var(--bs-secondary-color);
  font-size: 0.78rem;
  font-weight: 600;
}
table.dataTable > tbody > tr > * {
  border-bottom-color: var(--bs-border-color) !important;
  vertical-align: middle;
}
table.dataTable.hover > tbody > tr:hover > * {
  box-shadow: inset 0 0 0 9999px var(--bs-tertiary-bg);
  cursor: pointer;
}
table.dataTable > tbody > tr.selected > * {
  box-shadow: inset 0 0 0 9999px var(--bs-primary-bg-subtle) !important;
  color: var(--bs-body-color) !important;
}
table.dataTable > tbody > tr.selected > td:first-child {
  box-shadow: inset 3px 0 0 var(--bs-primary),
    inset 0 0 0 9999px var(--bs-primary-bg-subtle) !important;
}
div.dataTables_wrapper .pagination {
  --bs-pagination-font-size: 0.8rem;
  --bs-pagination-padding-x: 0.6rem;
  --bs-pagination-padding-y: 0.2rem;
}
div.dt-buttons .btn {
  --bs-btn-color: var(--bs-body-color);
  --bs-btn-bg: transparent;
  --bs-btn-border-color: var(--bs-border-color);
  --bs-btn-hover-color: var(--bs-body-color);
  --bs-btn-hover-bg: var(--bs-tertiary-bg);
  --bs-btn-hover-border-color: var(--bs-border-color);
  --bs-btn-active-color: var(--bs-body-color);
  --bs-btn-active-bg: var(--bs-secondary-bg);
  --bs-btn-active-border-color: var(--bs-border-color);
  --bs-btn-padding-x: 0.6rem;
  --bs-btn-padding-y: 0.2rem;
  --bs-btn-font-size: 0.8rem;
}
)"
}
