#' A styled empty-state / status notice
#'
#' @param message The text (or tag list) to display.
#' @param type One of `"info"`, `"secondary"`, `"warning"`, `"success"`.
#' @return A `div` styled as a Bootstrap alert.
#' @noRd
ui_notice <- function(message,
                      type = c("info", "secondary", "warning", "success")) {
  type <- match.arg(type)
  div(class = paste0("alert alert-", type), role = "status", message)
}

#' A Bootstrap badge
#'
#' @param text Badge text.
#' @param type Bootstrap contextual type (e.g. `"danger"`, `"warning"`).
#' @return A `span` tag.
#' @noRd
ui_badge <- function(text, type = "secondary") {
  tags$span(class = paste0("badge text-bg-", type, " me-1 fs-6"), text)
}

#' Bootstrap contextual type for a severity
#'
#' @param severity One of `"Critical"`, `"High"`, `"Medium"`.
#' @return A Bootstrap contextual type.
#' @noRd
severity_type <- function(severity) {
  switch(severity,
    Critical = "danger",
    High = "warning",
    "secondary"
  )
}

#' Colour the status cells of a findings table
#'
#' Uses Bootstrap's theme-aware subtle colours so the table stays readable in
#' dark mode.
#'
#' @param table A `DT::datatable()`.
#' @param column Name of the column to colour.
#' @param value_column Name of the column holding the status.
#' @return The styled datatable.
#' @noRd
style_status <- function(table, column = "status", value_column = column) {
  types <- c(
    flag = "danger", error = "danger", pass = "success", skip = "secondary"
  )
  DT::formatStyle(
    table, column,
    valueColumns = value_column,
    backgroundColor = DT::styleEqual(
      names(types), paste0("var(--bs-", types, "-bg-subtle)")
    ),
    color = DT::styleEqual(
      names(types), paste0("var(--bs-", types, "-text-emphasis)")
    ),
    fontWeight = "600"
  )
}

#' A "Run checks" action button that reflects readiness
#'
#' @param id Input id.
#' @param ready Logical; when `FALSE` the button is rendered disabled.
#' @param label Button label.
#' @return An `actionButton` tag.
#' @noRd
run_button <- function(id, ready, label = "Run checks") {
  btn <- actionButton(id, label, class = "btn-primary", icon = icon("play"))
  if (!isTRUE(ready)) {
    btn$attribs$disabled <- "disabled"
    btn$attribs$title <- "Load a dataset and confirm the mapping first"
  }
  btn
}

#' Round the non-integer numeric columns of a table for display
#'
#' @param data A data frame.
#' @param digits Significant digits to keep.
#' @return `data` with double columns rounded; dates and other classes are left
#'   untouched.
#' @noRd
signif_doubles <- function(data, digits = 5) {
  data[] <- lapply(data, function(x) {
    if (is.numeric(x) && !is.integer(x)) signif(x, digits) else x
  })
  data
}
