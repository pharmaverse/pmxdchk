#' A styled empty-state / status notice
#'
#' @param message The text (or tag list) to display.
#' @param type One of `"secondary"`, `"info"`, `"warning"`, `"success"`.
#' @return A `div` styled as a Bootstrap alert.
#' @noRd
ui_notice <- function(message,
                      type = c("secondary", "info", "warning", "success")) {
  type <- match.arg(type)
  glyph <- switch(type,
    warning = "triangle-exclamation",
    success = "circle-check",
    "circle-info"
  )
  div(
    class = paste0("alert alert-", type), role = "status",
    icon(glyph), div(message)
  )
}

#' Classes of a pill badge in a Bootstrap contextual colour
#'
#' @param type Bootstrap contextual type (e.g. `"danger"`, `"warning"`).
#' @return A class string.
#' @noRd
badge_class <- function(type) {
  sprintf(
    paste(
      "badge rounded-pill bg-%1$s-subtle text-%1$s-emphasis",
      "border border-%1$s-subtle"
    ),
    type
  )
}

#' A pill badge
#'
#' @param text Badge text.
#' @param type Bootstrap contextual type (e.g. `"danger"`, `"warning"`).
#' @return A `span` tag.
#' @noRd
ui_badge <- function(text, type = "secondary") {
  tags$span(class = paste(badge_class(type), "me-1"), text)
}

#' A numbered step heading
#'
#' @param n Step number.
#' @param title Step title.
#' @return A tag list for a card header.
#' @noRd
ui_step <- function(n, title) {
  tagList(tags$span(class = "step-num", n), title)
}

#' A key figure tile
#'
#' @param label Short label.
#' @param value The figure, usually a `textOutput()`.
#' @param glyph Optional icon name shown next to the label.
#' @return A `div` tag.
#' @noRd
ui_stat <- function(label, value, glyph = NULL) {
  div(
    class = "stat",
    div(class = "stat-label", label, if (!is.null(glyph)) icon(glyph)),
    div(class = "stat-value", value)
  )
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

#' Table cell renderer drawing a value as a pill badge
#'
#' @param types Named character vector mapping a value to a Bootstrap
#'   contextual type. Other values are drawn in `"secondary"`.
#' @param key_column Zero-based index of the column holding the value to look
#'   up, when it is not the rendered column itself.
#' @return A [DT::JS()] function for `columnDefs[[i]]$render`.
#' @noRd
pill_renderer <- function(types, key_column = NULL) {
  map <- paste(sprintf("\"%s\":\"%s\"", names(types), types), collapse = ",")
  key <- if (is.null(key_column)) "data" else sprintf("row[%d]", key_column)
  DT::JS(sprintf(
    paste(
      "function(data, type, row) {",
      "  if (type !== 'display' || !data) return data;",
      "  var t = {%s}[%s] || 'secondary';",
      "  return '<span class=\"%s\">' + data + '</span>';",
      "}",
      sep = "
"
    ),
    map, key, badge_class("' + t + '")
  ))
}

#' Pill renderer for a severity column
#'
#' @return A [DT::JS()] function.
#' @noRd
severity_renderer <- function() {
  pill_renderer(c(Critical = "danger", High = "warning"))
}

#' A "Run checks" action button that reflects readiness
#'
#' @param id Input id.
#' @param ready Logical; when `FALSE` the button is rendered disabled.
#' @param label Button label.
#' @return An `actionButton` tag.
#' @noRd
run_button <- function(id, ready, label = "Run checks") {
  btn <- actionButton(
    id, label,
    class = "btn-primary text-nowrap", icon = icon("play")
  )
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
