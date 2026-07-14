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
  tags$span(class = paste0("badge bg-", type, " me-1 fs-6"), text)
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
    btn$attribs$title <- "Upload a dataset and confirm the mapping first"
  }
  btn
}
