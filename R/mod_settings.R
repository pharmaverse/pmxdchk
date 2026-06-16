#' Settings module UI
#'
#' Threshold controls that feed [run_nmpk_checks()]. Conservative defaults from
#' [default_thresholds()].
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_settings_ui <- function(id) {
  ns <- NS(id)
  d <- default_thresholds()
  tagList(
    numericInput(
      ns("outlier_nmad"), "Outlier cutoff (robust |z|)",
      value = d$outlier_nmad, min = 1, step = 0.5
    ),
    numericInput(
      ns("min_quantifiable_n"), "Min quantifiable records",
      value = d$min_quantifiable_n, min = 1, step = 1
    )
  )
}

#' Settings module server
#'
#' @param id Module id.
#' @return A reactive returning the thresholds list.
#' @noRd
mod_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        outlier_nmad = input$outlier_nmad %||% default_thresholds()$outlier_nmad,
        min_quantifiable_n = input$min_quantifiable_n %||%
          default_thresholds()$min_quantifiable_n
      )
    })
  })
}
