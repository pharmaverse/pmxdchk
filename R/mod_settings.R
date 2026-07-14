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
      ns("outlier_nmad"),
      tagList(
        "Outlier cutoff (robust |z|) ",
        bslib::tooltip(
          icon("circle-question"),
          "MAD-based robust cutoff for outlier checks. Higher is more ",
          "conservative (fewer flags). Default 5."
        )
      ),
      value = d$outlier_nmad, min = 1, step = 0.5
    ),
    numericInput(
      ns("min_quantifiable_n"),
      tagList(
        "Min quantifiable records ",
        bslib::tooltip(
          icon("circle-question"),
          "Minimum quantifiable PK observations required per subject or ",
          "occasion (CORE-OBS-006). Default 2."
        )
      ),
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
      d <- default_thresholds()
      list(
        outlier_nmad = input$outlier_nmad %||% d$outlier_nmad,
        min_quantifiable_n = input$min_quantifiable_n %||% d$min_quantifiable_n
      )
    })
  })
}
