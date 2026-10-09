#' Numeric threshold input with a help tooltip
#'
#' @param id Input id.
#' @param label Input label.
#' @param help Tooltip text.
#' @param ... Passed to [shiny::numericInput()].
#' @return A `numericInput` tag.
#' @noRd
threshold_input <- function(id, label, help, ...) {
  numericInput(
    id,
    tagList(label, " ", bslib::tooltip(icon("circle-question"), help)),
    ...
  )
}

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
  bslib::layout_column_wrap(
    width = "260px",
    fill = FALSE,
    threshold_input(
      ns("outlier_nmad"), "Outlier cutoff (robust |z|)",
      paste(
        "Distance from the median, in robust standard deviations (MAD), beyond",
        "which a concentration, dose, or sampling-time deviation is flagged.",
        "Higher means fewer flags. Default 5."
      ),
      value = d$outlier_nmad, min = 1, step = 0.5
    ),
    threshold_input(
      ns("outlier_iqr_k"), "Covariate boxplot fence (x IQR)",
      paste(
        "Covariate values further than this many interquartile ranges from",
        "the quartiles are flagged. 1.5 is the usual boxplot whisker; the",
        "default 3 flags only far outliers."
      ),
      value = d$outlier_iqr_k, min = 0.5, step = 0.5
    ),
    threshold_input(
      ns("predose_cmax_pct"), "Predose cutoff (% of Cmax)",
      paste(
        "A quantifiable predose concentration above this percentage of the",
        "subject's Cmax is flagged (CORE-OBS-004). Default 5."
      ),
      value = 100 * d$predose_cmax_frac, min = 0, max = 100, step = 1
    ),
    threshold_input(
      ns("min_quantifiable_n"), "Min quantifiable records",
      paste(
        "Minimum quantifiable PK observations required per subject or",
        "occasion (CORE-OBS-006). Default 2."
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
      pct <- input$predose_cmax_pct
      list(
        outlier_nmad = input$outlier_nmad %||% d$outlier_nmad,
        outlier_iqr_k = input$outlier_iqr_k %||% d$outlier_iqr_k,
        min_quantifiable_n = input$min_quantifiable_n %||% d$min_quantifiable_n,
        predose_cmax_frac = if (is.null(pct)) d$predose_cmax_frac else pct / 100
      )
    })
  })
}
