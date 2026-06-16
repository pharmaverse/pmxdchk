#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  upload <- mod_data_upload_server("upload")
  data_r <- reactive(upload()$data)
  study_type <- mod_check_studytype_server("studytype", data_r)
  thresholds <- mod_settings_server("settings")

  results_rv <- reactiveVal(NULL)
  observeEvent(input$run, {
    if (is.null(upload())) {
      showNotification(
        "Upload a dataset and confirm the variable mapping first.",
        type = "warning"
      )
      return()
    }
    res <- run_nmpk_checks(
      data_r(),
      study_type = study_type(),
      thresholds = thresholds()
    )
    results_rv(res)
    showNotification(
      sprintf(
        "Checks complete: %d checks run, %d flagged.",
        nrow(res), sum(res$status == "flag")
      ),
      type = "message"
    )
    bslib::nav_select("main_nav", "Overview", session = session)
  })

  results <- reactive({
    req(results_rv())
    results_rv()
  })

  mod_overview_server("overview", data_r, results)
  mod_profile_server("profile", data_r, results)
  mod_findings_server("findings", results)
}
