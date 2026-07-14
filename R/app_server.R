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

  ready <- reactive(!is.null(upload()))
  results_rv <- reactiveVal(NULL)
  last_run <- reactiveVal(NULL)

  do_run <- function() {
    if (!isTRUE(ready())) {
      showNotification(
        "Upload a dataset and confirm the variable mapping first.",
        type = "warning"
      )
      return(invisible())
    }
    res <- run_nmpk_checks(
      data_r(),
      study_type = study_type(),
      thresholds = thresholds()
    )
    results_rv(res)
    last_run(list(
      time = Sys.time(),
      study_type = study_type(),
      thresholds = thresholds()
    ))
    showNotification(
      sprintf(
        "Checks complete: %d checks run, %d flagged.",
        nrow(res), sum(res$status == "flag")
      ),
      type = "message"
    )
    bslib::nav_select("main_nav", "Overview", session = session)
  }
  observeEvent(input$run, do_run())
  observeEvent(input$run_data, do_run())

  output$run_ui <- renderUI(run_button("run", ready()))
  output$run_data_ui <- renderUI(run_button("run_data", ready()))

  stale <- reactive({
    lr <- last_run()
    if (is.null(lr) || is.null(results_rv())) {
      return(FALSE)
    }
    !identical(lr$study_type, study_type()) ||
      !identical(lr$thresholds, thresholds())
  })

  output$run_status <- renderUI({
    lr <- last_run()
    if (is.null(lr)) {
      return(NULL)
    }
    tagList(
      tags$small(
        class = "text-muted",
        paste0("Last run: ", format(lr$time, "%H:%M:%S"))
      ),
      if (isTRUE(stale())) {
        ui_notice("Settings changed — re-run checks.", "warning")
      }
    )
  })

  results <- reactive(results_rv())

  mod_overview_server("overview", data_r, results)
  mod_profile_server("profile", data_r, results)
  mod_findings_server("findings", results)
}
