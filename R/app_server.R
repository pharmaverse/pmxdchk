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

  # Results belong to one dataset: drop them when another one is confirmed.
  observeEvent(upload(), {
    results_rv(NULL)
    last_run(NULL)
  }, ignoreNULL = FALSE)

  do_run <- function() {
    if (!isTRUE(ready())) {
      showNotification(
        "Load a dataset and confirm the variable mapping first.",
        type = "warning"
      )
      return(invisible())
    }
    res <- withProgress(
      run_nmpk_checks(
        data_r(),
        study_type = study_type(),
        thresholds = thresholds()
      ),
      message = "Running checks..."
    )
    results_rv(res)
    last_run(list(
      time = Sys.time(),
      study_type = study_type(),
      thresholds = thresholds()
    ))
    bslib::nav_select("main_nav", "Findings", session = session)
  }
  observeEvent(input$run, do_run())
  observeEvent(input$run_data, do_run())

  output$run_data_ui <- renderUI(run_button("run_data", ready()))

  stale <- reactive({
    lr <- last_run()
    if (is.null(lr) || is.null(results_rv())) {
      return(FALSE)
    }
    !identical(lr$study_type, study_type()) ||
      !identical(lr$thresholds, thresholds())
  })

  # One line under the navbar: which dataset is loaded and where the run stands.
  output$status_bar <- renderUI({
    if (!isTRUE(ready())) {
      return(NULL)
    }
    res <- results_rv()
    state <- if (is.null(res)) {
      "Checks not run yet."
    } else if (isTRUE(stale())) {
      "Study type or thresholds changed since the last run."
    } else {
      sprintf(
        "%d of %d checks flagged, run at %s.",
        sum(res$status == "flag"), nrow(res),
        format(last_run()$time, "%H:%M")
      )
    }
    type <- if (isTRUE(stale())) "warning" else "light"
    urgent <- is.null(res) || isTRUE(stale())
    div(
      class = paste0(
        "alert alert-", type,
        " d-flex align-items-center gap-3 py-2 mx-3 mt-3 mb-0"
      ),
      tags$strong(upload()$name),
      tags$span(
        sprintf(
          "%d subjects, %d records.",
          dplyr::n_distinct(data_r()$ID), nrow(data_r())
        ),
        state
      ),
      actionButton(
        "run", if (is.null(res)) "Run checks" else "Re-run checks",
        class = paste(
          "btn-sm ms-auto",
          if (urgent) "btn-primary" else "btn-outline-primary"
        ),
        icon = icon("play")
      )
    )
  })

  results <- reactive(results_rv())

  mod_overview_server("overview", data_r)
  to_profiles <- mod_findings_server("findings", data_r, results)
  mod_profile_server("profile", data_r, results, to_profiles)

  observeEvent(to_profiles(), {
    bslib::nav_select("main_nav", "Profiles", session = session)
  })

  output$library <- DT::renderDT({
    DT::datatable(
      check_catalogue(),
      colnames = c(
        "Check", "Title", "Domain", "Severity", "Applies to", "Rule",
        "What to do"
      ),
      filter = "top",
      class = "compact stripe",
      options = list(
        paging = FALSE, scrollX = TRUE, dom = "ft",
        columnDefs = list(list(className = "text-nowrap", targets = 0))
      ),
      rownames = FALSE
    )
  })
}
