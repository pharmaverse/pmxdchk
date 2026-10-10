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
    n_flag <- sum(res$status == "flag")
    state <- if (is.null(res)) {
      ui_badge("Checks not run yet")
    } else if (isTRUE(stale())) {
      ui_badge("Study type or thresholds changed since the last run", "warning")
    } else {
      ui_badge(
        sprintf(
          "%d of %d checks flagged \u00b7 run at %s",
          n_flag, nrow(res), format(last_run()$time, "%H:%M")
        ),
        if (n_flag > 0) "danger" else "success"
      )
    }
    urgent <- is.null(res) || isTRUE(stale())
    div(
      class = "app-status",
      icon("database", class = "text-body-secondary"),
      tags$strong(upload()$name),
      tags$span(
        class = "text-body-secondary",
        sprintf(
          "%s subjects \u00b7 %s records",
          format(dplyr::n_distinct(data_r()$ID), big.mark = ","),
          format(nrow(data_r()), big.mark = ",")
        )
      ),
      state,
      actionButton(
        "run", if (is.null(res)) "Run checks" else "Re-run checks",
        class = paste(
          "btn-sm ms-auto",
          if (urgent) "btn-primary" else "btn-outline-secondary"
        ),
        icon = icon("play")
      )
    )
  })

  output$nav_findings <- renderUI({
    res <- req(results_rv())
    n <- sum(res$status %in% c("flag", "error"))
    tags$span(class = badge_class(if (n > 0) "danger" else "success"), n)
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
      class = "compact",
      options = list(
        paging = FALSE, scrollX = TRUE, scrollY = "65vh", dom = "ft",
        columnDefs = list(
          list(className = "text-nowrap", targets = 0),
          list(render = severity_renderer(), targets = 3),
          list(width = "26%", targets = 5:6)
        )
      ),
      rownames = FALSE
    )
  })
}
