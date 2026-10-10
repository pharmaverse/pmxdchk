#' Overview module UI
#'
#' A first look at the confirmed dataset, available before any check runs: key
#' counts, concentration-time profiles of all subjects, dose levels, the
#' EVID x MDV cross-tab, and missing values by event type.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    conditionalPanel(
      "output.has_data", ns = ns,
      bslib::layout_column_wrap(
        width = "160px",
        fill = FALSE,
        class = "mb-3",
        ui_stat("Subjects", textOutput(ns("n_subjects")), "users"),
        ui_stat("Records", textOutput(ns("n_records")), "table-list"),
        ui_stat("Observations", textOutput(ns("n_obs")), "vial"),
        ui_stat("Doses", textOutput(ns("n_doses")), "syringe"),
        ui_stat("BLQ % (obs)", textOutput(ns("blq_pct")), "arrow-down"),
        ui_stat("Missing DV % (obs)", textOutput(ns("miss_pct")), "ban")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Concentration-time profiles, all subjects"),
        div(
          class = "toolbar",
          radioButtons(
            ns("time_axis"), "Time axis",
            choices = c(
              "Since first dose" = "first", "After last dose" = "last"
            ),
            inline = TRUE
          ),
          selectInput(
            ns("color_by"), "Colour by",
            choices = c("(none)" = ""), selectize = FALSE, width = "220px"
          ),
          checkboxInput(ns("log_y"), "Log y-axis", TRUE)
        ),
        plotOutput(ns("profiles"), height = "460px")
      ),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(lg = c(3, 3, 6)),
        fill = FALSE,
        bslib::card(
          bslib::card_header("Dose levels"),
          tableOutput(ns("dose_levels"))
        ),
        bslib::card(
          bslib::card_header("Records by EVID / MDV"),
          tableOutput(ns("evid_mdv"))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Missing values by event type"),
          DT::DTOutput(ns("missingness"), fill = FALSE)
        )
      )
    )
  )
}

#' Overview module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @return Invisibly `NULL`.
#' @noRd
mod_overview_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    output$notice <- renderUI({
      if (is.null(data_r())) {
        ui_notice(
          "Load a dataset and confirm the mapping on the Data tab."
        )
      }
    })

    output$has_data <- reactive(!is.null(data_r()))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    evid <- reactive(suppressWarnings(as.numeric(data_r()$EVID)))

    output$n_subjects <- renderText({
      req(data_r())
      as.character(dplyr::n_distinct(data_r()$ID))
    })
    output$n_records <- renderText({
      req(data_r())
      as.character(nrow(data_r()))
    })
    output$n_obs <- renderText({
      req(data_r())
      as.character(sum(evid() == 0, na.rm = TRUE))
    })
    output$n_doses <- renderText({
      req(data_r())
      as.character(sum(evid() %in% c(1, 4), na.rm = TRUE))
    })

    output$blq_pct <- renderText({
      req(data_r())
      d <- data_r()
      obs <- evid() == 0 & !is.na(evid())
      flag_col <- detect_col(names(d), "^BLQ|^CENS$")
      if (is.na(flag_col) || sum(obs) == 0) {
        return("n/a")
      }
      n_blq <- sum(is_positive_flag(d[[flag_col]]) & obs)
      paste0(round(100 * n_blq / sum(obs), 1), "%")
    })

    output$miss_pct <- renderText({
      req(data_r())
      d <- data_r()
      obs <- evid() == 0 & !is.na(evid())
      if (sum(obs) == 0) {
        return("n/a")
      }
      dv <- suppressWarnings(as.numeric(d$DV))
      paste0(round(100 * sum(is.na(dv) & obs) / sum(obs), 1), "%")
    })

    observeEvent(data_r(), {
      d <- data_r()
      by <- intersect(c("STUDYID", "SEX", "RACE", "ROUTE"), names(d))
      updateSelectInput(
        session, "color_by",
        choices = c("(none)" = "", "First dose amount" = "dose", by),
        selected = if (dplyr::n_distinct(d$STUDYID) > 1) "STUDYID" else "dose"
      )
    })

    output$profiles <- renderPlot({
      req(data_r())
      p <- plot_population_profile(
        data_r(),
        time_axis = input$time_axis, color_by = input$color_by %||% "",
        log_y = isTRUE(input$log_y)
      )
      validate(need(!is.null(p), "No observations to plot."))
      p
    }, res = 96)

    output$dose_levels <- renderTable({
      req(data_r())
      d <- data_r()
      dose <- evid() %in% c(1, 4)
      amt <- suppressWarnings(as.numeric(d$AMT))[dose]
      req(length(amt) > 0)
      data.frame(
        AMT = as.character(signif(sort(unique(amt)), 6)),
        doses = as.integer(table(amt)),
        subjects = as.integer(tapply(d$ID[dose], amt, dplyr::n_distinct))
      )
    })

    output$evid_mdv <- renderTable({
      req(data_r())
      d <- data_r()
      req("MDV" %in% names(d))
      tab <- as.data.frame.matrix(table(d$EVID, d$MDV, useNA = "ifany"))
      names(tab) <- paste("MDV =", names(tab))
      rownames(tab) <- paste("EVID =", rownames(tab))
      tab
    }, rownames = TRUE)

    output$missingness <- DT::renderDT({
      req(data_r())
      tab <- check_struct_missingness(data_r(), list())$summary_table
      validate(need(!is.null(tab) && nrow(tab) > 0, "No missing values."))
      tab$unexpected <- ifelse(tab$unexpected, "unexpected", "")
      DT::datatable(
        tab,
        colnames = c("Variable", "Event type", "Missing", "%", ""),
        class = "compact",
        options = list(
          pageLength = 8, dom = "tp",
          columnDefs = list(
            list(render = pill_renderer(c(unexpected = "warning")), targets = 4)
          )
        ),
        rownames = FALSE
      )
    })

    invisible(NULL)
  })
}
