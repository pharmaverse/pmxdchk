#' Overview dashboard module UI
#'
#' Landing summary of the uploaded dataset: key counts, the EVID x MDV
#' cross-tab, concentration and dose distributions, and a missingness table.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    uiOutput(ns("severity_bar")),
    bslib::layout_columns(
      bslib::value_box(
        "Subjects", textOutput(ns("n_subjects")),
        showcase = icon("users")
      ),
      bslib::value_box(
        "Records", textOutput(ns("n_records")),
        showcase = icon("table-list")
      ),
      bslib::value_box(
        "Observations", textOutput(ns("n_obs")),
        showcase = icon("vial")
      ),
      bslib::value_box(
        "Doses", textOutput(ns("n_doses")),
        showcase = icon("syringe")
      ),
      bslib::value_box("BLQ % (obs)", textOutput(ns("blq_pct"))),
      bslib::value_box("Missing DV % (obs)", textOutput(ns("miss_pct")))
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Concentration distribution (log10 DV)"),
        plotOutput(ns("dv_hist"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Dose amount distribution"),
        plotOutput(ns("dose_hist"))
      )
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Records by EVID / MDV"),
        tableOutput(ns("evid_mdv"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Missingness"),
        DT::DTOutput(ns("missingness"))
      )
    )
  )
}

#' Overview dashboard module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @param results_r A reactive returning the findings tibble from
#'   [run_nmpk_checks()] (with check results attached).
#' @return Invisibly `NULL`.
#' @noRd
mod_overview_server <- function(id, data_r, results_r) {
  moduleServer(id, function(input, output, session) {
    output$notice <- renderUI({
      if (is.null(data_r())) {
        return(ui_notice(
          "Upload a dataset and confirm the mapping on the Data tab."
        ))
      }
      if (is.null(results_r())) {
        return(ui_notice(
          "Run checks to populate the missingness panel.", "secondary"
        ))
      }
      NULL
    })

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
      paste0(round(100 * sum(is_positive_flag(d[[flag_col]]) & obs) / sum(obs), 1), "%")
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

    output$severity_bar <- renderUI({
      df <- results_r()
      if (is.null(df)) {
        return(NULL)
      }
      fl <- df[df$status == "flag", ]
      n_sev <- function(s) sum(fl$severity == s)
      div(
        class = "mb-3",
        ui_badge(paste0(n_sev("Critical"), " Critical"), "danger"),
        ui_badge(paste0(n_sev("High"), " High"), "warning"),
        ui_badge(paste0(n_sev("Medium"), " Medium"), "secondary"),
        ui_badge(paste0(sum(df$status == "pass"), " pass"), "success"),
        ui_badge(paste0(sum(df$status == "skip"), " skip"), "light")
      )
    })

    output$dv_hist <- renderPlot({
      req(data_r())
      d <- data_r()
      dv <- suppressWarnings(as.numeric(d$DV))
      obs <- dv[evid() == 0 & !is.na(dv) & dv > 0]
      req(length(obs) > 0)
      ggplot2::ggplot(data.frame(dv = obs), ggplot2::aes(x = dv)) +
        ggplot2::geom_histogram(bins = 30, fill = "#4C78A8") +
        ggplot2::scale_x_log10() +
        ggplot2::labs(x = "DV (log10)", y = "Count") +
        ggplot2::theme_minimal()
    })

    output$dose_hist <- renderPlot({
      req(data_r())
      d <- data_r()
      amt <- suppressWarnings(as.numeric(d$AMT))
      doses <- amt[evid() %in% c(1, 4) & !is.na(amt) & amt > 0]
      req(length(doses) > 0)
      ggplot2::ggplot(data.frame(amt = doses), ggplot2::aes(x = amt)) +
        ggplot2::geom_histogram(bins = 30, fill = "#F58518") +
        ggplot2::labs(x = "AMT", y = "Count") +
        ggplot2::theme_minimal()
    })

    output$evid_mdv <- renderTable({
      req(data_r())
      d <- data_r()
      req("MDV" %in% names(d))
      as.data.frame.matrix(table(EVID = d$EVID, MDV = d$MDV))
    }, rownames = TRUE)

    output$missingness <- DT::renderDT({
      req(results_r())
      res <- attr(results_r(), "results")[["CORE-STRUCT-006"]]
      req(!is.null(res), !is.null(res$summary_table))
      DT::datatable(
        res$summary_table,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    invisible(NULL)
  })
}
