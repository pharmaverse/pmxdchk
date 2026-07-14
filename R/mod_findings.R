#' Best record-level detail table for a check result
#'
#' Prefers flagged records (restricted to key NONMEM variables), then a summary
#' table, then a subject list. Returns `NULL` when there is no detail to show.
#'
#' @param result A `pmxdchk_check_result` object.
#' @return A data frame or `NULL`.
#' @noRd
finding_detail_table <- function(result) {
  key <- c(
    "ID", "STUDYID", "TIME", "NTIME", "EVID", "MDV", "DV", "AMT",
    "CMT", "DVID", "OCC", "RATE", "DUR", "SS", "ADDL", "II"
  )
  fr <- result$flagged_records
  if (!is.null(fr) && nrow(fr) > 0) {
    cols <- intersect(key, names(fr))
    if (length(cols) == 0) cols <- names(fr)
    return(fr[, cols, drop = FALSE])
  }
  if (!is.null(result$summary_table) && nrow(result$summary_table) > 0) {
    return(result$summary_table)
  }
  if (!is.null(result$subject_list) && nrow(result$subject_list) > 0) {
    return(result$subject_list)
  }
  NULL
}

#' Findings and triage module UI
#'
#' Master-detail review surface: a filterable, colour-coded findings table on
#' the left; the reason, flagged records, and triage controls for the selected
#' finding on the right; CSV export below.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_findings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Findings"),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(
            ns("f_status"), "Status",
            choices = c("(all)" = "", "flag", "pass", "skip"),
            selected = "", selectize = FALSE
          ),
          selectInput(
            ns("f_severity"), "Severity",
            choices = c("(all)" = "", "Critical", "High", "Medium"),
            selected = "", selectize = FALSE
          ),
          selectInput(
            ns("f_domain"), "Domain",
            choices = c("(all)" = ""), selected = "", selectize = FALSE
          )
        ),
        DT::DTOutput(ns("table"))
      ),
      tagList(
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Finding detail"),
          uiOutput(ns("detail_summary")),
          DT::DTOutput(ns("detail_table"))
        ),
        bslib::card(
          bslib::card_header("Triage"),
          radioButtons(
            ns("state"), "Decision",
            choices = c("Untriaged" = "untriaged", "Accept" = "accept",
                        "Reject (false positive)" = "reject"),
            inline = TRUE
          ),
          textInput(ns("comment"), "Comment", width = "100%"),
          actionButton(ns("save"), "Save triage", class = "btn-primary")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Export"),
      downloadButton(ns("dl_findings"), "Findings (CSV)"),
      downloadButton(ns("dl_flagged"), "Flagged records (CSV)")
    )
  )
}

#' Findings and triage module server
#'
#' @param id Module id.
#' @param results_r A reactive returning the findings tibble from
#'   [run_nmpk_checks()] (with check results attached).
#' @return Invisibly `NULL`.
#' @noRd
mod_findings_server <- function(id, results_r) {
  moduleServer(id, function(input, output, session) {
    triage <- reactiveVal(list())

    output$notice <- renderUI({
      if (is.null(results_r())) {
        ui_notice("Run checks on the Data tab to populate findings.")
      }
    })

    # All findings, triage columns attached, ordered flag-first by severity.
    findings <- reactive({
      req(results_r())
      tr <- triage()
      df <- results_r()
      df$triage_state <- vapply(
        df$check_id, function(x) tr[[x]]$state %||% "untriaged", character(1)
      )
      df$triage_comment <- vapply(
        df$check_id, function(x) tr[[x]]$comment %||% "", character(1)
      )
      st <- match(df$status, c("flag", "error", "skip", "pass"))
      sev <- match(df$severity, c("Critical", "High", "Medium"))
      df[order(st, sev, df$check_id), ]
    })

    observeEvent(results_r(), {
      df <- results_r()
      req(df)
      updateSelectInput(
        session, "f_domain",
        choices = c("(all)" = "", sort(unique(df$domain)))
      )
    })

    filtered <- reactive({
      df <- findings()
      if (nzchar(input$f_status %||% "")) df <- df[df$status == input$f_status, ]
      if (nzchar(input$f_severity %||% "")) {
        df <- df[df$severity == input$f_severity, ]
      }
      if (nzchar(input$f_domain %||% "")) df <- df[df$domain == input$f_domain, ]
      df
    })

    output$table <- DT::renderDT(
      {
        df <- filtered()
        df$message <- ifelse(
          nchar(df$message) > 80,
          paste0(substr(df$message, 1, 79), "…"), df$message
        )
        DT::datatable(
          df[, c(
            "check_id", "domain", "severity", "status", "n_flagged",
            "message", "triage_state"
          )],
          selection = "single",
          options = list(pageLength = 25, scrollX = TRUE),
          rownames = FALSE
        ) |>
          DT::formatStyle(
            "status",
            target = "row",
            backgroundColor = DT::styleEqual(
              c("flag", "error", "skip", "pass"),
              c("#fde8e8", "#fde8e8", "#f4f4f4", "#eafaea")
            )
          )
      },
      server = FALSE
    )

    selected_id <- reactive({
      row <- input$table_rows_selected
      req(length(row) == 1)
      filtered()$check_id[row]
    })

    selected_result <- reactive({
      req(selected_id())
      attr(results_r(), "results")[[selected_id()]]
    })

    output$detail_summary <- renderUI({
      r <- selected_result()
      req(r)
      sev_type <- switch(r$severity,
        Critical = "danger", High = "warning", "secondary"
      )
      tagList(
        tags$strong(paste0(r$check_id, " — ", r$title)),
        tags$br(),
        ui_badge(r$severity, sev_type),
        tags$span(
          style = "margin-left:8px;",
          paste0("status: ", r$status, " | flagged: ", r$n_flagged)
        ),
        tags$p(tags$em(r$message), style = "margin-top:8px;")
      )
    })

    output$detail_table <- DT::renderDT({
      r <- selected_result()
      req(r)
      detail <- finding_detail_table(r)
      validate(need(
        !is.null(detail),
        "No record-level detail for this finding (it passed or was skipped)."
      ))
      DT::datatable(
        detail,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    observeEvent(selected_id(), {
      tr <- triage()[[selected_id()]]
      updateRadioButtons(session, "state", selected = tr$state %||% "untriaged")
      updateTextInput(session, "comment", value = tr$comment %||% "")
    })

    observeEvent(input$save, {
      req(selected_id())
      tr <- triage()
      tr[[selected_id()]] <- list(state = input$state, comment = input$comment)
      triage(tr)
      showNotification("Triage saved.", type = "message", duration = 2)
    })

    flagged_all <- reactive({
      objs <- attr(results_r(), "results")
      rows <- lapply(names(objs), function(cid) {
        fr <- objs[[cid]]$flagged_records
        if (is.null(fr) || nrow(fr) == 0) {
          return(NULL)
        }
        dplyr::mutate(fr, check_id = cid, .before = 1)
      })
      dplyr::bind_rows(rows)
    })

    output$dl_findings <- downloadHandler(
      filename = function() "pmxdchk_findings.csv",
      content = function(file) readr::write_csv(findings(), file)
    )
    output$dl_flagged <- downloadHandler(
      filename = function() "pmxdchk_flagged_records.csv",
      content = function(file) readr::write_csv(flagged_all(), file)
    )

    invisible(NULL)
  })
}
