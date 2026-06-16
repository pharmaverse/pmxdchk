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
#' Lists all findings, shows the detail (reason and flagged records) for the
#' selected one, lets the reviewer triage it (accept / reject / comment), and
#' exports the triaged findings and the flagged records to CSV.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_findings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Findings"),
      helpText("Click a row to see its reason and flagged records below."),
      DT::DTOutput(ns("table"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Finding detail"),
      uiOutput(ns("detail_summary")),
      DT::DTOutput(ns("detail_table"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Triage selected finding"),
      uiOutput(ns("selected")),
      radioButtons(
        ns("state"), "Decision",
        choices = c("Untriaged" = "untriaged", "Accept" = "accept",
                    "Reject (false positive)" = "reject"),
        inline = TRUE
      ),
      textInput(ns("comment"), "Comment", width = "100%"),
      actionButton(ns("save"), "Save triage", class = "btn-primary")
    ),
    bslib::card(
      full_screen = TRUE,
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
      df
    })

    output$table <- DT::renderDT(
      {
        DT::datatable(
          findings(),
          selection = "single",
          options = list(pageLength = 25, scrollX = TRUE),
          rownames = FALSE
        )
      },
      server = TRUE
    )

    selected_id <- reactive({
      row <- input$table_rows_selected
      req(length(row) == 1)
      findings()$check_id[row]
    })

    output$selected <- renderUI({
      req(selected_id())
      strong(selected_id())
    })

    selected_result <- reactive({
      req(selected_id())
      attr(results_r(), "results")[[selected_id()]]
    })

    output$detail_summary <- renderUI({
      r <- selected_result()
      req(r)
      tagList(
        tags$strong(paste0(r$check_id, " — ", r$title)),
        tags$br(),
        tags$span(class = "badge bg-secondary", r$severity),
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
