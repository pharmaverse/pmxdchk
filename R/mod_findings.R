#' Order the columns of a flagged-records table for review
#'
#' Puts the subject, the diagnostic columns added by checks, and the key NONMEM
#' variables first; drops internal bookkeeping columns.
#'
#' @param records A data frame of flagged records.
#' @return The data frame with reordered columns.
#' @noRd
order_record_columns <- function(records) {
  front <- c(
    "ID", "issue", "duplicate_type", "pct_of_cmax", "time_difference",
    "STUDYID", "TIME", "NTIME", "EVID", "MDV", "DV", "AMT",
    "CMT", "DVID", "OCC", "RATE", "DUR", "SS", "ADDL", "II"
  )
  cols <- grep("^\\.", names(records), value = TRUE, invert = TRUE)
  records[, c(intersect(front, cols), setdiff(cols, front)), drop = FALSE]
}

#' Whether a concentration-time profile helps review a finding
#'
#' True for findings that point at individual records, and for subject-level
#' findings about dosing or observations.
#'
#' @param result A `pmxdchk_check_result` object.
#' @return A logical scalar.
#' @noRd
finding_has_profile <- function(result) {
  pk_domain <- result$domain %in% c("dosing", "observations")
  result$status == "flag" && length(result_subject_ids(result)) > 0 &&
    (!is.null(result$flagged_records) || pk_domain)
}

#' Rows of the findings list
#'
#' @param df Findings tibble (one row per check).
#' @param states Named character vector of triage states by check ID.
#' @return A data frame with the display columns of the findings list.
#' @noRd
findings_list <- function(df, states) {
  needs_review <- df$status %in% c("flag", "error")
  state <- ifelse(
    df$check_id %in% names(states), states[df$check_id], "untriaged"
  )
  labels <- c(
    untriaged = "to review", accept = "issue to fix", reject = "not an issue"
  )
  data.frame(
    check = sprintf(
      "%s<br><small class=\"text-muted\">%s</small>", df$title, df$check_id
    ),
    severity = df$severity,
    result = ifelse(
      df$status == "flag", paste(df$n_flagged, "flagged"), df$status
    ),
    triage = ifelse(needs_review, unname(labels[state]), ""),
    status = df$status,
    stringsAsFactors = FALSE
  )
}

#' Findings review module UI
#'
#' The main workspace: the list of findings on the left; on the right, for the
#' selected finding, the triage decision, the rule and guidance, the profile of
#' each affected subject, and the affected records.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_findings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      fill = FALSE,
      bslib::card(
        bslib::card_header("Findings"),
        uiOutput(ns("progress")),
        radioButtons(
          ns("show"), NULL,
          choices = c("To review" = "review", "All" = "all"), inline = TRUE
        ),
        DT::DTOutput(ns("table"), fill = FALSE),
        bslib::card_footer(
          downloadButton(
            ns("dl_findings"), "Findings with triage (CSV)",
            class = "btn-sm btn-outline-secondary"
          ),
          downloadButton(
            ns("dl_flagged"), "Flagged records (CSV)",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Finding detail"),
        uiOutput(ns("detail_head")),
        conditionalPanel(
          "output.needs_triage", ns = ns,
          div(
            class = "border rounded p-3 mb-3",
            div(
              class = "d-flex justify-content-between align-items-start",
              radioButtons(
                ns("state"), "Your decision",
                choices = c(
                  "To review" = "untriaged", "Issue to fix" = "accept",
                  "Not an issue" = "reject"
                ),
                inline = TRUE
              ),
              actionButton(
                ns("next_finding"), "Next finding",
                class = "btn-outline-primary btn-sm", icon = icon("arrow-down")
              )
            ),
            textInput(
              ns("comment"), NULL, width = "100%",
              placeholder = "Comment (optional), saved as you type"
            )
          )
        ),
        uiOutput(ns("detail_body"))
      )
    )
  )
}

#' Findings review module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @param results_r A reactive returning the findings tibble from
#'   [run_nmpk_checks()] (with check results attached).
#' @return A reactive that changes to `list(check_id, subject, time)` each time
#'   the user asks to open the profile browser from a finding.
#' @noRd
mod_findings_server <- function(id, data_r, results_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    states <- reactiveVal(character())
    comments <- reactiveVal(character())
    selected <- reactiveVal(NULL)
    to_profiles <- reactiveVal(NULL)

    observeEvent(results_r(), {
      selected(NULL)
      states(character())
      comments(character())
    }, ignoreNULL = FALSE)

    output$notice <- renderUI({
      if (is.null(results_r())) {
        ui_notice("Run checks on the Data tab to see the findings.")
      }
    })

    # Flag-first, most severe first.
    findings <- reactive({
      req(results_r())
      df <- results_r()
      st <- match(df$status, c("flag", "error", "skip", "pass"))
      sev <- match(df$severity, c("Critical", "High", "Medium"))
      df[order(st, sev, df$check_id), ]
    })

    listed <- reactive({
      df <- findings()
      if (identical(input$show, "all")) {
        return(df)
      }
      df[df$status %in% c("flag", "error"), ]
    })

    observeEvent(findings(), {
      df <- findings()
      n_review <- sum(df$status %in% c("flag", "error"))
      updateRadioButtons(
        session, "show",
        choiceNames = c(
          sprintf("To review (%d)", n_review),
          sprintf(
            "All checks (%d): %d passed, %d skipped",
            nrow(df), sum(df$status == "pass"), sum(df$status == "skip")
          )
        ),
        choiceValues = c("review", "all"),
        selected = "review", inline = TRUE
      )
    })

    output$progress <- renderUI({
      df <- findings()
      review <- df[df$status %in% c("flag", "error"), ]
      if (nrow(review) == 0) {
        return(ui_notice("No check flagged this dataset.", "success"))
      }
      done <- sum(states()[review$check_id] %in% c("accept", "reject"))
      n_sev <- function(s) sum(review$severity == s)
      tagList(
        div(
          class = "mb-2",
          ui_badge(paste0(n_sev("Critical"), " Critical"), "danger"),
          ui_badge(paste0(n_sev("High"), " High"), "warning"),
          ui_badge(paste0(n_sev("Medium"), " Medium"), "secondary")
        ),
        div(
          class = "progress mb-1", style = "height: 8px;",
          div(
            class = "progress-bar bg-success",
            style = sprintf("width: %.0f%%;", 100 * done / nrow(review))
          )
        ),
        tags$small(
          class = "text-muted",
          sprintf("%d of %d findings triaged", done, nrow(review))
        )
      )
    })

    output$table <- DT::renderDT({
      df <- listed()
      table <- DT::datatable(
        findings_list(df, isolate(states())),
        colnames = c("Check", "Severity", "Result", "Triage", "status"),
        escape = -1,
        selection = list(
          mode = "single",
          selected = match(isolate(selected()), df$check_id)
        ),
        class = "compact hover",
        options = list(
          paging = FALSE, scrollY = "60vh", scrollCollapse = TRUE, dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(className = "text-nowrap", targets = 1:3),
            list(visible = FALSE, targets = 4)
          )
        ),
        rownames = FALSE
      )
      style_status(table, "result", "status")
    })
    proxy <- DT::dataTableProxy("table")

    # Refresh the triage column in place so the list keeps its scroll position.
    observeEvent(states(), {
      req(results_r())
      DT::replaceData(
        proxy, findings_list(listed(), states()),
        resetPaging = FALSE, clearSelection = "none", rownames = FALSE
      )
    }, ignoreInit = TRUE)

    observeEvent(input$table_rows_selected, {
      selected(listed()$check_id[input$table_rows_selected])
    })

    observeEvent(input$next_finding, {
      i <- match(selected(), listed()$check_id)
      if (!is.na(i) && i < nrow(listed())) {
        DT::selectRows(proxy, i + 1)
      }
    })

    selected_result <- reactive({
      req(selected())
      attr(results_r(), "results")[[selected()]]
    })

    output$needs_triage <- reactive({
      !is.null(results_r()) && !is.null(selected()) &&
        selected_result()$status %in% c("flag", "error")
    })
    outputOptions(output, "needs_triage", suspendWhenHidden = FALSE)

    observeEvent(selected(), {
      id <- selected()
      state <- if (id %in% names(states())) states()[[id]] else "untriaged"
      comment <- if (id %in% names(comments())) comments()[[id]] else ""
      updateRadioButtons(session, "state", selected = state)
      updateTextInput(session, "comment", value = comment)
    })

    observeEvent(input$state, {
      req(selected())
      s <- states()
      s[[selected()]] <- input$state
      states(s)
    }, ignoreInit = TRUE)
    observeEvent(input$comment, {
      req(selected())
      cm <- comments()
      cm[[selected()]] <- input$comment
      comments(cm)
    }, ignoreInit = TRUE)

    output$detail_head <- renderUI({
      if (is.null(results_r()) || is.null(selected())) {
        return(ui_notice(
          paste(
            "Select a finding on the left to see what was checked, the",
            "affected records and subjects, and what to do about it."
          ),
          "secondary"
        ))
      }
      r <- selected_result()
      tagList(
        tags$h5(r$title),
        div(
          class = "mb-2",
          ui_badge(r$severity, severity_type(r$severity)),
          switch(r$status,
            flag = ui_badge(paste(r$n_flagged, "flagged"), "light"),
            pass = ui_badge("pass", "success"),
            ui_badge(r$status, "secondary")
          ),
          tags$small(class = "text-muted", r$check_id)
        ),
        tags$p(tags$strong(r$message))
      )
    })

    affected <- reactive(result_subject_ids(selected_result()))

    output$detail_body <- renderUI({
      req(results_r(), selected())
      r <- selected_result()
      tables <- list(
        records = list("Affected records", r$flagged_records),
        summary = list("Summary", r$summary_table),
        subjects = list("Subjects", r$subject_list)
      )
      tables <- Filter(function(t) !is.null(t[[2]]) && nrow(t[[2]]) > 0, tables)
      tagList(
        tags$p(tags$span(class = "text-muted", "What is checked: "), r$rule),
        tags$p(tags$span(class = "text-muted", "What to do: "), r$guidance),
        if (finding_has_profile(r)) {
          tagList(
            div(
              class = "d-flex align-items-end gap-2 mt-2",
              selectInput(
                ns("subject"),
                sprintf("Affected subject (%d)", length(affected())),
                choices = affected(), selectize = FALSE, width = "220px"
              ),
              div(
                class = "btn-group mb-3",
                actionButton(ns("prev_subject"), "Previous"),
                actionButton(ns("next_subject"), "Next")
              ),
              actionLink(
                ns("to_profiles"), "Open in Profiles",
                class = "mb-3 ms-auto"
              )
            ),
            plotOutput(ns("plot"), height = "340px")
          )
        },
        lapply(names(tables), function(key) {
          tagList(
            tags$h6(class = "mt-3", tables[[key]][[1]]),
            DT::DTOutput(ns(paste0("tbl_", key)), fill = FALSE)
          )
        })
      )
    })

    step_subject <- function(by) {
      ids <- affected()
      i <- match(input$subject, ids) + by
      if (!is.na(i) && i >= 1 && i <= length(ids)) {
        updateSelectInput(session, "subject", selected = ids[i])
      }
    }
    observeEvent(input$prev_subject, step_subject(-1))
    observeEvent(input$next_subject, step_subject(1))

    output$plot <- renderPlot({
      r <- selected_result()
      req(input$subject %in% affected())
      d <- add_review_columns(data_r())
      sub <- d[as.character(d$ID) == input$subject, , drop = FALSE]
      sub$flagged <- sub$.rowid %in% r$flagged_records[[".rowid"]]
      p <- plot_subject_profile(sub, input$subject)
      validate(need(!is.null(p), "This subject has no observations to plot."))
      p
    })

    detail_table <- function(get) {
      DT::renderDT({
        data <- get(selected_result())
        req(!is.null(data), nrow(data) > 0)
        DT::datatable(
          signif_doubles(data),
          class = "compact stripe nowrap",
          options = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
          rownames = FALSE
        )
      })
    }
    output$tbl_records <- detail_table(function(r) {
      if (!is.null(r$flagged_records)) order_record_columns(r$flagged_records)
    })
    output$tbl_summary <- detail_table(function(r) r$summary_table)
    output$tbl_subjects <- detail_table(function(r) r$subject_list)

    observeEvent(input$to_profiles, {
      to_profiles(list(
        check_id = selected(), subject = input$subject, time = Sys.time()
      ))
    })

    export <- reactive({
      df <- findings()
      lookup <- function(x, default) {
        ifelse(df$check_id %in% names(x), x[df$check_id], default)
      }
      df$triage_state <- unname(lookup(states(), "untriaged"))
      df$triage_comment <- unname(lookup(comments(), ""))
      df
    })

    flagged_all <- reactive({
      objs <- attr(results_r(), "results")
      rows <- lapply(names(objs), function(cid) {
        fr <- objs[[cid]]$flagged_records
        if (is.null(fr) || nrow(fr) == 0) {
          return(NULL)
        }
        fr[] <- lapply(fr, as.character)
        dplyr::mutate(fr, check_id = cid, .before = 1)
      })
      dplyr::bind_rows(rows)
    })

    output$dl_findings <- downloadHandler(
      filename = function() "pmxdchk_findings.csv",
      content = function(file) readr::write_csv(export(), file)
    )
    output$dl_flagged <- downloadHandler(
      filename = function() "pmxdchk_flagged_records.csv",
      content = function(file) readr::write_csv(flagged_all(), file)
    )

    reactive(to_profiles())
  })
}
