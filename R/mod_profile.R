#' Individual profile browser module UI
#'
#' Per-subject concentration-time profile with flagged records overlaid, dose
#' markers, prev/next navigation, a filter by check, and the subject's event
#' table.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    conditionalPanel(
      "output.has_data", ns = ns,
      div(
        class = "toolbar mb-3",
        selectInput(
          ns("show"), "Show subjects",
          choices = c("All subjects" = "all"), selectize = FALSE,
          width = "360px"
        ),
        selectizeInput(
          ns("subject"), "Subject", choices = NULL, width = "200px",
          options = list(dropdownParent = "body")
        ),
        div(
          class = "btn-group",
          actionButton(
            ns("prev"), "Previous", icon = icon("chevron-left"),
            class = "btn-outline-secondary"
          ),
          actionButton(
            ns("nxt"), "Next", icon = icon("chevron-right"),
            class = "btn-outline-secondary"
          )
        ),
        div(class = "pb-2", uiOutput(ns("counter"))),
        div(
          class = "ms-auto pb-1",
          checkboxInput(ns("log_y"), "Log y-axis", TRUE)
        )
      ),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(lg = c(7, 5)),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Concentration-time profile"),
          plotOutput(ns("plot"))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Checks flagged for this subject"),
          DT::DTOutput(ns("subject_checks"), fill = FALSE)
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Event records"),
        DT::DTOutput(ns("events"), fill = FALSE)
      )
    )
  )
}

#' Individual profile browser module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @param results_r A reactive returning the findings tibble from
#'   [run_nmpk_checks()] (with check results attached).
#' @param focus_r A reactive returning `list(check_id, subject, time)`; when it
#'   changes, the subject filter switches to that check and subject.
#' @return Invisibly `NULL`.
#' @noRd
mod_profile_server <- function(id, data_r, results_r,
                               focus_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$notice <- renderUI({
      if (is.null(data_r())) {
        return(ui_notice(
          "Confirm the variable mapping on the Data tab to browse profiles."
        ))
      }
      if (is.null(results_r())) {
        return(ui_notice(
          "Run checks to overlay flags; until then profiles show without them.",
          "secondary"
        ))
      }
      NULL
    })

    output$has_data <- reactive(!is.null(data_r()))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    data_id <- reactive({
      req(data_r())
      add_review_columns(data_r())
    })

    flagged_results <- reactive({
      if (is.null(results_r())) {
        return(list())
      }
      Filter(function(r) r$status == "flag", attr(results_r(), "results"))
    })

    # Record-level hits (.rowid) and subject-level hits (ID) per flagged check.
    row_hits <- reactive({
      hits <- lapply(flagged_results(), function(r) {
        rows <- as.integer(r$flagged_records[[".rowid"]])
        tibble::tibble(.rowid = rows, check_id = rep(r$check_id, length(rows)))
      })
      empty <- tibble::tibble(.rowid = integer(), check_id = character())
      dplyr::bind_rows(c(list(empty), unname(hits)))
    })
    subject_hits <- reactive({
      hits <- lapply(flagged_results(), function(r) {
        ids <- result_subject_ids(r)
        tibble::tibble(ID = ids, check_id = rep(r$check_id, length(ids)))
      })
      empty <- tibble::tibble(ID = character(), check_id = character())
      dplyr::bind_rows(c(list(empty), unname(hits)))
    })

    observeEvent(flagged_results(), {
      hits <- subject_hits()
      checks <- Filter(
        function(r) r$check_id %in% hits$check_id, flagged_results()
      )
      by_check <- vapply(checks, function(r) r$check_id, character(1))
      names(by_check) <- vapply(checks, function(r) {
        sprintf(
          "%s \u2014 %s (%d)", r$check_id, r$title,
          sum(hits$check_id == r$check_id)
        )
      }, character(1))
      choices <- c("All subjects" = "all")
      if (length(by_check) > 0) {
        choices <- c(
          choices,
          stats::setNames(
            "any",
            sprintf("Flagged by any check (%d)", length(unique(hits$ID)))
          ),
          by_check
        )
      }
      keep <- if (isTRUE(input$show %in% choices)) input$show else "all"
      updateSelectInput(session, "show", choices = choices, selected = keep)
    })

    wanted_subject <- reactiveVal(NULL)
    observeEvent(focus_r(), {
      target <- focus_r()$check_id
      if (!target %in% subject_hits()$check_id) {
        target <- "any"
      }
      wanted_subject(focus_r()$subject)
      updateSelectInput(session, "show", selected = target)
      updateSelectizeInput(session, "subject", selected = focus_r()$subject)
    })

    subjects <- reactive({
      ids <- unique(as.character(data_id()$ID))
      show <- input$show %||% "all"
      if (show == "all") {
        return(ids)
      }
      hits <- subject_hits()
      if (show != "any") {
        hits <- hits[hits$check_id == show, ]
      }
      ids[ids %in% hits$ID]
    })

    observeEvent(subjects(), {
      keep <- intersect(wanted_subject(), subjects())
      updateSelectizeInput(
        session, "subject",
        choices = subjects(), selected = if (length(keep) > 0) keep
      )
    })

    step_subject <- function(by) {
      s <- subjects()
      i <- match(input$subject, s) + by
      if (!is.na(i) && i >= 1 && i <= length(s)) {
        updateSelectizeInput(session, "subject", selected = s[i])
      }
    }
    observeEvent(input$prev, step_subject(-1))
    observeEvent(input$nxt, step_subject(1))

    output$counter <- renderUI({
      s <- subjects()
      if (length(s) == 0) {
        return(tags$span(class = "text-body-secondary", "No subjects match."))
      }
      req(input$subject)
      tags$span(
        class = "text-body-secondary",
        sprintf("%d of %d", match(input$subject, s), length(s))
      )
    })

    current <- reactive({
      req(input$subject)
      d <- data_id()
      sub <- d[as.character(d$ID) == input$subject, , drop = FALSE]
      hits <- row_hits()
      hits <- hits[hits$.rowid %in% sub$.rowid, ]
      by_row <- tapply(hits$check_id, hits$.rowid, paste, collapse = ", ")
      sub$flagged_by <- unname(by_row[as.character(sub$.rowid)])
      sub$flagged_by[is.na(sub$flagged_by)] <- ""
      focus <- input$show %||% "all"
      sub$flagged <- if (focus %in% c("all", "any")) {
        sub$flagged_by != ""
      } else {
        sub$.rowid %in% hits$.rowid[hits$check_id == focus]
      }
      sub
    })

    output$plot <- renderPlot({
      p <- plot_subject_profile(current(), input$subject, isTRUE(input$log_y))
      validate(need(!is.null(p), "This subject has no observations to plot."))
      p
    }, res = 96)

    output$subject_checks <- DT::renderDT({
      req(input$subject)
      ids <- c(
        subject_hits()$check_id[subject_hits()$ID == input$subject],
        row_hits()$check_id[row_hits()$.rowid %in% current()$.rowid]
      )
      validate(need(
        length(ids) > 0,
        "No checks flagged this subject (run checks first, or none flagged)."
      ))
      df <- results_r()
      df <- df[df$check_id %in% ids, c("check_id", "severity", "message")]
      DT::datatable(
        df,
        colnames = c("Check", "Severity", "Finding"),
        class = "compact",
        options = list(
          pageLength = 6, dom = "tp",
          columnDefs = list(
            list(className = "text-nowrap", targets = 0),
            list(render = severity_renderer(), targets = 1)
          )
        ),
        rownames = FALSE
      )
    })

    output$events <- DT::renderDT({
      sub <- current()
      cols <- intersect(
        c(
          ".rowid", "TIME", "NTIME", "EVID", "MDV", "DV", "AMT", "CMT", "DVID",
          "OCC", "VISIT", "flagged_by"
        ),
        names(sub)
      )
      events <- signif_doubles(sub[, cols, drop = FALSE])
      names(events)[match(c(".rowid", "flagged_by"), cols)] <- c(
        "row", "flagged by"
      )
      table <- DT::datatable(
        events,
        class = "compact nowrap",
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE
      )
      DT::formatStyle(
        table, names(events),
        valueColumns = "flagged by",
        backgroundColor = DT::styleEqual("", "", "var(--bs-danger-bg-subtle)")
      )
    })

    invisible(NULL)
  })
}
