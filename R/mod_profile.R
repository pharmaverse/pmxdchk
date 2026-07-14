#' Individual profile browser module UI
#'
#' Per-subject concentration-time profile with flagged records overlaid, dose
#' markers, prev/next navigation, and the subject's event table.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Subject"),
      bslib::layout_columns(
        selectizeInput(
          ns("subject"), "Subject", choices = NULL,
          options = list(dropdownParent = "body")
        ),
        checkboxInput(ns("flagged_only"), "Flagged subjects only", FALSE),
        checkboxInput(ns("log_y"), "Log y-axis", TRUE),
        div(
          actionButton(ns("prev"), "Previous"),
          actionButton(ns("nxt"), "Next")
        )
      ),
      uiOutput(ns("counter"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Concentration-time profile"),
      plotOutput(ns("plot"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Checks flagged for this subject"),
      DT::DTOutput(ns("subject_checks"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Event records"),
      DT::DTOutput(ns("events"))
    )
  )
}

#' Individual profile browser module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @param results_r A reactive returning the findings tibble from
#'   [run_nmpk_checks()] (with check results attached).
#' @return Invisibly `NULL`.
#' @noRd
mod_profile_server <- function(id, data_r, results_r) {
  moduleServer(id, function(input, output, session) {
    output$notice <- renderUI({
      if (is.null(data_r())) {
        return(ui_notice(
          "Confirm the variable mapping on the Data tab to browse profiles."
        ))
      }
      if (is.null(results_r())) {
        return(ui_notice(
          "Run checks to overlay flags. Profiles show without flags until then.",
          "secondary"
        ))
      }
      NULL
    })

    data_id <- reactive({
      d <- data_r()
      d[[".rowid"]] <- seq_len(nrow(d))
      d
    })

    flagged_rows <- reactive({
      objs <- attr(results_r(), "results")
      ids <- lapply(objs, function(r) {
        fr <- r$flagged_records
        if (!is.null(fr) && ".rowid" %in% names(fr)) fr$.rowid else NULL
      })
      unique(unlist(ids, use.names = FALSE))
    })

    subjects <- reactive({
      req(data_id())
      d <- data_id()
      ids <- unique(d$ID)
      if (isTRUE(input$flagged_only)) {
        req(results_r())
        flagged_ids <- unique(d$ID[d$.rowid %in% flagged_rows()])
        ids <- ids[ids %in% flagged_ids]
      }
      ids
    })

    observeEvent(subjects(), {
      updateSelectInput(session, "subject", choices = subjects())
    })

    observeEvent(input$prev, {
      s <- subjects()
      i <- match(input$subject, s)
      if (!is.na(i) && i > 1) {
        updateSelectInput(session, "subject", selected = s[i - 1])
      }
    })
    observeEvent(input$nxt, {
      s <- subjects()
      i <- match(input$subject, s)
      if (!is.na(i) && i < length(s)) {
        updateSelectInput(session, "subject", selected = s[i + 1])
      }
    })

    output$counter <- renderUI({
      s <- subjects()
      req(length(s) > 0, input$subject)
      i <- match(input$subject, s)
      tags$small(
        class = "text-muted",
        sprintf("Subject %d of %d", i, length(s))
      )
    })

    current <- reactive({
      req(input$subject)
      d <- data_id()
      keep <- as.character(d$ID) == as.character(input$subject)
      sub <- d[keep, , drop = FALSE]
      sub$TIME <- suppressWarnings(as.numeric(sub$TIME))
      sub$DV <- suppressWarnings(as.numeric(sub$DV))
      sub$.evid <- suppressWarnings(as.numeric(sub$EVID))
      sub$flagged <- sub$.rowid %in% flagged_rows()
      sub
    })

    output$plot <- renderPlot({
      sub <- current()
      obs <- sub[sub$.evid == 0 & !is.na(sub$DV), , drop = FALSE]
      doses <- sub[sub$.evid %in% c(1, 4), , drop = FALSE]
      req(nrow(obs) > 0)

      p <- ggplot2::ggplot(obs, ggplot2::aes(x = TIME, y = DV)) +
        ggplot2::geom_line(color = "grey40") +
        ggplot2::geom_point(ggplot2::aes(color = flagged), size = 2.5) +
        ggplot2::scale_color_manual(
          values = c("FALSE" = "black", "TRUE" = "red"),
          labels = c("FALSE" = "ok", "TRUE" = "flagged"),
          drop = FALSE
        ) +
        ggplot2::labs(x = "TIME", y = "DV", color = NULL) +
        ggplot2::theme_minimal()

      if (nrow(doses) > 0) {
        p <- p + ggplot2::geom_vline(
          data = doses, ggplot2::aes(xintercept = TIME),
          linetype = "dashed", color = "grey60"
        )
      }
      p <- p + ggplot2::labs(title = paste("Subject", input$subject))
      if (isTRUE(input$log_y)) {
        p <- p + ggplot2::scale_y_log10()
        n_np <- sum(obs$DV <= 0, na.rm = TRUE)
        if (n_np > 0) {
          p <- p + ggplot2::labs(caption = paste0(
            n_np, " BLQ / non-positive point(s) omitted on the log scale."
          ))
        }
      }
      p
    })

    subject_findings <- reactive({
      req(input$subject)
      if (is.null(results_r())) {
        return(NULL)
      }
      objs <- attr(results_r(), "results")
      rows <- current()$.rowid
      hits <- lapply(names(objs), function(cid) {
        fr <- objs[[cid]]$flagged_records
        if (!is.null(fr) && ".rowid" %in% names(fr) && any(fr$.rowid %in% rows)) {
          data.frame(
            check_id = cid, severity = objs[[cid]]$severity,
            message = objs[[cid]]$message, stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })
      dplyr::bind_rows(hits)
    })

    output$subject_checks <- DT::renderDT({
      sf <- subject_findings()
      validate(need(
        !is.null(sf) && nrow(sf) > 0,
        "No checks flagged this subject (run checks first, or none flagged)."
      ))
      DT::datatable(
        sf,
        options = list(pageLength = 5, dom = "tp"),
        rownames = FALSE
      )
    })

    output$events <- DT::renderDT({
      sub <- current()
      cols <- intersect(
        c(".rowid", "TIME", "EVID", "MDV", "DV", "AMT", "flagged"), names(sub)
      )
      DT::datatable(
        sub[, cols, drop = FALSE],
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "flagged",
          target = "row",
          backgroundColor = DT::styleEqual(TRUE, "#fde8e8")
        )
    })

    invisible(NULL)
  })
}
