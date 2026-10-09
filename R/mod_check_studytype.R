#' Study type module UI
#'
#' Displays the inferred study type(s) with the evidence for each and lets the
#' user confirm or override the selection that gates conditional checks.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_check_studytype_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("2. Confirm study type"),
    uiOutput(ns("notice")),
    checkboxGroupInput(
      ns("confirmed"),
      label = NULL,
      choices = character(),
      width = "100%"
    ),
    uiOutput(ns("n_checks"))
  )
}

#' Study type module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @return A reactive returning the confirmed study type(s) as a character
#'   vector; always includes `"All"`.
#' @noRd
mod_check_studytype_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    inferred <- reactive({
      req(data_r())
      inf <- infer_study_type(data_r())
      inf[inf$study_type != "All", ]
    })

    output$notice <- renderUI({
      if (is.null(data_r())) {
        return(ui_notice(
          "Confirm the variable mapping above to infer the study type."
        ))
      }
      helpText(
        "Pre-selected from the data. Each selected type adds its specific ",
        "checks to the general ones; change the selection if it is wrong."
      )
    })

    observeEvent(inferred(), {
      inf <- inferred()
      updateCheckboxGroupInput(
        session, "confirmed",
        choiceNames = lapply(seq_len(nrow(inf)), function(i) {
          tagList(
            tags$strong(inf$study_type[i]),
            tags$span(class = "text-muted", paste0(" \u2014 ", inf$reason[i]))
          )
        }),
        choiceValues = inf$study_type,
        selected = inf$study_type[inf$applicable]
      )
    })

    confirmed <- reactive(c("All", input$confirmed))

    output$n_checks <- renderUI({
      req(data_r())
      registry <- check_registry()
      n <- sum(vapply(registry, check_applies, logical(1), confirmed()))
      tags$small(
        class = "text-muted",
        sprintf("%d of %d checks apply to this selection.", n, length(registry))
      )
    })

    confirmed
  })
}
