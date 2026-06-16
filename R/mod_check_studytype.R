#' Study type module UI
#'
#' Displays the inferred study type(s) and lets the user confirm or override the
#' selection that gates conditional checks.
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_check_studytype_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Study type"),
    DT::DTOutput(ns("inferred")),
    checkboxGroupInput(
      ns("confirmed"),
      "Confirmed study type(s) used to select checks:",
      choices = character()
    ),
    helpText("Pre-selected from data inference; override as needed.")
  )
}

#' Study type module server
#'
#' @param id Module id.
#' @param data_r A reactive returning the mapped dataset.
#' @return A reactive returning the confirmed study type(s) as a character
#'   vector.
#' @noRd
mod_check_studytype_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    inferred <- reactive({
      req(data_r())
      infer_study_type(data_r())
    })

    output$inferred <- DT::renderDT(
      DT::datatable(
        inferred(),
        options = list(dom = "t"),
        rownames = FALSE
      )
    )

    observeEvent(inferred(), {
      inf <- inferred()
      updateCheckboxGroupInput(
        session, "confirmed",
        choices = inf$study_type,
        selected = inf$study_type[inf$applicable]
      )
    })

    reactive(input$confirmed)
  })
}
