#' Data upload module UI
#'
#' Upload a CSV NMPK dataset, preview it, and confirm the canonical variable
#' mapping (auto-guessed, manually overridable).
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      full_screen = TRUE,
      height = "600px",
      bslib::card_header("Upload & variable mapping"),
      fileInput(
        ns("file"), "NMPK dataset (CSV)",
        accept = c(".csv", ".txt")
      ),
      uiOutput(ns("map_status")),
      uiOutput(ns("map_panel")),
      actionButton(ns("confirm"), "Confirm mapping", class = "btn-primary")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Preview"),
      bslib::layout_columns(
        col_widths = c(5, 7),
        selectizeInput(
          ns("filter_col"), "Filter column",
          choices = NULL, options = list(dropdownParent = "body")
        ),
        textInput(ns("filter_val"), "Contains")
      ),
      DT::DTOutput(ns("preview"))
    )
  )
}

#' Data upload module server
#'
#' @param id Module id.
#' @return A reactive returning `list(data, mapping)` once the user confirms,
#'   where `data` uses canonical variable names.
#' @noRd
mod_data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    raw <- reactive({
      req(input$file)
      readr::read_csv(input$file$datapath, show_col_types = FALSE)
    })

    guesses <- reactive(guess_mapping(names(raw())))

    observeEvent(raw(), {
      updateSelectizeInput(
        session, "filter_col",
        choices = c("(all columns)" = "", names(raw())),
        selected = "", server = FALSE
      )
    })

    preview_data <- reactive({
      req(raw())
      d <- dplyr::slice_head(raw(), n = 500)
      col <- input$filter_col
      val <- input$filter_val
      if (!is.null(col) && nzchar(col) && !is.null(val) && nzchar(val)) {
        hit <- grepl(tolower(val), tolower(as.character(d[[col]])), fixed = TRUE)
        hit[is.na(hit)] <- FALSE
        d <- d[hit, , drop = FALSE]
      }
      d
    })

    output$preview <- DT::renderDT(
      {
        DT::datatable(
          preview_data(),
          extensions = "Buttons",
          class = "compact stripe",
          options = list(
            scrollX = TRUE,
            scrollY = "350px",
            scrollCollapse = TRUE,
            pageLength = 50,
            dom = "Bfrtip",
            buttons = c("colvis", "csv")
          ),
          rownames = FALSE
        )
      }
    )

    output$map_status <- renderUI({
      req(raw())
      core <- c("ID", "TIME", "EVID", "MDV", "DV", "AMT")
      missing <- setdiff(core, names(guesses()))
      if (length(missing) == 0) {
        div(
          class = "text-success",
          "All core variables detected. Review the mapping and confirm."
        )
      } else {
        div(
          class = "text-warning",
          paste0(
            "Core variables not auto-detected: ",
            paste(missing, collapse = ", "), ". Map them below."
          )
        )
      }
    })

    output$map_panel <- renderUI({
      req(raw())
      cols <- names(raw())
      g <- guesses()
      labels <- nmpk_var_labels()
      sels <- lapply(names(nmpk_var_dictionary()), function(canon) {
        selectizeInput(
          ns(paste0("map_", canon)),
          label = paste0(canon, " — ", labels[[canon]]),
          choices = c("(none)" = "", cols),
          selected = if (canon %in% names(g)) unname(g[canon]) else "",
          options = list(dropdownParent = "body")
        )
      })
      do.call(tagList, sels)
    })

    eventReactive(input$confirm, {
      canon <- names(nmpk_var_dictionary())
      mapping <- vapply(
        canon, function(x) input[[paste0("map_", x)]] %||% "", character(1)
      )
      mapping <- mapping[mapping != ""]
      list(
        data = apply_mapping(raw(), mapping),
        mapping = mapping
      )
    })
  })
}
