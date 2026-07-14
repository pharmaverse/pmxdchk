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
      bslib::card_header("1. Upload & variable mapping"),
      fileInput(
        ns("file"), "NMPK dataset (CSV)",
        accept = c(".csv", ".txt")
      ),
      uiOutput(ns("map_status")),
      uiOutput(ns("map_panel")),
      uiOutput(ns("confirm_ui"))
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
      uiOutput(ns("preview_note")),
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

    core_vars <- c("ID", "TIME", "EVID", "MDV", "DV", "AMT")

    current_mapping <- reactive({
      canon <- names(nmpk_var_dictionary())
      m <- vapply(
        canon, function(x) input[[paste0("map_", x)]] %||% NA_character_,
        character(1)
      )
      if (all(is.na(m))) {
        return(guesses())
      }
      m[!is.na(m) & m != ""]
    })

    core_missing <- reactive(setdiff(core_vars, names(current_mapping())))

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
        hit <- grepl(
          tolower(val), tolower(as.character(d[[col]])), fixed = TRUE
        )
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
      cm <- current_mapping()
      n_all <- length(nmpk_var_dictionary())
      n_core <- sum(core_vars %in% names(cm))
      missing <- core_missing()
      badges <- div(
        class = "mb-2",
        ui_badge(paste0("mapped ", length(cm), "/", n_all), "info"),
        ui_badge(
          paste0("core ", n_core, "/", length(core_vars)),
          if (n_core == length(core_vars)) "success" else "warning"
        )
      )
      status <- if (length(missing) == 0) {
        ui_notice("All core variables mapped. Ready to confirm.", "success")
      } else {
        ui_notice(
          paste0(
            "Core variables not yet mapped: ",
            paste(missing, collapse = ", "), "."
          ),
          "warning"
        )
      }
      tagList(badges, status)
    })

    output$confirm_ui <- renderUI({
      req(raw())
      btn <- actionButton(
        ns("confirm"), "Confirm mapping",
        class = "btn-primary"
      )
      if (length(core_missing()) > 0) {
        btn$attribs$disabled <- "disabled"
      }
      btn
    })

    output$preview_note <- renderUI({
      req(raw())
      tags$small(
        class = "text-muted",
        sprintf(
          "Showing %d of %d rows (first 500 previewed).",
          nrow(preview_data()), nrow(raw())
        )
      )
    })

    observeEvent(input$confirm, {
      showNotification(
        "Mapping confirmed. Review the study type, then Run checks.",
        type = "message", duration = 6
      )
    })

    map_groups <- list(
      "Core (required)" = c("ID", "TIME", "EVID", "MDV", "DV", "AMT"),
      "Timing" = "NTIME",
      "Dosing" = c("CMT", "RATE", "DUR", "II", "ADDL", "SS", "DVID", "ROUTE"),
      "Occasion / period" = c("OCC", "DOSNO", "PERIOD"),
      "Study" = "STUDYID",
      "Covariates" = c("AGE", "WT", "BMI", "SEX", "RACE")
    )

    output$map_panel <- renderUI({
      req(raw())
      cols <- names(raw())
      g <- guesses()
      labels <- nmpk_var_labels()
      make_select <- function(canon) {
        selectizeInput(
          ns(paste0("map_", canon)),
          label = paste0(canon, " — ", labels[[canon]]),
          choices = c("(none)" = "", cols),
          selected = if (canon %in% names(g)) unname(g[canon]) else "",
          options = list(dropdownParent = "body")
        )
      }
      panels <- lapply(names(map_groups), function(gname) {
        body <- do.call(
          bslib::layout_columns,
          c(list(col_widths = 6), lapply(map_groups[[gname]], make_select))
        )
        bslib::accordion_panel(gname, body)
      })
      do.call(
        bslib::accordion,
        c(panels, list(open = "Core (required)", multiple = TRUE))
      )
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
