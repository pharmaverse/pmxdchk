#' Data upload module UI
#'
#' Load a delimited NMPK dataset or a built-in example and confirm the canonical
#' variable mapping (auto-guessed, manually overridable).
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(ui_step(1, "Load data and map variables")),
      fileInput(
        ns("file"), "NMPK dataset (delimited text: .csv, .txt, .dat)",
        accept = c(".csv", ".txt", ".dat", ".tsv"), width = "100%"
      ),
      div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-3",
        tags$span(class = "text-body-secondary small", "Or try an example:"),
        actionButton(
          ns("example_issues"), "Example with issues",
          class = "btn-sm btn-outline-secondary"
        ),
        actionButton(
          ns("example_clean"), "Clean example",
          class = "btn-sm btn-outline-secondary"
        )
      ),
      uiOutput(ns("map_status")),
      uiOutput(ns("map_panel")),
      uiOutput(ns("confirm_ui"))
    )
  )
}

#' Data preview UI
#'
#' Filterable preview of the loaded dataset; shown once data is loaded. Uses the
#' same module id as [mod_data_upload_ui()].
#'
#' @param id Module id.
#' @return A UI definition.
#' @noRd
mod_data_preview_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Data preview"),
    conditionalPanel(
      "!output.has_data", ns = ns,
      div(
        class = "text-center text-body-secondary py-5",
        icon("table", class = "fa-2x mb-3"),
        tags$p("The loaded dataset appears here.")
      )
    ),
    conditionalPanel(
      "output.has_data", ns = ns,
      div(
        class = "toolbar mb-2",
        selectizeInput(
          ns("filter_col"), "Filter column",
          choices = NULL, width = "220px",
          options = list(dropdownParent = "body")
        ),
        textInput(ns("filter_val"), "Contains", width = "220px"),
        div(class = "ms-auto", uiOutput(ns("preview_note")))
      ),
      DT::DTOutput(ns("preview"), fill = FALSE)
    )
  )
}

#' Read a delimited NMPK dataset
#'
#' Guesses the delimiter (comma, semicolon, tab, or space) and reads `.`, the
#' NONMEM convention for a null value, as missing.
#'
#' @param path Path to the file.
#' @return A tibble.
#' @noRd
read_nmpk_file <- function(path) {
  suppressWarnings(readr::read_delim(
    path,
    delim = NULL, na = c("", "NA", "."), trim_ws = TRUE, show_col_types = FALSE
  ))
}

#' Data upload module server
#'
#' @param id Module id.
#' @return A reactive returning `list(data, mapping, name)` once the user
#'   confirms the mapping, where `data` uses canonical variable names. It
#'   returns `NULL` again as soon as a different dataset is loaded.
#' @noRd
mod_data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded <- reactiveVal(NULL)
    confirmed <- reactiveVal(NULL)
    load_data <- function(data, name) {
      confirmed(NULL)
      loaded(list(data = tibble::as_tibble(data), name = name))
    }

    observeEvent(input$file, {
      data <- tryCatch(
        read_nmpk_file(input$file$datapath),
        error = function(e) NULL
      )
      if (is.null(data) || ncol(data) < 2) {
        showNotification(
          "Could not read this file as a delimited text dataset.",
          type = "error"
        )
        return()
      }
      load_data(data, input$file$name)
    })
    observeEvent(input$example_clean, {
      load_data(pmxdchk::adppk_example, "adppk_example (built-in)")
    })
    observeEvent(input$example_issues, {
      load_data(pmxdchk::adppk_corrupted, "adppk_corrupted (built-in)")
    })

    raw <- reactive({
      req(loaded())
      loaded()$data
    })

    output$has_data <- reactive(!is.null(loaded()))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

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
      m <- m[!is.na(m) & m != ""]
      m[m %in% names(raw())]
    })

    core_missing <- reactive(setdiff(core_vars, names(current_mapping())))
    reused <- reactive({
      cm <- current_mapping()
      unique(cm[duplicated(cm)])
    })
    mapping_ok <- reactive(length(core_missing()) == 0 && length(reused()) == 0)

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
          class = "compact nowrap",
          options = list(
            scrollX = TRUE,
            scrollY = "55vh",
            scrollCollapse = TRUE,
            pageLength = 50,
            dom = "Brtip",
            buttons = c("colvis", "csv")
          ),
          rownames = FALSE
        )
      }
    )

    output$map_status <- renderUI({
      if (is.null(loaded())) {
        return(ui_notice(
          "Upload a NONMEM-style or ADPPK dataset, or load an example."
        ))
      }
      cm <- current_mapping()
      n_core <- sum(core_vars %in% names(cm))
      missing <- core_missing()
      badges <- div(
        class = "d-flex flex-wrap align-items-center gap-1 mb-2",
        tags$strong(loaded()$name),
        tags$span(
          class = "text-body-secondary me-2",
          sprintf("%d rows \u00b7 %d columns", nrow(raw()), ncol(raw()))
        ),
        ui_badge(
          paste0("core variables ", n_core, "/", length(core_vars)),
          if (n_core == length(core_vars)) "success" else "warning"
        ),
        ui_badge(paste0("optional ", length(cm) - n_core, " mapped"))
      )
      status <- if (length(reused()) > 0) {
        ui_notice(
          paste0(
            "A column can be mapped only once. Used more than once: ",
            paste(reused(), collapse = ", "), "."
          ),
          "warning"
        )
      } else if (length(missing) > 0) {
        ui_notice(
          paste0(
            "Core variables not yet mapped: ",
            paste(missing, collapse = ", "), "."
          ),
          "warning"
        )
      } else if (!is.null(confirmed())) {
        ui_notice(
          "Mapping confirmed. Continue with the study type below.", "success"
        )
      } else {
        ui_notice(
          "All core variables were matched. Review the mapping and confirm.",
          "info"
        )
      }
      tagList(badges, status)
    })

    output$confirm_ui <- renderUI({
      req(raw())
      first <- is.null(confirmed())
      btn <- actionButton(
        ns("confirm"),
        if (first) "Confirm mapping" else "Update mapping",
        class = if (first) "btn-primary" else "btn-outline-primary"
      )
      if (!mapping_ok()) {
        btn$attribs$disabled <- "disabled"
      }
      div(class = "mt-3", btn)
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

    map_groups <- list(
      "Core (required)" = c("ID", "TIME", "EVID", "MDV", "DV", "AMT"),
      "Timing" = c("NTIME", "VISIT"),
      "Dosing" = c("CMT", "RATE", "DUR", "II", "ADDL", "SS", "DVID", "ROUTE"),
      "Occasion / period" = c("OCC", "DOSNO", "PERIOD"),
      "Study and subject" = c("STUDYID", "USUBJID"),
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
          label = paste0(canon, " \u2014 ", labels[[canon]]),
          choices = c("(none)" = "", cols),
          selected = if (canon %in% names(g)) unname(g[canon]) else "",
          options = list(dropdownParent = "body")
        )
      }
      panels <- lapply(names(map_groups), function(gname) {
        vars <- map_groups[[gname]]
        body <- do.call(
          bslib::layout_columns,
          c(list(col_widths = 6), lapply(vars, make_select))
        )
        title <- sprintf(
          "%s (%d of %d matched)", gname, sum(vars %in% names(g)), length(vars)
        )
        bslib::accordion_panel(title, body, value = gname)
      })
      open <- if (all(core_vars %in% names(g))) FALSE else "Core (required)"
      do.call(
        bslib::accordion,
        c(panels, list(open = open, multiple = TRUE))
      )
    })

    observeEvent(input$confirm, {
      req(mapping_ok())
      mapping <- current_mapping()
      confirmed(list(
        data = apply_mapping(raw(), mapping),
        mapping = mapping,
        name = loaded()$name
      ))
    })

    reactive(confirmed())
  })
}
