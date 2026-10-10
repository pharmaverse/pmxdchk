#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "pmxdchk",
      id = "main_nav",
      fillable = FALSE,
      theme = app_theme(),
      navbar_options = bslib::navbar_options(underline = FALSE),
      header = uiOutput("status_bar"),
      bslib::nav_panel(
        "Data",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(lg = c(5, 7)),
          fill = FALSE,
          div(
            mod_data_upload_ui("upload"),
            mod_check_studytype_ui("studytype"),
            bslib::card(
              bslib::card_header(ui_step(3, "Run checks")),
              div(
                class = "d-flex align-items-center gap-3",
                uiOutput("run_data_ui"),
                tags$span(
                  class = "text-body-secondary small",
                  "Runs every check that applies to the confirmed study type ",
                  "and opens the findings."
                )
              ),
              bslib::accordion(
                open = FALSE,
                bslib::accordion_panel(
                  "Thresholds (optional)", mod_settings_ui("settings"),
                  icon = icon("sliders")
                )
              )
            )
          ),
          div(class = "sticky-pane", mod_data_preview_ui("upload"))
        )
      ),
      bslib::nav_panel("Overview", mod_overview_ui("overview")),
      bslib::nav_panel(
        title = tagList("Findings", uiOutput("nav_findings", inline = TRUE)),
        value = "Findings",
        mod_findings_ui("findings")
      ),
      bslib::nav_panel("Profiles", mod_profile_ui("profile")),
      bslib::nav_panel(
        "Check library",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("What pmxdchk checks"),
          helpText(
            "Every check, the rule it applies, and what to do when it flags. ",
            "Checks limited to a study type run only when that type is ",
            "confirmed on the Data tab."
          ),
          DT::DTOutput("library", fill = FALSE)
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        "About",
        bslib::card(
          style = "max-width: 760px;",
          bslib::card_header("pmxdchk"),
          tags$p(
            "Data quality checking and review for NONMEM / ADPPK ",
            "pharmacometric datasets."
          ),
          tags$p(tags$strong("Workflow:")),
          tags$ol(
            tags$li(
              "Data: load a CSV or an example, confirm the variable mapping ",
              "and the inferred study type, then run the checks."
            ),
            tags$li(
              "Overview: a first look at the dataset \u2014 counts, all ",
              "concentration-time profiles, dose levels, missing values."
            ),
            tags$li(
              "Findings: work through the flagged checks. Each shows its ",
              "rule, the affected records and subject profiles, and what to ",
              "do; record your decision and export the results as CSV."
            ),
            tags$li(
              "Profiles: browse any subject's profile and records, with ",
              "flagged records marked."
            ),
            tags$li("Check library: the full list of checks and their rules.")
          )
        )
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pmxdchk"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
