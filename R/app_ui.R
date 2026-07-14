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
      theme = bslib::bs_theme(version = 5),
      sidebar = bslib::sidebar(
        title = "Controls",
        uiOutput("run_ui"),
        uiOutput("run_status"),
        tags$hr(),
        mod_settings_ui("settings")
      ),
      bslib::nav_panel(
        "Data",
        mod_data_upload_ui("upload"),
        mod_check_studytype_ui("studytype"),
        bslib::card(
          bslib::card_header("3. Run checks"),
          uiOutput("run_data_ui"),
          helpText("Runs all applicable checks on the mapped dataset.")
        )
      ),
      bslib::nav_panel("Overview", mod_overview_ui("overview")),
      bslib::nav_panel("Profiles", mod_profile_ui("profile")),
      bslib::nav_panel("Findings", mod_findings_ui("findings")),
      bslib::nav_spacer(),
      bslib::nav_panel(
        "About",
        bslib::card(
          bslib::card_header("pmxdchk"),
          tags$p(
            "Data quality checking and review for NONMEM / ADPPK ",
            "pharmacometric datasets."
          ),
          tags$p(tags$strong("Workflow:")),
          tags$ol(
            tags$li("Data tab: upload a CSV, confirm the variable mapping, ",
                    "and confirm the inferred study type."),
            tags$li("Click Run checks (sidebar or Data tab)."),
            tags$li("Overview: dataset summary and severity of findings."),
            tags$li("Profiles: per-subject concentration-time review."),
            tags$li("Findings: inspect, triage, and export results.")
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
