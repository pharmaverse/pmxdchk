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
        mod_settings_ui("settings"),
        actionButton("run", "Run checks", class = "btn-primary")
      ),
      bslib::nav_panel(
        "Data",
        mod_data_upload_ui("upload"),
        mod_check_studytype_ui("studytype")
      ),
      bslib::nav_panel("Overview", mod_overview_ui("overview")),
      bslib::nav_panel("Profiles", mod_profile_ui("profile")),
      bslib::nav_panel("Findings", mod_findings_ui("findings"))
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
