#' App UI
#'
#' Start opens on two plain choices. "Check new data" is the upload-only
#' flow; the Project, Validate and Clean tabs keep the step-by-step folder
#' workflow of the example scripts (Validate is script 1, Clean script 2).
#'
#' @param request Needed for bookmarking; unused.
#' @noRd
app_ui <- function(request) {
  hosted <- ck_is_server()
  bslib::page_navbar(
    id = "nav",
    title = htmltools::span(
      shiny::icon("broom"),
      "cleaningkit"
    ),
    window_title = "cleaningkit",
    theme = ck_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      id = "status_sidebar",
      title = "Progress",
      width = 260,
      open = "desktop",
      shiny::uiOutput("status")
    ),
    bslib::nav_panel(
      "Start",
      value = "start",
      icon = shiny::icon("house"),
      mod_start_ui("start")
    ),
    bslib::nav_panel(
      "Check new data",
      value = "check_flow",
      icon = shiny::icon("upload"),
      mod_check_flow_ui("check_flow")
    ),
    bslib::nav_panel(
      "Project",
      value = "project",
      icon = shiny::icon("folder-open"),
      mod_project_ui("project", hosted = hosted)
    ),
    bslib::nav_menu(
      "Validate",
      icon = shiny::icon("magnifying-glass"),
      bslib::nav_panel(
        "1. Tool",
        value = "tool",
        icon = shiny::icon("file-lines"),
        mod_tool_ui("tool")
      ),
      bslib::nav_panel(
        "2. Data",
        value = "data",
        icon = shiny::icon("table"),
        mod_data_ui("data")
      ),
      bslib::nav_panel(
        "3. Checks",
        value = "checks",
        icon = shiny::icon("list-check"),
        mod_checks_ui("checks")
      ),
      bslib::nav_panel(
        "4. Results",
        value = "results",
        icon = shiny::icon("chart-column"),
        mod_results_ui("results")
      ),
      bslib::nav_panel(
        "5. Review workbook",
        value = "review",
        icon = shiny::icon("file-excel"),
        mod_review_ui("review")
      )
    ),
    bslib::nav_menu(
      "Clean",
      icon = shiny::icon("broom"),
      bslib::nav_panel(
        "6. Import reviewed",
        value = "import",
        icon = shiny::icon("file-import"),
        mod_import_ui("import")
      ),
      bslib::nav_panel(
        "7. Apply",
        value = "apply",
        icon = shiny::icon("wand-magic-sparkles"),
        mod_apply_ui("apply")
      ),
      bslib::nav_panel(
        "8. Final output",
        value = "final",
        icon = shiny::icon("file-export"),
        mod_final_ui("final")
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_panel(
      "Run log",
      value = "runlog",
      icon = shiny::icon("terminal"),
      mod_runlog_ui("runlog")
    )
  )
}
