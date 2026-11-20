#' App UI
#'
#' The navbar follows the two halves of the cleaningkit workflow. Validate
#' holds the steps of example script 1 built so far; the remaining steps
#' (checks, review workbook, and the Clean half) are added in later
#' milestones, see PLAN.md.
#'
#' @param request Needed for bookmarking; unused.
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(
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
      "Project",
      value = "project",
      icon = shiny::icon("folder-open"),
      mod_project_ui("project")
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
