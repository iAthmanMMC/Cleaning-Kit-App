#' App UI
#'
#' Start opens on two plain choices, each an upload-and-download flow:
#' "Check new data" and "Apply corrections". Run locally, the Project,
#' Validate and Clean tabs also keep the step-by-step folder workflow of the
#' example scripts (Validate is script 1, Clean script 2); online the app is
#' upload-only.
#'
#' @param request Needed for bookmarking; unused.
#' @noRd
app_ui <- function(request) {
  hosted <- ck_is_server()
  tabs <- list(
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
      "Apply corrections",
      value = "apply_flow",
      icon = shiny::icon("broom"),
      mod_apply_flow_ui("apply_flow")
    )
  )
  if (!hosted) {
    tabs <- c(tabs, folder_tabs())
  }
  do.call(
    bslib::page_navbar,
    c(
      list(
        id = "nav",
        title = htmltools::span(shiny::icon("broom"), "cleaningkit"),
        window_title = "cleaningkit",
        theme = ck_theme(),
        fillable = FALSE,
        sidebar = if (!hosted) {
          bslib::sidebar(
            id = "status_sidebar",
            title = "Folder progress",
            width = 260,
            open = "closed",
            shiny::uiOutput("status")
          )
        }
      ),
      tabs
    )
  )
}

#' The folder workflow: Project, Validate, Clean and the Run log
#'
#' Only when the app runs locally.
#' @noRd
folder_tabs <- function() {
  list(
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
