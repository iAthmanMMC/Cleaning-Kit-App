#' Run the cleaningkit app
#'
#' Starts the Shiny app on this computer. The app reads and writes files in a
#' project folder laid out by `cleaningkit::setup_project_folders()`, so survey
#' data stays on the machine it is run on.
#'
#' @param project_dir Optional path to a project folder to open on start-up.
#'   Leave `NULL` to choose one in the app.
#' @param launch.browser Open the app in the default browser. Passed to
#'   [shiny::runApp()].
#' @param port Port to listen on. `NULL` lets Shiny pick one.
#' @param max_upload_mb Largest file, in MB, that can be uploaded. Raw 4Mi
#'   exports can be large, so the default is 200.
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect of running the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(project_dir = "C:/4Mi/round_12")
#' }
run_app <- function(
  project_dir = NULL,
  launch.browser = TRUE,
  port = NULL,
  max_upload_mb = 200,
  ...
) {
  shiny::runApp(
    ck_app(project_dir = project_dir, max_upload_mb = max_upload_mb),
    launch.browser = launch.browser,
    port = port,
    ...
  )
}

#' Build the app object without running it
#'
#' Used by [run_app()] and by the tests.
#'
#' @inheritParams run_app
#' @return A `shiny.appobj`.
#' @noRd
ck_app <- function(project_dir = NULL, max_upload_mb = 200) {
  shiny::shinyApp(
    ui = app_ui,
    server = function(input, output, session) {
      app_server(input, output, session, project_dir = project_dir)
    },
    onStart = function() {
      old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
      shiny::onStop(function() options(old))
    }
  )
}
