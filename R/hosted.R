# Hosted mode: the app running on a server (Posit Connect Cloud, shinyapps.io,
# Shiny Server) instead of on the data officer's own computer.
#
# Run locally, the app works in a project folder the user chooses on their own
# disk. Served, "a folder on disk" means the server's disk, which every visitor
# shares. So when hosted:
#
#   * every browser session gets its own temporary workspace folder, created
#     when the session starts and deleted when it ends (tab closed or
#     refreshed, or the app going idle);
#   * the folder picker and the typed folder path are never set up: their
#     server-side observers do not exist, so no input sent over the websocket
#     can open another folder on the server, such as another session's
#     workspace;
#   * "Show in folder" is gone, since that folder is not on the user's
#     computer; every output is a download, and the whole workspace can be
#     downloaded as a zip and uploaded again to carry a round over several
#     days.

#' Is the app being served rather than run locally?
#'
#' An option and an environment variable decide first, so a deployment can say
#' what it is instead of relying on the guess. Set
#' `CLEANINGKITAPP_SERVER=true` in the host's environment settings.
#'
#' @return `TRUE` when the app appears to be served.
#' @export
ck_is_server <- function() {
  override <- getOption("cleaningkitapp.server", NULL)
  if (!is.null(override)) {
    return(isTRUE(override))
  }
  from_env <- Sys.getenv("CLEANINGKITAPP_SERVER", "")
  if (nzchar(from_env)) {
    return(tolower(from_env) %in% c("1", "true", "yes"))
  }
  any(c(
    nzchar(Sys.getenv("SHINY_PORT")), # Shiny Server, shinyapps.io
    nzchar(Sys.getenv("SHINY_SERVER_VERSION")), # Shiny Server
    identical(Sys.getenv("R_CONFIG_ACTIVE"), "rsconnect"), # shinyapps.io
    nzchar(Sys.getenv("CONNECT_SERVER")), # Posit Connect
    nzchar(Sys.getenv("RSTUDIO_CONNECT_HASTE")) # Posit Connect
  ))
}

#' Create a session's temporary workspace
#'
#' A project folder with the standard sub-folders, under the R process's temp
#' directory. The name is random, not derived from the session token, and it
#' is never shown to the user.
#'
#' @param root Where to create it. Defaults to `tempdir()`.
#' @return The folder path.
#' @noRd
new_workspace <- function(root = tempdir(), templates = FALSE) {
  dir <- tempfile("cleaningkitapp-", tmpdir = root)
  dir.create(dir, recursive = TRUE)
  if (templates) {
    # the folders plus the logical checklist template, as a local set-up does
    tryCatch(
      utils::capture.output(suppressMessages(
        cleaningkit::setup_project_folders(
          base_path = dir,
          copy_templates = TRUE
        )
      )),
      error = function(e) NULL
    )
  }
  for (sub in standard_folders()) {
    dir.create(file.path(dir, sub), recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(dir, winslash = "/")
}

#' Delete a workspace
#' @noRd
remove_workspace <- function(dir) {
  if (!is.null(dir) && dir.exists(dir)) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

#' Zip a workspace for download
#'
#' Everything in the project folder, with paths relative to it, so the zip
#' unpacks into `data/`, `resources/` and `output/`.
#'
#' @noRd
zip_workspace <- function(dir, zipfile) {
  files <- list.files(dir, recursive = TRUE, all.files = FALSE)
  if (length(files) == 0) {
    stop("The workspace is empty: there is nothing to download yet.")
  }
  zip::zip(zipfile, files = files, root = dir, mode = "mirror")
  invisible(zipfile)
}

#' Unpack an uploaded workspace zip into a new workspace
#'
#' Refuses entries that would land outside the new folder (absolute paths or
#' `..`). Accepts a zip whose files sit inside one top folder, which is what
#' zipping a project folder in Windows Explorer gives.
#'
#' @param zipfile Path to the uploaded zip.
#' @param root Where to create the new workspace.
#' @return The new workspace path.
#' @noRd
restore_workspace <- function(zipfile, root = tempdir()) {
  entries <- tryCatch(zip::zip_list(zipfile)$filename, error = function(e) {
    stop("This is not a zip file the app can read.")
  })
  unsafe <- grepl("^(/|\\\\|[A-Za-z]:)", entries) |
    grepl("(^|/|\\\\)[.][.](/|\\\\|$)", entries)
  if (any(unsafe)) {
    stop("The zip contains paths outside its own folder, so it was not opened.")
  }

  dir <- new_workspace(root)
  staging <- tempfile("unzip-", tmpdir = root)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  zip::unzip(zipfile, exdir = staging)

  # One top folder holding the project, e.g. round_12/data, round_12/output
  top <- list.files(staging, all.files = FALSE, no.. = TRUE)
  src <- staging
  if (
    length(top) == 1 &&
      dir.exists(file.path(staging, top)) &&
      !top %in% standard_folders()
  ) {
    src <- file.path(staging, top)
  }
  if (!any(dir.exists(file.path(src, standard_folders())))) {
    remove_workspace(dir)
    stop(
      "The zip has no data/, resources/ or output/ folder. Upload a workspace ",
      "downloaded from this app, or a zipped cleaningkit project folder."
    )
  }
  file.copy(
    list.files(src, full.names = TRUE, all.files = FALSE, no.. = TRUE),
    dir,
    recursive = TRUE,
    overwrite = TRUE
  )
  dir
}

#' File name for a downloaded workspace
#' @noRd
workspace_zip_name <- function(date = Sys.Date()) {
  paste0(format(date, "%Y-%m-%d"), "_cleaningkit_workspace.zip")
}
