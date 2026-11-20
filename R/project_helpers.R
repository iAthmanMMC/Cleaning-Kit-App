#' Folders `cleaningkit::setup_project_folders()` creates
#' @noRd
standard_folders <- function() {
  c("data", "resources", "output", "output/follow_ups", "output/final")
}

#' Which standard folders a project has
#'
#' @param project_dir Project folder.
#' @return A data frame with `folder`, `exists` and `files` (number of files
#'   directly inside, not counting sub-folders).
#' @noRd
project_folder_status <- function(project_dir) {
  folders <- standard_folders()
  paths <- file.path(project_dir, folders)
  exists <- dir.exists(paths)
  files <- vapply(
    seq_along(paths),
    function(i) {
      if (!exists[i]) {
        return(NA_integer_)
      }
      entries <- list.files(paths[i], full.names = TRUE)
      sum(!dir.exists(entries))
    },
    integer(1)
  )
  data.frame(
    folder = folders,
    exists = exists,
    files = files,
    stringsAsFactors = FALSE
  )
}

#' Clean up a folder path typed or pasted by the user
#'
#' Strips surrounding quotes and whitespace (Windows "Copy as path" adds
#' quotes) and normalises separators.
#'
#' @noRd
clean_dir_path <- function(path) {
  path <- trimws(path %||% "")
  path <- gsub('^["\']|["\']$', "", path)
  if (!nzchar(path)) {
    return("")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Copy uploaded files into a folder under their original names
#'
#' `shiny::fileInput()` stores uploads as `0.xlsx`, `1.xlsx`, ... in a temp
#' folder. cleaningkit finds files by name (`filter_new_records()` looks at
#' every Excel file in `data/`; the readers match `file_pattern`), so uploads
#' are copied over under the name they had on the user's computer.
#'
#' @param upload The value of a `fileInput()`: a data frame with `name` and
#'   `datapath`.
#' @param dest_dir Destination folder, created if needed.
#' @return The destination paths, invisibly.
#' @noRd
copy_uploads <- function(upload, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(upload$name))
  ok <- file.copy(upload$datapath, dest, overwrite = TRUE)
  if (!all(ok)) {
    stop(
      "Could not copy ",
      paste(upload$name[!ok], collapse = ", "),
      " into ",
      dest_dir,
      ". Is the file open in Excel?"
    )
  }
  invisible(dest)
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
