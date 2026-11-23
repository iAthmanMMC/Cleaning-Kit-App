#' File name for a review workbook
#'
#' `<date><suffix>.xlsm`, or `.xlsx` without macros, matching the scripts'
#' `paste0(Sys.Date(), "_follow-ups.xlsx")`.
#'
#' @noRd
review_file_name <- function(stem, vba = TRUE) {
  stem <- trimws(stem)
  stem <- sub("\\.xls[xm]?$", "", stem, ignore.case = TRUE)
  # characters Windows does not allow in file names
  stem <- gsub("[<>:\"/\\\\|?*]", "_", stem)
  if (!nzchar(stem)) {
    stop("Give the workbook a file name.")
  }
  paste0(stem, if (vba) ".xlsm" else ".xlsx")
}

#' Default file name stem for today's review workbook
#' @noRd
default_review_stem <- function(suffix = "_follow-ups", date = Sys.Date()) {
  paste0(format(date, "%Y-%m-%d"), suffix)
}

#' Build the review workbook
#'
#' The `create_review_workbook()` call of example script 1, with the settings
#' from the Review workbook screen.
#'
#' @param combined_log From `create_combined_log()`.
#' @param output_path Where to write it.
#' @param review The `review` section of the profile.
#' @param general The `general` section of the profile.
#' @param other_responses,other_db Leave `NULL` to leave the other-responses
#'   sheets out.
#' @param round_info Round memory from `make_round_info()`, written into the
#'   workbook's hidden sheets; `NULL` writes none.
#' @return `output_path`, invisibly.
#' @noRd
build_review_workbook <- function(
  combined_log,
  output_path,
  review,
  general,
  other_responses = NULL,
  other_db = NULL,
  round_info = NULL
) {
  if (is.null(combined_log)) {
    stop("Run the checks first: there is no cleaning log yet.")
  }
  include_other <- isTRUE(review$include_other) &&
    !is.null(other_responses) &&
    !is.null(other_db)
  group_by <- if (identical(review$group_by, "none")) NULL else review$group_by

  cleaningkit::create_review_workbook(
    write_list = combined_log,
    output_path = output_path,
    other_responses = if (include_other) other_responses,
    other_db = if (include_other) other_db,
    uuid_column = general$uuid_column,
    enumerator_column = general$enumerator_column,
    date_column = general$date_column,
    color_mode = review$color_mode,
    color_columns = review$color_columns,
    include_dataset = isTRUE(review$include_dataset),
    flag_columns = if (isTRUE(review$flag_columns)) {
      cleaningkit::ck_flag_column_defaults()
    },
    group_by = group_by,
    other_enumerator_id = if (include_other) general$enumerator_column,
    skip_label_row = general$skip_label_row,
    vba = isTRUE(review$vba),
    round_info = round_info
  )
  if (!file.exists(output_path)) {
    stop("The workbook was not written to ", output_path)
  }
  invisible(output_path)
}

#' Review workbooks already in output/follow_ups
#' @noRd
review_files <- function(project_dir) {
  dir <- file.path(project_dir, "output", "follow_ups")
  files <- list.files(dir, pattern = "\\.xls[xm]$", ignore.case = TRUE)
  files <- files[!grepl("^~\\$", files)]
  if (length(files) == 0) {
    return(data.frame(file = character(0), modified = character(0)))
  }
  info <- file.info(file.path(dir, files))
  out <- data.frame(
    file = files,
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
  out[order(info$mtime, decreasing = TRUE), , drop = FALSE]
}

#' Open a folder in the file explorer (desktop use only)
#' @noRd
open_folder <- function(path) {
  if (ck_is_server()) {
    return(invisible(NULL))
  }
  if (.Platform$OS.type == "windows") {
    shell.exec(normalizePath(path))
  } else {
    utils::browseURL(path)
  }
}
