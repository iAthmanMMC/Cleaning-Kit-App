#' Excel files in the project's data folder
#'
#' The same files `cleaningkit::filter_new_records()` will look at: `.xlsx`
#' and `.xls` directly inside `data/`, without Excel's `~$` lock files.
#'
#' @param data_dir The data folder.
#' @return A data frame with `file`, `size_kb` and `modified`, newest first.
#' @noRd
data_files <- function(data_dir) {
  empty <- data.frame(
    file = character(0),
    size_kb = numeric(0),
    modified = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(data_dir) || !dir.exists(data_dir)) {
    return(empty)
  }
  files <- list.files(
    data_dir,
    pattern = "\\.xlsx$|\\.xls$",
    ignore.case = TRUE
  )
  files <- files[!grepl("^~\\$", files)]
  if (length(files) == 0) {
    return(empty)
  }
  info <- file.info(file.path(data_dir, files))
  out <- data.frame(
    file = files,
    size_kb = round(info$size / 1024),
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
  out[order(info$mtime, decreasing = TRUE), , drop = FALSE]
}

#' Number of surveys already recorded in the processed-uuid ledger
#'
#' @return An integer, or `NA` when there is no ledger yet (first round).
#' @noRd
ledger_count <- function(data_dir, ledger_name = "processed_uuids.csv") {
  path <- file.path(data_dir, ledger_name)
  if (!file.exists(path)) {
    return(NA_integer_)
  }
  ledger <- tryCatch(
    utils::read.csv(path, colClasses = "character"),
    error = function(e) NULL
  )
  if (is.null(ledger) || !"uuid" %in% names(ledger)) {
    return(NA_integer_)
  }
  length(unique(ledger$uuid[nzchar(ledger$uuid)]))
}

#' Prepare the raw data for a round
#'
#' The data block of example script 1: optionally reduce the exports in
#' `data/` to records not validated before (`filter_new_records()`), then read
#' the result with `read_raw_data()`.
#'
#' @param data_dir The project's data folder.
#' @param tool_survey Survey sheet from `load_tool()`.
#' @param filter_new Run `filter_new_records()` first.
#' @param file File in `data_dir` to read when `filter_new` is `FALSE`.
#' @param uuid_column,skip_label_row,sheet,output_name,use_ledger,archive
#'   Passed to cleaningkit.
#' @param extra_date_cols,extra_datetime_cols Passed to `read_raw_data()`.
#' @return A list with `raw_data`, `path` (the file read) and
#'   `filter_summary` (the list `filter_new_records()` returns, without its
#'   `data`, or `NULL`).
#' @noRd
prepare_data <- function(
  data_dir,
  tool_survey,
  filter_new = TRUE,
  file = NULL,
  uuid_column = "_uuid",
  skip_label_row = TRUE,
  sheet = 1L,
  output_name = "data.xlsx",
  use_ledger = TRUE,
  archive = TRUE,
  extra_date_cols = character(0),
  extra_datetime_cols = character(0)
) {
  filter_summary <- NULL
  if (isTRUE(filter_new)) {
    res <- cleaningkit::filter_new_records(
      data_folder = data_dir,
      uuid_column = uuid_column,
      skip_label_row = skip_label_row,
      output_name = output_name,
      sheet = sheet,
      use_ledger = use_ledger,
      archive = archive,
      verbose = TRUE
    )
    filter_summary <- res[setdiff(names(res), "data")]
    path <- res$output_path
    # the filtered output is always written as a single-sheet workbook
    read_sheet <- 1L
  } else {
    if (is.null(file) || !nzchar(file)) {
      stop("Choose which file in data/ to read.")
    }
    path <- file.path(data_dir, file)
    read_sheet <- sheet
  }

  raw_data <- cleaningkit::read_raw_data(
    filename = path,
    tool_survey = tool_survey,
    extra_date_cols = if (length(extra_date_cols)) extra_date_cols else NULL,
    extra_datetime_cols = if (length(extra_datetime_cols)) {
      extra_datetime_cols
    } else {
      NULL
    },
    sheet = read_sheet,
    skip_label_row = skip_label_row
  )

  list(raw_data = raw_data, path = path, filter_summary = filter_summary)
}

#' Checks on the key columns of the raw data
#'
#' Catches the most common set-up mistake early, a column name in the
#' settings that does not match the export, before every check in the next
#' step fails on it.
#'
#' @param raw_data Data from `prepare_data()`.
#' @param general The `general` section of the profile.
#' @return A data frame with `item`, `state` (`"done"`, `"warn"`, `"error"`)
#'   and `detail`.
#' @noRd
data_column_checks <- function(raw_data, general) {
  body <- data_body(raw_data, general$skip_label_row)
  rows <- list()
  add <- function(item, state, detail) {
    rows[[length(rows) + 1]] <<- data.frame(
      item = item,
      state = state,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  uuid <- general$uuid_column
  if (!uuid %in% names(body)) {
    add("Survey ID", "error", paste0("No column '", uuid, "' in the data."))
  } else {
    ids <- body[[uuid]]
    n_missing <- sum(is.na(ids) | !nzchar(ids))
    n_dup <- sum(duplicated(ids[!is.na(ids) & nzchar(ids)]))
    if (n_missing + n_dup == 0) {
      add("Survey ID", "done", paste0("'", uuid, "' is filled in and unique."))
    } else {
      add(
        "Survey ID",
        "warn",
        paste0(
          "'",
          uuid,
          "': ",
          n_missing,
          " missing, ",
          n_dup,
          " duplicated."
        )
      )
    }
  }

  enum <- general$enumerator_column
  if (!enum %in% names(body)) {
    add("Enumerator", "error", paste0("No column '", enum, "' in the data."))
  } else {
    add(
      "Enumerator",
      "done",
      paste0(length(unique(stats::na.omit(body[[enum]]))), " enumerator(s).")
    )
  }

  date <- general$date_column
  if (!date %in% names(body)) {
    add("Interview date", "warn", paste0("No column '", date, "' in the data."))
  } else {
    rng <- date_range(body[[date]])
    add(
      "Interview date",
      if (is.null(rng)) "warn" else "done",
      if (is.null(rng)) "No readable dates." else paste(rng, collapse = " to ")
    )
  }

  do.call(rbind, rows)
}

#' The data without the ONA label row
#' @noRd
data_body <- function(raw_data, skip_label_row = TRUE) {
  if (isTRUE(skip_label_row) && nrow(raw_data) > 0) {
    raw_data[-1, , drop = FALSE]
  } else {
    raw_data
  }
}

#' First and last date in a column, as text, or `NULL`
#' @noRd
date_range <- function(x) {
  d <- suppressWarnings(as.Date(substr(as.character(x), 1, 10)))
  d <- d[!is.na(d)]
  if (length(d) == 0) {
    return(NULL)
  }
  format(range(d))
}

#' Records per value of a column, most first
#' @noRd
count_by <- function(raw_data, column, skip_label_row = TRUE) {
  body <- data_body(raw_data, skip_label_row)
  if (!column %in% names(body)) {
    return(NULL)
  }
  values <- as.character(body[[column]])
  values[is.na(values) | !nzchar(values)] <- "(blank)"
  counts <- sort(table(values), decreasing = TRUE)
  out <- data.frame(
    value = names(counts),
    records = as.integer(counts),
    stringsAsFactors = FALSE
  )
  names(out)[1] <- column
  out
}
