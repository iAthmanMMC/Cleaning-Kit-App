# The "Check new data" flow: files in, files out, no project folder.
#
# Each step is a plain function here, so the flow screen stays thin and the
# steps can be tested without Shiny. The round's memory (survey IDs already
# checked, the questionnaire, the settings) comes in and goes out through the
# hidden sheets cleaningkit writes into the review workbook; see
# cleaningkit::ck_round_info().

#' Columns tried, in order, when detecting the main columns of an export
#' @noRd
.column_candidates <- list(
  uuid_column = c("_uuid", "uuid", "meta/instanceID", "instanceID"),
  enumerator_column = c(
    "username",
    "enumerator",
    "enumerator_id",
    "_submitted_by"
  ),
  date_column = c("today", "date", "start", "_submission_time")
)

#' Does the first row hold the ONA question labels?
#'
#' ONA exports with labels put them in row one. That row's survey ID cell is
#' empty, the column name, or text with spaces, or it is simply a different
#' shape from the IDs below it.
#'
#' @noRd
detect_label_row <- function(df, uuid_column) {
  v <- as.character(df[[uuid_column]])
  if (length(v) < 2) {
    return(FALSE)
  }
  first <- v[1]
  if (is.na(first) || !nzchar(trimws(first))) {
    return(TRUE)
  }
  if (grepl("\\s", first) || tolower(first) %in% c("_uuid", "uuid")) {
    return(TRUE)
  }
  rest <- v[-1]
  rest <- rest[!is.na(rest) & nzchar(rest)]
  if (length(rest) == 0) {
    return(FALSE)
  }
  shape <- function(x) gsub("[0-9]", "9", gsub("[A-Za-z]", "a", x))
  common <- names(sort(table(shape(rest)), decreasing = TRUE))[1]
  !identical(shape(first), common)
}

#' Work out the general settings from the download itself
#'
#' Keeps a setting from `general` when the download has that column, and
#' otherwise takes the first candidate the download has. The label row is
#' always detected.
#'
#' @param df The download, read as text.
#' @param general The `general` section of a profile: the starting point.
#' @return `general` with the detected values, plus `missing`: the settings
#'   for which no column was found.
#' @noRd
detect_general <- function(df, general = default_profile()$general) {
  cols <- names(df)
  missing <- character(0)
  for (key in names(.column_candidates)) {
    current <- general[[key]]
    if (!is.null(current) && current %in% cols) {
      next
    }
    hit <- intersect(.column_candidates[[key]], cols)
    if (length(hit)) {
      general[[key]] <- hit[1]
    } else {
      missing <- c(missing, key)
    }
  }
  if (!"uuid_column" %in% missing) {
    general$skip_label_row <- detect_label_row(df, general$uuid_column)
  }
  general$missing <- missing
  general
}

#' Read the first sheet of a download as text, for detection
#' @noRd
read_download_text <- function(path) {
  df <- tryCatch(
    readxl::read_excel(path, sheet = 1, col_types = "text"),
    error = function(e) {
      stop("The download could not be read as an Excel file (.xlsx).")
    }
  )
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read what earlier rounds checked from uploaded files
#'
#' @param upload A `fileInput()` value (`name`, `datapath`), or `NULL` for the
#'   first round.
#' @return A `ck_round_info`, or `NULL` for the first round.
#' @noRd
read_previous <- function(upload, general) {
  if (is.null(upload) || nrow(upload) == 0) {
    return(NULL)
  }
  cleaningkit::read_round_info(
    upload$datapath,
    uuid_column = general$uuid_column,
    skip_label_row = general$skip_label_row,
    names = upload$name
  )
}

#' The profile a round starts from
#'
#' The settings kept in the last workbook if there are any, otherwise the
#' defaults.
#' @noRd
profile_from_previous <- function(previous) {
  text <- previous$settings$cleaningkitapp_profile
  if (is.null(text) || !nzchar(text)) {
    return(default_profile())
  }
  saved <- tryCatch(yaml::yaml.load(text), error = function(e) NULL)
  if (!is.list(saved)) {
    return(default_profile())
  }
  normalise_profile(utils::modifyList(default_profile(), saved))
}

#' Put the questionnaire somewhere load_tool() can read it
#'
#' From an uploaded XLSForm when there is one, otherwise from the copy kept in
#' the last workbook.
#'
#' @param xlsform A `fileInput()` value, or `NULL`.
#' @param previous A `ck_round_info`, or `NULL`.
#' @param dir Folder to write into (the session workspace).
#' @return The path of the XLSForm to load.
#' @noRd
questionnaire_path <- function(xlsform, previous, dir) {
  dest <- file.path(dir, "resources", "questionnaire.xlsx")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(xlsform) && nrow(xlsform) > 0) {
    file.copy(xlsform$datapath[1], dest, overwrite = TRUE)
    return(dest)
  }
  if (is.null(previous$tool_survey) || is.null(previous$tool_choices)) {
    stop(
      "The files from the last round don't include the questionnaire. ",
      "Upload the XLSForm."
    )
  }
  openxlsx::write.xlsx(
    list(survey = previous$tool_survey, choices = previous$tool_choices),
    dest
  )
  dest
}

#' Put the logical checklist where the logical check looks for it
#'
#' An uploaded checklist wins, then the one kept in the last workbook. With
#' neither, the empty template `setup_project_folders()` copied is removed, so
#' the logical check is left out rather than run on blank rules.
#' @return The checklist path, or `NULL` when there is none.
#' @noRd
place_checklist <- function(upload, previous, dir) {
  dest <- file.path(dir, "resources", "logical_checklist.xlsx")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(upload) && nrow(upload) > 0) {
    file.copy(upload$datapath[1], dest, overwrite = TRUE)
  } else if (!is.null(previous$logical_checklist)) {
    openxlsx::write.xlsx(previous$logical_checklist, dest)
  } else {
    unlink(dest)
    return(NULL)
  }
  dest
}

#' Read a workbook sheet as text, for keeping in the round memory
#' @noRd
sheet_as_text <- function(path, sheet = 1) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  out <- tryCatch(
    readxl::read_excel(path, sheet = sheet, col_types = "text"),
    error = function(e) NULL
  )
  if (is.null(out)) NULL else as.data.frame(out, check.names = FALSE)
}

#' Start a round from the uploaded files
#'
#' Reads what earlier rounds checked, loads the questionnaire, reads the new
#' download and keeps only the surveys not checked before.
#'
#' @param download `fileInput()` value of the new ONA download.
#' @param previous_files `fileInput()` value of last round's workbook(s), or
#'   `NULL` for the first round.
#' @param xlsform `fileInput()` value of the XLSForm, or `NULL` to use the copy
#'   in the last workbook.
#' @param checklist `fileInput()` value of a logical checklist, or `NULL`.
#' @param dir The session workspace.
#' @param general_override Settings typed in by the user, or `NULL`.
#' @return A list: `profile`, `general`, `previous`, `tool` (as from
#'   `load_tool()`), `xlsform_path`, `checklist_path`, `raw_data` (the new
#'   surveys, label row kept) and `counts` (`n_total`, `n_checked_before`,
#'   `n_new`).
#' @noRd
start_round <- function(
  download,
  previous_files = NULL,
  xlsform = NULL,
  checklist = NULL,
  dir,
  general_override = NULL
) {
  if (is.null(download) || nrow(download) == 0) {
    stop("Upload the new ONA download first.")
  }
  first_rows <- read_download_text(download$datapath[1])

  # what earlier rounds checked: their survey IDs, settings and questionnaire
  general0 <- detect_general(first_rows)
  previous <- read_previous(previous_files, general0)
  profile <- profile_from_previous(previous)
  general <- detect_general(first_rows, profile$general)
  if (!is.null(general_override)) {
    general <- utils::modifyList(general, general_override)
    general$missing <- setdiff(general$missing, names(general_override))
  }
  if ("uuid_column" %in% general$missing) {
    stop(
      "The download has no survey ID column (_uuid). Check it is the ONA ",
      "export, or type the column name under Advanced."
    )
  }
  if (!general$uuid_column %in% names(first_rows)) {
    stop("The download has no column called '", general$uuid_column, "'.")
  }
  profile$general <- utils::modifyList(
    profile$general,
    general[setdiff(names(general), "missing")]
  )

  xlsform_path <- questionnaire_path(xlsform, previous, dir)
  checklist_path <- place_checklist(checklist, previous, dir)
  tool <- load_tool(xlsform_path, language = profile$general$label_language)

  raw <- cleaningkit::read_raw_data(
    filename = download$datapath[1],
    tool_survey = tool$tool_survey,
    skip_label_row = profile$general$skip_label_row
  )
  kept <- cleaningkit::keep_new_records(
    raw,
    previous = previous,
    uuid_column = profile$general$uuid_column,
    skip_label_row = profile$general$skip_label_row
  )

  list(
    profile = profile,
    general = general,
    previous = previous,
    tool = tool,
    xlsform_path = xlsform_path,
    checklist_path = checklist_path,
    raw_data = kept$data,
    counts = kept[c("n_total", "n_checked_before", "n_new")]
  )
}

#' One sentence on what the round will check
#' @noRd
round_counts_text <- function(counts, previous = NULL) {
  if (is.null(previous)) {
    return(paste0(
      format(counts$n_total, big.mark = ","),
      " surveys in this download. This is the first round, so all of them ",
      "will be checked."
    ))
  }
  rounds <- unique(stats::na.omit(previous$checked$round_name))
  where <- if (length(rounds) == 0) {
    "in earlier rounds"
  } else if (length(rounds) <= 3) {
    paste0("in ", paste(rounds, collapse = ", "))
  } else {
    paste0("in ", length(rounds), " earlier rounds")
  }
  paste0(
    format(counts$n_total, big.mark = ","),
    " surveys in this download. ",
    format(counts$n_checked_before, big.mark = ","),
    " were checked ",
    where,
    ". ",
    format(counts$n_new, big.mark = ","),
    if (counts$n_new == 1) " is new." else " are new."
  )
}

#' Build the round memory for the review workbook
#'
#' @param round The result of `start_round()`.
#' @param round_name Free text from the user.
#' @noRd
make_round_info <- function(round, round_name) {
  g <- round$profile$general
  cleaningkit::ck_round_info(
    dataset = round$raw_data,
    round_name = round_name,
    previous = round$previous,
    uuid_column = g$uuid_column,
    skip_label_row = g$skip_label_row,
    tool_survey = sheet_as_text(round$xlsform_path, "survey"),
    tool_choices = sheet_as_text(round$xlsform_path, "choices"),
    logical_checklist = sheet_as_text(round$checklist_path),
    settings = list(
      cleaningkitapp_profile = yaml::as.yaml(normalise_profile(round$profile))
    )
  )
}

#' Default name for a round: today's date
#' @noRd
default_round_name <- function(date = Sys.Date()) {
  format(date, "%Y-%m-%d")
}

#' File name for the reviewers' workbook of a round
#' @noRd
round_review_file <- function(round_name, vba = TRUE) {
  review_file_name(paste0(round_name, "_follow-ups"), vba = vba)
}
