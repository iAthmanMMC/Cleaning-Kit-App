# The "Apply corrections" flow: the reviewers' workbooks and an ONA download
# in, the final workbook out.
#
# The workbooks carry the round's memory (see cleaningkit::ck_round_info()):
# which surveys were in the round, what their answers were when checked, the
# questionnaire and the settings. So the ONA download can be the same one the
# round was checked from or any later one: the round's surveys are picked out
# of it with cleaningkit::round_records(), and surveys edited or deleted on ONA
# since are reported.

#' Settings for reading uploaded workbooks
#'
#' Reviewers can save under any name, so every Excel file in the upload folder
#' is read.
#' @noRd
upload_clean_settings <- function(clean) {
  clean$file_pattern <- "\\.xls[xm]$"
  clean
}

#' Copy the uploaded workbooks into one folder for the readers
#'
#' @param uploads A `fileInput()` value.
#' @param dir The session workspace.
#' @return The folder.
#' @noRd
place_reviewed <- function(uploads, dir) {
  folder <- file.path(dir, "reviewed")
  unlink(folder, recursive = TRUE)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  names <- gsub("[<>:\"/\\\\|?*]", "_", basename(uploads$name))
  names <- make.unique(names, sep = "_")
  # make.unique puts the number after the extension; move it before
  names <- sub("^(.*)(\\.xls[xm])_([0-9]+)$", "\\1_\\3\\2", names)
  file.copy(uploads$datapath, file.path(folder, names), overwrite = TRUE)
  folder
}

#' Read the round memory from the reviewers' workbooks
#'
#' Each workbook must carry round memory, and all must come from the same
#' round: copies split between reviewers are fine, two different rounds are
#' not.
#'
#' @param uploads A `fileInput()` value.
#' @return A `ck_round_info`.
#' @noRd
read_reviewed_round <- function(uploads, uuid_column = "_uuid") {
  if (is.null(uploads) || nrow(uploads) == 0) {
    stop("Upload the workbook the reviewers filled in.")
  }
  bad_type <- !grepl("\\.xls[xm]$", uploads$name, ignore.case = TRUE)
  if (any(bad_type)) {
    stop(
      "Upload the reviewers' Excel workbooks (.xlsx or .xlsm). Not: ",
      paste(uploads$name[bad_type], collapse = ", "),
      "."
    )
  }
  rounds <- lapply(seq_len(nrow(uploads)), function(i) {
    info <- tryCatch(
      cleaningkit::read_round_info(
        uploads$datapath[i],
        uuid_column = uuid_column,
        names = uploads$name[i]
      ),
      error = function(e) NULL
    )
    if (is.null(info) || is.null(info$meta$round_id)) {
      stop(
        uploads$name[i],
        " does not say which surveys it covers. Upload the workbook made by ",
        "\"Check new data\" (reviewers can rename it, but not copy its ",
        "sheets into a new file)."
      )
    }
    info
  })
  ids <- vapply(rounds, function(x) x$meta$round_id, "")
  if (length(unique(ids)) > 1) {
    names <- vapply(rounds, function(x) x$meta$round_name, "")
    stop(
      "These workbooks come from different rounds (",
      paste(unique(names), collapse = ", "),
      "). Apply the corrections of one round at a time."
    )
  }
  cleaningkit::read_round_info(
    uploads$datapath,
    uuid_column = uuid_column,
    names = uploads$name
  )
}

#' Read everything the corrections need
#'
#' @param workbooks `fileInput()` value of the reviewers' workbook(s).
#' @param download `fileInput()` value of an ONA download that holds the
#'   round's surveys.
#' @param xlsform `fileInput()` value of the XLSForm, or `NULL` to use the
#'   copy in the workbook.
#' @param dir The session workspace.
#' @return A list: `info` (the round memory), `profile`, `tool`, `raw_data`
#'   (the round's surveys from the download, label row kept), `records` (from
#'   `round_records()`), `folder` and `reviewed` (from `read_reviewed()`).
#' @noRd
start_apply <- function(workbooks, download, xlsform = NULL, dir) {
  if (is.null(download) || nrow(download) == 0) {
    stop(
      "Upload the data downloaded from ONA: the one the round was checked ",
      "from, or a later one."
    )
  }
  info <- read_reviewed_round(workbooks)
  profile <- profile_from_previous(info)

  general <- detect_general(
    read_download_text(download$datapath[1]),
    profile$general
  )
  if ("uuid_column" %in% general$missing) {
    stop(
      "The download has no survey ID column (_uuid). Check it is the ONA ",
      "export of the same survey."
    )
  }
  profile$general <- utils::modifyList(
    profile$general,
    general[setdiff(names(general), "missing")]
  )
  g <- profile$general

  xlsform_path <- questionnaire_path(xlsform, info, dir)
  tool <- load_tool(xlsform_path, language = g$label_language)
  raw <- cleaningkit::read_raw_data(
    filename = download$datapath[1],
    tool_survey = tool$tool_survey,
    skip_label_row = g$skip_label_row
  )
  records <- cleaningkit::round_records(
    raw,
    info,
    uuid_column = g$uuid_column,
    skip_label_row = g$skip_label_row
  )
  if (n_records(records$data, g$skip_label_row) == 0) {
    stop(
      "None of the ",
      records$n_round,
      " surveys of round \"",
      info$meta$round_name,
      "\" are in this download. Upload the ONA download of the same survey."
    )
  }

  folder <- place_reviewed(workbooks, dir)
  reviewed <- read_reviewed(
    project_dir = dir,
    raw_data = records$data,
    clean = upload_clean_settings(profile$clean),
    general = g,
    folder = folder
  )

  list(
    info = info,
    profile = profile,
    tool = tool,
    raw_data = records$data,
    records = records,
    folder = folder,
    reviewed = reviewed
  )
}

#' What the download holds of the round, in plain words
#'
#' @return A list with `text` (one sentence) and `warnings` (sentences about
#'   surveys deleted or edited on ONA since the round was checked).
#' @noRd
round_records_text <- function(records, info) {
  n_found <- records$n_round - length(records$missing)
  text <- paste0(
    "Round \"",
    info$meta$round_name,
    "\" (checked ",
    info$meta$checked_on,
    ") had ",
    records$n_round,
    " surveys; ",
    if (n_found == records$n_round) {
      "all of them are in this download."
    } else {
      paste0(n_found, " of them are in this download.")
    }
  )
  warnings <- character(0)
  if (length(records$missing)) {
    warnings <- c(
      warnings,
      paste0(
        length(records$missing),
        " survey(s) are no longer in the download, probably deleted on ONA: ",
        paste(utils::head(records$missing, 5), collapse = ", "),
        if (length(records$missing) > 5) ", ..." else "",
        ". Their corrections are skipped."
      )
    )
  }
  if (length(records$changed)) {
    warnings <- c(
      warnings,
      paste0(
        length(records$changed),
        " survey(s) were edited on ONA after they were checked: ",
        paste(utils::head(records$changed, 5), collapse = ", "),
        if (length(records$changed) > 5) ", ..." else "",
        ". The reviewers saw the old answers, so check their corrections ",
        "still make sense."
      )
    )
  }
  list(text = text, warnings = warnings)
}

#' Apply the corrections and compare the result
#'
#' @param state From `start_apply()`.
#' @return A list with `clean_result` (from `apply_reviewed()`) and `check`
#'   (from `split_check_rows()`).
#' @noRd
finish_apply <- function(state, dir) {
  g <- state$profile$general
  clean <- upload_clean_settings(state$profile$clean)
  result <- apply_reviewed(
    project_dir = dir,
    raw_data = state$raw_data,
    cleaning_log = state$reviewed$cleaning_log,
    clean = clean,
    general = g,
    tool_choices = state$tool$tool_choices,
    other_db = state$tool$other_db,
    folder = state$folder
  )
  check <- check_clean_data(state$raw_data, result, clean, g)
  list(clean_result = result, check = split_check_rows(check))
}

#' File name for the final workbook of a round
#' @noRd
round_final_file <- function(round_name) {
  final_file_name(paste0(round_name, "_4Mi_final_output"))
}
