# The Clean half of the workflow: example script 2 as plain functions, so the
# screens stay thin and the steps can be tested without Shiny.

#' Evaluation problems that stop the log from being applied
#'
#' `apply_cleaning_log()` refuses an unknown action, and a row with no action
#' or no new value cannot be applied as the reviewer meant it. Every other
#' issue type from `evaluate_cleaning_log()` is a warning: worth a look, but
#' the log can be applied.
#'
#' @noRd
.blocking_issues <- c("missing_action", "invalid_action", "missing_new_value")

#' Reviewed workbooks in output/follow_ups
#'
#' @param pattern The `file_pattern` the readers use.
#' @return A data frame with `file` and `modified`, newest first.
#' @noRd
reviewed_files <- function(project_dir, pattern) {
  dir <- file.path(project_dir, "output", "follow_ups")
  files <- list.files(dir, pattern = pattern, ignore.case = TRUE)
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

#' Check, read and evaluate the reviewed cleaning logs
#'
#' `check_log_files()` on both sheets, then `read_cleaning_log()` and
#' `evaluate_cleaning_log()`, as in example script 2.
#'
#' @param project_dir Project folder; the workbooks are read from
#'   `output/follow_ups/`.
#' @param raw_data The data that was validated.
#' @param clean The `clean` section of the profile.
#' @param general The `general` section of the profile.
#' @return A list with `file_checks` (a list with one `check_log_files()`
#'   table per sheet), `cleaning_log` (as evaluated, with an
#'   `evaluation_issue` column) and `evaluation_log`.
#' @noRd
read_reviewed <- function(project_dir, raw_data, clean, general) {
  if (is.null(raw_data)) {
    stop("Load the data that was validated first.")
  }
  folder <- file.path(project_dir, "output", "follow_ups")
  if (nrow(reviewed_files(project_dir, clean$file_pattern)) == 0) {
    stop(
      "No reviewed workbook in output/follow_ups/. Reviewers save the edited ",
      "copy with a name ending in _edited (e.g. 2026-11-20_follow-ups_edited.xlsm)."
    )
  }

  file_checks <- list(
    cleaning_log = cleaningkit::check_log_files(
      folder,
      file_pattern = clean$file_pattern,
      sheet = "cleaning_log",
      verbose = FALSE
    ),
    other_responses = tryCatch(
      cleaningkit::check_log_files(
        folder,
        file_pattern = clean$file_pattern,
        sheet = "other_responses",
        verbose = FALSE
      ),
      error = function(e) NULL
    )
  )
  bad <- file_checks$cleaning_log[
    !file_checks$cleaning_log$matches_reference |
      file_checks$cleaning_log$status != "ok",
    ,
    drop = FALSE
  ]
  if (nrow(bad) > 0) {
    stop(
      "These workbooks do not have the same cleaning_log columns as the others: ",
      paste(bad$file, collapse = ", "),
      ". See the File check tab."
    )
  }

  cleaning_log <- cleaningkit::read_cleaning_log(
    path = folder,
    raw_dataset = raw_data,
    raw_uuid_column = general$uuid_column,
    uuid_col = clean$log_uuid_col,
    action_col = clean$log_action_col,
    question_col = clean$log_question_col,
    old_value_col = clean$log_old_value_col,
    new_value_col = clean$log_new_value_col,
    file_pattern = clean$file_pattern,
    default_blank_action = clean$default_blank_action,
    skip_label_row = general$skip_label_row,
    verbose = TRUE
  )

  evaluation <- cleaningkit::evaluate_cleaning_log(
    cleaning_log = cleaning_log,
    raw_dataset = raw_data,
    uuid_col = clean$log_uuid_col,
    action_col = clean$log_action_col,
    question_col = clean$log_question_col,
    old_value_col = clean$log_old_value_col,
    new_value_col = clean$log_new_value_col,
    raw_uuid_column = general$uuid_column,
    valid_actions = clean$valid_actions,
    default_blank_action = clean$default_blank_action,
    skip_label_row = general$skip_label_row,
    flag_issues_inline = TRUE
  )

  list(
    file_checks = file_checks,
    cleaning_log = evaluation$cleaning_log,
    evaluation_log = evaluation$evaluation_log
  )
}

#' Count blocking problems and warnings in an evaluation log
#' @noRd
evaluation_counts <- function(evaluation_log) {
  if (is.null(evaluation_log) || nrow(evaluation_log) == 0) {
    return(list(blocking = 0L, warnings = 0L))
  }
  blocking <- evaluation_log$issue_type %in% .blocking_issues
  list(blocking = sum(blocking), warnings = sum(!blocking))
}

#' Rows per action taken in a reviewed log
#' @noRd
action_counts <- function(cleaning_log, action_col = "Action taken") {
  if (is.null(cleaning_log) || !action_col %in% names(cleaning_log)) {
    return(NULL)
  }
  a <- as.character(cleaning_log[[action_col]])
  a[is.na(a) | !nzchar(a)] <- "(blank)"
  counts <- sort(table(a), decreasing = TRUE)
  data.frame(
    action = names(counts),
    rows = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

#' Write the evaluated log to a workbook reviewers can correct
#'
#' The log as read, with the `evaluation_issue` column beside the rows that
#' need fixing, so it can be sent back to the reviewers.
#'
#' @noRd
write_annotated_log <- function(cleaning_log, evaluation_log, path) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "cleaning_log")
  openxlsx::writeDataTable(wb, "cleaning_log", as.data.frame(cleaning_log))
  if ("evaluation_issue" %in% names(cleaning_log)) {
    flagged <- which(
      !is.na(cleaning_log$evaluation_issue) &
        nzchar(cleaning_log$evaluation_issue)
    )
    if (length(flagged)) {
      openxlsx::addStyle(
        wb,
        "cleaning_log",
        openxlsx::createStyle(fgFill = "#F8D7DA"),
        rows = flagged + 1,
        cols = seq_len(ncol(cleaning_log)),
        gridExpand = TRUE
      )
    }
  }
  openxlsx::addWorksheet(wb, "problems")
  openxlsx::writeDataTable(wb, "problems", as.data.frame(evaluation_log))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Apply the reviewed logs to the data
#'
#' `apply_cleaning_log()`, then the other responses
#' (`read_other_responses()` and `apply_other_responses()`), then
#' `combine_reviewed_logs()`, as in example script 2.
#'
#' @param raw_data The data that was validated.
#' @param cleaning_log The evaluated log from `read_reviewed()`.
#' @param tool_choices,other_db From the Tool screen; needed for the other
#'   responses.
#' @return A list with `clean_dataset`, `audit_log` (changes from the cleaning
#'   log), `other_log`, `other_audit_log` (changes from the other responses),
#'   `final_log` (the combined log for the final workbook) and `n_before`,
#'   `n_after` (records).
#' @noRd
apply_reviewed <- function(
  project_dir,
  raw_data,
  cleaning_log,
  clean,
  general,
  tool_choices = NULL,
  other_db = NULL
) {
  applied <- cleaningkit::apply_cleaning_log(
    raw_dataset = raw_data,
    cleaning_log = cleaning_log,
    raw_uuid_column = general$uuid_column,
    log_uuid_col = clean$log_uuid_col,
    log_question_col = clean$log_question_col,
    log_new_value_col = clean$log_new_value_col,
    log_action_col = clean$log_action_col,
    change_response_values = clean$change_response_values,
    blank_response_values = clean$blank_response_values,
    remove_survey_values = clean$remove_survey_values,
    no_action_values = clean$no_action_values,
    default_blank_action = clean$default_blank_action,
    apply_repeated_entries = isTRUE(clean$apply_repeated_entries),
    skip_label_row = general$skip_label_row,
    restore_types = isTRUE(clean$restore_types),
    verbose = TRUE
  )
  clean_dataset <- applied$clean_dataset

  other_log <- NULL
  other_audit <- NULL
  if (isTRUE(clean$apply_other)) {
    if (is.null(other_db) || is.null(tool_choices)) {
      stop("Load the XLSForm on the Tool screen: the other responses need it.")
    }
    other_log <- cleaningkit::read_other_responses(
      path = file.path(project_dir, "output", "follow_ups"),
      dataset = clean_dataset,
      other_db = other_db,
      tool_choices = tool_choices,
      uuid_column = general$uuid_column,
      log_uuid_col = clean$other_uuid_col,
      file_pattern = clean$file_pattern,
      skip_label_row = general$skip_label_row,
      verbose = TRUE
    )
    other_applied <- cleaningkit::apply_other_responses(
      clean_dataset,
      other_log,
      uuid_column = general$uuid_column,
      skip_label_row = general$skip_label_row
    )
    clean_dataset <- other_applied$dataset
    other_audit <- other_applied$audit_log
  }

  final_log <- cleaningkit::combine_reviewed_logs(
    main_log = applied$cleaning_log,
    other_log = other_log,
    dataset = raw_data,
    uuid_column = general$uuid_column,
    enumerator_column = general$enumerator_column,
    date_column = general$date_column,
    issue_labels = c(
      true_other = clean$label_true_other,
      recode = clean$label_recode,
      remove = clean$label_remove
    ),
    skip_label_row = general$skip_label_row
  )

  list(
    clean_dataset = clean_dataset,
    audit_log = applied$audit_log,
    other_log = other_log,
    other_audit_log = other_audit,
    final_log = final_log,
    n_before = n_records(raw_data, general$skip_label_row),
    n_after = n_records(clean_dataset, general$skip_label_row)
  )
}

#' Compare the clean data with the raw data and the final log
#'
#' `review_cleaned_data()` as in example script 2: every difference between
#' raw and clean that the log does not explain, and every log entry the clean
#' data does not reflect.
#'
#' @return The `review_cleaned_data()` table; zero rows means they agree.
#' @noRd
check_clean_data <- function(raw_data, clean_result, clean, general) {
  cleaningkit::review_cleaned_data(
    raw_dataset = raw_data,
    clean_dataset = clean_result$clean_dataset,
    cleaning_log = clean_result$final_log,
    raw_uuid_column = general$uuid_column,
    clean_uuid_column = general$uuid_column,
    log_uuid_col = clean$log_uuid_col,
    log_question_col = clean$log_question_col,
    log_new_value_col = clean$log_new_value_col,
    log_old_value_col = clean$log_old_value_col,
    log_action_col = clean$log_action_col,
    # the other-response actions are changes too
    change_response_values = union(
      clean$change_response_values,
      c("recode", "remove", "true_other")
    ),
    blank_response_values = clean$blank_response_values,
    discard_values = clean$remove_survey_values,
    no_action_values = clean$no_action_values,
    columns_to_skip = clean$columns_to_skip,
    default_blank_action = clean$default_blank_action,
    skip_label_row = general$skip_label_row,
    verbose = TRUE
  )
}

#' Split the comparison into rows tied to a survey and rows without a uuid
#'
#' `review_cleaned_data()` can report an `undocumented_change` with an empty
#' uuid after the "other" responses are applied, even when the clean data
#' matches the final log. Those rows are shown, but not counted as a
#' disagreement.
#'
#' @return A list with `tied` and `untied` data frames.
#' @noRd
split_check_rows <- function(check) {
  if (is.null(check) || nrow(check) == 0 || !"uuid" %in% names(check)) {
    empty <- if (is.null(check)) data.frame() else check[0, , drop = FALSE]
    return(list(tied = if (is.null(check)) empty else check, untied = empty))
  }
  untied <- is.na(check$uuid) | !nzchar(as.character(check$uuid))
  list(
    tied = check[!untied, , drop = FALSE],
    untied = check[untied, , drop = FALSE]
  )
}

#' Write the final workbook
#'
#' `export_final_output()` with the readme fields from the Final output
#' screen.
#'
#' @noRd
export_final <- function(raw_data, clean_result, final, path) {
  cleaningkit::export_final_output(
    raw_dataset = raw_data,
    clean_dataset = clean_result$clean_dataset,
    combined_log = clean_result$final_log,
    output_path = path,
    project_name = final$project_name,
    data_collection_round = final$data_collection_round,
    prepared_by = final$prepared_by,
    project_description = final$project_description,
    overwrite = TRUE
  )
  if (!file.exists(path)) {
    stop("The final workbook was not written to ", path)
  }
  invisible(path)
}

#' File name for the final workbook
#' @noRd
final_file_name <- function(stem) {
  sub("\\.xlsm$", ".xlsx", review_file_name(stem, vba = FALSE))
}
