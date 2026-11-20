#' Languages an XLSForm has labels for
#'
#' Reads only the header of the survey sheet and returns the language part of
#' each `label::<language>` column, without the `(code)` suffix: cleaningkit
#' matches `label_column` as a regular expression, so `"English (en)"` would
#' not match `label::English (en)`, while `"English"` does.
#'
#' @param path Path to the XLSForm.
#' @param sheet Survey sheet name.
#' @return Character vector, possibly empty.
#' @noRd
tool_languages <- function(path, sheet = "survey") {
  headers <- tryCatch(
    names(readxl::read_excel(path, sheet = sheet, n_max = 0)),
    error = function(e) character(0)
  )
  label_cols <- grep("^label\\s*::", headers, value = TRUE, ignore.case = TRUE)
  langs <- sub("^label\\s*::\\s*", "", label_cols, ignore.case = TRUE)
  langs <- trimws(sub("\\s*\\(.*\\)\\s*$", "", langs))
  unique(langs[nzchar(langs)])
}

#' Sheets in an Excel file, or an empty vector if it cannot be read
#' @noRd
excel_sheets_safe <- function(path) {
  tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
}

#' Read the XLSForm and build everything derived from it
#'
#' The tool block of example script 1 in one call: survey and choices sheets,
#' the "other" text questions and the other-responses database, plus the
#' constraint check (`check_constraints()`), which only reports and never
#' stops the tool from loading.
#'
#' Must be run with the project folder as working directory, because
#' `get_other_labels()` saves `resources/labels_questions_others.xlsx`
#' relative to it. `run_step()` takes care of that.
#'
#' @param path Path to the XLSForm.
#' @param survey_sheet,choices_sheet Sheet names.
#' @param language Label language, e.g. `"English"`.
#' @param other_text_types Extra "other" text questions the tool did not
#'   flag, passed to `get_other_labels()`.
#' @return A list with `tool_survey`, `tool_choices`, `other_labels`,
#'   `other_db` and `constraint_issues` (a character vector; empty when every
#'   constraint is valid, `NA` when the check could not run).
#' @noRd
load_tool <- function(
  path,
  survey_sheet = "survey",
  choices_sheet = "choices",
  language = "English",
  other_text_types = character(0)
) {
  if (!file.exists(path)) {
    stop("XLSForm not found: ", path)
  }
  sheets <- readxl::excel_sheets(path)
  for (sh in c(survey_sheet, choices_sheet)) {
    if (!sh %in% sheets) {
      stop(
        "The XLSForm has no sheet called '",
        sh,
        "'. Sheets found: ",
        paste(sheets, collapse = ", ")
      )
    }
  }

  tool_survey <- cleaningkit::read_tool_survey(
    path,
    sheet_name = survey_sheet,
    label_column = language
  )
  tool_choices <- cleaningkit::read_tool_choices(
    path,
    sheet_name = choices_sheet,
    label_column = language
  )
  other_labels <- cleaningkit::get_other_labels(
    tool_survey = tool_survey,
    preferred_language = language,
    other_text_types = if (length(other_text_types)) other_text_types else NULL
  )
  other_db <- cleaningkit::get_other_db(
    tool_survey = tool_survey,
    tool_choices = tool_choices,
    other_labels = other_labels,
    preferred_language = language
  )

  constraint_issues <- tryCatch(
    as.character(cleaningkit::check_constraints(tool_survey, tool_choices)),
    error = function(e) {
      warning("Constraint check could not run: ", conditionMessage(e))
      NA_character_
    }
  )

  list(
    tool_survey = tool_survey,
    tool_choices = tool_choices,
    other_labels = other_labels,
    other_db = other_db,
    constraint_issues = constraint_issues
  )
}

#' Names of the text questions in a tool, for the "other" picker
#' @noRd
text_questions <- function(tool_survey) {
  if (is.null(tool_survey)) {
    return(character(0))
  }
  tool_survey$name[tool_survey$q_type %in% "text"]
}
