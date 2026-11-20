#' Create the session store
#'
#' One `reactiveValues` object shared by every module. Each module reads what
#' it needs and writes only its own slots, so the order of the workflow is
#' visible in one place:
#'
#' | Slot | Written by |
#' |---|---|
#' | `project_dir`, `profile` | Project |
#' | `tool_survey`, `tool_choices`, `other_labels`, `other_db`, `constraint_issues` | Tool |
#' | `raw_data`, `raw_data_path`, `filter_summary` | Data |
#' | `log` | every step, through `run_step()` |
#'
#' @noRd
new_store <- function() {
  shiny::reactiveValues(
    project_dir = NULL,
    profile = default_profile(),
    tool_survey = NULL,
    tool_choices = NULL,
    other_labels = NULL,
    other_db = NULL,
    constraint_issues = NULL,
    raw_data = NULL,
    raw_data_path = NULL,
    filter_summary = NULL,
    log = empty_log()
  )
}

#' Clear everything that was derived from a project
#'
#' Called when a different project is opened, so the tool and data of the
#' previous project cannot leak into the new one.
#'
#' @noRd
reset_project_state <- function(store) {
  for (slot in c(
    "tool_survey",
    "tool_choices",
    "other_labels",
    "other_db",
    "constraint_issues",
    "raw_data",
    "raw_data_path",
    "filter_summary"
  )) {
    store[[slot]] <- NULL
  }
  invisible(store)
}

#' Rows of a data set, not counting the ONA label row
#' @noRd
n_records <- function(df, skip_label_row = TRUE) {
  if (is.null(df)) {
    return(0L)
  }
  n <- nrow(df)
  if (isTRUE(skip_label_row) && n > 0) n - 1L else n
}
