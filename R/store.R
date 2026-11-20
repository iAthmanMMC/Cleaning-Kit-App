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
#' | `check_results`, `logs`, `other_responses`, `combined_log` | Checks |
#' | `review_path` | Review workbook |
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
    # per check id: list(status = "done"/"error", n, error, time)
    check_results = list(),
    # per log name (e.g. "duration_log"): the log data frame
    logs = list(),
    other_responses = NULL,
    combined_log = NULL,
    review_path = NULL,
    log = empty_log()
  )
}

#' Slots each workflow step produces, in workflow order
#' @noRd
.step_slots <- list(
  tool = c(
    "tool_survey",
    "tool_choices",
    "other_labels",
    "other_db",
    "constraint_issues"
  ),
  data = c("raw_data", "raw_data_path", "filter_summary"),
  checks = c("check_results", "logs", "other_responses", "combined_log"),
  review = "review_path"
)

#' Clear what a step and every later step produced
#'
#' When the tool or the data change, the check results built on the old ones
#' no longer apply, so they are cleared rather than left looking current.
#'
#' @param from First step to clear: `"tool"`, `"data"`, `"checks"` or
#'   `"review"`.
#' @noRd
reset_from <- function(store, from) {
  steps <- names(.step_slots)
  for (step in steps[match(from, steps):length(steps)]) {
    for (slot in .step_slots[[step]]) {
      store[[slot]] <- if (slot %in% c("check_results", "logs")) {
        list()
      } else {
        NULL
      }
    }
  }
  invisible(store)
}

#' Clear everything that was derived from a project
#'
#' Called when a different project is opened, so the tool and data of the
#' previous project cannot leak into the new one.
#'
#' @noRd
reset_project_state <- function(store) {
  reset_from(store, "tool")
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
