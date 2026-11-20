#' Run a cleaningkit call and capture what it reports
#'
#' cleaningkit reports progress through `message()`, `warning()` and
#' `cat()`. In a script those land in the console; in the app they would be
#' lost. This runs `fun` and returns its value together with every message,
#' warning, printed line and error as rows of a log.
#'
#' `fun` runs with the working directory set to `dir`, because some
#' cleaningkit functions write relative to it (for example
#' `get_other_labels()` saves `resources/labels_questions_others.xlsx`). In the
#' scripts the working directory is the project folder, so the app does the
#' same.
#'
#' @param fun A function with no arguments.
#' @param step Short label for the log, e.g. `"Read tool"`.
#' @param dir Directory to run in. `NULL` leaves the working directory alone.
#'
#' @return A list with `value` (the result, or `NULL` on error), `log` (a
#'   data frame with `time`, `step`, `level` and `text`) and `error` (the
#'   error message, or `NULL`).
#' @noRd
capture_run <- function(fun, step, dir = NULL) {
  entries <- list()
  add <- function(level, text) {
    text <- strip_ansi(trimws(text))
    text <- text[nzchar(text)]
    if (length(text) == 0) {
      return(invisible())
    }
    entries[[length(entries) + 1]] <<- data.frame(
      time = format(Sys.time(), "%H:%M:%S"),
      step = step,
      level = level,
      text = text,
      stringsAsFactors = FALSE
    )
  }

  error <- NULL
  value <- NULL
  run <- function() {
    withCallingHandlers(
      fun(),
      message = function(m) {
        add("info", conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        add("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  printed <- utils::capture.output(
    value <- tryCatch(
      if (is.null(dir)) run() else withr::with_dir(dir, run()),
      error = function(e) {
        error <<- conditionMessage(e)
        NULL
      }
    ),
    type = "output"
  )
  add("info", printed)
  if (!is.null(error)) {
    add("error", error)
  }

  log <- if (length(entries) == 0) empty_log() else do.call(rbind, entries)
  list(value = value, log = log, error = error)
}

#' @noRd
empty_log <- function() {
  data.frame(
    time = character(0),
    step = character(0),
    level = character(0),
    text = character(0),
    stringsAsFactors = FALSE
  )
}

#' Remove terminal colour codes (crayon) from text
#' @noRd
strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x)
}

#' Run a step inside the app
#'
#' Wraps `capture_run()` for use in a server function: shows a progress
#' notice, appends the log to the session's run log, and pops up a
#' notification for errors and warnings so they are not missed.
#'
#' @param store The session store from `new_store()`.
#' @inheritParams capture_run
#' @param notify Pop up errors and warnings. Turn off when the caller runs
#'   several steps and reports them together.
#' @param full Return the whole `capture_run()` result instead of the value,
#'   for callers that need the error message.
#' @return The value of `fun`, or `NULL` if it failed; the full result when
#'   `full = TRUE`.
#' @noRd
run_step <- function(
  store,
  fun,
  step,
  dir = NULL,
  notify = TRUE,
  full = FALSE
) {
  res <- shiny::withProgress(
    message = paste0(step, "..."),
    value = 0.3,
    capture_run(fun, step = step, dir = dir)
  )
  store$log <- rbind(store$log, res$log)

  n_warn <- sum(res$log$level == "warning")
  if (!notify) {
    # reported by the caller
  } else if (!is.null(res$error)) {
    shiny::showNotification(
      paste0(step, " failed: ", res$error),
      type = "error",
      duration = NULL
    )
  } else if (n_warn > 0) {
    shiny::showNotification(
      paste0(
        step,
        " finished with ",
        n_warn,
        " warning(s). See the Run log tab."
      ),
      type = "warning",
      duration = 8
    )
  }
  if (full) res else res$value
}
