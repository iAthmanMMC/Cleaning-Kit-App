# The check registry ---------------------------------------------------------
#
# Every validate_*() check is described here once: what it is called, which
# arguments the user can change and how they are shown. The Checks screen is
# drawn from this list, so adding a check to the app means adding one entry.
#
# Arguments that come from the project rather than from the user (the data,
# the tool, the ID and enumerator columns, skip_label_row) are filled in by
# `check_context_args()`, so they are set once on the Project screen and never
# repeated per check.
#
# Defaults are read from the cleaningkit function itself (`arg_default()`), so
# the app cannot drift from the package; test-check_registry.R checks that
# every argument named here exists in the function.

# Argument specs ---------------------------------------------------------------

#' Describe one user-facing argument of a check
#'
#' @param type How the argument is shown and stored: `"number"`, `"toggle"`,
#'   `"text"`, `"texts"` (several free values), `"column"` (one data column),
#'   `"columns"` (several), `"choice"` (one of `choices`) or `"file"` (a path
#'   relative to the project).
#' @param label Label shown in the app.
#' @param help Optional one-line hint.
#' @param default Value used when the profile has none. `NULL` takes the
#'   default of the cleaningkit function.
#' @noRd
arg_spec <- function(
  type,
  label,
  help = NULL,
  default = NULL,
  choices = NULL,
  min = NA,
  max = NA,
  step = NA,
  required = FALSE
) {
  list(
    type = type,
    label = label,
    help = help,
    default = default,
    choices = choices,
    min = min,
    max = max,
    step = step,
    required = required
  )
}

#' The checks the app can run, in the order a reviewer thinks about them
#'
#' Each entry has:
#' - `label`, `description`: shown on the Checks screen.
#' - `fn`: the cleaningkit function.
#' - `kind`: `"log"` for a `validate_*()` check that returns a log, `"other"`
#'   for `prepare_other_responses()`, whose table goes to its own sheet.
#' - `needs`: what must be loaded first: `"tool_survey"`, `"tool_choices"`,
#'   `"internet"`.
#' - `enabled`: on by default?
#' - `args`: named list of `arg_spec()`; names are the function's arguments.
#' - `build`: optional `function(args, ctx)` that turns the user's values into
#'   the call's arguments (e.g. reads the logical checklist file).
#'
#' @noRd
ck_checks <- function() {
  list(
    duration = list(
      label = "Interview duration",
      description = "Flags interviews shorter than the minimum. Long interviews are only flagged when you ask for it.",
      fn = cleaningkit::validate_duration,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        column_to_check = arg_spec("column", "Duration column (seconds)"),
        lower_bound = arg_spec("number", "Minimum minutes", min = 0, step = 1),
        upper_bound = arg_spec("number", "Maximum minutes", min = 0, step = 1),
        flag_above_upper = arg_spec(
          "toggle",
          "Also flag interviews longer than the maximum"
        )
      )
    ),
    completeness = list(
      label = "Completeness",
      description = "Flags interviews with fewer answered questions than a complete interview has.",
      fn = cleaningkit::validate_completeness,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        min_content_cells = arg_spec(
          "number",
          "Minimum answered questions",
          help = "Roughly how many questions a complete interview answers.",
          min = 0,
          step = 1
        ),
        metadata_cols = arg_spec(
          "columns",
          "Metadata columns (not counted)"
        )
      )
    ),
    refused = list(
      label = "Refusals",
      description = "Flags interviews with more refusals than allowed.",
      fn = cleaningkit::validate_refused,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        max_refused = arg_spec("number", "Maximum refusals", min = 0, step = 1),
        refused_value = arg_spec("text", "Value used for \"Refused\"")
      )
    ),
    back_to_back = list(
      label = "Back-to-back interviews",
      description = "Flags interviews by the same enumerator that follow each other too closely.",
      fn = cleaningkit::validate_back_to_back,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        start_column = arg_spec("column", "Start time column"),
        end_column = arg_spec("column", "End time column"),
        threshold_hours = arg_spec(
          "number",
          "Minimum gap: hours",
          min = 0,
          step = 1
        ),
        threshold_mins = arg_spec(
          "number",
          "Minimum gap: minutes",
          min = 0,
          step = 1
        ),
        gap_from = arg_spec(
          "choice",
          "Measure the gap from the previous interview's",
          default = "end",
          choices = c("End" = "end", "Start" = "start")
        )
      )
    ),
    country_of_interview = list(
      label = "Country of interview",
      description = "Flags respondents interviewed in their country of nationality.",
      fn = cleaningkit::validate_country_of_interview,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        country_interview_col = arg_spec(
          "column",
          "Country of interview question"
        ),
        nationality_col = arg_spec("column", "Nationality question"),
        journey_start_col = arg_spec("column", "Journey start country question")
      )
    ),
    interview_time = list(
      label = "Time of interview",
      description = "Flags interviews that started outside plausible hours.",
      fn = cleaningkit::validate_interview_time,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        time_column = arg_spec("column", "Time column"),
        earliest_hour = arg_spec(
          "number",
          "Earliest hour (0-23)",
          min = 0,
          max = 23,
          step = 1
        ),
        latest_hour = arg_spec(
          "number",
          "Latest hour (0-23)",
          min = 0,
          max = 23,
          step = 1
        ),
        flag_missing = arg_spec("toggle", "Also flag interviews with no time")
      )
    ),
    outliers = list(
      label = "Outliers",
      description = "Flags unusual values in integer questions.",
      fn = cleaningkit::validate_outliers,
      kind = "log",
      needs = "tool_survey",
      enabled = TRUE,
      args = list(
        columns_to_check = arg_spec(
          "columns",
          "Questions to check",
          help = "Leave empty to check every integer question."
        ),
        columns_to_skip = arg_spec("columns", "Questions to skip"),
        strongness_factor = arg_spec(
          "number",
          "Strictness factor",
          help = "Higher flags fewer values.",
          min = 0,
          step = 0.5
        ),
        min_unique_values = arg_spec(
          "number",
          "Skip questions with fewer distinct values than",
          min = 1,
          step = 1
        )
      )
    ),
    interview_location = list(
      label = "Interview location (GPS)",
      description = "Checks each in-person interview's GPS point against the country and city recorded. Needs internet: each city is looked up on OpenStreetMap, about one per second.",
      fn = cleaningkit::validate_interview_location,
      kind = "log",
      needs = "internet",
      enabled = TRUE,
      args = list(
        lat_column = arg_spec("column", "Latitude column"),
        lon_column = arg_spec("column", "Longitude column"),
        location_column = arg_spec("column", "Raw geopoint column (fallback)"),
        country_question = arg_spec("column", "Country of interview question"),
        city_question = arg_spec("column", "City of interview question"),
        city_radius_km = arg_spec(
          "number",
          "City radius (km)",
          min = 0,
          step = 5
        ),
        border_tolerance_km = arg_spec(
          "number",
          "Border tolerance (km)",
          min = 0,
          step = 1
        ),
        check_country = arg_spec("toggle", "Check the country"),
        check_city = arg_spec("toggle", "Check the city"),
        flag_missing_gps = arg_spec("toggle", "Flag interviews with no GPS"),
        treat_blank_gps_as_phone = arg_spec(
          "toggle",
          "Treat interviews with no GPS at all as phone interviews"
        ),
        use_location_fallback = arg_spec(
          "toggle",
          "Use the raw geopoint when latitude/longitude are unusable"
        )
      )
    ),
    repeated_entries = list(
      label = "Repeated journey entries",
      description = "Flags countries, stops and dangerous locations entered twice in the journey loops.",
      fn = cleaningkit::validate_repeated_entries,
      kind = "log",
      needs = "tool_choices",
      enabled = TRUE,
      args = list(
        similarity = arg_spec(
          "number",
          "Text similarity (0-1)",
          help = "How close two typed \"Other\" answers must be to count as the same place.",
          min = 0,
          max = 1,
          step = 0.05
        )
      )
    ),
    logical = list(
      label = "Logical checks",
      description = "Runs every check in the logical checklist (one row per check).",
      fn = cleaningkit::validate_logical_with_list,
      kind = "log",
      needs = character(0),
      enabled = TRUE,
      args = list(
        list_of_check = arg_spec(
          "file",
          "Checklist file",
          help = "Path inside the project folder.",
          default = "resources/logical_checklist.xlsx"
        ),
        check_id_column = arg_spec(
          "text",
          "Check ID column",
          default = "check_id"
        ),
        check_to_perform_column = arg_spec(
          "text",
          "Check expression column",
          default = "check_to_perform"
        ),
        columns_to_clean_column = arg_spec(
          "text",
          "Columns to clean column",
          default = "columns_to_clean"
        ),
        description_column = arg_spec(
          "text",
          "Description column",
          default = "description"
        )
      ),
      build = function(args, ctx) {
        path <- project_file(ctx$project_dir, args$list_of_check)
        if (is.null(path) || !file.exists(path)) {
          stop("Logical checklist not found: ", args$list_of_check)
        }
        args$list_of_check <- openxlsx::read.xlsx(path, sheet = 1)
        args
      }
    ),
    spatial_proximity = list(
      label = "Surveys close together",
      description = "Flags surveys conducted within a short distance of each other. Not in the example script; off by default.",
      fn = cleaningkit::validate_spatial_proximity,
      kind = "log",
      needs = character(0),
      enabled = FALSE,
      args = list(
        lat_column = arg_spec("column", "Latitude column"),
        lon_column = arg_spec("column", "Longitude column"),
        distance_threshold_m = arg_spec(
          "number",
          "Distance (metres)",
          min = 0,
          step = 10
        )
      )
    ),
    similar_surveys = list(
      label = "Similar surveys (soft duplicates)",
      description = "Flags surveys almost identical to another one. Compares every pair, so it is slow on large rounds. Off by default.",
      fn = cleaningkit::validate_similar_surveys,
      kind = "log",
      needs = "tool_survey",
      enabled = FALSE,
      args = list(
        threshold = arg_spec(
          "number",
          "Flag when fewer columns differ than",
          min = 0,
          step = 1
        ),
        idnk_value = arg_spec("text", "Value used for \"Don't know\"")
      )
    ),
    similar_questions = list(
      label = "Same answers to chosen questions",
      description = "Flags surveys by the same enumerator with identical answers to the questions you choose. Off by default.",
      fn = cleaningkit::validate_similar_questions,
      kind = "log",
      needs = character(0),
      enabled = FALSE,
      args = list(
        questions_to_check = arg_spec(
          "columns",
          "Questions to compare",
          default = character(0),
          required = TRUE
        )
      )
    ),
    other_responses = list(
      label = "\"Other\" text responses",
      description = "Collects the typed \"Other, specify\" answers for review. They go to their own sheet of the review workbook.",
      fn = cleaningkit::prepare_other_responses,
      kind = "other",
      needs = c("tool_survey", "tool_choices", "other_db"),
      enabled = TRUE,
      args = list(
        questions = arg_spec(
          "columns",
          "Questions to include",
          help = "Leave empty for every \"other\" question.",
          default = character(0)
        ),
        extra_columns = arg_spec(
          "columns",
          "Extra columns shown next to each response",
          default = "username"
        )
      )
    )
  )
}

# Defaults -------------------------------------------------------------------

#' Default value of one argument of a check
#'
#' The spec's own default if it has one, otherwise the default of the
#' cleaningkit function, evaluated in the cleaningkit namespace (so defaults
#' such as `ck_repeated_entries_sections()` resolve). A function default that
#' is a vector of options (`gap_from = c("end", "start")`) is reduced to its
#' first element, the way `match.arg()` reads it.
#'
#' @noRd
arg_default <- function(check, name) {
  spec <- check$args[[name]]
  if (!is.null(spec$default)) {
    return(spec$default)
  }
  f <- formals(check$fn)
  if (!name %in% names(f)) {
    stop("'", name, "' is not an argument of this check's function.")
  }
  value <- eval(f[[name]], envir = asNamespace("cleaningkit"))
  if (identical(spec$type, "choice") && length(value) > 1) {
    value <- value[[1]]
  }
  coerce_arg(spec, value)
}

#' Default settings for every check: `list(<id> = list(enabled, args))`
#' @noRd
default_check_settings <- function(checks = ck_checks()) {
  lapply(checks, function(check) {
    list(
      enabled = check$enabled,
      args = stats::setNames(
        lapply(names(check$args), function(nm) arg_default(check, nm)),
        names(check$args)
      )
    )
  })
}

#' Put a stored value back in the type its spec expects
#'
#' Profile values come back from YAML or from Shiny inputs: numbers as
#' strings, empty vectors as `NULL` or `list()`.
#'
#' @noRd
coerce_arg <- function(spec, value) {
  if (is.list(value)) {
    value <- unlist(value)
  }
  switch(
    spec$type,
    number = {
      v <- suppressWarnings(as.numeric(value[1]))
      if (length(v) == 0) NA_real_ else v
    },
    toggle = isTRUE(as.logical(value[1])),
    texts = ,
    columns = if (is.null(value)) character(0) else as.character(value),
    # text, column, choice, file: a single string
    if (is.null(value) || length(value) == 0) "" else as.character(value[[1]])
  )
}

#' Merge saved check settings over the defaults
#'
#' A check added to the app after the profile was saved gets its defaults; a
#' setting for a check that no longer exists is dropped.
#'
#' @noRd
merge_check_settings <- function(saved, checks = ck_checks()) {
  out <- default_check_settings(checks)
  for (id in intersect(names(saved), names(checks))) {
    if (!is.null(saved[[id]]$enabled)) {
      out[[id]]$enabled <- isTRUE(as.logical(saved[[id]]$enabled))
    }
    for (nm in intersect(names(saved[[id]]$args), names(checks[[id]]$args))) {
      out[[id]]$args[[nm]] <- coerce_arg(
        checks[[id]]$args[[nm]],
        saved[[id]]$args[[nm]]
      )
    }
  }
  out
}

# Running --------------------------------------------------------------------

#' Context a check runs in
#'
#' @param raw_data,tool_survey,tool_choices,other_db From the store.
#' @param general The `general` section of the profile.
#' @param project_dir Project folder.
#' @noRd
check_context <- function(
  raw_data,
  tool_survey,
  tool_choices,
  general,
  project_dir,
  other_db = NULL
) {
  list(
    raw_data = raw_data,
    tool_survey = tool_survey,
    tool_choices = tool_choices,
    other_db = other_db,
    general = general,
    project_dir = project_dir
  )
}

#' Arguments filled in from the project rather than by the user
#'
#' Only those the function actually takes are returned.
#'
#' @noRd
check_context_args <- function(check, ctx) {
  g <- ctx$general
  candidates <- list(
    dataset = ctx$raw_data,
    raw_data = ctx$raw_data,
    uuid_column = g$uuid_column,
    enumerator_column = g$enumerator_column,
    skip_label_row = g$skip_label_row,
    tool_survey = ctx$tool_survey,
    tool_choices = ctx$tool_choices,
    other_db = ctx$other_db
  )
  candidates[intersect(names(candidates), names(formals(check$fn)))]
}

#' What is missing before a check can run
#'
#' @return Character vector of reasons; empty when the check can run.
#'   Internet access is not tested here: the check reports it if it fails.
#' @noRd
check_missing <- function(check, ctx) {
  out <- character(0)
  if (is.null(ctx$raw_data)) {
    out <- c(out, "the data (Data screen)")
  }
  if ("tool_survey" %in% check$needs && is.null(ctx$tool_survey)) {
    out <- c(out, "the XLSForm (Tool screen)")
  }
  tool_needs <- c("tool_survey", "tool_choices", "other_db")
  tool_missing <- vapply(
    intersect(check$needs, tool_needs),
    function(slot) is.null(ctx[[slot]]),
    logical(1)
  )
  if (any(tool_missing)) {
    out <- c(out, "the XLSForm (Tool screen)")
  }
  out
}

#' Run one check
#'
#' @param check A registry entry.
#' @param args The user's values for `check$args`.
#' @param ctx From `check_context()`.
#' @return For a `"log"` check, the log data frame; for `"other"`, the
#'   other-responses table.
#' @noRd
run_check <- function(check, args, ctx) {
  missing <- check_missing(check, ctx)
  if (length(missing)) {
    stop("Load ", paste(missing, collapse = " and "), " first.")
  }

  args <- args[names(check$args)]
  for (nm in names(args)) {
    spec <- check$args[[nm]]
    value <- coerce_arg(spec, args[[nm]])
    if (spec$type == "number" && is.na(value)) {
      stop("'", spec$label, "' must be a number.")
    }
    # an empty multi-value setting means "use the function's default"
    # (e.g. columns_to_check = NULL checks every integer question)
    if (spec$type %in% c("columns", "texts") && length(value) == 0) {
      value <- NULL
    }
    if (isTRUE(spec$required) && (is.null(value) || identical(value, ""))) {
      stop("Fill in '", spec$label, "' first.")
    }
    args[nm] <- list(value)
  }
  if (!is.null(check$build)) {
    args <- check$build(args, ctx)
  }

  call_args <- c(check_context_args(check, ctx), args)
  call_args <- call_args[!vapply(call_args, is.null, logical(1))]
  result <- do.call(check$fn, call_args)

  if (check$kind == "other") {
    return(result)
  }
  log_name <- eval(formals(check$fn)$log_name)
  log <- result[[log_name]]
  if (!is.data.frame(log)) {
    stop("The check returned no log called '", log_name, "'.")
  }
  log
}

#' Log name a check's rows are stored under, e.g. `"duration_log"`
#' @noRd
check_log_name <- function(check) {
  eval(formals(check$fn)$log_name)
}
