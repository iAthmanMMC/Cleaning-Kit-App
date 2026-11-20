#' Default project profile
#'
#' The profile holds every setting a data officer would otherwise edit in the
#' `CUSTOMISE` blocks of cleaningkit's example scripts. It is saved as YAML in
#' the project's `resources/` folder, so the next round, or a colleague
#' opening the same project, starts with the same settings.
#'
#' Paths are stored relative to the project folder, so a project can be moved
#' or synced (OneDrive) without breaking.
#'
#' @return A named list.
#' @noRd
default_profile <- function() {
  list(
    profile_version = 1L,
    general = list(
      uuid_column = "_uuid",
      enumerator_column = "username",
      date_column = "today",
      label_language = "English",
      skip_label_row = TRUE
    ),
    tool = list(
      path = "resources/tool.xlsx",
      survey_sheet = "survey",
      choices_sheet = "choices",
      other_text_types = character(0)
    ),
    data = list(
      filter_new = TRUE,
      use_ledger = TRUE,
      archive = TRUE,
      output_name = "data.xlsx",
      sheet = 1L,
      extra_date_cols = character(0),
      extra_datetime_cols = character(0)
    ),
    # one entry per check in ck_checks(): list(enabled, args)
    checks = default_check_settings(),
    review = list(
      vba = TRUE,
      include_dataset = TRUE,
      flag_columns = TRUE,
      include_other = TRUE,
      color_mode = "partial",
      color_columns = "old_value",
      group_by = "issue",
      file_suffix = "_follow-ups"
    )
  )
}

#' Settings that hold several values, even when their default has one
#' @noRd
.vector_settings <- c(
  "other_text_types",
  "extra_date_cols",
  "extra_datetime_cols",
  "color_columns"
)

#' Where the profile lives inside a project
#' @noRd
profile_path <- function(project_dir) {
  file.path(project_dir, "resources", "cleaningkitapp.yml")
}

#' Read a project profile, filling in any setting it does not have
#'
#' A profile written by an older version of the app lacks the settings added
#' since; those take their default value instead of failing.
#'
#' @param project_dir Project folder.
#' @return A profile list, see `default_profile()`.
#' @noRd
read_profile <- function(project_dir) {
  path <- profile_path(project_dir)
  if (!file.exists(path)) {
    return(default_profile())
  }
  saved <- yaml::read_yaml(path)
  if (!is.list(saved)) {
    stop("The project profile is not valid YAML: ", path)
  }
  normalise_profile(utils::modifyList(default_profile(), saved))
}

#' Write a project profile
#' @noRd
write_profile <- function(profile, project_dir) {
  path <- profile_path(project_dir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(normalise_profile(profile), path)
  invisible(path)
}

#' Coerce each setting to the type the app expects
#'
#' YAML loses some R types on a round trip: an empty character vector comes
#' back as an empty list, and a whole number may come back as a double. This
#' puts every value back to the type of its default.
#'
#' @noRd
normalise_profile <- function(profile) {
  defaults <- default_profile()
  for (section in setdiff(names(defaults), c("profile_version", "checks"))) {
    for (key in names(defaults[[section]])) {
      value <- profile[[section]][[key]]
      default <- defaults[[section]][[key]]
      profile[[section]][[key]] <- coerce_like(
        value,
        default,
        vector = key %in% .vector_settings
      )
    }
  }
  # the check settings follow the registry, see merge_check_settings()
  profile$checks <- merge_check_settings(profile$checks)
  profile
}

#' @noRd
coerce_like <- function(value, default, vector = FALSE) {
  if (vector) {
    return(if (is.null(value)) character(0) else as.character(unlist(value)))
  }
  if (is.null(value) || length(value) == 0) {
    if (is.character(default)) {
      return(if (length(default) == 0) character(0) else default)
    }
    return(default)
  }
  if (is.logical(default)) {
    return(isTRUE(as.logical(value[[1]])))
  }
  if (is.integer(default)) {
    return(as.integer(value[[1]]))
  }
  if (is.character(default) && length(default) == 0) {
    # a vector setting
    return(as.character(unlist(value)))
  }
  if (is.character(default)) {
    return(as.character(value[[1]]))
  }
  value
}

#' Resolve a path stored in the profile against the project folder
#' @noRd
project_file <- function(project_dir, relative_path) {
  if (is.null(project_dir) || !nzchar(relative_path)) {
    return(NULL)
  }
  if (is_absolute_path(relative_path)) {
    return(relative_path)
  }
  file.path(project_dir, relative_path)
}

#' @noRd
is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:[/\\\\]|/|\\\\\\\\)", path)
}
