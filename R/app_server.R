#' App server
#'
#' @param project_dir Folder to open on start-up, or `NULL`.
#' @noRd
app_server <- function(input, output, session, project_dir = NULL) {
  store <- new_store()
  hosted <- ck_is_server()

  mod_project_server(
    "project",
    store,
    initial_dir = project_dir,
    hosted = hosted
  )
  mod_tool_server("tool", store)
  mod_data_server("data", store)
  mod_checks_server("checks", store)
  mod_results_server("results", store)
  mod_review_server("review", store, hosted = hosted)
  mod_import_server("import", store)
  mod_apply_server("apply", store)
  mod_final_server("final", store, hosted = hosted)
  mod_runlog_server("runlog", store)

  output$status <- shiny::renderUI({
    steps <- workflow_status(store)
    htmltools::tags$ul(
      class = "ck-status-list",
      lapply(steps, function(s) {
        htmltools::tags$li(
          status_icon(s$state),
          htmltools::div(
            htmltools::div(s$label),
            if (!is.null(s$detail)) {
              htmltools::div(class = "ck-status-detail", s$detail)
            }
          )
        )
      })
    )
  })
}

#' State of each workflow step, for the progress sidebar
#'
#' @param store The session store.
#' @return A list of steps, each a list with `label`, `state` (see
#'   `status_icon()`) and `detail`.
#' @noRd
workflow_status <- function(store) {
  dir <- store$project_dir
  project <- if (is.null(dir)) {
    list(label = "Project", state = "todo", detail = "No folder open")
  } else {
    missing <- !all(project_folder_status(dir)$exists)
    list(
      label = "Project",
      state = if (missing) "warn" else "done",
      detail = if (missing) {
        "Folders not set up"
      } else if (ck_is_server()) {
        "Online workspace"
      } else {
        basename(dir)
      }
    )
  }

  tool <- if (is.null(store$tool_survey)) {
    list(label = "Tool", state = "todo", detail = NULL)
  } else {
    issues <- store$constraint_issues
    n_issues <- if (length(issues) == 1 && is.na(issues)) 0L else length(issues)
    list(
      label = "Tool",
      state = if (n_issues > 0) "warn" else "done",
      detail = paste0(
        nrow(store$tool_survey),
        " questions",
        if (n_issues > 0) paste0(", ", n_issues, " constraint issue(s)")
      )
    )
  }

  data <- if (is.null(store$raw_data)) {
    list(label = "Data", state = "todo", detail = NULL)
  } else {
    list(
      label = "Data",
      state = "done",
      detail = paste0(
        n_records(store$raw_data, store$profile$general$skip_label_row),
        " records"
      )
    )
  }

  list(
    project,
    tool,
    data,
    checks_status(store$check_results),
    if (is.null(store$review_path)) {
      list(label = "Review workbook", state = "todo", detail = NULL)
    } else {
      list(
        label = "Review workbook",
        state = "done",
        detail = basename(store$review_path)
      )
    },
    import_status(store$reviewed_log, store$evaluation),
    if (is.null(store$clean_result)) {
      list(label = "Apply", state = "todo", detail = NULL)
    } else {
      list(
        label = "Apply",
        state = "done",
        detail = paste0(
          store$clean_result$n_after,
          " of ",
          store$clean_result$n_before,
          " records kept"
        )
      )
    },
    if (is.null(store$final_path)) {
      list(label = "Final output", state = "todo", detail = NULL)
    } else {
      list(
        label = "Final output",
        state = "done",
        detail = basename(store$final_path)
      )
    }
  )
}

#' Progress-sidebar entry for the reviewed logs
#' @noRd
import_status <- function(reviewed_log, evaluation) {
  if (is.null(reviewed_log)) {
    return(list(label = "Import reviewed", state = "todo", detail = NULL))
  }
  counts <- evaluation_counts(evaluation)
  list(
    label = "Import reviewed",
    state = if (counts$blocking > 0) {
      "error"
    } else if (counts$warnings > 0) {
      "warn"
    } else {
      "done"
    },
    detail = paste0(
      nrow(reviewed_log),
      " log rows",
      if (counts$blocking > 0) paste0(", ", counts$blocking, " to fix")
    )
  )
}

#' Progress-sidebar entry for the checks
#' @noRd
checks_status <- function(results) {
  if (length(results) == 0) {
    return(list(label = "Checks", state = "todo", detail = NULL))
  }
  failed <- sum(vapply(results, function(r) r$status == "error", logical(1)))
  flags <- sum(vapply(
    results,
    function(r) if (r$status == "done" && r$kind == "log") r$n else 0L,
    integer(1)
  ))
  list(
    label = "Checks",
    state = if (failed > 0) "warn" else "done",
    detail = paste0(
      flags,
      " flags",
      if (failed > 0) paste0(", ", failed, " check(s) failed")
    )
  )
}
