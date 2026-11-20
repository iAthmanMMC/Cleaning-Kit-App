#' Flags per check, for the Results screen
#'
#' @param results `store$check_results`.
#' @param logs `store$logs`.
#' @return A data frame with `check`, `status`, `flags` and `surveys`.
#' @noRd
flags_per_check <- function(results, logs, checks = ck_checks()) {
  ids <- intersect(names(checks), names(results))
  ids <- ids[vapply(checks[ids], function(x) x$kind == "log", logical(1))]
  if (length(ids) == 0) {
    return(NULL)
  }
  rows <- lapply(ids, function(cid) {
    r <- results[[cid]]
    log <- logs[[check_log_name(checks[[cid]])]]
    data.frame(
      check = checks[[cid]]$label,
      status = if (r$status == "error") "failed" else "ran",
      flags = if (is.data.frame(log)) nrow(log) else NA_integer_,
      surveys = if (is.data.frame(log)) {
        length(unique(log$uuid))
      } else {
        NA_integer_
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(-xtfrm(ifelse(is.na(out$flags), -1, out$flags))), , drop = FALSE]
}

#' All flags in one table, with the check and enumerator added
#'
#' For browsing only: the review workbook is built from the combined log
#' exactly as `create_combined_log()` returns it.
#'
#' @noRd
flags_table <- function(logs, raw_data, general, checks = ck_checks()) {
  if (length(logs) == 0) {
    return(NULL)
  }
  labels <- stats::setNames(
    vapply(checks, `[[`, "", "label"),
    vapply(
      checks,
      function(x) {
        if (x$kind == "log") check_log_name(x) else ""
      },
      ""
    )
  )
  rows <- lapply(names(logs), function(nm) {
    lg <- logs[[nm]]
    if (!is.data.frame(lg) || nrow(lg) == 0) {
      return(NULL)
    }
    data.frame(
      check = if (nm %in% names(labels)) labels[[nm]] else nm,
      uuid = as.character(lg$uuid),
      question = as.character(lg$question %||% NA),
      old_value = as.character(lg$old_value %||% NA),
      issue = as.character(lg$issue %||% NA),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    return(NULL)
  }
  body <- data_body(raw_data, general$skip_label_row)
  if (all(c(general$uuid_column, general$enumerator_column) %in% names(body))) {
    lookup <- stats::setNames(
      as.character(body[[general$enumerator_column]]),
      as.character(body[[general$uuid_column]])
    )
    out$enumerator <- unname(lookup[out$uuid])
  }
  out
}

#' Rank enumerators by how suspicious their flags look
#'
#' Wraps `cleaningkit::prioritize_enumerators()`.
#'
#' @return The priority table, or `NULL` when there are no logs.
#' @noRd
enumerator_priority <- function(
  logs,
  raw_data,
  general,
  diversity_bonus = 0.5,
  include_clean = FALSE,
  min_interviews = 1
) {
  if (length(logs) == 0 || is.null(raw_data)) {
    return(NULL)
  }
  res <- cleaningkit::prioritize_enumerators(
    c(list(checked_dataset = raw_data), logs),
    uuid_column = general$uuid_column,
    enumerator_column = general$enumerator_column,
    diversity_bonus = diversity_bonus,
    include_clean = include_clean,
    min_interviews = min_interviews,
    skip_label_row = general$skip_label_row
  )
  res$enumerator_priority
}
