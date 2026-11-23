# Machine translation of the "other" responses ---------------------------------
#
# cleaningkit's prepare_other_responses() can fill a `translation` column with
# an English machine translation (Google Translate's free service). It is off
# unless the user switches it on, and "preview" lists the exact texts that
# would be sent without sending anything. Only the distinct "other" texts are
# sent: no survey ID, enumerator or any other column.
#
# The "Other" text responses panel of the Checks screen offers all three
# settings through the registry. The "Check new data" screen offers only Off
# and Translate for the run, and a separate "Preview the texts" button that
# runs the other-responses step alone: a preview should not need every check
# to run, and a button does not leave the screen in a mode the run has to be
# switched back out of.

#' The three translation settings, labelled for the user
#' @noRd
translate_choices <- function() {
  c(
    "Off - nothing is sent" = "off",
    "Preview - show what would be sent, send nothing" = "preview",
    "Translate - send the texts to Google Translate" = "send"
  )
}

#' The settings offered for a run on the "Check new data" screen
#'
#' The preview has its own button there, so it is not one of them.
#' @noRd
translate_run_choices <- function() {
  translate_choices()[c(1, 3)]
}

#' One-paragraph explanation shown under the setting
#' @noRd
translate_hint <- function() {
  paste(
    "Fills the translation column next to each \"other\" answer.",
    "Only the typed \"other\" texts are sent, each one once: no survey ID,",
    "enumerator or any other column. Translations can be wrong, so reviewers",
    "must check them. Check your data-protection rules before sending."
  )
}

#' The value prepare_other_responses() takes for a setting
#'
#' Anything other than "preview" or "send" counts as off, so an empty or
#' unknown stored value never sends anything.
#' @noRd
translate_arg <- function(value) {
  value <- if (length(value)) as.character(value[[1]]) else ""
  if (identical(value, "send")) {
    return(TRUE)
  }
  if (identical(value, "preview")) {
    return("preview")
  }
  FALSE
}

#' `build` step of the "other" check: settings in, call arguments out
#'
#' The translation cache goes in the project's output folder, so a re-run of
#' the round only sends texts that were not translated before. It holds
#' response text, like every other file in that folder.
#' @noRd
build_other_responses <- function(args, ctx) {
  mask <- if (is.null(args$translate_options)) {
    TRUE
  } else {
    isTRUE(args$translate_options)
  }
  args$translate <- translate_arg(args$translate)
  args$translate_options <- cleaningkit::ck_translate_options(
    mask_numbers = mask,
    cache_file = if (!is.null(ctx$project_dir)) {
      file.path(ctx$project_dir, "output", "translation_cache.rds")
    }
  )
  args
}

#' The preview cleaningkit attached to the other-responses table, if any
#' @noRd
translation_preview <- function(other_responses) {
  if (is.null(other_responses)) {
    return(NULL)
  }
  attr(other_responses, "translation_preview")
}

#' Box shown after a preview run
#'
#' @param preview Data frame from `translation_preview()`.
#' @param table_output A `DT::DTOutput()` for the texts.
#' @param next_step What to do if the list is acceptable, as a sentence.
#' @noRd
translation_preview_box <- function(
  preview,
  table_output,
  next_step = "If that is acceptable, choose \"Translate\" and run the checks again."
) {
  n <- nrow(preview)
  htmltools::div(
    class = "alert alert-info mt-3",
    htmltools::p(
      class = "mb-1",
      htmltools::strong("Translation preview: nothing was sent. ")
    ),
    htmltools::p(
      class = if (n > 0) "mb-2" else "mb-0",
      if (n == 0) {
        "No text would be sent: there is nothing new to translate."
      } else {
        paste0(
          n,
          " distinct text(s) would be sent to Google Translate, exactly as ",
          "listed below (long numbers already hidden as #####). ",
          next_step
        )
      }
    ),
    if (n > 0) table_output
  )
}

#' One line after a run that sent texts
#' @noRd
translation_sent_note <- function() {
  htmltools::p(
    class = "ck-hint mt-2",
    "The \"other\" responses were machine-translated into the translation ",
    "column. Translations may be inaccurate: reviewers should confirm them ",
    "against the original answer."
  )
}
