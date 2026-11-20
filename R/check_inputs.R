# Inputs for check arguments, drawn from the registry's arg specs.

#' Input id of one argument of a check, e.g. `"duration__lower_bound"`
#' @noRd
arg_input_id <- function(check_id, arg_name) {
  paste0(check_id, "__", arg_name)
}

#' Input id of a check's on/off switch
#' @noRd
enabled_input_id <- function(check_id) {
  paste0(check_id, "__enabled")
}

#' Build the input for one argument
#'
#' @param id Namespaced input id.
#' @param spec An `arg_spec()`.
#' @param value Current value.
#' @noRd
arg_input <- function(id, spec, value) {
  input <- switch(
    spec$type,
    number = shiny::numericInput(
      id,
      spec$label,
      value = value,
      min = spec$min,
      max = spec$max,
      step = spec$step
    ),
    toggle = shiny::checkboxInput(id, spec$label, value = isTRUE(value)),
    choice = shiny::selectInput(
      id,
      spec$label,
      choices = spec$choices,
      selected = value
    ),
    column = ,
    columns = ,
    texts = shiny::selectizeInput(
      id,
      spec$label,
      choices = value,
      selected = value,
      multiple = spec$type != "column",
      options = list(
        create = TRUE,
        placeholder = if (spec$type == "texts") {
          "Type and press Enter"
        } else {
          "Pick or type a column"
        }
      )
    ),
    # text, file
    shiny::textInput(id, spec$label, value = value)
  )
  htmltools::div(
    input,
    if (!is.null(spec$help)) {
      htmltools::div(class = "ck-hint mt-n2 mb-2", spec$help)
    }
  )
}

#' Put a value into an existing argument input
#'
#' @param columns Column names of the data, offered in column pickers.
#' @noRd
update_arg_input <- function(session, id, spec, value, columns = NULL) {
  switch(
    spec$type,
    number = shiny::updateNumericInput(session, id, value = value),
    toggle = shiny::updateCheckboxInput(session, id, value = isTRUE(value)),
    choice = shiny::updateSelectInput(session, id, selected = value),
    column = ,
    columns = shiny::updateSelectizeInput(
      session,
      id,
      choices = union(value[nzchar(value)], columns),
      selected = value,
      server = TRUE
    ),
    texts = shiny::updateSelectizeInput(
      session,
      id,
      choices = value,
      selected = value
    ),
    shiny::updateTextInput(session, id, value = value)
  )
}

#' Read every check's settings from the inputs
#'
#' @param input The module's `input`.
#' @param current Settings to fall back on for an input that has not been
#'   drawn yet (Shiny only creates inputs in panels that were shown).
#' @return Settings in the shape of `default_check_settings()`.
#' @noRd
read_check_inputs <- function(input, current, checks = ck_checks()) {
  out <- current
  for (id in names(checks)) {
    enabled <- input[[enabled_input_id(id)]]
    if (!is.null(enabled)) {
      out[[id]]$enabled <- isTRUE(enabled)
    }
    for (nm in names(checks[[id]]$args)) {
      spec <- checks[[id]]$args[[nm]]
      value <- input[[arg_input_id(id, nm)]]
      # an emptied multi-select reads as NULL, which is a real value here
      drawn <- !is.null(value) || spec$type %in% c("columns", "texts")
      if (drawn && !is.null(input[[enabled_input_id(id)]])) {
        out[[id]]$args[[nm]] <- coerce_arg(spec, value)
      }
    }
  }
  out
}
