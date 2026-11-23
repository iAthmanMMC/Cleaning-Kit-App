#' Start page: pick what to do
#'
#' Two choices in plain words, so a first-time user does not have to know the
#' Validate/Clean steps.
#'
#' @noRd
mod_start_ui <- function(id) {
  ns <- shiny::NS(id)
  choice <- function(button, icon, title, text, label) {
    bslib::card(
      class = "h-100",
      bslib::card_body(
        htmltools::h4(shiny::icon(icon), title),
        htmltools::p(text),
        shiny::actionButton(
          ns(button),
          label,
          class = "btn-primary",
          icon = shiny::icon("arrow-right")
        )
      )
    )
  }
  htmltools::tagList(
    htmltools::h3("What do you want to do?"),
    htmltools::p(
      class = "ck-hint",
      "Your files stay in this session only and are deleted when you close ",
      "the page."
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      choice(
        "check",
        "magnifying-glass",
        "Check new data",
        paste(
          "Upload the survey data you downloaded from ONA. The app checks the",
          "surveys that were not checked before and gives you a workbook for",
          "the reviewers."
        ),
        "Check new data"
      ),
      choice(
        "apply",
        "broom",
        "Apply corrections",
        paste(
          "Upload the workbook the reviewers filled in. The app applies their",
          "corrections and gives you the clean data."
        ),
        "Apply corrections"
      )
    )
  )
}

#' @param go `function(value)` that opens the navbar tab `value`.
#' @noRd
mod_start_server <- function(id, go) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$check, go("check_flow"))
    shiny::observeEvent(input$apply, go("import"))
  })
}
