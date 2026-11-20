#' Run log: every message, warning and error cleaningkit reported
#'
#' What the console showed when the example scripts were run, kept per
#' session and downloadable, so a data officer can see why a step flagged or
#' dropped something.
#'
#' @noRd
mod_runlog_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    min_height = "70vh",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Run log",
      htmltools::div(
        class = "d-flex gap-2",
        shiny::checkboxGroupInput(
          ns("levels"),
          label = NULL,
          choices = c(Info = "info", Warnings = "warning", Errors = "error"),
          selected = c("info", "warning", "error"),
          inline = TRUE
        ),
        shiny::downloadButton(
          ns("download"),
          "Download",
          class = "btn-sm btn-outline-primary"
        ),
        shiny::actionButton(
          ns("clear"),
          "Clear",
          class = "btn-sm btn-outline-secondary"
        )
      )
    ),
    DT::DTOutput(ns("table"))
  )
}

#' @noRd
mod_runlog_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    shown <- shiny::reactive({
      log <- store$log
      log <- log[log$level %in% input$levels, , drop = FALSE]
      log[rev(seq_len(nrow(log))), , drop = FALSE]
    })

    output$table <- DT::renderDT({
      DT::datatable(
        shown(),
        rownames = FALSE,
        colnames = c("Time", "Step", "Level", "Message"),
        options = list(pageLength = 25, dom = "ftip", ordering = FALSE),
        selection = "none"
      ) |>
        DT::formatStyle(
          "level",
          target = "row",
          backgroundColor = DT::styleEqual(
            c("warning", "error"),
            c("#FFF4CC", "#F8D7DA")
          )
        )
    })

    shiny::observeEvent(input$clear, {
      store$log <- empty_log()
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_cleaningkit_run_log.txt")
      },
      content = function(file) {
        log <- store$log
        lines <- sprintf(
          "%s  [%s]  %-8s %s",
          log$time,
          log$step,
          toupper(log$level),
          log$text
        )
        writeLines(lines, file, useBytes = TRUE)
      }
    )
  })
}
