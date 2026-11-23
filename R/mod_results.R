#' Results screen: what the checks found
#'
#' Not in the example scripts: there the logs only became visible once the
#' review workbook was opened. Includes the enumerator ranking from
#' `prioritize_enumerators()`.
#'
#' @noRd
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Per check", DT::DTOutput(ns("per_check"))),
      bslib::nav_panel("All flags", DT::DTOutput(ns("flags"))),
      bslib::nav_panel(
        "Enumerators",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = "right",
            width = 260,
            open = TRUE,
            title = "Ranking",
            shiny::sliderInput(
              ns("diversity_bonus"),
              "Extra weight per additional check",
              min = 0,
              max = 2,
              value = 0.5,
              step = 0.1
            ),
            shiny::numericInput(
              ns("min_interviews"),
              "Hide enumerators with fewer interviews than",
              1,
              min = 1,
              step = 1
            ),
            shiny::checkboxInput(
              ns("include_clean"),
              "Also list enumerators with no flags",
              FALSE
            ),
            htmltools::p(
              class = "ck-hint",
              "An enumerator who trips several different checks ranks above ",
              "one with many flags from a single check."
            )
          ),
          DT::DTOutput(ns("enumerators"))
        )
      ),
      bslib::nav_panel(
        "\"Other\" responses",
        shiny::uiOutput(ns("translation")),
        DT::DTOutput(ns("other"))
      )
    )
  )
}

#' @noRd
mod_results_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    general <- shiny::reactive(store$profile$general)

    flags <- shiny::reactive({
      flags_table(store$logs, store$raw_data, general())
    })

    output$summary <- shiny::renderUI({
      if (length(store$check_results) == 0) {
        return(bslib::card(bslib::card_body(
          class = "ck-hint",
          "Run the checks on the Checks screen to see what they found."
        )))
      }
      f <- flags()
      g <- general()
      n_records <- n_records(store$raw_data, g$skip_label_row)
      n_flags <- if (is.null(f)) 0L else nrow(f)
      n_surveys <- if (is.null(f)) 0L else length(unique(f$uuid))
      n_enum <- if (is.null(f) || is.null(f$enumerator)) {
        "?"
      } else {
        length(unique(stats::na.omit(f$enumerator)))
      }
      pct <- if (n_records > 0) round(100 * n_surveys / n_records) else 0
      bslib::layout_column_wrap(
        width = 1 / 4,
        fill = FALSE,
        bslib::value_box(
          "Flags",
          n_flags,
          showcase = shiny::icon("flag"),
          theme = "primary"
        ),
        bslib::value_box(
          "Surveys flagged",
          n_surveys,
          htmltools::p(paste0(pct, "% of ", n_records)),
          showcase = shiny::icon("users"),
          theme = "secondary"
        ),
        bslib::value_box(
          "Enumerators flagged",
          n_enum,
          showcase = shiny::icon("user-pen"),
          theme = "info"
        ),
        bslib::value_box(
          "\"Other\" responses",
          if (is.null(store$other_responses)) {
            "-"
          } else {
            nrow(store$other_responses)
          },
          showcase = shiny::icon("pen"),
          theme = "light"
        )
      )
    })

    output$per_check <- DT::renderDT({
      tbl <- flags_per_check(store$check_results, store$logs)
      shiny::req(tbl)
      DT::datatable(
        tbl,
        rownames = FALSE,
        colnames = c("Check", "Status", "Flags", "Surveys"),
        selection = "none",
        options = list(dom = "t", pageLength = 50, ordering = FALSE)
      )
    })

    output$flags <- DT::renderDT({
      f <- flags()
      shiny::req(f)
      dt_table(f, page_length = 25)
    })

    output$enumerators <- DT::renderDT({
      shiny::req(length(store$logs) > 0)
      tbl <- tryCatch(
        enumerator_priority(
          store$logs,
          store$raw_data,
          general(),
          diversity_bonus = input$diversity_bonus %||% 0.5,
          include_clean = isTRUE(input$include_clean),
          min_interviews = input$min_interviews %||% 1
        ),
        error = function(e) {
          shiny::validate(paste(
            "Could not rank enumerators:",
            conditionMessage(e)
          ))
        }
      )
      shiny::req(tbl)
      dt_table(tbl, page_length = 25)
    })

    output$other <- DT::renderDT({
      shiny::req(store$other_responses)
      dt_table(store$other_responses, page_length = 25)
    })

    output$translation <- shiny::renderUI({
      shiny::req(store$other_responses)
      preview <- translation_preview(store$other_responses)
      if (!is.null(preview)) {
        return(translation_preview_box(
          preview,
          DT::DTOutput(session$ns("translation_preview"))
        ))
      }
      setting <- store$profile$checks$other_responses$args$translate
      if (identical(setting, "send")) translation_sent_note()
    })

    output$translation_preview <- DT::renderDT({
      preview <- translation_preview(store$other_responses)
      shiny::req(preview, nrow(preview) > 0)
      dt_table(preview, page_length = 10)
    })
  })
}
