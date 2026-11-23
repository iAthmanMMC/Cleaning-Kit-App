#' "Apply corrections": upload, review problems, download
#'
#' The reviewers' workbook(s) and an ONA download in, the final workbook out.
#' The questionnaire and the settings come from the workbook, so nothing else
#' is asked. See `start_apply()`.
#'
#' Like "Check new data", the screen keeps its own store and temporary
#' workspace, removed when the session ends.
#'
#' @noRd
mod_apply_flow_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::card(
      bslib::card_header(htmltools::strong("1. Your files")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),
          htmltools::div(
            shiny::fileInput(
              ns("workbooks"),
              "Workbook(s) the reviewers filled in",
              accept = c(".xlsx", ".xlsm"),
              multiple = TRUE
            ),
            htmltools::p(
              class = "ck-hint",
              "If the reviewers shared the work, upload each one's copy."
            )
          ),
          htmltools::div(
            shiny::fileInput(
              ns("download"),
              "Data downloaded from ONA (.xlsx)",
              accept = c(".xlsx", ".xls")
            ),
            htmltools::p(
              class = "ck-hint",
              "The download the round was checked from, or a newer one."
            )
          )
        ),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            "Advanced",
            icon = shiny::icon("sliders"),
            shiny::fileInput(
              ns("xlsform"),
              "Questionnaire (XLSForm)",
              accept = c(".xlsx", ".xls")
            ),
            htmltools::p(
              class = "ck-hint",
              "Leave empty to use the questionnaire kept in the workbook."
            )
          )
        ),
        shiny::actionButton(
          ns("start"),
          "Read the files",
          class = "btn-primary mt-3",
          icon = shiny::icon("file-import")
        ),
        shiny::uiOutput(ns("round_status"))
      )
    ),
    shiny::uiOutput(ns("problems_card")),
    shiny::uiOutput(ns("final_card")),
    details_fold(ns("details"))
  )
}

#' @noRd
mod_apply_flow_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- new_store()
    state <- shiny::reactiveVal(NULL)
    problem <- shiny::reactiveVal(NULL)
    apply_problem <- shiny::reactiveVal(NULL)
    applied <- shiny::reactiveVal(NULL)
    # the session's temporary folder; an environment so observers can replace it
    ws <- new.env()
    ws$dir <- NULL
    mod_runlog_server("details", store)

    session$onSessionEnded(function() {
      if (!is.null(ws$dir)) remove_workspace(ws$dir)
    })

    shiny::observeEvent(input$start, {
      if (is.null(input$workbooks)) {
        shiny::showNotification(
          "Upload the workbook the reviewers filled in.",
          type = "warning"
        )
        return()
      }
      if (is.null(input$download)) {
        shiny::showNotification(
          "Upload the data downloaded from ONA.",
          type = "warning"
        )
        return()
      }
      if (!is.null(ws$dir)) {
        remove_workspace(ws$dir)
      }
      ws$dir <- new_workspace()
      state(NULL)
      applied(NULL)
      problem(NULL)
      apply_problem(NULL)

      workbooks <- input$workbooks
      download <- input$download
      xlsform <- input$xlsform
      dir <- ws$dir
      res <- run_step(
        store,
        function() {
          start_apply(
            workbooks = workbooks,
            download = download,
            xlsform = xlsform,
            dir = dir
          )
        },
        step = "Read the files",
        dir = dir,
        notify = FALSE,
        full = TRUE
      )
      if (is.null(res$error)) {
        state(res$value)
      } else {
        problem(res$error)
      }
    })

    output$round_status <- shiny::renderUI({
      if (!is.null(problem())) {
        return(problem_box(problem()))
      }
      st <- state()
      if (is.null(st)) {
        return(NULL)
      }
      txt <- round_records_text(st$records, st$info)
      htmltools::div(
        class = paste(
          "alert mt-3",
          if (length(txt$warnings)) "alert-warning" else "alert-success"
        ),
        htmltools::p(class = "mb-1", txt$text),
        lapply(txt$warnings, function(w) {
          htmltools::p(class = "mb-1", status_icon("warn"), " ", w)
        })
      )
    })

    output$problems_card <- shiny::renderUI({
      st <- state()
      if (is.null(st)) {
        return(NULL)
      }
      ns <- session$ns
      counts <- evaluation_counts(st$reviewed$evaluation_log)
      ok <- counts$blocking == 0
      bslib::card(
        bslib::card_header(htmltools::strong("2. Review problems")),
        bslib::card_body(
          if (ok) {
            htmltools::p(
              status_icon("done"),
              " Every row of the workbook can be applied.",
              if (counts$warnings) {
                paste0(
                  " ",
                  counts$warnings,
                  " row(s) are worth a look, but don't stop the corrections."
                )
              }
            )
          } else {
            htmltools::div(
              class = "alert alert-danger",
              htmltools::p(
                class = "mb-1",
                htmltools::strong(counts$blocking),
                " row(s) can't be applied as they are: an action is missing ",
                "or unknown, or a new value is missing."
              ),
              htmltools::p(
                class = "mb-0",
                "Download a copy with these rows marked, send it to the ",
                "reviewer to fix, then upload the fixed workbook."
              )
            )
          },
          if (counts$blocking + counts$warnings > 0) {
            bslib::accordion(
              open = if (ok) FALSE else "Rows to fix",
              bslib::accordion_panel(
                if (ok) "Rows worth a look" else "Rows to fix",
                DT::DTOutput(ns("problems"))
              )
            )
          },
          htmltools::div(
            class = "mt-3 d-flex gap-2 flex-wrap",
            if (!ok) {
              shiny::downloadButton(
                ns("annotated"),
                "Download a copy for the reviewer to fix",
                class = "btn-outline-danger"
              )
            },
            if (ok) {
              shiny::actionButton(
                ns("apply"),
                "Apply the corrections",
                class = "btn-primary",
                icon = shiny::icon("wand-magic-sparkles")
              )
            }
          )
        )
      )
    })

    output$problems <- DT::renderDT({
      st <- state()
      shiny::req(st)
      ev <- plain_evaluation(st$reviewed$evaluation_log)
      shiny::req(ev)
      dt_table(ev, page_length = 10) |>
        DT::formatStyle(
          "Must fix?",
          target = "row",
          backgroundColor = DT::styleEqual(
            c("yes", "no"),
            c("#F8D7DA", "#FFF4CC")
          )
        )
    })

    output$annotated <- shiny::downloadHandler(
      filename = function() {
        paste0(state()$info$meta$round_name, "_rows_to_fix.xlsx")
      },
      content = function(file) {
        st <- state()
        write_annotated_log(
          st$reviewed$cleaning_log,
          st$reviewed$evaluation_log,
          file
        )
      }
    )

    shiny::observeEvent(input$apply, {
      st <- state()
      shiny::req(st)
      dir <- ws$dir
      apply_problem(NULL)
      res <- run_step(
        store,
        function() finish_apply(st, dir),
        step = "Apply the corrections",
        dir = dir,
        notify = FALSE,
        full = TRUE
      )
      if (is.null(res$error)) {
        applied(res$value)
      } else {
        apply_problem(res$error)
      }
    })

    output$final_card <- shiny::renderUI({
      if (!is.null(apply_problem())) {
        return(problem_box(apply_problem()))
      }
      out <- applied()
      st <- state()
      if (is.null(out) || is.null(st)) {
        return(NULL)
      }
      ns <- session$ns
      r <- out$clean_result
      removed <- r$n_before - r$n_after
      n_disagree <- nrow(out$check$tied)
      fin <- st$profile$final
      bslib::card(
        bslib::card_header(htmltools::strong("3. Final data")),
        bslib::card_body(
          htmltools::p(
            status_icon("done"),
            paste0(
              " ",
              r$n_after,
              " of ",
              r$n_before,
              " surveys kept",
              if (removed > 0) {
                paste0(" (", removed, " removed by the reviewers)")
              },
              ". ",
              nrow(r$final_log),
              " correction(s) recorded."
            )
          ),
          if (n_disagree == 0) {
            htmltools::p(
              status_icon("done"),
              " The clean data matches the corrections: every change in the ",
              "data is in the log, and every correction is in the data."
            )
          } else {
            htmltools::div(
              htmltools::p(
                status_icon("warn"),
                " ",
                n_disagree,
                " difference(s) between the clean data and the corrections. ",
                "Look at them before using the data."
              ),
              DT::DTOutput(ns("disagree"))
            )
          },
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::textInput(
              ns("project_name"),
              "Project name (for the README sheet)",
              value = fin$project_name
            ),
            shiny::textInput(
              ns("prepared_by"),
              "Prepared by",
              value = fin$prepared_by
            )
          ),
          shiny::downloadButton(
            ns("final"),
            "Download the final workbook",
            class = "btn-success"
          ),
          htmltools::p(
            class = "ck-hint mt-2",
            "The final workbook also remembers this round: you can upload it ",
            "instead of the reviewer workbook when you check the next round."
          )
        )
      )
    })

    output$disagree <- DT::renderDT({
      out <- applied()
      shiny::req(out, nrow(out$check$tied) > 0)
      dt_table(plain_disagreements(out$check$tied), page_length = 10)
    })

    output$final <- shiny::downloadHandler(
      filename = function() {
        round_final_file(state()$info$meta$round_name)
      },
      content = function(file) {
        st <- state()
        out <- applied()
        fin <- st$profile$final
        fin$project_name <- trimws(input$project_name %||% "")
        fin$prepared_by <- trimws(input$prepared_by %||% "")
        if (!nzchar(fin$data_collection_round %||% "")) {
          fin$data_collection_round <- st$info$meta$round_name
        }
        path <- file.path(ws$dir, round_final_file(st$info$meta$round_name))
        res <- run_step(
          store,
          function() {
            export_final(
              st$raw_data,
              out$clean_result,
              fin,
              path,
              round_info = st$info
            )
          },
          step = "Write the final workbook",
          dir = ws$dir,
          full = TRUE
        )
        if (!is.null(res$error)) {
          stop(res$error)
        }
        file.copy(path, file, overwrite = TRUE)
      }
    )

    invisible(store)
  })
}
