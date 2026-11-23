#' "Check new data": upload, check, download
#'
#' Files in, one workbook out, no project folder. The round's memory (which
#' surveys were already checked, the questionnaire, the settings) travels in
#' hidden sheets of the reviewer workbook, so the next round needs only the
#' new ONA download and the last workbook. See `start_round()`.
#'
#' The screen keeps its own store and its own temporary workspace, apart from
#' the folder screens, and removes the workspace when the session ends.
#'
#' @noRd
mod_check_flow_ui <- function(id) {
  ns <- shiny::NS(id)
  excel <- c(".xlsx", ".xls")
  htmltools::tagList(
    bslib::card(
      bslib::card_header(htmltools::strong("1. Your files")),
      bslib::card_body(
        shiny::radioButtons(
          ns("first"),
          "Have you checked data from this survey before?",
          choices = c(
            "No, this is the first round" = "first",
            "Yes, I have last round's workbook" = "next"
          ),
          selected = "next"
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::fileInput(
            ns("download"),
            "New data downloaded from ONA (.xlsx)",
            accept = excel
          ),
          shiny::conditionalPanel(
            "input.first == 'next'",
            ns = ns,
            shiny::fileInput(
              ns("previous"),
              "Last round's reviewer workbook",
              accept = c(".xlsx", ".xlsm", ".csv"),
              multiple = TRUE
            ),
            htmltools::p(
              class = "ck-hint",
              "The workbook remembers which surveys were checked before. ",
              "You can upload more than one."
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          htmltools::div(
            shiny::fileInput(
              ns("xlsform"),
              "Questionnaire (XLSForm)",
              accept = excel
            ),
            shiny::conditionalPanel(
              "input.first == 'next'",
              ns = ns,
              htmltools::p(
                class = "ck-hint",
                "Optional: leave empty to use the questionnaire kept in ",
                "last round's workbook. Upload it if the form changed."
              )
            )
          ),
          shiny::textInput(
            ns("round_name"),
            "Name of this round",
            value = default_round_name()
          )
        ),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            "Advanced",
            icon = shiny::icon("sliders"),
            htmltools::p(
              class = "ck-hint",
              "The app finds these columns by itself. Fill one in only if ",
              "it got it wrong."
            ),
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::textInput(ns("uuid_column"), "Survey ID column"),
              shiny::textInput(ns("enumerator_column"), "Enumerator column"),
              shiny::textInput(ns("date_column"), "Date column")
            ),
            bslib::layout_columns(
              col_widths = c(4, 8),
              shiny::selectInput(
                ns("label_row"),
                "Row 2 holds question labels",
                choices = c(
                  "Find out" = "auto",
                  "Yes" = "yes",
                  "No" = "no"
                )
              ),
              shiny::fileInput(
                ns("checklist"),
                "Logical checklist (.xlsx)",
                accept = excel
              )
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
    shiny::uiOutput(ns("checks_card")),
    shiny::uiOutput(ns("results_card"))
  )
}

#' @noRd
mod_check_flow_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- new_store()
    round <- shiny::reactiveVal(NULL)
    outcome <- shiny::reactiveVal(NULL)
    # the session's temporary folder; an environment so observers can replace it
    ws <- new.env()
    ws$dir <- NULL
    checks <- ck_checks()

    session$onSessionEnded(function() {
      if (!is.null(ws$dir)) remove_workspace(ws$dir)
    })

    overrides <- function() {
      out <- list()
      for (key in c("uuid_column", "enumerator_column", "date_column")) {
        v <- trimws(input[[key]] %||% "")
        if (nzchar(v)) out[[key]] <- v
      }
      if (identical(input$label_row, "yes")) {
        out$skip_label_row <- TRUE
      }
      if (identical(input$label_row, "no")) {
        out$skip_label_row <- FALSE
      }
      if (length(out)) out
    }

    shiny::observeEvent(input$start, {
      if (is.null(input$download)) {
        shiny::showNotification(
          "Upload the data downloaded from ONA first.",
          type = "warning"
        )
        return()
      }
      next_round <- identical(input$first, "next")
      if (next_round && is.null(input$previous)) {
        shiny::showNotification(
          paste(
            "Upload last round's workbook, or choose",
            "\"No, this is the first round\"."
          ),
          type = "warning"
        )
        return()
      }
      if (!next_round && is.null(input$xlsform)) {
        shiny::showNotification(
          "Upload the questionnaire (XLSForm) for the first round.",
          type = "warning"
        )
        return()
      }

      # a fresh workspace per attempt, so no file from an earlier try lingers
      if (!is.null(ws$dir)) {
        remove_workspace(ws$dir)
      }
      ws$dir <- new_workspace(templates = TRUE)
      round(NULL)
      outcome(NULL)

      download <- input$download
      previous <- if (next_round) input$previous
      xlsform <- input$xlsform
      checklist <- input$checklist
      general_override <- overrides()
      dir <- ws$dir
      res <- run_step(
        store,
        function() {
          start_round(
            download = download,
            previous_files = previous,
            xlsform = xlsform,
            checklist = checklist,
            dir = dir,
            general_override = general_override
          )
        },
        step = "Read the files",
        dir = dir,
        full = TRUE
      )
      if (is.null(res$error)) {
        round(res$value)
      }
    })

    output$round_status <- shiny::renderUI({
      r <- round()
      if (is.null(r)) {
        return(NULL)
      }
      g <- r$general
      missing <- c(
        enumerator_column = "enumerator",
        date_column = "date"
      )[intersect(c("enumerator_column", "date_column"), g$missing)]
      htmltools::div(
        class = paste(
          "alert mt-3",
          if (r$counts$n_new == 0) "alert-warning" else "alert-success"
        ),
        htmltools::p(
          class = "mb-1",
          round_counts_text(r$counts, r$previous)
        ),
        if (r$counts$n_new == 0) {
          htmltools::p(
            class = "mb-1",
            "There is nothing new to check. Is this the latest download?"
          )
        },
        htmltools::p(
          class = "mb-0 small",
          "Survey ID column: ",
          htmltools::code(g$uuid_column),
          ". Enumerator column: ",
          htmltools::code(r$profile$general$enumerator_column),
          ". Row 2 holds labels: ",
          if (isTRUE(r$profile$general$skip_label_row)) "yes" else "no",
          "."
        ),
        if (length(missing)) {
          htmltools::p(
            class = "mb-0 small text-danger",
            "No ",
            paste(missing, collapse = " or "),
            " column found: type its name under Advanced if you need it."
          )
        }
      )
    })

    output$checks_card <- shiny::renderUI({
      r <- round()
      if (is.null(r) || r$counts$n_new == 0) {
        return(NULL)
      }
      ns <- session$ns
      saved <- r$profile$checks
      choices <- stats::setNames(
        names(checks),
        vapply(checks, `[[`, "", "label")
      )
      selected <- names(checks)[vapply(
        names(checks),
        function(cid) isTRUE(saved[[cid]]$enabled),
        logical(1)
      )]
      # the logical check needs a checklist (uploaded under Advanced)
      if (is.null(r$checklist_path)) {
        selected <- setdiff(selected, "logical")
      }
      bslib::card(
        bslib::card_header(htmltools::strong("2. Checks")),
        bslib::card_body(
          shiny::checkboxGroupInput(
            ns("checks"),
            "Checks to run",
            choices = choices,
            selected = selected
          ),
          htmltools::p(
            class = "ck-hint",
            "The settings of each check come from last round's workbook, or ",
            "the defaults in the first round.",
            if (is.null(r$checklist_path)) {
              paste(
                "Logical checks need a logical checklist: upload one under",
                "Advanced to run them."
              )
            }
          ),
          shiny::actionButton(
            ns("run"),
            "Run the checks",
            class = "btn-primary",
            icon = shiny::icon("play")
          )
        )
      )
    })

    shiny::observeEvent(input$run, {
      r <- round()
      shiny::req(r)
      selected <- intersect(names(checks), input$checks)
      if (length(selected) == 0) {
        shiny::showNotification("Choose at least one check.", type = "warning")
        return()
      }
      settings <- r$profile$checks
      for (cid in names(checks)) {
        settings[[cid]]$enabled <- cid %in% selected
      }
      r$profile$checks <- settings
      round(r)

      ctx <- check_context(
        raw_data = r$raw_data,
        tool_survey = r$tool$tool_survey,
        tool_choices = r$tool$tool_choices,
        other_db = r$tool$other_db,
        general = r$profile$general,
        project_dir = ws$dir
      )
      out <- run_selected_checks(store, checks[selected], settings, ctx)
      if (length(out$logs)) {
        out$combined_log <- run_step(
          store,
          function() {
            cleaningkit::create_combined_log(
              c(list(checked_dataset = r$raw_data), out$logs)
            )
          },
          step = "Combine the results",
          notify = TRUE
        )
      }
      notify_run(out$results, checks)
      outcome(out)
    })

    output$results_card <- shiny::renderUI({
      out <- outcome()
      if (is.null(out)) {
        return(NULL)
      }
      ns <- session$ns
      failed <- names(out$results)[vapply(
        out$results,
        function(x) x$status == "error",
        logical(1)
      )]
      bslib::card(
        bslib::card_header(htmltools::strong("3. Results")),
        bslib::card_body(
          if (length(failed)) {
            htmltools::div(
              class = "alert alert-warning py-2",
              htmltools::strong("Some checks could not run: "),
              htmltools::tags$ul(lapply(failed, function(cid) {
                htmltools::tags$li(
                  checks[[cid]]$label,
                  ": ",
                  out$results[[cid]]$error
                )
              }))
            )
          },
          bslib::layout_columns(
            col_widths = c(6, 6),
            htmltools::div(
              htmltools::h5("Flags per check"),
              DT::DTOutput(ns("per_check"))
            ),
            htmltools::div(
              htmltools::h5("Enumerators to follow up first"),
              DT::DTOutput(ns("enumerators"))
            )
          ),
          if (is.null(out$combined_log)) {
            htmltools::p(class = "text-danger", "No check produced results.")
          } else {
            htmltools::div(
              class = "mt-3",
              shiny::downloadButton(
                ns("workbook"),
                "Download the reviewer workbook",
                class = "btn-success"
              ),
              htmltools::p(
                class = "ck-hint mt-2",
                "Keep this workbook: upload it next round so the surveys ",
                "checked now are not checked again."
              )
            )
          }
        )
      )
    })

    output$per_check <- DT::renderDT({
      out <- outcome()
      shiny::req(out)
      tab <- flags_per_check(out$results, out$logs, checks)
      shiny::req(tab)
      names(tab) <- c("Check", "Status", "Flags", "Surveys")
      dt_table(tab, page_length = 10)
    })

    output$enumerators <- DT::renderDT({
      out <- outcome()
      r <- round()
      shiny::req(out, r)
      tab <- tryCatch(
        enumerator_priority(out$logs, r$raw_data, r$profile$general),
        error = function(e) NULL
      )
      shiny::req(tab)
      dt_table(utils::head(tab, 50), page_length = 10)
    })

    output$workbook <- shiny::downloadHandler(
      filename = function() {
        r <- round()
        name <- trimws(input$round_name %||% "")
        if (!nzchar(name)) {
          name <- default_round_name()
        }
        round_review_file(name, vba = isTRUE(r$profile$review$vba))
      },
      content = function(file) {
        r <- round()
        out <- outcome()
        name <- trimws(input$round_name %||% "")
        if (!nzchar(name)) {
          name <- default_round_name()
        }
        path <- file.path(
          ws$dir,
          "output",
          round_review_file(name, vba = isTRUE(r$profile$review$vba))
        )
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        res <- run_step(
          store,
          function() {
            build_review_workbook(
              out$combined_log,
              path,
              review = r$profile$review,
              general = r$profile$general,
              other_responses = out$other_responses,
              other_db = r$tool$other_db,
              round_info = make_round_info(r, name)
            )
          },
          step = "Build the reviewer workbook",
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
