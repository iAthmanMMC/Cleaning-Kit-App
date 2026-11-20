#' Import reviewed screen: read back the workbooks the reviewers filled in
#'
#' Replaces `check_log_files()`, `read_cleaning_log()` and
#' `evaluate_cleaning_log()` in example script 2.
#'
#' @noRd
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      title = "Reviewed workbooks",
      shiny::uiOutput(ns("data_status")),
      shiny::uiOutput(ns("files")),
      shiny::fileInput(
        ns("upload"),
        "Add edited workbook(s)",
        accept = c(".xlsx", ".xlsm"),
        multiple = TRUE,
        buttonLabel = "Choose...",
        placeholder = "copied into output/follow_ups/"
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Advanced",
          icon = shiny::icon("sliders"),
          shiny::textInput(
            ns("file_pattern"),
            "Workbook name pattern (regular expression)"
          ),
          shiny::selectizeInput(
            ns("valid_actions"),
            "Valid actions",
            choices = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          ),
          shiny::textInput(
            ns("default_blank_action"),
            "Action for rows left blank"
          )
        )
      ),
      shiny::actionButton(
        ns("read"),
        "Check and read",
        icon = shiny::icon("magnifying-glass"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Problems", DT::DTOutput(ns("problems"))),
      bslib::nav_panel("Actions", DT::DTOutput(ns("actions"))),
      bslib::nav_panel("Reviewed log", DT::DTOutput(ns("log"))),
      bslib::nav_panel("File check", shiny::uiOutput(ns("file_check")))
    )
  )
}

#' @noRd
mod_import_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh <- shiny::reactiveVal(0)

    shiny::observeEvent(store$project_dir, {
      c <- store$profile$clean
      shiny::updateTextInput(session, "file_pattern", value = c$file_pattern)
      shiny::updateSelectizeInput(
        session,
        "valid_actions",
        choices = c$valid_actions,
        selected = c$valid_actions
      )
      shiny::updateTextInput(
        session,
        "default_blank_action",
        value = c$default_blank_action
      )
      refresh(refresh() + 1)
    })

    pattern <- shiny::reactive({
      p <- input$file_pattern
      if (is.null(p) || !nzchar(p)) store$profile$clean$file_pattern else p
    })

    files <- shiny::reactive({
      refresh()
      shiny::invalidateLater(5000)
      shiny::req(store$project_dir)
      tryCatch(
        reviewed_files(store$project_dir, pattern()),
        error = function(e) {
          data.frame(file = character(0), modified = character(0))
        }
      )
    })

    output$data_status <- shiny::renderUI({
      if (is.null(store$project_dir)) {
        return(htmltools::p(class = "ck-hint", "Open a project first."))
      }
      items <- list(
        list(
          "Data that was validated",
          !is.null(store$raw_data),
          if (is.null(store$raw_data)) {
            "load it on the Data screen with \"Keep only new records\" off, choosing the file you validated"
          } else {
            relative_to(store$raw_data_path, store$project_dir)
          }
        ),
        list(
          "XLSForm",
          !is.null(store$tool_survey),
          if (is.null(store$tool_survey)) {
            "needed for the \"other\" responses"
          } else {
            NULL
          }
        )
      )
      htmltools::tags$ul(
        class = "ck-status-list mb-3",
        lapply(items, function(x) {
          htmltools::tags$li(
            status_icon(if (x[[2]]) "done" else "error"),
            htmltools::div(
              x[[1]],
              if (!is.null(x[[3]])) {
                htmltools::div(class = "ck-status-detail", x[[3]])
              }
            )
          )
        })
      )
    })

    output$files <- shiny::renderUI({
      shiny::req(store$project_dir)
      f <- files()
      htmltools::tagList(
        htmltools::tags$ul(
          class = "ck-status-list mb-2",
          if (nrow(f) == 0) {
            htmltools::tags$li(
              status_icon("todo"),
              htmltools::span(
                class = "ck-hint",
                "No edited workbook in output/follow_ups/ yet. Its name must end in _edited, e.g. 2026-11-20_follow-ups_edited.xlsm."
              )
            )
          } else {
            lapply(seq_len(nrow(f)), function(i) {
              htmltools::tags$li(
                shiny::icon("file-excel", class = "text-success"),
                htmltools::span(class = "ck-path", f$file[i]),
                htmltools::span(class = "ck-status-detail", f$modified[i])
              )
            })
          }
        )
      )
    })

    shiny::observeEvent(input$upload, {
      shiny::req(store$project_dir)
      tryCatch(
        {
          copy_uploads(
            input$upload,
            file.path(store$project_dir, "output", "follow_ups")
          )
          not_matching <- !grepl(
            pattern(),
            input$upload$name,
            ignore.case = TRUE
          )
          if (any(not_matching)) {
            shiny::showNotification(
              paste0(
                "Added, but these names do not end in _edited so they will not be read: ",
                paste(input$upload$name[not_matching], collapse = ", ")
              ),
              type = "warning",
              duration = NULL
            )
          } else {
            shiny::showNotification(
              paste0("Added ", nrow(input$upload), " workbook(s)."),
              type = "message"
            )
          }
        },
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = NULL
          )
        }
      )
      refresh(refresh() + 1)
    })

    shiny::observeEvent(input$read, {
      if (is.null(store$project_dir)) {
        shiny::showNotification(
          "Open a project folder first.",
          type = "warning"
        )
        return()
      }
      if (is.null(store$raw_data)) {
        shiny::showNotification(
          "Load the data that was validated on the Data screen first.",
          type = "warning",
          duration = 8
        )
        return()
      }
      profile <- store$profile
      profile$clean$file_pattern <- pattern()
      profile$clean$valid_actions <- as.character(
        input$valid_actions %||% profile$clean$valid_actions
      )
      profile$clean$default_blank_action <- trimws(
        input$default_blank_action %||% "no_action"
      )
      save_profile(store, profile, notify = FALSE)

      dir <- store$project_dir
      raw <- store$raw_data
      res <- run_step(
        store,
        function() read_reviewed(dir, raw, profile$clean, profile$general),
        step = "Read reviewed logs",
        dir = dir
      )
      reset_from(store, "import")
      shiny::req(res)
      store$reviewed_checks <- res$file_checks
      store$reviewed_log <- res$cleaning_log
      store$evaluation <- res$evaluation_log
    })

    output$summary <- shiny::renderUI({
      log <- store$reviewed_log
      if (is.null(log)) {
        return(bslib::card(bslib::card_body(
          class = "ck-hint",
          "Put the reviewers' edited workbooks in output/follow_ups/ and press Check and read."
        )))
      }
      counts <- evaluation_counts(store$evaluation)
      c <- store$profile$clean
      actions <- as.character(log[[c$log_action_col]])
      htmltools::tagList(
        bslib::layout_column_wrap(
          width = 1 / 4,
          fill = FALSE,
          bslib::value_box(
            "Log rows",
            nrow(log),
            showcase = shiny::icon("list"),
            theme = "primary"
          ),
          bslib::value_box(
            "Changes",
            sum(
              actions %in% c(c$change_response_values, c$blank_response_values)
            ),
            showcase = shiny::icon("pen-to-square"),
            theme = "secondary"
          ),
          bslib::value_box(
            "Surveys to discard",
            length(unique(log[[c$log_uuid_col]][
              actions %in% c$remove_survey_values
            ])),
            showcase = shiny::icon("trash"),
            theme = "info"
          ),
          bslib::value_box(
            "Problems",
            counts$blocking + counts$warnings,
            htmltools::p(paste0(counts$blocking, " must be fixed")),
            showcase = shiny::icon(
              if (counts$blocking) "circle-xmark" else "circle-check"
            ),
            theme = if (counts$blocking) {
              "danger"
            } else if (counts$warnings) {
              "warning"
            } else {
              "success"
            }
          )
        ),
        if (counts$blocking > 0) {
          htmltools::div(
            class = "alert alert-danger d-flex justify-content-between align-items-center",
            htmltools::span(
              htmltools::strong(counts$blocking),
              " row(s) cannot be applied as they are (no action, an unknown action or no new value). ",
              "Send the annotated log back to the reviewers, then read the corrected workbook again."
            ),
            shiny::downloadButton(
              session$ns("annotated"),
              "Annotated log",
              class = "btn-sm btn-light"
            )
          )
        } else if (counts$warnings > 0) {
          htmltools::div(
            class = "alert alert-warning d-flex justify-content-between align-items-center",
            htmltools::span(
              counts$warnings,
              " row(s) worth a look (see Problems). The log can be applied on the Apply screen."
            ),
            shiny::downloadButton(
              session$ns("annotated"),
              "Annotated log",
              class = "btn-sm btn-light"
            )
          )
        } else {
          htmltools::div(
            class = "alert alert-success",
            "No problems found. Go on to the Apply screen."
          )
        }
      )
    })

    output$problems <- DT::renderDT({
      ev <- store$evaluation
      shiny::req(ev)
      ev$severity <- ifelse(
        ev$issue_type %in% .blocking_issues,
        "must fix",
        "check"
      )
      ev <- ev[
        order(ev$severity != "must fix"),
        c("severity", setdiff(names(ev), "severity")),
        drop = FALSE
      ]
      dt_table(ev, page_length = 25) |>
        DT::formatStyle(
          "severity",
          target = "row",
          backgroundColor = DT::styleEqual(
            c("must fix", "check"),
            c("#F8D7DA", "#FFF4CC")
          )
        )
    })

    output$actions <- DT::renderDT({
      shiny::req(store$reviewed_log)
      tbl <- action_counts(
        store$reviewed_log,
        store$profile$clean$log_action_col
      )
      DT::datatable(
        tbl,
        rownames = FALSE,
        colnames = c("Action taken", "Rows"),
        selection = "none",
        options = list(dom = "t", pageLength = 50)
      )
    })

    output$log <- DT::renderDT({
      shiny::req(store$reviewed_log)
      dt_table(store$reviewed_log, page_length = 25)
    })

    output$file_check <- shiny::renderUI({
      checks <- store$reviewed_checks
      shiny::req(checks)
      show <- function(tbl, sheet) {
        if (is.null(tbl)) {
          return(htmltools::p(
            status_icon("warn"),
            " No ",
            htmltools::code(sheet),
            " sheet found."
          ))
        }
        htmltools::tagList(
          htmltools::h6(htmltools::code(sheet)),
          htmltools::tags$ul(
            class = "ck-status-list mb-3",
            lapply(seq_len(nrow(tbl)), function(i) {
              ok <- isTRUE(tbl$matches_reference[i]) &&
                identical(tbl$status[i], "ok")
              htmltools::tags$li(
                status_icon(if (ok) "done" else "error"),
                htmltools::span(class = "ck-path", tbl$file[i]),
                htmltools::span(
                  class = "ck-status-detail",
                  if (ok) {
                    paste0(tbl$n_columns[i], " columns")
                  } else {
                    paste0(
                      tbl$status[i],
                      if (nzchar(tbl$missing[i] %||% "")) {
                        paste0("; missing: ", tbl$missing[i])
                      },
                      if (nzchar(tbl$extra[i] %||% "")) {
                        paste0("; extra: ", tbl$extra[i])
                      }
                    )
                  }
                )
              )
            })
          )
        )
      }
      htmltools::tagList(
        show(checks$cleaning_log, "cleaning_log"),
        show(checks$other_responses, "other_responses")
      )
    })

    output$annotated <- shiny::downloadHandler(
      filename = function() {
        paste0(format(Sys.Date()), "_cleaning_log_problems.xlsx")
      },
      content = function(file) {
        write_annotated_log(store$reviewed_log, store$evaluation, file)
      }
    )
  })
}
