#' Apply screen: turn the reviewed logs into the clean dataset
#'
#' Replaces `apply_cleaning_log()`, `read_other_responses()`,
#' `apply_other_responses()` and `combine_reviewed_logs()` in example
#' script 2.
#'
#' @noRd
mod_apply_ui <- function(id) {
  ns <- shiny::NS(id)
  action_input <- function(name, label) {
    shiny::selectizeInput(
      ns(name),
      label,
      choices = NULL,
      multiple = TRUE,
      options = list(create = TRUE)
    )
  }
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      title = "Apply the reviewed log",
      shiny::checkboxInput(
        ns("apply_other"),
        "Apply the \"other\" responses sheet too",
        TRUE
      ),
      shiny::checkboxInput(
        ns("apply_repeated_entries"),
        "Apply a change to every repeat of the same entry",
        TRUE
      ),
      shiny::checkboxInput(
        ns("restore_types"),
        "Keep the original column types",
        TRUE
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "What each action means",
          icon = shiny::icon("sliders"),
          action_input("change_response_values", "Change the value"),
          action_input("blank_response_values", "Blank the value"),
          action_input("remove_survey_values", "Discard the survey"),
          action_input("no_action_values", "Leave as it is")
        )
      ),
      shiny::actionButton(
        ns("apply"),
        "Apply",
        icon = shiny::icon("wand-magic-sparkles"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Changes", DT::DTOutput(ns("audit"))),
      bslib::nav_panel("\"Other\" responses", DT::DTOutput(ns("other"))),
      bslib::nav_panel("Final log", DT::DTOutput(ns("final_log"))),
      bslib::nav_panel("Clean data", DT::DTOutput(ns("clean")))
    )
  )
}

#' @noRd
mod_apply_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    action_settings <- c(
      "change_response_values",
      "blank_response_values",
      "remove_survey_values",
      "no_action_values"
    )

    shiny::observeEvent(store$project_dir, {
      c <- store$profile$clean
      for (name in action_settings) {
        shiny::updateSelectizeInput(
          session,
          name,
          choices = union(c$valid_actions, c[[name]]),
          selected = c[[name]]
        )
      }
      for (name in c(
        "apply_other",
        "apply_repeated_entries",
        "restore_types"
      )) {
        shiny::updateCheckboxInput(session, name, value = isTRUE(c[[name]]))
      }
    })

    blocked <- shiny::reactive({
      if (is.null(store$reviewed_log)) {
        return(
          "Read the reviewed workbooks on the Import reviewed screen first."
        )
      }
      if (evaluation_counts(store$evaluation)$blocking > 0) {
        return(paste(
          "The reviewed log has rows that cannot be applied.",
          "Fix them in the workbook and read it again on the Import reviewed screen."
        ))
      }
      NULL
    })

    shiny::observeEvent(input$apply, {
      if (!is.null(blocked())) {
        shiny::showNotification(blocked(), type = "warning", duration = 8)
        return()
      }
      profile <- store$profile
      for (name in action_settings) {
        profile$clean[[name]] <- as.character(input[[name]])
      }
      profile$clean$apply_other <- isTRUE(input$apply_other)
      profile$clean$apply_repeated_entries <- isTRUE(
        input$apply_repeated_entries
      )
      profile$clean$restore_types <- isTRUE(input$restore_types)
      save_profile(store, profile, notify = FALSE)

      dir <- store$project_dir
      raw <- store$raw_data
      log <- store$reviewed_log
      choices <- store$tool_choices
      other_db <- store$other_db
      res <- run_step(
        store,
        function() {
          apply_reviewed(
            dir,
            raw,
            log,
            profile$clean,
            profile$general,
            tool_choices = choices,
            other_db = other_db
          )
        },
        step = "Apply cleaning log",
        dir = dir
      )
      reset_from(store, "apply")
      shiny::req(res)
      store$clean_result <- res
      shiny::showNotification(
        paste0(
          "Applied. ",
          res$n_after,
          " of ",
          res$n_before,
          " records kept."
        ),
        type = "message"
      )
    })

    output$summary <- shiny::renderUI({
      reason <- blocked()
      if (!is.null(reason)) {
        return(htmltools::div(
          class = "alert alert-secondary",
          status_icon("todo"),
          " ",
          reason
        ))
      }
      res <- store$clean_result
      if (is.null(res)) {
        return(bslib::card(bslib::card_body(
          htmltools::p(
            htmltools::strong(nrow(store$reviewed_log)),
            " reviewed log row(s) ready. Check the options on the left and press Apply."
          )
        )))
      }
      n_audit <- if (is.null(res$audit_log)) 0L else nrow(res$audit_log)
      n_other <- if (is.null(res$other_audit_log)) {
        0L
      } else {
        nrow(res$other_audit_log)
      }
      htmltools::tagList(
        bslib::layout_column_wrap(
          width = 1 / 4,
          fill = FALSE,
          bslib::value_box(
            "Records kept",
            res$n_after,
            htmltools::p(paste0("of ", res$n_before)),
            showcase = shiny::icon("users"),
            theme = "primary"
          ),
          bslib::value_box(
            "Discarded",
            res$n_before - res$n_after,
            showcase = shiny::icon("trash"),
            theme = "info"
          ),
          bslib::value_box(
            "Values changed",
            n_audit,
            showcase = shiny::icon("pen-to-square"),
            theme = "secondary"
          ),
          bslib::value_box(
            "\"Other\" changes",
            if (isTRUE(store$profile$clean$apply_other)) n_other else "off",
            showcase = shiny::icon("language"),
            theme = "secondary"
          )
        ),
        htmltools::div(
          class = "alert alert-success",
          "Done. Check the clean data and write the final workbook on the Final output screen."
        )
      )
    })

    output$audit <- DT::renderDT({
      shiny::req(store$clean_result$audit_log)
      dt_table(store$clean_result$audit_log, page_length = 25)
    })

    output$other <- DT::renderDT({
      shiny::req(store$clean_result$other_audit_log)
      dt_table(store$clean_result$other_audit_log, page_length = 25)
    })

    output$final_log <- DT::renderDT({
      shiny::req(store$clean_result$final_log)
      dt_table(store$clean_result$final_log, page_length = 25)
    })

    output$clean <- DT::renderDT({
      shiny::req(store$clean_result$clean_dataset)
      dt_table(
        data_body(
          store$clean_result$clean_dataset,
          store$profile$general$skip_label_row
        ),
        page_length = 15
      )
    })
  })
}
