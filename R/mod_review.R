#' Review workbook screen: the file reviewers fill in
#'
#' Replaces the `create_review_workbook()` block of example script 1. The
#' workbook is saved in the project's `output/follow_ups/` folder, where
#' script 2 (and the Clean half of the app, M3) reads the edited copy back.
#'
#' @noRd
mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      title = "Review workbook",
      shiny::textInput(ns("stem"), "File name", value = default_review_stem()),
      shiny::checkboxInput(
        ns("vba"),
        "Macro-enabled (.xlsm), with the cleaning-log buttons",
        TRUE
      ),
      shiny::checkboxInput(
        ns("include_other"),
        "Include the \"other\" responses sheet",
        TRUE
      ),
      shiny::checkboxInput(
        ns("include_dataset"),
        "Include the dataset sheet",
        TRUE
      ),
      shiny::checkboxInput(
        ns("flag_columns"),
        "Put the check flags in front of the dataset",
        TRUE
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Layout",
          icon = shiny::icon("sliders"),
          shiny::selectInput(
            ns("group_by"),
            "Group the cleaning log by",
            choices = c(
              "Issue" = "issue",
              "Survey" = "uuid",
              "Question" = "question",
              "No grouping" = "none"
            )
          ),
          shiny::selectInput(
            ns("color_mode"),
            "Row colours",
            choices = c(
              "Only the chosen columns" = "partial",
              "Whole row" = "on",
              "None" = "off"
            )
          ),
          shiny::selectizeInput(
            ns("color_columns"),
            "Coloured columns",
            choices = c("old_value", "new_value", "question", "issue", "uuid"),
            selected = "old_value",
            multiple = TRUE
          )
        )
      ),
      shiny::actionButton(
        ns("create"),
        "Create workbook",
        icon = shiny::icon("file-excel"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("result")),
    bslib::card(
      bslib::card_header("In output/follow_ups/"),
      DT::DTOutput(ns("files"))
    )
  )
}

#' @noRd
mod_review_server <- function(id, store, hosted = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh <- shiny::reactiveVal(0)

    shiny::observeEvent(store$project_dir, {
      r <- store$profile$review
      shiny::updateTextInput(
        session,
        "stem",
        value = default_review_stem(r$file_suffix)
      )
      shiny::updateCheckboxInput(session, "vba", value = r$vba)
      shiny::updateCheckboxInput(
        session,
        "include_other",
        value = r$include_other
      )
      shiny::updateCheckboxInput(
        session,
        "include_dataset",
        value = r$include_dataset
      )
      shiny::updateCheckboxInput(
        session,
        "flag_columns",
        value = r$flag_columns
      )
      shiny::updateSelectInput(session, "group_by", selected = r$group_by)
      shiny::updateSelectInput(session, "color_mode", selected = r$color_mode)
      shiny::updateSelectizeInput(
        session,
        "color_columns",
        selected = r$color_columns
      )
      refresh(refresh() + 1)
    })

    current_review <- function() {
      r <- store$profile$review
      r$vba <- isTRUE(input$vba)
      r$include_other <- isTRUE(input$include_other)
      r$include_dataset <- isTRUE(input$include_dataset)
      r$flag_columns <- isTRUE(input$flag_columns)
      r$group_by <- input$group_by
      r$color_mode <- input$color_mode
      r$color_columns <- as.character(input$color_columns)
      r
    }

    target <- shiny::reactive({
      shiny::req(store$project_dir)
      name <- tryCatch(
        review_file_name(input$stem, isTRUE(input$vba)),
        error = function(e) NULL
      )
      shiny::req(name)
      file.path(store$project_dir, "output", "follow_ups", name)
    })

    shiny::observeEvent(input$create, {
      if (is.null(store$combined_log)) {
        shiny::showNotification("Run the checks first.", type = "warning")
        return()
      }
      if (!nzchar(trimws(input$stem))) {
        shiny::showNotification(
          "Give the workbook a file name.",
          type = "warning"
        )
        return()
      }
      if (file.exists(target())) {
        shiny::showModal(shiny::modalDialog(
          title = "Replace the existing workbook?",
          htmltools::p(
            htmltools::code(basename(target())),
            " already exists in output/follow_ups/. If a reviewer has started ",
            "filling it in, their changes will be lost. Close it in Excel first."
          ),
          footer = htmltools::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              session$ns("confirm_replace"),
              "Replace",
              class = "btn-danger"
            )
          )
        ))
      } else {
        create()
      }
    })

    shiny::observeEvent(input$confirm_replace, {
      shiny::removeModal()
      create()
    })

    create <- function() {
      review <- current_review()
      profile <- store$profile
      profile$review <- review
      save_profile(store, profile, notify = FALSE)
      path <- target()
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      combined <- store$combined_log
      other <- store$other_responses
      other_db <- store$other_db
      general <- profile$general
      res <- run_step(
        store,
        function() {
          build_review_workbook(
            combined,
            path,
            review = review,
            general = general,
            other_responses = other,
            other_db = other_db
          )
        },
        step = "Create review workbook",
        dir = store$project_dir
      )
      refresh(refresh() + 1)
      shiny::req(res)
      store$review_path <- path
      shiny::showNotification(
        paste0("Saved ", basename(path)),
        type = "message"
      )
    }

    output$result <- shiny::renderUI({
      path <- store$review_path
      if (is.null(store$combined_log)) {
        return(bslib::card(bslib::card_body(
          class = "ck-hint",
          "Run the checks first. The workbook holds their cleaning log, the ",
          "\"other\" responses and the dataset."
        )))
      }
      n_rows <- nrow(store$combined_log$cleaning_log)
      if (is.null(path)) {
        return(bslib::card(bslib::card_body(
          htmltools::p(
            htmltools::strong(n_rows),
            " cleaning-log row(s)",
            if (!is.null(store$other_responses)) {
              paste0(
                " and ",
                nrow(store$other_responses),
                " \"other\" response(s)"
              )
            },
            " ready. Choose the options on the left and press Create workbook."
          )
        )))
      }
      bslib::card(
        class = "border-success",
        bslib::card_body(
          htmltools::p(
            status_icon("done"),
            " Saved ",
            htmltools::span(
              class = "ck-path",
              relative_to(path, store$project_dir)
            )
          ),
          htmltools::p(
            class = "ck-hint",
            "Send it to the reviewers. They fill in Action taken / New value ",
            "and save the edited copy with a name ending in ",
            htmltools::code("_edited"),
            if (hosted) {
              ". Download the workspace now if you are closing the tab; then upload the edited copy on the Import reviewed screen."
            } else {
              ", back in output/follow_ups/."
            }
          ),
          htmltools::div(
            class = "d-flex gap-2",
            shiny::downloadButton(
              session$ns("download"),
              "Download",
              class = "btn-primary"
            ),
            # hosted, the folder is on the server, not the user's computer
            if (!hosted) {
              shiny::actionButton(
                session$ns("open_folder"),
                "Show in folder",
                icon = shiny::icon("folder-open"),
                class = "btn-outline-primary"
              )
            }
          )
        )
      )
    })

    output$download <- shiny::downloadHandler(
      filename = function() basename(store$review_path),
      content = function(file) {
        file.copy(store$review_path, file, overwrite = TRUE)
      }
    )

    if (!hosted) {
      shiny::observeEvent(input$open_folder, {
        shiny::req(store$review_path)
        open_folder(dirname(store$review_path))
      })
    }

    output$files <- DT::renderDT({
      refresh()
      shiny::req(store$project_dir)
      shiny::invalidateLater(10000)
      DT::datatable(
        review_files(store$project_dir),
        rownames = FALSE,
        colnames = c("File", "Modified"),
        selection = "none",
        options = list(dom = "t", pageLength = 50)
      )
    })
  })
}
