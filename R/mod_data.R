#' Data screen: bring in the raw export
#'
#' Replaces the "filter read raw data" and "Read raw data" blocks of example
#' script 1.
#'
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      title = "Raw data",
      shiny::uiOutput(ns("files")),
      shiny::fileInput(
        ns("upload"),
        "Add ONA export(s) to data/",
        accept = c(".xlsx", ".xls"),
        multiple = TRUE,
        buttonLabel = "Choose...",
        placeholder = "copied into data/"
      ),
      shiny::checkboxInput(
        ns("filter_new"),
        "Keep only records not validated in an earlier round",
        TRUE
      ),
      shiny::conditionalPanel(
        "!input.filter_new",
        ns = ns,
        shiny::selectInput(ns("file"), "File to read", choices = NULL)
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Advanced",
          icon = shiny::icon("sliders"),
          shiny::conditionalPanel(
            "input.filter_new",
            ns = ns,
            shiny::checkboxInput(
              ns("use_ledger"),
              "Use the ledger of processed surveys (processed_uuids.csv)",
              TRUE
            ),
            shiny::checkboxInput(
              ns("archive"),
              "Move the input files to data/archive/ (instead of deleting them)",
              TRUE
            ),
            shiny::textInput(
              ns("output_name"),
              "Name of the filtered file",
              "data.xlsx"
            )
          ),
          shiny::numericInput(
            ns("sheet"),
            "Sheet number in the export",
            1,
            min = 1,
            step = 1
          ),
          shiny::selectizeInput(
            ns("extra_date_cols"),
            "Extra date columns",
            choices = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          ),
          shiny::selectizeInput(
            ns("extra_datetime_cols"),
            "Extra date-time columns",
            choices = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          ),
          htmltools::p(
            class = "ck-hint",
            "Date columns are found from the XLSForm. Add any it misses."
          )
        )
      ),
      shiny::actionButton(
        ns("prepare"),
        "Prepare data",
        icon = shiny::icon("table"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Checks", shiny::uiOutput(ns("column_checks"))),
      bslib::nav_panel("Per enumerator", DT::DTOutput(ns("by_enumerator"))),
      bslib::nav_panel("Per date", DT::DTOutput(ns("by_date"))),
      bslib::nav_panel("Preview", DT::DTOutput(ns("preview")))
    )
  )
}

#' @noRd
mod_data_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    data_dir <- shiny::reactive({
      shiny::req(store$project_dir)
      file.path(store$project_dir, "data")
    })
    refresh <- shiny::reactiveVal(0)
    files <- shiny::reactive({
      refresh()
      # notice exports copied into data/ outside the app (e.g. in Explorer)
      shiny::invalidateLater(5000)
      data_files(data_dir())
    })

    shiny::observeEvent(store$project_dir, {
      d <- store$profile$data
      shiny::updateCheckboxInput(session, "filter_new", value = d$filter_new)
      shiny::updateCheckboxInput(session, "use_ledger", value = d$use_ledger)
      shiny::updateCheckboxInput(session, "archive", value = d$archive)
      shiny::updateTextInput(session, "output_name", value = d$output_name)
      shiny::updateNumericInput(session, "sheet", value = d$sheet)
      shiny::updateSelectizeInput(
        session,
        "extra_date_cols",
        choices = d$extra_date_cols,
        selected = d$extra_date_cols
      )
      shiny::updateSelectizeInput(
        session,
        "extra_datetime_cols",
        choices = d$extra_datetime_cols,
        selected = d$extra_datetime_cols
      )
      refresh(refresh() + 1)
    })

    shiny::observe({
      f <- files()$file
      selected <- shiny::isolate(input$file)
      shiny::updateSelectInput(
        session,
        "file",
        choices = f,
        selected = if (!is.null(selected) && selected %in% f) selected else f[1]
      )
    })

    output$files <- shiny::renderUI({
      if (is.null(store$project_dir)) {
        return(htmltools::p(class = "ck-hint", "Open a project first."))
      }
      f <- files()
      n_ledger <- ledger_count(data_dir())
      items <- if (nrow(f) == 0) {
        list(htmltools::tags$li(
          status_icon("todo"),
          "No Excel files in data/ yet."
        ))
      } else {
        lapply(seq_len(nrow(f)), function(i) {
          htmltools::tags$li(
            shiny::icon("file-excel", class = "text-success"),
            htmltools::span(class = "ck-path", f$file[i]),
            htmltools::span(
              class = "ck-status-detail",
              paste0(f$size_kb[i], " KB, ", f$modified[i])
            )
          )
        })
      }
      too_many <- isTRUE(input$filter_new) && nrow(f) > 2
      htmltools::tagList(
        htmltools::tags$ul(class = "ck-status-list mb-2", items),
        if (too_many) {
          htmltools::div(
            class = "alert alert-warning py-2 small",
            "Filtering compares at most 2 files: the new export and last ",
            "round's file. Move the others out of data/."
          )
        },
        htmltools::p(
          class = "ck-hint",
          if (is.na(n_ledger)) {
            "No ledger yet: this is treated as the first round."
          } else {
            paste0(n_ledger, " survey(s) already validated in earlier rounds.")
          }
        )
      )
    })

    shiny::observeEvent(input$upload, {
      shiny::req(store$project_dir)
      ok <- tryCatch(
        {
          copy_uploads(input$upload, data_dir())
          TRUE
        },
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = NULL
          )
          FALSE
        }
      )
      if (ok) {
        shiny::showNotification(
          paste0("Added ", nrow(input$upload), " file(s) to data/."),
          type = "message"
        )
      }
      refresh(refresh() + 1)
    })

    current_settings <- function() {
      d <- store$profile$data
      d$filter_new <- isTRUE(input$filter_new)
      d$use_ledger <- isTRUE(input$use_ledger)
      d$archive <- isTRUE(input$archive)
      d$output_name <- trimws(input$output_name)
      d$sheet <- as.integer(input$sheet %||% 1L)
      d$extra_date_cols <- as.character(input$extra_date_cols)
      d$extra_datetime_cols <- as.character(input$extra_datetime_cols)
      d
    }

    shiny::observeEvent(input$prepare, {
      if (is.null(store$project_dir)) {
        shiny::showNotification(
          "Open a project folder first.",
          type = "warning"
        )
        return()
      }
      if (is.null(store$tool_survey)) {
        shiny::showNotification(
          "Load the XLSForm on the Tool screen first: it tells cleaningkit which columns are dates.",
          type = "warning"
        )
        return()
      }
      if (nrow(files()) == 0) {
        shiny::showNotification(
          "There are no Excel files in data/ yet.",
          type = "warning"
        )
        return()
      }
      if (isTRUE(input$filter_new)) {
        shiny::showModal(confirm_filter_modal(
          session$ns,
          current_settings(),
          files()
        ))
      } else {
        do_prepare()
      }
    })

    shiny::observeEvent(input$confirm_filter, {
      shiny::removeModal()
      do_prepare()
    })

    do_prepare <- function() {
      dir <- store$project_dir
      settings <- current_settings()
      profile <- store$profile
      profile$data <- settings
      save_profile(store, profile, notify = FALSE)
      general <- profile$general
      tool_survey <- store$tool_survey
      file <- input$file
      ddir <- data_dir()

      res <- run_step(
        store,
        function() {
          prepare_data(
            data_dir = ddir,
            tool_survey = tool_survey,
            filter_new = settings$filter_new,
            file = file,
            uuid_column = general$uuid_column,
            skip_label_row = general$skip_label_row,
            sheet = settings$sheet,
            output_name = settings$output_name,
            use_ledger = settings$use_ledger,
            archive = settings$archive,
            extra_date_cols = settings$extra_date_cols,
            extra_datetime_cols = settings$extra_datetime_cols
          )
        },
        step = "Prepare data",
        dir = dir
      )
      refresh(refresh() + 1)
      shiny::req(res)
      store$raw_data <- res$raw_data
      store$raw_data_path <- res$path
      store$filter_summary <- res$filter_summary
    }

    general <- shiny::reactive(store$profile$general)

    output$summary <- shiny::renderUI({
      df <- store$raw_data
      if (is.null(df)) {
        return(bslib::card(bslib::card_body(
          class = "ck-hint",
          "Add the ONA export to data/ and press Prepare data."
        )))
      }
      g <- general()
      body <- data_body(df, g$skip_label_row)
      n_enum <- if (g$enumerator_column %in% names(body)) {
        length(unique(stats::na.omit(body[[g$enumerator_column]])))
      } else {
        "?"
      }
      rng <- if (g$date_column %in% names(body)) {
        date_range(body[[g$date_column]])
      }
      fs <- store$filter_summary
      htmltools::tagList(
        bslib::layout_column_wrap(
          width = 1 / 4,
          fill = FALSE,
          bslib::value_box(
            "Records",
            nrow(body),
            showcase = shiny::icon("users"),
            theme = "primary"
          ),
          bslib::value_box(
            "Columns",
            ncol(body),
            showcase = shiny::icon("table-columns"),
            theme = "secondary"
          ),
          bslib::value_box(
            "Enumerators",
            n_enum,
            showcase = shiny::icon("user-pen"),
            theme = "info"
          ),
          bslib::value_box(
            "Interview dates",
            if (is.null(rng)) {
              "?"
            } else {
              htmltools::span(
                style = "font-size:1.1rem",
                paste(rng, collapse = " to ")
              )
            },
            showcase = shiny::icon("calendar"),
            theme = "light"
          )
        ),
        htmltools::p(
          class = "ck-hint",
          htmltools::HTML(paste0(
            "Read from <span class=\"ck-path\">",
            htmltools::htmlEscape(relative_to(
              store$raw_data_path,
              store$project_dir
            )),
            "</span>",
            if (!is.null(fs)) {
              htmltools::htmlEscape(filter_summary_text(fs))
            } else {
              "."
            }
          ))
        )
      )
    })

    output$column_checks <- shiny::renderUI({
      shiny::req(store$raw_data)
      checks <- data_column_checks(store$raw_data, general())
      htmltools::tags$ul(
        class = "ck-status-list",
        lapply(seq_len(nrow(checks)), function(i) {
          htmltools::tags$li(
            status_icon(checks$state[i]),
            htmltools::strong(checks$item[i]),
            htmltools::span(checks$detail[i])
          )
        })
      )
    })

    output$by_enumerator <- DT::renderDT({
      shiny::req(store$raw_data)
      dt_table(count_by(
        store$raw_data,
        general()$enumerator_column,
        general()$skip_label_row
      ))
    })
    output$by_date <- DT::renderDT({
      shiny::req(store$raw_data)
      g <- general()
      counts <- count_by(store$raw_data, g$date_column, g$skip_label_row)
      shiny::req(counts)
      counts <- counts[order(counts[[1]]), , drop = FALSE]
      dt_table(counts)
    })
    output$preview <- DT::renderDT({
      shiny::req(store$raw_data)
      dt_table(utils::head(store$raw_data, 200), page_length = 10)
    })
  })
}

#' Confirmation shown before `filter_new_records()` moves files
#' @noRd
confirm_filter_modal <- function(ns, settings, files) {
  shiny::modalDialog(
    title = "Keep only new records?",
    htmltools::p("cleaningkit will:"),
    htmltools::tags$ol(
      htmltools::tags$li(paste0(
        "compare ",
        paste(files$file, collapse = " and "),
        if (settings$use_ledger) " and the ledger of processed surveys",
        ";"
      )),
      htmltools::tags$li(htmltools::HTML(paste0(
        "save the records not validated before as <code>data/",
        htmltools::htmlEscape(settings$output_name),
        "</code>;"
      ))),
      htmltools::tags$li(
        if (settings$archive) {
          "move the input files to data/archive/;"
        } else {
          htmltools::strong("delete the input files;")
        }
      ),
      if (settings$use_ledger) {
        htmltools::tags$li("add the new records to processed_uuids.csv.")
      }
    ),
    htmltools::p(
      class = "ck-hint",
      "With a single file and no ledger (the first round) nothing is ",
      "filtered: the file is only renamed."
    ),
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        ns("confirm_filter"),
        "Continue",
        class = "btn-primary"
      )
    )
  )
}

#' One line describing what `filter_new_records()` did
#' @noRd
filter_summary_text <- function(fs) {
  if (!isTRUE(fs$filtered)) {
    return(paste0(
      ". First round: nothing filtered, ",
      fs$n_kept,
      " record(s) kept."
    ))
  }
  paste0(
    ". Filtered ",
    fs$new_export,
    ": ",
    fs$n_dropped,
    " record(s) already validated were dropped, ",
    fs$n_kept,
    " new record(s) kept."
  )
}

#' Path relative to the project, for display
#' @noRd
relative_to <- function(path, root) {
  if (is.null(path)) {
    return("")
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (startsWith(tolower(path), tolower(paste0(root, "/")))) {
    substring(path, nchar(root) + 2)
  } else {
    path
  }
}
