#' Tool screen: load the XLSForm
#'
#' Replaces the "Read tool data" and "create other response db" blocks of the
#' example scripts.
#'
#' @noRd
mod_tool_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      title = "XLSForm",
      shiny::uiOutput(ns("tool_file")),
      shiny::fileInput(
        ns("upload"),
        "Upload a new XLSForm",
        accept = c(".xlsx", ".xls"),
        buttonLabel = "Choose...",
        placeholder = "copied into resources/"
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Advanced",
          icon = shiny::icon("sliders"),
          shiny::selectInput(ns("survey_sheet"), "Survey sheet", "survey"),
          shiny::selectInput(ns("choices_sheet"), "Choices sheet", "choices"),
          shiny::selectizeInput(
            ns("other_text_types"),
            "Extra \"other\" text questions",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = "e.g. Q31_2"
            )
          ),
          htmltools::p(
            class = "ck-hint",
            "Only needed when an \"other, specify\" question is missing from ",
            "the list on the right."
          )
        )
      ),
      shiny::selectInput(ns("language"), "Label language", "English"),
      shiny::actionButton(
        ns("load"),
        "Load tool",
        icon = shiny::icon("file-import"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("\"Other\" questions", DT::DTOutput(ns("other_labels"))),
      bslib::nav_panel("Other responses DB", DT::DTOutput(ns("other_db"))),
      bslib::nav_panel("Survey", DT::DTOutput(ns("survey"))),
      bslib::nav_panel("Choices", DT::DTOutput(ns("choices"))),
      bslib::nav_panel("Constraint check", shiny::uiOutput(ns("constraints")))
    )
  )
}

#' @noRd
mod_tool_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    tool_path <- shiny::reactive({
      project_file(store$project_dir, store$profile$tool$path)
    })
    # bumped when a new file is uploaded so the file-dependent UI refreshes
    file_changed <- shiny::reactiveVal(0)

    # fill the inputs from the profile when a project is opened
    shiny::observeEvent(store$project_dir, {
      p <- store$profile
      shiny::updateSelectizeInput(
        session,
        "other_text_types",
        choices = p$tool$other_text_types,
        selected = p$tool$other_text_types
      )
      file_changed(file_changed() + 1)
    })

    # sheet and language choices follow the file
    shiny::observe({
      file_changed()
      path <- tool_path()
      p <- shiny::isolate(store$profile)
      sheets <- if (!is.null(path) && file.exists(path)) {
        excel_sheets_safe(path)
      } else {
        character(0)
      }
      pick <- function(wanted) if (wanted %in% sheets) wanted else sheets[1]
      shiny::updateSelectInput(
        session,
        "survey_sheet",
        choices = union(sheets, p$tool$survey_sheet),
        selected = if (length(sheets)) {
          pick(p$tool$survey_sheet)
        } else {
          p$tool$survey_sheet
        }
      )
      shiny::updateSelectInput(
        session,
        "choices_sheet",
        choices = union(sheets, p$tool$choices_sheet),
        selected = if (length(sheets)) {
          pick(p$tool$choices_sheet)
        } else {
          p$tool$choices_sheet
        }
      )
      langs <- if (length(sheets)) {
        tool_languages(path, pick(p$tool$survey_sheet))
      } else {
        character(0)
      }
      lang <- p$general$label_language
      shiny::updateSelectInput(
        session,
        "language",
        choices = union(langs, lang),
        selected = if (lang %in% langs || !length(langs)) lang else langs[1]
      )
    })

    output$tool_file <- shiny::renderUI({
      file_changed()
      shiny::invalidateLater(5000)
      path <- tool_path()
      if (is.null(store$project_dir)) {
        return(htmltools::p(class = "ck-hint", "Open a project first."))
      }
      found <- !is.null(path) && file.exists(path)
      htmltools::p(
        status_icon(if (found) "done" else "error"),
        htmltools::span(class = "ck-path", store$profile$tool$path),
        if (!found) {
          htmltools::div(
            class = "ck-hint",
            "Not found: upload the XLSForm below."
          )
        }
      )
    })

    shiny::observeEvent(input$upload, {
      dir <- store$project_dir
      if (is.null(dir)) {
        shiny::showNotification(
          "Open a project folder first.",
          type = "warning"
        )
        return()
      }
      dest <- tryCatch(
        copy_uploads(input$upload, file.path(dir, "resources")),
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )
      shiny::req(dest)
      profile <- store$profile
      profile$tool$path <- file.path("resources", basename(dest))
      save_profile(store, profile, notify = FALSE)
      shiny::showNotification(
        paste0("Saved as resources/", basename(dest), ". Press Load tool."),
        type = "message"
      )
      file_changed(file_changed() + 1)
    })

    shiny::observeEvent(input$load, {
      dir <- store$project_dir
      path <- tool_path()
      if (is.null(dir)) {
        shiny::showNotification(
          "Open a project folder first.",
          type = "warning"
        )
        return()
      }
      profile <- store$profile
      profile$tool$survey_sheet <- input$survey_sheet
      profile$tool$choices_sheet <- input$choices_sheet
      profile$tool$other_text_types <- as.character(input$other_text_types)
      profile$general$label_language <- input$language
      save_profile(store, profile, notify = FALSE)

      res <- run_step(
        store,
        function() {
          load_tool(
            path,
            survey_sheet = profile$tool$survey_sheet,
            choices_sheet = profile$tool$choices_sheet,
            language = profile$general$label_language,
            other_text_types = profile$tool$other_text_types
          )
        },
        step = "Load tool",
        dir = dir
      )
      shiny::req(res)
      for (slot in names(res)) {
        store[[slot]] <- res[[slot]]
      }
      # the data was read against the old tool's date columns, and the
      # checks ran on that data
      reset_from(store, "data")
      shiny::updateSelectizeInput(
        session,
        "other_text_types",
        choices = union(
          text_questions(res$tool_survey),
          profile$tool$other_text_types
        ),
        selected = profile$tool$other_text_types,
        server = TRUE
      )
    })

    output$summary <- shiny::renderUI({
      if (is.null(store$tool_survey)) {
        return(bslib::card(
          bslib::card_body(
            class = "ck-hint",
            "Load the XLSForm to see what cleaningkit picked up from it."
          )
        ))
      }
      issues <- store$constraint_issues
      n_issues <- if (length(issues) == 1 && is.na(issues)) {
        NA
      } else {
        length(issues)
      }
      bslib::layout_column_wrap(
        width = 1 / 4,
        fill = FALSE,
        bslib::value_box(
          "Questions",
          nrow(store$tool_survey),
          showcase = shiny::icon("list-check"),
          theme = "primary"
        ),
        bslib::value_box(
          "Choice lists",
          length(unique(store$tool_choices$list_name)),
          showcase = shiny::icon("list-ul"),
          theme = "secondary"
        ),
        bslib::value_box(
          "\"Other\" questions",
          nrow(store$other_labels),
          showcase = shiny::icon("pen"),
          theme = "info"
        ),
        bslib::value_box(
          "Constraint issues",
          if (is.na(n_issues)) "?" else n_issues,
          showcase = shiny::icon(
            if (isTRUE(n_issues == 0)) {
              "circle-check"
            } else {
              "triangle-exclamation"
            }
          ),
          theme = if (isTRUE(n_issues == 0)) "success" else "warning"
        )
      )
    })

    output$other_labels <- DT::renderDT(dt_table(store$other_labels))
    output$other_db <- DT::renderDT(dt_table(store$other_db))
    output$survey <- DT::renderDT(dt_table(store$tool_survey))
    output$choices <- DT::renderDT(dt_table(store$tool_choices))

    output$constraints <- shiny::renderUI({
      issues <- store$constraint_issues
      if (is.null(store$tool_survey)) {
        return(NULL)
      }
      if (length(issues) == 1 && is.na(issues)) {
        return(htmltools::p(
          status_icon("warn"),
          "The constraint check could not run. See the Run log."
        ))
      }
      if (length(issues) == 0) {
        return(htmltools::p(
          status_icon("done"),
          "Every selected() condition in the relevance column points to a ",
          "choice that exists."
        ))
      }
      htmltools::tagList(
        htmltools::p(
          status_icon("warn"),
          length(issues),
          " selected() condition(s) refer to a choice that is not in the ",
          "question's choice list. These questions may never be shown:"
        ),
        htmltools::tags$ul(lapply(issues, function(x) {
          htmltools::tags$li(htmltools::code(x))
        }))
      )
    })
  })
}

#' A DT table with the app's defaults
#' @noRd
dt_table <- function(df, page_length = 15) {
  shiny::req(df)
  DT::datatable(
    as.data.frame(df),
    rownames = FALSE,
    filter = "top",
    selection = "none",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = "tip"
    )
  )
}
