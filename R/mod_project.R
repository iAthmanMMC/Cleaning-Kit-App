#' Project screen: choose the project folder and the shared settings
#'
#' Replaces the top of example script 1: `setup_project_folders()` and the
#' column names that every later call repeats (`uuid_column`,
#' `enumerator_column`, `date_column`, `skip_label_row`).
#'
#' @noRd
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("Project folder"),
      htmltools::p(
        class = "ck-hint",
        "One folder per survey round. cleaningkit keeps the raw exports in ",
        htmltools::code("data/"),
        ", the XLSForm and checklists in ",
        htmltools::code("resources/"),
        " and everything it produces in ",
        htmltools::code("output/"),
        "."
      ),
      htmltools::div(
        class = "d-flex gap-2 align-items-end",
        htmltools::div(
          class = "flex-grow-1",
          shiny::textInput(
            ns("path"),
            "Folder path",
            placeholder = "C:/Users/me/4Mi/round_12",
            width = "100%"
          )
        ),
        htmltools::div(
          class = "mb-3",
          shinyFiles::shinyDirButton(
            ns("browse"),
            "Browse...",
            title = "Choose the project folder",
            class = "btn-outline-primary"
          )
        ),
        htmltools::div(
          class = "mb-3",
          shiny::actionButton(ns("open"), "Open", class = "btn-primary")
        )
      ),
      shiny::uiOutput(ns("folder_status"))
    ),
    bslib::card(
      bslib::card_header("Settings used in every step"),
      htmltools::p(
        class = "ck-hint",
        "These are the column names in your ONA/Kobo export. They are saved ",
        "with the project, so you set them once."
      ),
      shiny::textInput(ns("uuid_column"), "Survey ID column", "_uuid"),
      shiny::textInput(
        ns("enumerator_column"),
        "Enumerator column",
        "username"
      ),
      shiny::textInput(ns("date_column"), "Interview date column", "today"),
      shiny::checkboxInput(
        ns("skip_label_row"),
        "The first data row holds the ONA question labels",
        TRUE
      ),
      shiny::actionButton(
        ns("save"),
        "Save settings",
        icon = shiny::icon("floppy-disk"),
        class = "btn-outline-primary"
      ),
      shiny::uiOutput(ns("profile_note"))
    )
  )
}

#' @param initial_dir Folder to open on start-up, or `NULL`.
#' @noRd
mod_project_server <- function(id, store, initial_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    volumes <- c(
      Home = normalizePath("~", winslash = "/"),
      shinyFiles::getVolumes()()
    )
    shinyFiles::shinyDirChoose(
      input,
      "browse",
      roots = volumes,
      session = session
    )

    # bumped after anything that changes the folders, so the status refreshes
    refresh <- shiny::reactiveVal(0)

    open_project <- function(path) {
      path <- clean_dir_path(path)
      if (!nzchar(path) || !dir.exists(path)) {
        shiny::showNotification(
          paste0("Folder not found: ", path),
          type = "error"
        )
        return(invisible(FALSE))
      }
      profile <- tryCatch(read_profile(path), error = function(e) {
        shiny::showNotification(
          paste0(
            "Could not read the saved settings, using defaults: ",
            conditionMessage(e)
          ),
          type = "warning",
          duration = NULL
        )
        default_profile()
      })
      if (!identical(path, store$project_dir)) {
        reset_project_state(store)
      }
      store$project_dir <- path
      store$profile <- profile
      shiny::updateTextInput(session, "path", value = path)
      fill_general(profile$general)
      refresh(refresh() + 1)
      store$log <- rbind(
        store$log,
        data.frame(
          time = format(Sys.time(), "%H:%M:%S"),
          step = "Open project",
          level = "info",
          text = paste0("Opened ", path),
          stringsAsFactors = FALSE
        )
      )
      invisible(TRUE)
    }

    fill_general <- function(general) {
      shiny::updateTextInput(
        session,
        "uuid_column",
        value = general$uuid_column
      )
      shiny::updateTextInput(
        session,
        "enumerator_column",
        value = general$enumerator_column
      )
      shiny::updateTextInput(
        session,
        "date_column",
        value = general$date_column
      )
      shiny::updateCheckboxInput(
        session,
        "skip_label_row",
        value = general$skip_label_row
      )
    }

    if (!is.null(initial_dir)) {
      shiny::observeEvent(TRUE, open_project(initial_dir), once = TRUE)
    }

    shiny::observeEvent(input$browse, {
      chosen <- shinyFiles::parseDirPath(volumes, input$browse)
      if (length(chosen) == 1 && nzchar(chosen)) {
        open_project(chosen)
      }
    })

    shiny::observeEvent(input$open, open_project(input$path))

    output$folder_status <- shiny::renderUI({
      refresh()
      dir <- store$project_dir
      if (is.null(dir)) {
        return(htmltools::p(class = "ck-hint", "No project open yet."))
      }
      status <- project_folder_status(dir)
      rows <- lapply(seq_len(nrow(status)), function(i) {
        htmltools::tags$li(
          status_icon(if (status$exists[i]) "done" else "error"),
          htmltools::code(status$folder[i]),
          htmltools::span(
            class = "ck-status-detail",
            if (status$exists[i]) {
              paste0(status$files[i], " file(s)")
            } else {
              "missing"
            }
          )
        )
      })
      missing <- !all(status$exists)
      htmltools::tagList(
        htmltools::p(
          htmltools::strong("Open: "),
          htmltools::span(class = "ck-path", dir)
        ),
        htmltools::tags$ul(class = "ck-status-list", rows),
        if (missing) {
          htmltools::div(
            class = "mt-3",
            htmltools::p(
              class = "ck-hint",
              "Some folders are missing. Set them up to use this folder as a ",
              "project. Existing files are never overwritten."
            ),
            shiny::checkboxInput(
              session$ns("copy_templates"),
              "Also copy the logical checklist template into resources/",
              TRUE
            ),
            shiny::actionButton(
              session$ns("setup"),
              "Set up project folders",
              icon = shiny::icon("folder-plus"),
              class = "btn-primary"
            )
          )
        }
      )
    })

    shiny::observeEvent(input$setup, {
      dir <- store$project_dir
      shiny::req(dir)
      copy_templates <- isTRUE(input$copy_templates)
      run_step(
        store,
        function() {
          cleaningkit::setup_project_folders(
            base_path = dir,
            copy_templates = copy_templates
          )
        },
        step = "Set up project folders",
        dir = dir
      )
      refresh(refresh() + 1)
    })

    shiny::observeEvent(input$save, {
      dir <- store$project_dir
      if (is.null(dir)) {
        shiny::showNotification(
          "Open a project folder first.",
          type = "warning"
        )
        return()
      }
      profile <- store$profile
      profile$general$uuid_column <- trimws(input$uuid_column)
      profile$general$enumerator_column <- trimws(input$enumerator_column)
      profile$general$date_column <- trimws(input$date_column)
      profile$general$skip_label_row <- isTRUE(input$skip_label_row)
      save_profile(store, profile)
    })

    output$profile_note <- shiny::renderUI({
      dir <- store$project_dir
      if (is.null(dir)) {
        return(NULL)
      }
      path <- profile_path(dir)
      htmltools::p(
        class = "ck-hint mt-2",
        if (file.exists(path)) "Saved in " else "Will be saved to ",
        htmltools::span(class = "ck-path", "resources/cleaningkitapp.yml")
      )
    })

    invisible(list(open_project = open_project))
  })
}

#' Put a profile in the store and write it to the project folder
#' @noRd
save_profile <- function(store, profile, notify = TRUE) {
  store$profile <- profile
  dir <- store$project_dir
  if (is.null(dir)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      write_profile(profile, dir)
      TRUE
    },
    error = function(e) {
      shiny::showNotification(
        paste0("Could not save settings: ", conditionMessage(e)),
        type = "error"
      )
      FALSE
    }
  )
  if (ok && notify) {
    shiny::showNotification("Settings saved.", type = "message", duration = 3)
  }
  invisible(ok)
}
