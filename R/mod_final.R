#' Final output screen: compare raw and clean, then write the final workbook
#'
#' Replaces `review_cleaned_data()` and `export_final_output()` in example
#' script 2.
#'
#' @noRd
mod_final_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      title = "Final workbook",
      shiny::textInput(
        ns("stem"),
        "File name",
        value = default_review_stem("_4Mi_final_output")
      ),
      shiny::textInput(ns("project_name"), "Project name"),
      shiny::textInput(ns("data_collection_round"), "Data collection round"),
      shiny::textInput(ns("prepared_by"), "Prepared by"),
      shiny::textAreaInput(ns("project_description"), "Description", rows = 3),
      shiny::actionButton(
        ns("export"),
        "Write final workbook",
        icon = shiny::icon("file-export"),
        class = "btn-primary w-100"
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Raw against clean",
        shiny::actionButton(
          ns("check"),
          "Compare again",
          icon = shiny::icon("rotate"),
          class = "btn-sm btn-outline-primary"
        )
      ),
      DT::DTOutput(ns("check_table"))
    )
  )
}

#' @noRd
mod_final_server <- function(id, store, hosted = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    fields <- c(
      "project_name",
      "data_collection_round",
      "prepared_by",
      "project_description"
    )

    shiny::observeEvent(store$project_dir, {
      f <- store$profile$final
      shiny::updateTextInput(
        session,
        "stem",
        value = default_review_stem(f$file_suffix)
      )
      for (name in fields) {
        shiny::updateTextInput(session, name, value = f[[name]] %||% "")
      }
    })

    run_check_clean <- function() {
      res <- store$clean_result
      shiny::req(res)
      raw <- store$raw_data
      profile <- store$profile
      out <- run_step(
        store,
        function() check_clean_data(raw, res, profile$clean, profile$general),
        step = "Compare raw and clean",
        dir = store$project_dir
      )
      store$final_check <- out %||% NULL
    }

    # Compare as soon as a new clean dataset arrives.
    shiny::observeEvent(
      store$clean_result,
      run_check_clean(),
      ignoreNULL = TRUE
    )
    shiny::observeEvent(input$check, run_check_clean())

    target <- shiny::reactive({
      shiny::req(store$project_dir)
      name <- tryCatch(final_file_name(input$stem), error = function(e) NULL)
      shiny::req(name)
      file.path(store$project_dir, "output", "final", name)
    })

    shiny::observeEvent(input$export, {
      if (is.null(store$clean_result)) {
        shiny::showNotification(
          "Apply the reviewed log first.",
          type = "warning"
        )
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
            " already exists in output/final/. Close it in Excel first."
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
        export()
      }
    })

    shiny::observeEvent(input$confirm_replace, {
      shiny::removeModal()
      export()
    })

    export <- function() {
      profile <- store$profile
      for (name in fields) {
        profile$final[[name]] <- input[[name]] %||% ""
      }
      save_profile(store, profile, notify = FALSE)
      path <- target()
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      raw <- store$raw_data
      res <- store$clean_result
      final <- profile$final
      out <- run_step(
        store,
        function() export_final(raw, res, final, path),
        step = "Write final workbook",
        dir = store$project_dir
      )
      shiny::req(out)
      store$final_path <- path
      shiny::showNotification(
        paste0("Saved ", basename(path)),
        type = "message"
      )
    }

    output$summary <- shiny::renderUI({
      res <- store$clean_result
      if (is.null(res)) {
        return(htmltools::div(
          class = "alert alert-secondary",
          status_icon("todo"),
          " Apply the reviewed log on the Apply screen first."
        ))
      }
      ck <- store$final_check
      split <- split_check_rows(ck)
      htmltools::tagList(
        if (is.null(ck)) {
          NULL
        } else if (nrow(split$tied) == 0) {
          htmltools::div(
            class = "alert alert-success",
            status_icon("done"),
            " The clean data and the final log agree: every change is in the log",
            " and every logged change is in the data.",
            if (nrow(split$untied) > 0) {
              htmltools::div(
                class = "ck-status-detail mt-1",
                nrow(split$untied),
                " difference(s) could not be tied to a survey (uuid is empty).",
                " review_cleaned_data() reported them without a uuid; they are listed below."
              )
            }
          )
        } else {
          htmltools::div(
            class = "alert alert-warning",
            status_icon("warn"),
            " ",
            htmltools::strong(nrow(split$tied)),
            " difference(s) between the clean data and the final log. See the table below."
          )
        },
        if (!is.null(store$final_path)) {
          bslib::card(
            class = "border-success",
            bslib::card_body(
              htmltools::p(
                status_icon("done"),
                " Saved ",
                htmltools::span(
                  class = "ck-path",
                  relative_to(store$final_path, store$project_dir)
                )
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
        }
      )
    })

    output$check_table <- DT::renderDT({
      ck <- store$final_check
      shiny::req(ck)
      if (nrow(ck) == 0) {
        return(DT::datatable(
          data.frame(Result = "No differences."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      dt_table(ck, page_length = 25)
    })

    output$download <- shiny::downloadHandler(
      filename = function() basename(store$final_path),
      content = function(file) {
        file.copy(store$final_path, file, overwrite = TRUE)
      }
    )

    if (!hosted) {
      shiny::observeEvent(input$open_folder, {
        shiny::req(store$final_path)
        open_folder(dirname(store$final_path))
      })
    }
  })
}
