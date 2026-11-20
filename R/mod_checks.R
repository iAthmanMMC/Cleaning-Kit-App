#' Checks screen: configure and run the validate_*() checks
#'
#' Replaces the validate_*() blocks, `prepare_other_responses()` and
#' `create_combined_log()` of example script 1. One panel per entry of
#' `ck_checks()`.
#'
#' @noRd
mod_checks_ui <- function(id) {
  ns <- shiny::NS(id)
  checks <- ck_checks()
  settings <- default_check_settings(checks)

  panels <- lapply(names(checks), function(cid) {
    check <- checks[[cid]]
    bslib::accordion_panel(
      title = htmltools::span(
        check$label,
        shiny::uiOutput(ns(paste0(cid, "__badge")), inline = TRUE)
      ),
      value = cid,
      bslib::input_switch(
        ns(enabled_input_id(cid)),
        "Run this check",
        value = settings[[cid]]$enabled
      ),
      htmltools::p(class = "ck-hint", check$description),
      shiny::uiOutput(ns(paste0(cid, "__status"))),
      bslib::layout_column_wrap(
        width = "260px",
        fill = FALSE,
        !!!lapply(names(check$args), function(nm) {
          arg_input(
            ns(arg_input_id(cid, nm)),
            check$args[[nm]],
            settings[[cid]]$args[[nm]]
          )
        })
      )
    )
  })

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      title = "Run checks",
      shiny::uiOutput(ns("ready")),
      shiny::actionButton(
        ns("run"),
        "Run selected checks",
        icon = shiny::icon("play"),
        class = "btn-primary w-100"
      ),
      htmltools::div(
        class = "d-flex gap-2",
        shiny::actionButton(
          ns("all"),
          "Select all",
          class = "btn-sm btn-outline-secondary flex-fill"
        ),
        shiny::actionButton(
          ns("none"),
          "Select none",
          class = "btn-sm btn-outline-secondary flex-fill"
        )
      ),
      shiny::uiOutput(ns("last_run"))
    ),
    bslib::accordion(
      id = ns("panels"),
      open = FALSE,
      multiple = TRUE,
      !!!panels
    )
  )
}

#' @noRd
mod_checks_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    checks <- ck_checks()

    # settings in the profile -> inputs, when a project is opened
    shiny::observeEvent(store$project_dir, {
      fill_inputs(store$profile$checks)
    })

    # offer the data's columns in every column picker
    shiny::observeEvent(store$raw_data, {
      fill_inputs(
        read_check_inputs(input, store$profile$checks, checks),
        columns = names(store$raw_data)
      )
    })

    fill_inputs <- function(settings, columns = names(store$raw_data)) {
      for (cid in names(checks)) {
        bslib::update_switch(
          enabled_input_id(cid),
          value = settings[[cid]]$enabled,
          session = session
        )
        for (nm in names(checks[[cid]]$args)) {
          update_arg_input(
            session,
            arg_input_id(cid, nm),
            checks[[cid]]$args[[nm]],
            settings[[cid]]$args[[nm]],
            columns = columns
          )
        }
      }
    }

    set_all <- function(value) {
      for (cid in names(checks)) {
        bslib::update_switch(
          enabled_input_id(cid),
          value = value,
          session = session
        )
      }
    }
    shiny::observeEvent(input$all, set_all(TRUE))
    shiny::observeEvent(input$none, set_all(FALSE))

    ctx <- shiny::reactive({
      check_context(
        raw_data = store$raw_data,
        tool_survey = store$tool_survey,
        tool_choices = store$tool_choices,
        other_db = store$other_db,
        general = store$profile$general,
        project_dir = store$project_dir
      )
    })

    output$ready <- shiny::renderUI({
      c <- ctx()
      items <- list(
        list("Data", !is.null(c$raw_data)),
        list("XLSForm", !is.null(c$tool_survey))
      )
      htmltools::tags$ul(
        class = "ck-status-list mb-3",
        lapply(items, function(x) {
          htmltools::tags$li(
            status_icon(if (x[[2]]) "done" else "error"),
            x[[1]],
            if (!x[[2]]) {
              htmltools::span(class = "ck-status-detail", "not loaded")
            }
          )
        })
      )
    })

    shiny::observeEvent(input$run, {
      c <- ctx()
      if (is.null(c$raw_data)) {
        shiny::showNotification(
          "Prepare the data on the Data screen first.",
          type = "warning"
        )
        return()
      }
      settings <- read_check_inputs(input, store$profile$checks, checks)
      profile <- store$profile
      profile$checks <- settings
      save_profile(store, profile, notify = FALSE)

      selected <- names(checks)[vapply(
        names(checks),
        function(cid) isTRUE(settings[[cid]]$enabled),
        logical(1)
      )]
      if (length(selected) == 0) {
        shiny::showNotification("No check is selected.", type = "warning")
        return()
      }

      outcome <- run_selected_checks(store, checks[selected], settings, c)
      reset_from(store, "checks")
      store$check_results <- outcome$results
      store$logs <- outcome$logs
      store$other_responses <- outcome$other_responses

      if (length(outcome$logs)) {
        store$combined_log <- run_step(
          store,
          function() {
            cleaningkit::create_combined_log(
              c(list(checked_dataset = c$raw_data), outcome$logs)
            )
          },
          step = "Combine logs",
          notify = TRUE
        )
      }
      notify_run(outcome$results, checks)
    })

    # one badge and one status line per check
    lapply(names(checks), function(cid) {
      output[[paste0(cid, "__badge")]] <- shiny::renderUI({
        check_badge(store$check_results[[cid]], checks[[cid]]$kind)
      })
      output[[paste0(cid, "__status")]] <- shiny::renderUI({
        r <- store$check_results[[cid]]
        if (is.null(r) || r$status != "error") {
          return(NULL)
        }
        htmltools::div(
          class = "alert alert-danger py-2 small",
          htmltools::strong("Last run failed: "),
          r$error
        )
      })
    })

    output$last_run <- shiny::renderUI({
      res <- store$check_results
      if (length(res) == 0) {
        return(htmltools::p(class = "ck-hint mt-3", "No checks run yet."))
      }
      n_flags <- sum(vapply(
        res,
        function(r) if (r$status == "done" && r$kind == "log") r$n else 0L,
        integer(1)
      ))
      n_failed <- sum(vapply(res, function(r) r$status == "error", logical(1)))
      htmltools::div(
        class = "mt-3",
        htmltools::p(
          htmltools::strong(n_flags),
          " flag(s) from ",
          length(res) - n_failed,
          " check(s)",
          htmltools::br(),
          htmltools::span(class = "ck-hint", "Run at ", res[[1]]$time)
        ),
        if (n_failed) {
          htmltools::p(
            class = "text-danger small",
            n_failed,
            " check(s) failed: open their panel to see why."
          )
        }
      )
    })
  })
}

#' Run several checks, each isolated so one failure does not stop the rest
#'
#' @return A list with `results` (per check id: `status`, `kind`, `n`,
#'   `error`, `time`), `logs` (per log name) and `other_responses`.
#' @noRd
run_selected_checks <- function(store, checks, settings, ctx) {
  results <- list()
  logs <- list()
  other_responses <- NULL
  time <- format(Sys.time(), "%H:%M")
  for (cid in names(checks)) {
    check <- checks[[cid]]
    res <- run_step(
      store,
      function() run_check(check, settings[[cid]]$args, ctx),
      step = check$label,
      dir = ctx$project_dir,
      notify = FALSE,
      full = TRUE
    )
    ok <- is.null(res$error)
    results[[cid]] <- list(
      status = if (ok) "done" else "error",
      kind = check$kind,
      n = if (ok && is.data.frame(res$value)) nrow(res$value) else 0L,
      error = res$error,
      time = time
    )
    if (ok && check$kind == "log") {
      logs[[check_log_name(check)]] <- res$value
    }
    if (ok && check$kind == "other") {
      other_responses <- res$value
    }
  }
  list(results = results, logs = logs, other_responses = other_responses)
}

#' Badge next to a check's name
#' @noRd
check_badge <- function(result, kind = "log") {
  if (is.null(result)) {
    return(NULL)
  }
  if (result$status == "error") {
    return(htmltools::span(class = "badge text-bg-danger ms-2", "failed"))
  }
  noun <- if (kind == "other") "response" else "flag"
  htmltools::span(
    class = paste(
      "badge ms-2",
      if (result$n > 0) "text-bg-warning" else "text-bg-success"
    ),
    paste0(result$n, " ", noun, if (result$n != 1) "s")
  )
}

#' One notification summing up a run
#' @noRd
notify_run <- function(results, checks) {
  failed <- names(results)[vapply(
    results,
    function(r) r$status == "error",
    logical(1)
  )]
  n_flags <- sum(vapply(
    results,
    function(r) if (r$kind == "log") r$n else 0L,
    integer(1)
  ))
  msg <- paste0(
    length(results) - length(failed),
    " check(s) ran, ",
    n_flags,
    " flag(s)."
  )
  if (length(failed)) {
    msg <- paste0(
      msg,
      " Failed: ",
      paste(vapply(checks[failed], `[[`, "", "label"), collapse = ", "),
      "."
    )
  }
  shiny::showNotification(
    msg,
    type = if (length(failed)) "warning" else "message",
    duration = if (length(failed)) NULL else 6
  )
}
