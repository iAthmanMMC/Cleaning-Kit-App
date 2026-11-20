# The Clean half: read the reviewed workbook, apply it, write the final output.

clean_store <- function(ctx) {
  store <- new_store()
  shiny::isolate({
    store$project_dir <- ctx$project_dir
    store$profile <- read_profile(ctx$project_dir)
    store$raw_data <- ctx$raw_data
    store$tool_survey <- ctx$tool_survey
    store$tool_choices <- ctx$tool_choices
    store$other_db <- ctx$other_db
  })
  store
}

test_that("reviewed_files lists only the edited workbooks", {
  ctx <- local_reviewed_project()
  files <- reviewed_files(ctx$project_dir, default_profile()$clean$file_pattern)
  expect_identical(files$file, "2026-11-20_follow-ups_edited.xlsx")
})

test_that("read_reviewed reads and evaluates the edited log", {
  ctx <- local_reviewed_project()
  p <- default_profile()
  rv <- quiet_clean(
    function() {
      read_reviewed(ctx$project_dir, ctx$raw_data, p$clean, ctx$general)
    },
    ctx
  )
  expect_true(all(rv$file_checks$cleaning_log$matches_reference))
  expect_identical(evaluation_counts(rv$evaluation_log)$blocking, 0L)
  acts <- action_counts(rv$cleaning_log)
  expect_identical(acts$rows[acts$action == "discard"], 1L)
})

test_that("an unknown action is a blocking problem", {
  ctx <- local_reviewed_project(edit = function(cl) {
    cl$`Action taken`[cl$`Survey UUID` == "uuid-0007"] <- "fixed it"
    cl
  })
  p <- default_profile()
  rv <- quiet_clean(
    function() {
      read_reviewed(ctx$project_dir, ctx$raw_data, p$clean, ctx$general)
    },
    ctx
  )
  expect_gt(evaluation_counts(rv$evaluation_log)$blocking, 0L)

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_annotated_log(rv$cleaning_log, rv$evaluation_log, path)
  expect_setequal(readxl::excel_sheets(path), c("cleaning_log", "problems"))
})

test_that("read_reviewed says what to do when there is no edited workbook", {
  ctx <- local_check_context()
  p <- default_profile()
  expect_error(
    read_reviewed(ctx$project_dir, ctx$raw_data, p$clean, ctx$general),
    "_edited"
  )
})

test_that("apply_reviewed discards, recodes and applies the other responses", {
  ctx <- local_reviewed_project()
  p <- default_profile()
  rv <- quiet_clean(
    function() {
      read_reviewed(ctx$project_dir, ctx$raw_data, p$clean, ctx$general)
    },
    ctx
  )
  ap <- quiet_clean(
    function() {
      apply_reviewed(
        ctx$project_dir,
        ctx$raw_data,
        rv$cleaning_log,
        p$clean,
        ctx$general,
        ctx$tool_choices,
        ctx$other_db
      )
    },
    ctx
  )
  expect_identical(ap$n_before - ap$n_after, 1L)
  clean <- as.data.frame(ap$clean_dataset)
  expect_false("uuid-0006" %in% clean$`_uuid`)
  expect_identical(as.character(clean$Q21[clean$`_uuid` == "uuid-0007"]), "4")
  expect_identical(clean$Q31[clean$`_uuid` == "uuid-0005"], "Somalia")
  expect_gt(nrow(ap$other_audit_log), 0L)
  expect_gt(nrow(ap$final_log), 0L)

  ck <- quiet_clean(
    function() check_clean_data(ctx$raw_data, ap, p$clean, ctx$general),
    ctx
  )
  expect_identical(nrow(split_check_rows(ck)$tied), 0L)

  out <- file.path(
    ctx$project_dir,
    "output",
    "final",
    final_file_name("x_final")
  )
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  quiet_clean(function() export_final(ctx$raw_data, ap, p$final, out), ctx)
  expect_true(file.exists(out))
  expect_identical(basename(out), "x_final.xlsx")
})

test_that("split_check_rows separates rows without a uuid", {
  ck <- data.frame(uuid = c("a", NA, ""), issue = 1:3)
  s <- split_check_rows(ck)
  expect_identical(nrow(s$tied), 1L)
  expect_identical(nrow(s$untied), 2L)
  expect_identical(nrow(split_check_rows(NULL)$tied), 0L)
})

test_that("the three Clean screens run the whole of script 2", {
  ctx <- local_reviewed_project()
  store <- clean_store(ctx)

  shiny::testServer(mod_import_server, args = list(store = store), {
    session$setInputs(
      file_pattern = store$profile$clean$file_pattern,
      valid_actions = store$profile$clean$valid_actions,
      default_blank_action = "no_action",
      read = 1
    )
    expect_true(is.data.frame(store$reviewed_log))
    expect_identical(evaluation_counts(store$evaluation)$blocking, 0L)
  })

  shiny::testServer(mod_apply_server, args = list(store = store), {
    c <- store$profile$clean
    session$setInputs(
      change_response_values = c$change_response_values,
      blank_response_values = c$blank_response_values,
      remove_survey_values = c$remove_survey_values,
      no_action_values = c$no_action_values,
      apply_other = TRUE,
      apply_repeated_entries = TRUE,
      restore_types = TRUE,
      apply = 1
    )
    expect_identical(
      store$clean_result$n_before - store$clean_result$n_after,
      1L
    )
  })

  shiny::testServer(mod_final_server, args = list(store = store), {
    session$setInputs(
      stem = "2026-11-20_4Mi_final_output",
      project_name = "Test",
      data_collection_round = "R1",
      prepared_by = "me",
      project_description = "",
      check = 1
    )
    expect_false(is.null(store$final_check))
    session$setInputs(export = 1)
    expect_true(file.exists(store$final_path))
    expect_identical(read_profile(store$project_dir)$final$project_name, "Test")
  })

  status <- shiny::isolate(workflow_status(store))
  labels <- vapply(status, `[[`, "", "label")
  expect_identical(
    vapply(
      status[labels %in% c("Import reviewed", "Apply", "Final output")],
      `[[`,
      "",
      "state"
    ),
    rep("done", 3)
  )
})

test_that("Apply is refused while the log has blocking problems", {
  ctx <- local_reviewed_project()
  store <- clean_store(ctx)
  shiny::isolate({
    store$reviewed_log <- data.frame(x = 1)
    store$evaluation <- data.frame(issue_type = "invalid_action")
  })
  shiny::testServer(mod_apply_server, args = list(store = store), {
    session$setInputs(apply = 1)
    expect_null(store$clean_result)
  })
  expect_identical(
    shiny::isolate(import_status(store$reviewed_log, store$evaluation))$state,
    "error"
  )
})
