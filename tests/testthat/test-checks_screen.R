# Store filled as the Tool and Data screens leave it.
local_ready_store <- function(env = parent.frame()) {
  ctx <- local_check_context(env = env)
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

# Settings for a quick run: the GPS check needs internet and columns the
# fixture does not have, so it is left off.
quick_settings <- function() {
  s <- default_check_settings()
  s$interview_location$enabled <- FALSE
  s$outliers$args$columns_to_check <- "Q21"
  s
}

test_that("running the checks fills the logs, the other responses and the combined log", {
  store <- local_ready_store()
  shiny::isolate(store$profile$checks <- quick_settings())

  shiny::testServer(mod_checks_server, args = list(store = store), {
    session$setInputs(run = 1)

    results <- store$check_results
    expect_setequal(
      names(results),
      names(Filter(function(x) x$enabled, quick_settings()))
    )
    expect_identical(results$duration$n, 1L)
    expect_identical(results$repeated_entries$status, "done")
    expect_true("duration_log" %in% names(store$logs))
    expect_identical(nrow(store$other_responses), 4L)
    expect_true(is.data.frame(store$combined_log$cleaning_log))
    expect_identical(
      nrow(store$combined_log$cleaning_log),
      sum(vapply(store$logs, nrow, integer(1)))
    )
  })
  expect_false(
    read_profile(shiny::isolate(
      store$project_dir
    ))$checks$interview_location$enabled
  )
})

test_that("a failing check is recorded and the others still run", {
  store <- local_ready_store()
  settings <- quick_settings()
  settings$logical$args$list_of_check <- "resources/missing.xlsx"
  shiny::isolate(store$profile$checks <- settings)

  shiny::testServer(mod_checks_server, args = list(store = store), {
    session$setInputs(run = 1)

    expect_identical(store$check_results$logical$status, "error")
    expect_match(store$check_results$logical$error, "not found")
    expect_identical(store$check_results$duration$status, "done")
    expect_false("logical_log" %in% names(store$logs))
  })
})

test_that("new data clears the previous check results", {
  store <- local_ready_store()
  shiny::isolate({
    store$check_results <- list(
      duration = list(status = "done", kind = "log", n = 1L)
    )
    store$review_path <- "x.xlsm"
  })

  reset_from(store, "checks")

  expect_identical(shiny::isolate(store$check_results), list())
  expect_null(shiny::isolate(store$review_path))
  expect_false(is.null(shiny::isolate(store$raw_data)))
})

test_that("the review workbook is written to output/follow_ups", {
  store <- local_ready_store()
  shiny::isolate(store$profile$checks <- quick_settings())
  shiny::testServer(mod_checks_server, args = list(store = store), {
    session$setInputs(run = 1)
  })

  shiny::testServer(mod_review_server, args = list(store = store), {
    session$setInputs(
      stem = "2026-11-20_follow-ups",
      vba = TRUE,
      include_other = TRUE,
      include_dataset = TRUE,
      flag_columns = TRUE,
      group_by = "issue",
      color_mode = "partial",
      color_columns = "old_value"
    )
    session$setInputs(create = 1)

    path <- store$review_path
    expect_identical(basename(path), "2026-11-20_follow-ups.xlsm")
    expect_true(file.exists(path))
    sheets <- readxl::excel_sheets(path)
    expect_true(all(
      c("cleaning_log", "other_responses", "dataset") %in% sheets
    ))

    # a second press asks before replacing
    store$review_path <- NULL
    session$setInputs(create = 2)
    expect_null(store$review_path)
    session$setInputs(confirm_replace = 1)
    expect_identical(basename(store$review_path), "2026-11-20_follow-ups.xlsm")
  })
})

test_that("review_file_name() sets the extension and strips bad characters", {
  expect_identical(
    review_file_name("a_follow-ups", vba = TRUE),
    "a_follow-ups.xlsm"
  )
  expect_identical(
    review_file_name("a_follow-ups.xlsx", vba = TRUE),
    "a_follow-ups.xlsm"
  )
  expect_identical(
    review_file_name("round 3: east", vba = FALSE),
    "round 3_ east.xlsx"
  )
  expect_error(review_file_name("  "), "file name")
})

test_that("results tables count flags per check and add the enumerator", {
  ctx <- local_check_context()
  logs <- list(
    duration_log = run_check_quietly("duration", ctx),
    country_of_interview_log = run_check_quietly("country_of_interview", ctx)
  )
  results <- list(
    duration = list(status = "done", kind = "log", n = 1L),
    country_of_interview = list(status = "done", kind = "log", n = 1L),
    logical = list(status = "error", kind = "log", n = 0L, error = "x")
  )

  per_check <- flags_per_check(results, logs)
  expect_identical(
    per_check$check[per_check$status == "failed"],
    "Logical checks"
  )
  expect_identical(sum(per_check$flags, na.rm = TRUE), 2L)

  flags <- flags_table(logs, ctx$raw_data, ctx$general)
  expect_identical(flags$enumerator[flags$uuid == "uuid-0006"], "enum_1")

  ranking <- enumerator_priority(logs, ctx$raw_data, ctx$general)
  expect_setequal(ranking$enumerator, c("enum_1", "enum_3"))
})
