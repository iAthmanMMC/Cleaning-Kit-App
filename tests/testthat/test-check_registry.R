test_that("every argument in the registry exists in its cleaningkit function", {
  for (id in names(ck_checks())) {
    check <- ck_checks()[[id]]
    expect_true(
      all(names(check$args) %in% names(formals(check$fn))),
      label = paste0(id, ": arguments exist in the function")
    )
  }
})

test_that("every check has a known kind and argument types", {
  types <- c(
    "number",
    "toggle",
    "text",
    "texts",
    "column",
    "columns",
    "choice",
    "file"
  )
  for (id in names(ck_checks())) {
    check <- ck_checks()[[id]]
    expect_true(check$kind %in% c("log", "other"), label = id)
    expect_true(
      all(vapply(check$args, `[[`, "", "type") %in% types),
      label = paste0(id, ": argument types")
    )
  }
})

test_that("defaults come from the cleaningkit functions", {
  checks <- ck_checks()
  expect_identical(arg_default(checks$duration, "lower_bound"), 15)
  expect_identical(arg_default(checks$back_to_back, "gap_from"), "end")
  expect_identical(
    arg_default(checks$outliers, "columns_to_check"),
    character(0)
  )
  expect_true("_uuid" %in% arg_default(checks$completeness, "metadata_cols"))
  # a default the registry sets itself wins
  expect_identical(
    arg_default(checks$logical, "list_of_check"),
    "resources/logical_checklist.xlsx"
  )
})

test_that("saved settings are merged over the defaults", {
  saved <- list(
    duration = list(enabled = FALSE, args = list(lower_bound = "20")),
    removed_check = list(enabled = TRUE)
  )

  merged <- merge_check_settings(saved)

  expect_false(merged$duration$enabled)
  expect_identical(merged$duration$args$lower_bound, 20)
  expect_identical(merged$duration$args$upper_bound, 60)
  expect_true(merged$refused$enabled)
  expect_false("removed_check" %in% names(merged))
})

test_that("checks run on the fixture and find what was planted", {
  ctx <- local_check_context()

  duration <- run_check_quietly("duration", ctx)
  expect_identical(duration$uuid, "uuid-0006")

  country <- run_check_quietly("country_of_interview", ctx)
  expect_identical(country$uuid, "uuid-0005")

  outliers <- run_check_quietly("outliers", ctx, list(columns_to_check = "Q21"))
  expect_identical(outliers$uuid, "uuid-0007")

  logical <- run_check_quietly("logical", ctx)
  expect_setequal(
    unique(logical$uuid),
    c("uuid-0004", "uuid-0005", "uuid-0007", "uuid-0009")
  )

  other <- run_check_quietly("other_responses", ctx)
  expect_setequal(other$question_name, c("Q13_1", "Q30_1"))
})

test_that("a check says what to load first instead of failing inside cleaningkit", {
  ctx <- local_check_context()
  ctx$tool_survey <- NULL
  expect_error(
    run_check(
      ck_checks()$outliers,
      default_check_settings()$outliers$args,
      ctx
    ),
    "Load the XLSForm"
  )

  ctx$raw_data <- NULL
  expect_error(
    run_check(
      ck_checks()$duration,
      default_check_settings()$duration$args,
      ctx
    ),
    "Load the data"
  )
})

test_that("required and numeric arguments are checked before the call", {
  ctx <- local_check_context()
  expect_error(
    run_check(
      ck_checks()$similar_questions,
      default_check_settings()$similar_questions$args,
      ctx
    ),
    "Fill in 'Questions to compare'"
  )

  args <- default_check_settings()$duration$args
  args$lower_bound <- NA
  expect_error(run_check(ck_checks()$duration, args, ctx), "must be a number")
})

test_that("a missing checklist file is reported by name", {
  ctx <- local_check_context()
  args <- default_check_settings()$logical$args
  args$list_of_check <- "resources/nope.xlsx"
  expect_error(
    run_check(ck_checks()$logical, args, ctx),
    "Logical checklist not found: resources/nope.xlsx"
  )
})

test_that("the project fills in the ID, enumerator and data arguments", {
  ctx <- local_check_context()
  ctx$general$enumerator_column <- "enum"

  args <- check_context_args(ck_checks()$back_to_back, ctx)

  expect_identical(args$enumerator_column, "enum")
  expect_identical(args$uuid_column, "_uuid")
  expect_false("tool_survey" %in% names(args))
})
