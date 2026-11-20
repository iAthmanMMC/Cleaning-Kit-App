fixture_tool_survey <- function(dir) {
  suppressMessages(utils::capture.output(
    survey <- cleaningkit::read_tool_survey(file.path(
      dir,
      "resources",
      "tool.xlsx"
    ))
  ))
  survey
}

quietly <- function(expr) {
  res <- capture_run(function() expr, step = "test")
  if (!is.null(res$error)) {
    stop(res$error)
  }
  res$value
}

test_that("the first round reads the single export without filtering", {
  dir <- local_fixture_project()
  data_dir <- file.path(dir, "data")
  write_fixture_export(file.path(data_dir, "export.xlsx"), n = 10)

  res <- quietly(prepare_data(data_dir, fixture_tool_survey(dir)))

  expect_false(res$filter_summary$filtered)
  expect_identical(n_records(res$raw_data), 10L)
  expect_identical(basename(res$path), "data.xlsx")
  expect_null(res$filter_summary$data)
})

test_that("a later round keeps only records not validated before", {
  dir <- local_fixture_project()
  data_dir <- file.path(dir, "data")
  write_fixture_export(file.path(data_dir, "export.xlsx"), n = 10)
  quietly(prepare_data(data_dir, fixture_tool_survey(dir)))

  # the next ONA download holds the first 10 surveys again, plus 5 new ones
  write_fixture_export(file.path(data_dir, "export_2.xlsx"), n = 15)
  res <- quietly(prepare_data(data_dir, fixture_tool_survey(dir)))

  expect_true(res$filter_summary$filtered)
  expect_identical(n_records(res$raw_data), 5L)
  body <- data_body(res$raw_data)
  expect_identical(body$`_uuid`, sprintf("uuid-%04d", 11:15))
  expect_identical(ledger_count(data_dir), 15L)
  expect_identical(data_files(data_dir)$file, "data.xlsx")
})

test_that("with filtering off, the chosen file is read as it is", {
  dir <- local_fixture_project()
  data_dir <- file.path(dir, "data")
  write_fixture_export(file.path(data_dir, "a.xlsx"), n = 4)
  write_fixture_export(file.path(data_dir, "b.xlsx"), n = 7)

  res <- quietly(prepare_data(
    data_dir,
    fixture_tool_survey(dir),
    filter_new = FALSE,
    file = "b.xlsx"
  ))

  expect_null(res$filter_summary)
  expect_identical(n_records(res$raw_data), 7L)
  expect_setequal(data_files(data_dir)$file, c("a.xlsx", "b.xlsx"))
})

test_that("data_column_checks() catches duplicate IDs and wrong column names", {
  raw <- fixture_export(n = 5, duplicate_uuid = TRUE)
  general <- default_profile()$general
  general$enumerator_column <- "enumerator"

  checks <- data_column_checks(raw, general)

  expect_identical(checks$state, c("warn", "error", "done"))
  expect_match(checks$detail[1], "1 duplicated")
  expect_match(checks$detail[2], "No column 'enumerator'")
})

test_that("data_files() skips Excel lock files and non-Excel files", {
  dir <- withr::local_tempdir()
  file.create(file.path(
    dir,
    c("export.xlsx", "~$export.xlsx", "processed_uuids.csv")
  ))
  expect_identical(data_files(dir)$file, "export.xlsx")
})

test_that("ledger_count() is NA when there is no ledger yet", {
  expect_identical(ledger_count(withr::local_tempdir()), NA_integer_)
})

test_that("copy_uploads() keeps the original file names", {
  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  file.create(file.path(src, c("0.xlsx", "1.xlsx")))
  upload <- data.frame(
    name = c("ONA export 12-11.xlsx", "previous.xlsx"),
    datapath = file.path(src, c("0.xlsx", "1.xlsx"))
  )

  copy_uploads(upload, dest)

  expect_setequal(list.files(dest), c("ONA export 12-11.xlsx", "previous.xlsx"))
})

test_that("clean_dir_path() strips the quotes Windows 'Copy as path' adds", {
  dir <- withr::local_tempdir()
  expect_identical(
    clean_dir_path(paste0('"', dir, '"')),
    normalizePath(dir, winslash = "/")
  )
  expect_identical(clean_dir_path("  "), "")
})
