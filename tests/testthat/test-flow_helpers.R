# The "Check new data" flow without Shiny: detection, starting a round from
# uploaded files, and the round memory written into the review workbook.

test_that("the label row is detected from the survey ID column", {
  df <- fixture_export(n = 4)
  expect_true(detect_label_row(df, "_uuid"))
  expect_false(detect_label_row(df[-1, ], "_uuid"))
  real <- data.frame(
    `_uuid` = c(
      "_uuid",
      "0b8f8a2e-1c2d-4e5f-8a9b-0c1d2e3f4a5b",
      "1c9f8a2e-1c2d-4e5f-8a9b-0c1d2e3f4a5b"
    ),
    check.names = FALSE
  )
  expect_true(detect_label_row(real, "_uuid"))
  expect_false(detect_label_row(real[-1, , drop = FALSE], "_uuid"))
})

test_that("the main columns are detected, keeping saved names that exist", {
  df <- fixture_export(n = 3)
  g <- detect_general(df)
  expect_identical(g$uuid_column, "_uuid")
  expect_identical(g$enumerator_column, "username")
  expect_identical(g$date_column, "today")
  expect_true(g$skip_label_row)
  expect_length(g$missing, 0)

  names(df)[names(df) == "username"] <- "enumerator"
  df <- df[setdiff(names(df), .column_candidates$date_column)]
  g <- detect_general(df)
  expect_identical(g$enumerator_column, "enumerator")
  expect_identical(g$missing, "date_column")
})

test_that("a first round keeps every survey and needs the XLSForm", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona.xlsx"), n = 8)
  write_fixture_tool(file.path(src, "form.xlsx"))

  expect_error(
    quiet_round(dir, download = as_upload(src, "ona.xlsx")),
    "Upload the XLSForm"
  )
  round <- quiet_round(
    dir,
    download = as_upload(src, "ona.xlsx"),
    xlsform = as_upload(src, "form.xlsx")
  )
  expect_identical(round$counts$n_new, 8L)
  expect_identical(n_records(round$raw_data, TRUE), 8L)
  expect_match(round_counts_text(round$counts), "first round")
  expect_false(is.null(round$tool$tool_survey))
  # no checklist uploaded: the blank template is not used
  expect_null(round$checklist_path)
  expect_false(file.exists(file.path(
    dir,
    "resources",
    "logical_checklist.xlsx"
  )))
})

test_that("an uploaded checklist is placed where the logical check reads it", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  openxlsx::write.xlsx(
    data.frame(check_id = "c1", check_to_perform = "Q1 == 'x'"),
    file.path(src, "rules.xlsx")
  )
  path <- place_checklist(as_upload(src, "rules.xlsx"), NULL, dir)
  expect_identical(path, file.path(dir, "resources", "logical_checklist.xlsx"))
  expect_identical(sheet_as_text(path)$check_id, "c1")
})

test_that("the next round needs only the new download and the last workbook", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona_1.xlsx"), n = 8)
  write_fixture_tool(file.path(src, "form.xlsx"))

  r1 <- quiet_round(
    dir,
    download = as_upload(src, "ona_1.xlsx"),
    xlsform = as_upload(src, "form.xlsx")
  )
  r1$profile$general$label_language <- "English"
  info <- make_round_info(r1, "R1")
  expect_identical(info$meta$round_name, "R1")
  expect_false(is.null(info$tool_survey))

  # the review workbook carries the round memory
  ctx <- check_context(
    raw_data = r1$raw_data,
    tool_survey = r1$tool$tool_survey,
    tool_choices = r1$tool$tool_choices,
    general = r1$profile$general,
    project_dir = dir,
    other_db = r1$tool$other_db
  )
  log <- run_check_quietly("duration", ctx)
  wb <- file.path(src, round_review_file("R1", vba = FALSE))
  res <- capture_run(
    function() {
      combined <- cleaningkit::create_combined_log(
        list(checked_dataset = r1$raw_data, duration_log = log)
      )
      review <- default_profile()$review
      review$vba <- FALSE
      build_review_workbook(
        combined,
        wb,
        review,
        r1$profile$general,
        round_info = info
      )
    },
    step = "test",
    dir = dir
  )
  expect_null(res$error)

  # round 2: 12 surveys in the download, 8 checked in R1
  dir2 <- new_workspace(withr::local_tempdir(), templates = TRUE)
  write_fixture_export(file.path(src, "ona_2.xlsx"), n = 12)
  r2 <- quiet_round(
    dir2,
    download = as_upload(src, "ona_2.xlsx"),
    previous_files = as_upload(src, basename(wb))
  )
  expect_identical(r2$counts$n_total, 12L)
  expect_identical(r2$counts$n_checked_before, 8L)
  expect_identical(r2$counts$n_new, 4L)
  expect_identical(
    r2$raw_data$`_uuid`[-1],
    sprintf("uuid-%04d", 9:12)
  )
  # the questionnaire came from the workbook, not an upload
  expect_false(is.null(r2$tool$tool_survey))
  expect_match(
    round_counts_text(r2$counts, r2$previous),
    "8 were checked in R1"
  )
})

test_that("a download without a survey ID column is refused plainly", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  df <- fixture_export(n = 3)
  df$`_uuid` <- NULL
  openxlsx::write.xlsx(df, file.path(src, "ona.xlsx"))
  write_fixture_tool(file.path(src, "form.xlsx"))
  expect_error(
    quiet_round(
      dir,
      download = as_upload(src, "ona.xlsx"),
      xlsform = as_upload(src, "form.xlsx")
    ),
    "no survey ID column"
  )
})

test_that("settings kept in the last workbook are used again", {
  p <- default_profile()
  p$general$enumerator_column <- "enum_code"
  p$review$vba <- FALSE
  info <- list(
    settings = list(cleaningkitapp_profile = yaml::as.yaml(p))
  )
  back <- profile_from_previous(info)
  expect_identical(back$general$enumerator_column, "enum_code")
  expect_false(back$review$vba)
  expect_identical(profile_from_previous(NULL), default_profile())
})

test_that("the Check new data screen goes from uploads to a workbook", {
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona.xlsx"), n = 6)
  write_fixture_tool(file.path(src, "form.xlsx"))
  withr::local_options(cleaningkitapp.server = TRUE)

  shiny::testServer(mod_check_flow_server, {
    session$setInputs(
      first = "first",
      download = as_upload(src, "ona.xlsx"),
      xlsform = as_upload(src, "form.xlsx"),
      round_name = "Round A",
      label_row = "auto"
    )
    session$setInputs(start = 1)
    expect_identical(round()$counts$n_new, 6L)
    expect_true(dir.exists(ws$dir))

    session$setInputs(checks = c("duration", "completeness"), run = 1)
    expect_false(is.null(outcome()$combined_log))
    expect_setequal(names(outcome()$results), c("duration", "completeness"))

    round_now <- round()
    round_now$profile$review$vba <- FALSE
    round(round_now)
    path <- output$workbook
    expect_true(file.exists(path))
    info <- cleaningkit::read_round_info(path)
    expect_identical(info$meta$round_name, "Round A")
    expect_identical(nrow(info$checked), 6L)
  })
})
