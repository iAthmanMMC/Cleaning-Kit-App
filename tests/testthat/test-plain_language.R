# M8: the upload screens explain themselves. Checks a download can't support
# are left out with a reason, the usual file mix-ups get a plain message, and
# tables use words from the job instead of the code.

test_that("checks are left out when the download lacks their columns", {
  s <- default_check_settings()
  df <- fixture_export(n = 3)
  expect_length(check_missing_columns("duration", s$duration$args, df), 0)
  expect_setequal(
    check_missing_columns("interview_location", s$interview_location$args, df),
    c("_location_latitude", "_location_longitude", "Q14")
  )
  # checks with no required columns are never left out for that reason
  expect_length(check_missing_columns("refused", s$refused$args, df), 0)
})

test_that("the standard run leaves out what can't run, with the reason", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona.xlsx"), n = 6)
  write_fixture_tool(file.path(src, "form.xlsx"))
  round <- quiet_round(
    dir,
    download = as_upload(src, "ona.xlsx"),
    xlsform = as_upload(src, "form.xlsx")
  )
  plan <- plan_checks(round)
  expect_setequal(names(plan$left_out), c("interview_location", "logical"))
  expect_match(plan$left_out[["interview_location"]], "no _location_latitude")
  expect_match(plan$left_out[["logical"]], "no logical checklist")
  expect_true(all(c("duration", "completeness") %in% plan$selected))
  # checks that are off by default stay off
  expect_false("spatial_proximity" %in% plan$selected)
})

test_that("the usual file mix-ups get a plain message", {
  src <- withr::local_tempdir()
  utils::write.csv(fixture_export(n = 2), file.path(src, "ona.csv"))
  expect_error(read_download_text(file.path(src, "ona.csv")), "is a CSV file")

  writeLines("not excel", file.path(src, "notes.xlsx"))
  expect_error(
    read_download_text(file.path(src, "notes.xlsx")),
    "could not be opened as an Excel file"
  )

  # one of the app's own workbooks in the download slot
  openxlsx::write.xlsx(
    list(dataset = fixture_export(n = 2), `_ck_round` = data.frame(key = "x")),
    file.path(src, "review.xlsx")
  )
  expect_error(
    read_download_text(file.path(src, "review.xlsx"), "Week 1.xlsx"),
    "Week 1.xlsx is a workbook made by this app"
  )

  # the ONA download in the questionnaire slot
  write_fixture_export(file.path(src, "ona.xlsx"), n = 2)
  expect_error(
    load_questionnaire(file.path(src, "ona.xlsx"), "English", "ona.xlsx"),
    "ona.xlsx doesn't look like an XLSForm"
  )
})

test_that("a wrong file in the questionnaire slot stops the round plainly", {
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona.xlsx"), n = 3)
  expect_error(
    quiet_round(
      dir,
      download = as_upload(src, "ona.xlsx"),
      xlsform = as_upload(src, "ona.xlsx")
    ),
    "doesn't look like an XLSForm"
  )
})

test_that("rounds that look wrong are pointed out", {
  expect_length(
    round_warnings(list(n_total = 5, n_checked_before = 2, n_new = 3), list()),
    0
  )
  expect_match(
    round_warnings(list(n_total = 5, n_checked_before = 5, n_new = 0)),
    "nothing new to check"
  )
  expect_match(
    round_warnings(list(n_total = 5, n_checked_before = 0, n_new = 5), list()),
    "same survey"
  )
  # the first round has no previous round to compare with
  expect_length(
    round_warnings(list(n_total = 5, n_checked_before = 0, n_new = 5)),
    0
  )
})

test_that("result tables use plain column names", {
  checks <- ck_checks()
  results <- list(
    duration = list(status = "done", kind = "log", n = 1L),
    refused = list(status = "error", kind = "log", n = 0L, error = "x")
  )
  logs <- list(duration_log = data.frame(uuid = "u1", question = "_duration"))
  tab <- plain_per_check(results, logs, checks)
  expect_named(tab, c("Check", "Problems found", "Surveys affected"))
  expect_identical(
    tab$`Problems found`[tab$Check == "Refusals"],
    "could not run"
  )

  enum <- data.frame(
    rank = 1,
    enumerator = "enum_1",
    priority_score = 2,
    n_checks_triggered = 2,
    total_flags = 3,
    flagged_interviews = 2,
    n_interviews = 4,
    flag_rate = 0.5,
    checks_triggered = "duration_log (1); completeness_log (2)"
  )
  plain <- plain_enumerators(enum, checks)
  expect_identical(
    plain$`Checks that found them`,
    "Interview duration (1), Completeness (2)"
  )

  ev <- data.frame(
    row = 1:2,
    uuid = c("u1", "u2"),
    question = "Q1",
    action = c("no_action", "fix it"),
    issue_type = c("new_equals_old", "invalid_action"),
    issue = c("Same value", "Unknown action")
  )
  pe <- plain_evaluation(ev)
  expect_identical(pe$`Must fix?`, c("yes", "no"))
  expect_identical(pe$Problem[1], "Unknown action")
  expect_null(plain_evaluation(ev[0, ]))
})

test_that("a failed read shows inline, not as an error popup", {
  src <- withr::local_tempdir()
  utils::write.csv(fixture_export(n = 2), file.path(src, "ona.csv"))
  write_fixture_tool(file.path(src, "form.xlsx"))
  withr::local_options(cleaningkitapp.server = TRUE)
  shiny::testServer(mod_check_flow_server, {
    session$setInputs(
      first = "first",
      download = as_upload(src, "ona.csv"),
      xlsform = as_upload(src, "form.xlsx")
    )
    session$setInputs(start = 1)
    expect_null(round())
    expect_match(problem(), "is a CSV file")
    expect_match(as.character(output$round_status$html), "That didn't work")
  })
})

test_that("the questionnaire in the download slot is named as such", {
  src <- withr::local_tempdir()
  write_fixture_tool(file.path(src, "form.xlsx"))
  expect_error(
    read_download_text(file.path(src, "form.xlsx")),
    "form.xlsx is the questionnaire (XLSForm), not a download",
    fixed = TRUE
  )
})
