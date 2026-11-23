# "Apply corrections" without Shiny: the reviewers' workbook and an ONA
# download in, the final workbook out.

test_that("the round's corrections are applied from the workbook alone", {
  fx <- local_round_workbook()
  st <- quiet_apply(
    workbooks = as_upload(fx$src, fx$workbook),
    download = as_upload(fx$src, fx$download)
  )
  expect_identical(st$info$meta$round_name, "Week 1")
  expect_identical(n_records(st$raw_data, TRUE), 12L)
  expect_length(st$records$missing, 0)
  expect_length(st$records$changed, 0)
  # the questionnaire came from the workbook
  expect_false(is.null(st$tool$tool_survey))
  expect_identical(evaluation_counts(st$reviewed$evaluation_log)$blocking, 0L)

  res <- capture_run(
    function() finish_apply(st, st$dir),
    step = "test",
    dir = st$dir
  )
  expect_null(res$error)
  out <- res$value
  clean <- out$clean_result$clean_dataset
  expect_identical(out$clean_result$n_after, 11L)
  expect_false("uuid-0006" %in% clean$`_uuid`)
  expect_identical(clean$Q31[clean$`_uuid` == "uuid-0005"], "Somalia")
  expect_identical(nrow(out$check$tied), 0L)

  # the final workbook keeps the round memory for the next round
  path <- file.path(st$dir, round_final_file("Week 1"))
  res <- capture_run(
    function() {
      export_final(
        st$raw_data,
        out$clean_result,
        st$profile$final,
        path,
        round_info = st$info
      )
    },
    step = "test",
    dir = st$dir
  )
  expect_null(res$error)
  back <- cleaningkit::read_round_info(path)
  expect_identical(back$meta$round_id, st$info$meta$round_id)
})

test_that("a later download works, with ONA edits and deletions reported", {
  fx <- local_round_workbook()
  # a later download: survey 3 edited on ONA, survey 9 deleted, 4 new ones
  later <- fixture_export(n = 16)
  later$Q31[later$`_uuid` == "uuid-0003"] <- "Ethiopia"
  later <- later[later$`_uuid` != "uuid-0009", ]
  openxlsx::write.xlsx(later, file.path(fx$src, "ona_later.xlsx"))

  st <- quiet_apply(
    workbooks = as_upload(fx$src, fx$workbook),
    download = as_upload(fx$src, "ona_later.xlsx")
  )
  expect_identical(n_records(st$raw_data, TRUE), 11L)
  expect_false(any(sprintf("uuid-%04d", 13:16) %in% st$raw_data$`_uuid`))
  expect_identical(st$records$missing, "uuid-0009")
  expect_identical(st$records$changed, "uuid-0003")
  txt <- round_records_text(st$records, st$info)
  expect_match(txt$text, "11 of them are in this download")
  expect_length(txt$warnings, 2)
  expect_match(txt$warnings[2], "edited on ONA")
})

test_that("workbooks without round memory or from two rounds are refused", {
  fx <- local_round_workbook()
  openxlsx::write.xlsx(
    list(cleaning_log = data.frame(x = 1)),
    file.path(fx$src, "old.xlsx")
  )
  expect_error(
    read_reviewed_round(as_upload(fx$src, "old.xlsx")),
    "does not say which surveys"
  )
  expect_error(
    read_reviewed_round(as_upload(fx$src, fx$download)[0, ]),
    "Upload the workbook"
  )

  # a copy of the same round is fine
  file.copy(
    file.path(fx$src, fx$workbook),
    file.path(fx$src, "second reviewer.xlsx")
  )
  both <- as_upload(fx$src, c(fx$workbook, "second reviewer.xlsx"))
  expect_identical(read_reviewed_round(both)$meta$round_name, "Week 1")

  # a workbook of another round is not
  dir <- new_workspace(withr::local_tempdir(), templates = TRUE)
  r2 <- quiet_round(
    dir,
    download = as_upload(fx$src, fx$download),
    xlsform = as_upload(fx$src, fx$form)
  )
  other <- file.path(fx$src, "week 2.xlsx")
  res <- capture_run(
    function() {
      build_review_workbook(
        cleaningkit::create_combined_log(list(checked_dataset = r2$raw_data)),
        other,
        utils::modifyList(r2$profile$review, list(vba = FALSE)),
        r2$profile$general,
        round_info = make_round_info(r2, "Week 2")
      )
    },
    step = "test",
    dir = dir
  )
  expect_error(
    read_reviewed_round(as_upload(fx$src, c(fx$workbook, "week 2.xlsx"))),
    "different rounds"
  )
})

test_that("a download of another survey is refused plainly", {
  fx <- local_round_workbook()
  other <- fixture_export(n = 3)
  other$`_uuid`[-1] <- c("x1", "x2", "x3")
  openxlsx::write.xlsx(other, file.path(fx$src, "other.xlsx"))
  expect_error(
    quiet_apply(
      workbooks = as_upload(fx$src, fx$workbook),
      download = as_upload(fx$src, "other.xlsx")
    ),
    "None of the 12 surveys"
  )
})

test_that("uploaded workbooks keep distinct names in the reading folder", {
  src <- withr::local_tempdir()
  for (f in c("a.xlsx", "b.xlsx")) {
    writeLines("x", file.path(src, f))
  }
  up <- as_upload(src, c("a.xlsx", "b.xlsx"))
  up$name <- c("review.xlsx", "review.xlsx")
  folder <- place_reviewed(up, withr::local_tempdir())
  expect_setequal(list.files(folder), c("review.xlsx", "review_1.xlsx"))
})

test_that("the Apply corrections screen goes from uploads to the final workbook", {
  fx <- local_round_workbook()
  withr::local_options(cleaningkitapp.server = TRUE)

  shiny::testServer(mod_apply_flow_server, {
    session$setInputs(
      workbooks = as_upload(fx$src, fx$workbook),
      download = as_upload(fx$src, fx$download)
    )
    session$setInputs(start = 1)
    expect_identical(state()$info$meta$round_name, "Week 1")
    expect_true(dir.exists(ws$dir))

    session$setInputs(apply = 1)
    expect_identical(applied()$clean_result$n_after, 11L)

    session$setInputs(project_name = "4Mi test", prepared_by = "Data unit")
    path <- output$final
    expect_true(file.exists(path))
    info <- cleaningkit::read_round_info(path)
    expect_identical(info$meta$round_name, "Week 1")
    readme <- suppressMessages(readxl::read_excel(path, sheet = 1, col_names = FALSE))
    expect_true(any(grepl("4Mi test", unlist(readme))))
  })
})

test_that("online, only the upload screens are built", {
  withr::local_options(cleaningkitapp.server = TRUE)
  html <- as.character(app_ui(NULL))
  expect_match(html, "apply_flow-workbooks", fixed = TRUE)
  expect_no_match(html, "project-", fixed = TRUE)
  expect_no_match(html, "tool-", fixed = TRUE)
  withr::local_options(cleaningkitapp.server = FALSE)
  expect_match(as.character(app_ui(NULL)), "project-", fixed = TRUE)
})
