test_that("tool_languages() drops the (code) suffix so cleaningkit can match it", {
  dir <- local_fixture_project()
  expect_identical(
    tool_languages(file.path(dir, "resources", "tool.xlsx")),
    c("English", "French")
  )
})

test_that("tool_languages() is empty for a file that cannot be read", {
  expect_identical(tool_languages(tempfile(fileext = ".xlsx")), character(0))
})

test_that("load_tool() builds the other DB and reports bad constraints", {
  dir <- local_fixture_project()

  res <- capture_run(
    function() load_tool(file.path(dir, "resources", "tool.xlsx")),
    step = "Load tool",
    dir = dir
  )

  expect_null(res$error)
  tool <- res$value
  expect_identical(nrow(tool$tool_survey), 10L)
  expect_setequal(tool$other_labels$name, c("Q13_1", "Q30_1"))
  expect_setequal(tool$other_db$name, c("Q13_1", "Q30_1"))
  expect_identical(tool$constraint_issues, "selected(${Q20}, 'nope')")
})

test_that("load_tool() writes cleaningkit's side file into the project, not the working directory", {
  dir <- local_fixture_project()
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  capture_run(
    function() load_tool(file.path(dir, "resources", "tool.xlsx")),
    step = "Load tool",
    dir = dir
  )

  expect_true(file.exists(
    file.path(dir, "resources", "labels_questions_others.xlsx")
  ))
  expect_false(dir.exists(file.path(wd, "resources")))
})

test_that("load_tool() names the sheets it found when one is missing", {
  dir <- local_fixture_project()
  expect_error(
    load_tool(
      file.path(dir, "resources", "tool.xlsx"),
      survey_sheet = "Survey 2"
    ),
    "Sheets found: survey, choices"
  )
})
