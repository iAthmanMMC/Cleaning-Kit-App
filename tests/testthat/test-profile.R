test_that("a project without a profile gets the defaults", {
  dir <- withr::local_tempdir()
  expect_identical(read_profile(dir), default_profile())
})

test_that("a profile survives a save and reload unchanged", {
  dir <- withr::local_tempdir()
  profile <- default_profile()
  profile$general$uuid_column <- "uuid"
  profile$tool$other_text_types <- c("Q31_2", "Q40_1")
  profile$data$sheet <- 2L

  write_profile(profile, dir)

  expect_true(file.exists(file.path(dir, "resources", "cleaningkitapp.yml")))
  expect_identical(read_profile(dir), profile)
})

test_that("empty vector settings come back as character(0), not list()", {
  dir <- withr::local_tempdir()
  write_profile(default_profile(), dir)

  reloaded <- read_profile(dir)

  expect_identical(reloaded$tool$other_text_types, character(0))
  expect_identical(reloaded$data$extra_date_cols, character(0))
})

test_that("settings missing from an older profile take their default", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "resources"))
  writeLines(
    c("general:", "  uuid_column: my_id"),
    file.path(dir, "resources", "cleaningkitapp.yml")
  )

  profile <- read_profile(dir)

  expect_identical(profile$general$uuid_column, "my_id")
  expect_identical(profile$general$enumerator_column, "username")
  expect_identical(profile$data$output_name, "data.xlsx")
})

test_that("project_file() resolves relative paths and keeps absolute ones", {
  expect_identical(
    project_file("C:/proj", "resources/tool.xlsx"),
    file.path("C:/proj", "resources/tool.xlsx")
  )
  expect_identical(
    project_file("C:/proj", "D:/tools/tool.xlsx"),
    "D:/tools/tool.xlsx"
  )
  expect_null(project_file(NULL, "resources/tool.xlsx"))
})
