test_that("opening a project loads its saved settings", {
  dir <- local_fixture_project()
  profile <- default_profile()
  profile$general$uuid_column <- "my_uuid"
  write_profile(profile, dir)
  store <- new_store()

  shiny::testServer(mod_project_server, args = list(store = store), {
    session$setInputs(path = dir, open = 1)

    expect_identical(store$project_dir, dir)
    expect_identical(store$profile$general$uuid_column, "my_uuid")
  })
})

test_that("set up creates the cleaningkit folders in a new project", {
  dir <- normalizePath(withr::local_tempdir(), winslash = "/")
  store <- new_store()

  shiny::testServer(mod_project_server, args = list(store = store), {
    session$setInputs(path = dir, open = 1)
    expect_false(all(project_folder_status(dir)$exists))

    session$setInputs(copy_templates = FALSE, setup = 1)

    expect_true(all(project_folder_status(dir)$exists))
  })
})

test_that("saving settings writes them to the project profile", {
  dir <- local_fixture_project()
  store <- new_store()

  shiny::testServer(mod_project_server, args = list(store = store), {
    session$setInputs(path = dir, open = 1)
    session$setInputs(
      uuid_column = "uuid",
      enumerator_column = "enum_id",
      date_column = "today",
      skip_label_row = FALSE,
      save = 1
    )
  })

  saved <- read_profile(dir)
  expect_identical(saved$general$uuid_column, "uuid")
  expect_identical(saved$general$enumerator_column, "enum_id")
  expect_false(saved$general$skip_label_row)
})

test_that("opening another project clears the previous project's tool and data", {
  store <- new_store()
  shiny::isolate({
    store$project_dir <- "C:/old"
    store$tool_survey <- data.frame(name = "Q1")
    store$raw_data <- data.frame(x = 1)
  })
  dir <- local_fixture_project()

  shiny::testServer(mod_project_server, args = list(store = store), {
    session$setInputs(path = dir, open = 1)
    expect_null(store$tool_survey)
    expect_null(store$raw_data)
  })
})

test_that("the Tool then Data screens fill the store end to end", {
  dir <- local_fixture_project()
  write_fixture_export(file.path(dir, "data", "export.xlsx"), n = 8)
  store <- new_store()
  shiny::isolate({
    store$project_dir <- dir
    store$profile <- read_profile(dir)
  })

  shiny::testServer(mod_tool_server, args = list(store = store), {
    session$setInputs(
      survey_sheet = "survey",
      choices_sheet = "choices",
      language = "English",
      other_text_types = NULL,
      load = 1
    )
    expect_identical(nrow(store$tool_survey), 10L)
    expect_identical(store$constraint_issues, "selected(${Q20}, 'nope')")
  })

  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(
      filter_new = TRUE,
      use_ledger = TRUE,
      archive = TRUE,
      output_name = "data.xlsx",
      sheet = 1,
      extra_date_cols = NULL,
      extra_datetime_cols = NULL,
      file = "export.xlsx"
    )
    session$setInputs(prepare = 1)
    # filtering moves files, so it waits for the user to confirm
    expect_null(store$raw_data)

    session$setInputs(confirm_filter = 1)
    expect_identical(n_records(store$raw_data), 8L)
    expect_false(store$filter_summary$filtered)
  })

  expect_true(read_profile(dir)$data$filter_new)
  expect_true(any(shiny::isolate(store$log)$step == "Prepare data"))
})

test_that("Prepare data asks for the tool first", {
  dir <- local_fixture_project()
  store <- new_store()
  shiny::isolate(store$project_dir <- dir)

  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(filter_new = FALSE, file = NULL, prepare = 1)
    expect_null(store$raw_data)
  })
})

test_that("the progress sidebar follows the store", {
  store <- new_store()
  steps <- shiny::isolate(workflow_status(store))
  expect_identical(steps[[1]]$state, "todo")

  dir <- local_fixture_project()
  shiny::isolate({
    store$project_dir <- dir
    store$tool_survey <- data.frame(name = paste0("Q", 1:3))
    store$constraint_issues <- character(0)
  })
  steps <- shiny::isolate(workflow_status(store))
  expect_identical(steps[[1]]$state, "done")
  expect_identical(steps[[2]]$detail, "3 questions")
})

test_that("the app object builds", {
  expect_s3_class(ck_app(), "shiny.appobj")
})
