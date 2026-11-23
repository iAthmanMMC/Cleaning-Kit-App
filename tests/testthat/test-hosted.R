# Hosted mode: one temporary workspace per session, no access to the
# server's disk, and the workspace carried between sessions as a zip.

test_that("ck_is_server follows the option, then the environment variable", {
  withr::local_options(cleaningkitapp.server = TRUE)
  expect_true(ck_is_server())
  withr::local_options(cleaningkitapp.server = FALSE)
  expect_false(ck_is_server())

  withr::local_options(cleaningkitapp.server = NULL)
  withr::local_envvar(CLEANINGKITAPP_SERVER = "true")
  expect_true(ck_is_server())
  withr::local_envvar(CLEANINGKITAPP_SERVER = "no")
  expect_false(ck_is_server())
})

test_that("a new workspace has the project folders and is removed cleanly", {
  root <- withr::local_tempdir()
  dir <- new_workspace(root)
  expect_true(all(project_folder_status(dir)$exists))
  expect_match(basename(dir), "^cleaningkitapp-")

  other <- new_workspace(root)
  expect_false(identical(dir, other))

  remove_workspace(dir)
  expect_false(dir.exists(dir))
  expect_true(dir.exists(other))
})

test_that("a workspace survives a download and upload", {
  root <- withr::local_tempdir()
  dir <- new_workspace(root)
  writeLines("a,b", file.path(dir, "data", "export.csv"))
  write_profile(default_profile(), dir)

  zipfile <- file.path(root, "ws.zip")
  zip_workspace(dir, zipfile)
  restored <- restore_workspace(zipfile, root)

  expect_false(identical(restored, dir))
  expect_identical(readLines(file.path(restored, "data", "export.csv")), "a,b")
  expect_true(file.exists(profile_path(restored)))
})

test_that("a zipped project folder with one top folder opens too", {
  root <- withr::local_tempdir()
  project <- file.path(root, "round_12")
  dir.create(file.path(project, "data"), recursive = TRUE)
  writeLines("x", file.path(project, "data", "export.csv"))
  zipfile <- file.path(root, "round_12.zip")
  zip::zip(zipfile, "round_12", root = root)

  restored <- restore_workspace(zipfile, root)
  expect_true(file.exists(file.path(restored, "data", "export.csv")))
  expect_true(all(project_folder_status(restored)$exists))
})

test_that("restore_workspace refuses zips that are not projects or escape their folder", {
  root <- withr::local_tempdir()
  writeLines("x", file.path(root, "notes.txt"))
  zipfile <- file.path(root, "notes.zip")
  zip::zip(zipfile, "notes.txt", root = root)
  expect_error(restore_workspace(zipfile, root), "no data/")

  local_mocked_bindings(
    zip_list = function(...) data.frame(filename = "../../evil.R"),
    .package = "zip"
  )
  expect_error(restore_workspace(zipfile, root), "outside its own folder")
})

test_that("zip_workspace refuses an empty workspace", {
  dir <- withr::local_tempdir()
  expect_error(zip_workspace(dir, file.path(dir, "x.zip")), "empty")
})

test_that("hosted, each session gets its own workspace, deleted when it ends", {
  store <- new_store()
  dir <- NULL
  shiny::testServer(
    mod_project_server,
    args = list(store = store, hosted = TRUE),
    {
      dir <<- store$project_dir
      expect_true(dir.exists(dir))
      expect_true(all(project_folder_status(dir)$exists))
    }
  )
  expect_false(dir.exists(dir))
})

test_that("hosted, a typed folder path is ignored", {
  other <- local_fixture_project()
  store <- new_store()
  shiny::testServer(
    mod_project_server,
    args = list(store = store, hosted = TRUE),
    {
      own <- store$project_dir
      session$setInputs(path = other, open = 1)
      expect_identical(store$project_dir, own)
    }
  )
})

test_that("hosted, uploading a workspace replaces the session's workspace", {
  root <- withr::local_tempdir()
  saved <- new_workspace(root)
  writeLines("a,b", file.path(saved, "data", "export.csv"))
  zipfile <- file.path(root, "ws.zip")
  zip_workspace(saved, zipfile)

  store <- new_store()
  shiny::testServer(
    mod_project_server,
    args = list(store = store, hosted = TRUE),
    {
      first <- store$project_dir
      session$setInputs(
        upload_workspace = data.frame(
          name = "ws.zip",
          size = file.size(zipfile),
          type = "application/zip",
          datapath = zipfile
        )
      )
      expect_false(identical(store$project_dir, first))
      expect_false(dir.exists(first))
      expect_true(file.exists(file.path(
        store$project_dir,
        "data",
        "export.csv"
      )))
    }
  )
})

test_that("hosted, the app has no folder picker or Show in folder button", {
  withr::local_options(cleaningkitapp.server = TRUE)
  html <- as.character(app_ui(NULL))
  expect_false(grepl("project-browse", html, fixed = TRUE))

  withr::local_options(cleaningkitapp.server = FALSE)
  expect_true(grepl("project-browse", as.character(app_ui(NULL)), fixed = TRUE))
})

test_that("hosted, the whole app keeps uploads and outputs in the session workspace", {
  withr::local_options(cleaningkitapp.server = TRUE)
  src <- withr::local_tempdir()
  write_fixture_tool(file.path(src, "tool.xlsx"))
  write_fixture_export(file.path(src, "export.xlsx"), n = 8)
  upload <- function(name) {
    data.frame(
      name = name,
      size = file.size(file.path(src, name)),
      type = "",
      datapath = file.path(src, name)
    )
  }

  workspace <- NULL
  shiny::testServer(
    function(input, output, session) {
      store <- new_store()
      mod_project_server("project", store, hosted = TRUE)
      mod_tool_server("tool", store)
      mod_data_server("data", store)
      session$userData$store <- store
    },
    {
      store <- session$userData$store
      workspace <<- store$project_dir
      session$setInputs(`tool-upload` = upload("tool.xlsx"))
      session$setInputs(`data-upload` = upload("export.xlsx"))
      expect_true(file.exists(file.path(workspace, "resources", "tool.xlsx")))
      expect_true(file.exists(file.path(workspace, "data", "export.xlsx")))
      expect_identical(store$profile$tool$path, "resources/tool.xlsx")
    }
  )
  expect_false(dir.exists(workspace))
})
