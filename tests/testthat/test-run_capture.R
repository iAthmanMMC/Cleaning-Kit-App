test_that("messages, warnings and printed lines are captured in order of level", {
  res <- capture_run(
    function() {
      message("reading")
      cat("\033[32m--> 10 entries\033[39m\n")
      warning("one odd row")
      42
    },
    step = "Test"
  )

  expect_identical(res$value, 42)
  expect_null(res$error)
  expect_setequal(res$log$text, c("reading", "--> 10 entries", "one odd row"))
  expect_identical(res$log$level[res$log$text == "one odd row"], "warning")
  expect_true(all(res$log$step == "Test"))
})

test_that("an error is returned, not thrown", {
  res <- capture_run(function() stop("bad sheet"), step = "Test")

  expect_null(res$value)
  expect_identical(res$error, "bad sheet")
  expect_identical(res$log$level, "error")
})

test_that("the call runs in the given directory and the old one is restored", {
  dir <- normalizePath(withr::local_tempdir(), winslash = "/")
  before <- getwd()

  res <- capture_run(
    function() normalizePath(getwd(), winslash = "/"),
    step = "Test",
    dir = dir
  )

  expect_identical(res$value, dir)
  expect_identical(getwd(), before)
})

test_that("a call with no output gives an empty log with the right columns", {
  res <- capture_run(function() invisible(NULL), step = "Test")
  expect_identical(names(res$log), c("time", "step", "level", "text"))
  expect_identical(nrow(res$log), 0L)
})
