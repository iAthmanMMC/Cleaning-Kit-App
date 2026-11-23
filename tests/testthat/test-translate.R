# Machine translation of the "other" responses. Nothing here contacts Google:
# only "off" and "preview" are run, and neither sends anything.

test_that("translation is off by default", {
  expect_identical(
    default_check_settings()$other_responses$args$translate,
    "off"
  )
  expect_true(default_check_settings()$other_responses$args$translate_options)
})

test_that("only 'send' sends; anything unknown counts as off", {
  expect_true(translate_arg("send"))
  expect_identical(translate_arg("preview"), "preview")
  expect_false(translate_arg("off"))
  expect_false(translate_arg(""))
  expect_false(translate_arg(NULL))
  expect_false(translate_arg("yes"))
})

test_that("the settings become prepare_other_responses() arguments", {
  args <- build_other_responses(
    list(translate = "send", translate_options = FALSE),
    list(project_dir = "C:/proj")
  )
  expect_true(args$translate)
  expect_false(args$translate_options$mask_numbers)
  expect_identical(
    args$translate_options$cache_file,
    file.path("C:/proj", "output", "translation_cache.rds")
  )

  # no project folder: no cache, numbers hidden
  args <- build_other_responses(list(translate = "off"), list())
  expect_false(args$translate)
  expect_true(args$translate_options$mask_numbers)
  expect_null(args$translate_options$cache_file)
})

test_that("off adds an empty translation column and no preview", {
  ctx <- local_check_context()
  other <- run_check_quietly("other_responses", ctx)
  expect_true("translation" %in% names(other))
  expect_true(all(is.na(other$translation)))
  expect_null(translation_preview(other))
})

test_that("preview lists the texts and sends nothing", {
  ctx <- local_check_context()
  other <- run_check_quietly(
    "other_responses",
    ctx,
    args = list(translate = "preview")
  )
  preview <- translation_preview(other)
  expect_s3_class(preview, "data.frame")
  expect_true(all(c("text_to_send", "n_responses") %in% names(preview)))
  expect_true(all(preview$text_to_send %in% other$response))
  # nothing was sent, so nothing was translated
  expect_true(all(is.na(other$translation)))
  expect_false(file.exists(
    file.path(ctx$project_dir, "output", "translation_cache.rds")
  ))
})

test_that("the preview box says nothing was sent", {
  box <- as.character(translation_preview_box(
    data.frame(text_to_send = "a", n_responses = 1L),
    htmltools::div("table")
  ))
  expect_match(box, "nothing was sent")
  expect_match(box, "1 distinct text")
  empty <- as.character(translation_preview_box(
    data.frame(text_to_send = character(0)),
    htmltools::div("table")
  ))
  expect_match(empty, "nothing new to translate")
})

test_that("the run offers Off and Translate; the preview has its own button", {
  expect_identical(unname(translate_run_choices()), c("off", "send"))
})

test_that("'Preview the texts' runs only the other responses and sends nothing", {
  src <- withr::local_tempdir()
  write_fixture_export(file.path(src, "ona.xlsx"), n = 12)
  write_fixture_tool(file.path(src, "form.xlsx"))

  shiny::testServer(mod_check_flow_server, {
    session$setInputs(
      first = "first",
      download = as_upload(src, "ona.xlsx"),
      xlsform = as_upload(src, "form.xlsx"),
      start = 1
    )
    expect_false(is.null(round()))

    session$setInputs(preview_translation = 1)
    preview <- translation_check()$preview
    expect_s3_class(preview, "data.frame")
    expect_gt(nrow(preview), 0)
    # no check ran, so there are no results yet
    expect_null(outcome())

    # running the checks replaces the preview with the results
    session$setInputs(checks = "other_responses", translate = "off", run = 1)
    expect_null(translation_check())
    expect_false(is.null(outcome()))
    expect_true(all(is.na(outcome()$other_responses$translation)))
  })
})
