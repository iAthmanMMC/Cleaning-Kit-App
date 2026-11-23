# Fixtures for the upload flows: "Check new data" and "Apply corrections".

# A fileInput() value for files in `dir`.
as_upload <- function(dir, files) {
  data.frame(
    name = files,
    size = file.size(file.path(dir, files)),
    type = "",
    datapath = file.path(dir, files),
    stringsAsFactors = FALSE
  )
}

# Start a round quietly, in `dir`.
quiet_round <- function(dir, ...) {
  res <- capture_run(
    function() start_round(..., dir = dir),
    step = "test",
    dir = dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }
  res$value
}

# A round checked with "Check new data", and the workbook a reviewer filled
# in and saved under a name of their own. The reviewer's decisions are those
# of local_reviewed_project(). Returns the source folder, the file names and
# the round.
local_round_workbook <- function(n = 12, env = parent.frame()) {
  src <- withr::local_tempdir(.local_envir = env)
  dir <- new_workspace(
    withr::local_tempdir(.local_envir = env),
    templates = TRUE
  )
  write_fixture_export(file.path(src, "ona.xlsx"), n = n)
  write_fixture_tool(file.path(src, "form.xlsx"))
  write_fixture_checklist(file.path(src, "rules.xlsx"))

  round <- quiet_round(
    dir,
    download = as_upload(src, "ona.xlsx"),
    xlsform = as_upload(src, "form.xlsx"),
    checklist = as_upload(src, "rules.xlsx")
  )
  round$profile$review$vba <- FALSE
  ctx <- check_context(
    raw_data = round$raw_data,
    tool_survey = round$tool$tool_survey,
    tool_choices = round$tool$tool_choices,
    other_db = round$tool$other_db,
    general = round$profile$general,
    project_dir = dir
  )
  settings <- default_check_settings()
  settings$outliers$args$columns_to_check <- "Q21"
  logs <- list()
  for (id in c("duration", "country_of_interview", "outliers", "logical")) {
    logs[[check_log_name(ck_checks()[[id]])]] <-
      run_check_quietly(id, ctx, settings[[id]]$args)
  }
  other <- run_check_quietly("other_responses", ctx)

  path <- file.path(src, round_review_file("Week 1", vba = FALSE))
  res <- capture_run(
    function() {
      combined <- cleaningkit::create_combined_log(
        c(list(checked_dataset = round$raw_data), logs)
      )
      build_review_workbook(
        combined,
        path,
        round$profile$review,
        round$profile$general,
        other,
        round$tool$other_db,
        round_info = make_round_info(round, "Week 1")
      )
    },
    step = "fixture",
    dir = dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }

  # the reviewer's edits, saved into the same workbook as Excel would
  cl <- as.data.frame(
    readxl::read_excel(path, "cleaning_log", col_types = "text"),
    check.names = FALSE
  )
  oth <- as.data.frame(
    readxl::read_excel(path, "other_responses", col_types = "text"),
    check.names = FALSE
  )
  set_action <- function(cl, uuid, question, action, value = NA) {
    i <- which(cl$`Survey UUID` == uuid & cl$`Question number` == question)
    cl$`Action taken`[i] <- action
    cl$`New value`[i] <- value
    cl
  }
  cl <- set_action(cl, "uuid-0006", "_duration", "discard")
  cl <- set_action(cl, "uuid-0007", "Q21", "recoded", "4")
  cl <- set_action(cl, "uuid-0005", "Q31", "recoded", "Somalia")
  # review columns by header, not position: columns such as `translation`
  # can be added before them
  col <- function(pattern) grep(pattern, names(oth), ignore.case = TRUE)[1]
  oth[[col("^input translation")]][oth$uuid == "uuid-0004"] <- "Somaliland region"
  oth[[col("^correct to existing")]][oth$uuid == "uuid-0008"] <- "Somalia"
  oth[[col("^invalid other")]][oth$uuid == "uuid-0012"] <- "Yes"
  wb <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(wb, "cleaning_log", cl)
  openxlsx::writeData(wb, "other_responses", oth)
  edited <- "checked by Amina.xlsx"
  openxlsx::saveWorkbook(wb, file.path(src, edited), overwrite = TRUE)

  list(
    src = src,
    workbook = edited,
    download = "ona.xlsx",
    form = "form.xlsx",
    round = round
  )
}

# Start applying quietly, in a fresh workspace.
quiet_apply <- function(...) {
  dir <- new_workspace(withr::local_tempdir(.local_envir = parent.frame()))
  res <- capture_run(
    function() start_apply(..., dir = dir),
    step = "test",
    dir = dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }
  res$value$dir <- dir
  res$value
}
