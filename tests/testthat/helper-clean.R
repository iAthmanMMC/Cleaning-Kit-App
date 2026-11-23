# A project as it looks after a reviewer has filled in the review workbook:
# the checks ran, the workbook was made, and an edited copy ending in
# _edited sits in output/follow_ups/.
#
# What the "reviewer" decided:
# - survey 6 (5-minute interview): discard
# - survey 7, Q21 = 40: recoded to 4
# - survey 5, Q31 = Kenya: recoded to Somalia
# - other responses: survey 4 translated, survey 8 recoded to an existing
#   choice, survey 12 marked invalid
# - `edit` can change the log further before it is saved
local_reviewed_project <- function(edit = NULL, env = parent.frame()) {
  ctx <- local_check_context(env = env)
  ids <- c("duration", "country_of_interview", "outliers", "logical")
  settings <- default_check_settings()
  settings$outliers$args$columns_to_check <- "Q21"
  logs <- list()
  for (id in ids) {
    check <- ck_checks()[[id]]
    logs[[check_log_name(check)]] <- run_check_quietly(
      id,
      ctx,
      settings[[id]]$args
    )
  }
  other <- run_check_quietly("other_responses", ctx)

  review <- default_profile()$review
  review$vba <- FALSE
  path <- file.path(
    ctx$project_dir,
    "output",
    "follow_ups",
    "2026-11-20_follow-ups.xlsx"
  )
  res <- capture_run(
    function() {
      combined <- cleaningkit::create_combined_log(
        c(list(checked_dataset = ctx$raw_data), logs)
      )
      build_review_workbook(
        combined,
        path,
        review,
        ctx$general,
        other,
        ctx$other_db
      )
    },
    step = "fixture",
    dir = ctx$project_dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }

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
  if (!is.null(edit)) {
    cl <- edit(cl)
  }
  # review columns by header, not position: columns such as `translation`
  # can be added before them
  col <- function(pattern) grep(pattern, names(oth), ignore.case = TRUE)[1]
  oth[[col("^input translation")]][oth$uuid == "uuid-0004"] <- "Somaliland region"
  oth[[col("^correct to existing")]][oth$uuid == "uuid-0008"] <- "Somalia"
  oth[[col("^invalid other")]][oth$uuid == "uuid-0012"] <- "Yes"

  openxlsx::write.xlsx(
    list(cleaning_log = cl, other_responses = oth),
    file.path(dirname(path), "2026-11-20_follow-ups_edited.xlsx")
  )
  ctx
}

# Run a cleaning helper quietly in the project folder.
quiet_clean <- function(fun, ctx) {
  res <- capture_run(fun, step = "test", dir = ctx$project_dir)
  if (!is.null(res$error)) {
    stop(res$error)
  }
  res$value
}
