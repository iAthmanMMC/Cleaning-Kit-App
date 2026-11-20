# A logical checklist on the fixture's columns.
write_fixture_checklist <- function(path) {
  openxlsx::write.xlsx(
    data.frame(
      module = "core",
      check_id = c("check_01", "check_02"),
      description = c(
        "Travels alone but says people travel with them",
        "Nationality is the country of interview"
      ),
      check_to_perform = c(
        "Q20 == \"Yes\" & as.numeric(Q21) > 3",
        "Q13 == Q31"
      ),
      columns_to_clean = c("Q20, Q21", "Q13, Q31")
    ),
    path
  )
  invisible(path)
}

# Raw data, tool and context for running checks on the fixture.
local_check_context <- function(n = 12, env = parent.frame()) {
  dir <- local_fixture_project(env = env)
  write_fixture_export(file.path(dir, "data", "data.xlsx"), n = n)
  write_fixture_checklist(file.path(dir, "resources", "logical_checklist.xlsx"))
  res <- capture_run(
    function() {
      tool <- load_tool(file.path(dir, "resources", "tool.xlsx"))
      raw <- cleaningkit::read_raw_data(
        file.path(dir, "data", "data.xlsx"),
        tool_survey = tool$tool_survey
      )
      list(tool = tool, raw = raw)
    },
    step = "fixture",
    dir = dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }
  check_context(
    raw_data = res$value$raw,
    tool_survey = res$value$tool$tool_survey,
    tool_choices = res$value$tool$tool_choices,
    other_db = res$value$tool$other_db,
    general = default_profile()$general,
    project_dir = dir
  )
}

# Run one check quietly, in the project folder.
run_check_quietly <- function(id, ctx, args = NULL) {
  checks <- ck_checks()
  settings <- default_check_settings(checks)[[id]]$args
  if (!is.null(args)) {
    settings[names(args)] <- args
  }
  res <- capture_run(
    function() run_check(checks[[id]], settings, ctx),
    step = id,
    dir = ctx$project_dir
  )
  if (!is.null(res$error)) {
    stop(res$error)
  }
  res$value
}
