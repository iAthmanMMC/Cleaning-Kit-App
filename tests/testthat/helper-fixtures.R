# Synthetic 4Mi-shaped fixtures. Nothing here comes from a real survey: the
# tool is a cut-down made-up XLSForm and the export is generated.

fixture_survey <- function() {
  data.frame(
    type = c(
      "start",
      "end",
      "today",
      "username",
      "select_one country",
      "text",
      "select_one yesno",
      "integer",
      "select_multiple reasons",
      "text"
    ),
    name = c(
      "start",
      "end",
      "today",
      "username",
      "Q13",
      "Q13_1",
      "Q20",
      "Q21",
      "Q30",
      "Q30_1"
    ),
    `label::English (en)` = c(
      NA,
      NA,
      NA,
      NA,
      "Country of interview",
      "Other country, specify",
      "Do you travel alone?",
      "How many people travel with you?",
      "Reasons for leaving",
      "Other reason, specify"
    ),
    `label::French (fr)` = c(
      NA,
      NA,
      NA,
      NA,
      "Pays de l'entretien",
      "Autre pays",
      "Voyagez-vous seul ?",
      "Combien de personnes ?",
      "Raisons du depart",
      "Autre raison"
    ),
    relevant = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "selected(${Q13}, '197')",
      NA,
      "selected(${Q20}, 'nope')",
      NA,
      "selected(${Q30}, '297')"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

fixture_choices <- function() {
  data.frame(
    list_name = c(
      rep("country", 3),
      rep("yesno", 2),
      rep("reasons", 3)
    ),
    name = c("kenya", "somalia", "197", "1", "2", "work", "safety", "297"),
    `label::English (en)` = c(
      "Kenya",
      "Somalia",
      "Other",
      "Yes",
      "No",
      "Work",
      "Safety",
      "Other"
    ),
    `label::French (fr)` = c(
      "Kenya",
      "Somalie",
      "Autre",
      "Oui",
      "Non",
      "Travail",
      "Securite",
      "Autre"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

write_fixture_tool <- function(path) {
  openxlsx::write.xlsx(
    list(survey = fixture_survey(), choices = fixture_choices()),
    path
  )
  invisible(path)
}

# An ONA-style export: a label row, then one row per survey.
fixture_export <- function(n = 10, start_id = 1, duplicate_uuid = FALSE) {
  ids <- seq(start_id, length.out = n)
  rows <- data.frame(
    start = sprintf("2026-10-%02d 09:00:00", (ids %% 28) + 1),
    end = sprintf("2026-10-%02d 09:40:00", (ids %% 28) + 1),
    today = sprintf("2026-10-%02d", (ids %% 28) + 1),
    username = paste0("enum_", (ids %% 3) + 1),
    Q13 = ifelse(ids %% 4 == 0, "Other", "Kenya"),
    Q13_1 = ifelse(ids %% 4 == 0, "Somaliland", NA),
    Q20 = "Yes",
    Q21 = as.character(ids %% 5),
    Q30 = "Work",
    Q30_1 = NA,
    `_uuid` = sprintf("uuid-%04d", ids),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (duplicate_uuid && n > 1) {
    rows$`_uuid`[2] <- rows$`_uuid`[1]
  }
  label_row <- as.data.frame(
    as.list(stats::setNames(paste0("label of ", names(rows)), names(rows))),
    check.names = FALSE
  )
  rbind(label_row, rows)
}

write_fixture_export <- function(path, ...) {
  openxlsx::write.xlsx(fixture_export(...), path)
  invisible(path)
}

# A project folder with the standard layout and the fixture tool.
local_fixture_project <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("ckapp-", .local_envir = env)
  for (f in c("data", "resources", "output/follow_ups", "output/final")) {
    dir.create(file.path(dir, f), recursive = TRUE, showWarnings = FALSE)
  }
  write_fixture_tool(file.path(dir, "resources", "tool.xlsx"))
  normalizePath(dir, winslash = "/")
}
