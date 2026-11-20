# cleaningkitapp: plan for a Shiny UI over cleaningkit

Status: proposal, nothing built yet. Written 2026-11-20 against cleaningkit
`2026.08.0` (branch `feat-app-with-ui`).

## 1. What we are wrapping

cleaningkit is ~17,500 lines of R across 37 files and about 50 exported
functions. Almost all of it follows the same pattern: data frames in, a list
of data frames out. Users run it today by editing two scripts in
`inst/examples/`. The app should replace that script editing, and leave the
package as the engine.

The workflow has two halves with a manual Excel review in between:

```
 PHASE A: VALIDATE (script 1)                    OFFLINE              PHASE B: CLEAN (script 2)
 ─────────────────────────────                   ───────              ─────────────────────────
 setup_project_folders()                                              read_tool_* / get_other_db()
 read_tool_survey / read_tool_choices                                 read_raw_data()
 get_other_labels / get_other_db                                      check_log_files()  (x2 sheets)
 filter_new_records()  (ledger, archive)                              read_cleaning_log()
 read_raw_data()                                                      evaluate_cleaning_log()
 prepare_other_responses()                  reviewers fill in         apply_cleaning_log()
 validate_* x10 (+3 optional)       ──►     follow-ups.xlsm     ──►   read_other_responses()
 create_combined_log()                      in Excel                  apply_other_responses()
 create_review_workbook()                                             combine_reviewed_logs()
                                                                      review_cleaned_data()
                                                                      export_final_output()
```

### The checks, and how they fit a UI

| Check | Needs | UI notes |
|---|---|---|
| `validate_duration` | data | 2 numeric thresholds and a toggle |
| `validate_completeness` | data | `min_content_cells`; `metadata_cols` multi-select |
| `validate_refused` | data | `max_refused`, `refused_value` |
| `validate_back_to_back` | data | column pickers (enumerator/start/end), hours/mins |
| `validate_country_of_interview` | data | 3 question-code pickers (Q13/Q31/Q41) |
| `validate_interview_time` | data | hour range slider |
| `validate_outliers` | data, tool_survey | pick integer questions; strongness factor |
| `validate_interview_location` | data, **internet** | slow: geocodes via Nominatim at 1 s per city. Needs a background task with progress |
| `validate_repeated_entries` | data, tool_choices | `ck_repeated_entries_sections()` / `ck_country_aliases()` are editable tables |
| `validate_logical_with_list` | data, checklist xlsx | upload or edit the checklist |
| `validate_similar_surveys` | data, tool_survey | not in the example script. O(n²) Gower distance, so slow on big rounds. Background task |
| `validate_spatial_proximity` | data | not in the example script |
| `validate_similar_questions` | data | not in the example script |

Each check returns `dataset[[log_name]] <- log` with the columns
`uuid / old_value / question / issue / check_binding`, and
`create_combined_log()` stacks them. That shared shape is what makes the app
manageable: the UI can treat every check the same way.

### Findings that shape the design

1. **No Excel or COM is needed at runtime.** The `.xlsm` is made by
   `add_vba_project()`, which zips the prebuilt `cleaningkit_vba.bin` into the
   workbook. Only `dev/build_vba_bin.R` uses COM, so the app can create
   macro-enabled review workbooks on any machine, and even on a server.
2. **Console output.** The package prints progress with 78 `message()`,
   66 `warning()` and 26 `cat()` calls. Messages and warnings can be captured
   with `withCallingHandlers()` and shown in a run-log panel. `cat()` cannot
   be captured cleanly from a background process, so those 26 calls should
   become `message()` (a small package change, see section 6).
3. **File and folder functions.** `filter_new_records()` (ledger and archive),
   `setup_project_folders()` and `check_log_files()` work on folders.
   `read_cleaning_log()` and `read_other_responses()` take either a folder or
   a single file. Shiny's `fileInput` saves uploads as `0.xlsx`, `1.xlsx`, so
   any `file_pattern` match fails. The app should copy uploads into a temp
   folder under their original names before calling these functions.
4. **The workflow has state that lasts between rounds** (the processed-UUID
   ledger, the archive, `output/follow_ups/`). The app has to keep that, or
   round 2 will include round 1's records again.
5. **Settings are 4Mi-specific but overridable** (Q-codes, `_uuid`,
   `username`, `ck_*()` defaults). Users should not have to re-enter them each
   round, so the app needs a saved project profile.

## 2. Key decision: where the app runs

This is personal data about people on the move, which narrows the options:

| Option | Pros | Cons |
|---|---|---|
| **A. Local desktop app** (recommended to start) | data never leaves the laptop; full folder access (ledger, archive); no server cost; matches how the scripts are used today | each user needs R installed (or a bundled installer later) |
| B. Internal server (Posit Connect / Shiny Server inside the DRC/MMC network) | nothing to install; one version for all | needs IT approval and a DPIA; upload-only (no local folders), so the ledger has to live on the server per project; auth required |
| C. Public hosting (shinyapps.io) | easy | **not acceptable** for this data |

Recommendation: **build for A, keep B possible.** Put all file access behind
one small "project storage" layer: a local folder now, a server project
directory later. Then moving to B is mostly a deployment job, not a rewrite.

## 3. Architecture

**Structure the app as an R package**, the same way cleaningkit is built: a
`run_app()` entry point, Shiny modules in `R/mod_*.R`, testthat tests, and
`pkg::fn()` calls. Plain package conventions are enough, so golem isn't
required.

```
cleaningkitapp/
  DESCRIPTION            Imports: shiny, bslib, cleaningkit, DT or reactable,
                         shinyFiles, mirai, yaml, openxlsx, readxl
  R/
    run_app.R            run_app(project_dir = NULL)
    app_ui.R / app_server.R
    check_registry.R     one entry per validate_* check (see below)
    project_store.R      read/write profile, ledger and output paths
    run_capture.R        runs a cleaningkit call, captures messages/warnings
    mod_project.R        Step 0: project folder + profile
    mod_tool.R           Step 1: XLSForm
    mod_data.R           Step 2: raw data, filter_new_records
    mod_checks.R         Step 3: configure + run checks (built from the registry)
    mod_results.R        Step 4: combined log browser, enumerator dashboard
    mod_review_wb.R      Step 5: build + download the review workbook
    mod_import_review.R  Step 6: upload reviewed workbooks, check + evaluate
    mod_apply.R          Step 7: apply log + other responses
    mod_final.R          Step 8: review_cleaned_data + export_final_output
    mod_runlog.R         run log panel shown on every page
  inst/app/www/          MMC logo, CSS
  inst/app/_brand.yml    MMC colours (bslib / brand.yml)
  tests/testthat/        testServer() module tests + shinytest2 smoke test
```

### UI layout (bslib)

`page_navbar` with two sections that match the two phases, plus a sidebar
that tracks progress:

- **Project**: open or create a project folder (`shinyFiles` directory picker
  → `setup_project_folders()`), load or save the profile.
- **Validate**: Tool → Data → Checks → Results → Review workbook
- **Clean**: Import reviewed → Evaluate → Apply → Final output
- **Tools** (secondary): tool constraint QA (`check_constraints`),
  label lookups, `combine_sm_questions`, `rename_sm_choice_label`.

A step stays locked until its inputs exist (for example, Checks needs the tool
and the data), and the progress sidebar shows ✓ / ⚠ / ✗ for each step.

### The check registry: build the UI from data, not by hand

Rather than writing 13 separate panels, describe each check once:

```r
ck_app_checks <- list(
  duration = list(
    label    = "Interview duration",
    fn       = cleaningkit::validate_duration,
    needs    = "data",
    slow     = FALSE,
    args = list(
      column_to_check = arg_column("Duration column", default = "_duration"),
      lower_bound     = arg_number("Min minutes", 15),
      upper_bound     = arg_number("Max minutes", 60),
      flag_above_upper= arg_toggle("Also flag long interviews", FALSE)
    )
  ),
  interview_location = list(..., needs = c("data", "internet"), slow = TRUE),
  ...
)
```

`mod_checks` loops over the registry and draws one accordion panel per check:
an enable switch, the argument inputs (column pickers take their choices from
`names(raw_data)`), and a "last run" badge showing the row count. Common
arguments (`dataset`, `uuid_column`, `skip_label_row`, `tool_survey`,
`tool_choices`) are filled in automatically from project state. A new check
added to cleaningkit then needs one registry entry and nothing else.

Defaults come from the `formals()` of the real function, so the app and the
package can't drift apart. Add a unit test that every registry argument exists
in the function's formals.

### State

A single `reactiveValues` "session store" holds `tool_survey`,
`tool_choices`, `other_db`, `raw_data`, `logs` (named list),
`combined_log`, `reviewed_log`, `evaluation`, `clean_data` and `final_log`.
Every module receives the store and writes only its own slots.

The **project profile** (`<project>/resources/cleaningkitapp.yml`) stores all
settings: column names, Q-codes, thresholds, enabled checks, valid actions,
issue labels and readme fields. It is loaded at startup and saved on change.
That replaces the CUSTOMISE blocks in the scripts, and it can be shared with a
colleague working on the same survey.

### Long-running work

`validate_interview_location`, `validate_similar_surveys` and
`apply_cleaning_log` on large rounds can take minutes. Run them with
`shiny::ExtendedTask` + `mirai`, so the UI stays responsive. Show a progress
bar and stream the run log. Cache the Nominatim geocodes in the project folder
so a re-run doesn't query them again (this needs a small package hook, see
section 6).

### Showing results

- Value boxes: records checked, flags per check, unique surveys flagged.
- Combined log table (reactable/DT): filter by check, enumerator or date. A
  click opens the full record.
- **Enumerator dashboard** using `prioritize_enumerators()`: ranked table plus
  a bar chart. It is already in the package but not in the scripts, and it is
  the most useful view for supervisors.
- Phase B: an evaluation table with problems highlighted
  (`flag_issues_inline = TRUE`), a before/after diff from
  `review_cleaned_data()`, and the audit log from `apply_cleaning_log()`.

Downloads are always produced into `<project>/output/...` (local mode) and
offered through `downloadHandler`.

## 4. Screen-by-screen mapping

| Step | Screen | Calls | Inputs exposed |
|---|---|---|---|
| 0 | Project | `setup_project_folders` | folder, extra folders, copy templates |
| 1 | Tool | `read_tool_survey/choices`, `get_other_labels`, `get_other_db`, `check_constraints` | XLSForm upload, label column, language, extra `other_text_types` |
| 2 | Data | `filter_new_records`, `read_raw_data` | export upload(s), "new records only" toggle (ledger), uuid column, sheet; preview of n records, new vs already processed |
| 3 | Checks | registry → `validate_*`; `prepare_other_responses` | per-check panels; "Run selected" / "Run all" |
| 4 | Results | `create_combined_log`, `prioritize_enumerators` | filters; weights for prioritisation |
| 5 | Review workbook | `create_review_workbook` | vba on/off, colour mode, group_by, flag columns, include dataset → download `.xlsm` |
| 6 | Import reviewed | `check_log_files` (both sheets), `read_cleaning_log`, `evaluate_cleaning_log` | multi-file upload; column-name mapping; valid actions. Blocks moving on while errors remain, with "download annotated log" to send back to reviewers |
| 7 | Apply | `apply_cleaning_log`, `read_other_responses`, `apply_other_responses`, `combine_reviewed_logs` | action-value mapping, apply repeated entries toggle |
| 8 | Final | `review_cleaned_data`, `export_final_output` | readme fields, sheet names → download final workbook; on confirm, the ledger is committed |

The ledger in step 2 should only be committed **after** step 8 succeeds (or
on an explicit "mark round as processed" button). Otherwise a failed round
silently drops records from the next round. `filter_new_records()` writes the
ledger immediately today, so this needs a small package change (section 6).

## 5. Milestones

| # | Deliverable | Rough size |
|---|---|---|
| M0 | Package skeleton, `run_app()`, bslib navbar, MMC theme, run-log module, CI (R CMD check) | 1–2 days |
| M1 | Project + Tool + Data screens, profile load/save | 3–4 days |
| M2 | Check registry with the 10 checks from script 1, results table, review workbook download. **This is the first usable release: it replaces script 1.** | 1–1.5 weeks |
| M3 | Phase B screens 6–8. **Replaces script 2.** | 1–1.5 weeks |
| M4 | Background tasks for the slow checks, geocode cache, enumerator dashboard, the 3 optional checks, logical-checklist editor | 1 week |
| M5 | Hardening: shinytest2 end-to-end run on a synthetic dataset, user guide, desktop launcher (a `.bat`/shortcut calling `cleaningkitapp::run_app()`, later possibly a bundled R-portable installer); decide on option B | 1 week |

## 6. Changes needed in cleaningkit itself

All small, all useful outside the app too:

1. **`cat()` → `message()`** (26 calls) so the app can capture and show the
   output.
2. **`filter_new_records(commit_ledger = TRUE)`**: let the caller defer writing
   the ledger, and export a `commit_ledger()` helper.
3. **Geocode cache** for `validate_interview_location()` (`cache_path =
   NULL` argument).
4. Optional **`progress` callback** argument on the slow checks
   (`validate_interview_location`, `validate_similar_surveys`), defaulting
   to `NULL`, for real progress bars.
5. Optional: export `ck_valid_actions()` / `ck_metadata_cols()` defaults, so
   the app and scripts use one source for the values that are repeated in
   both example scripts today.

Each of these gets a NEWS entry and a test, following cleaningkit's
conventions.

## 7. Testing and data safety

- No real survey data in either repo. Build a synthetic 4Mi-shaped fixture
  (≈200 rows, a cut-down tool) in `tests/testthat/fixtures/`, generated by a
  script, and use it for both the module tests and the shinytest2 run.
- `testServer()` tests per module. One registry test (args exist in
  `formals()`). One shinytest2 test for the full Phase A → B flow with a
  pre-filled "reviewed" workbook fixture.
- Temp copies of uploads live in a per-session `tempdir()` subfolder, deleted
  in `session$onSessionEnded`.
- `options(shiny.maxRequestSize = 200 * 1024^2)` for large exports.

## 8. Open questions for you

1. **Deployment**: desktop only (A), or should an internal server (B) be
   planned for from the start?
2. **Users**: only data/IM staff who have R, or country teams who don't? This
   decides whether M5 needs a no-R installer.
3. **Multiple reviewers**: should the app split the review workbook per
   enumerator or per reviewer? `read_cleaning_log` already combines several
   edited files.
4. **Optional checks**: should `validate_similar_surveys`,
   `validate_spatial_proximity` and `validate_similar_questions` be on by
   default, off, or hidden behind "advanced"?
5. **Kobo API**: pull exports straight from KoboToolbox/ONA (token in
   `.Renviron`, never typed into the app), or keep file upload only?
