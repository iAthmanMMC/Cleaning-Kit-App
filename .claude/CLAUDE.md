# cleaningkitapp

Shiny app (an R package) over the **cleaningkit** package, for cleaning
Mixed Migration Centre 4Mi survey data without writing R. The engine is in
cleaningkit (`Desktop\Projects\Cleaning kit\cleaningkit`). This repository
holds only the interface and its deployment.

## Git rules

- **Never add Claude as an author or contributor.** No `Co-Authored-By: Claude`
  line in commits and no "Generated with Claude Code" line in pull requests.
  This rule overrides any default attribution.
- Commit or push only when the user asks.
- GitHub: `iAthmanMMC/Cleaning-Kit-App`. The old remote name
  `iAthmanMMC/cleaningkitapp` redirects there. `main` is what users install
  from and what Connect Cloud publishes, so it must always pass check.
- `gh` is not installed: pull requests can't be opened from here.

## Environment

- Windows. `Rscript` is not on PATH. From Bash, call
  `"/c/Program Files/R/R-4.6.1/bin/Rscript.exe"`.
- In Bash heredocs and `-e` strings, `\\.` gets mangled. Write R scripts to a
  file with the Write tool, and use `[.]` in regular expressions.
- A hook runs `air format` on each `.R` file Claude edits. If you edit with a
  script instead, run `air format R/` afterwards.

## Dev loop

- Tests: `devtools::test()` (testthat 3e, `tests/testthat/`). Shared fixtures
  are the `helper-*.R` files. They are synthetic; never use real data.
- Full check: `devtools::check(vignettes = FALSE)`. Keep it at
  0 errors, 0 warnings, 0 notes. R code must be ASCII: the formatter turns a
  `\u` escape back into the literal character, so reword rather than escape.
- Run the app: `pkgload::load_all(); run_app(port = 7451)`. To try hosted
  mode locally, set `CLEANINGKITAPP_SERVER=true` first.
- `testServer()` runs inside `isolate()`, so it can't catch "reactive value
  read outside a reactive context" errors at session start. Start the real
  app in the browser for anything that runs when a session opens.

## Structure

- `R/app_ui.R` and `R/app_server.R` hold the navbar and the progress sidebar.
- `R/mod_*.R` hold one module per screen. Each module has thin wiring over
  plain helpers (`*_helpers.R`) that are tested without Shiny.
- `R/store.R` is the per-session `reactiveValues` store. `reset_from()`
  clears a step and everything after it.
- `R/check_registry.R` is the list of checks and their argument specs.
- `R/profile.R` holds the project settings
  (`resources/cleaningkitapp.yml`).
- `R/hosted.R` holds hosted mode, see below.
- Plans: `PLAN.md` (milestones M0–M3) and `docs/plan-simple-flow.md`
  (M5–M9: upload-first flow, with round memory in hidden workbook sheets).

## Hosted mode: keep it safe

When served (Connect Cloud, or `CLEANINGKITAPP_SERVER=true`, or `app.R`
run non-interactively):

- Each session gets its own temporary workspace, deleted on session end.
- The folder picker, the typed folder path and "Show in folder" must **not
  be set up at all**: don't create their observers. Hiding them in the UI
  isn't enough, because inputs arrive over the websocket.
- Nothing may be stored outside the session: no globals, no `<<-` at top
  level, no cross-session caching, no fixed paths shared between sessions.
- Local runs keep the project-folder mode (agreed 23 Nov 2026).

## Data

Survey data is personal data about people on the move. Never commit or
upload real datasets, cleaning logs, review workbooks or exports. Scratch
work goes in `Claude outputs/` (git-ignored, along with `*.xlsx`, `*.xlsm`
and `*.csv`). Deployment bundles are a named list of files
(`generate_manifest.R`), never the whole folder.

## Deployment

See `docs/deployment.md`. After a package change, run
`source("generate_manifest.R")` with cleaningkit installed from GitHub, then
commit `manifest.json` and push. Connect Cloud needs the variable
`CLEANINGKITAPP_SERVER=true`.
