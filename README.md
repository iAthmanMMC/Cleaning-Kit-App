# cleaningkitapp

A point-and-click app for cleaning 4Mi survey data with the
[cleaningkit](https://github.com/mixedmigrationcentre/cleaningkit) package.
It walks you through the whole round—checking the raw KoboToolbox export,
making the review workbook, reading the reviewers' edits back and writing
the final output—without editing R scripts.

You can use it two ways:

- **Online**, with nothing to install. Each browser tab gets its own
  temporary workspace, which is deleted when you close or refresh the tab.
  Download the workspace to keep your work between days. See
  [docs/deployment.md](docs/deployment.md) for what happens to the data.
- **On your own computer.** Survey data never leaves your machine: the app
  only reads and writes files in the project folder you open.

## Install (once)

You need:

- **R 4.1 or newer**: <https://cran.r-project.org>
- **RStudio** (optional, but easier): <https://posit.co/download/rstudio-desktop/>
- **Excel**, for the reviewers who fill in the review workbook

Open R or RStudio and run:

```r
install.packages("remotes")
remotes::install_github("iAthmanMMC/Cleaning-Kit-App")
```

This also installs cleaningkit and everything else the app needs. The first
install can take a few minutes.

If the install asks *"Which packages would you like to update?"*, choose
**1: All**, or **3: None** if you're in a hurry.

## Start the app

```r
cleaningkitapp::run_app()
```

The app opens in your web browser. Keep the R window open while you work.
Closing it stops the app.

To open a project straight away:

```r
cleaningkitapp::run_app(project_dir = "C:/4Mi/round_12")
```

## Update to the latest version

Run the same install command again:

```r
remotes::install_github("iAthmanMMC/Cleaning-Kit-App")
```

Then restart R before starting the app.

## How a round works

The menus follow the two halves of a round.

First, on the **Project** tab, choose the project folder. The app checks
it has the standard folders (`data/`, `resources/`, `output/`) and can set
them up.

**Validate**

1. **Tool**: load the XLSForm.
2. **Data**: load the raw export. By default only records that were not
   cleaned in an earlier round are kept.
3. **Checks**: choose the checks and run them.
4. **Results**: see the flags per check and per enumerator.
5. **Review workbook**: create the workbook the reviewers fill in. It is
   saved in `output/follow_ups/`.

**Clean**

6. **Import reviewed**: the reviewers save their edited copy in
   `output/follow_ups/` with a name ending in `_edited` (for example
   `2026-11-20_follow-ups_edited.xlsm`). The app reads it and lists any rows
   that need fixing first.
7. **Apply**: apply the reviewed log and the "other" responses to the data.
8. **Final output**: compare the raw and clean data, then write the final
   workbook to `output/final/`.

Your settings are saved in `resources/cleaningkitapp.yml` inside the
project folder, so the next round starts where the last one ended. The
**Run log** tab shows everything cleaningkit printed while it worked.

## Getting help

Report problems or ask for features on the
[issues page](https://github.com/iAthmanMMC/Cleaning-Kit-App/issues). Never
attach survey data, cleaning logs or exports to an issue: they contain
personal data.
