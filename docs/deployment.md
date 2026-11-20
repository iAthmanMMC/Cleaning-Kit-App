# Deploying to Posit Connect Cloud

The online copy runs on **Posit Connect Cloud** (connect.posit.cloud), published
from this GitHub repository. This page covers how it gets there, what the
hosted mode changes, and what happens to the data people upload.

## Publish or update

1. Make sure cleaningkit is installed **from GitHub**. Connect Cloud installs it
   from there, at the exact commit you have:

   ```r
   pak::pkg_install("mixedmigrationcentre/cleaningkit")
   ```

2. From the project folder, write the manifest:

   ```r
   source("generate_manifest.R")
   ```

   It records your R version and package versions. Run it again whenever a
   package changes (including a new cleaningkit), then commit `manifest.json`
   and push to `main`.

3. The first time only, in Connect Cloud: **Publish → Shiny**, choose the
   `iAthmanMMC/Cleaning-Kit-App` repository, branch `main`, primary file
   `app.R`. Under **Advanced settings → Variables**, add

   ```
   CLEANINGKITAPP_SERVER = true
   ```

   After that, Connect Cloud republishes when `main` changes (if automatic
   publishing is on), or use **Republish** on the content page.

## What hosted mode changes

The app knows it is hosted from `CLEANINGKITAPP_SERVER=true`, from Posit's own
environment variables, or from `app.R` being run non-interactively. Then:

- **One workspace per browser tab.** Each session gets its own temporary
  project folder with a random name, created when the tab opens and deleted
  when the session ends: the tab is closed or refreshed, or the connection
  drops. Nothing else on the server can be opened from the app.
- **No folder picker and no typed folder path.** Their server-side code is
  never set up when hosted, so no message sent to the app can make it list or
  open a folder on the server, such as another user's workspace.
- **No "Show in folder".** The review workbook, the final output and the whole
  workspace are downloads.
- **Rounds over several days.** Download the workspace (`.zip`) before closing
  the tab and upload it on the Project screen to carry on. It holds the data,
  settings, logs and the ledger of surveys already cleaned.

## What happens to uploaded data

- **During a session** the uploaded files sit in that session's workspace on
  the server's disk and the data in that session's memory. Other sessions
  cannot see either: every session has its own store and its own folder.
- **When the session ends** the workspace folder is deleted, and Shiny deletes
  its own copies of the uploads.
- **When the app goes idle** Connect Cloud stops it and its filesystem is
  reset, so anything left behind (for example after a crash) goes too.
- **What it does not change**: while a session runs, the data is on Posit's
  servers (AWS, region not selectable). Posit encrypts it at rest and in
  transit and holds a SOC 2 Type I attestation, but it is a third-party
  processor. The app is public: anyone with the link can use it for their own
  data, which is intended.
- The interview location check, if switched on, sends **city and country
  names** (not respondent data) to OpenStreetMap's Nominatim service.

For data that must not leave your computer, install the app and run it locally
(see the README).
