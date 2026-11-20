# Entry point for RStudio's "Run App" button and for Posit Connect Cloud.
# Users with R installed run cleaningkitapp::run_app() instead.
#
# A server runs this file non-interactively, so a non-interactive run is
# treated as hosted: one temporary workspace per browser session and no access
# to the server's folders (see R/hosted.R). This is on top of the
# CLEANINGKITAPP_SERVER=true environment variable set in Connect Cloud, so the
# app never offers a folder path on a server, even if that variable is lost.
if (!interactive()) {
  options(cleaningkitapp.server = TRUE)
}

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
cleaningkitapp:::ck_app()
