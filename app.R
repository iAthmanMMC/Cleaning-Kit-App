# Launch the app from RStudio's "Run App" button while developing.
# Users run cleaningkitapp::run_app() once the package is installed.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
cleaningkitapp:::ck_app()
