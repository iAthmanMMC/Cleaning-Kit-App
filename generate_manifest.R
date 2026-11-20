# Write manifest.json for Posit Connect Cloud
#
#   source("generate_manifest.R")
#
# Run it on the machine whose R library the app was developed with: the
# manifest records that R version and those package versions, and Connect
# Cloud rebuilds exactly them. Regenerate and commit it whenever a package is
# added or updated, then push; Connect Cloud republishes from GitHub.
#
# The bundle is a named list of files, never the whole folder: the folder can
# hold .RData / .Rhistory (a saved R session, which may include a loaded
# dataset) and "Claude outputs/" (test projects). .rscignore covers the same
# ground for anyone who deploys another way.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect first: install.packages(\"rsconnect\")")
}

# Connect Cloud installs cleaningkit from GitHub, so the installed copy must
# have come from GitHub, not from a local folder.
ck <- utils::packageDescription("cleaningkit")
if (!identical(ck$RemoteType, "github")) {
  stop(
    "cleaningkit is not installed from GitHub. Run\n",
    "  pak::pkg_install(\"mixedmigrationcentre/cleaningkit\")\n",
    "and try again."
  )
}

files <- c(
  "app.R",
  "DESCRIPTION",
  "NAMESPACE",
  "LICENSE",
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("inst", recursive = TRUE, full.names = TRUE)
)

rsconnect::writeManifest(
  appDir = ".",
  appFiles = files,
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)
message("manifest.json written. Commit it and push to GitHub.")
