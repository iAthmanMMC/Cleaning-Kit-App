#' MMC colours used across the app
#'
#' The same palette `cleaningkit::export_final_output()` uses for the final
#' workbook, so the app and the Excel outputs look alike.
#'
#' @noRd
.mmc <- list(
  navy = "#003D58",
  teal = "#00A2A5",
  light_teal = "#AFDFE4",
  pale_teal = "#D5EEF0",
  yellow = "#FFE07C",
  green = "#5B9E62",
  orange = "#FBBC75",
  red = "#C8553D"
)

#' bslib theme for the app
#' @noRd
ck_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = .mmc$navy,
    secondary = .mmc$teal,
    success = .mmc$green,
    info = .mmc$teal,
    warning = .mmc$yellow,
    danger = .mmc$red,
    base_font = bslib::font_collection(
      "Segoe UI",
      "Roboto",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    ),
    "navbar-bg" = .mmc$navy
  ) |>
    bslib::bs_add_rules(
      "
      .ck-status-list { list-style: none; padding-left: 0; margin: 0; }
      .ck-status-list li { display: flex; gap: .5rem; align-items: baseline;
                           padding: .35rem 0; border-bottom: 1px solid var(--bs-border-color); }
      .ck-status-list li:last-child { border-bottom: 0; }
      .ck-status-detail { font-size: .8rem; color: var(--bs-secondary-color);
                          word-break: break-all; }
      .ck-path { font-family: var(--bs-font-monospace); font-size: .85rem;
                 word-break: break-all; }
      .ck-hint { font-size: .875rem; color: var(--bs-secondary-color); }
      "
    )
}

#' Small status icon used in the sidebar and in tables
#'
#' @param state One of `"done"`, `"warn"`, `"error"` or `"todo"`.
#' @noRd
status_icon <- function(state) {
  switch(
    state,
    done = shiny::icon("circle-check", class = "text-success"),
    warn = shiny::icon("triangle-exclamation", class = "text-warning"),
    error = shiny::icon("circle-xmark", class = "text-danger"),
    shiny::icon("circle", class = "text-body-tertiary")
  )
}
