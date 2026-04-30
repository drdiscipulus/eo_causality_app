help_icon <- function(text) {
  tags$span(class = "help-dot", tabindex = "0", `data-tooltip` = text, "?")
}

app_reactable_theme <- function() {
  reactable::reactableTheme(
    highlightColor = "#d8dfdc",
    stripedColor = "#f6faf8",
    backgroundColor = "#ffffff",
    borderColor = "#d8dfdc"
  )
}

plotly_export_options <- function(filename, width = 1200, height = 720, scale = 2) {
  list(
    format = "png",
    filename = filename,
    width = width,
    height = height,
    scale = scale
  )
}

app_plotly_config <- function(plot, filename, width = 1200, height = 720, scale = 2) {
  plot |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      toImageButtonOptions = plotly_export_options(
        filename = filename,
        width = width,
        height = height,
        scale = scale
      )
    )
}

result_panel <- function(title, ..., class = NULL) {
  div(
    class = paste(c("result-panel", class), collapse = " "),
    div(class = "result-panel-heading", h3(title)),
    ...
  )
}

result_placeholder <- function(title, detail, class = NULL) {
  div(
    class = paste(c("result-placeholder", class), collapse = " "),
    h3(title),
    p(detail)
  )
}

table_note_panel <- function(...) {
  tags$details(
    class = "table-notes",
    tags$summary("Table notes"),
    div(...)
  )
}

spinner <- function(output) {
  shinycssloaders::withSpinner(output, type = 6, color = "#009260")
}

numeric_columns <- function(names) {
  stats::setNames(
    lapply(names, function(x) reactable::colDef(format = reactable::colFormat(digits = 2))),
    names
  )
}

model_result_columns <- function() {
  list(
    model = reactable::colDef(name = "Model", align = "left", minWidth = 210),
    estimate = reactable::colDef(name = "Estimate", align = "center"),
    std.error = reactable::colDef(name = "Std.Error", align = "center"),
    Difference = reactable::colDef(align = "center")
  )
}

about_copy <- function(...) {
  tags$div(class = "about-copy", ...)
}
