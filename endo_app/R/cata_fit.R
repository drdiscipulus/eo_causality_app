format_p_value <- function(p.value) {
  p.value <- round(p.value, 3)
  dplyr::if_else(p.value < 0.001, "<0.001", as.character(p.value))
}

cata_parameter_data <- function(model) {
  broom::tidy(model) |>
    dplyr::mutate(
      estimate = round(estimate, 2),
      std.error = round(std.error, 3),
      statistic = round(statistic, 2),
      p.value = format_p_value(p.value)
    )
}

cata_fit_data <- function(model) {
  broom::glance(model) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    dplyr::mutate(p.value = dplyr::if_else(as.numeric(p.value) < 0.001, "<0.001", as.character(p.value)))
}

get_cata_parameters <- function(model) {
  reactable::reactable(
    cata_parameter_data(model),
    highlight = TRUE,
    bordered = TRUE,
    compact = TRUE,
    columns = list(
      term = reactable::colDef(name = "Variable", align = "left"),
      estimate = reactable::colDef(name = "Estimate", align = "center"),
      std.error = reactable::colDef(name = "Std.Error", align = "center"),
      statistic = reactable::colDef(name = "Test Statistic", align = "center"),
      p.value = reactable::colDef(name = "p-Value", align = "center")
    ),
    theme = app_reactable_theme()
  )
}

get_cata_fit <- function(model) {
  reactable::reactable(
    cata_fit_data(model),
    highlight = TRUE,
    bordered = TRUE,
    compact = TRUE,
    columns = list(
      r.squared = reactable::colDef(name = "R2", align = "center"),
      adj.r.squared = reactable::colDef(name = "R2 (adj.)", align = "center"),
      statistic = reactable::colDef(name = "Test Statistic", align = "center"),
      p.value = reactable::colDef(name = "p-Value", align = "center"),
      deviance = reactable::colDef(name = "Deviance", align = "center"),
      df.residual = reactable::colDef(name = "Residual DF", align = "center"),
      nobs = reactable::colDef(name = "N", align = "center")
    ),
    theme = app_reactable_theme()
  )
}
