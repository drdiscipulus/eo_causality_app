get_cata_parameters <- function(table) {

  # extract parameters
  table <- tidy(table)
  table <- table %>%
    mutate(estimate = round(estimate, 2)) %>%
    mutate(std.error = round(std.error, 3)) %>%
    mutate(statistic = round(statistic, 2)) %>%
    mutate(p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value)))

  # create reactable
  cata_correct_tidy <- reactable(table,
    highlight = TRUE,
    bordered = TRUE,
    compact = TRUE,
    # column formatting
    columns = list(
      term = colDef(name = "Variable", align = "left"),
      estimate = colDef(name = "Estimate", align = "center"),
      std.error = colDef(name = "Std.Error", align = "center"),
      statistic = colDef(name = "Test Statistic", align = "center"),
      p.value = colDef(name = "p-Value", align = "center")
    ),
    # table theming
    theme = reactableTheme(
      highlightColor = "#bdbdbd",
      stripedColor = "#E0E0E0",
      backgroundColor = "#F0F0F0"
    )
  )
}

get_cata_fit <- function(table) {

  # get model fit info
  table <- glance(table)
  table <- table %>%
    mutate_all(round, 3) %>%
    mutate(p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value)))

  # create reactable
  cata_correct_glance <- reactable(table,
    highlight = TRUE,
    bordered = TRUE,
    compact = TRUE,
    # column formatting
    columns = list(
      r.squared = colDef(name = "R2", align = "center"),
      adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
      statistic = colDef(name = "Test Statistic", align = "center"),
      p.value = colDef(name = "p-Value", align = "center"),
      deviance = colDef(name = "Deviance", align = "center"),
      df.residual = colDef(name = "Residual DF", align = "center"),
      nobs = colDef(name = "N", align = "center")
    ),
    # table theming
    theme = reactableTheme(
      highlightColor = "#bdbdbd",
      stripedColor = "#E0E0E0",
      backgroundColor = "#F0F0F0"
    )
  )
}
