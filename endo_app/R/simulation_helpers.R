survey_defaults <- function() {
  list(
    eo_pf = 0.25,
    mun_eo = 0.43,
    mun_pf = 0.16,
    sample = 200,
    selection = 0.35
  )
}

cata_defaults <- function() {
  list(
    cata_eo_pf = 0.25,
    cata_mun_eo = 0.43,
    cata_mun_pf = 0.16,
    cata_eo_ui = 0.8,
    cata_num_obs = 10,
    cata_num_firms = 1000,
    cata_m_error = 1.85
  )
}

survey_model_spec <- function(eo_pf, mun_eo, mun_pf, selection) {
  paste0(
    "# First order reflective measurement model \n",
    "EO =~ .6 * INN1 + .85 * INN2 + .85 * INN3 + .79 * PRO1 + .82 * PRO2 + .73 * PRO3 + .79 * RISK1 + .82 * RISK2 + .82 * RISK3 \n",
    "Performance ~", eo_pf, " * EO \n",
    "EO ~ ", selection, " * Selection +", mun_eo, " * Munificence \n",
    "Performance ~ ", selection, " * Selection + ", mun_pf, " * Munificence \n"
  )
}

simulate_survey_data <- function(eo_pf, mun_eo, mun_pf, selection, sample) {
  loadings <- c(
    INN1 = 0.60, INN2 = 0.85, INN3 = 0.85,
    PRO1 = 0.79, PRO2 = 0.82, PRO3 = 0.73,
    RISK1 = 0.79, RISK2 = 0.82, RISK3 = 0.82
  )

  Selection <- stats::rnorm(sample)
  Munificence <- stats::rnorm(sample)
  eo_error_sd <- sqrt(max(1 - selection^2 - mun_eo^2, 0.001))
  EO <- selection * Selection + mun_eo * Munificence + stats::rnorm(sample, sd = eo_error_sd)
  EO <- as.numeric(scale(EO))

  performance_signal_variance <- eo_pf^2 + selection^2 + mun_pf^2 +
    (2 * eo_pf * selection * stats::cor(EO, Selection)) +
    (2 * eo_pf * mun_pf * stats::cor(EO, Munificence)) +
    (2 * selection * mun_pf * stats::cor(Selection, Munificence))
  performance_error_sd <- sqrt(max(1 - performance_signal_variance, 0.001))
  Performance <- eo_pf * EO + selection * Selection + mun_pf * Munificence +
    stats::rnorm(sample, sd = performance_error_sd)
  Performance <- as.numeric(scale(Performance))

  indicators <- purrr::imap_dfc(loadings, function(loading, name) {
    value <- loading * EO + sqrt(1 - loading^2) * stats::rnorm(sample)
    tibble::tibble(!!name := as.numeric(scale(value)))
  })

  dplyr::bind_cols(
    tibble::tibble(
      avgEO = rowMeans(indicators),
      Performance = Performance
    ),
    indicators,
    tibble::tibble(
      Selection = as.numeric(scale(Selection)),
      Munificence = as.numeric(scale(Munificence))
    )
  )
}

survey_fit_specs <- function() {
  list(
    correct = "# First order reflective measurement model
      EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
      # Structural parameters
      Performance ~ EO + Selection + Munificence
      # Covariances between EO and the omitted variables
      EO ~~ Selection
      EO ~~ Munificence",
    selection = "# First order reflective measurement model
      EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
      # Structural parameters
      Performance ~ EO + Munificence
      # Covariances between EO and the omitted variables
      EO ~~ Munificence",
    omitted = "# First order reflective measurement model
      EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
      # Structural parameters
      Performance ~ EO + Selection
      # Covariances between EO and the omitted variables
      EO ~~ Selection",
    naive = "# First order reflective measurement model
      EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
      # Structural parameters
      Performance ~ EO"
  )
}

fit_survey_models <- function(dat) {
  specs <- survey_fit_specs()
  list(
    correct = lavaan::sem(specs$correct, data = dat, std.lv = TRUE),
    selection = lavaan::sem(specs$selection, data = dat, std.lv = TRUE),
    omitted = lavaan::sem(specs$omitted, data = dat, std.lv = TRUE),
    naive = lavaan::sem(specs$naive, data = dat, std.lv = TRUE),
    error = stats::lm(scale(Performance) ~ scale(avgEO), data = dat)
  )
}

survey_model_summary <- function(fits, true_effect) {
  survey_extract <- function(fit, term) {
    tidied <- broom::tidy(fit)
    if (all(c("lhs", "op", "rhs") %in% names(tidied))) {
      return(
        tidied |>
          dplyr::filter(.data$lhs == "Performance", .data$op == "~", .data$rhs == "EO") |>
          dplyr::select(estimate, std.error)
      )
    }

    tidied |>
      dplyr::filter(.data$term == .env$term) |>
      dplyr::select(estimate, std.error)
  }

  dplyr::bind_rows(
    survey_extract(fits$correct, "Performance ~ EO"),
    survey_extract(fits$selection, "Performance ~ EO"),
    survey_extract(fits$omitted, "Performance ~ EO"),
    survey_extract(fits$naive, "Performance ~ EO"),
    survey_extract(fits$error, "scale(avgEO)")
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(.x, 3))) |>
    dplyr::mutate(
      model = c(
        "Correct Model",
        "Selection Effect Model",
        "Omitted Variable Model",
        "Naive Model",
        "Measurement Error Model"
      ),
      Difference = as.character(round(100 * (abs(true_effect - estimate) / ((true_effect + estimate) / 2)))),
      Difference = glue::glue("{Difference}%"),
      .before = estimate
    )
}

simulate_cata_data <- function(cata_eo_pf, cata_mun_eo, cata_mun_pf, cata_eo_ui,
                               cata_num_obs, cata_num_firms, cata_m_error) {
  level2_df <- tibble::tibble(
    FirmID = seq_len(cata_num_firms),
    u_i = stats::rnorm(cata_num_firms),
    NumObs = pmax(1, stats::rpois(cata_num_firms, lambda = cata_num_obs))
  )

  tidyr::uncount(level2_df, weights = NumObs, .remove = FALSE) |>
    dplyr::group_by(FirmID) |>
    dplyr::mutate(ObsID = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      e_ij = stats::rnorm(dplyr::n()),
      Munificence = stats::rnorm(dplyr::n()),
      EO_True = stats::rnorm(dplyr::n()),
      EO = EO_True + (cata_eo_ui * u_i) + (cata_mun_eo * Munificence) + stats::rnorm(dplyr::n()),
      M_Error = stats::rnorm(dplyr::n(), sd = sqrt(cata_m_error))
    ) |>
    dplyr::mutate(Performance = (cata_eo_pf * EO) + (cata_mun_pf * Munificence) + u_i + e_ij)
}

fit_cata_models <- function(dat) {
  if (!requireNamespace("plm", quietly = TRUE)) {
    dat_with_error <- dplyr::mutate(dat, EO_Error = EO + M_Error)
    return(list(
      correct = stats::lm(Performance ~ EO + Munificence + factor(FirmID), data = dat),
      omitted = stats::lm(Performance ~ EO + factor(FirmID), data = dat),
      random = stats::lm(Performance ~ EO + Munificence, data = dat),
      naive = stats::lm(Performance ~ EO, data = dat),
      measurement_error = stats::lm(Performance ~ EO_Error, data = dat_with_error)
    ))
  }

  list(
    correct = plm::plm(Performance ~ EO + Munificence, data = dat, index = c("FirmID", "ObsID"), model = "within"),
    omitted = plm::plm(Performance ~ EO, data = dat, index = c("FirmID", "ObsID"), model = "within"),
    random = plm::plm(Performance ~ EO + Munificence, data = dat, index = c("FirmID", "ObsID"), model = "random"),
    naive = plm::plm(Performance ~ EO, data = dat, index = c("FirmID", "ObsID"), model = "random"),
    measurement_error = plm::plm(
      Performance ~ EO_Error,
      data = dplyr::mutate(dat, EO_Error = EO + M_Error),
      index = c("FirmID", "ObsID"),
      model = "random"
    )
  )
}

cata_model_summary <- function(fits, true_effect) {
  cata_extract <- function(fit, term) {
    broom::tidy(fit) |>
      dplyr::filter(.data$term == .env$term) |>
      dplyr::select(estimate, std.error)
  }

  dplyr::bind_rows(
    cata_extract(fits$correct, "EO"),
    cata_extract(fits$omitted, "EO"),
    cata_extract(fits$random, "EO"),
    cata_extract(fits$naive, "EO"),
    cata_extract(fits$measurement_error, "EO_Error")
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(.x, 3))) |>
    dplyr::mutate(
      model = c(
        "Correct Model",
        "Omitted Variable Model",
        "Random Effects Model",
        "Naive Model",
        "Measurement Error Model"
      ),
      Difference = as.character(round(100 * (abs(true_effect - estimate) / ((true_effect + estimate) / 2)))),
      Difference = glue::glue("{Difference}%"),
      .before = estimate
    )
}

density_plot <- function(dat, variables, filename) {
  density_data <- dat |>
    dplyr::select(dplyr::all_of(variables)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variables",
      values_to = "value"
    )

  plot <- ggplot2::ggplot(
    density_data,
    ggplot2::aes(value, fill = variables, colour = variables)
  ) +
    ggplot2::geom_density(alpha = 0.4, adjust = 4) +
    ggplot2::scale_x_continuous("Values") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::labs(fill = "Variables", colour = "Variables") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      legend.title = ggplot2::element_text(family = "Arial", face = "bold"),
      legend.text = ggplot2::element_text(family = "Arial"),
      panel.grid.major = ggplot2::element_line(colour = "white", linewidth = 0.5)
    ) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::scale_fill_viridis_d()

  plot <- plotly::ggplotly(plot, tooltip = c("y", "x"), height = 400) |>
    plotly::layout(
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      font = list(family = "Arial"),
      legend = list(title = list(text = "Variables", font = list(family = "Arial")))
    ) |>
    app_plotly_config(filename)

  for (i in seq_along(plot$x$data)) {
    x <- plot$x$data[[i]]$x
    y <- plot$x$data[[i]]$y
    plot$x$data[[i]]$text <- purrr::map2(x, y, ~ paste0("Density: ", round(.y, digits = 2), "<br />Value: ", round(.x, digits = 2)))
  }

  plot
}

simulation_table <- function(dat, columns) {
  reactable::reactable(
    dat,
    defaultPageSize = 9,
    highlight = TRUE,
    striped = TRUE,
    bordered = TRUE,
    compact = TRUE,
    defaultColDef = reactable::colDef(align = "center", minWidth = 110),
    columns = numeric_columns(columns),
    theme = app_reactable_theme()
  )
}

model_summary_table <- function(dat) {
  reactable::reactable(
    dat,
    highlight = TRUE,
    striped = TRUE,
    bordered = TRUE,
    compact = TRUE,
    columns = model_result_columns(),
    theme = app_reactable_theme()
  )
}

sem_diagram <- function(model) {
  parameters <- lavaan::parameterEstimates(model, standardized = TRUE) |>
    dplyr::filter(op %in% c("=~", "~", "~~")) |>
    dplyr::filter(op != "~~" | lhs != rhs)

  node_labels <- unique(c(parameters$lhs, parameters$rhs))
  node_ids <- paste0("n", seq_along(node_labels))
  names(node_ids) <- node_labels

  node_lines <- purrr::map2_chr(node_ids, node_labels, function(id, label) {
    paste0(
      "  ", id,
      " [label = \"", label, "\"];"
    )
  })

  edge_lines <- purrr::pmap_chr(parameters, function(lhs, op, rhs, std.all, ...) {
    label <- ifelse(is.na(std.all), "", round(std.all, 2))
    if (op == "~~") {
      return(paste0(
        "  ", node_ids[[lhs]], " -> ", node_ids[[rhs]],
        " [label = \"", label, "\", dir = both, style = dashed];"
      ))
    }

    source <- if (op == "=~") lhs else rhs
    target <- if (op == "=~") rhs else lhs
    paste0(
      "  ", node_ids[[source]], " -> ", node_ids[[target]],
      " [label = \"", label, "\"];"
    )
  })

  dot <- paste(
    "digraph sem {",
    "  graph [rankdir = LR, bgcolor = \"transparent\", margin = 0.05];",
    "  node [shape = box, style = \"rounded,filled\", fillcolor = \"#ffffff\", color = \"#d8dfdc\", fontname = \"Arial\", fontsize = 12];",
    "  edge [color = \"#5f6b66\", fontname = \"Arial\", fontsize = 10, arrowsize = 0.7];",
    paste(node_lines, collapse = "\n"),
    paste(edge_lines, collapse = "\n"),
    "}",
    sep = "\n"
  )

  DiagrammeR::grViz(dot)
}
