tabPanel(
  "Longitudinal Design (CATA)",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      class = "workflow-sidebar",
      h4("Parameters", help_icon("Set the population relationships used to simulate longitudinal CATA data.")),
      sliderInput("cata_eo_pf", tagList("EO - Performance", help_icon("True relationship between entrepreneurial orientation and performance.")), value = 0.25, min = 0, max = 0.7),
      sliderInput("cata_mun_eo", tagList("Munificence - EO", help_icon("Relationship between environmental munificence and entrepreneurial orientation.")), value = 0.43, min = 0, max = 0.7),
      sliderInput("cata_mun_pf", tagList("Munificence - Performance", help_icon("Relationship between environmental munificence and performance.")), value = 0.16, min = 0, max = 0.7),
      sliderInput("cata_eo_ui", tagList("EO - Disturbance Term", help_icon("Correlation between EO and the level-2 disturbance term.")), value = 0.8, min = 0, max = 1),
      sliderInput("cata_num_obs", tagList("Mean Observations per Firm", help_icon("Expected number of observations generated per firm.")), value = 10, min = 5, max = 15),
      sliderInput("cata_num_firms", tagList("Number of Firms", help_icon("Number of simulated firms in the panel.")), value = 1000, min = 50, max = 3000),
      sliderInput("cata_m_error", tagList("Measurement Error Variance", help_icon("Variance of the measurement error added to EO in the measurement-error model.")), value = 1.85, min = 0.5, max = 2.5),
      hr(),
      actionButton("cata_compute", "Run", class = "btn-primary", width = "100%", icon = icon("cog")),
      actionButton("cata_reset", "Reset", class = "btn-secondary", width = "100%", icon = icon("undo"))
    ),
    mainPanel(
      width = 10,
      class = "workflow-main-panel",
      fluidRow(
        class = "app-output-row app-output-top-row",
        column(width = 6, class = "app-output-col app-output-top-col", uiOutput("cata_data_panel")),
        column(width = 6, class = "app-output-col app-output-top-col", uiOutput("cata_density_panel"))
      ),
      fluidRow(
        class = "app-output-row app-output-bottom-row",
        column(width = 6, class = "app-output-col", uiOutput("cata_summary_panel")),
        column(width = 6, class = "app-output-col", uiOutput("cata_fit_panel"))
      )
    )
  )
)
