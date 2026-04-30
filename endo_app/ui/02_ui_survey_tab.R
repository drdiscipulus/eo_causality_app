tabPanel(
  "Survey Design",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      class = "workflow-sidebar",
      h4("Parameters", help_icon("Set the population relationships used to simulate survey data.")),
      sliderInput("eo_pf", tagList("EO - Performance", help_icon("True relationship between entrepreneurial orientation and performance.")), value = 0.25, min = 0, max = 0.7),
      sliderInput("mun_eo", tagList("Munificence - EO", help_icon("Relationship between environmental munificence and entrepreneurial orientation.")), value = 0.43, min = 0, max = 0.7),
      sliderInput("mun_pf", tagList("Munificence - Performance", help_icon("Relationship between environmental munificence and performance.")), value = 0.16, min = 0, max = 0.7),
      sliderInput("sample", tagList("Sample Size (Firms)", help_icon("Number of simulated firms in the survey design.")), value = 200, min = 50, max = 10000),
      sliderInput("selection", tagList("Selection Effect", help_icon("Unobserved selection into the survey condition.")), value = 0.35, min = 0, max = 0.5),
      hr(),
      actionButton("compute", "Run", class = "btn-primary", width = "100%", icon = icon("cog")),
      actionButton("reset", "Reset", class = "btn-secondary", width = "100%", icon = icon("undo"))
    ),
    mainPanel(
      width = 10,
      class = "workflow-main-panel",
      fluidRow(
        class = "app-output-row app-output-top-row",
        column(width = 6, class = "app-output-col app-output-top-col", uiOutput("survey_data_panel")),
        column(width = 6, class = "app-output-col app-output-top-col", uiOutput("survey_density_panel"))
      ),
      fluidRow(
        class = "app-output-row app-output-bottom-row",
        column(width = 6, class = "app-output-col", uiOutput("survey_summary_panel")),
        column(width = 6, class = "app-output-col", uiOutput("survey_model_plot_panel"))
      )
    )
  )
)
