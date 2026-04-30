observeEvent(input$reset, {
  defaults <- survey_defaults()
  updateSliderInput(inputId = "eo_pf", value = defaults$eo_pf, min = 0, max = 0.7)
  updateSliderInput(inputId = "mun_eo", value = defaults$mun_eo, min = 0, max = 0.7)
  updateSliderInput(inputId = "mun_pf", value = defaults$mun_pf, min = 0, max = 0.7)
  updateSliderInput(inputId = "sample", value = defaults$sample, min = 50, max = 10000)
  updateSliderInput(inputId = "selection", value = defaults$selection, min = 0, max = 0.5)
})

sim1_df <- reactive({
  simulate_survey_data(
    eo_pf = input$eo_pf,
    mun_eo = input$mun_eo,
    mun_pf = input$mun_pf,
    selection = input$selection,
    sample = input$sample
  )
}) |> bindEvent(input$compute)

survey_fits <- reactive({
  fit_survey_models(sim1_df())
}) |> bindEvent(input$compute)

output$survey_data_panel <- renderUI({
  if (input$compute == 0) {
    return(result_placeholder("Simulated Data", "Run the survey simulation to inspect the generated firm-level data."))
  }

  result_panel(
    "Simulated Data",
    spinner(reactableOutput("data"))
  )
})

output$survey_density_panel <- renderUI({
  if (input$compute == 0) {
    return(result_placeholder("Simulated Data Densities", "Density plots for EO, performance, selection, and munificence appear here after the simulation runs."))
  }

  result_panel(
    "Simulated Data Densities",
    spinner(plotlyOutput("density"))
  )
})

output$survey_summary_panel <- renderUI({
  if (input$compute == 0) {
    return(result_placeholder("Model Results", "Run the simulation to compare each model estimate with the true EO-performance correlation."))
  }

  result_panel(
    "Model Results",
    spinner(reactableOutput("table_fit")),
    tags$div(class = "result-summary", strong("Estimate:"), " Strength of the EO - Performance relationship")
  )
})

output$survey_model_plot_panel <- renderUI({
  if (input$compute == 0) {
    return(result_placeholder("Model Result Plots", "SEM diagrams appear here after the survey simulation runs."))
  }

  result_panel(
    "Model Result Plots",
    div(
      class = "result-panel-toolbar",
      actionButton("expand_survey_model_plot", "Expand", class = "btn-info btn-sm", icon = icon("expand"))
    ),
    tabsetPanel(
      tabPanel("Correct Model", spinner(grVizOutput("correct_plot", width = "100%", height = "390px"))),
      tabPanel("Selection Model", spinner(grVizOutput("selection_plot", width = "100%", height = "390px"))),
      tabPanel("Omitted Model", spinner(grVizOutput("omitted_plot", width = "100%", height = "390px"))),
      tabPanel("Naive Model", spinner(grVizOutput("naive_plot", width = "100%", height = "390px")))
    )
  )
})

observeEvent(input$expand_survey_model_plot, {
  req(input$compute > 0)
  showModal(
    modalDialog(
      title = "Model Result Plots",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tabsetPanel(
        tabPanel("Correct Model", grVizOutput("correct_plot_large", width = "100%", height = "680px")),
        tabPanel("Selection Model", grVizOutput("selection_plot_large", width = "100%", height = "680px")),
        tabPanel("Omitted Model", grVizOutput("omitted_plot_large", width = "100%", height = "680px")),
        tabPanel("Naive Model", grVizOutput("naive_plot_large", width = "100%", height = "680px"))
      )
    )
  )
})

output$data <- renderReactable({
  simulation_table(
    sim1_df(),
    c(
      "avgEO", "Performance", "INN1", "INN2", "INN3", "PRO1", "PRO2",
      "PRO3", "RISK1", "RISK2", "RISK3", "Selection", "Munificence"
    )
  )
}) |> bindEvent(input$compute)

output$density <- renderPlotly({
  density_plot(
    sim1_df(),
    c("avgEO", "Performance", "Selection", "Munificence"),
    "survey_density_plot"
  )
}) |> bindEvent(input$compute)

output$table_fit <- renderReactable({
  model_summary_table(survey_model_summary(survey_fits(), input$eo_pf))
}) |> bindEvent(input$compute)

output$correct_plot <- renderGrViz({
  sem_diagram(survey_fits()$correct)
}) |> bindEvent(input$compute)

output$selection_plot <- renderGrViz({
  sem_diagram(survey_fits()$selection)
}) |> bindEvent(input$compute)

output$omitted_plot <- renderGrViz({
  sem_diagram(survey_fits()$omitted)
}) |> bindEvent(input$compute)

output$naive_plot <- renderGrViz({
  sem_diagram(survey_fits()$naive)
}) |> bindEvent(input$compute)

output$correct_plot_large <- renderGrViz({
  sem_diagram(survey_fits()$correct)
}) |> bindEvent(input$compute)

output$selection_plot_large <- renderGrViz({
  sem_diagram(survey_fits()$selection)
}) |> bindEvent(input$compute)

output$omitted_plot_large <- renderGrViz({
  sem_diagram(survey_fits()$omitted)
}) |> bindEvent(input$compute)

output$naive_plot_large <- renderGrViz({
  sem_diagram(survey_fits()$naive)
}) |> bindEvent(input$compute)
