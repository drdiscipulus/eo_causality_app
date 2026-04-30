observeEvent(input$cata_reset, {
  defaults <- cata_defaults()
  updateSliderInput(inputId = "cata_eo_pf", value = defaults$cata_eo_pf, min = 0, max = 0.7)
  updateSliderInput(inputId = "cata_mun_eo", value = defaults$cata_mun_eo, min = 0, max = 0.7)
  updateSliderInput(inputId = "cata_mun_pf", value = defaults$cata_mun_pf, min = 0, max = 0.7)
  updateSliderInput(inputId = "cata_eo_ui", value = defaults$cata_eo_ui, min = 0, max = 1)
  updateSliderInput(inputId = "cata_num_obs", value = defaults$cata_num_obs, min = 5, max = 15)
  updateSliderInput(inputId = "cata_num_firms", value = defaults$cata_num_firms, min = 50, max = 3000)
  updateSliderInput(inputId = "cata_m_error", value = defaults$cata_m_error, min = 0.5, max = 2.5)
})

sim2_df <- reactive({
  simulate_cata_data(
    cata_eo_pf = input$cata_eo_pf,
    cata_mun_eo = input$cata_mun_eo,
    cata_mun_pf = input$cata_mun_pf,
    cata_eo_ui = input$cata_eo_ui,
    cata_num_obs = input$cata_num_obs,
    cata_num_firms = input$cata_num_firms,
    cata_m_error = input$cata_m_error
  )
}) |> bindEvent(input$cata_compute)

cata_fits <- reactive({
  fit_cata_models(sim2_df())
}) |> bindEvent(input$cata_compute)

output$cata_data_panel <- renderUI({
  if (input$cata_compute == 0) {
    return(result_placeholder("Simulated Data", "Run the longitudinal simulation to inspect the generated panel data."))
  }

  result_panel(
    "Simulated Data",
    spinner(reactableOutput("cata_data"))
  )
})

output$cata_density_panel <- renderUI({
  if (input$cata_compute == 0) {
    return(result_placeholder("Simulated Data Densities", "Density plots for EO, performance, munificence, measurement error, and disturbances appear here after the simulation runs."))
  }

  result_panel(
    "Simulated Data Densities (Across Firms)",
    spinner(plotlyOutput("cata_density"))
  )
})

output$cata_summary_panel <- renderUI({
  if (input$cata_compute == 0) {
    return(result_placeholder("Model Results", "Run the simulation to compare each model estimate with the true EO-performance correlation."))
  }

  result_panel(
    "Model Results",
    spinner(reactableOutput("cata_table_fit")),
    tags$div(class = "result-summary", strong("Estimate:"), " Strength of the EO - Performance relationship")
  )
})

output$cata_fit_panel <- renderUI({
  if (input$cata_compute == 0) {
    return(result_placeholder("Model Fit", "Detailed model parameters and fit statistics appear here after the longitudinal simulation runs."))
  }

  result_panel(
    "Model Fit",
    tabsetPanel(
      tabPanel("Correct Model", spinner(reactableOutput("cata_correct_tidy")), br(), spinner(reactableOutput("cata_correct_glance"))),
      tabPanel("Omitted Model", spinner(reactableOutput("cata_omitted_tidy")), br(), spinner(reactableOutput("cata_omitted_glance"))),
      tabPanel("Random Model", spinner(reactableOutput("cata_random_tidy")), br(), spinner(reactableOutput("cata_random_glance"))),
      tabPanel("Naive Model", spinner(reactableOutput("cata_naive_tidy")), br(), spinner(reactableOutput("cata_naive_glance"))),
      tabPanel("ME Model", spinner(reactableOutput("cata_me_tidy")), br(), spinner(reactableOutput("cata_me_glance")))
    )
  )
})

output$cata_data <- renderReactable({
  simulation_table(
    sim2_df(),
    c("u_i", "e_ij", "Munificence", "EO_True", "EO", "M_Error", "Performance")
  )
}) |> bindEvent(input$cata_compute)

output$cata_density <- renderPlotly({
  density_plot(
    sim2_df(),
    c("EO", "EO_True", "Performance", "Munificence", "M_Error", "u_i", "e_ij"),
    "cata_density_plot"
  )
}) |> bindEvent(input$cata_compute)

output$cata_table_fit <- renderReactable({
  model_summary_table(cata_model_summary(cata_fits(), input$cata_eo_pf))
}) |> bindEvent(input$cata_compute)

output$cata_correct_tidy <- renderReactable({
  get_cata_parameters(cata_fits()$correct)
}) |> bindEvent(input$cata_compute)

output$cata_correct_glance <- renderReactable({
  get_cata_fit(cata_fits()$correct)
}) |> bindEvent(input$cata_compute)

output$cata_omitted_tidy <- renderReactable({
  get_cata_parameters(cata_fits()$omitted)
}) |> bindEvent(input$cata_compute)

output$cata_omitted_glance <- renderReactable({
  get_cata_fit(cata_fits()$omitted)
}) |> bindEvent(input$cata_compute)

output$cata_random_tidy <- renderReactable({
  get_cata_parameters(cata_fits()$random)
}) |> bindEvent(input$cata_compute)

output$cata_random_glance <- renderReactable({
  get_cata_fit(cata_fits()$random)
}) |> bindEvent(input$cata_compute)

output$cata_naive_tidy <- renderReactable({
  get_cata_parameters(cata_fits()$naive)
}) |> bindEvent(input$cata_compute)

output$cata_naive_glance <- renderReactable({
  get_cata_fit(cata_fits()$naive)
}) |> bindEvent(input$cata_compute)

output$cata_me_tidy <- renderReactable({
  get_cata_parameters(cata_fits()$measurement_error)
}) |> bindEvent(input$cata_compute)

output$cata_me_glance <- renderReactable({
  get_cata_fit(cata_fits()$measurement_error)
}) |> bindEvent(input$cata_compute)
