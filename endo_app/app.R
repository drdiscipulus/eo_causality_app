#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
library(shiny)
library(bslib)
library(tidyverse)
library(lavaan)
library(simstandard)
library(reactable)
library(plotly)
library(broom)
library(glue)
library(lavaanPlot)
library(DiagrammeR)
library(simstudy)
library(plm)

# set seed for random numbers
set.seed(1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # custom theming based on corporate design of the university
  theme = bslib::bs_theme(
    version = 4, fg = "#0C0C0C", primary = "#009260", base_font = "Gill Sans",
    code_font = font_google("Roboto Mono"), bootswatch = "united",
    bg = "#FFFFFF"
  ),

  # load css styling that puts the image to the right side
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "image_style.css")
  ),

  # define top level navigation bar with image
  navbarPage(
    title = div(
      div(
        id = "img-id",
        img(src = "index.png")
      ),
      "The Chicken or the Egg?"
    ),

    # define first panel
    tabPanel(
      # panel title
      "Survey Design",
      # define sidebar
      sidebarLayout(
        sidebarPanel(
          width = 2,
          style = "height: 774px",
          strong(h5("Correlations")),
          sliderInput("eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          hr(),
          strong(h5("Options")),
          sliderInput("sample", "Sample Size", value = 200, min = 50, max = 10000),
          sliderInput("selection", "Selection Effect", value = 0.35, min = 0, max = 0.5)
        ),
        # define main window
        mainPanel(
          width = 10,
          # first row
          fluidRow(
            # shows simulated data in table
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 470px",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("data")
              )
            ),
            # shows density plot of simulated data
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 470px",
                strong("Density Plot of Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                plotlyOutput("density")
              )
            )
          ),
          # second row
          br(),
          fluidRow(
            # shows results as table
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 280px",
                strong("Result Table (Difference to true EO - Performance Correlation)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("table_fit")
              )
            ),
            # shows results as plots
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 280px",
                strong("Plotted Model Results"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                tabsetPanel(
                  tabPanel("Correct Model", grVizOutput("correct_plot", width = "100%", height = "170px")), 
                  tabPanel("Selection Model", grVizOutput("selection_plot", width = "100%", height = "170px")),  
                  tabPanel("Omitted Model", grVizOutput("omitted_plot", width = "100%", height = "170px")), 
                  tabPanel("Naive Model", grVizOutput("naive_plot", width = "100%", height = "170px"))
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      # panel title
      "CATA Design",
      # define sidebar
      sidebarLayout(
        sidebarPanel(
          width = 2,
          strong(h5("Correlations")),
          sliderInput("cata_eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("cata_mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("cata_mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          sliderInput("cata_eo_ui", "EO - Disturbance Term", value = 0.8, min = 0, max = 1),
          hr(),
          strong(h5("Options")),
          sliderInput("cata_num_obs", "Mean Observations per Firm", value = 10, min = 5, max = 20),
          sliderInput("cata_num_firms", "Number of Firms", value = 1000, min = 50, max = 10000),
          sliderInput("cata_m_error", "Measurement Error Variance", value = 1.85, min = 0.5, max = 2.5)
        ),
        # define main window
        mainPanel(
          width = 10,
          # first row
          fluidRow(
            # shows simulated data in table
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 470px",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("cata_data")
              )
              
            ),
            # shows density plot of simulated data
            column(
              6,
             
            )
          ),
          # second row
          br(),
          fluidRow(
            # shows results as table
            column(
              6,

            ),
            # shows results as plots
            column(
              6,

              )
            )
          )
        )
      )
    )
  )





# Define server logic required to draw a histogram
server <- function(input, output) {

  # Define the simulation model
  sim_model <- reactive({
    paste0(
      "# First order reflective measurement model \n",
      "EO =~ .6 * INN1 + .85 * INN2 + .85 * INN3 + .79 * PRO1 + .82 * PRO2 + .73 * PRO3 + .79 * RISK1 + .82 * RISK2 + .82 * RISK3 \n",
      "Performance ~", input$eo_pf, " * EO \n",
      "EO ~ ", input$selection, " * Selection +", input$mun_eo, " * Munificence \n",
      "Performance ~ ", input$selection, " * Selection + ", input$mun_pf, " * Munificence \n"
    )
  })

  # simulate the data set
  sim1_df <- reactive({
    sim1_df <- sim_standardized(sim_model(), n = input$sample, latent = FALSE, errors = FALSE)

    # add an aggregate eo column
    sim1_df <- sim1_df %>%
      mutate(avgEO = (INN1 + INN2 + INN3 +
        PRO1 + PRO2 + PRO3 +
        RISK1 + RISK2 + RISK3) / 9) %>%
      # reorder columns a bit
      relocate(avgEO, Performance)
  })

  # create an reactable table
  output$data <- renderReactable({
    reactable(sim1_df(),
      defaultPageSize = 9,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      defaultColDef = colDef(
        align = "center",
        minWidth = 110,
      ),
      columns = list(
        avgEO = colDef(format = colFormat(digits = 2)),
        Performance = colDef(format = colFormat(digits = 2)),
        INN1 = colDef(format = colFormat(digits = 2)),
        INN2 = colDef(format = colFormat(digits = 2)),
        INN3 = colDef(format = colFormat(digits = 2)),
        PRO1 = colDef(format = colFormat(digits = 2)),
        PRO2 = colDef(format = colFormat(digits = 2)),
        PRO3 = colDef(format = colFormat(digits = 2)),
        RISK1 = colDef(format = colFormat(digits = 2)),
        RISK2 = colDef(format = colFormat(digits = 2)),
        RISK3 = colDef(format = colFormat(digits = 2)),
        Selection = colDef(format = colFormat(digits = 2)),
        Munificence = colDef(format = colFormat(digits = 2))
      )
    )
  })

  # create a density plot with ggplot2 and convert it to plotly
  output$density <- renderPlotly({
    # cast dataframe from wide to long
    sim1_density <- sim1_df() %>%
      select(avgEO, Performance, Selection, Munificence) %>%
      pivot_longer(cols = c(avgEO, Performance, Selection, Munificence), names_to = "variables", values_to = "value")

    # create ggplot2 density plot
    p <- ggplot(sim1_density, aes(value, fill = factor(variables), colour = factor(variables))) +
      geom_density(alpha = 0.3, adjust = 4) +
      scale_x_continuous("Values") +
      scale_y_continuous("Density") +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      scale_colour_viridis_d() +
      scale_fill_viridis_d()

    # convert ggplot2 to plotly
    p <- ggplotly(p, tooltip = c("y", "x"), height = 400) %>%
      layout(paper_bgcolor = "transparent") %>%
      layout(plot_bgcolor = "transparent") %>%
      config(displayModeBar = FALSE)
  })

  # run the correct model (no bias)
  correct_fit <- reactive({

    # speficy model
    model <- "# First order reflective measurement model
                    EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
                    # Structural parameters
                    Performance ~ EO + Selection + Munificence
                    # Covariances between EO and the omitted variables
                    EO ~~ Selection
                    EO ~~ Munificence"

    # fit model
    correct_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  })

  # run the selection effects model (only selection bias)
  selection_fit <- reactive({

    # specify model
    model <- "# First order reflective measurement model
                EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
                # Structural parameters
                Performance ~ EO + Munificence
                # Covariances between EO and the omitted variables
                EO ~~ Munificence"

    # fit model
    selection_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  })

  # run omitted variable model (only omitted variable bias)
  omitted_fit <- reactive({

    # specify model
    model <- "# First order reflective measurement model
              EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
              # Structural parameters
              Performance ~ EO + Selection
              # Covariances between EO and the omitted variables
              EO ~~ Selection"

    # fit model
    omitted_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  })
  
  # run naive model (omitted variable and selection bias)
  naive_fit <- reactive({
    
    # specify model
    model <- "# First order reflective measurement model
              EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
              # Structural parameters
              Performance ~ EO"
    
    # fit model
    naive_fit <- sem(model, data = sim1_df(), std.lv = TRUE)    
  })
  
  # run measurement error model (measurement error only model - done with lm)
  error_fit <- reactive({
    
    # fit model
    error_fit <- lm(scale(Performance) ~ scale(avgEO), data = sim1_df())
  })
  
  # create output table
  output$table_fit <- renderReactable({
    
    # extract required model parameters from all models
    # The correct model
    correct_res <- tidy(correct_fit()) %>% 
      filter(term == "Performance ~ EO") %>% 
      select(estimate, std.error)
    # The selection effect model
    selection_res <- tidy(selection_fit()) %>% 
      filter(term == "Performance ~ EO") %>% 
      select(estimate, std.error)
    # The omitted variable model
    omitted_res <- tidy(omitted_fit()) %>% 
      filter(term == "Performance ~ EO") %>% 
      select(estimate, std.error)
    # The naive model
    naive_res <- tidy(naive_fit()) %>% 
      filter(term == "Performance ~ EO") %>% 
      select(estimate, std.error)
    # The measurement error model
    error_res <- tidy(error_fit()) %>% 
      filter(term == "scale(avgEO)") %>% 
      select(estimate, std.error)
    
    # combine into one table
    res <- bind_rows(correct_res, selection_res, omitted_res, naive_res, error_res) %>% 
      mutate_all(round, 3) %>% 
      mutate(Difference = as.character(round(100 * (abs(input$eo_pf - estimate) / ((input$eo_pf + estimate) / 2))))) %>% 
      mutate(Difference = glue("{Difference}%"))
    
    # create a new dataframe to hold our model names
    model_names <- data_frame(model = c("Correct Model", "Selection Effect Model", "Omitted Variable Model", "Naive Model", "Measurement Error Model"))
    
    # add the model names 
    res <- bind_cols(model_names, res)
    
    # create reactable
    table_fit <- reactable(res,
                           highlight = TRUE,
                           striped = TRUE,
                           bordered = TRUE,
                           compact = TRUE,
                           columns = list(
                             model = colDef(name = "Model", align = "left", width = 200),
                             estimate = colDef(name = "Estimate", align = "center"),
                             std.error = colDef(name = "Std.Error", align = "center"),
                             Difference = colDef(align = "center")
                           ))
  })
  
  # create model plots
  # correct model
  output$correct_plot <- renderGrViz({
    lavaanPlot(model = correct_fit(), node_options = list(shape = "box", fontname = "Helvetica"), 
               edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress")
  })
  
  # selection model
  output$selection_plot <- renderGrViz({
    lavaanPlot(model = selection_fit(), node_options = list(shape = "box", fontname = "Helvetica"), 
               edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress")
  })
  
  # omitted variable model
  output$omitted_plot <- renderGrViz({
    lavaanPlot(model = omitted_fit(), node_options = list(shape = "box", fontname = "Helvetica"), 
               edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress")
  })
  
  # naive model
  output$naive_plot <- renderGrViz({
    lavaanPlot(model = naive_fit(), node_options = list(shape = "box", fontname = "Helvetica"), 
               edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress")
  })
  
  
  
 
  
  
  
  # cata model
  # between firms data (level 2)
  level2_df <- reactive({
    level2_df  <- defData(varname = "u_i", dist = "normal", formula = 0, variance = 1, id = "FirmID")
    level2_df  <- defData(level2_df , varname = "NumObs", dist = "noZeroPoisson", formula = input$cata_num_obs) # fix
    level2_df <- genData(10000, level2_df)
  })
  
  # within firm data (level 1)
  level1_df <- reactive({
    level1_df <- genCluster(level2_df(), cLevelVar = "FirmID", numIndsVar = "NumObs", 
                            level1ID = "ObsID")
    level1_model <- defDataAdd(varname = "e_ij", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "Munificence", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "EO_True", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "EO", dist = "normal", 
                               formula = paste0("EO_True + ", input$cata_eo_ui," * u_i + ", input$cata_mun_eo," * Munificence"), variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "M_Error", dist = "normal", formula = 0, variance = input$cata_m_error)
    level1_df <- addColumns(level1_model, level1_df)
  })
  
  # create data set
  sim2_df <- reactive({
    sim2_df <- level1_df() %>% 
      group_by(FirmID) %>% 
      mutate(ObsID = row_number()) %>% 
      ungroup() %>% 
      mutate(Performance = (input$cata_eo_pf * EO) + (input$cata_mun_pf * Munificence) + u_i + e_ij)
  })
  
  # table for simulated data
  output$cata_data <- renderReactable({
    reactable(sim2_df(),
              defaultPageSize = 9,
              highlight = TRUE,
              striped = TRUE,
              bordered = TRUE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "center",
                minWidth = 110,
              ),
              columns = list(
                u_i = colDef(format = colFormat(digits = 2)),
                e_ij = colDef(format = colFormat(digits = 2)),
                Munificence = colDef(format = colFormat(digits = 2)),
                EO_True = colDef(format = colFormat(digits = 2)),
                EO = colDef(format = colFormat(digits = 2)),
                M_Error = colDef(format = colFormat(digits = 2)),
                Performance = colDef(format = colFormat(digits = 2))
              )
              
              )
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
