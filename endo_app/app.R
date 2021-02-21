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
  # tabpanel themeing
  tags$style(HTML(".nav-tabs > li.active > a {background-color: #009260; color: #fff; border-color: #B6B6B6}")),
  # define top level navigation bar with image
  navbarPage(
    title = div("The Chicken or the Egg?", img(src = "index.png", height = "50px", style = "position: fixed; right: 20px; top:5px;")),
    windowTitle = "The Chicken or the Egg?",
    navbarMenu(
      "Info",
      tabPanel(
        "About",
        fluidRow(
          tags$div(
            tags$h4("Purpose"),
            "This web application is a companion app to the following publication in Entrepreneurship Theory and Practice:",
            tags$br(), tags$br(), tags$a(href = "https://doi.org/10.1177/1042258720976368", "Anderson, B. S., Schüler, J., Baum, M., Wales, W. J., & Gupta, V. K. (2020). The Chicken or the Egg? Causal Inference in Entrepreneurial Orientation–Performance Research. Entrepreneurship Theory and Practice. Online First.", target = "_blank"),
            tags$br(), tags$br(), "The simulations focus on two research designs that are commonly applied in Entrepreneurial Orientation (EO) resarch:",
            tags$ul(
              tags$li("Survey design with psychometric indicators"),
              tags$li("A longitudinal design drawing from secondary data (e.g. content analysis of annual reports)")
            ),
            tags$h4("Abstract"),
            "While entrepreneurial orientation (EO) correlates with many organizational phenomena, we lack convincing evidence of causal relationships within EO’s nomological network. 
              We explore the challenges to establishing causal relationships with a systematic review of EO–performance research. 
              We then use a simulation to illustrate how popular research designs in EO research limit our ability to make causal claims. 
              We conclude by outlining the research design considerations to move from associational to causal EO–performance research. 
              Our message is that while experiments may not be practical or feasible in many areas of organizational research, 
          including EO, scholars can nevertheless move towards causal understanding.",
            tags$br(), tags$br(), "All code and data used in this publication is available on the Open Science Framework:",
            tags$a(href = "https://osf.io/gxhmj/?view_only=5acc084c3dd240d38c72562fb44f2806", "Link to Repository", target = "_blank"),
            tags$br(), tags$h4("Profile Pages"),
            tags$ul(
              tags$li(tags$a(href = "https://bloch.umkc.edu/faculty-directory-anderson-brian/", "Brian S. Anderson", target = "_blank")),
              tags$li(tags$a(href = "https://www.eship.uni-bayreuth.de/de/team/schueler_jens/index.php", "Jens Schüler", target = "_blank")),
              tags$li(tags$a(href = "https://www.eship.uni-bayreuth.de/de/team/baum_matthias/index.php", "Matthias Baum", target = "_blank")),
              tags$li(tags$a(href = "https://www.albany.edu/business/faculty/william-wales", "William J. Wales", target = "_blank")),
              tags$li(tags$a(href = "https://culverhouse.ua.edu/news/directory/vishal-gupta/", "Vishal K. Gupta", target = "_blank"))
            ),
            tags$h6("R Shiny App written by: Jens Schüler")
          )
        )
      ),
      tabPanel(
        "Survey Design",
        tags$h4("Survey Design"),
        "Mimics a survey-based design, where the researcher collects psychometric EO measures, often the nine-item Covin and Slevin (1989) scale scale, from the senior-most executive of a business unit.",
        tags$h5("Parameters"),
        tags$ul(
          tags$li("Performance Outcome: Continuous, normally distributed variable (Y ~ N(0,1))"),
          tags$li("EO Predictor: For the nine EO indicators, we use the same factor loadings as George (2011)"),
          tags$li("EO - Performance Correlation: Following Rauch et al. (2009), we assume a true effect of 0.25"),
          tags$li("Omitted Variable: Environmental Munificence"),
          tags$li("Munificence - EO Correlation: Following Rosenbusch et al. (2013), we assume a true effect of 0.46"),
          tags$li("Munificence - Performance Correlation: Following Rosenbusch et al. (2013), we assume a true effect of 0.16"),
          tags$li("Unobserved Selection Effect: 0.35 assuming that not all firms can be observed and respondents self-select into the survey condition"),
          tags$li("Sample Size: To demonstrate that sample size does not impact endogeneity, we set our sample size to 100,000 observations (here limited to 10,000)")
        ),
        tags$h5("Models"),
        tags$ul(
          tags$li("Correct Model: Includes the selection term, the environmental munificence term and accounts for measurement error (SEM)"),
          tags$li("Selection-Effect Model: Excludes the selection term from the correct model"),
          tags$li("Omitted Variable Model: Excludes the munificence term from the correct model"),
          tags$li("Naive Model: Excludes both selection and munificence term from the correct model"),
          tags$li("Measurement Error Model: Run the naive model as an OLS Regression instead of SEM")
        ),
      ),
      tabPanel(
        "Longitudinal Design",
        tags$h4("Longitudinal Design (CATA)"),
        "We chose a design that is using secondary indicators for EO with panel, or longitudinal data, most often from content analysis of publicly traded firm’s letters to shareholders or annual reports (McKenny et al., 2018).",
        tags$br(), tags$h5("Parameters"),
        tags$ul(
          tags$li("Performance Outcome: Continuous, normally distributed variable (Y ~ N(0,1))"),
          tags$li("EO Predictor: Unidimensional composite variable (innovativeness, proactiveness, risk-taking)"),
          tags$li("EO - Performance Correlation: Following Rauch et al. (2009), we assume a true effect of 0.25"),
          tags$li("Omitted Variable: Environmental Munificence"),
          tags$li("Munificence - EO Correlation: Following Rosenbusch et al. (2013), we assume a true effect of 0.46"),
          tags$li("Munificence - Performance Correlation: Following Rosenbusch et al. (2013), we assume a true effect of 0.16"),
          tags$li("Sample Size: We set the number of firms to be 1,000 and we set the mean number of observations per firm to 10"),
          tags$li("Level 2 Disturbance Term - EO Correlation: We set the correlation between EOij and ui at 0.8, reflecting the self-selection into the firm’s level of EO, and that this relationship is likely to be very stable over time"),
          tags$li("Measurement Error: We drew from McKenny et al. (2018) to explore the impact of measurement error.")
        ),
        tags$h5("Models"),
        tags$ul(
          tags$li("Correct Model: Includes the selection term, the environmental munificence term, accounts for measurement error (SEM), the level 2 disturbance term and runs a fixed effect model."),
          tags$li("Omitted Variable Model: Excludes the munificence term from the correct model"),
          tags$li("Random Effects Model: Applies random effects to the correct model"),
          tags$li("Naive Model: Excludes the munificence term from the correct model and draws on random effects"),
          tags$li("Measurement Error Model: Introduce measurement error to the naive model")
        ),
      )
    ),
    # define second panel
    tabPanel(
      # panel title
      "Survey Design",
      # define sidebar
      sidebarLayout(
        sidebarPanel(
          width = 2,
          style = "height: 814px; background: #EBEBE4",
          h4("Parameters", style = "margin-top: 0rem"),
          hr(),
          strong(h5("Correlations")),
          sliderInput("eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          hr(),
          strong(h5("Options")),
          sliderInput("sample", "Sample Size (Firms)", value = 200, min = 50, max = 10000),
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
                style = "padding: 0.7rem; height: 470px; background: #EBEBE4",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("data")
              )
            ),
            # shows density plot of simulated data
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 470px; background: #EBEBE4",
                strong("Simulated Data Densities"),
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
                style = "padding: 0.7rem; height: 320px; background: #EBEBE4",
                strong("Model Results (% difference to true EO - Performance Correlation of 0.25)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("table_fit"),
                hr(),
                strong("Estimate:"), "Strength of the EO - Performance relationship"
              )
            ),
            # shows results as plots
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 320px; background: #EBEBE4",
                strong("Model Results Plots"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                tabsetPanel(
                  tabPanel("Correct Model", 
                           br(),
                           grVizOutput("correct_plot", width = "100%", height = "180px")),
                  tabPanel("Selection Model", 
                           br(),
                           grVizOutput("selection_plot", width = "100%", height = "180px")),
                  tabPanel("Omitted Model", 
                           br(),
                           grVizOutput("omitted_plot", width = "100%", height = "180px")),
                  tabPanel("Naive Model", 
                           br(),
                           grVizOutput("naive_plot", width = "100%", height = "180px"))
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      # panel title
      "Longitudinal Design (CATA)",
      # define sidebar
      sidebarLayout(
        sidebarPanel(
          width = 2,
          style = "background: #EBEBE4",
          h4("Parameters", style = "margin-top: 0rem"),
          hr(),
          strong(h5("Correlations")),
          sliderInput("cata_eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("cata_mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("cata_mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          sliderInput("cata_eo_ui", "EO - Disturbance Term", value = 0.8, min = 0, max = 1),
          hr(),
          strong(h5("Options")),
          sliderInput("cata_num_obs", "Mean Observations per Firm", value = 10, min = 5, max = 15),
          sliderInput("cata_num_firms", "Number of Firms", value = 1000, min = 50, max = 3000),
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
                style = "padding: 0.7rem; height: 470px; background: #EBEBE4",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("cata_data")
              )
            ),
            # shows density plot of simulated data
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 470px; background: #EBEBE4",
                strong("Simulated Data Densities (Across Firms)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5 rem"),
                plotlyOutput("cata_density")
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
                style = "padding: 0.7rem; height: 446px; background: #EBEBE4",
                strong("Model Results (% difference to true EO - Performance Correlation of 0.25)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                reactableOutput("cata_table_fit"),
                hr(),
                strong("Estimate:"), "Strength of the EO - Performance relationship"
              )
            ),
            # shows results as plots
            column(
              6,
              wellPanel(
                style = "padding: 0.7rem; height: 446px; background: #EBEBE4",
                strong("Model Fit"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                tabsetPanel(
                  tabPanel(
                    "Correct Model",
                    br(),
                    reactableOutput("cata_correct_tidy"),
                    br(),
                    reactableOutput("cata_correct_glance")
                  ),
                  tabPanel(
                    "Omitted Model",
                    br(),
                    reactableOutput("cata_omitted_tidy"),
                    br(),
                    reactableOutput("cata_omitted_glance")
                  ),
                  tabPanel(
                    "Random Model",
                    br(),
                    reactableOutput("cata_random_tidy"),
                    br(),
                    reactableOutput("cata_random_glance")
                  ),
                  tabPanel(
                    "Naive Model",
                    br(),
                    reactableOutput("cata_naive_tidy"),
                    br(),
                    reactableOutput("cata_naive_glance")
                  ),
                  tabPanel(
                    "ME Model",
                    br(),
                    reactableOutput("cata_me_tidy"),
                    br(),
                    reactableOutput("cata_me_glance")
                  )
                )
              )
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
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#EBEBE4"
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
        ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#EBEBE4"
      )
    )
  })

  # create model plots
  # correct model
  output$correct_plot <- renderGrViz({
    lavaanPlot(
      model = correct_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  })

  # selection model
  output$selection_plot <- renderGrViz({
    lavaanPlot(
      model = selection_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  })

  # omitted variable model
  output$omitted_plot <- renderGrViz({
    lavaanPlot(
      model = omitted_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  })

  # naive model
  output$naive_plot <- renderGrViz({
    lavaanPlot(
      model = naive_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  })

  # cata model
  # between firms data (level 2)
  level2_df <- reactive({
    level2_df <- defData(varname = "u_i", dist = "normal", formula = 0, variance = 1, id = "FirmID")
    level2_df <- defData(level2_df, varname = "NumObs", dist = "noZeroPoisson", formula = input$cata_num_obs) # fix
    level2_df <- genData(input$cata_num_firms, level2_df)
  })

  # within firm data (level 1)
  level1_df <- reactive({
    level1_df <- genCluster(level2_df(),
      cLevelVar = "FirmID", numIndsVar = "NumObs",
      level1ID = "ObsID"
    )
    level1_model <- defDataAdd(varname = "e_ij", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "Munificence", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "EO_True", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model,
      varname = "EO", dist = "normal",
      formula = paste0("EO_True + ", input$cata_eo_ui, " * u_i + ", input$cata_mun_eo, " * Munificence"), variance = 1
    )
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
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#EBEBE4"
      )
    )
  })

  # create a density plot with ggplot2 and convert it to plotly
  output$cata_density <- renderPlotly({
    # cast dataframe from wide to long
    sim2_density <- sim2_df() %>%
      select(EO, EO_True, Performance, Munificence, M_Error, u_i, e_ij) %>%
      pivot_longer(cols = c(EO, EO_True, Performance, Munificence, M_Error, u_i, e_ij), names_to = "variables", values_to = "value")

    # create ggplot2 density plot
    p <- ggplot(sim2_density, aes(value, fill = factor(variables), colour = factor(variables))) +
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


  # fit models
  # cata correct fit
  cata_correct_fit <- reactive({
    cata_correct_fit <- plm(Performance ~ EO + Munificence, data = sim2_df(), index = c("FirmID", "ObsID"), model = "within")
  })

  # cata ommitted fit
  cata_omitted_fit <- reactive({
    cata_omitted_fit <- plm(Performance ~ EO, data = sim2_df(), index = c("FirmID", "ObsID"), model = "within")
  })

  # cata random fit
  cata_random_fit <- reactive({
    cata_random_fit <- plm(Performance ~ EO + Munificence, data = sim2_df(), index = c("FirmID", "ObsID"), model = "random")
  })

  # cata naive model (random effects and omitted variable)
  cata_naive_fit <- reactive({
    cata_naive_fit <- plm(Performance ~ EO, data = sim2_df(), index = c("FirmID", "ObsID"), model = "random")
  })

  # cata naive with measurement error
  cata_me_fit <- reactive({
    sim2_tmp <- sim2_df() %>%
      mutate(EO_Error = EO + M_Error)

    cata_me_fit <- plm(Performance ~ EO_Error, data = sim2_tmp, index = c("FirmID", "ObsID"), model = "random")
  })

  # create cata results table
  output$cata_table_fit <- renderReactable({

    # We start by storing the model results in new dataframes.
    # The correct model
    correct_res <- tidy(cata_correct_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # omitted variable model
    omitted_res <- tidy(cata_omitted_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # random effects fit model
    random_res <- tidy(cata_random_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # The naive model
    naive_res <- tidy(cata_naive_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # The measurement error model
    me_res <- tidy(cata_me_fit()) %>%
      filter(term == "EO_Error") %>%
      select(estimate, std.error)

    # create results data frame
    cata_res <- bind_rows(correct_res, omitted_res, random_res, naive_res, me_res) %>%
      mutate_all(round, 3) %>% # Round to three decimal places
      #  Calculate the percentage difference between the model parameter and the
      #   simulation parameter of .25
      mutate(Difference = as.character(round(100 * (abs(input$cata_eo_pf - estimate) / ((input$cata_eo_pf + estimate) / 2))))) %>%
      mutate(Difference = glue("{Difference}%"))
    # Create a new dataframe to hold our model names
    model_names <- data_frame(model = c("Correct Model", "Omitted Variable Model", "Random Effects Model", "Naive Model", "Measurement Error Model"))

    # Add the model names
    cata_res <- bind_cols(model_names, cata_res)

    # create reactable
    cata_table_fit <- reactable(cata_res,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        model = colDef(name = "Model", align = "left", width = 200),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        Difference = colDef(align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#EBEBE4"
      )
    )
  })

  # create model fit tables
  # correct model
  # parameters table
  output$cata_correct_tidy <- renderReactable({

    # extract model fit info
    table <- tidy(cata_correct_fit())
    table <- table %>%
      mutate(estimate = round(estimate, 2)) %>%
      mutate(std.error = round(std.error, 3)) %>%
      mutate(statistic = round(statistic, 2)) %>%
      mutate(p.value = round(p.value, 4))

    cata_correct_tidy <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        term = colDef(name = "Variable", align = "left"),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })
  # model fit
  output$cata_correct_glance <- renderReactable({
    table <- glance(cata_correct_fit())
    table <- table %>%
      mutate_all(round, 3)

    cata_correct_glance <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        r.squared = colDef(name = "R2", align = "center"),
        adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center"),
        deviance = colDef(name = "Deviance", align = "center"),
        df.residual = colDef(name = "Residual DF", align = "center"),
        nobs = colDef(name = "N", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })

  # omitted variable model
  # parameters table
  output$cata_omitted_tidy <- renderReactable({

    # extract model fit info
    table <- tidy(cata_omitted_fit())
    table <- table %>%
      mutate(estimate = round(estimate, 2)) %>%
      mutate(std.error = round(std.error, 3)) %>%
      mutate(statistic = round(statistic, 2)) %>%
      mutate(p.value = round(p.value, 4))

    cata_omitted_tidy <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        term = colDef(name = "Variable", align = "left"),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })

  # model fit table
  output$cata_omitted_glance <- renderReactable({
    table <- glance(cata_omitted_fit())
    table <- table %>%
      mutate_all(round, 3)

    cata_omitted_glance <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        r.squared = colDef(name = "R2", align = "center"),
        adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center"),
        deviance = colDef(name = "Deviance", align = "center"),
        df.residual = colDef(name = "Residual DF", align = "center"),
        nobs = colDef(name = "N", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })

  # random model
  # parameters table
  output$cata_random_tidy <- renderReactable({

    # extract model fit info
    table <- tidy(cata_random_fit())
    table <- table %>%
      mutate(estimate = round(estimate, 2)) %>%
      mutate(std.error = round(std.error, 3)) %>%
      mutate(statistic = round(statistic, 2)) %>%
      mutate(p.value = round(p.value, 4))

    cata_random_tidy <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        term = colDef(name = "Variable", align = "left"),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })
  # model fit table
  output$cata_random_glance <- renderReactable({
    table <- glance(cata_random_fit())
    table <- table %>%
      mutate_all(round, 3)

    cata_random_glance <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        r.squared = colDef(name = "R2", align = "center"),
        adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center"),
        deviance = colDef(name = "Deviance", align = "center"),
        df.residual = colDef(name = "Residual DF", align = "center"),
        nobs = colDef(name = "N", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })

  # naive model
  # parameters table
  output$cata_naive_tidy <- renderReactable({

    # extract model fit info
    table <- tidy(cata_naive_fit())
    table <- table %>%
      mutate(estimate = round(estimate, 2)) %>%
      mutate(std.error = round(std.error, 3)) %>%
      mutate(statistic = round(statistic, 2)) %>%
      mutate(p.value = round(p.value, 4))

    cata_naive_tidy <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        term = colDef(name = "Variable", align = "left"),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })
  # model fit table
  output$cata_naive_glance <- renderReactable({
    table <- glance(cata_naive_fit())
    table <- table %>%
      mutate_all(round, 3)

    cata_naive_glance <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        r.squared = colDef(name = "R2", align = "center"),
        adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center"),
        deviance = colDef(name = "Deviance", align = "center"),
        df.residual = colDef(name = "Residual DF", align = "center"),
        nobs = colDef(name = "N", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })

  # measurement error model (naive + me)
  # parameters table
  output$cata_me_tidy <- renderReactable({

    # extract model fit info
    table <- tidy(cata_me_fit())
    table <- table %>%
      mutate(estimate = round(estimate, 2)) %>%
      mutate(std.error = round(std.error, 3)) %>%
      mutate(statistic = round(statistic, 2)) %>%
      mutate(p.value = round(p.value, 4))

    cata_me_tidy <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        term = colDef(name = "Variable", align = "left"),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })
  # model fit table
  output$cata_me_glance <- renderReactable({
    table <- glance(cata_me_fit())
    table <- table %>%
      mutate_all(round, 3)

    cata_me_glance <- reactable(table,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        r.squared = colDef(name = "R2", align = "center"),
        adj.r.squared = colDef(name = "R2 (adj.)", align = "center"),
        statistic = colDef(name = "Test Statistic", align = "center"),
        p.value = colDef(name = "p-Value", align = "center"),
        deviance = colDef(name = "Deviance", align = "center"),
        df.residual = colDef(name = "Residual DF", align = "center"),
        nobs = colDef(name = "N", align = "center")
      ),
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor =  "#f5f5f5",
        backgroundColor = "#f5f5f5"
      )
    )
  })
}
# Run the application
shinyApp(ui = ui, server = server)
