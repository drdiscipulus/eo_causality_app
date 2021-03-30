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

# load scripts
source("cata_fit.R")

# set seed for random numbers
set.seed(123)

# define the ui of the app
ui <- fluidPage(
  # some theming first
  # use bslib to customize the bootswatch united theme
  theme = bslib::bs_theme(
    version = 4, fg = "#0C0C0C", primary = "#009260", base_font = "Segoe UI",
    code_font = font_google("Roboto Mono"), bootswatch = "united",
    bg = "#FFFFFF"
  ),
  # customize the theming of the active tabpanels
  tags$style(HTML(".nav-tabs > li.active > a {background-color: #009260; color: #fff; border-color: #B6B6B6}")),

  # base layout of the app is a navbarpage that gives us a navigation panel with tabs at the top
  navbarPage(
    # set the title of the page and put logo of the university at at the top right
    title = div("The Chicken or the Egg?", img(src = "index.png", height = "50px", style = "position: absolute; right: 20px; top:5px;")),
    windowTitle = "The Chicken or the Egg?",
    # the first element in the navbar should have sub elements, thus set it up as a navbarmenu
    navbarMenu(
      # menu title
      "Info",
      # first panel of the menu
      tabPanel(
        # title
        "About",
        # define the page as fluid row
        fluidRow(
          # plain text can be added in simple quotation marks, more refined option require html tags
          tags$div(
            tags$h4("Purpose"),
            "This web application is a companion app to the following publication in Entrepreneurship Theory and Practice:",
            tags$br(), tags$br(), tags$a(href = "https://doi.org/10.1177/1042258720976368", "Anderson, B. S., Schüler, J., Baum, M., Wales, W. J., & Gupta, V. K. (2020). The Chicken or the Egg? Causal Inference in Entrepreneurial Orientation–Performance Research. Entrepreneurship Theory and Practice. Online First.", target = "_blank"),
            tags$br(), tags$br(), "The simulations focus on two research designs that are commonly applied in Entrepreneurial Orientation (EO) resarch:",
            tags$ul(
              tags$li("Survey design with psychometric indicators"),
              tags$li("A longitudinal design drawing from secondary data (e.g. content analysis of annual reports)")
            ),
            tags$h4("Usage"),
            tags$ul(
              tags$li("Select a parameter combination of your choice and click the Run-Button to start the simulations"),
              tags$li("Click the Reset-Button to restore default vaules")
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
      # second panel of the menu that explains the survey design simulation
      tabPanel(
        # title
        "Survey Design",
        # again, work with html tags to format text
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
      # third panel of the menu that explains the longitudinal design simulation
      tabPanel(
        # title
        "Longitudinal Design",
        # again, work with html tags to format text
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
    # define the second panel in the navbar
    tabPanel(
      # panel title
      "Survey Design",
      # this panel should have a sidebar and a mainpanel
      # define sidebar that holds all inputs
      sidebarLayout(
        # do some styling first
        sidebarPanel(
          width = 2,
          style = "height: 814px; background: #F0F0F0",
          h4("Parameters", style = "margin-top: 0rem"),
          hr(),
          # shiny slider inputs
          sliderInput("eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          sliderInput("sample", "Sample Size (Firms)", value = 200, min = 50, max = 10000),
          sliderInput("selection", "Selection Effect", value = 0.35, min = 0, max = 0.5),
          hr(),
          actionButton("compute", "Run", class = "btn-primary btn-block"),
          actionButton("reset", "Reset", class = "btn-secondary btn-block")
        ),
        # define main window
        mainPanel(
          # shiny allows for 12 columns in terms of width, sidebar has 2, thus mainpanel has 10
          width = 10,
          # we put two columns into the first row of the main panel
          fluidRow(
            # column 1 shows the simulated data in a table and has a width of 6
            column(
              6,
              # well panel puts a visual box around the column in that row
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 470px; background: #F0F0F0",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # put reactable output in that box
                reactableOutput("data")
              )
            ),
            # column 2 shos the density plots of the simulated variables, has a width of 6
            column(
              6,
              # again, we put a box around the area
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 470px; background: #F0F0F0",
                strong("Simulated Data Densities"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # put plotly density plot in that box
                plotlyOutput("density")
              )
            )
          ),
          # empty line between top and bottom row
          br(),
          # setup the second row
          fluidRow(
            # the first column has a width of 6 and shows the difference between the models in a table
            column(
              6,
              # again, we put a box around the area
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 320px; background: #F0F0F0",
                strong("Model Results (% difference to true EO - Performance Correlation of 0.25)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # put the reactable table in that box
                reactableOutput("table_fit"),
                # add horizontal line
                hr(),
                # add some explanation
                strong("Estimate:"), "Strength of the EO - Performance relationship"
              )
            ),
            # the second column shows the sem plots with parameters, width of 6
            column(
              6,
              # again, we put a box around the area
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 320px; background: #F0F0F0",
                strong("Model Results Plots"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # setup clickable tabpanels, each containing the respective plot
                tabsetPanel(
                  # lavaanplot draws on diagrammer, thus use the appropriate shiny functions
                  tabPanel(
                    "Correct Model",
                    br(),
                    grVizOutput("correct_plot", width = "100%", height = "180px")
                  ),
                  tabPanel(
                    "Selection Model",
                    br(),
                    grVizOutput("selection_plot", width = "100%", height = "180px")
                  ),
                  tabPanel(
                    "Omitted Model",
                    br(),
                    grVizOutput("omitted_plot", width = "100%", height = "180px")
                  ),
                  tabPanel(
                    "Naive Model",
                    br(),
                    grVizOutput("naive_plot", width = "100%", height = "180px")
                  )
                )
              )
            )
          )
        )
      )
    ),
    # define the third panel in the navbar
    tabPanel(
      # panel title
      "Longitudinal Design (CATA)",
      # this panel should have a sidebar and a mainpanel
      # define sidebar that holds all inputs
      sidebarLayout(
        sidebarPanel(
          # some styling first
          width = 2,
          style = "background: #F0F0F0; height: 960px;",
          h4("Parameters", style = "margin-top: 0rem"),
          hr(),
          sliderInput("cata_eo_pf", "EO - Performance", value = 0.25, min = 0, max = 0.7),
          sliderInput("cata_mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 0.7),
          sliderInput("cata_mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 0.7),
          sliderInput("cata_eo_ui", "EO - Disturbance Term", value = 0.8, min = 0, max = 1),
          sliderInput("cata_num_obs", "Mean Observations per Firm", value = 10, min = 5, max = 15),
          sliderInput("cata_num_firms", "Number of Firms", value = 1000, min = 50, max = 3000),
          sliderInput("cata_m_error", "Measurement Error Variance", value = 1.85, min = 0.5, max = 2.5),
          hr(),
          actionButton("cata_compute", "Run", class = "btn-primary btn-block"),
          actionButton("cata_reset", "Reset", class = "btn-secondary btn-block")
        ),
        # define main window
        mainPanel(
          width = 10,
          # first row
          fluidRow(
            # show table of simulated data in the first panel, width of 6
            column(
              6,
              # well panel puts a visual box around the column in that row
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 468px; background: #F0F0F0",
                strong("Simulated Data"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # put the reactable table in that box
                reactableOutput("cata_data")
              )
            ),
            # shows density plot of simulated data
            column(
              6,
              # well panel puts a visual box around the column in that row
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 468px; background: #F0F0F0",
                strong("Simulated Data Densities (Across Firms)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5 rem"),
                # put plotly density plot in that box
                plotlyOutput("cata_density")
              )
            )
          ),
          # empty line between top and bottom row
          br(),
          # setup the second row
          fluidRow(
            # show model comparison results as a table in the first column - width of 6
            column(
              6,
              # well panel puts a visual box around the column in that row
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 468px; background: #F0F0F0",
                strong("Model Results (% difference to true EO - Performance Correlation of 0.25)"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # put the reactable table in that box
                reactableOutput("cata_table_fit"),
                # add horizontal line
                hr(),
                # add explanation
                strong("Estimate:"), "Strength of the EO - Performance relationship"
              )
            ),
            # show model fit results as table
            column(
              6,
              # well panel puts a visual box around the column in that row
              wellPanel(
                # style the box
                style = "padding: 0.7rem; height: 468px; background: #F0F0F0",
                strong("Model Fit"),
                hr(style = "margin-top: 0.5rem; margin-bottom: 0.5rem"),
                # present model fit info in different tabs
                tabsetPanel(
                  # first tab
                  tabPanel(
                    "Correct Model",
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_correct_tidy"),
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_correct_glance")
                  ),
                  # second tab
                  tabPanel(
                    "Omitted Model",
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_omitted_tidy"),
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_omitted_glance")
                  ),
                  # third tab
                  tabPanel(
                    "Random Model",
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_random_tidy"),
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_random_glance")
                  ),
                  # fourth tab
                  tabPanel(
                    "Naive Model",
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_naive_tidy"),
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_naive_glance")
                  ),
                  # fifth tab
                  tabPanel(
                    "ME Model",
                    br(),
                    # put the reactable table in that box
                    reactableOutput("cata_me_tidy"),
                    br(),
                    # put the reactable table in that box
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

# define server logic that runs all computations
server <- function(input, output) {

  # reset inputs
  observeEvent(input$reset, {
    updateSliderInput(inputId = "eo_pf", value = 0.25, min = 0, max = 0.7)
    updateSliderInput(inputId = "mun_eo", value = 0.43, min = 0, max = 0.7)
    updateSliderInput(inputId = "mun_pf", value = 0.16, min = 0, max = 0.7)
    updateSliderInput(inputId = "sample", value = 200, min = 50, max = 10000)
    updateSliderInput(inputId = "selection", value = 0.35, min = 0, max = 0.5)
  })

  # simulation 1
  # define the simulation model
  sim_model <- reactive({
    # wrap it up with paste0 to factor in dynamic inputs
    paste0(
      "# First order reflective measurement model \n",
      "EO =~ .6 * INN1 + .85 * INN2 + .85 * INN3 + .79 * PRO1 + .82 * PRO2 + .73 * PRO3 + .79 * RISK1 + .82 * RISK2 + .82 * RISK3 \n",
      "Performance ~", input$eo_pf, " * EO \n",
      "EO ~ ", input$selection, " * Selection +", input$mun_eo, " * Munificence \n",
      "Performance ~ ", input$selection, " * Selection + ", input$mun_pf, " * Munificence \n"
    )
  }) %>% bindEvent(input$compute)

  # simulate the data set based on the model
  sim1_df <- reactive({
    sim1_df <- sim_standardized(sim_model(), n = input$sample, latent = FALSE, errors = FALSE)

    # add an aggregate eo column
    sim1_df <- sim1_df %>%
      mutate(avgEO = (INN1 + INN2 + INN3 +
        PRO1 + PRO2 + PRO3 +
        RISK1 + RISK2 + RISK3) / 9) %>%
      # reorder columns a bit
      relocate(avgEO, Performance)
  }) %>% bindEvent(input$compute)

  # create an reactable table for the simulated data set
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
      # apply some column styling
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
      # apply theming options
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor = "#E0E0E0",
        backgroundColor = "#F0F0F0"
      )
    )
  }) %>% bindEvent(input$compute)

  # three step density plot creation procedure
  output$density <- renderPlotly({
<<<<<<< HEAD

    # first, case data frame from wide to long
=======
    
    # first, cast data frame from wide to long
>>>>>>> 269266bf9a83984ae7bbacb1c7de3dafc2bbe74e
    sim1_density <- sim1_df() %>%
      select(avgEO, Performance, Selection, Munificence) %>%
      pivot_longer(cols = c(avgEO, Performance, Selection, Munificence), names_to = "variables", values_to = "value")

    # second, create density plot with ggplot2
    p <- ggplot(sim1_density, aes(value, fill = factor(variables), colour = factor(variables))) +
      geom_density(alpha = 0.3, adjust = 4) +
      scale_x_continuous("Values") +
      scale_y_continuous("Density") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "white", size = 0.5)
      ) +
      scale_colour_viridis_d() +
      scale_fill_viridis_d()

    # third, convert ggplot into a plotly plot
    p <- ggplotly(p, tooltip = c("y", "x"), height = 400) %>%
      layout(paper_bgcolor = "transparent") %>%
      layout(plot_bgcolor = "transparent") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c(
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ))

    # edit tooltip
    for (i in seq_along(p$x$data)) {
      x <- p$x$data[[i]]$x
      y <- p$x$data[[i]]$y
      p$x$data[[i]]$text <- map2(x, y, ~ paste0("Density: ", round(.y, digits = 2), "<br />Value: ", round(.x, digits = 2)))
    }
    
    p
  }) %>% bindEvent(input$compute)

  # fit the various models
  # fit the correct model
  correct_fit <- reactive({

    # specify the model
    model <- "# First order reflective measurement model
                    EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
                    # Structural parameters
                    Performance ~ EO + Selection + Munificence
                    # Covariances between EO and the omitted variables
                    EO ~~ Selection
                    EO ~~ Munificence"

    # get model fit
    correct_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  }) %>% bindEvent(input$compute)

  # fit the selection effects model (only selection bias)
  selection_fit <- reactive({

    # specify the model
    model <- "# First order reflective measurement model
                EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
                # Structural parameters
                Performance ~ EO + Munificence
                # Covariances between EO and the omitted variables
                EO ~~ Munificence"

    # get model fit
    selection_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  }) %>% bindEvent(input$compute)

  # run omitted variable model (only omitted variable bias)
  omitted_fit <- reactive({

    # specify the model
    model <- "# First order reflective measurement model
              EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
              # Structural parameters
              Performance ~ EO + Selection
              # Covariances between EO and the omitted variables
              EO ~~ Selection"

    # get model fit
    omitted_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  }) %>% bindEvent(input$compute)

  # run naive model (omitted variable and selection bias)
  naive_fit <- reactive({

    # specify the model
    model <- "# First order reflective measurement model
              EO =~ INN1 + INN2 + INN3 + PRO1 + PRO2 + PRO3 + RISK1 + RISK2 + RISK3
              # Structural parameters
              Performance ~ EO"

    # get model fit
    naive_fit <- sem(model, data = sim1_df(), std.lv = TRUE)
  }) %>% bindEvent(input$compute)

  # run measurement error model (measurement error only model - done with lm)
  error_fit <- reactive({

    # get model fit
    error_fit <- lm(scale(Performance) ~ scale(avgEO), data = sim1_df())
  }) %>% bindEvent(input$compute)

  # create the output table that contrasts the models
  output$table_fit <- renderReactable({

    # extract required model parameters from all models
    # the correct model
    correct_res <- tidy(correct_fit()) %>%
      filter(term == "Performance ~ EO") %>%
      select(estimate, std.error)

    # the selection effect model
    selection_res <- tidy(selection_fit()) %>%
      filter(term == "Performance ~ EO") %>%
      select(estimate, std.error)

    # the omitted variable model
    omitted_res <- tidy(omitted_fit()) %>%
      filter(term == "Performance ~ EO") %>%
      select(estimate, std.error)

    # the naive model
    naive_res <- tidy(naive_fit()) %>%
      filter(term == "Performance ~ EO") %>%
      select(estimate, std.error)

    # the measurement error model
    error_res <- tidy(error_fit()) %>%
      filter(term == "scale(avgEO)") %>%
      select(estimate, std.error)

    # combine this information into a single data frame
    res <- bind_rows(correct_res, selection_res, omitted_res, naive_res, error_res) %>%
      mutate_all(round, 3) %>%
      # calculate the respective model's deviation from the true eo-performance correlation
      mutate(Difference = as.character(round(100 * (abs(input$eo_pf - estimate) / ((input$eo_pf + estimate) / 2))))) %>%
      mutate(Difference = glue("{Difference}%"))

    # set the model names
    model_names <- data_frame(model = c("Correct Model", "Selection Effect Model", "Omitted Variable Model", "Naive Model", "Measurement Error Model"))

    # add model names to the results
    res <- bind_cols(model_names, res)

    # use this data set to create a reactable
    table_fit <- reactable(res,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      # some column styling
      columns = list(
        model = colDef(name = "Model", align = "left", width = 200),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        Difference = colDef(align = "center")
      ),
      # table theming
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor = "#E0E0E0",
        backgroundColor = "#F0F0F0"
      )
    )
  }) %>% bindEvent(input$compute)

  # plot the sem models with lavaanplot (draws on diagrammer)
  # correct model
  output$correct_plot <- renderGrViz({
    lavaanPlot(
      model = correct_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  }) %>% bindEvent(input$compute)

  # selection model
  output$selection_plot <- renderGrViz({
    lavaanPlot(
      model = selection_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  }) %>% bindEvent(input$compute)

  # omitted variable model
  output$omitted_plot <- renderGrViz({
    lavaanPlot(
      model = omitted_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  }) %>% bindEvent(input$compute)

  # naive model
  output$naive_plot <- renderGrViz({
    lavaanPlot(
      model = naive_fit(), node_options = list(shape = "box", fontname = "Helvetica"),
      edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "regress"
    )
  }) %>% bindEvent(input$compute)

  # simulation 2: longitudinal cata model

  # reset inputs
  observeEvent(input$cata_reset, {
    updateSliderInput(inputId = "cata_eo_pf", value = 0.25, min = 0, max = 0.7)
    updateSliderInput(inputId = "cata_mun_eo", value = 0.43, min = 0, max = 0.7)
    updateSliderInput(inputId = "cata_mun_pf", value = 0.16, min = 0, max = 0.7)
    updateSliderInput(inputId = "cata_eo_ui", value = 0.8, min = 0, max = 1)
    updateSliderInput(inputId = "cata_num_obs", value = 10, min = 5, max = 15)
    updateSliderInput(inputId = "cata_num_firms", value = 1000, min = 50, max = 3000)
    updateSliderInput(inputId = "cata_m_error", value = 1.85, min = 0.5, max = 2.5)
  })

  # level 2: between firms
  # level 1: within firms

  # between firms data (level 2)
  level2_df <- reactive({
    level2_df <- defData(varname = "u_i", dist = "normal", formula = 0, variance = 1, id = "FirmID")
    level2_df <- defData(level2_df, varname = "NumObs", dist = "noZeroPoisson", formula = input$cata_num_obs) # fix
    level2_df <- genData(input$cata_num_firms, level2_df)
  }) %>% bindEvent(input$cata_compute)

  # within firm data (level 1)
  level1_df <- reactive({
    # create the data frame
    level1_df <- genCluster(level2_df(),
      cLevelVar = "FirmID", numIndsVar = "NumObs",
      level1ID = "ObsID"
    )
    # create model, variable by variable
    level1_model <- defDataAdd(varname = "e_ij", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "Munificence", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model, varname = "EO_True", dist = "normal", formula = 0, variance = 1)
    level1_model <- defDataAdd(level1_model,
      varname = "EO", dist = "normal",
      formula = paste0("EO_True + ", input$cata_eo_ui, " * u_i + ", input$cata_mun_eo, " * Munificence"), variance = 1
    )
    # add measurement error
    level1_model <- defDataAdd(level1_model, varname = "M_Error", dist = "normal", formula = 0, variance = input$cata_m_error)
    level1_df <- addColumns(level1_model, level1_df)
  }) %>% bindEvent(input$cata_compute)

  # create the final simulation data set
  sim2_df <- reactive({
    # group data frame by firm id
    sim2_df <- level1_df() %>%
      group_by(FirmID) %>%
      # within each group/firm, number observations
      mutate(ObsID = row_number()) %>%
      # ungroup
      ungroup() %>%
      # calculate performance
      mutate(Performance = (input$cata_eo_pf * EO) + (input$cata_mun_pf * Munificence) + u_i + e_ij)
  }) %>% bindEvent(input$cata_compute)

  # create reactable for simulated data
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
      # some column formatting
      columns = list(
        u_i = colDef(format = colFormat(digits = 2)),
        e_ij = colDef(format = colFormat(digits = 2)),
        Munificence = colDef(format = colFormat(digits = 2)),
        EO_True = colDef(format = colFormat(digits = 2)),
        EO = colDef(format = colFormat(digits = 2)),
        M_Error = colDef(format = colFormat(digits = 2)),
        Performance = colDef(format = colFormat(digits = 2))
      ),
      # theme table
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor = "#E0E0E0",
        backgroundColor = "#F0F0F0"
      )
    )
  }) %>% bindEvent(input$cata_compute)

  # three step density plot creation procedure (non-nested)
  output$cata_density <- renderPlotly({

    # first, cast data frame from wide to long
    sim2_density <- sim2_df() %>%
      select(EO, EO_True, Performance, Munificence, M_Error, u_i, e_ij) %>%
      pivot_longer(cols = c(EO, EO_True, Performance, Munificence, M_Error, u_i, e_ij), names_to = "variables", values_to = "value")

    # second, create density plot with ggplot2
    p <- ggplot(sim2_density, aes(value, fill = factor(variables), colour = factor(variables))) +
      geom_density(alpha = 0.3, adjust = 4) +
      scale_x_continuous("Values") +
      scale_y_continuous("Density") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "white", size = 0.5)
      ) +
      scale_colour_viridis_d() +
      scale_fill_viridis_d()

    # third, convert ggplot to plotly
    p <- ggplotly(p, tooltip = c("y", "x"), height = 400) %>%
      layout(paper_bgcolor = "transparent") %>%
      layout(plot_bgcolor = "transparent") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c(
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ))

    # edit tooltip
    for (i in seq_along(p$x$data)) {
      x <- p$x$data[[i]]$x
      y <- p$x$data[[i]]$y
      p$x$data[[i]]$text <- map2(x, y, ~ paste0("Density: ", round(.y, digits = 2), "<br />Value: ", round(.x, digits = 2)))
    }

    p
  }) %>% bindEvent(input$cata_compute)

  # fit all the models
  # correct model
  cata_correct_fit <- reactive({
    cata_correct_fit <- plm(Performance ~ EO + Munificence, data = sim2_df(), index = c("FirmID", "ObsID"), model = "within")
  }) %>% bindEvent(input$cata_compute)

  # omitted variable model
  cata_omitted_fit <- reactive({
    cata_omitted_fit <- plm(Performance ~ EO, data = sim2_df(), index = c("FirmID", "ObsID"), model = "within")
  }) %>% bindEvent(input$cata_compute)

  # random effects model
  cata_random_fit <- reactive({
    cata_random_fit <- plm(Performance ~ EO + Munificence, data = sim2_df(), index = c("FirmID", "ObsID"), model = "random")
  }) %>% bindEvent(input$cata_compute)

  # naive model (random effects and omitted variable)
  cata_naive_fit <- reactive({
    cata_naive_fit <- plm(Performance ~ EO, data = sim2_df(), index = c("FirmID", "ObsID"), model = "random")
  }) %>% bindEvent(input$cata_compute)

  # naive model with measurement error
  cata_me_fit <- reactive({
    sim2_tmp <- sim2_df() %>%
      mutate(EO_Error = EO + M_Error)

    cata_me_fit <- plm(Performance ~ EO_Error, data = sim2_tmp, index = c("FirmID", "ObsID"), model = "random")
  }) %>% bindEvent(input$cata_compute)

  # create the output table that conrasts the models
  output$cata_table_fit <- renderReactable({

    # extract required model parameters from all models
    # the correct model
    correct_res <- tidy(cata_correct_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # the omitted variable model
    omitted_res <- tidy(cata_omitted_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # the random effects model
    random_res <- tidy(cata_random_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # the naive model (random effects and omitted variable)
    naive_res <- tidy(cata_naive_fit()) %>%
      filter(term == "EO") %>%
      select(estimate, std.error)

    # the measurement error model (naive with measurement error)
    me_res <- tidy(cata_me_fit()) %>%
      filter(term == "EO_Error") %>%
      select(estimate, std.error)

    # combine fit results into one data frame
    cata_res <- bind_rows(correct_res, omitted_res, random_res, naive_res, me_res) %>%
      mutate_all(round, 3) %>%
      # calculate the respective model's deviation from the true eo-performance correlation
      mutate(Difference = as.character(round(100 * (abs(input$cata_eo_pf - estimate) / ((input$cata_eo_pf + estimate) / 2))))) %>%
      mutate(Difference = glue("{Difference}%"))

    # set model names
    model_names <- data_frame(model = c("Correct Model", "Omitted Variable Model", "Random Effects Model", "Naive Model", "Measurement Error Model"))

    # add model names to the results
    cata_res <- bind_cols(model_names, cata_res)

    # use this data to create a reactable
    cata_table_fit <- reactable(cata_res,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      # column formatting
      columns = list(
        model = colDef(name = "Model", align = "left", width = 200),
        estimate = colDef(name = "Estimate", align = "center"),
        std.error = colDef(name = "Std.Error", align = "center"),
        Difference = colDef(align = "center")
      ),
      # table theming
      theme = reactableTheme(
        highlightColor = "#bdbdbd",
        stripedColor = "#E0E0E0",
        backgroundColor = "#F0F0F0"
      )
    )
  }) %>% bindEvent(input$cata_compute)

  # create model fit tables for all models
  # extract model parameters from the correct model
  output$cata_correct_tidy <- renderReactable({

    # extract parameters
    get_cata_parameters(cata_correct_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model fit info from the correct model
  output$cata_correct_glance <- renderReactable({

    # extract parameters
    get_cata_fit(cata_correct_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model parameters from the omitted variable model
  output$cata_omitted_tidy <- renderReactable({

    # extract parameters
    get_cata_parameters(cata_omitted_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model fit info from the omitted variable model
  output$cata_omitted_glance <- renderReactable({

    # extract model fit
    get_cata_fit(cata_omitted_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract parameters from the random effects model
  output$cata_random_tidy <- renderReactable({

    # extract parameters
    get_cata_parameters(cata_random_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model fit from the random effects model
  output$cata_random_glance <- renderReactable({

    # extract model fit
    get_cata_fit(cata_random_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract parameters from the naive model
  output$cata_naive_tidy <- renderReactable({

    # extract parameters
    get_cata_parameters(cata_naive_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model fit from the naive model
  output$cata_naive_glance <- renderReactable({

    # extract model fit
    get_cata_fit(cata_naive_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract parameters from the measurement error model
  output$cata_me_tidy <- renderReactable({

    # extract parameters
    get_cata_parameters(cata_me_fit())
  }) %>% bindEvent(input$cata_compute)

  # extract model fit from the measurement error model
  output$cata_me_glance <- renderReactable({

    # extract model fit
    get_cata_fit(cata_me_fit())
  }) %>% bindEvent(input$cata_compute)
}

<<<<<<< HEAD
# run the application
=======
# Run the application
>>>>>>> 269266bf9a83984ae7bbacb1c7de3dafc2bbe74e
shinyApp(ui = ui, server = server)
