#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
library(lavaan)
library(simstandard)
library(reactable)
library(plotly)
set.seed(1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # custom theming based on corporate design of the university
  theme = bslib::bs_theme(
    version = 4, fg = "#0C0C0C", primary = "#009260", base_font = "Gill Sans",
    code_font = font_google("Roboto Mono"), bootswatch = "united",
    bg = "#FFFFFF"
  ),

  # define top level navigation bar
  navbarPage(
    "The Chicken or the Egg?",
    id = "main_navbar",

    # define first panel
    tabPanel(
      "Survey Design",
      sidebarLayout(
        sidebarPanel(width = 2,
          h5("Correlations"),
          sliderInput("eo_pf", "EO - Performance", value = 0.25, min = 0, max = 1),
          sliderInput("mun_eo", "Munificence - EO", value = 0.43, min = 0, max = 1),
          sliderInput("mun_pf", "Munificence - Performance", value = 0.16, min = 0, max = 1),
          h5("Options"),
          sliderInput("sample", "Sample Size", value = 200, min = 50, max = 10000),
          sliderInput("selection", "Selection Effect", value = 0.35, min = 0, max = 1)
        ),
        mainPanel(width = 10,
                  fluidRow(
                      column(5,
                             wellPanel(
                               h5("Simulated Data"),
                               reactableOutput("data")
                             )
                      ),
                    column(5,
                           wellPanel(
                             h5("Density Plot"),
                             plotlyOutput("density")
                           )
                           )
                  ),
                  fluidRow(
                    column(5,
                           "hello"),
                    column(5,
                           "hello again")
                  )
          
        )
          
     
      ),
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
 
  # Define the simulation model
  sim.model <- reactive({
    paste0(
      "# First order reflective measurement model \n",
      "EO =~ .6 * INN1 + .85 * INN2 + .85 * INN3 + .79 * PRO1 + .82 * PRO2 + .73 * PRO3 + .79 * RISK1 + .82 * RISK2 + .82 * RISK3 \n",
      "Perf ~", input$eo_pf, " * EO \n",
      "EO ~ ", input$selection, " * Selection +", input$mun_eo, " * Munificence \n",
      " Perf ~ ", input$selection, " * Selection + ", input$mun_pf, " * Munificence \n"
    )
  })

  sim1.df <- reactive({
    sim1.df <- sim_standardized(sim.model(), n = input$sample, latent = FALSE, errors = FALSE)
    sim1.df <- sim1.df %>%
      mutate(EO = (INN1 + INN2 + INN3 +
        PRO1 + PRO2 + PRO3 +
        RISK1 + RISK2 + RISK3) / 9) %>%
      relocate(EO, Perf)
  })

  output$data <- renderReactable({
    reactable(sim1.df(),
      defaultPageSize = 9,
      height = 400,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = list(
        EO = colDef(format = colFormat(digits = 2)),
        Perf = colDef(format = colFormat(digits = 2)),
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
  
  output$density <- renderPlotly({
      sim1.ds <- sim1.df() %>% 
          select(EO, Perf, Selection, Munificence) %>% 
          pivot_longer(cols = c(EO, Perf, Selection, Munificence),names_to = "variables", values_to = "value")
      
      p <- ggplot(sim1.ds, aes(value, fill = factor(variables), colour = factor(variables))) +
          geom_density(alpha = 0.3, adjust = 4) +
          scale_x_continuous("Values") +
          scale_y_continuous("Density") +
          theme_minimal() +
          theme(legend.title = element_blank()) + 
          scale_colour_viridis_d() +
          scale_fill_viridis_d()
     p <- ggplotly(p, tooltip = c("y", "x"), height = 400)
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
