# Load packages
library(shiny)
library(shinycssloaders)
library(bslib)
library(tidyverse)
library(lavaan)
library(reactable)
library(plotly)
library(broom)
library(glue)
library(lavaanPlot)
library(DiagrammeR)

# Load helpers
source("R/ui_components.R")
source("R/simulation_helpers.R")
source("R/cata_fit.R")

set.seed(123)

custom_theme <- bs_theme(
  version = 5,
  primary = "#009260",
  secondary = "#7F8990",
  base_font = "Arial",
  code_font = font_google("Roboto Mono"),
  `enable-gradients` = FALSE,
  `enable-transitions` = FALSE,
  `enable-shadows` = TRUE
)

ui <- fluidPage(
  theme = custom_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  navbarPage(
    title = div(
      "The Chicken or the Egg?",
      img(
        src = "index.png",
        height = "34px",
        class = "navbar-logo-lockup"
      )
    ),
    windowTitle = "The Chicken or the Egg?",
    navbarMenu(
      "About",
      source(file.path("ui/01_ui_about_tab.R"), local = TRUE)$value,
      source(file.path("ui/01_ui_survey_about_tab.R"), local = TRUE)$value,
      source(file.path("ui/01_ui_cata_about_tab.R"), local = TRUE)$value
    ),
    source(file.path("ui/02_ui_survey_tab.R"), local = TRUE)$value,
    source(file.path("ui/03_ui_cata_tab.R"), local = TRUE)$value
  )
)

server <- function(input, output, session) {
  source(file.path("server/01_server_survey_tab.R"), local = TRUE)$value
  source(file.path("server/02_server_cata_tab.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
