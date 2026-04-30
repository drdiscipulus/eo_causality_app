tabPanel(
  "Longitudinal Design",
  about_copy(
    h3("Longitudinal Design (CATA)"),
    p("Uses secondary indicators for EO with panel data, often from content analysis of publicly traded firms' letters to shareholders or annual reports."),
    h4("Parameters"),
    tags$ul(
      tags$li("Performance Outcome: continuous, normally distributed variable."),
      tags$li("EO Predictor: unidimensional composite variable using innovativeness, proactiveness, and risk-taking."),
      tags$li("EO-Performance Correlation: true effect of 0.25 following Rauch et al. (2009)."),
      tags$li("Omitted Variable: environmental munificence."),
      tags$li("Munificence-EO Correlation: true effect of 0.46 following Rosenbusch et al. (2013)."),
      tags$li("Munificence-Performance Correlation: true effect of 0.16 following Rosenbusch et al. (2013)."),
      tags$li("Sample Size: default is 1,000 firms with a mean of 10 observations per firm."),
      tags$li("Level 2 Disturbance Term-EO Correlation: default of 0.8."),
      tags$li("Measurement Error: variance drawn from McKenny et al. (2018).")
    ),
    h4("Models"),
    tags$ul(
      tags$li("Correct Model: includes munificence and uses fixed effects."),
      tags$li("Omitted Variable Model: excludes munificence from the correct model."),
      tags$li("Random Effects Model: applies random effects to the correct model."),
      tags$li("Naive Model: excludes munificence and uses random effects."),
      tags$li("Measurement Error Model: introduces measurement error to the naive model.")
    )
  )
)
