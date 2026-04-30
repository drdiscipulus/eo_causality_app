tabPanel(
  "Survey Design",
  about_copy(
    h3("Survey Design"),
    p("Mimics a survey-based design where the researcher collects psychometric EO measures, often the nine-item Covin and Slevin (1989) scale, from the senior-most executive of a business unit."),
    h4("Parameters"),
    tags$ul(
      tags$li("Performance Outcome: continuous, normally distributed variable."),
      tags$li("EO Predictor: nine EO indicators with factor loadings from George (2011)."),
      tags$li("EO-Performance Correlation: true effect of 0.25 following Rauch et al. (2009)."),
      tags$li("Omitted Variable: environmental munificence."),
      tags$li("Munificence-EO Correlation: true effect of 0.46 following Rosenbusch et al. (2013)."),
      tags$li("Munificence-Performance Correlation: true effect of 0.16 following Rosenbusch et al. (2013)."),
      tags$li("Unobserved Selection Effect: 0.35 for self-selection into the survey condition."),
      tags$li("Sample Size: default is 200 firms, with the app allowing up to 10,000.")
    ),
    h4("Models"),
    tags$ul(
      tags$li("Correct Model: includes the selection term, munificence term, and accounts for measurement error with SEM."),
      tags$li("Selection-Effect Model: excludes the selection term from the correct model."),
      tags$li("Omitted Variable Model: excludes the munificence term from the correct model."),
      tags$li("Naive Model: excludes both selection and munificence."),
      tags$li("Measurement Error Model: runs the naive model as OLS instead of SEM.")
    )
  )
)
