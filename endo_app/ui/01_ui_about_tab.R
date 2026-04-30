tabPanel(
  "General",
  about_copy(
    h3("The Chicken or the Egg?"),
    h4("Purpose"),
    p("This web application is a companion app to the following publication in Entrepreneurship Theory and Practice:"),
    p(
      tags$a(
        href = "https://doi.org/10.1177/1042258720976368",
        "Anderson, B. S., Schüler, J., Baum, M., Wales, W. J., & Gupta, V. K. (2020). The Chicken or the Egg? Causal Inference in Entrepreneurial Orientation-Performance Research. Entrepreneurship Theory and Practice. Online First.",
        target = "_blank"
      )
    ),
    p("The simulations focus on two research designs commonly applied in Entrepreneurial Orientation research:"),
    tags$ul(
      tags$li("Survey design with psychometric indicators"),
      tags$li("A longitudinal design drawing from secondary data, such as content analysis of annual reports")
    ),
    h4("Usage"),
    tags$ul(
      tags$li("Select a parameter combination and click Run to start the simulation."),
      tags$li("Click Reset to restore default values.")
    ),
    h4("Abstract"),
    p("While entrepreneurial orientation (EO) correlates with many organizational phenomena, we lack convincing evidence of causal relationships within EO's nomological network. We explore the challenges to establishing causal relationships with a systematic review of EO-performance research. We then use a simulation to illustrate how popular research designs in EO research limit our ability to make causal claims. We conclude by outlining the research design considerations to move from associational to causal EO-performance research."),
    p(
      "All code and data used in this publication is available on the Open Science Framework: ",
      tags$a(href = "https://osf.io/gxhmj/?view_only=5acc084c3dd240d38c72562fb44f2806", "Link to Repository", target = "_blank")
    ),
    h4("Profile Pages"),
    tags$ul(
      tags$li(tags$a(href = "https://bloch.umkc.edu/faculty-directory-anderson-brian/", "Brian S. Anderson", target = "_blank")),
      tags$li(tags$a(href = "https://www.eship.uni-bayreuth.de/de/team/schueler_jens/index.php", "Jens Schüler", target = "_blank")),
      tags$li(tags$a(href = "https://www.eship.uni-bayreuth.de/de/team/baum_matthias/index.php", "Matthias Baum", target = "_blank")),
      tags$li(tags$a(href = "https://www.albany.edu/business/faculty/william-wales", "William J. Wales", target = "_blank")),
      tags$li(tags$a(href = "https://culverhouse.ua.edu/news/directory/vishal-gupta/", "Vishal K. Gupta", target = "_blank"))
    ),
    tags$p(class = "about-credit", "R Shiny App written by: Jens Schüler")
  )
)
