# EO Causality Companion Shiny Source

This repository contains the R Shiny source code behind the companion web app
for the paper *The Chicken or the Egg? Causal Inference in Entrepreneurial
Orientation-Performance Research*. The app lets readers explore how common
entrepreneurial orientation (EO) research designs can limit causal inference by
simulating survey and longitudinal/CATA workflows under different assumptions.

- Live web app: https://shiny.drdiscipulus.de/eo_causality_app/
- Paper: https://doi.org/10.1177/1042258720976368
- Open Science Framework repository: https://osf.io/gxhmj/?view_only=5acc084c3dd240d38c72562fb44f2806

This repository is shared as archival and educational companion code. It is
intended to show how the simulations and Shiny app were built for the
publication, rather than to serve as an actively developed software package.

## What The App Does

The app illustrates two research designs commonly used in EO-performance
research:

1. A survey-based design using psychometric EO indicators, selection effects,
   omitted-variable bias, and measurement error.
2. A longitudinal design using secondary/CATA indicators, panel data, omitted
   variables, random effects, fixed effects, and measurement error.

Both workflows compare a correct model with simpler misspecified models so that
readers can inspect how design choices affect estimated EO-performance
relationships.

## How The App Is Built

- `endo_app/app.R` is the entry point. It loads packages, sources helper files,
  defines the Shiny navbar, and starts the app.
- `endo_app/ui/` contains the About pages and tab layouts shown in the browser.
- `endo_app/server/` contains the simulation logic connected to the interactive
  controls.
- `endo_app/R/` contains helpers for simulation setup, model summaries, CATA
  output tables, and reusable UI components.
- `endo_app/www/` contains the app stylesheet and static image assets.

This is intentionally a plain Shiny app rather than an R package. The structure
keeps the publication companion easy to inspect and close to the deployed web
app.

## Local Setup

Install the required R packages before running the app:

```r
install.packages(c(
  "shiny", "shinycssloaders", "bslib", "tidyverse", "lavaan",
  "reactable", "plotly", "broom", "glue", "lavaanPlot",
  "DiagrammeR", "plm"
))
```

## Run Locally

From the repository root, start the app with:

```r
shiny::runApp("endo_app")
```

## Citation

If you use the app, source code, or simulation workflow in research or teaching,
please cite the article:

Anderson, B. S., Schüler, J., Baum, M., Wales, W. J., & Gupta, V. K. (2020).
The Chicken or the Egg? Causal Inference in Entrepreneurial
Orientation-Performance Research. *Entrepreneurship Theory and Practice*.
https://doi.org/10.1177/1042258720976368

## Author Pages

- [Brian S. Anderson](https://bloch.umkc.edu/faculty-directory-anderson-brian/)
- [Jens Schüler](https://www.eship.uni-bayreuth.de/de/team/schueler_jens/index.php)
- [Matthias Baum](https://www.eship.uni-bayreuth.de/de/team/baum_matthias/index.php)
- [William J. Wales](https://www.albany.edu/business/faculty/william-wales)
- [Vishal K. Gupta](https://culverhouse.ua.edu/news/directory/vishal-gupta/)

## License

This project is licensed under the MIT License. See `LICENSE`.

R Shiny app written by Jens Schüler.
