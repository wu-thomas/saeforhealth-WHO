
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RShiny-app saeforhealth (WHO)

<!-- badges: start -->
<!-- badges: end -->

This RShiny app facilitates the mapping of health indicators prevalence
using data from Demographic and Health Surveys (DHS). Powered by the R
package SurveyPrev, our Shiny app revolutionizes the acquisition of
prevalence estimates through Small Area Estimation (SAE) techniques. It
is tailored for users of all backgrounds, enabling the performance of
complex statistical analyses without prior knowledge spatial data
management or modelling.

The application streamlines the analytical process into clear,
manageable steps. It guides users through model fitting and enhances
their experience with interactive visualization tools. Users can
dynamically explore the spatial distribution of health and demographic
indicators and directly interpret statistical outputs within the app’s
interface. Additionally, the application supports result exports,
facilitating detailed examinations and sharing in both spreadsheet and
graphical formats.

The Github repo maintains the source code for this RShiny application
and it is structured as an R package. There are three primary venues to
utilize this application:

1.  Web-Based Version:

- Location: Deployed on the online server.
- Requirements: Internet access and a web browser.
- Usage: Ideal for users who prefer quick access without local setup.

2.  Local R Package Installation:

- Requirements: R environment and dependent packages.
- Usage: Suitable for users who wish to run the application directly
  within their local R environment. This method allows for enhanced
  customization.

3.  Docker Deployment:

- Requirements: Docker Desktop installation.
- Usage: Perfect for users who do not have R installed. This method
  encapsulates the application within a Docker image, simplifying setup
  and ensuring compatibility.

Further details on the usage of this application via the above methods,
including step-by-step guide for installation, are available in the
subsequent sections.

## Installation

Some non-CRAN dependencies can be installed using the following command.
We strongly recommend installing the most recent version of SUMMER and
surveyPrev package from Github.

``` r
# install.packages("devtools")
devtools::install_github("rspatial/geodata")
devtools::install_github("richardli/SUMMER")
devtools::install_github("richardli/surveyPrev")
devtools::install_github("statnmap/HatchedPolygons")
install.packages("INLA",repos=c(getOption("repos"),
                        INLA="https://inla.r-inla-download.org/R/testing"),dep=TRUE)
```

You can then install the development version of saeforhealth with:

``` r
devtools::install_github("wu-thomas/saeforhealth-WHO")
```

## Launch the app

You can launch the Shiny app with the following command:

``` r
library(saeforhealth)
saeforhealth::run_app()
```
