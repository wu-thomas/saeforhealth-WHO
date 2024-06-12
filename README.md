
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SurveyPrevRshinyWHO

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

You can then install the development version of SurveyPrevRshinyWHO
with:

``` r
devtools::install_github("wu-thomas/saeforhealth-WHO")
```

## Launch the app

You can launch the Shiny app with the following command:

``` r
library(saeforhealth)
saeforhealth::run_app()
```
