FROM rocker/shiny-verse:4.4.0

# common geospatial dependencies
# https://github.com/rocker-org/rocker-versioned2/blob/master/scripts/install_geospatial.sh
RUN /rocker_scripts/install_geospatial.sh

# install geodata from github because it has been removed (temporarily?) from CRAN
# "Archived on 2024-05-27"
# https://cran.r-project.org/web/packages/geodata/index.html
# RUN installGithub.r --deps TRUE \
#      richardli/SurveyPrev \
#      && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# install more dependencies not available on cran
RUN R -q -e "BiocManager::install(c('graph', 'Rgraphviz'))"

# standard INLA installation is timing out so use github installation https://stackoverflow.com/a/66626028
RUN R -q -e 'install.packages("INLA",repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)'
# RUN R -q -e 'remotes::install_github(repo = "https://github.com/hrue/r-inla", ref = "stable", subdir = "rinla", build = FALSE)'

# install first with dependencies (install again later without dependencies for speedy re-build with docker)
RUN installGithub.r --deps TRUE \
     wu-thomas/saeforhealth-WHO \
     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

ARG CACHEBUST=1
# comment out following line if you want to skip the 'saeforhealth-WHO' install
# https://stackoverflow.com/questions/35134713/disable-cache-for-specific-run-commands
RUN echo "$CACHEBUST"

# always install the latest github version of these packages (but don't update the dependencies)
RUN installGithub.r \
     richardli/SUMMER \
     richardli/SurveyPrev \
     wu-thomas/saeforhealth-WHO \
     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(saeforhealth);saeforhealth::run_app()"
