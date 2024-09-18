#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


options(shiny.maxRequestSize=150*1024^2) ## make the maximum size 150Mb for data input

app_server <- function(input, output, session) {

  shinyjs::useShinyjs()

  ### initialize R6 objects
  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()


  ### home page for the website
  CountryInfo$website_link('https://wu-thomas.github.io/surveyPrev_website/')


  ### WHO/non-WHO version related parameters

  # non-WHO version

  if(FALSE){
    CountryInfo$WHO_version(F)
    CountryInfo$use_basemap('OSM')
    CountryInfo$shapefile_source('GADM-preload') # CountryInfo$shapefile_source('GADM-download')
  }


  # WHO version

  if(TRUE){

    CountryInfo$WHO_version(T)
    CountryInfo$use_basemap('None')
    CountryInfo$shapefile_source('WHO-download')
    ### docker WHO : CountryInfo$shapefile_source('WHO-download')
  }


  ### other parameters for this version of the app
  CountryInfo$legend_color_reverse(T)


  CountryInfo$use_preloaded_Zambia(F)
  CountryInfo$use_preloaded_Madagascar(F)


  ### load modules
  mod_landing_page_server("landing_page_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_country_specify_server("country_specify_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_survey_dat_input_server("survey_dat_input_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_model_selection_server("model_selection_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_result_tabulate_server("result_tabulate_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_prev_map_server("res_visual_prev_map_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_multiple_maps_server("res_visual_multiple_maps_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_scatter_server("res_visual_scatter_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_ridge_server("res_visual_ridge_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_indicator_in_app_server("indicator_in_app_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_indicator_dictionary_server("indicator_dictionary_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_DHS_API_est_server("DHS_API_est_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)

}
