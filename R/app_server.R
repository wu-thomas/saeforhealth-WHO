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


  ### Set up parameters for this version of the app
  CountryInfo$WHO_version(T)
  CountryInfo$use_basemap('None') #CountryInfo$use_basemap('OSM')

  CountryInfo$shapefile_source('WHO-download')

  CountryInfo$legend_color_reverse(T)


  CountryInfo$use_preloaded_Zambia(F)
  CountryInfo$use_preloaded_Madagascar(F)


  ### load modules
  mod_country_specify_server("country_specify_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_survey_dat_input_server("survey_dat_input_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_model_selection_server("model_selection_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_result_tabulate_server("result_tabulate_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_prev_map_server("res_visual_prev_map_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_multiple_maps_server("res_visual_multiple_maps_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_scatter_server("res_visual_scatter_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_ridge_server("res_visual_ridge_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)


}
