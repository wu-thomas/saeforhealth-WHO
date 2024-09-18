#' model_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'


mod_model_selection_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    tags$head(
      tags$script('Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {
                        Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                        });'
      ),
      tags$style(HTML("
        /* General Styles for the checkboxTable within the model-checkbox-table class */
        .shiny-input-container:not(.shiny-input-container-inline) {
         width: 700px;
         max-width: 100%;
        }

        .model-checkbox-table {
          width: 100%; /* Full width to contain the DataTable */
          max-width: 800px;
          margin: 0 auto; /* Center the table horizontally */
          float: left;
        }

        .model-checkbox-table .dataTable {
          font-size: 16px; /* Larger text for readability */
          width: 100% !important; /* Force the table to expand to the container width */
          table-layout: fixed; /* Equal column widths */
          border-collapse: collapse; /* For border styling */
        }

        /* Header and cells styling */
        .model-checkbox-table .dataTable th,
        .model-checkbox-table .dataTable td {
          border: 1px solid #ddd; /* Light grey border */
          text-align: center; /* Center alignment for text */
  max-width: 300px !important; /* Ensure cells are less than 300px in width */

        }

        /* Zebra striping for rows */
        .model-checkbox-table .dataTable tr:nth-child(even){background-color: #f2f2f2;}

        /* Column and row headers styling */
        .model-checkbox-table .dataTable thead th {
          background-color: #ADD8E6; /* Green background for column headers */
          color: white; /* White text for contrast */
        }

         .model-checkbox-table .dataTable tbody tr td:first-child,
        .model-checkbox-table .dataTable thead th:first-child {
          width: 20%; /* Increase the width of the row names */
        }

        .model-checkbox-table .dataTable td input[type='checkbox'],
        .model-checkbox-table .dataTable td input[type='radio'] {
          display: block;
          margin-top: 10px;
          padding-left:3px;
          display: flex !important; justify-content: center !important; align-items: center !important;
          /* Additional custom styles for checkboxes and radio buttons can go here */
        }
        .navbar { background-color: #ADD8E6; }
      .navbar .navbar-nav .nav-item .nav-link { color: #FFFFFF; }
        .pretty-button {
      background-color: #509cb5; /* Green */
      border: none;
      color: white;
      padding: 15px 32px;
      text-align: center;
      text-decoration: none;
      display: block;
      font-size: 20px;
      margin: 4px 2px;
      cursor: pointer;
      border-radius: 12px; /* Rounded corners */
        }
      .panel-title {
        font-size: 20px;
        background-color: #f7f7f7;
        border-bottom: 1px solid #e1e1e1;
        padding: 8px;
        border-radius: 5px;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      hr.gradient-hr {
        border: none; /* Removes the default border */
        height: 1px; /* Sets a specific thickness for the HR */
        background: linear-gradient(to right, rgba(0,0,0,0.1), rgba(0,0,0,0.55), rgba(0,0,0,0.1));
        margin: 20px 10px; /* Adds some vertical spacing around the HR */
      }


      "))
    ),
    div(class = "module-title",
    h4("Statistical Analysis")),
    fluidRow(
      column(10,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("model_text_display"))
             )
      ),
      column(2,
             div(style = "display: flex; flex-wrap: wrap;",
                 uiOutput(ns("checklist"))
             )
      )
    ),
    navbarPage(title = "",
               tabPanel("Model Implementation",
                        fluidRow(
                          div(style = "width: 100%; max-width: 800px;margin-top:10px;",
                              h4("Model Selection", class = "panel-title"),
                          column(12,
                                 div(DT::DTOutput(ns('checkboxTable')), class = "model-checkbox-table"),
                          )
                          )),

                        fluidRow(

                          div(style = "width: 100%; max-width: 800px;",
                              tags$hr(class="gradient-hr"),
                              h4("Model Screening", class = "panel-title"),
                          column(12,
                                 div(style = "display: flex; justify-content: center; padding: 20px 0;margin-top: -10px;",
                                     actionButton(ns("screen_check"), "Data Sparsity Check", class = "pretty-button"))
                          ),
                          column(12,
                                 div(DT::DTOutput(ns('screen_res')),class = "model-checkbox-table")
                          ),
                          column(12,
                                 div(style = " margin: auto;float: left;",
                                     uiOutput(ns("screen_text_display"))
                                 )
                          ),
                          column(12,
                                 tags$hr(class="gradient-hr"))
                          )),

                        shinyjs::hidden(
                          div(id = ns("after_check"),
                          fluidRow(div(style = "width: 100%; max-width: 800px;",
                                     h4("Model Fitting", class = "panel-title"))),
                        #fluidRow(div(style = "display: flex; justify-content: center;width: 100%; max-width: 800px;",
                         # column(6,
                         #        div(style = "display: flex; justify-content: center; padding: 20px 0;width: 100%; max-width: 400px;",
                         #            actionButton(ns("run_analysis_clear"), HTML("Run models that passed check"), class = "pretty-button"))
                         # ),
                         # column(6,
                         #        div(style = "display: flex; justify-content: center; padding: 20px 0;width: 100%; max-width: 400px;",
                         #            actionButton(ns("run_analysis_all"), "Run all selected models", class = "pretty-button"))
                         # ))
                         # ),
                        fluidRow(
                          div(style = "display: flex; justify-content: center; width: 100%; max-width: 800px;",
                              uiOutput(ns("dynamic_buttons"))  # This will receive dynamic content from server
                          )
                        ),
                        fluidRow(
                          #DT::DTOutput(ns('valuesTable')),
                          #DT::DTOutput(ns('Res_Tracker_Table')),
                          #div(DT::DTOutput(ns('Selected_Res_Tracker_Table')),class = "model-checkbox-table"),
                          column(12,
                                 div(DT::DTOutput(ns('Res_Status')),class = "model-checkbox-table",
                                     style = "margin-bottom:80px;")
                          )

                        )
                        ))

               ),
               navbarMenu(
                 title = "Model Details",

                 # First sub-tab for "Direct Estimates"
                 tabPanel(title = "Direct Estimates Method",
                div(
                            style = "font-size: 16px;max-width: 1200px;",

                          withMathJax(),
                          tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                'HTML-CSS': {
                      fonts: ['TeX'],
                      styles: {
                        scale: 110,
                        '.MathJax': { padding: '1em 0.1em', color: 'royalblue ! important' }
                      }
                    }
                });
                </script>
                ")), withMathJax(includeMarkdown(system.file("app", 'www', paste0("method_direct.rmd"),
                                           package = "saeforhealth")))
                         # withMathJax(includeMarkdown("inst/app/www/method_direct.rmd"))
                 )),

                 # Second sub-tab for "Area-level Model"
                 tabPanel(title = "Area-level Model Method",
                          div(
                            style = "font-size: 16px;max-width: 1200px;",
                            withMathJax(),
                          tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                'HTML-CSS': {
                      fonts: ['TeX'],
                      styles: {
                        scale: 110,
                        '.MathJax': { padding: '1em 0.1em', color: 'royalblue ! important' }
                      }
                    }
                });
                </script>
                ")),
                          withMathJax(includeMarkdown(system.file("app", 'www', paste0("method_FH.rmd"),
                                                 package = "saeforhealth")))
                          #withMathJax(includeMarkdown("inst/app/www/method_FH.rmd"))
                 )),

                 # Third sub-tab for "Method 3"
                 tabPanel(title = "Unit-level Model Method",
                          div(
                            style = "font-size: 16px;max-width: 1200px;",
                            withMathJax(),
                            tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                'HTML-CSS': {
                      fonts: ['TeX'],
                      styles: {
                        scale: 110,
                        '.MathJax': { padding: '1em 0.1em', color: 'royalblue ! important' }
                      }
                    }
                });
                </script>
                ")),withMathJax(includeMarkdown(system.file("app", 'www', paste0("method_unit.rmd"),
                                                                    package = "saeforhealth")))
                            #withMathJax(includeMarkdown("inst/app/www/method_unit.rmd"))
                          ))
               )
    )


    #div(DT::DTOutput(ns('checkboxTable')), class = "model-checkbox-table"),
    #DT::DTOutput(ns('valuesTable'))

    #tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
  )

}

#' model selection Server Functions
#' @description A fct function
#'
#' @noRd


mod_model_selection_server <-  function(id,CountryInfo,AnalysisInfo,parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    adm.05.strata.country <- c('DOM','COD','TZA','KEN')
    method_names <- c('Direct Estimates','Area-level Model','Unit-level Model')

    ###############################################################
    ### text instructions on model selection
    ###############################################################

    output$model_text_display <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description

      if(FALSE){
        # report odds ratio later when incorporating stratified model
      OR_vec <- get_natl_UR_OR(CountryInfo$svy_analysis_dat())

      hi_or_lo <- 'higher'
      if(OR_vec[1]<1){hi_or_lo ='lower'}

      #'The odds ratio for urban vs rural is ',
      #sprintf("%.2f(%.2f, %.2f)", OR_vec[1], OR_vec[2], OR_vec[3]),
      #', with urban having a ',hi_or_lo,' odds.',
      }


      HTML(paste0(
        "<p style='font-size: large;'>",
        "Selected Country: <span style='font-weight:bold;'>", country, "</span>.",
        " Survey Year: <span style='font-weight:bold;'>", svy_year, "</span>.",
        "<br>",
        "Indicator: <span style='font-weight:bold;'>", CountryInfo$svy_indicator_des(),"</span>.",
        #" at <span style='font-weight:bold;'>", concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), "</span> level(s).",
        "<br><span style='font-weight:bold;background-color:#F2DF8D'>",
        "Before starting the analysis, please check the app's national estimates for consistency with the DHS final report in the ",
        actionButton(
          ns("switch_verification"),
          "verification panel",
          style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-weight:bold;font-size: large;"
        ),
        ".</span><br>",
        "</p>",
        "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;'>",
        "Recommended Modelling Approaches: (Methodology under 'Model Details')",
        "<ul style='font-size: large;'>",
        "<li><strong>National Level:</strong> Use <span style='font-weight:bold;'>Survey-weighted Direct Estimates</span>.</li>",
        "<li><strong>Admin-1 Level:</strong> Apply <span style='font-weight:bold;'> Area-level (Fay-Herriot) models</span>.</li>",
        "<li><strong>Finer Levels:</strong> Implement <span style='font-weight:bold;'>Unit-level models</span>.</li>",
        "</ul>",
        "</div>",

        "<hr style='border-top-color: #E0E0E0;'>"
      ))



    })

    observeEvent(input$switch_verification, {

      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "DHS_API_est")
      shinyjs::js$activateTab("tool_kit")

    })

    ###############################################################
    ### reset analysis related data for new country/survey/indicator
    ###############################################################

    ### When changes in the following variables are detected,
    ### reset all analysis parameters
    ### including model selection, fitted models and results tracker

    ## setup indicator for changes
    meta_snapshot <- reactive({
      list(
        country_selected = CountryInfo$country(),
        year_selected = CountryInfo$svyYear_selected(),
        indicator_selected = CountryInfo$svy_indicator_var()
      )
    })


    # reset tracker matrix on all models when new country/indicator/survey is selected
    observeEvent(meta_snapshot(),{

      AnalysisInfo$model_res_list(NULL)
      AnalysisInfo$model_res_tracker_list(NULL)

      #AnalysisInfo$model_selection_mat(NULL)

      AnalysisInfo$cluster_admin_info_list(NULL)
      AnalysisInfo$model_screen_list(NULL)

    })





    ###############################################################
    ### model selection checkbox table
    ###############################################################

    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })



    ### detach checkboxes to table if the table is modified
    observeEvent(CountryInfo$GADM_analysis_levels(),{
      session$sendCustomMessage('unbinding_table_elements', ns('checkboxTable'))
    })

    observeEvent(meta_snapshot(),{
      session$sendCustomMessage('unbinding_table_elements', ns('checkboxTable'))
    })


    # Render the DataTable
    output$checkboxTable <- DT::renderDataTable({
      tmp.meta <- meta_snapshot()
      # Convert the reactive matrix to a regular matrix to create the dataframe
      df <- as.data.frame(matrix(vector('list', nrows * ncols()), nrow = nrows, dimnames = list(row_names, col_names())))

      # Populate the dataframe with checkbox inputs
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {


          df[i, j] <- as.character(shiny::checkboxInput(inputId = ns(paste0("cb_", i, "_", j)),
                                                        label = NULL))

        }
      }


      if( 'National' %in%  col_names()){
        df[2, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
        df[3, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
      }


      rownames(df) <- method_names
      # Return the DataTable
      DT::datatable(df, escape = FALSE, selection = 'none',
                    options = list(dom = 't', paging = FALSE, ordering = FALSE,
                                   #autoWidth = TRUE,
                                   #columnDefs = list(list(width = '150px', targets = "_all")),
                                   preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                   drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); }')))
    }, server = FALSE)


    ### make sure national is always selected
    observe({
      selected <- input$cb_1_1
      if (is.null(selected) || !selected) {
        #message('reselect national')
        updateCheckboxInput(session, "cb_1_1", value = T)
      }
    })

    ### track user's selection on models

    observe({

      matrix_status <- matrix(FALSE, nrow = nrows, ncol = ncols(), dimnames = list(row_names, col_names()))
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {
          inputId <- paste0("cb_", i, "_", j)
          matrix_status[i, j] <- input[[inputId]] %||% FALSE
        }
      }
      AnalysisInfo$model_selection_mat(matrix_status)

    })


    ###############################################################
    ### run initial screening
    ###############################################################

    ### Only when the screen_check button is hit, screening will be processed and results will be displayed

    observeEvent(input$screen_check, {

      ### pop-up window if no model is selected
      selected_matrix <- AnalysisInfo$model_selection_mat()

      if(sum(selected_matrix == T, na.rm = TRUE)==0){
        showNoModelModal()
        return()
      }

      ### pop-up window if data upload is incomplete
      if(is.null(CountryInfo$svy_analysis_dat())){
        showNoDataModal()
        return()
      }

      req(CountryInfo$svy_analysis_dat())


      ###############################################
      ### assigning cluster and admin information
      ###############################################

      if(FALSE){
      gadm.names <- names(CountryInfo$GADM_list())

      geo_info_list <- AnalysisInfo$cluster_admin_info_list()

      if(is.null(geo_info_list)){

        tryCatch({
          for(adm.level in gadm.names){
            message(adm.level)

            tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                       gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                       model.gadm.level = admin_to_num(adm.level),
                                                       strat.gadm.level = CountryInfo$GADM_strata_level())


            AnalysisInfo$set_info_list(adm.level,tmp.cluster.adm.info)

          }
        },error = function(e) {
          message(e$message)
        })
      }

      }


      ###############################################
      ### run checks through all admin x methods
      ###############################################

      col_names_tmp <- col_names()
      screen_check_list <- AnalysisInfo$model_screen_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()

      ### set alternative survey strata
      svy.strata = NULL
      country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName== CountryInfo$country(),'ISO3_CountryCode']
      if(country_iso3 %in% adm.05.strata.country){
        svy.strata = 'v024'
      }

      for (j in seq_len(ncols())) {


        tmp.adm <- col_names_tmp[j]
        tmp.adm.num <- admin_to_num(tmp.adm)

        session$sendCustomMessage('controlSpinner', list(action = "show",
        message = paste0("Running data sparsity check for ",tmp.adm," level model(s). Please wait...")))


        for (i in seq_len(nrows)) {

          if(selected_matrix[i,j]==T){
            #message(paste0(i),':',paste0(j))

            tmp.method <- row_names[i]
            tmp.method.display <- method_names[i]

            message('Checking at ',tmp.adm,' using ',tmp.method,' model.')

            tmp.check.model <- screen_check_list[[tmp.method]][[tmp.adm]]


            ### skip model if already tried
            if(!is.null(tmp.check.model$screen.flag)){
              next
            }


            ### prepare admin level GPS info if not stored
            geo_info_list <- AnalysisInfo$cluster_admin_info_list()
            tmp.geo.info <- geo_info_list[[tmp.adm]]

            if(is.null(tmp.geo.info)){

              tryCatch({

                message(tmp.adm)

                tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                           gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                           model.gadm.level = admin_to_num(tmp.adm),
                                                           strat.gadm.level = CountryInfo$GADM_strata_level())


                AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

                geo_info_list <- AnalysisInfo$cluster_admin_info_list()
                tmp.geo.info <- geo_info_list[[tmp.adm]]

              },error = function(e) {
                message(e$message)
              })
            }

            ### set model fitting status to Successful, assuming no error occurs
            tmp.check.model$screen.flag <- 'Error'
            tmp.check.model$screen.message <- 'Unable to process cluster and admin information.'

            ### process check results

            tryCatch(
              {
                #R.utils::withTimeout({
                tmp.check.model <- suppressWarnings(
                  screen_svy_model(cluster.admin.info=tmp.geo.info,
                                   analysis.dat= CountryInfo$svy_analysis_dat(),
                                   model.gadm.level= tmp.adm.num,
                                   strat.gadm.level = strat.gadm.level,
                                   method=tmp.method,
                                   svy.strata=svy.strata)
                  )
                #}, timeout = 300) ### 5 minutes for timeout
              },error = function(e) {
                tmp.check.model$screen.flag  <<- 'Error'
                tmp.check.model$screen.message <<- e$message
                message(e$message)

              }
            )


            if(tmp.check.model$screen.flag == 'Warning' & tmp.method=='FH'){
              tmp.check.model$screen.flag  <- 'Error'
            }


            message(tmp.check.model$screen.flag)

            ### store model results
            AnalysisInfo$set_screen_Check(tmp.method,tmp.adm,tmp.check.model)


          }

        }
        session$sendCustomMessage('controlSpinner', list(action = "hide"))

      }



    })




    ###############################################################
    ### UI updates for model fitting
    ###############################################################

    ### only show model fitting module once all selected models had gone through data sparsity check

    observe({

      tmp.ind.list <- AnalysisInfo$model_screen_ind_list()

      #message('number of unchecked: ',sum((1-tmp.ind.list),na.rm=T))
      #message('number of checked: ',sum((tmp.ind.list),na.rm=T))

      if(sum((1-tmp.ind.list),na.rm=T)==0 &sum((tmp.ind.list),na.rm=T)>0){
        shinyjs::show("after_check")
      }else{        shinyjs::hide("after_check")}


    })

    ### check whether this is model that is of concern (run with caution)

    warning_count_track <- reactiveVal(0)

    observe({

      screen_res_list <- AnalysisInfo$model_screen_list()
      selected_matrix <- AnalysisInfo$model_selection_mat()
      tmp.ind.list <- AnalysisInfo$model_screen_ind_list()

      if(is.null(selected_matrix)||is.null(screen_res_list)||is.null(tmp.ind.list)){
        return(NULL)
      }

      tmp.ind.list <- AnalysisInfo$model_screen_ind_list()

      if(!(sum((1-tmp.ind.list),na.rm=T)==0 &sum((tmp.ind.list),na.rm=T)>0)){
        return(NULL)
      }



      col_names_tmp <- col_names()
      warning_count <- 0

      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {

          if(selected_matrix[i,j]==T){

            tmp.method <- row_names[i]
            tmp.adm <- col_names_tmp[j]
            tmp.screen <- screen_res_list[[tmp.method]][[tmp.adm]]
            tmp.flag <- tmp.screen$screen.flag

            if(tmp.flag=='Warning'){
              warning_count <- warning_count+1
            }

          }
        }
      }
      warning_count_track(as.numeric(warning_count))
      message(paste0('number of models need to be run with caution: ',warning_count))

    })


    output$dynamic_buttons <- renderUI({

      req(warning_count_track())

      no_warning_model <- warning_count_track() == 0

      message(no_warning_model)

      if (!no_warning_model) {
        # Condition to display both buttons
        fluidRow(
          column(6,
                 div(style = "display: flex; justify-content: center; padding: 20px 0; width: 100%; max-width: 400px;",
                     actionButton(ns("run_analysis_clear"), HTML("Run models that passed check"), class = "pretty-button"))
          ),
          column(6,
                 div(style = "display: flex; justify-content: center; padding: 20px 0; width: 100%; max-width: 400px;",
                     actionButton(ns("run_analysis_all"), "Run all selected models", class = "pretty-button"))
          )
        )
      } else {
        # Condition to display only one button, centered
        fluidRow(
          column(12,
                 div(style = "display: flex; justify-content: center; padding: 20px 0; width: 100%; max-width: 800px;",
                     actionButton(ns("run_analysis_clear"), HTML("Run models that passed check"), class = "pretty-button"))
          )
        )
      }
    })



    ### detailed explanation about the data check
    observeEvent(input$triggerModal, {
      showModal(
        modalDialog(
          title = "Data Sparsity Check Explained",

          HTML(paste0(
            "<p style='font-size: medium; margin-bottom: 20px; line-height: 2;'>",
            "The data sparsity check assesses whether a model is appropriate and feasible for your selected method at given administrative level. Review the table above for results. Here are the possible indicators:",
            "</p>",
            "<ol style='font-size: medium; margin-top: 0; margin-bottom: 20px; line-height: 2;'>",
            "<li><span style='color:green;'>&#10004;</span>: Model passed the check and is ready for implementation.</li>",
            "<li><span style='color:orange;'>&#9888;</span>: Concerns about data sparsity exist. Model fitting is not recommended, but you may proceed (by clicking 'Run all selected models' in the model fitting section). Interpret results with caution.</li>",
            "<li><span style='color:red;'>&#10008;</span>: High risk of failure or biased results. Fitting this model is not permitted.</li>",
            "</ol>"
          )),

          footer = tagList(
            actionButton(ns("closeModal"), "Close")  # Button to close the modal
          )
        )
      )
    })

    # Observer to close the modal when "Close" button is clicked
    observeEvent(input$closeModal, {
      removeModal()
    })


    ### display the data sparsity check instruction text
    output$screen_text_display <- renderUI({

      #req(CountryInfo$country())
      ##req(CountryInfo$svy_indicator_var())
      #req(CountryInfo$svy_analysis_dat())

      HTML(paste0(
        "<p style='font-size: large;margin-top: 15px;'>",
        "Please make sure all selected models have gone through data sparsity check, before proceeding to model fitting. ",
        "Click ",
        actionButton(
          ns("triggerModal"),  # Button ID to trigger the modal
          "here",
          style = "border: none; background: none; color: blue; padding: 0; margin-top: -3px; font-size: large;"  # Larger font
        ),
        " for detailed explainations about this check."
      ))

    })



    ###############################################################
    ### run analysis based on model selection
    ###############################################################

    ### Only when the run_anlaysis button is hit, models will be fitted and results will be tracked.

    ### run only models passed check

    observeEvent(input$run_analysis_clear, {



      if(CountryInfo$use_preloaded_Madagascar()){

        AnalysisInfo$model_res_tracker_list(mdg.ex.res.tracker)

      }


      ### pop-up window if no model is selected
      selected_matrix <- AnalysisInfo$model_selection_mat()

      if(sum(selected_matrix == T, na.rm = TRUE)==0){
        showNoModelModal()
        return()
      }

      ### pop-up window if data upload is incomplete
      if(is.null(CountryInfo$svy_analysis_dat())){
        showNoDataModal()
        return()
      }

      req(CountryInfo$svy_analysis_dat())



      col_names_tmp <- col_names()
      res_tracker_list <- AnalysisInfo$model_res_tracker_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()

      ### set alternative survey strata
      svy.strata = NULL
      country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName== CountryInfo$country(),'ISO3_CountryCode']
      if(country_iso3 %in% adm.05.strata.country){
        svy.strata = 'v024'
      }

      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {

          if(selected_matrix[i,j]==T){
            #message(paste0(i),':',paste0(j))

            tmp.method <- row_names[i]
            tmp.method.display <- method_names[i]
            tmp.adm <- col_names_tmp[j]
            tmp.adm.num <- admin_to_num(tmp.adm)



            message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')

            session$sendCustomMessage('controlSpinner', list(action = "show",
                                                             message = paste0('Modelling at ',tmp.adm,' using ',tmp.method.display,' approach. This might take a few minutes. Please wait...')))


            tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]

            geo_info_list <- AnalysisInfo$cluster_admin_info_list()
            tmp.geo.info <- geo_info_list[[tmp.adm]]

            ### skip model if already tried
            if(!is.null(tmp.tracker.list$status)){

              #message('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')
              session$sendCustomMessage('controlSpinner', list(action = "show",
                message = paste0('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')))
              Sys.sleep(0.5)
              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
            }

            ### set model fitting status to Successful, assuming no error occurs
            tmp.tracker.list$status <- 'Successful'
            tmp.tracker.list$message <- 'Successful'


            screen_res_list <- AnalysisInfo$model_screen_list()

            tmp.screen <- screen_res_list[[tmp.method]][[tmp.adm]]

            tmp.flag <- tmp.screen$screen.flag
            tmp.message <- tmp.screen$screen.message

            if(tmp.flag=='Warning'){

              tmp.tracker.list$status <- 'Warning'
              tmp.tracker.list$message <- 'Model not fitted.'
              AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
            }

            if(tmp.flag=='Error'){

              tmp.tracker.list$status <- 'Unallowed'
              tmp.tracker.list$message <- 'Model will not be fitted.'
              AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
            }


            ### Run model
            tmp.res <- tryCatch(
              {
                #R.utils::withTimeout({
                tmp.res <- suppressWarnings(fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),
                                                          cluster.admin.info = tmp.geo.info,
                                                          gadm.list = CountryInfo$GADM_list(),
                                                          analysis.dat =   CountryInfo$svy_analysis_dat(),
                                                          model.gadm.level = tmp.adm.num,
                                                          strat.gadm.level = strat.gadm.level,
                                                          method = tmp.method,
                                                          aggregation =T,
                                                          svy.strata = svy.strata))
                #}, timeout = 300) ### 5 minutes for timeout
              },error = function(e) {
                tmp.tracker.list$status <<- 'Unsuccessful'

                if(inherits(e, "TimeoutException")) {
                  message("The operation timed out!")
                  tmp.tracker.list$message <<- 'Timed out. Took too long to fit the model.'

                } else {
                  tmp.tracker.list$message <<- e$message
                  message(e$message)
                }
                return(NULL)
              }
            )

            #if(!is.null(tmp.res$warning)){
             # tmp.tracker.list$status <- 'Warning'
            #  tmp.tracker.list$message <- tmp.res$warning}

            ### store model results
            AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

            AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)

            ### set national estimates
            if(tmp.method=='Direct'&&tmp.adm=='National'){
              AnalysisInfo$Natl_res(tmp.res$res.admin0)
              message(tmp.res$res.admin0$direct.est)
            }


            session$sendCustomMessage('controlSpinner', list(action = "hide"))

          }

        }
      }


    })


    ### run all models

    observeEvent(input$run_analysis_all, {



      if(CountryInfo$use_preloaded_Madagascar()){

        AnalysisInfo$model_res_tracker_list(mdg.ex.res.tracker)

      }


      ### pop-up window if no model is selected
      selected_matrix <- AnalysisInfo$model_selection_mat()

      if(sum(selected_matrix == T, na.rm = TRUE)==0){
        showNoModelModal()
        return()
      }

      ### pop-up window if data upload is incomplete
      if(is.null(CountryInfo$svy_analysis_dat())){
        showNoDataModal()
        return()
      }

      req(CountryInfo$svy_analysis_dat())



      col_names_tmp <- col_names()
      res_tracker_list <- AnalysisInfo$model_res_tracker_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()

      ### set alternative survey strata
      svy.strata = NULL
      country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName== CountryInfo$country(),'ISO3_CountryCode']
      if(country_iso3 %in% adm.05.strata.country){
        svy.strata = 'v024'
      }

      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {

          if(selected_matrix[i,j]==T){
            #message(paste0(i),':',paste0(j))

            tmp.method <- row_names[i]
            tmp.method.display <- method_names[i]
            tmp.adm <- col_names_tmp[j]
            tmp.adm.num <- admin_to_num(tmp.adm)



            message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')

            session$sendCustomMessage('controlSpinner', list(action = "show",
                                                             message = paste0('Modelling at ',tmp.adm,' using ',tmp.method.display,' approach. This might take a few minutes. Please wait...')))


            tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]

            geo_info_list <- AnalysisInfo$cluster_admin_info_list()
            tmp.geo.info <- geo_info_list[[tmp.adm]]

            ### skip model if already tried
            if(!is.null(tmp.tracker.list$status)){

              if(tmp.tracker.list$status=='Warning'&tmp.tracker.list$message=='Model not fitted.'){
              }else{
              #message('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')
              session$sendCustomMessage('controlSpinner', list(action = "show",
                                                               message = paste0('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')))
              Sys.sleep(0.5)
              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
              }
            }

            ### set model fitting status to Successful, assuming no error occurs
            tmp.tracker.list$status <- 'Successful'
            tmp.tracker.list$message <- 'Successful'


            screen_res_list <- AnalysisInfo$model_screen_list()

            tmp.screen <- screen_res_list[[tmp.method]][[tmp.adm]]

            tmp.flag <- tmp.screen$screen.flag
            tmp.message <- tmp.screen$screen.message


            if(tmp.flag=='Error'){

              tmp.tracker.list$status <- 'Unallowed'
              tmp.tracker.list$message <- 'Model will not be fitted.'
              AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
            }

            if(tmp.flag=='Warning'){

              tmp.tracker.list$status <- 'Warning'
              tmp.tracker.list$message <- 'Model fitted, but interpret with caution due to data sparsity.'

            }

            ### Run model
            tmp.res <- tryCatch(
              {
                #R.utils::withTimeout({
                tmp.res <- suppressWarnings(fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),
                                                          cluster.admin.info = tmp.geo.info,
                                                          gadm.list = CountryInfo$GADM_list(),
                                                          analysis.dat =   CountryInfo$svy_analysis_dat(),
                                                          model.gadm.level = tmp.adm.num,
                                                          strat.gadm.level = strat.gadm.level,
                                                          method = tmp.method,
                                                          aggregation =T,
                                                          svy.strata = svy.strata))
                #}, timeout = 300) ### 5 minutes for timeout
              },error = function(e) {
                tmp.tracker.list$status <<- 'Unsuccessful'

                if(inherits(e, "TimeoutException")) {
                  message("The operation timed out!")
                  tmp.tracker.list$message <<- 'Timed out. Took too long to fit the model.'

                } else {
                  tmp.tracker.list$message <<- e$message
                  message(e$message)
                }
                return(NULL)
              }
            )

            #if(!is.null(tmp.res$warning)){
            # tmp.tracker.list$status <- 'Warning'
            #  tmp.tracker.list$message <- tmp.res$warning}

            ### store model results
            AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

            AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)

            ### set national estimates
            if(tmp.method=='Direct'&&tmp.adm=='National'){
              AnalysisInfo$Natl_res(tmp.res$res.admin0)
              message(tmp.res$res.admin0$direct.est)
            }


            session$sendCustomMessage('controlSpinner', list(action = "hide"))

          }

        }
      }


    })







    ###############################################################
    ### reactive tables internally checking models
    ###############################################################

    if(FALSE){
    # Render a reactive table showing the current status of checkboxes
    output$valuesTable <- DT::renderDT({

      df <- as.data.frame(AnalysisInfo$model_selection_mat())

      DT::datatable(df, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })

    # Render a reactive table showing the status of fitted models
    output$Res_Tracker_Table <- DT::renderDT({

      df <- as.data.frame(AnalysisInfo$model_res_tracker_mat_old())
      rownames(df) <- method_names

      DT::datatable(df, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })

    }

    ###############################################################
    ### Render a reactive table showing the status of selected models
    ###############################################################

    if(FALSE){
    output$Selected_Res_Tracker_Table <- DT::renderDT({


      all_res_tracker <- AnalysisInfo$model_res_tracker_mat_old()
      model_selection_tracker <- AnalysisInfo$model_selection_mat()

      selected_res_tracker <- model_selection_tracker
      selected_res_tracker[,] <- NA  # Assign NA to all entries

      all_res_subset <- all_res_tracker[, colnames(model_selection_tracker), drop = FALSE]


      selected_res_tracker[all_res_subset == TRUE & model_selection_tracker == TRUE] <- TRUE
      selected_res_tracker[all_res_subset == FALSE & model_selection_tracker == TRUE] <- FALSE

      #message(selected_res_tracker)
      rownames(selected_res_tracker) <- method_names

      DT::datatable(selected_res_tracker, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })
    }


    ###############################################################
    ### Render a reactive table showing screening results
    ###############################################################

    output$screen_res <- DT::renderDT({


      screen_res_list <- AnalysisInfo$model_screen_list()
      model_selection_tracker <- AnalysisInfo$model_selection_mat()


      if(is.null(screen_res_list)|is.null(model_selection_tracker)){return()}
      ### initialize results storage
      screening_progress <- model_selection_tracker # whether a model has been checked
      screening_progress[,] <- T

      selected_res_tracker <- model_selection_tracker # check results
      rownames(selected_res_tracker) <- method_names

      selected_res_tracker[,] <- NA

      for (i in seq_len(dim(model_selection_tracker)[1])) {
        for (j in seq_len(dim(model_selection_tracker)[2])) {


          # do not display anything if not selected
          if(model_selection_tracker[i,j]==F){

            screening_progress[i,j] <- NA
            next

          }else{
            tmp.method <- rownames(model_selection_tracker)[i]
            tmp.adm <- colnames(model_selection_tracker)[j]


            tmp.screen <- screen_res_list[[tmp.method]][[tmp.adm]]

            tmp.flag <- tmp.screen$screen.flag
            tmp.message <- tmp.screen$screen.message

            #if(tmp.adm=='National'){tmp.status <- NULL}
            #message('Now at row ',tmp.method,' and column ',tmp.adm,' with model fitted',tmp.status, ' and message ',tmp.message)

            if(is.null(tmp.flag)){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML('<span style="color:orange;">&#9888; Model has not been checked. </span>'))
              screening_progress[i,j] <- F
              next
            }

            if(tmp.flag=='Clear'){
              selected_res_tracker[i,j] <- as.character(htmltools::HTML(paste0('<span style="color:green;">&#10004;',tmp.message, '</span>')))
              next
            }

            if(tmp.flag=='Error'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:red;">&#10008;',tmp.message, '</span>')))
              next
            }

            if(tmp.flag=='Warning'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:orange;">&#9888;',tmp.message, '</span>')))
              next
            }



          }


        }
      }

      AnalysisInfo$model_screen_ind_list(screening_progress)

      df <- DT::datatable(selected_res_tracker,
                          escape = FALSE, options = list(dom = 't',paging = FALSE, ordering = FALSE))
      return(df)

    })




    ###############################################################
    ### Render a reactive table showing the status of selected models
    ###############################################################

    output$Res_Status <- DT::renderDT({


      #res_status_list <- mdg.ex.res.tracker
      res_status_list <- AnalysisInfo$model_res_tracker_list()

      model_selection_tracker <- AnalysisInfo$model_selection_mat()

      selected_res_tracker <- model_selection_tracker
      rownames(selected_res_tracker) <- method_names

      selected_res_tracker[,] <- NA

      for (i in seq_len(dim(model_selection_tracker)[1])) {
        for (j in seq_len(dim(model_selection_tracker)[2])) {


          # do not display anything if not selected
          if(model_selection_tracker[i,j]==F){


            next

          }else{
            tmp.method <- rownames(model_selection_tracker)[i]
            tmp.adm <- colnames(model_selection_tracker)[j]

            tmp.status <- res_status_list[[tmp.method]][[tmp.adm]]$status
            tmp.message <- res_status_list[[tmp.method]][[tmp.adm]]$message

            #if(tmp.adm=='National'){tmp.status <- NULL}
            #message('Now at row ',tmp.method,' and column ',tmp.adm,' with model fitted',tmp.status, ' and message ',tmp.message)

            if(is.null(tmp.status)){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML('<span style="color:orange;">&#9888; Model not yet run. </span>'))
              next
            }

            if(tmp.status=='Successful'){
              selected_res_tracker[i,j] <- as.character(htmltools::HTML('<span style="color:green;">&#10004; Successful</span>'))
              next
            }

            if(tmp.status=='Unallowed'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:gray;">&#128711;',tmp.message, '</span>')))
              next
            }

            if(tmp.status=='Unsuccessful'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:red;">&#10008;', 'Unsuccessful: ',tmp.message, '</span>')))
              next
            }

            if(tmp.status=='Warning'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:orange;">&#9888;',tmp.message, '</span>')))
              next
            }



          }


        }
      }

      df <- DT::datatable(selected_res_tracker,
                          escape = FALSE, options = list(dom = 't',paging = FALSE, ordering = FALSE))
      return(df)

    })




  })
}

## To be copied in the UI
# mod_model_selection_ui("model_selection_1")

## To be copied in the server
# mod_model_selection_server("model_selection_1")

