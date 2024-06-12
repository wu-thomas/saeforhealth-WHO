
###############################################################
### test Guatemala vaccine
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(F)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Guatemala'
  ex.svy.year <- '2015'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'CH_VACC_C_BAS'
  #file_path <-'E:/Downloads/GU_2014-15_DHS_05302024_2055_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source=NULL)
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                            recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  CountryInfo$svy_analysis_dat(analysis_dat)

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

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
                             method=tmp.method)
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

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



      ### store model results
      AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

      AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  tmp.natl <- prevMap.leaflet(res.obj = examine.res$Unit$'Admin-2',
                              gadm.shp = gadm.list.tmp[["Admin-2"]],
                              model.gadm.level = 2,
                              strata.gadm.level = 1,
                              value.to.plot ='exceed_prob',
                              legend.label = 'mean',
                              map.title='this is a long longlong title',
                              threshold.p = 0.5898549,
                              use.basemap='OSM')


  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$Direct$`Admin-2`,
                               res.obj.y = examine.res$Unit$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2



}






###############################################################
### test COD shapefile
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(T)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Democratic Republic of the Congo'
  ex.svy.year <- '2013'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'CM_ECMR_C_NNR'
  #file_path <-'C:/Users/wu-th/Downloads/CD_2013-14_DHS_06042024_60_143411.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source=NULL)
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                            recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())

  CountryInfo$svy_analysis_dat(analysis_dat)


  ### check data map
  tmp.res.obj <- ncluster.map.interactive(gadm.level='Admin-1',
                                          gadm.list=CountryInfo$GADM_list_smoothed(),
                                          cluster.geo=GPS.dat)
  tmp.res.obj$map



  ### check cluster admin-2 info

  check.cluster.info <- surveyPrev::clusterInfo(geo=GPS.dat,
                                          poly.adm1=CountryInfo$GADM_list()[[paste0('Admin-',1)]],
                                          poly.adm2=poly.adm2,
                                          by.adm1 = paste0("NAME_",1),
                                          by.adm2 = paste0("NAME_",2))


  poly.adm2 <- CountryInfo$GADM_list()[[paste0('Admin-',2)]]
  #overlap.adm2 <- poly.adm2[poly.adm2$ADM2_NAME %in% c('BOSOBOLO','BILI2'),]
  poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry <-
    sf::st_difference(poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry ,
  poly.adm2[poly.adm2$ADM2_NAME=='BILI2',]$geometry)


  plot(sf::st_geometry(overlap.adm2))
  plot(sf::st_geometry(overlap.adm2[1,]))
  plot(sf::st_geometry(overlap.adm2[2,]))

  overlap.adm2$geometry[2] <- st_difference(overlap.adm2$geometry[2],
                                            overlap.adm2$geometry[1])

  sf::st_is_valid(overlap.adm2)



  #poly.adm2 <- poly.adm2[340,]
  poly.adm2 <- poly.adm2[-340,]
  st_is_valid(poly.adm2)
  #plot(poly.adm2)
  admin2.sf <- sf::st_join(GPS.dat, poly.adm2) %>%
    sf::st_transform(sf::st_crs(poly.adm2))
  admin2.sf <- admin2.sf[!duplicated(admin2.sf[,c('DHSCLUST')]),]

  poly.adm1 <- CountryInfo$GADM_list()[[paste0('Admin-',1)]]

  admin1.sf <- st_join(GPS.dat, poly.adm1) %>%
    sf::st_transform(st_crs(poly.adm1))

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

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
                             method=tmp.method)
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

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



      ### store model results
      AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

      AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  tmp.natl <- prevMap.leaflet(res.obj = examine.res$Unit$'Admin-2',
                              gadm.shp = gadm.list.tmp[["Admin-2"]],
                              model.gadm.level = 2,
                              strata.gadm.level = 1,
                              value.to.plot ='exceed_prob',
                              legend.label = 'mean',
                              map.title='this is a long longlong title',
                              threshold.p = 0.5898549,
                              use.basemap='OSM')


  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$Direct$`Admin-2`,
                               res.obj.y = examine.res$Unit$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2



}






###############################################################
### test Benin shapefile
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(T)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Benin'
  ex.svy.year <- '2017'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'CH_VACC_C_BAS'
  #file_path <-'C:/Users/wu-th/Downloads/BJ_2017-18_DHS_06042024_2153_143411.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source=NULL)
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                            recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())

  CountryInfo$svy_analysis_dat(analysis_dat)




  ### check cluster admin-2 info

  check.cluster.info <- surveyPrev::clusterInfo(geo=GPS.dat,
                                                poly.adm1=CountryInfo$GADM_list()[[paste0('Admin-',1)]],
                                                poly.adm2=CountryInfo$GADM_list()[[paste0('Admin-',2)]],
                                                by.adm1 = paste0("NAME_",1),
                                                by.adm2 = paste0("NAME_",2))


  poly.adm2 <- CountryInfo$GADM_list()[[paste0('Admin-',2)]]

  st_is_valid(poly.adm2)
  #plot(poly.adm2)
  admin2.sf <- sf::st_join(GPS.dat, poly.adm2) %>%
    sf::st_transform(sf::st_crs(poly.adm2))
  admin2.sf <- admin2.sf[!duplicated(admin2.sf[,c('DHSCLUST')]),]

  poly.adm1 <- CountryInfo$GADM_list()[[paste0('Admin-',1)]]

  admin1.sf <- st_join(GPS.dat, poly.adm1) %>%
    sf::st_transform(st_crs(poly.adm1))

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

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
                             method=tmp.method)
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

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



      ### store model results
      AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

      AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  tmp.natl <- prevMap.leaflet(res.obj = examine.res$Unit$'Admin-2',
                              gadm.shp = gadm.list.tmp[["Admin-2"]],
                              model.gadm.level = 2,
                              strata.gadm.level = 1,
                              value.to.plot ='exceed_prob',
                              legend.label = 'mean',
                              map.title='this is a long longlong title',
                              threshold.p = 0.5898549,
                              use.basemap='OSM')


  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$Direct$`Admin-2`,
                               res.obj.y = examine.res$Unit$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2



}



