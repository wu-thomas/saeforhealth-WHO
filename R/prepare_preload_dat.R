###############################################################
### load DHS meta data
###############################################################

DHS_api_timeout = F

if(FALSE){
tryCatch({
    R.utils::withTimeout({

      DHS.country.meta <- rdhs::dhs_countries()
      DHS.survey.meta <- rdhs::dhs_surveys()
      DHS.dataset.meta <- rdhs::dhs_datasets()
      #Sys.sleep(20)  # Simulating a delay

      message('DHS API is working fine. Use most up-to-date info.')
    },
    timeout = 3)  # Timeout in seconds
    },
    TimeoutException = function(ex) {

      message("DHS API call timed out, using backup data.")
      DHS_api_timeout = T

      ### use backup
      pkg_files <- list.files('data')
      filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

      load(file=paste0('data/',filtered_files))


      DHS.country.meta <- DHS.country.meta.preload
      DHS.survey.meta <- DHS.survey.meta.preload
      DHS.dataset.meta <- DHS.dataset.meta.preload

    },
    error = function(e) {

      message("Error loading 'rdhs' library: ", e$message)
      DHS_api_timeout = T

      ### use backup
      pkg_files <- list.files('data')
      filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

      load(file=paste0('data/',filtered_files))

      DHS.country.meta <- DHS.country.meta.preload
      DHS.survey.meta <- DHS.survey.meta.preload
      DHS.dataset.meta <- DHS.dataset.meta.preload

})
}
### use backup
pkg_files <- list.files('data')
filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

load(file=paste0('data/',filtered_files))

DHS.country.meta <- DHS.country.meta.preload
DHS.country.meta[DHS.country.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
DHS.country.meta[DHS.country.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'


DHS.survey.meta <- DHS.survey.meta.preload
DHS.survey.meta[DHS.survey.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
DHS.survey.meta[DHS.survey.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'

DHS.dataset.meta <- DHS.dataset.meta.preload
DHS.dataset.meta[DHS.dataset.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
DHS.dataset.meta[DHS.dataset.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'

#message(list.files('data'))
#message(list.files('data/GADM_shp'))
#load('data/GADM_shp/zmb_example_GADM.rda')


###############################################################
### prepare preloaded DHS meta data
###############################################################

### only executed when preparing data

if(FALSE){

  ### load dhs meta data
  DHS.country.listing <- rdhs::dhs_countries()
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.dataset.listing <- rdhs::dhs_datasets()

  ##################################################
  ### Determine the surveys eligible for analysis
  ##################################################
  ### criteria: conducted later than 2000; is DHS standard survey (not MIS etc.); contains GPS data

  ### subset to >2000 and standard DHS
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.survey.listing <- DHS.survey.listing[as.numeric(DHS.survey.listing$SurveyYear)>2000 & DHS.survey.listing$SurveyType== 'DHS',]

  ### check GPS data set availability
  check_GPS_avail <- function(surveyID){
    svy.tmp.list <- DHS.dataset.listing[DHS.dataset.listing$SurveyId==surveyID,]
    return( ('Geographic Data' %in% unique(svy.tmp.list$FileType)))
  }

  ### subset to surveys with GPS
  DHS.survey.listing$GPS_avail <-  sapply(DHS.survey.listing$SurveyId,check_GPS_avail)
  DHS.survey.listing <- DHS.survey.listing[DHS.survey.listing$GPS_avail==T,]

  DHS.survey.meta.preload <- DHS.survey.listing

  ##################################################
  ### Determine data sets according to surveys
  ##################################################

  DHS.dataset.meta.preload <- DHS.dataset.listing[DHS.dataset.listing$SurveyId %in%
                                                    unique(DHS.survey.meta.preload$SurveyId),]
  DHS.country.meta.preload <- DHS.country.listing[DHS.country.listing$CountryName %in%
                                                    unique(DHS.survey.meta.preload$CountryName),]

  #DHS.country.meta.preload <- rdhs::dhs_countries()
  #DHS.survey.meta.preload <- rdhs::dhs_surveys()
  #DHS.dataset.meta.preload <- rdhs::dhs_datasets()



  formatted_date <- format(Sys.Date(), "%m%d%Y")

  setwd('data/')
  save(DHS.country.meta.preload,DHS.survey.meta.preload,DHS.dataset.meta.preload,file=paste0('DHS_meta_preload_',formatted_date,'.rda'))

}


###############################################################
### prepare preloaded GADM shapefile
###############################################################


if(FALSE){

  all_country_names <- sort(DHS.country.meta[['CountryName']])

  for(country in all_country_names){
    #country = 'Ethiopia'
   #country='Armenia'
    message(paste0('working on ',country))

    country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']

    #country_iso3='ARM'
    dir.create(file.path('data/GADM_shp', country_iso3), showWarnings = FALSE)

    if(length(list.files(paste0('data/GADM_shp/', country_iso3)))==2){
      message(paste0(country,' already downloaded, skip.'))
      next}

    ### store fine level GADM for analysis (neighborhood structure etc.)
    one_country_GADM <- get_country_GADM(country,resolution=1)

    adm_region_nums <- check_gadm_levels(one_country_GADM)

    ### only keep admin levels that have less than 1000 regions
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.fine <- one_country_GADM[model_levels]

    saveRDS(country.GADM.list.fine,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))

    ### store smoothed GADM for display
    one_country_GADM <- get_country_GADM(country,resolution=2)

    adm_region_nums <- check_gadm_levels(one_country_GADM)
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.smoothed <- one_country_GADM[model_levels]

    saveRDS(country.GADM.list.smoothed,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
  }

}





###############################################################
### process WHO shapefile
###############################################################

### WHO requested countries
WHO.app.countries <- c('Benin', 'Burkina Faso',
                       "Democratic Republic of the Congo",
                       'Rwanda', 'Senegal', 'Sierra Leone',
                       'United Republic of Tanzania','Zambia')

WHO.app.countries.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName %in% WHO.app.countries,]$ISO3_CountryCode


### save linkage file for WHO boundaries across admin levels
if(FALSE){
  #adm2.link.all <- read.csv("/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]
  save(adm2.link.all,file='WHO_shp_linkage.rda')

}


### function to read WHO shapefiles and constrain to WHO countries
read_WHO_shp <- function(file_path,adm_level=0){

  temp <- tempfile()
  unzip(file_path, exdir = temp)

  file_name <- paste0('GLOBAL_ADM',adm_level,'.shp')
  allPaths <- list.files(temp, #pattern =  '(GLOBAL_ADM0.shp|GLOBAL_ADM1.shp|GLOBAL_ADM2.shp)',
                         pattern = file_name,
                         full.names = TRUE, recursive = T, include.dirs = TRUE,
                         ignore.case = TRUE)

  if (length(allPaths) > 0) {
    shp_Paths <- allPaths[which.min(nchar(allPaths))]
  } else {
    shp_Paths <- NULL  # If no matching directories are found
    return(NULL)
  }

  WHO.shp = NULL
  tryCatch({
    WHO.shp <- suppressWarnings(sf::st_read(shp_Paths))
    WHO.shp <- WHO.shp[WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

    WHO.shp <- sf::st_as_sf(WHO.shp)
    WHO.shp <- st_transform(WHO.shp,4326)
  },error = function(e) {
    message(e$message)
    return(NULL)
  })



  return(WHO.shp)

}

### save national (only national boudaries can be stored within this version of the app)

if(FALSE){
  natl.WHO.shp <- read_WHO_shp(adm_level=0,
    file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM0_565521753006392799.zip')

  #save(natl.WHO.shp,file='natl_WHO_shp.rda')

  adm1.WHO.shp <- read_WHO_shp(adm_level=1,
    file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM1_-6406508520251409053.zip')

  #save(adm1.WHO.shp,file='adm1_WHO_shp.rda')

  adm2.WHO.shp <- read_WHO_shp(adm_level=2,
    file_path = 'C:/Users/wu-th/Downloads/Detailed_Boundary_ADM2_1272841991903683246.zip')
  #save(adm2.WHO.shp,file='adm2_WHO_shp.rda')


}


### Function to produce country specific shapefile
prepare_WHO_country_shp <- function(country.ISO3,
                                    natl.WHO.shp,
                                    adm1.WHO.shp,
                                    adm2.WHO.shp){

  return.shp.list <- list()

  ### initialize
  #country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
  country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]


  ### national shapefile
  natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
  natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
  country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(natl.WHO.shp$GUID) %in% natl.GUID,]

  if(dim(country.natl.shp)[1]>0){

    adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
    country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]

    return.shp.list[['National']]<-country.natl.shp

  }else{return(NULL)}



  ### Admin-1 shapefile
  adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
  adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)

  country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]

  adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
  adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)

  adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)


  if(dim(country.adm1.shp)[1]>0){

    country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
    country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]

    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm1.shp))){

      wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
      country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
      country.adm1.shp <- sf::st_as_sf(country.adm1.shp)
      country.adm1.shp <- st_transform(country.adm1.shp,4326)

      message('Fixed invalid shapefile.')

    }

    return.shp.list[['Admin-1']]<-country.adm1.shp

  }else{return(return.shp.list)}


  ### Admin-2 shapefile
  adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
  adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)

  country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]

  adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                    'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])

  adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)

  adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)


  if(dim(country.adm2.shp)[1]>0){

    country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]

    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm2.shp))){

      wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
      country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
      country.adm2.shp <- sf::st_as_sf(country.adm2.shp)
      country.adm2.shp <- st_transform(country.adm2.shp,4326)

      message('Fixed invalid shapefile.')

    }

    ### fix wrong shapefile for congo, overlapping
    if(country.ISO3 =='COD'){

      country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
        sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                          country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)

      message('Fixed overlapping shapefile.')

    }

    return.shp.list[['Admin-2']]<-country.adm2.shp


  }else{return(return.shp.list)}




  ### return shapefile list
  return(return.shp.list)

}


if(FALSE){
  tmp.cod.shp.list <- prepare_WHO_country_shp(country.fullname='Democratic Republic of the Congo',
                                              natl.WHO.shp=natl.WHO.shp,
                                              adm1.WHO.shp=adm1.WHO.shp,
                                              adm2.WHO.shp=adm2.WHO.shp)

}



###

# download link: https://gis-who.hub.arcgis.com/pages/detailedboundary





###############################################################
### prepare the WHO shapefiles for online version
###############################################################

if(FALSE){


  ### load linkage meta data
  #adm2.link.all <- read.csv("C:/Users/wu-th/Dropbox/YunhanJon/saeforhealth/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]


  #######################
  ### process national
  #######################

  #natl.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM0/GLOBAL_ADM0.shp'
  natl.WHO.shp <- suppressWarnings(sf::st_read(natl.WHO.shp.path))
  natl.WHO.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  natl.WHO.shp <- sf::st_as_sf(natl.WHO.shp)
  natl.WHO.shp <- st_transform(natl.WHO.shp,4326)

  #######################
  ### process Admin-1
  #######################

  #adm1.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM1/GLOBAL_ADM1.shp'
  adm1.WHO.shp <- suppressWarnings(sf::st_read(adm1.WHO.shp.path))
  adm1.WHO.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  adm1.WHO.shp <- sf::st_as_sf(adm1.WHO.shp)
  adm1.WHO.shp <- st_transform(adm1.WHO.shp,4326)

  #######################
  ### process Admin-2
  #######################

  #adm2.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM2/GLOBAL_ADM2.shp'
  adm2.WHO.shp <- suppressWarnings(sf::st_read(adm2.WHO.shp.path))
  adm2.WHO.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  adm2.WHO.shp <- sf::st_as_sf(adm2.WHO.shp)
  adm2.WHO.shp <- st_transform(adm2.WHO.shp,4326)

  #tmp.adm2.ctry <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE=='BFA',]
  #tmp.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE=='BFA',]


  country.fullname <- 'Burkina Faso'

  prepare_WHO_country_shp <- function(country.fullname){

    return.shp.list <- list()

    ### initialize
    country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
    country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]


    ### national shapefile
    natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
    natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
    country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(natl.WHO.shp$GUID) %in% natl.GUID,]

    if(dim(country.natl.shp)[1]>0){

      adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
      country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]

      return.shp.list[['National']]<-country.natl.shp

    }else{return(NULL)}



    ### Admin-1 shapefile
    adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
    adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)

    country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]

    adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
    adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)

    adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)


    if(dim(country.adm1.shp)[1]>0){

      country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
      country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]

      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm1.shp))){

        wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
        country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
        country.adm1.shp <- sf::st_as_sf(tmp.adm)
        country.adm1.shp <- st_transform(country.adm1.shp,4326)

        message('Fixed invalid shapefile.')

      }

      return.shp.list[['Admin-1']]<-country.adm1.shp

    }else{return(return.shp.list)}


    ### Admin-2 shapefile
    adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
    adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)

    country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]

    adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                      'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])

    adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)

    adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)


    if(dim(country.adm2.shp)[1]>0){

      country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]

      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm2.shp))){

        wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
        country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
        country.adm2.shp <- sf::st_as_sf(tmp.adm)
        country.adm2.shp <- st_transform(country.adm2.shp,4326)

        message('Fixed invalid shapefile.')

      }

      ### fix wrong shapefile for congo, overlapping
      if(country.ISO3 =='COD'){

      country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
        sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                          country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)
      }

      return.shp.list[['Admin-2']]<-country.adm2.shp


    }else{return(return.shp.list)}




    ### return shapefile list
    return(return.shp.list)

  }

  tmp.rwa.shp.list <- prepare_WHO_country_shp('Rwanda')

  ##################################
  ### save WHO country shapefiles
  ##################################


  for(WHO.country in WHO.app.countries){

    message(WHO.country)

    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)

    ### make directory
    dir.create(file.path('data/WHO_shp', WHO.country.ISO3), showWarnings = FALSE)

    if(length(list.files(paste0('data/WHO_shp/', WHO.country.ISO3)))>0){
      message(paste0(country,' already downloaded, skip.'))
      next
    }

    ### prepare shapefile
    country.shp.list <- prepare_WHO_country_shp(country.fullname=WHO.country)


    ### save shapefile
    saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))



  }


  ### make valid
  for(WHO.country in WHO.app.countries){

    # WHO.country = 'Democratic Republic of the Congo'
    message(WHO.country)

    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)

    country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                           '/',WHO.country.ISO3,'_shp.rds'))

    for (tmp.name in names(country.shp.list)){

      tmp.adm <- country.shp.list[[tmp.name]]

      if(!all(sf::st_is_valid(tmp.adm))){

        tmp.adm <- sf::st_make_valid(tmp.adm)
        tmp.adm <- sf::st_as_sf(tmp.adm)
        tmp.adm <- st_transform(tmp.adm,4326)

        country.shp.list[[tmp.name]] <- tmp.adm

        message('Country: ',WHO.country, ' ',tmp.name,' shapefile not valid.')

        saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))

        message('Fixed.')

      }

    }

  }


  ### fix wrong shapefile for congo
  WHO.country <- 'Democratic Republic of the Congo'
  WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
  message(WHO.country.ISO3)

  country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                         '/',WHO.country.ISO3,'_shp.rds'))

  poly.adm2 <- country.shp.list[['Admin-2']]

  poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry <-
    sf::st_difference(poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry ,
                      poly.adm2[poly.adm2$ADM2_NAME=='BILI2',]$geometry)


  country.shp.list[['Admin-2']] <- poly.adm2
  saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))


}


