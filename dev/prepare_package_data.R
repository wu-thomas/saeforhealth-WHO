# Prepare internal data for the RShiny Application.
#
# 1. List of country names and survey years for the drop down manual
#
##############################################################################
#########   load packages
##############################################################################


dhs_countries <- rdhs::dhs_countries()
dhs_surveys <- rdhs::dhs_surveys()




##############################################################################
#########   configuration for rdhs
##############################################################################

rdhs::set_rdhs_config(email = "wu-thomas@outlook.com",
                project = "Spatial analysis of childhood mortality and fertility rate in Africa")


##############################################################################
#########   setup parameters
##############################################################################

indicator <- "ancvisit4+"
year <- 2021
country <- "Madagascar"


##############################################################################
#########   set up data directories
##############################################################################

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-4)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)



##############################################################################
#########   data preparation
##############################################################################


### load IR recode

mdg.dat <- getDHSdata(country = country, indicator = NULL, year = year,Recode= 'Individual Recode')

### define contraceptive_use

contraceptive_use <- function(RData) {
  IRdata <- RData %>%
    mutate(contraceptive_use = case_when(v361 == 1 ~ 1, v361 %in% c(2,3,4) ~ 0, TRUE~ NA)) %>%
    set_value_labels(contraceptive_use = c(Yes = 1, No = 0)) %>%
    set_variable_labels(contraceptive_use = "Currently using contraceptive methods")
  colnames(IRdata)[colnames(IRdata) == "contraceptive_use"] <- "value"
  return(IRdata)
}

### get indicator data

mdg.dat2 <- getDHSindicator(mdg.dat, indicator = NULL, FUN = contraceptive_use)

### get DHS geo

geo <- getDHSgeo(country = country, year = year)


### GADM shapefile ### need some customization

setwd(data.dir)

#poly.adm1 <- st_read(dsn = "../gadm41_ZMB_shp", layer = "gadm41_ZMB_1", options = "ENCODING=UTF-8")
#poly.adm2 <- st_read(dsn = "../gadm41_ZMB_shp", layer = "gadm41_ZMB_1", options = "ENCODING=UTF-8")

gadm.abbrev = 'MDG'

poly.path <- paste0("shapeFiles_gadm") # specify the folder of the country shape files
poly.layer.adm1 <- paste('gadm41', gadm.abbrev,
                         '1', sep = "_") # specify the name of the admin1 shape file
poly.layer.adm2 <- paste('gadm41', gadm.abbrev,
                         '2', sep = "_") # specify the name of the admin2 shape file

poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions


cluster.info <- clusterInfo(geo = geo, poly.adm1 = poly.adm1, poly.adm2 = poly.adm2)
head(cluster.info$cluster.info)

head(cluster.info$wrong.points)

admin.info1 <- adminInfo(geo = poly.adm1, admin = 1)
admin.info2 <- adminInfo(geo = poly.adm2, admin = 2)

head(admin.info2$admin.info)






##############################################################################
#########   Direct estimates
##############################################################################

# lonely PSU?
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

res_ad1 <- directEST(data = mdg.dat2, cluster.info = cluster.info, admin = 1, aggregation = FALSE)

res_ad2 <- directEST(data = mdg.dat2, cluster.info = cluster.info, admin = 2, aggregation = FALSE)

# document needs update?
est1 <- res_ad1$res.admin1[, c("admin1.name", "direct.est")]
est2 <- res_ad2$res.admin2[, c("admin2.name", "direct.est")]
g1 <- mapPlot(data = est1, geo = poly.adm1, by.data = "admin1.name", by.geo = "NAME_1",
              variable = "direct.est", legend.label = "Admin1 Direct", removetab = TRUE, ylim = range(est2$direct.est))
g2 <- mapPlot(data = est2, geo = poly.adm2, by.data = "admin2.name", by.geo = "NAME_2",
              variable = "direct.est", legend.label = "Admin2 Direct", removetab = TRUE, ylim = range(est2$direct.est))

# g1 + g2


##############################################################################
#########   Fay-Herriot Estimates
##############################################################################


smth_res_ad1_bym2 <- fhModel(mdg.dat2, cluster.info = cluster.info, admin.info = admin.info1,
                             admin = 1, model = "bym2", aggregation = F)


cl_res_ad2 <- clusterModel(data = mdg.dat2, cluster.info = cluster.info, admin.info = admin.info2,
                           model = "bym2", stratification = F, admin = 2, aggregation = F, CI = 0.95)


est1 <- res_ad2$res.admin2[, c("admin2.name", "direct.est", "direct.se")]
est1$model <- "Direct Estimates"
est4 <- cl_res_ad2$res.admin2[, c("admin2.name", "mean", "sd")]
est4$model <- "Unit-level model"

colnames(est1) <- c("admin2.name", "mean", "sd",'model')

est <- rbind(est1, est4)

mapPlot(data = est, geo = poly.adm2, by.data = "admin2.name", by.geo = "NAME_2",
        variable = "model", value = "mean", is.long = TRUE, legend.label = "Estimates")
mapPlot(data = est, geo = poly.adm2, by.data = "admin2.name", by.geo = "NAME_2",
        variable = "model", value = "sd", is.long = TRUE, legend.label = "SE")

