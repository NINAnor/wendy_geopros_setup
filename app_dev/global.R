library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(dplyr)
library(DT)
library(shinyjs)
library(leafem)
library(tibble)
library(leafpop)
library(mapview)
library(shinyRadioMatrix)
library(shinylogs)
library(leaflet.extras)
library(leaflet.extras2)
library(stringi)
library(shinyWidgets)
library(tidyverse)
library(bigrquery)
library(DBI)
library(shinyjs)
library(shinyBS)
library(giscoR)
library(googleCloudStorageR)
library(shinybusy)
library(terra)
library(bsicons)
library(bslib)
library(irr)


## change this to wendy
### BQ connection to store rectangles
env<-"prod"
project<-"eu-wendy"
var_lang<-"en"
bqprojID<-"wendy"
bq_auth(
  path = "bq_wendy.json"
)


#bucket
bucket_name<-paste0(bqprojID,"_geopros_",env)
gcs_auth("bq_wendy.json")
gcs_global_bucket(bucket_name)

source("mod_manage_study.R")
source("ahp_function.R")


dataset <- paste0("wendy_",env)
# dataset <- "admin_data"

con_admin<-data.frame(
  project = project,
  dataset = dataset,
  billing = project
)


con_admin <- dbConnect(
  bigrquery::bigquery(),
  project = con_admin$project,
  dataset = con_admin$dataset,
  billing = con_admin$billing
)

orange = "#ffa626"
blue = "#53adc9"
green = "#50b330"


## define on - offshore min-max area km2
on_min<-50
on_max<-5000
off_min<-500
off_max<-15000

cntr<-gisco_get_countries(year = "2020",
                          epsg = "4326",
                          cache = TRUE,
                          update_cache = FALSE,
                          cache_dir = NULL,
                          verbose = FALSE,
                          resolution = "60",
                          spatialtype = "RG",
                          country = NULL,
                          region = "Europe")
cntr<-cntr%>%filter(CNTR_ID != "RU")

coast<-gisco_get_coastallines()
# coast<-st_read("data/eez_v12_sel.gpkg")


map_cntr<- leaflet() %>%
  addProviderTiles(provider= "CartoDB.Positron")%>%
  addFeatures(st_sf(cntr), layerId = ~cntr$CNTR_ID)

map_coast<- leaflet(st_sf(coast)) %>%
  addPolygons(color = "blue", weight = 3, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3)%>%
  addProviderTiles(provider= "CartoDB.Positron")%>%
  addDrawToolbar(targetGroup='drawPoly',
                 polylineOptions = F,
                 polygonOptions = F,
                 circleOptions = F,
                 markerOptions = F,
                 circleMarkerOptions = F,
                 rectangleOptions = T,
                 singleFeature = FALSE,
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))



# es_descr<-tbl(con_admin, "es_descr")
# es_descr<-es_descr%>%collect()



