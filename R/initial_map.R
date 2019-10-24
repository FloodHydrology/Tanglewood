#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Map
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 10/18/2019
#Purpose: Create Leaflet Map to assist with site selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Defin relevant working directories
data_dir<-"C://Tanglewood_GIS//spatial_data"

#Download packages 
library(htmlwidgets)
library(leaflet)
library(shiny)
library(sf)
library(raster)
library(tidyverse)


#Define master projection [note, we have to reproject to coordinates later]
p<-"+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Downlaod releavnt relavant layers
streams<-st_read(paste0(data_dir,"/streams.shp")) %>% st_transform(., crs=4326) %>% st_zm(.) 
field<-st_read(paste0(data_dir,"/House Field.shp")) %>% st_transform(., crs=4326) %>% st_zm(.)
property<-st_read(paste0(data_dir,"/Tanglewood.shp")) %>% st_transform(., crs=4326) %>% st_zm(.)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Map========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m<- leaflet(property) %>% 
  #Add Basemaps
  addProviderTiles("Esri.WorldImagery", group = "Ortho") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>% 
  addProviderTiles("OpenTopoMap", group = "DEM") %>% 
  addTiles(group = "Imagery","Topography") %>%
  #Add properties
  addPolylines(data=streams, weight =3, col="blue", fill=F, group = "streams") %>% 
  #Add properties
  addPolygons(data=property, weight =2, col="grey", fill=F, group = "property boundary") %>% 
  #Add subsheds
  addPolygons(data=field, weight =2, col="red", group = "field") %>% 
  #Add Layer Control
  addLayersControl(baseGroups = c("Ortho", "Topo"), 
                   overlayGroups = c("streams",
                                     "field",
                                     "property boundary"
                                     
                   ))

htmlwidgets::saveWidget(m, "initial_watershed_map.html")


