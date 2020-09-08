#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Analysis
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 10/18/2019
#Purpose: Initial Watershed Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Defin relevant working directories
data_dir<-"C://Users//cnjones7//Box Sync//My Folders//Research Projects//Tanglewood//spatial_data//"
working_dir<-"C://Workspace//Scratch//"

#Download packages 
library(htmlwidgets)
library(leaflet)
library(stars)
library(sf)
library(raster)
library(whitebox)
library(tidyverse)

#Define master projection [note, we have to reproject to coordinates later]
p<-"+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Downlaod releavnt relavant layers
streams<-st_read(paste0(data_dir,"streams.shp")) %>% st_transform(.,p) #st_transform(., crs=4326) %>% st_zm(.)
streams_nhd<-st_read(paste0(data_dir, 'NHDPlus03W/NHDSnapshot/Hydrography/NHDFlowline.shp')) %>% st_transform(.,p) #st_transform(., crs=4326) %>% st_zm(.) 
field<-st_read(paste0(data_dir,"House Field.shp")) %>% st_transform(.,p) #%>% st_transform(., crs=4326) %>% st_zm(.)
property<-st_read(paste0(data_dir,"Tanglewood.shp")) %>% st_transform(.,p) #%>% st_transform(., crs=4326) %>% st_zm(.)

#dems
dem<-raster(paste0(data_dir,"elevation//ned10m32087g6.tif")) %>% projectRaster(., crs=p) %>% crop(st_buffer(streams, 1600))

#Crop NHD streams down to size
streams_nhd<-streams_nhd %>% st_zm(.) %>% st_crop(., st_bbox(st_buffer(streams, 1600)))
streams_nhd<-streams_nhd[property,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Delineate Watersheds-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create Flow Accumulation and Flow Direction Rsaters~~~~~~~~~~~~~~~~~~~~~~~~
#Write DEM to scratch workspace
writeRaster(dem, paste0(working_dir, "dem.tif"), overwrite=T)

#apply gaussian filter to smooth dem a bit
wbt_gaussian_filter(input = paste0(working_dir, "dem.tif"), 
                    output = paste0(working_dir, "dem_filter.tif"))

#fill single cell depressions
wbt_breach_single_cell_pits(dem = paste0(working_dir, "dem_filter.tif"), 
                          output = paste0(working_dir, "dem_fill.tif"))

#breach larger depressions
wbt_breach_depressions(dem = paste0(working_dir, "dem_fill.tif"), 
                       output = paste0(working_dir, "dem_breach.tif"))

#Create flow accumulation 
wbt_d8_flow_accumulation(input = paste0(working_dir, "dem_breach.tif"), 
                          output = paste0(working_dir, "fac.tif"))

#Create flow direction 
wbt_d8_pointer(dem = paste0(working_dir, "dem_breach.tif"), 
               output = paste0(working_dir, "fdr.tif"))

#2.2 Manually create Pout Points~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read fac into R environment
fac<-raster(paste0(working_dir, "fac.tif"))

#Plot everything
fac %>%  log10(.) %>%  plot(.)
#streams %>% st_geometry() %>% plot(., add=T, col="blue", lty=2, lwd=2)
field %>% st_geometry() %>% plot(., add=T, lwd=2)
property %>% st_geometry() %>% plot(., add=T, lwd=2)

#Mannual define watershed outlets
#pnt<-click(n=2)

#These are the pnts resulting from the manual additions!
pnt<-data.frame(matrix(c(438023.1, 3635449,
                         437900.9, 3635023, 
                         438246.5, 3635598), ncol=2, byrow=T)) 
colnames(pnt)<-c('x','y')
pnt<-pnt %>% as_tibble() %>% st_as_sf(., coords = c("x","y"), crs=st_crs(streams))
pnt %>% st_geometry() %>% plot(., add=T, col="red", pch=19)

#Split into 
pnt_paired<-pnt[1:2,]
pnt_creek<-pnt[3,]

#2.3 Delineate watershed~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Write pour points to workspace
st_write(pnt_paired, paste0(working_dir, "pp_paired.shp"), delete_layer =T)
st_write(pnt_creek, paste0(working_dir, "pp_creek.shp"), delete_layer =T)

#Delienate with WBT
wbt_watershed(d8_pntr = paste0(working_dir, "fdr.tif"), 
              pour_pts =  paste0(working_dir, "pp_paired.shp"), 
              output = paste0(working_dir, "ws_paired.tif"))
wbt_watershed(d8_pntr = paste0(working_dir, "fdr.tif"), 
              pour_pts =  paste0(working_dir, "pp_creek.shp"), 
              output = paste0(working_dir, "ws_creek.tif"))

#Bring shape into R environment
watershed<-raster(paste0(working_dir, "ws_paired.tif")) %>% st_as_stars() %>% st_as_sf(., merge = TRUE)
  watershed_a<-watershed[1,]
  watershed_b<-watershed[2,]
watershed_c<-raster(paste0(working_dir, "ws_creek.tif")) %>% st_as_stars() %>% st_as_sf(., merge = TRUE)

#Export to workspace
st_write(watershed_a, paste0(working_dir, "watershed_a.shp"))
st_write(watershed_b, paste0(working_dir, "watershed_b.shp"))
st_write(watershed_c, paste0(working_dir, "watershed_C.shp"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Create Leaflet Map---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Prep Data
streams_nhd %<>% st_transform(., crs=4326) %>% st_zm(.) 
streams     %<>% st_transform(., crs=4326) %>% st_zm(.) 
field       %<>% st_transform(., crs=4326) %>% st_zm(.) 
property    %<>% st_transform(., crs=4326) %>% st_zm(.) 
watershed_a %<>% st_transform(., crs=4326) %>% st_zm(.) 
watershed_b %<>% st_transform(., crs=4326) %>% st_zm(.) 
watershed_c %<>% st_transform(., crs=4326) %>% st_zm(.) 

m<-leaflet(property) %>% 
  #Add Basemaps
  addProviderTiles("Esri.WorldImagery", group = "Ortho") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>% 
  addProviderTiles("OpenTopoMap", group = "DEM") %>% 
  addTiles(group = "Imagery","Topography") %>%
  #Add properties
  addPolylines(data=streams_nhd, weight =3, col="blue", fill=F, group = "Mapped Streams (NHD)") %>% 
  #Add properties
  addPolygons(data=property, weight =2, col="grey", fill=F, group = "Biological Station Boundary") %>% 
  #Add properties
  addPolygons(data=watershed_c, weight =3, col="Brown", fill=F, group = "Watershed C") %>% 
  #Add properties
  addPolygons(data=watershed_a, weight =2, col="Red", fill=F, group = "Watershed A") %>% 
  #Add properties
  addPolygons(data=watershed_b, weight =2, col="Orange", fill=F, group = "Watershed B") %>% 
  #Add subsheds
  addPolygons(data=field, weight =2, col="red", group = "Experimental Field") %>% 
  #Add Layer Control
  addLayersControl(baseGroups = c("Ortho", "Topo"), 
                   overlayGroups = c("Mapped Streams (NHD)",
                                     "Watershed A",
                                     "Watershed B",
                                     "Watershed C",
                                     "Experimental Field",
                                     "Biological Station Boundary"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Export Products-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export headwater watersheds
watershed %>% st_transform(., p) %>% st_write(., paste0(data_dir,"paired_watershed.shp"))


