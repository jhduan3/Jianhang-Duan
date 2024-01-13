

setwd("/Users/jackduan/Desktop/ESDA/BENG0093_Spatial/2nd_Assignment/data_codes")
library(terra)
library(sf)
library(tmap)
library(dplyr)
library(raster)
library(sp)
library(tmap)
library(spatstat)
library(ggplot2)
library(reshape)
library(plotly)
library(gstat)

# Indonesia 
indonesia <-st_read("idn_admbnda_adm1_bps_20200401.shp")
indonesia <- st_transform(indonesia, 4326) 
raster_template = rast( resolution = 0.05,
xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
# Protected area
pro_area<-read_sf("WDPA_WDOECM_Nov2023_Public_IDN_shp-polygons.shp")

# Buffered Area
pro_area_bf<-st_buffer(pro_area,d=1500)

# We can raster size them if we want!
# pro_area_raster <- rasterize( pro_area,raster_template)    
pro_area_bf_raster <- rasterize( pro_area_bf,raster_template)     
plot(pro_area_bf_raster)
res(pro_area_bf_raster)

tmap_mode("plot")
tm_shape(indonesia) + 
  tm_borders()+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(pro_area)+ tm_borders()+tm_fill(col = "green")+
  tm_layout(main.title="Protected area", main.title.position = "center", main.title.size = 1)

tmap_mode("plot")
tm_shape(indonesia) + 
  tm_borders()+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(pro_area_bf)+ tm_borders()+tm_fill(col = "green")+
  tm_layout(main.title="Protected area(Buffered)", main.title.position = "center", main.title.size = 1)



mask_raster <- rasterize(wind, protected_sf)     
####################################################################################################################
# Roads
Roads<-st_read("IDN_roads.shp")
Roads <- st_transform(Roads, 4326) 

tmap_mode("plot")
tm_shape(indonesia) + 
  tm_borders()+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(Roads)+  tm_lines(col = "green")+
  tm_layout(main.title="Roads in Indonesia", main.title.position = "center", main.title.size = 1)

# transfer it to raster data
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
Roads_bf_20<-st_buffer(Roads,d=20000)
Roads_raster_bf20<- rasterize(Roads_bf_20,raster_template)     
plot(Roads_raster_bf20)

####################################################################################################################
# Power line in Indonesia
Grid<-read_sf("grid.geojson")
Grid<- st_transform(Grid, 4326)

tmap_mode("plot")
  tm_shape(indonesia)+tm_borders()+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(Grid)+  tm_lines(col = "green")+
  tm_layout(main.title="Sites to the Powerline", main.title.position = "center", main.title.size = 1)+
  tm_shape(first_class_suit_sf)+tm_borders(col = "black" )

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,
                        crs = st_crs(indonesia)$wkt)
Grid_bf_55<-st_buffer(Grid,d=55000)
Grid_raster_bf55<- rasterize(Grid_bf_55,raster_template)     

tmap_mode("plot")

Grid_bf_55<-st_buffer(Grid,d=12000)



Grid_raster_bf55<- rasterize(Grid_bf_55,raster_template)     




