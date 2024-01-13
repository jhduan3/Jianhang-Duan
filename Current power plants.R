
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

# Indonesia 
indonesia <-st_read("idn_admbnda_adm1_bps_20200401.shp")
indonesia <- st_transform(indonesia, 4326) 


Power_plant <-na.omit(read.csv("BENV0093_2ndAssignment_TeamWork - Sheet1.csv")[c(1,2,3,4,5,6)])
# Generate spatial points
Power_plant <- st_as_sf(Power_plant, coords = c("longitude", "latitude"), crs=4326) 
# solar
solar<-Power_plant[Power_plant$type=="solar",]
solar_con<-solar[solar$status=="construction",]
solar_pln<-solar[solar$status=="planned",]


# hydro
hydro_planned<-Power_plant[Power_plant$type=="hydro",]
hydro_ext<-Power_plant[Power_plant$type=="Hydro",]

# geothermal
geothermal_ext<-Power_plant[Power_plant$type=="Geothermal",]

# wind
wind_pln<-Power_plant[Power_plant$type=="wind",]

# tmap_mode("plot")

tmap_mode("plot")
tm_shape(indonesia) +
  tm_borders() +
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(wind_pln) + 
  tm_dots(size=0.02, col="orange", shape=4) +
  tm_text("capacity_mw", size=.6, col='black', shadow=TRUE, xmod=1,ymod = 0.5) +
  tm_add_legend("symbol", labels = "Planned Wind plant", col = "orange", size =  0.3,shape = 4)+
  tm_shape(solar_con) + 
  tm_dots(size=0.02, col="blue", shape=4) +
  tm_add_legend("symbol", labels = "Constructing Wind plant", col = "blue", size =  0.3,shape=4)+
  tm_shape(solar_pln) + 
  tm_dots(size=0.02, col="red", shape=22) +
  tm_add_legend("symbol", labels = "Planned Solar plant", shape = 22,col = "red", size =  0.3)+
  tm_shape(geothermal_ext) + 
  tm_dots(size=0.02, col="purple", shape=23) +
  tm_add_legend("symbol", labels = "Planned Geothermal plant",shape = 23, col = "purple", size =  0.3)+
  tm_shape(hydro_ext) + 
  tm_dots(size=0.02, col="brown", shape=7) + 
    tm_add_legend("symbol", labels = "Existing Hydro plant",shape = 7, col = "brown", size = 0.3)+
  tm_shape(hydro_planned) + 
  tm_dots(size=0.02, col="green", shape=7) +
    tm_add_legend("symbol", labels = "Planned Hydro plant",shape = 7,col = "green", size = 0.3)+
  tm_layout(legend.position = c("left", "bottom"),
    legend.text.size = 0.4,  # Adjust the text size
    legend.width = 0.18,     # Adjust the width of the legend box
    legend.height = 0.35     # Adjust the height of the legend box
  )



