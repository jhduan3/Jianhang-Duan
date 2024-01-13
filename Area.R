
setwd("/Users/jackduan/Desktop/ESDA/BENG0093_Spatial/2nd_Assignment/data_codes")
library(terra)
library(sf)
library(tmap)
library(dplyr)
library(raster)
library(sp)
library(tmap)
library(spatstat)
library(reshape)
library(spdep) #key function used for conducting spatial autocorrelation
library(plotly)
library(gstat)
library(automap)
library(ncdf4)
library(chron)
library(lattice)
library(stars)
library(ncdf4)


final_suitability<-rast("final_suitability.tif")
ssrd<-rast("SSRD.tif")
ssrd_power<-rast('SSRD_power.tif')
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 meter

res(final_suitability)

final_suitability
final_suitability[is.na(final_suitability)]<-0
summary(final_suitability)

Roads_raster_bf20
Roads_raster_bf20[is.na(Roads_raster_bf20)]<-0
summary(Roads_raster_bf20)

Grid_raster_bf55
Grid_raster_bf55[is.na(Grid_raster_bf55)]<-0
plot(Grid_raster_bf55)

tmap_mode("view")
tm_shape(Grid_bf_55)+tm_polygons()+
  tm_shape(first_class_suit_sf)+tm_borders(col = "red")


solar_ghi<-rast("GHI.tif")
solar_temp<-rast("TEMP.tif")
ssrd<-rast("SSRD.tif")
SSRD_power<-rast('SSRD_power.tif')
distance_to_inlandwater<-rast("idn_esaccilc_dst_water_100m_2000_2012.tif")


final_suitability2<-resample(final_suitability,Roads_raster_bf20)
Grid_raster_bf552<-resample(Grid_raster_bf55,Roads_raster_bf20 )
SSRD_power2<-resample(SSRD_power,Roads_raster_bf20 )
solar_ghi2<-resample(solar_ghi,Roads_raster_bf20 )
solar_temp2<-resample(solar_temp,Roads_raster_bf20 )
# distance_to_inlandwater2<-resample(distance_to_inlandwater,Roads_raster_bf20)
# plot(distance_to_inlandwater2,xlim=c(94,141))
newdat = c(final_suitability2,Roads_raster_bf20,Grid_raster_bf552,SSRD_power2,solar_ghi2,solar_temp2,distance_to_inlandwater2)
names(newdat) <- c("final_suit","Roads", "Grid","Solar_Power","GHI","Temp","D_water")

newdat_df= as.data.frame(newdat, xy=TRUE)
newdat_df[is.na(newdat_df$Solar_Power), "Solar_Power"]<- 0 
newdat_df[is.na(newdat_df$GHI), "GHI"]<- 0 
newdat_df[is.na(newdat_df$Temp), "Temp"]<- 0 
newdat_df[is.na(newdat_df$D_water), "D_water"]<- 0 

summary(newdat_df)

id = which( newdat_df$Roads==1
            & newdat_df$Grid==1 
            & newdat_df$final_suit==1 
            & (newdat_df$GHI <= 2300 & newdat_df$GHI>= 2180)
            & newdat_df$Temp>=0
            & newdat_df$D_water<= 25 )
length (id)

length (id)/ ncell(final_suitability2)
length (id)/ ncell(Roads_raster_bf20) 
length (id)/ ncell(Grid_raster_bf552) 
length (id)/ ncell(SSRD_power2) 
length (id)/ ncell(solar_ghi2) 
length (id)/ ncell(solar_temp2) 
length (id)/ ncell(distance_to_inlandwater2) 

#add a suitability layer
suitability = Grid_raster_bf552
values(suitability)= NA

#add the suitability layer onto newdata
newdat=c(newdat_df, suitability)
names(newdat)[10] = "suitability"
#assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1
plot(newdat$suitability, col="brown",main="Suitable sites for constructing the solar plants")
# plot(indonesia, col=NA, add=TRUE, )

first_class_suit<-newdat$suitability
plot(first_class_suit)

first_class_suit_v <- as.polygons(first_class_suit)
first_class_suit_sf <- st_as_sf(first_class_suit_v)
plot(first_class_suit_sf)

ssrd_croped<-crop(ssrd,first_class_suit_sf,mask=TRUE)
plot(ssrd_croped)
summary(ssrd_croped)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# SOLAR
radiation_to_power <- function (G, A, r=0.175, p=0.6, hours=1) {
  MWh <- G * A * r * p * (hours/3600000000) 
  return(MWh)
}

radiation_to_power(9000000,12000000)
314640*(111.1*0.05)*(111.1*0.05)*1000

# 一个cell 有 30.85802 平方千里
# 一平方千米最多 两个plant 一个plant 最多 5， 那么一平方公里就是10mw
# gap= 24652.9MW

rad<-radiation_to_power(G=16346341,A=6900000)
rad*2
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
## WIND
wind_power<-rast("wind_power.tif")
capacity_factor<-rast("IDN_capacity-factor_IEC1.tif")
capacity_factor<-mask(capacity_factor, indonesia)
final_suitability<-rast("final_suitability.tif")
plot(capacity_factor)

final_suitability[is.na(final_suitability)]<-0
summary(final_suitability)
Roads_raster_bf20[is.na(Roads_raster_bf20)]<-0
summary(Roads_raster_bf20)
Grid_raster_bf55[is.na(Grid_raster_bf55)]<-0
summary(Grid_raster_bf55)


capacity_factor2<-resample(capacity_factor,final_suitability)
wind_power2<-resample(wind_power,final_suitability)

plot(final_suitability)
plot(capacity_factor2)
plot(Grid_raster_bf552)
plot(Roads_raster_bf202)

newdat = c(final_suitability,capacity_factor2,wind_power2,Grid_raster_bf55,Roads_raster_bf20)
names(newdat) <- c("final_suit","capacity_factor",'wind_power',"Grid","Roads")

newdat_df= as.data.frame(newdat, xy=TRUE)
newdat_df[is.na(newdat_df$capacity_factor), "capacity_factor"]<- 0 
newdat_df[is.na(newdat_df$wind_power), "wind_power"]<- 0 
summary(newdat_df)

id = which( newdat_df$Roads==1
            & newdat_df$Grid==1
            & newdat_df$final_suit==1
            & newdat_df$wind_power>=700) # 700
length (id)

length (id)/ ncell(final_suitability)
length (id)/ ncell(Roads_raster_bf20) 
length (id)/ ncell(Grid_raster_bf55) 
length (id)/ ncell(capacity_factor2) 

#add a suitability layer
suitability = wind_power2
values(suitability)= NA

#add the suitability layer onto newdata
newdat=c(newdat_df, suitability)
names(newdat)[8] = "suitability"
#assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1
plot(newdat$suitability, col="brown",main="Suitable sites for constructing the solar plants")
# plot(indonesia, col=NA, add=TRUE, )
  

first_class_suit<-newdat$suitability
plot(first_class_suit)

first_class_suit_v <- as.polygons(first_class_suit)
first_class_suit_sf <- st_as_sf(first_class_suit_v)
plot(first_class_suit_sf)

tmap_mode("view")
tm_shape(first_class_suit_sf)+tm_borders(col = "red")+
tm_shape(Grid)+tm_lines()



wind_power_croped<-crop(wind_power2,first_class_suit_sf,mask=TRUE)
plot(wind_power_croped, col=c("purple","red", "blue", "green", "black", "orange"),xlim=c(90,130),ylim=c(-15,8))
plot(indonesia, col=NA, add=TRUE, )




30000000/45239 

