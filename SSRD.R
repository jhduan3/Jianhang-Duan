
setwd("/Users/jackduan/Desktop/ESDA/BENG0093_Spatial/2nd_Assignment/data_codes")

library(ncdf4)
## Indonesia solar radiance
era <- nc_open("radiance.nc" )
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
tunits <- ncatt_get(era,"time","units")
#8am, 12pm, 4pm, 8pm.
#get the remainder. You will find that the time data comes in a group. For each group, there are four time period: 8, 12,16,20
(time[5]%%(24*365) ) %% 24 #2019-July-29
library(chron)
#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.
# Step 3. Extract variables
#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array) #dimension is 501 * 186 *8. Think about the Figure 1. The reason why it is called array is it is composed of 8 slices 
dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

# Step 4. Slice data
library(lattice)
library(RColorBrewer)

#Get a single time slice of the data using ssrd_array
ssrd_slice <- ssrd_array[,,2] 
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 2 in ssrd_array[,,2] indicate?  What if I want to slice all four time slice for "07/01/19"? 
length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) #8.5% are valid
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )
#example: check one max point
max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad

# Step 5. combine data and visualise
lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186
ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)
ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 
#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("plot")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_layout(main.title="Indonesia SSRD", main.title.position = "center", main.title.size = 1)

# interpolation for ssrd instead of power
coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) #get rid of geometry but keep all other attributes
ssrd_nogeom=na.omit(ssrd_nogeom)
gs <- gstat(formula=ssrd~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template 
idw <- interpolate(raster_template, gs, debug.level=0) 
idw_mask <- mask(idw, indonesia)
names(idw_mask) = c( "predicted","observed" )
library(tmap)
tm_shape(idw_mask$predicted) +
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE) +
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom")) +
  tm_layout(main.title="Indonesia Interpolated Solar Power", main.title.position = "center", main.title.size = 1,
            legend.position = c("left", "bottom")) + 
  tm_legend(width=0.5, height=0.5) 

writeRaster(idw_mask$predicted,'SSRD.tif')








# Step 6. Convert solar radiation to power

ncatt_get(era,"ssrd","units") #joul per metre2 
# an example of a 1m2 (A) solar panel
radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}
# Radiation data for solar electric (photovoltaic) systems are often represented as kilowatt-hours per square meter (kWh/m2)
# 1 joule/m2 = 1/3600/1000 kWh / m2 (one 1KWh contains 3.6×106 Joules)
ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

tmap_mode("plot")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")
  
# solar radiance to power
radiation_to_power <- function (G, A=1, r=0.175, p=0.6, hours=1) {
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

##########################################################.
#Interpolation
indonesia = st_read("idn_admbnda_adm0_bps_20200401.shp")
ssrd_sf = st_transform(ssrd_sf, 4326)
indonesia = st_transform(indonesia, st_crs(ssrd_sf))

coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) #get rid of geometry but keep all other attributes
ssrd_nogeom=na.omit(ssrd_nogeom)
# gstat::idw(formula, locations, ...) Function "idw" performs just as "krige" without a model being passed, but allows direct specification of the inverse distance weighting power. 
gs <- gstat(formula=ssrd_kwh~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs

#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 metre
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra

idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred,main="Interpolated SSRD Power")
names(idw_mask) = c( "predicted","observed" )

SSRD_power<-idw_mask$predicted


# Step 4.Visualising the interpolated results
library(tmap)

tm_shape(idw_mask$predicted) +
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE) +
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom")) +
  tm_layout(main.title="Indonesia Interpolated Solar Power", main.title.position = "center", main.title.size = 1,
            legend.position = c("left", "bottom")) + 
  tm_legend(width=0.5, height=0.5) # Adjust width and height as necessary

writeRaster(SSRD_power,'SSRD_power.tif')
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

# from wind speed to turbine speed
calc_turbine_speed <- function(speed, turbine_height=70, data_height=10, hellman_exponent=1/7){
  turbine_speed <-  speed*(turbine_height/data_height)^hellman_exponent
  return(turbine_speed)
}

# speed to power conversion
#define a curve that indicate the relationship between speed and wind
speed_to_power <- function(speed){ 
  wind_speed= seq(from=0, to=24, by=1)
  power = c(0,  0,  0,  102,  257,  471,  750,  1194, 1713,  2227,  2465,  2500,  2500,  2500,  2500,  2500,  2500,  2500,2500,  2500,  2500,  2500,  2500, 2400,  2250)
  wind_convert = data.frame(wind_speed, power)
  speed= round(speed) +1
  power=wind_convert[speed, "power"]
  return(power)
}
wind_u10_array <- ncvar_get(era,"u10")
wind_v10_array <- ncvar_get(era,"v10")
u10_slice <- wind_u10_array[,,2]
v10_slice <- wind_u10_array[,,2]
wind = sqrt ( u10_slice**2 + v10_slice**2) #calculate the speed

lonlat <- as.matrix( (expand.grid(lon, lat)))
wind_vec <- as.vector( wind)
wind_df <- data.frame( cbind( lonlat,wind_vec  ))
colnames(wind_df) <- c("lon", "lat", "wind")
wind_df_value <- na.omit (wind_df)
head(wind_df_value, 3) 

wind_sf<- st_as_sf( wind_df_value, coords = c(  "lon", "lat")  )
# To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(wind_sf) <- 4326 
wind_sf <- st_transform(wind_sf, 4326 )
tmap_mode("plot")
tm_shape(wind_sf)+
  tm_dots(col="wind", style = "quantile", size=.001, palette = "Blues")
# Carry out interpolation for wind speed

indonesia = st_read("idn_admbnda_adm0_bps_20200401.shp")
wind_sf = st_transform(wind_sf, 4326)
indonesia = st_transform(indonesia, st_crs(wind_sf))

coor = as.data.frame(st_coordinates(wind_sf))
wind_sf$x = coor$X
wind_sf$y = coor$Y
wind_nogeom = st_drop_geometry(wind_sf) #get rid of geometry but keep all other attributes
wind_nogeom=na.omit(wind_nogeom)
summary(wind_nogeom)
# gstat::idw(formula, locations, ...) Function "idw" performs just as "krige" without a model being passed, but allows direct specification of the inverse distance weighting power. 
gs <- gstat(formula=wind~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs

#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 metre
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template 
idw <- interpolate(raster_template, gs, debug.level=0) 
idw_mask <- mask(idw, indonesia)

plot(idw_mask$var1.pred)

tm_shape(idw_mask$var1.pred) +
  tm_raster(col="var1.pred", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE) +
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom")) +
  tm_layout(main.title="Indonesia Interpolated Wind Speed", main.title.position = "center", main.title.size = 1,
            legend.position = c("left", "bottom")) + 
  tm_legend(width=0.5, height=0.5) # Adjust width and height as necessary


# convert speed to power and do some maps
# use the functions we defined previously
wind_sf$speed= calc_turbine_speed (wind_sf$wind)
wind_sf$wind_power= speed_to_power(wind_sf$speed)
#i am only interested in some high potential areas, so i use 120 (subjective choice, you can also make it higher or lower) as a cutting point.
tm_shape(wind_sf[wind_sf$wind_power>2499, ])+
  tm_dots(col="wind_power", style = "quantile", size=.01, palette = c("yellow", "darkgreen"))+
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom")) +
  tm_layout(main.title="Area with High Wind Potential", main.title.position = "center", main.title.size = 1,
            legend.position = c("left", "bottom")) + 
  tm_legend(width=0.5, height=0.5)

tm_shape(wind_sf[wind_sf$wind_power>120, ])+
  tm_dots(col="wind_power", style = "quantile", size=.01, palette ="Blues")

# High potential areas concentrated in the coastal regions


wind_power<- wind_sf %>% 
  select(wind_power)  

tm_shape(wind_power)+
  tm_dots(col="wind_power", style = "quantile", size=.01, palette = c("yellow", "darkgreen"))

wind_power<-rasterize(wind_power,raster_template,field="wind_power")
writeRaster(wind_power,'wind_power.tif',overwrite=TRUE)
plot(wind_power)



####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# Interpolation
# Step 1.extract coordinates of wind_sf
indonesia = st_read("idn_admbnda_adm0_bps_20200401.shp")
wind_sf = st_transform(wind_sf, 4326)
indonesia = st_transform(indonesia, st_crs(wind_sf))

coor = as.data.frame(st_coordinates(wind_sf))
wind_sf$x = coor$X
wind_sf$y = coor$Y
wind_nogeom = st_drop_geometry(wind_sf) #get rid of geometry but keep all other attributes
wind_nogeom=na.omit(wind_nogeom)
# gstat::idw(formula, locations, ...) Function "idw" performs just as "krige" without a model being passed, but allows direct specification of the inverse distance weighting power. 
gs <- gstat(formula=wind~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs

#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 metre
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)

idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)
names(idw_mask) = c( "predicted","observed" )
writeRaster(idw_mask$var1.pred,"wind_speed.tif")

# Step 4.Visualising the interpolated results
tmap_mode("plot")
tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE)+
tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom")) +
  tm_layout(main.title="Indonesia Interpolated Wind Speed", main.title.position = "center", main.title.size = 1,
            legend.position = c("left", "bottom")) + 
  tm_legend(width=0.5, height=0.5) # Adjust width and height as necessary








