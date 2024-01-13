
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

## 所有分辨率都以solar 为基准
# plot(ssrd)
solar <-rast("DIF.tif")
solar_temp<-rast("TEMP.tif")
solar_ghi<-rast("GHI.tif")
elevation <-rast("IDN_elevation_w_bathymetry.tif")
# elevation<-aggregate(elevation, fact=20)
# res(elevation)
# plot(elevation)
# 在这里改变数据的像素 不然数据太大了
# 现在可以去掉数据里的na
# elevation[is.na(elevation)] <- 0
# summary(elevation)
wind<-rast("IDN_wind-speed_10m.tif")
power<-rast("IDN_power-density_50m.tif")
capacity_factor<-rast("IDN_capacity-factor_IEC1.tif")
slope<-rast("idn_srtm_slope_100m.tif")
distance_to_mainroad<-rast("idn_osm_dst_road_100m_2016.tif")
distance_to_inlandwater<-rast("idn_esaccilc_dst_water_100m_2000_2012.tif")
landcover<-rast("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
waterbodies<-rast("ESACCI-LC-L4-WB-Map-150m-P13Y-2000-v4.0 3.tif")
residence_area<-rast("idn_ppp_2020_UNadj.tif")
primary_forest<-rast("idn_primary.tif")

## Indonesia
indonesia <-st_read("idn_admbnda_adm1_bps_20200401.shp")
indonesia <- st_transform(indonesia, 4326) 
## Mask the data (choose wanted area)
wind_mask <- mask(wind, indonesia) 
elevation_mask<-mask(elevation, indonesia) 
power_mask<-mask(power, indonesia) 
capacity_factor_maske<-mask(capacity_factor, indonesia)
landcover_masked<-crop(landcover,indonesia,mask  = TRUE)
waterbodies_masked<-crop(waterbodies,indonesia,mask  = TRUE)

plot(wind_mask, main = "Wind speed")
plot(elevation_mask,main= "Elevation")

plot(solar,main = "Yearly average of diffuse horizontal irradiation \n(kWh/m2)")
plot(solar_temp,main = " Yearly average of \n air temperature (Degress)")
plot(solar_ghi,main = "  Yearly average of global horizontal irradiation[GHI]  \n( kWh/m2)")

plot(power_mask,main = "Wind Power Density")
plot(capacity_factor_masked,main = "Wind Capacity Factor")
plot(slope, main = "Slope in Indonesia" , xlim=c(0,141))
plot(distance_to_mainroad,main = "Distance to Major Road (km)",xlim=c(0,141))
plot(distance_to_inlandwater,main = "Distance to Inland water (km)",xlim=c(0,141))
plot(waterbodies_masked,main="Indonesia Inland water",col=c("grey","blue"))

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
## Residence Area
## Reclassify data, assign suitable area as 1, and unsuitable as 0.
## 20-300 设置为 1 
## 0-20 设置为 0
residence_area<-rast("idn_ppp_2020_UNadj.tif")
residence_area_m<-matrix(c(20,300,1,
                           0,20,0), ncol=3, byrow=TRUE)
residence_area_rc <- classify(residence_area, residence_area_m, include.lowest=TRUE)
# tm_shape(residence_area_rc)+ 
#  tm_raster(style= "cat", labels= c("Resident Area", "Non-resident Area"), alpha=.7)

# 只保留residence 数据
residence_area_rc[residence_area_rc==0]=NA

plot(residence_area_rc)
# 数据转换
residence_area_v <- as.polygons(residence_area_rc)
residence_area_sf <- st_as_sf(residence_area_v)
residence_area_sfbf<-st_buffer(residence_area_sf,d=1500)

# 得到Buffer的raste data
residence_area_bf_raster<-rasterize(residence_area_sfbf,raster_template)     
plot(residence_area_bf_raster)

# 画图
tmap_mode("plot")
tm_shape(indonesia) + 
  tm_borders()+
  tm_compass(type = "8star", position = c("right", "top"),size = 2)+
  tm_scale_bar(breaks=c(0 , 200, 400), position=c("right", "bottom"))+
  tm_shape(residence_area_bf_raster)+ tm_raster()+
  tm_layout(main.title="Residential Area (Buffered)", main.title.position = "center", main.title.size = 1,
            legend.show = FALSE)

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#Site selection for topographic inclination
# Step 0 确保数据没有NA
# 不然无法进行！！ 所以这一步我们需要使用interpolation
# 使用 IDW_spatial interpolation
# 尝试降低一下原始数据 resolution
indonesia <-st_read("idn_admbnda_adm1_bps_20200401.shp")
elevation <-rast("IDN_elevation_w_bathymetry.tif")
elevation<-aggregate(elevation, fact=20)

# res(elevation)
# plot(elevation)

coor = as.data.frame(elevation,xy=TRUE)
colnames(coor)<-c("x","y",'elevation')
coor = na.omit(coor)
# 这个是用于 sf data
# b<-st_as_sf(coor, coords = c("x", "y"))
# coor = as.data.frame(elevation,xy=TRUE)
# b$x<-coor$x
# b$y<-coor$y
# elevation_nogeom=st_drop_geometry(b)
# elevation_nogeom=na.omit(elevation_nogeom)
# gs <- gstat(formula=elevation~1,locations=~x+y, data=elevation_nogeom,nmax=1,set=list(idp=2))
gs <- gstat(formula=elevation~1,locations=~x+y, data=coor,nmax=20,set=list(idp=2))
#st_bbox(indonesia)
raster_template = rast( resolution = 0.05, xmin=95.01079 , 
                        ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  , crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template,gs, debug.level=0)
elevation_interpo<-idw$var1.pred

elevation_interpo_crop<-crop(elevation_interpo,indonesia)
summary(elevation_interpo_crop)
## 现在不需要把crop界外的数值清理掉
plot(elevation_interpo_crop)
writeRaster(elevation_interpo_mask,"elevation_interpo.tif",overwrite=TRUE)

# Step 1 classify  slope data
slope = terrain(elevation_interpo_crop, v='slope', unit='degrees') 
slope[is.na(slope)] <- 0
plot(slope)
summary(slope)

# slope_m<-matrix(c x(0,5,1,5,80,0), ncol=3, byrow=TRUE)
# slope_rc <- classify(slope, slope_m, include.lowest=TRUE) # o是unsuitable 1 是suitable
# classify elevation data
elevation_m<-matrix(c(-2122,0,0,
                      0,750,1,
                      750,4756,0
), ncol=3, byrow=TRUE)
elevation_rc <- classify(elevation_interpo_mask,elevation_m,include.lowest=TRUE)
# plot(elevation_rc)
# step 2. put all in the same resolution as slope
# They are already in the same resolution
newdat= c(elevation_rc,slope)  #combine all three spatial raster data into one dataset
names(newdat) <- c("elevation", "slope") #rename layer

# step 4. identify cells matches our criterias.
newdat_df= as.data.frame(newdat, xy=TRUE) #xy=TRUE, will help to export the coordinates of cells
id = which( newdat_df$elevation == 1
            & newdat_df$slope<=5 )
# id = which( newdat_df$slope <=100
#            & newdat_df$elevation<= 100)
# same result, 因为same resolution
length (id)/ ncell(slope) 
length (id)/ ncell(elevation_rc) 

# step 5. extract matched areas and visualise
# add a suitability layer
suitability = elevation_rc
values(suitability)= 0

#add the suitability layer onto newdata
newdat=c(newdat, suitability)
names(newdat)[3] = "suitability"

# assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1 # 满足id 的为 1
#newdat$suitability[newdat$suitability ==0] <- NA
plot(newdat$suitability)

# 最后mask
newdat_mask = crop(newdat, indonesia,mask=TRUE)
plot(newdat_mask$suitability,col=c("grey","brown"),main="Geographical Suitability \n (Consider Elevation and Slope) ")
plot(indonesia, col=NA, add=TRUE, )
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

# LAND COVER
# Step 0 Interpolate
landcover<-rast("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
landcover<-aggregate(landcover, fact=20)
#res(landcover)
landcover<-crop(landcover,indonesia, mask=TRUE)
plot(landcover, main="Land cover in indonesia")

coor = as.data.frame(landcover,xy=TRUE)
colnames(coor)<-c("x","y",'landcover')
coor = na.omit(coor)
gs <- gstat(formula=landcover~1,locations=~x+y, data=coor,nmax=20,set=list(idp=2))
raster_template = rast( resolution = 0.05, xmin=95.01079 , 
                        ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  , crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template,gs, debug.level=0)
landcover_interpo<-idw$var1.pred

#选择需要的区域 #这一步可加可不加
#landcover_interpo<-crop(landcover_interpo,indonesia,mask=TRUE)
#区域外的na去掉 #这一步可加可不加
#landcover_interpo[is.na(landcover_interpo)] <- 0

# summary(landcover_interpo)
# plot(landcover_interpo)
# plot(landcover)
r_matrix<-matrix (c(0,12,0, # cropland,rainfed (no)
                    12,40,1,  # Tree, shrub
                    40, 100, 2,# Forest  (no)
                    100, 153, 3,# small Shrubland
                    153,170,4,  # Tree cover, flooded, fresh or brakish water  (no)
                    170, 180, 1,  # herbaceous cover
                    180,190,5,   # Urban area  (no)
                    190,202,6, # Bare area 
                    202,211,7), ncol=3, byrow= TRUE) # water body  (no)
landcover_rc <- classify(landcover_interpo,r_matrix,include.lowest=TRUE)
plot(landcover_rc)
#r_matrix<-matrix (c(0,12,0, # cropland,rainfed (no)
#                    12,40,1,  # Tree, shrub
#                    40, 100, 2,# Forest  (no)
#                   100, 153, 3,# small Shrubland
#                    153,170,4,  # Tree cover, flooded, fresh or brakish water  (no)
#                    170, 180, 1,  # herbaceous cover
#                    180,190,5,   # Urban area  (no)
#                    190,202,6, # Bare area 
# landcover_rc <- classify(landcover,r_matrix,include.lowest=TRUE)
# plot(landcover_rc,main="Land cover reclassified")

newdat= c(landcover_rc)
names(newdat) <- c("landcover") #rename layer
newdat_df= as.data.frame(newdat, xy=TRUE)
id = which(newdat_df$landcover ==1 |newdat_df$landcover == 3| newdat_df$landcover==6)

length (id)/ ncell(landcover_rc) 
#add a suitability layer
suitability = landcover_rc
values(suitability)= 0

#add the suitability layer onto newdata
newdat=c(newdat, suitability)
names(newdat)[2] = "suitability"

#assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1
plot(newdat$suitability,col=c("grey","brown"))

# mask
newdat_mask = crop(newdat, indonesia,mask=TRUE)
plot(newdat_mask$suitability,col=c("grey","brown"), main= "Landcover  Suitability")
plot(indonesia, col=NA, add=TRUE, )

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# Final suitability analysis
# graphical_suit<-newdat_mask$suitability
# land_suit<-newdat_mask$suitability
# plot(graphical_suit)
# plot(land_suit)

# summary(graphical_suit)
# summary(land_suit)
# summary(pro_area_bf_raster)
# summary(residence_area_bf_raster)
graphical_suit[is.na(graphical_suit)]<-0
land_suit[is.na(land_suit)]<-0
pro_area_bf_raster
residence_area_bf_raster
Roads_raster

# resample based on graphical_suit
land_suit2= resample(land_suit, graphical_suit)
pro_area_bf_raster2= resample(pro_area_bf_raster, graphical_suit)
residence_area_bf_raster2= resample(residence_area_bf_raster, graphical_suit)
Roads_raster2= resample(Roads_raster, graphical_suit)
newdat= c(graphical_suit,land_suit2,pro_area_bf_raster2,residence_area_bf_raster,Roads_raster2)
names(newdat) <- c("Geographical", "landcover", "pro_area","res_area","roads")

newdat_df= as.data.frame(newdat, xy=TRUE) #xy=TRUE, will help to export the coordinates of cells
newdat_df[is.na(newdat_df$pro_area), "pro_area"]<- 0
newdat_df[is.na(newdat_df$res_area), "res_area"]<- 0 
newdat_df[is.na(newdat_df$roads), "roads"]<- 0 

id = which( newdat_df$Geographical==1
            & newdat_df$landcover==1 
            & newdat_df$pro_area==0
            & newdat_df$res_area==0
            & newdat_df$roads==0)

length (id)/ ncell(graphical_suit) 
length (id)/ ncell(land_suit2) 
length (id)/ ncell(pro_area_bf_raster2) 
length (id)/ ncell(residence_area_bf_raster2) 
length (id)/ ncell(Roads_raster2) 

#add a suitability layer
suitability = graphical_suit
values(suitability)= NA

#add the suitability layer onto newdata
newdat=c(newdat, suitability)
names(newdat)[6] = "suitability"

#assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1
plot(newdat$suitability, col="brown",main="Overall suitability of Indonesia based on the \n spatial constriants")
plot(indonesia, col=NA, add=TRUE, )


final_suitability<-newdat$suitability
writeRaster(final_suitability,"final_suitability.tif",overwrite=TRUE)

final<-rast("final_suitability.tif")
plot(final)
