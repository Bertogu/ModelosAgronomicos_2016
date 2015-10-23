rm(list=ls()) # Clean the workspace
gc()

library(rgdal)
library(gdalUtils)
library(stringr) # for manging strings
# library(GSIF)
library(sp)
library(gstat)
# library(aqp)
# library(randomForest)
library(plyr)
library(ggplot2)
# library(e1071)
library(maptools)
library(sqldf)


source("scripts/R/funcionesComunes.R")


prop.hidraulicas<-cargaDatos(dirIn="ImagesOut/",sk=FALSE)

ptos.prediccion<-readOGR(dsn="Vectoriales/Reticula_1000m_ETRS89.shp",layer="Reticula_1000m_ETRS89",encoding="ESRI Shapefile")
proj4string(ptos.prediccion)<-CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

ov<-over(x=ptos.prediccion,y=prop.hidraulicas)
ptos.prediccion<-as(ptos.prediccion,Class="data.frame")
ptos.prediccion<-cbind(ptos.prediccion,ov)


ptos.prediccion$fila<-round((ptos.prediccion$coords.x2-min(ptos.prediccion$coords.x2))/1000,digits=0)
ptos.prediccion$columna<-round((ptos.prediccion$coords.x1-min(ptos.prediccion$coords.x1))/1000,digits=0)

# Para transformar fila columna a coordenadas
# ptos.prediccion$X<-x_min+1000*(ptos.prediccion$columna)
# ptos.prediccion$y<-y_min+1000*(ptos.prediccion$fila)


ptos.prediccion$Clay<-ifelse(test=is.na(ptos.prediccion$Clay),yes=-9999,no=ptos.prediccion$Clay)
ptos.prediccion$CRAD_percen_vol<-ifelse(test=is.na(ptos.prediccion$CRAD_percen_vol),yes=-9999,no=ptos.prediccion$CRAD_percen_vol)
names(ptos.prediccion)[6]<-"CRAD"
ptos.prediccion$FC_percen_vol<-ifelse(test=is.na(ptos.prediccion$FC_percen_vol),yes=-9999,no=ptos.prediccion$FC_percen_vol)
names(ptos.prediccion)[7]<-"FC"
ptos.prediccion$Ksat_mm_dia<-ifelse(test=is.na(ptos.prediccion$Ksat_mm_dia),yes=-9999,no=ptos.prediccion$Ksat_mm_dia)
names(ptos.prediccion)[8]<-"KSAT"
ptos.prediccion$MO<-ifelse(test=is.na(ptos.prediccion$MO),yes=-9999,no=ptos.prediccion$MO)
ptos.prediccion$Sand<-ifelse(test=is.na(ptos.prediccion$Sand),yes=-9999,no=ptos.prediccion$Sand)
ptos.prediccion$Sat_percen_vol<-ifelse(test=is.na(ptos.prediccion$Sat_percen_vol),yes=-9999,no=ptos.prediccion$Sat_percen_vol)
names(ptos.prediccion)[11]<-"SAT"
ptos.prediccion$Silt<-ifelse(test=is.na(ptos.prediccion$Silt),yes=-9999,no=ptos.prediccion$Silt)
ptos.prediccion$Soil_depth_m<-ifelse(test=is.na(ptos.prediccion$Soil_depth_m),yes=-9999,no=ptos.prediccion$Soil_depth_m)
names(ptos.prediccion)[13]<-"THCK"
ptos.prediccion$WP_percen_vol<-ifelse(test=is.na(ptos.prediccion$WP_percen_vol),yes=-9999,no=ptos.prediccion$WP_percen_vol)
names(ptos.prediccion)[14]<-"WP"



# Load results in the database but I finally didn´t use this, I export them in shapefile format and then
# load this shapefile into the database.
# vuelcaSoilProperties2DB(dirDB="BD/Calibracion_2016.sqlite",soilDataFrame=ptos.prediccion)


coordinates(ptos.prediccion)<- ~coords.x1+coords.x2
proj4string(ptos.prediccion)<-CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


writeOGR(obj=ptos.prediccion,dsn="Temp/SoilProperties.shp",layer="SoilProperties",driver="ESRI Shapefile")

# Rasterize predictions

# Field capacity
gdal_rasterize(src_datasource ="Temp/SoilProperties.shp",dst_filename ="Temp/suelo_1000m/FC_porcentaje.tif",a="FC",
               of="GTiff",a_srs="EPSG:25830",tr=c(1000,1000),a_nodata=-9999,ot="Float32",co="TFW=YES")
# Permiabilidad en saturación
gdal_rasterize(src_datasource ="Temp/SoilProperties.shp",dst_filename ="Temp/suelo_1000m/KSAT_mm_dia.tif",a="KSAT",
               of="GTiff",a_srs="EPSG:25830",tr=c(1000,1000),a_nodata=-9999,ot="Float32",co="TFW=YES")
# Saturation
gdal_rasterize(src_datasource ="Temp/SoilProperties.shp",dst_filename ="Temp/suelo_1000m/SAT_porcentaje.tif",a="SAT",
               of="GTiff",a_srs="EPSG:25830",tr=c(1000,1000),a_nodata=-9999,ot="Float32",co="TFW=YES")
# thick of the horizont
gdal_rasterize(src_datasource ="Temp/SoilProperties.shp",dst_filename ="Temp/suelo_1000m/THICK_m.tif",a="THCK",
               of="GTiff",a_srs="EPSG:25830",tr=c(1000,1000),a_nodata=-9999,ot="Float32",co="TFW=YES")
# Wilting point
gdal_rasterize(src_datasource ="Temp/SoilProperties.shp",dst_filename ="Temp/suelo_1000m/WP_porcentaje.tif",a="WP",
               of="GTiff",a_srs="EPSG:25830",tr=c(1000,1000),a_nodata=-9999,ot="Float32",co="TFW=YES")









