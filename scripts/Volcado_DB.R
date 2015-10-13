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
source("scripts/funcionesComunes.R")


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

summary(ptos.prediccion$y-ptos.prediccion$coords.x2)

str(ptos.prediccion)










