###############################################################
############### Clean WS and load libraries ###################
###############################################################

rm(list=ls()) # Clean the workspace
gc()

library(rgdal)
library(gdalUtils)
library(stringr) # for manging strings
library(GSIF)
library(sp)
library(gstat)
library(aqp)
library(randomForest)
library(plyr)
library(ggplot2)
library(e1071)
library(maptools)



source("scripts/funcionesComunes.R")
CargaimagenesDeTextura()

#TODO: Remove 0 values from clay 
a<-preparaDatos()
str(a)

writeGDAL(fname="ImagesOut/Clay.tif",drivername="GTiff",dataset=a[7])









