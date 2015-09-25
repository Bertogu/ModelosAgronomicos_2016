###############################################################
############### Clean WS and load libraries ###################
###############################################################

rm(list=ls()) # Clean the workspace
gc()
# load required libraries
# library(rgdal)
# library(gdalUtils)
# library(stringr) # for manging strings
# library(GSIF)
# library(sp)
# library(gstat)
# library(aqp)
# library(randomForest)
# library(plyr)
# library(ggplot2)
library(e1071)
library(maptools)


getwd()
a="ImagesOut/"
print(paste(a,"RK_Sand_NoData.tif",sep="",collapse=NULL))

CargaImagenesDeTextura<-function(dirOut="ImagesOut/"){
    
    gdalwarp(srcfile="Textura/RegressionKriging/500m_exp/RK_Sand.tif",dstfile=paste(dirOut,"RK_Sand_NoData.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES", srcnodata= "0",
             dstnodata="-9999",tr="500 500",te="165150 4439190 602150 4789190")
    
    gdalwarp(srcfile="Textura/RegressionKriging/500m_exp/RK_Silt.tif",dstfile=paste(dirOut,"RK_Sand_NoData.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES", srcnodata= "0",
             dstnodata="-9999",tr="500 500",te="165150 4439190 602150 4789190")
    
    
    gdalwarp(srcfile="Textura/SimpleKriging/500m_exp/SK_Arena_500m.tif",dstfile="Textura/SK_Sand_Resample.tif",
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="165150 4439190 602150 4789190")
    
    gdalwarp(srcfile="Textura/SimpleKriging/500m_exp/SK_Limo_500m.tif",dstfile="Textura/SK_Silt_Resample.tif",
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="165150 4439190 602150 4789190")
    
    
    
    
    
}












# handle NoData Values in Regresion Kriging interpolation





Tif_files<-list.files(path="Textura/",pattern="*.tif")
tfw_file<-list.files(path="Textura/",pattern="*.tfw")
xml_file<-list.files(path="Textura/",pattern="*.xml")
Tif_files<-Tif_files[!Tif_files %in% tfw_file] # delete from the list tfw and xml files
Tif_files<-Tif_files[!Tif_files %in% xml_file]

for (i in 1:length(Tif_files)){
    
    if (i==1){
        covar.grid<-readGDAL(paste("Textura/",Tif_files[i],sep="",collapse=NULL))
        names(covar.grid)[1]<-strsplit(Tif_files[i],"\\.")[[1]][1]        
    }
    else{
        nombre<-(strsplit(Tif_files[i],"\\.")[[1]][1])
        covar.grid[[nombre]]<-readGDAL(paste("Textura/",Tif_files[i],sep="",collapse=NULL))$band1
    }
    
}
str(covar.grid)

rm(covar.grid$sand_fix)

covar.grid$sand_fix<-ifelse(is.na(covar.grid$RK_Sand_NoData),covar.grid$SK_Sand_Resample,covar.grid$RK_Sand_NoData)

is.na(covar.grid$SK_Sand_Resample[1])










