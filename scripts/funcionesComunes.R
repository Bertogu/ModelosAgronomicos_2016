
AjustaimagenesDeTextura<-function(dirOut="ImagesIn/Resampled/"){
    
    # Carga de las imagenes interpoladas mediante Regression Kriging de textura (Arena y limo).
    # se ajusta cada imagen al marco y resolución de trabajo
    
#     gdalwarp(srcfile="Textura/RegressionKriging/500m_exp/RK_Sand.tif",dstfile=paste(dirOut,"RK_Sand_NoData.tif",sep="",collapse=NULL),
#              t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES", srcnodata= "0",
#              dstnodata="-9999",tr="500 500",te="165150 4439190 602150 4789190")
#     
#     gdalwarp(srcfile="Textura/RegressionKriging/500m_exp/RK_Silt.tif",dstfile=paste(dirOut,"RK_Silt_NoData.tif",sep="",collapse=NULL),
#              t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES", srcnodata= "0",
#              dstnodata="-9999",tr="500 500",te="165150 4439190 602150 4789190")
       
    gdalwarp(srcfile="ImagesIn/Original/SK_Arena_500m.tif",dstfile=paste(dirOut,"sand.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="165150 4439190 602150 4789190")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_Limo_500m.tif",dstfile=paste(dirOut,"silt.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="165150 4439190 602150 4789190")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_MO_500m.tif",dstfile=paste(dirOut,"MO.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="165150 4439190 602150 4789190")
    
    
}

cargaDatos<-function(dirIn="ImagesIn/Resampled/"){
    Tif_files<-list.files(path=dirIn,pattern="*.tif")
    tfw_file<-list.files(path=dirIn,pattern="*.tfw")
    xml_file<-list.files(path=dirIn,pattern="*.xml")
    Tif_files<-Tif_files[!Tif_files %in% tfw_file] # delete from the list tfw and xml files
    Tif_files<-Tif_files[!Tif_files %in% xml_file]
    
    for (i in 1:length(Tif_files)){
        
        if (i==1){
            covar.grid<-readGDAL(paste(dirIn,Tif_files[i],sep="",collapse=NULL))
            names(covar.grid)[1]<-strsplit(Tif_files[i],"\\.")[[1]][1]        
        }
        else{
            nombre<-(strsplit(Tif_files[i],"\\.")[[1]][1])
            covar.grid[[nombre]]<-readGDAL(paste(dirIn,Tif_files[i],sep="",collapse=NULL))$band1
        }
        
    }

    covar.grid$clay<-100-(covar.grid$sand+covar.grid$silt)
    covar.grid$clay<-ifelse(covar.grid$clay<0,0,covar.grid$clay)
    print("Sand statistics:")
    print(summary(covar.grid$sand))
    
    print("Silt statistics:")
    print(summary(covar.grid$silt))
    
    print("Clay statistics:")
    print(summary(covar.grid$clay))
    
    print("MO statistics:")
    print(summary(covar.grid$MO))
    

    return(covar.grid)
    
    
}

