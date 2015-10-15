CargaimagenesDeTextura<-function(dirOut="ImagesOut/"){
    
    # Carga de las imagenes interpoladas mediante Regression Kriging de textura (Arena y limo).
    # se ajusta cada imagen al marco y resolución de trabajo
    
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
