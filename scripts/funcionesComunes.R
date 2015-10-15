
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
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_Limo_500m.tif",dstfile=paste(dirOut,"silt.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_MO_500m.tif",dstfile=paste(dirOut,"MO.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    
}

cargaDatos<-function(dirIn="ImagesIn/Resampled/",sk=TRUE){
# This function load the texture images in a SpatialGridDataFrame structure
# parameters:
#   - dirIn: Directory where texture images are stored
#   - sk: wethear clay must be compute or only load data
    
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
    
    if (sk==TRUE){
        
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
    
    return(covar.grid)

    
}


AjustaimagenesDeTexturaConRregressionKriging<-function(dirOut="ImagesIn/Original/Temp/"){

    # Resample texture images: Simple Kriging and Regression Kriging. Then load them (sk=Flase) 
    # and fill the gaps in RK images with Simple kriging values. Finally export the results in 
    # the resampled directory.
    
    
    gdalwarp(srcfile="ImagesIn/Original/SK_Arena_500m.tif",dstfile=paste(dirOut,"sk_sand.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_Limo_500m.tif",dstfile=paste(dirOut,"sk_silt.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/SK_MO_500m.tif",dstfile=paste(dirOut,"sk_MO.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/RK_Sand.tif",dstfile=paste(dirOut,"rk_sand.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999",srcnodata="0", tr="500 500",te="164400 4438440 602900 4789940")
    
    gdalwarp(srcfile="ImagesIn/Original/RK_Silt.tif",dstfile=paste(dirOut,"rk_silt.tif",sep="",collapse=NULL),
             t_srs='EPSG:25830', of="GTiff", ot="Float32", co="TFW=YES",
             dstnodata="-9999",srcnodata="0", tr="500 500",te="164400 4438440 602900 4789940")
    
    imagenesAModificar<-cargaDatos("ImagesIn/Original/Temp/",sk=FALSE)
    
    
    imagenesAModificar$MO<-imagenesAModificar$sk_MO
    imagenesAModificar$silt<-ifelse(test=is.na(imagenesAModificar$rk_silt),yes=imagenesAModificar$sk_silt,
                                    no=imagenesAModificar$rk_silt)
    imagenesAModificar$sand<-ifelse(test=is.na(imagenesAModificar$rk_silt),yes=imagenesAModificar$sk_sand,
                                    no=imagenesAModificar$rk_sand)
    
    
    writeGDAL(dataset=imagenesAModificar["sand"],fname="ImagesIn/Resampled/sand.tif",drivername="GTiff",type="Float32",options="TFW=YES")
    writeGDAL(dataset=imagenesAModificar["silt"],fname="ImagesIn/Resampled/silt.tif",drivername="GTiff",type="Float32",options="TFW=YES")
    writeGDAL(dataset=imagenesAModificar["MO"],fname="ImagesIn/Resampled/MO.tif",drivername="GTiff",type="Float32",options="TFW=YES")
    
    
}

vuelcaSoilProperties2DB<-function(dirDB,soilDataFrame){
    
    connExp <- dbConnect(SQLite(), dbname=dirDB)
    
    query<-'DROP TABLE IF EXISTS SOIL_PROPERTIES_CYL;'
    dbSendQuery(conn = connExp,query)
    query<-'DROP TABLE IF EXISTS Soil_Temp;'
    dbSendQuery(conn = connExp,query)
    
    dbWriteTable(connExp, "Soil_Temp", soilDataFrame)
    
    query<-'CREATE TABLE SOIL_PROPERTIES_CYL AS 
    SELECT ROUND("fila") AS FILA, 
    ROUND("columna") AS COLUMNA, 
    ROUND("coords.x1") AS COOR_X,
    ROUND("coords.x2") AS COOR_Y,
    ROUND("Clay",1) AS CLAY, 
    ROUND("Sand",1) AS SAND, 
    ROUND("Silt",1) AS LIMO,
    ROUND("MO",1) AS MO, 
    ROUND("Sat_percen_vol",1) AS SAT_VOL,
    ROUND("FC_percen_vol",1) AS FC_VOL, 
    ROUND("WP_percen_vol",1) AS WP_VOL,
    ROUND("CRAD_percen_vol",1) AS CRA_VOL, 
    ROUND("Ksat_mm_dia",1) AS KSAT_MM_DIA,
    ROUND("Soil_depth_m") AS SOIL_DEPTH_M
    FROM "Soil_Temp";'
    
    dbSendQuery(conn = connExp,query)
    
    query<-'DROP TABLE IF EXISTS Soil_Temp;'
        
    dbSendQuery(conn = connExp,query)
    dbDisconnect(connExp)
    
}
