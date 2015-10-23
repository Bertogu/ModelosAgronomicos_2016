###############################################################
############### Clean WS and load libraries ###################
###############################################################

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

### Carga las funciones 
source("scripts/funcionesComunes.R")
# this function must only be run when new texture images are used
 # Load texture interpolated maps and work with them. More info in the function


## Sólo Simple kriging
AjustaimagenesDeTextura()
Textura<-cargaDatos(sk=TRUE) # Load information into the structure to work with
str(Textura)

### con Regresion Kriging
AjustaimagenesDeTexturaConRregressionKriging()
Textura<-cargaDatos(sk=TRUE)


########################################
## Formulas for FC, WP and Permeability
########################################

# Field Capacity
Textura$CC_Prov<--0.251*Textura$sand/100+0.195*Textura$clay/100+0.011*Textura$MO+0.006*Textura$sand/100*Textura$MO-0.027*Textura$clay/100*Textura$MO+0.452*Textura$sand/100*Textura$clay/100+0.299
Textura$CC_Vol<-Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015)

# Wilting point
Textura$PM_Prov<- -0.024*Textura$sand/100+0.487*Textura$clay/100+0.006*Textura$MO+0.005*Textura$sand/100*Textura$MO-0.013*Textura$clay/100*Textura$MO+0.068*Textura$sand/100*Textura$clay/100+0.031
Textura$PM_Vol<-Textura$PM_Prov+Textura$PM_Prov*0.14-0.02 # if (PM_Vol < 0) {PM_Prov<-0.017545}

Textura$PM_Prov<-ifelse(test=Textura$PM_Vol<0,yes=0.017545,no=Textura$PM_Prov)
Textura$PM_Vol<-ifelse(test=Textura$PM_Vol<0,yes=1.0e-06,no=Textura$PM_Vol) # if (PM_Vol < 0) {PM_Prov<-0.017545}

Textura$Perme_1<- 0.278*Textura$sand/100+0.034*Textura$clay/100+0.022*Textura$MO-0.018*Textura$sand/100*Textura$MO-0.027*Textura$clay/100*Textura$MO-0.584*Textura$sand/100*Textura$clay/100+0.078
Textura$Perme_2 <- 1-(1-(Textura$Perme_1+0.636*Textura$Perme_1-0.107+Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov -0.374*Textura$CC_Prov-0.015)-0.097*Textura$sand/100+0.043))-(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))
Textura$Perme_3<-(log(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))-log(Textura$PM_Prov+0.14*Textura$PM_Prov-0.02))/(log(1500)-log(33))
Textura$Perme_mm_dia<- (1930*Textura$Perme_2^(3-Textura$Perme_3))*24

Textura$CC_Vol<-100*Textura$CC_Vol
Textura$PM_Vol<-100*Textura$PM_Vol



# Water retention capabilitie
# This parameter is not necesary for AquaCrop but for mapping (el mundo paper)
Textura$CRAD<-Textura$CC_Vol-Textura$PM_Vol

# Saturation
Textura$aa52<-0.278*(Textura$sand/100)+0.034*(Textura$clay/100)+0.022*Textura$MO-0.018*(Textura$sand/100)*Textura$MO-0.027*(Textura$clay/100)*Textura$MO-0.584*(Textura$sand/100)*(Textura$clay/100)+0.078
Textura$y52=-0.251*(Textura$sand/100)+0.195*(Textura$clay/100)+0.011*Textura$MO+0.006*(Textura$sand/100)*Textura$MO-0.027*(Textura$clay/100)*Textura$MO+0.452*(Textura$sand/100)*(Textura$clay/100)+0.299
Textura$z52=Textura$y52+(1.283*Textura$y52*Textura$y52-0.374*Textura$y52-0.015)
Textura$ab52=Textura$aa52+(0.636*Textura$aa52-0.107)
Textura$ad52=-0.097*(Textura$sand/100)+0.043
Textura$ac52=Textura$ab52+Textura$z52
Textura$ae52=Textura$ac52+Textura$ad52
Textura$af52=(1-Textura$ae52)*2.65
Textura$ag52=Textura$af52*1
Textura$ah52=1-(Textura$ag52/2.65)
Textura$sat=Textura$ah52*100
str(Textura)

# Export water content
writeGDAL(dataset=Textura["CC_Vol"],fname="ImagesOut/FC_percen_vol.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["PM_Vol"],fname="ImagesOut/WP_percen_vol.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["sat"],fname="ImagesOut/Sat_percen_vol.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["Perme_mm_dia"],fname="ImagesOut/Ksat_mm_dia.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["CRAD"],fname="ImagesOut/CRAD_percen_vol.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["Soil_depth"],fname="ImagesOut/Soil_depth_m.tif",drivername="GTiff",type="Float32",options="TFW=YES")


# Export Texture
writeGDAL(dataset=Textura["sand"],fname="ImagesOut/Sand.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["silt"],fname="ImagesOut/Silt.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["clay"],fname="ImagesOut/Clay.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["MO"],fname="ImagesOut/MO.tif",drivername="GTiff",type="Float32",options="TFW=YES")


#############################################

















