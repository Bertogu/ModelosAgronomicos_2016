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
AjustaimagenesDeTextura() # Load texture interpolated maps and work with them. More info in the function

Textura<-cargaDatos() # Load information into the structure to work with
str(Textura)

########################################
## Formulas for FC, WP and Permeability
########################################


# Arena_Porc<-98
# Arcilla_Porc<-0
# MO_Porc<-0.83

# Field Capacity
Textura$CC_Prov<- -0.251*Textura$sand/100+0.195*Textura$clay/100+0.011*Textura$MO+0.006*Textura$sand/100*Textura$clay/100-0.027*Textura$clay/100*Textura$MO+0.452*Textura$sand/100*Textura$clay/100+0.299
Textura$CC_Vol<-Textura$CC_Prov+(1.283*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015)

# Wilting point
Textura$PM_Prov<- -0.024*Textura$sand/100+0.487*Textura$clay/100+0.006*Textura$MO+0.005*Textura$sand/100*Textura$MO-0.013*Textura$clay/100*Textura$clay+0.068*Textura$sand/100*Textura$clay/100+0.031
Textura$PM_Vol<-Textura$PM_Prov+Textura$PM_Prov*0.14-0.02 # if (PM_Vol < 0) {PM_Prov<-0.017545}

Textura$PM_Vol<-ifelse(test=Textura$PM_Vol<0,yes=1.0e-06,no=Textura$PM_Vol)


# if ((PM_Prov+PM_Prov*0.14-0.02)<0){
#     PM_Vol<-0.017545+0.017545*0.14-0.02
# } else{
#     PM_Vol<-PM_Prov+PM_Prov*0.14-0.02
# }




Perme_1<- 0.278*sand/100+0.034*clay/100+0.022*MO-0.018*sand/100*MO-0.027*clay/100*MO-0.584*sand/100*clay/100+0.078
Perme_2<- 1-(1-(Perme_1+0.636*Perme_1-0.107+CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015)-0.097*sand/100+0.043))-(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))
Perme_3<- (log(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))-log(PM_Vol))/(log(1500)-log(33))
Perme_mm_dia<- (1930*Perme_2^(3-Perme_3))*24










# Permeability
Textura$Perme_1<- 0.278*Textura$sand/100+0.034*Textura$clay/100+0.022*Textura$MO-0.018*Textura$sand/100*Textura$MO-0.027*Textura$clay/100*Textura$MO-0.584*Textura$sand/100*Textura$clay/100+0.078
Textura$Perme_2<- 1-(1-(Textura$Perme_1+0.636*Textura$Perme_1-0.107+Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015)-0.097*Textura$sand/100+0.043))-(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))
Textura$Perme_3<- (log(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))-log(Textura$PM_Vol))/(log(1500)-log(33))
Textura$Perme_mm_dia<- (1930*Textura$Perme_2^(3-Textura$Perme_3))*24


# Water retention capabilitie
# This parameter is not necesary for AquaCrop but for mapping (el mundo paper)
Textura$CRAD<-Textura$CC_Vol-Textura$PM_Vol

# Saturation


Textura$sand_fix
Textura$clay_fix
Textura$MO_fix


sand<-27.24/100
clay<-51.94/100
mo<-1.6968

aa52<-0.278*sand+0.034*clay+0.022*mo-0.018*sand*mo-0.027*clay*mo-0.584*sand*clay+0.078
y52<--0.251*sand+0.195*clay+0.011*mo+0.006*sand*mo-0.027*clay*mo+0.452*sand*clay+0.299
z52<-y52+(1.283*y52*y52-0.374*y52-0.015)
ab52<-aa52+(0.636*aa52-0.107)
ad52<--0.097*sand+0.043
ac52<-ab52+z52
ae52<-ac52+ad52
af52<-(1-ae52)*2.65
ag52<-af52*1
ah52<-1-(ag52/2.65)
sat<-ah52*100



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


Textura$sand_fix
Textura$clay_fix
Textura$silt_fix
Textura$MO_fix
Textura$CC_Vol
Textura$PM_Vol
Textura$Perme_mm_dia
Textura$CRAD
Textura$CC_Prov
summary(Textura$sat)



str(Textura)
summary(Textura$CC_Vol)


writeGDAL(dataset=Textura["CC_Vol"],fname="ImagesOut/FiledCapacity_percent.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["CC_Prov"],fname="ImagesOut/FiledCapacity_percent.tif",drivername="GTiff",type="Float32",options="TFW=YES")

writeGDAL(dataset=Textura["PM_Vol"],fname="ImagesOut/WiltingPoint.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["Perme_mm_dia"],fname="ImagesOut/Ksat_mm_dia.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["CRAD"],fname="ImagesOut/CRAD.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["sat"],fname="ImagesOut/Sat_volumen.tif",drivername="GTiff",type="Float32",options="TFW=YES")

writeGDAL(dataset=Textura["sand"],fname="ImagesOut/Sand.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["silt"],fname="ImagesOut/Silt.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["clay"],fname="ImagesOut/Clay.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["MO"],fname="ImagesOut/MO.tif",drivername="GTiff",type="Float32",options="TFW=YES")



str(Textura)

#############################################




