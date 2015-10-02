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
CargaimagenesDeTextura() # Load texture interpolated maps and work with them. More info in the function

Textura<-preparaDatos() # Load information into the structure to work with
str(Textura)

########################################
## Formulas for FC, WP and Permeability
########################################


# Arena_Porc<-98
# Arcilla_Porc<-0
# MO_Porc<-0.83

# Field Capacity
Textura$CC_Prov<- -0.251*Textura$sand_fix/100+0.195*Textura$clay_fix/100+0.011*Textura$MO_fix+0.006*Textura$sand_fix/100*Textura$clay_fix/100-0.027*Textura$clay_fix/100*Textura$MO_fix+0.452*Textura$sand_fix/100*Textura$clay_fix/100+0.299
Textura$CC_Vol<-Textura$CC_Prov+(1.283*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015)

# Wilting point
Textura$PM_Prov<- -0.024*Textura$sand_fix/100+0.487*Textura$clay_fix/100+0.006*Textura$MO_fix+0.005*Textura$sand_fix/100*Textura$MO_fix-0.013*Textura$clay_fix/100*Textura$clay_fix+0.068*Textura$sand_fix/100*Textura$clay_fix/100+0.031
Textura$PM_Vol<-Textura$PM_Prov+Textura$PM_Prov*0.14-0.02 # if (PM_Vol < 0) {PM_Prov<-0.017545}

Textura$PM_Vol<-ifelse(test=PM_Vol<0,yes=1.0e-06,no=Textura$PM_Vol)


# if ((PM_Prov+PM_Prov*0.14-0.02)<0){
#     PM_Vol<-0.017545+0.017545*0.14-0.02
# } else{
#     PM_Vol<-PM_Prov+PM_Prov*0.14-0.02
# }


# Permeabilidad
Textura$Perme_1<- 0.278*Textura$sand_fix/100+0.034*Textura$clay_fix/100+0.022*Textura$MO_fix-0.018*Textura$sand_fix/100*Textura$MO_fix-0.027*Textura$clay_fix/100*Textura$MO_fix-0.584*Textura$sand_fix/100*Textura$clay_fix/100+0.078
Textura$Perme_2<- 1-(1-(Textura$Perme_1+0.636*Textura$Perme_1-0.107+Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015)-0.097*Textura$sand_fix/100+0.043))-(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))
Textura$Perme_3<- (log(Textura$CC_Prov+(1.283*Textura$CC_Prov*Textura$CC_Prov-0.374*Textura$CC_Prov-0.015))-log(Textura$PM_Vol))/(log(1500)-log(33))
Textura$Perme_mm_h<- 1930*Textura$Perme_2^(3-Textura$Perme_3)

Textura$CRAD<-Textura$CC_Vol-Textura$PM_Vol

summary(Textura$PM_Vol)
summary(Textura$CC_Vol)

writeGDAL(dataset=Textura["CRAD"],fname="Results/CRAD.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["CC_Vol"],fname="Results/FiledCapacity.tif",drivername="GTiff",type="Float32",options="TFW=YES")
writeGDAL(dataset=Textura["PM_Vol"],fname="Results/WiltingPoint.tif",drivername="GTiff",type="Float32",options="TFW=YES")



str(Textura)

#############################################




