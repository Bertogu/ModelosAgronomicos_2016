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

Arena_Porc<-98
Arcilla_Porc<-0
MO_Porc<-0.83

# Field Capacity
CC_Prov<- -0.251*Arena_Porc/100+0.195*Arcilla_Porc/100+0.011*MO_Porc+0.006*Arena_Porc/100*Arcilla_Porc/100-0.027*Arcilla_Porc/100*MO_Porc+0.452*Arena_Porc/100*Arcilla_Porc/100+0.299
CC_Vol<-CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015)

# Wilting point
PM_Prov<- -0.024*Arena_Porc/100+0.487*Arcilla_Porc/100+0.006*MO_Porc+0.005*Arena_Porc/100*MO_Porc-0.013*Arcilla_Porc/100*MO_Porc+0.068*Arena_Porc/100*Arcilla_Porc/100+0.031
#PM_Vol<-PM_Prov+PM_Prov*0.14-0.02 # if (PM_Vol < 0) {PM_Prov<-0.017545} 

if ((PM_Prov+PM_Prov*0.14-0.02)<0){
    PM_Vol<-0.017545+0.017545*0.14-0.02
} else{
    PM_Vol<-PM_Prov+PM_Prov*0.14-0.02
}
    

# Permeabilidad
Perme_1<- 0.278*Arena_Porc/100+0.034*Arcilla_Porc/100+0.022*MO_Porc-0.018*Arena_Porc/100*MO_Porc-0.027*Arcilla_Porc/100*MO_Porc-0.584*Arena_Porc/100*Arcilla_Porc/100+0.078
Perme_2<- 1-(1-(Perme_1+0.636*Perme_1-0.107+CC_Prov+(1.283 *CC_Prov*CC_Prov-0.374*CC_Prov-0.015)-0.097*Arena_Porc/100+0.043))-(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))
Perme_3<- (log(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))-log(PM_Vol))/(log(1500)-log(33))

Perme_mm_h<- 1930*Perme_2^(3-Perme_3)

#############################################




