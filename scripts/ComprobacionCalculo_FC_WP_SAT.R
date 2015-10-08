
sand<-44.18
clay<-26.01
MO<-1.22


# Field Capacity

CC_Prov<--0.251*sand/100+0.195*clay/100+0.011*MO+0.006*sand/100*MO-0.027*clay/100*MO+0.452*sand/100*clay/100+0.299
CC_Vol<-CC_Prov+(1.283* CC_Prov*CC_Prov-0.374* CC_Prov-0.015)


# Wilting point
PM_Prov<- -0.024*sand/100+0.487*clay/100+0.006*MO+0.005*sand/100*MO-0.013*clay/100*MO+0.068*sand/100*clay/100+0.031
PM_Vol<-PM_Prov+PM_Prov*0.14-0.02 # if (PM_Vol < 0) {PM_Prov<-0.017545}
PM_Vol<-ifelse(test=PM_Vol<0,yes=1.0e-06,no=PM_Vol)



Perme_1<- 0.278*sand/100+0.034*clay/100+0.022*MO-0.018*sand/100*MO-0.027*clay/100*MO-0.584*sand/100*clay/100+0.078
Perme_2 <- 1-(1-(Perme_1+0.636*Perme_1-0.107+CC_Prov+(1.283*CC_Prov*CC_Prov -0.374*CC_Prov-0.015)-0.097*sand/100+0.043))-(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))
Perme_3<-(log(CC_Prov+(1.283*CC_Prov*CC_Prov-0.374*CC_Prov-0.015))-log(PM_Prov+0.14*PM_Prov-0.02))/(log(1500)-log(33))
Perme_mm_h<- (1930*Perme_2^(3-Perme_3))


aa52<-0.278*(sand/100)+0.034*(clay/100)+0.022*MO-0.018*(sand/100)*MO-0.027*(clay/100)*MO-0.584*(sand/100)*(clay/100)+0.078
y52<--0.251*(sand/100)+0.195*(clay/100)+0.011*MO+0.006*(sand/100)*MO-0.027*(clay/100)*MO+0.452*(sand/100)*(clay/100)+0.299
z52<-y52+(1.283*y52*y52-0.374*y52-0.015)
ab52<-aa52+(0.636*aa52-0.107)
ad52<--0.097*(sand/100)+0.043
ac52<-ab52+z52
ae52<-ac52+ad52
af52<-(1-ae52)*2.65
ag52<-af52*1
ah52<-1-(ag52/2.65)
sat<-ah52*100
sat
