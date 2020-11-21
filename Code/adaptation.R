rm(list = ls(all = TRUE))
library(spdep)
source('C:\\Box\\Research\\Code\\Routines\\outreg.r')



Towns<-read.csv("C:\\Box\\Research\\Telephone\\Data\\Input\\Towns.csv", header=TRUE) 
MatInvDistSq<-read.csv("C:\\Box\\Research\\Telephone\\Data\\Input\\MatInvDistSq.csv", header=TRUE, row.names = 1) 
#source('C:\\Box\\Research\\Telephone\\Code\\Data.r')
MatInvDistSq<-as.matrix(MatInvDistSq)



Main<-Towns$Region!='PF'

#rescale population to make coefficients readable

Towns$Y1905<-Towns$Y1905/1000
Towns$Y1900<-Towns$Y1900/1000
Towns$Y1896<-Towns$Y1896/1000

##Analysis 1905###############################################
SpatModel1905<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

SpatMatrix1905<-mat2listw(MatInvDistSq[Main==TRUE,Main==TRUE]) #make sure it fits
Estimation1905<-lagsarlm(SpatModel1905,data=Towns[Main==TRUE,],SpatMatrix1905,tol.solve=1.0e-24)

##Analysis 1900###############################################
SpatModel1900<-as.formula(Lines1900~-1+Y1900+I(Y1900*InstallTime)+I(Y1900*MarketAccess1880)+I(Y1900*MarketSize1880)+I(Y1900^2)+I(Y1900*City)+I(Y1900*PopShare1900)+I(Y1900*Fringe)+I(Y1900*Border)+I(Y1900*Gov1900)+I(Y1900*Pub1900)+I(Y1900*Agriculture)+I(Y1900*EmpRatio95)+I(Y1900*IndexDisSim95)+I(Y1900*StateTax)+I(Y1900*LocalTax)+I(Y1900*RailStation)+I(Y1900*RailRevenues)+I(Y1900*RailWeight)+I(Y1900*PostRevenues)+I(Y1900*Participation)+I(Y1900*Zentrum)+I(Y1900*(Catholics-Zentrum))+I(Y1900*Liberal)+I(Y1900*Socialist))


SpatMatrix1900<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits
Estimation1900<-lagsarlm(SpatModel1900,data=Towns[Main==TRUE & Towns$Lines1900>0,],SpatMatrix1900,tol.solve=1.0e-24)


##Analysis 1905# with 1900 set######################

SpatModel1905small<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

SpatMatrix1905small<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits
Estimation1905small<-lagsarlm(SpatModel1905small,data=Towns[Main==TRUE & Towns$Lines1900>0,],SpatMatrix1905small,tol.solve=1.0e-24)





##Analysis 1896###############################################
SpatModel1896<-as.formula(Lines1896~-1+Y1896+I(Y1896*InstallTime)+I(Y1896*MarketAccess1880)+I(Y1896*MarketSize1880)+I(Y1896^2)+I(Y1896*City)+I(Y1896*PopShare1896)+I(Y1896*Fringe)+I(Y1896*Border)+I(Y1896*Gov1896)+I(Y1896*Pub1896)+I(Y1896*Agriculture)+I(Y1896*EmpRatio95)+I(Y1896*IndexDisSim95)+I(Y1896*StateTax)+I(Y1896*LocalTax)+I(Y1896*RailStation)+I(Y1896*RailRevenues)+I(Y1896*RailWeight)+I(Y1896*PostRevenues)+I(Y1896*Participation)+I(Y1896*Zentrum)+I(Y1896*(Catholics-Zentrum))+I(Y1896*Liberal)+I(Y1896*Socialist))

SpatMatrix1896<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1896>0,Main==TRUE & Towns$Lines1896>0]) #make sure it fits
Estimation1896<-lagsarlm(SpatModel1896,data=Towns[Main==TRUE & Towns$Lines1896>0,],SpatMatrix1896,tol.solve=1.0e-24)


################
#City PopShare1905 Fringe Border
#log(InstallTime) Gov1905 Pub1905
#EmpRatio07 IndesDisSim07 Agriculture
#StateTax LocalTax
#PostRevenues RailStation RailWeight RailRevenues Nachnahme
#Participation Socialist Liberal Zentrum (Catholics-Zentrum)

Shares<-(0.4936*(MatInvDistSq[Main==TRUE,Main==TRUE]%*%Towns$Lines1905[Main==TRUE]))/Towns$Lines1905[Main==TRUE]


