rm(list = ls(all = TRUE))
library(spatialreg)
library(spdep)
source('C:\\Box\\Research\\Telephone\\project_telephone\\Code\\Code_outreg.r')


##Read in Data sets
Towns<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv", header=TRUE) 

MatInvDist<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDist.csv", header=TRUE, row.names = 1) 
MatInvDistSq<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistSq.csv", header=TRUE, row.names = 1) 
MatInvDistTel<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistTel.csv", header=TRUE, row.names = 1) 


##clean data
MatInvDist<-as.matrix(MatInvDist)  #confirm data in matrix form
MatInvDistSq<-as.matrix(MatInvDistSq)  #confirm data in matrix form
MatInvDistTel<-as.matrix(MatInvDistTel)  #confirm data in matrix form

Main<-Towns$Region!='PF'               #remove Pfalz from analysis    


#rescale population to make coefficients readable
Towns$Y1905<-Towns$Y1905/1000
Towns$Y1900<-Towns$Y1900/1000
Towns$Y1896<-Towns$Y1896/1000


##Analysis 1905###############################################
#SpatModel1905<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

SpatModel1905<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MA_Pop_Out_1880)+I(Y1905*MA_Pop_In_1880)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues/Y1880)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

#+I(Y1905*MA_Post_Out_1880)+I(Y1905*MA_Post_In_1880)
#+I(Y1905*MA_Pop_Out_1880)+I(Y1905*MA_Pop_In_1880)


#SpatMatrix1905<-mat2listw(MatInvDistSq[Main==TRUE,Main==TRUE]) #make sure it fits
SpatMatrix1905<-mat2listw(MatInvDistTel[Main==TRUE,Main==TRUE]) #make sure it fits


Estimation1905<-spatialreg::lagsarlm(SpatModel1905,data=Towns[Main==TRUE,],SpatMatrix1905,tol.solve=1.0e-24)


##Analysis 1900###############################################
#SpatModel1900<-as.formula(Lines1900~-1+Y1900+I(Y1900*InstallTime)+I(Y1900*MarketAccess1880)+I(Y1900*MarketSize1880)+I(Y1900^2)+I(Y1900*City)+I(Y1900*PopShare1900)+I(Y1900*Fringe)+I(Y1900*Border)+I(Y1900*Gov1900)+I(Y1900*Pub1900)+I(Y1900*Agriculture)+I(Y1900*EmpRatio95)+I(Y1900*IndexDisSim95)+I(Y1900*StateTax)+I(Y1900*LocalTax)+I(Y1900*RailStation)+I(Y1900*RailRevenues)+I(Y1900*RailWeight)+I(Y1900*PostRevenues)+I(Y1900*Participation)+I(Y1900*Zentrum)+I(Y1900*(Catholics-Zentrum))+I(Y1900*Liberal)+I(Y1900*Socialist))

SpatModel1900<-as.formula(Lines1900~-1+Y1900+I(Y1900*InstallTime)+I(Y1900*MA_Pop_Out_1880)+I(Y1900*MA_Pop_In_1880)+I(Y1900^2)+I(Y1900*PostRevenues_pc)+I(Y1900*TelegraphRevenues/Y1880)+I(Y1900*City)+I(Y1900*PopShare1900)+I(Y1900*Fringe)+I(Y1900*Border)+I(Y1900*Gov1900)+I(Y1900*Pub1900)+I(Y1900*Agriculture)+I(Y1900*EmpRatio95)+I(Y1900*IndexDisSim95)+I(Y1900*StateTax)+I(Y1900*LocalTax)+I(Y1900*RailStation)+I(Y1900*RailRevenues)+I(Y1900*RailWeight)+I(Y1900*Participation)+I(Y1900*Zentrum)+I(Y1900*(Catholics-Zentrum))+I(Y1900*Liberal)+I(Y1900*Socialist))



#SpatMatrix1900<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits
SpatMatrix1900<-mat2listw(MatInvDistTel[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits


Estimation1900<-lagsarlm(SpatModel1900,data=Towns[Main==TRUE & Towns$Lines1900>0,],SpatMatrix1900,tol.solve=1.0e-24)


##Analysis 1905# with 1900 set######################

#SpatModel1905small<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

SpatModel1905small<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MA_Pop_Out_1880)+I(Y1905*MA_Pop_In_1880)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues/Y1880)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))


#SpatMatrix1905small<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits

SpatMatrix1905small<-mat2listw(MatInvDistTel[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits



Estimation1905small<-lagsarlm(SpatModel1905small,data=Towns[Main==TRUE & Towns$Lines1900>0,],SpatMatrix1905small,tol.solve=1.0e-24)






################
#City PopShare1905 Fringe Border
#log(InstallTime) Gov1905 Pub1905
#EmpRatio07 IndesDisSim07 Agriculture
#StateTax LocalTax
#PostRevenues RailStation RailWeight RailRevenues Nachnahme
#Participation Socialist Liberal Zentrum (Catholics-Zentrum)

Shares<-(0.4936*(MatInvDistSq[Main==TRUE,Main==TRUE]%*%Towns$Lines1905[Main==TRUE]))/Towns$Lines1905[Main==TRUE]


