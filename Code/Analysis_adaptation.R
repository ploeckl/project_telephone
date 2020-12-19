rm(list = ls(all = TRUE))
library(spatialreg)
library(spdep)
library(texreg)

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





##Analysis 1905###############################################

SpatMatrix1905<-mat2listw(MatInvDistTel) 



#Market access only
SpatModel1905_MA<-as.formula(Lines1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime))


Estimation1905_MA<-spatialreg::lagsarlm(SpatModel1905_MA,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)




#Market Access + Econ
SpatModel1905_EC<-as.formula(Lines1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)+I(Y1905*Agriculture) +I(Y1905*EmpRatio07) +I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax))#+I(Y1905*LocalTax))

Estimation1905_EC<-spatialreg::lagsarlm(SpatModel1905_EC,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)


#Market Access + Econ + Politics
SpatModel1905_PO<-as.formula(Lines1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime) +I(Y1905*Agriculture) +I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum)))#

Estimation1905_PO<-spatialreg::lagsarlm(SpatModel1905_PO,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)



#Full Model
SpatModel1905_AD<-as.formula(Lines1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border)+ I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)+I(Y1905*Agriculture) +I(Y1905*EmpRatio07) +I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*City)+I(Y1905*Fringe)+I(Y1905*PopShare1905)) #

Estimation1905<-spatialreg::lagsarlm(SpatModel1905_AD,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)






##Analysis 1900###############################################

#Modify Install time variable to take into account 1905 vs 1900
Towns$InstallTime1900<-Towns$InstallTime-60


SpatModel1900<-as.formula(Lines1900~-1+Y1900+I(Y1900*MA_Post_Out_1880)+I(Y1900*Border) + I(Y1900^2)+I(Y1900*PostRevenues_pc)+I(Y1900*TelegraphRevenues_pc)+I(Y1900*InstallTime1900)  +I(Y1900*Agriculture)+I(Y1900*EmpRatio95)+I(Y1900*IndexDisSim95)+I(Y1900*RailStation)+I(Y1900*RailRevenues)+I(Y1900*StateTax)+I(Y1900*Participation)+I(Y1900*Socialist)+I(Y1900*Zentrum)+I(Y1900*(Catholics-Zentrum))+I(Y1900*City)+I(Y1900*PopShare1900)+I(Y1900*Fringe))



SpatMatrix1900<-mat2listw(MatInvDistTel[Towns$Lines1900>0,Towns$Lines1900>0]) 


Estimation1900<-spatialreg::lagsarlm(SpatModel1900,data=Towns[Towns$Lines1900>0,],SpatMatrix1900,tol.solve=1.0e-24)


##Analysis 1905# with 1900 set######################


SpatModel1905small<-as.formula(Lines1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border) + I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)  +I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe))

SpatMatrix1905small<-mat2listw(MatInvDistTel[Towns$Lines1900>0,Towns$Lines1900>0]) #make sure it fits



Estimation1905small<-spatialreg::lagsarlm(SpatModel1905small,data=Towns[Towns$Lines1900>0,],SpatMatrix1905small,tol.solve=1.0e-24)



##Analysis by line type###############################################


##Private###############################################

SpatModel1905privat<-as.formula(Privat1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border) + I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)  +I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe))


Estimation1905privat<-spatialreg::lagsarlm(SpatModel1905privat,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)


##gov###############################################

SpatModel1905gov<-as.formula(Government1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border) + I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)  +I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe))


Estimation1905gov<-spatialreg::lagsarlm(SpatModel1905gov,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)


##public###############################################

SpatModel1905public<-as.formula(Public1905~-1+Y1905+I(Y1905*MA_Pop_Out_1880)+I(Y1905*Border) + I(Y1905^2)+I(Y1905*PostRevenues_pc)+I(Y1905*TelegraphRevenues_pc)+I(Y1905*InstallTime)  +I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*StateTax)+I(Y1905*Participation)+I(Y1905*Socialist)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe))


Estimation1905public<-spatialreg::lagsarlm(SpatModel1905public,data=Towns,SpatMatrix1905,tol.solve=1.0e-24)

#Estimation1905publicsmall<-spatialreg::lagsarlm(SpatModel1905public,data=Towns[Towns$Lines1900>0,],SpatMatrix1905small,tol.solve=1.0e-24)



#####################

IndEffect1905<-impacts(Estimation1905, listw=SpatMatrix1905)


####################
ExtLines<- 0.29 * (MatInvDistTel%*%Towns$Lines1905)
Shares<- ExtLines/Towns$Lines1905
Shares[Shares>1]<-1


##########################################3
texreg(list(Estimation1905_MA, Estimation1905_EC,Estimation1905_PO,Estimation1905))

