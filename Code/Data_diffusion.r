rm(list = ls(all = TRUE))               
library(timereg)
library("foreign")

##Read in Data sets
Towns<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv", header=TRUE) 

MatInvDist<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDist.csv", header=TRUE, row.names = 1) 
MatInvDistSq<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistSq.csv", header=TRUE, row.names = 1) 
MatInvDistTel<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistTel.csv", header=TRUE, row.names = 1) 


##clean data
MatInvDist<-as.matrix(MatInvDist)  #confirm data in matrix form
MatInvDistSq<-as.matrix(MatInvDistSq)  #confirm data in matrix form
MatInvDistTel<-as.matrix(MatInvDistTel)  #confirm data in matrix form


##remove Pfalz from analysis 
Main<-Towns$Region!='PF'                 
Towns<-Towns[Main==TRUE,]
MatInvDistTel<-MatInvDistTel[Main==TRUE,Main==TRUE]
MatInvDist<-MatInvDist[Main==TRUE,Main==TRUE]
MatInvDistSq<-MatInvDistSQ[Main==TRUE,Main==TRUE]

############################################
Towns$InstallMonth<-max(Towns$InstallTime)+1-Towns$InstallTime

TeleMonth<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])
k<-max(Towns$InstallTime)
l<-dim(Towns)[1]
TeleMonth[k,]<-Towns$Lines1905
TeleMonth[k-60,]<-Towns$Lines1900
TeleMonth[k-108,]<-Towns$Lines1896

for (i in 1:k){
for (j in 1:l){
if (Towns$InstallMonth[j]<=i){

if (i < k-108){TeleMonth[i,j]<-(Towns$Lines1896[j]/((k-108)-Towns$InstallMonth[j]+1))*(i-Towns$InstallMonth[j]+1)}


if (i > k-108 & i < k-60){

if (Towns$Lines1896[j]==0){
TeleMonth[i,j]<-(Towns$Lines1900[j]/((k-60)-Towns$InstallMonth[j]+1))*(i-Towns$InstallMonth[j]+1)
}
else {TeleMonth[i,j]<-Towns$Lines1896[j]+ ((Towns$Lines1900[j]-Towns$Lines1896[j])/48)*(i-(k-108)) }
}

if (i > k-60){

if (Towns$Lines1900[j]==0){
TeleMonth[i,j]<-(Towns$Lines1905[j]/((k-Towns$InstallMonth[j]+1)))*(i-Towns$InstallMonth[j]+1)
}
else {TeleMonth[i,j]<-Towns$Lines1900[j]+ ((Towns$Lines1905[j]-Towns$Lines1900[j])/60)*(i-(k-60)) }
}

}
}
}

##########################################################################
PopMonth<-matrix(0,nrow=301,ncol=dim(Towns)[1])
PopMonth[1,]<-Towns$Y1880
PopMonth[61,]<-Towns$Y1885
for (i in 2:60){PopMonth[i,]<-Towns$Y1880+((Towns$Y1885-Towns$Y1880)/60)*(i-1)}

PopMonth[121,]<-Towns$Y1890
for (i in 62:120){PopMonth[i,]<-Towns$Y1885+((Towns$Y1890-Towns$Y1885)/60)*(i-61)}

PopMonth[181,]<-Towns$Y1895
for (i in 122:180){PopMonth[i,]<-Towns$Y1890+((Towns$Y1895-Towns$Y1890)/60)*(i-121)}

PopMonth[241,]<-Towns$Y1900
for (i in 182:240){PopMonth[i,]<-Towns$Y1895+((Towns$Y1900-Towns$Y1895)/60)*(i-181)}

PopMonth[301,]<-Towns$Y1905
for (i in 242:300){PopMonth[i,]<-Towns$Y1900+((Towns$Y1905-Towns$Y1900)/60)*(i-241)}


###############################################################################

TotalObs<-sum(Towns$InstallMonth)

#InstallMatrix<-TeleMonth>0
#MatInvDistSq

#Matrix with months in rows and towns in columns
MarketAccessMatrix<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])

MatMAMain<-MatInvDist*MainTowns

for (i in 1:(dim(MarketAccessMatrix)[1]-1)){MarketAccessMatrix[i+1,]<-colSums(MatMAMain*TeleMonth[i,])}  #Only MarketAccess in Main State



MatUsedMain<-(t(t(MatInvDist)*MainTowns))

MarketSizeMatrix<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])

for (i in 1:dim(MarketSizeMatrix)[1]){MarketSizeMatrix[i,]<-colSums(t(MatUsedMain*PopMonth[i+(301-dim(MarketSizeMatrix)[1]),]) *(TeleMonth[i,]>0))}          #Consider replacing telemonth indicator with telemonth amount



Row<-matrix(0,length(Towns$InstallMonth),2)
Row[1,]<-c(1,Towns$InstallMonth[1])
for (i in 2:length(Towns$InstallMonth)){
Row[i,2]<-Row[i-1,2]+Towns$InstallMonth[i]
Row[i,1]<-Row[i-1,2]+1
}

TownsVS<-Towns[,c("Town","Bezirk","Region","PostOffice","Bahnbezirk","PostBahn","MarketAccess1880","MarketSize1880","MarketDistance1880","PostRevenues","RailStation","RailWeight","RailRevenues", "CollectedNachnahme","PaidOutNachnahme","Nachnahme", "EmpRatio82","IndexDisSim82","Y1880","PopShare1880","City", "Fringe","Border","StateTax", "LocalTax","Agriculture","Participation","Zentrum","Socialist","Liberal","Catholics","DifCatholicsZentrum","InstallTime","InstallMonth") ] 

     

consV<-dim(TownsVS)[2]
TownsVS<-as.data.frame(TownsVS)
maxT<-dim(MarketSizeMatrix)[1]
 

VarChar<-c(1:5)
VarNum<-c(6:34) 
TownsChar<-as.matrix(TownsVS[,VarChar])
TownsNum<-as.matrix(TownsVS[,VarNum])

TownsHazardChar<-matrix(0,TotalObs,length(VarChar))
TownsHazardNum<-matrix(0,TotalObs,length(VarNum))
TownsHazardPop<-matrix(0,TotalObs,1)
TownsHazardTime<-matrix(0,TotalObs,1)
TownsHazardMA<-matrix(0,TotalObs,4)


for (i in 1:dim(Row)[1]){
for (j in Row[i,1]:Row[i,2]){

TownsHazardPop[j,]<-PopMonth[j-Row[i,1]+1+(301-maxT),i]
TownsHazardTime[j,]<-j-Row[i,1]+1

TownsHazardMA[j,1]<-MarketAccessMatrix[j-Row[i,1]+1,i]
TownsHazardMA[j,3]<-MarketSizeMatrix[j-Row[i,1]+1,i]

TownsHazardChar[j,]<-TownsChar[i,]
TownsHazardNum[j,]<-TownsNum[i,]

}
}

#TownsHazard$Time<-Time1


TownsHazardVary<-as.data.frame(cbind(TownsHazardChar,TownsHazardPop, TownsHazardTime, TownsHazardMA,TownsHazardNum))
names(TownsHazardVary)<-c(names(TownsVS)[VarChar],"Population","Time","MarketAccess","MarketAccessPfalz","MarketSize","MarketSizePfalz",names(TownsVS)[VarNum])

TownsHazardVary$Failure<-0
TownsHazardVary$Failure[Row[,2]]<-1

write.csv(TownsHazardVary,"C:\\Research\\Telephone\\Code\\Stata\\Data\\TownsHazardVary.csv")
write.dta(TownsHazardVary,"C:\\Research\\Telephone\\Code\\Stata\\Data\\TownsHazardVary.dta")

TownsHazardCons<-Towns[,c("Town","Bezirk","Region","PostOffice","Bahnbezirk","PostBahn","PostRevenues","RailStation","RailRevenues","RailWeight","EmpRatio82","IndexDisSim82","Y1880","MarketAccess1880Both","MarketAccess1880","MarketSize1880","MarketDistance1880","PopShare1880","City", "Fringe","Border","StateTax", "LocalTax","Agriculture","Participation","Zentrum", "Socialist","Liberal","Catholics","DifCatholicsZentrum","InstallTime","InstallMonth") ]

write.csv(TownsHazardCons,"C:\\Research\\Telephone\\Code\\Stata\\Data\\TownsHazardCons.csv") 
write.dta(TownsHazardCons,"C:\\Research\\Telephone\\Code\\Stata\\Data\\TownsHazardCons.dta") 


##### Weight Matrices for Spatial Duration Hazard


### Inverse Distance
write.dta(as.data.frame(MatInvDistSq), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSq.dta")
write.dta(as.data.frame(MatInvDistSq/rowSums(MatInvDistSq)), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSqStandard.dta")


#write.dta(as.data.frame(MatInvDistSq), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSqFull.dta")
#write.dta(as.data.frame(MatInvDistSq/rowSums(MatInvDistSq)), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSqFullStandard.dta")


## Distance 

DistanceWeight<-matrix(as.integer(MatDist<50 & MatDist>0),dim(MatDist)[1],dim(MatDist)[1])
write.dta(as.data.frame(DistanceWeight), "C:\\Research\\Telephone\\Code\\Stata\\Data\\ProximityWeight.dta")
DistanceWeight<-DistanceWeight/rowSums(DistanceWeight)
write.dta(as.data.frame(DistanceWeight), "C:\\Research\\Telephone\\Code\\Stata\\Data\\ProximityWeightStandard.dta")


## Politischer Bezirk

RegionWeight<-matrix(0, length(Towns$Region),length(Towns$Region))
for (i in 1:length(Towns$Region)){
for (j in 1:length(Towns$Region)){
if (Towns$Region[i] == Towns$Region[j]){
RegionWeight[i,j]<-1
}}}
diag(RegionWeight)<-0

write.dta(as.data.frame(RegionWeight), "C:\\Research\\Telephone\\Code\\Stata\\Data\\RegionWeight.dta")
RegionWeigth<-RegionWeight/rowSums(RegionWeight)
write.dta(as.data.frame(RegionWeight), "C:\\Research\\Telephone\\Code\\Stata\\Data\\RegionWeightStandard.dta")



#############################
###########################
#Plot presentation

Cumulative<-mat.or.vec(max(Towns$InstallMonth),2)
Cumulative[,1]<-seq(1,max(Towns$InstallMonth),1)
for (i in 1: dim(Cumulative)[1]){
Cumulative[i,2]<-sum(Towns$InstallMonth[Main==TRUE]<=i)
}
plot(Cumulative, type="l", ylab="Number of Local Exchanges", xlab="")


Penetration<-as.matrix(Towns[,58:60])
Penetration[Penetration==0]<-NA
matplot(c(1896,1900,1905),t(Penetration),col="BLACK",ylab="Phone Lines per Capita", xlab="Year",type="p",pch=19, lty=3)
