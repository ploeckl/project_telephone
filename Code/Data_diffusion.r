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
MatInvDistSq<-MatInvDistSq[Main==TRUE,Main==TRUE]


#retain regular distance matrix
MatDist<-1/MatInvDist
MatDist[MatDist==Inf]<-0



############################################

#Create months of exchange in service
Towns$InstallMonth<-max(Towns$InstallTime)+1-Towns$InstallTime

#Calculate & interpolate lines installed by month
TeleMonth<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])
k<-max(Towns$InstallTime)
l<-dim(Towns)[1]

TeleMonth[k,]<-Towns$Lines1905                #load lines in three years
TeleMonth[k-60,]<-Towns$Lines1900
TeleMonth[k-108,]<-Towns$Lines1896

for (i in 1:k){                             #interpolate lines in between reference yeras
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
#Interpolate population by month

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
#create market access measure by month


TotalObs<-sum(Towns$InstallMonth)    #calculate number of rows necessary for month - town observations


#Matrix with months in rows and towns in columns for Telephone market access
MarketTelMatrix<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])


#Market Access with number of lines and inverse distance (^1.8)
for (i in 1:(dim(MarketTelMatrix)[1]-1)){MarketTelMatrix[i+1,]<-colSums(MatInvDistTel*TeleMonth[i,])} #use symmetry of distance matrix



#General Population Market Access (telephone selection)
#Matrix with months in rows and towns in columns for Population (Telephone-weighted) market access
MarketPopMatrix<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])

     
for (i in 1:dim(MarketPopMatrix)[1]){MarketPopMatrix[i,]<-colSums(MatInvDist*(PopMonth[i+(301-dim(MarketPopMatrix)[1]),] *(TeleMonth[i,]>0)))}          



####Total Population matrix (no selection)
MarketPopTotMatrix<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])


for (i in 1:dim(MarketPopTotMatrix)[1]){MarketPopTotMatrix[i,]<-colSums(MatInvDist*PopMonth[i+(301-dim(MarketPopTotMatrix)[1]),])}          




###################################################
#Build data set


#create matrix indicating which rows in full month matrix belonging to which town (each matrix row is one town start and end row in full marix)

Row<-matrix(0,length(Towns$InstallMonth),2)
Row[1,]<-c(1,Towns$InstallMonth[1])
for (i in 2:length(Towns$InstallMonth)){
Row[i,2]<-Row[i-1,2]+Towns$InstallMonth[i]
Row[i,1]<-Row[i-1,2]+1
}



#Select Variables

###Update###

TownsVS<-Towns[,c("Town","Bezirk","Region","PostOffice","Bahnbezirk","PostRevenues","PostRevenues_pc","Post_1900","TelegraphRevenues","TelegraphRevenues_pc","Telegraph_1900","MA_Pop_Out_1880","MA_Pop_In_1880","MA_Pop_Out_1900","MA_Pop_In_1900","MA_Post_Out_1880","MA_Post_In_1880","MA_Post_Out_1900","MA_Post_In_1900","PostBahn","RailStation","RailWeight","RailRevenues","CollectedNachnahme","PaidOutNachnahme","Nachnahme", "EmpRatio82","IndexDisSim82","Y1880","Y1900","PopShare1880","City", "Fringe","Border","StateTax", "LocalTax","Agriculture","Participation","Zentrum","Socialist","Liberal","Catholics","DifCatholicsZentrum","InstallTime","InstallMonth") ] 

     

########Combine data into matrix ###############

#prep for variables
consV<-dim(TownsVS)[2]
TownsVS<-as.data.frame(TownsVS)
maxT<-dim(MarketPopMatrix)[1]

#put covariates into matrices
VarChar<-c(1:5)   #variables in TownsVS that are character, not numeric
VarNum<-c(6:consV) 
TownsChar<-as.matrix(TownsVS[,VarChar])
TownsNum<-as.matrix(TownsVS[,VarNum])

#Prepare variables for varying+constant variables
TownsHazardChar<-matrix(0,TotalObs,length(VarChar))     #character variables
TownsHazardNum<-matrix(0,TotalObs,length(VarNum))       #numeric variables
TownsHazardPop<-matrix(0,TotalObs,1)                    #varying population
TownsHazardTime<-matrix(0,TotalObs,1)                   #time variable
TownsHazardMA<-matrix(0,TotalObs,3)                     #varying market access variable


#read values into help variables

for (i in 1:dim(Row)[1]){
for (j in Row[i,1]:Row[i,2]){

TownsHazardPop[j,]<-PopMonth[j-Row[i,1]+1+(301-maxT),i]
TownsHazardTime[j,]<-j-Row[i,1]+1

TownsHazardMA[j,1]<-MarketTelMatrix[j-Row[i,1]+1,i]
TownsHazardMA[j,2]<-MarketPopMatrix[j-Row[i,1]+1,i]
TownsHazardMA[j,3]<-MarketPopTotMatrix[j-Row[i,1]+1,i]

TownsHazardChar[j,]<-TownsChar[i,]
TownsHazardNum[j,]<-TownsNum[i,]

}
}


#consolidate varying variables and save

TownsHazardVary<-as.data.frame(cbind(TownsHazardChar,TownsHazardPop, TownsHazardTime, TownsHazardMA,TownsHazardNum))
names(TownsHazardVary)<-c(names(TownsVS)[VarChar],"Population","Time","MarketAccess_Tel","MarketAccess_Pop","MarketAccess_PopTot",names(TownsVS)[VarNum])

TownsHazardVary$Failure<-0
TownsHazardVary$Failure[Row[,2]]<-1

write.csv(TownsHazardVary,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardVary.csv")
write.dta(TownsHazardVary,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardVary.dta")

#consolidate constant variables and save


TownsHazardCons<-Towns[,c("Town","Bezirk","Region","PostOffice","Bahnbezirk","PostRevenues","PostRevenues_pc","Post_1900","TelegraphRevenues","TelegraphRevenues_pc","Telegraph_1900","MA_Pop_Out_1880","MA_Pop_In_1880","MA_Pop_Out_1900","MA_Pop_In_1900","MA_Post_Out_1880","MA_Post_In_1880","MA_Post_Out_1900","MA_Post_In_1900","PostBahn","RailStation","RailWeight","RailRevenues", "CollectedNachnahme","PaidOutNachnahme","Nachnahme", "EmpRatio82","IndexDisSim82","Y1880","Y1900","PopShare1880","City", "Fringe","Border","StateTax", "LocalTax","Agriculture","Participation","Zentrum","Socialist","Liberal","Catholics","DifCatholicsZentrum","InstallTime","InstallMonth") ] 


write.csv(TownsHazardCons,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardCons.csv") 
write.dta(TownsHazardCons,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardCons.dta") 




##### Weight Matrices for Spatial Duration Hazard


### Inverse Distance
InvDistWeight<-matrix(MatInvDistTel,nrow=dim(MatInvDistTel)[1],ncol=dim(MatInvDistTel))
InvDistWeight<-round(InvDistWeight,4)
write.dta(as.data.frame(InvDistWeight), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\WeightsDistanceTel.dta")
write.dta(as.data.frame(InvDistWeight/rowSums(InvDistWeight)), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\WeightsDistanceTelStandard.dta")


write.dta(as.data.frame(MatInvDist), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\WeightsDistance.dta")
write.dta(as.data.frame(MatInvDist/rowSums(MatInvDist)),"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\WeightsDistanceStandard.dta")



#write.dta(as.data.frame(MatInvDistSq), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSqFull.dta")
#write.dta(as.data.frame(MatInvDistSq/rowSums(MatInvDistSq)), "C:\\Research\\Telephone\\Code\\Stata\\Data\\WeightsDistanceSqFullStandard.dta")


## Distance band 

DistanceWeight<-matrix(as.integer(MatDist<50 & MatDist>0),dim(MatDist)[1],dim(MatDist)[1])
write.dta(as.data.frame(DistanceWeight), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\ProximityWeight.dta")
DistanceWeight<-DistanceWeight/rowSums(DistanceWeight)
write.dta(as.data.frame(DistanceWeight), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\ProximityWeightStandard.dta")


## Politischer Bezirk

RegionWeight<-matrix(0, length(Towns$Region),length(Towns$Region))
for (i in 1:length(Towns$Region)){
for (j in 1:length(Towns$Region)){
if (Towns$Region[i] == Towns$Region[j]){
RegionWeight[i,j]<-1
}}}
diag(RegionWeight)<-0

write.dta(as.data.frame(RegionWeight), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\RegionWeight.dta")
RegionWeigth<-RegionWeight/rowSums(RegionWeight)
write.dta(as.data.frame(RegionWeight), "C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\RegionWeightStandard.dta")




