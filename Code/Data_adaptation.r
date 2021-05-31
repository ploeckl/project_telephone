
#########setup

                                
library("foreign")
library(quantreg)
library(GEOmap)
library(gmt)
rm(list = ls(all = TRUE))

#Source Gewerbe Information############################################################################

source('C:\\Box\\Research\\Telephone\\project_telephone\\Code\\Dataprep_gewerbe.r')



#Load Towns Population Information############################################################################

Population<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Population.csv", header=TRUE)
 
Population<-as.data.frame(Population)

colnames(Population)<-c("Town","County","BezirkNR","Bezirk","Y1840","Y1875","Y1880","Y1885","Y1890","Y1895","Y1900","Y1905","Y1910","Region","Longitude","Latitude","NNID","NameGNU","PostOffice","PostRevenues","PostBahn","Bahnbezirk","RankRailWeight","TotalRailWeight","SentRailWeight","RankRailRevenues","RailRevenues","CollectedNachnahme","PaidOutNachnahme","TelegraphRevenues","Post_1900","Telegraph_1900")
Population$Town<-as.character(Population$Town)
Population$Bezirk<-as.character(Population$Bezirk)


#Load Time since Exchange opening############################################################################ 

Install<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Anlagen.csv", header=TRUE)

Install<-as.data.frame(Install)
 Install$Town<-as.character(Install$Town)

Install$M1896<-(1896-Install$Year)*12+(12-Install$Month)+1
Install$M1900<-(1900-Install$Year)*12+(12-Install$Month)+1
Install$M1905<-(1905-Install$Year)*12+(12-Install$Month)+1

#Create Town Characteristics 1896############################################################################ 

Towns1896<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Towns1896.csv", header=TRUE)
 Towns1896<-as.data.frame(Towns1896)
 colnames(Towns1896)<-c("Town","Latitude","Longitude","Privat","Government","Public","Participants","MainLines","LocalCallsPrivate","LocalCallsPublic")
Towns1896$Town<-as.character(Towns1896$Town)
Towns1896$Lines<-Towns1896$Privat+Towns1896$Government+Towns1896$Public
Towns1896$Region<-"Missing"
Towns1896$Population<-0
Towns1896$Population1880<-0
Towns1896$Install<-0

Towns1896$LocalCalls<-Towns1896$LocalCallsPublic+Towns1896$LocalCallsPrivate

for (i in 1:dim(Towns1896)[1]){
Towns1896$Population[i]<-Population$Y1895[Population$Town==Towns1896$Town[i]]+0.2*(Population$Y1900[Population$Town==Towns1896$Town[i]]-Population$Y1895[Population$Town==Towns1896$Town[i]])
Towns1896$Population1880[i]<-Population$Y1880[Population$Town==Towns1896$Town[i]]
Towns1896$Install[i]<-Install$M1896[Install$Town==Towns1896$Town[i]]
Towns1896$Region[i]<-Population$Region[Population$Town==Towns1896$Town[i]]
}

Towns1896$Penetration<-Towns1896$MainLines/Towns1896$Population

                                               
 #Create Town Characteristics 1900###################################################

Towns1900<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Towns1900.csv", header=TRUE)
 Towns1900<-as.data.frame(Towns1900)
 colnames(Towns1900)<-c("Town","Latitude","Longitude","Privat","Government","Public","Participants","MainLines", "LocalCallsBausch", "LocalCallsBasic","LocalCallsPublic","DistanceCallsPrivat","DistanceCallsGovernment","DistanceCallsPublic","DistanceCallsPrivatFees")
Towns1900$Lines<-Towns1900$Privat+Towns1900$Government+Towns1900$Public
Towns1900$Region<-'Missing'
Towns1900$Population<-0
Towns1900$Install<-0


Towns1900$LocalCalls<-Towns1900$LocalCallsBausch+Towns1900$LocalCallsBasic + Towns1900$LocalCallsPublic
Towns1900$DistanceCalls<-Towns1900$DistanceCallsPrivat +Towns1900$DistanceCallsGovernment + Towns1900$DistanceCallsPublic
Towns1900$TotalCalls<-Towns1900$DistanceCalls +Towns1900$LocalCalls


for (i in 1:dim(Towns1900)[1]){
Towns1900$Population[i]<-Population$Y1900[Population$Town==Towns1900$Town[i]]
Towns1900$Population1880[i]<-Population$Y1880[Population$Town==Towns1900$Town[i]]
Towns1900$Install[i]<-Install$M1900[Install$Town==Towns1900$Town[i]]
Towns1900$Region[i]<-Population$Region[Population$Town==Towns1900$Town[i]]
}

Towns1900$Penetration<-Towns1900$MainLines/Towns1900$Population

#Create Town Characteristics 1905############################################################################


Towns1905<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Towns1905.csv", header=TRUE)
 Towns1905<-as.data.frame(Towns1905)
colnames(Towns1905)<-c("Town","Privat","Government","Public","Lines","Participants","MainLines")

Towns1905$Town<-as.character(Towns1905$Town)
Towns1905$Lines<-Towns1905$Privat+Towns1905$Government+Towns1905$Public
Towns1905$Region<-0
Towns1905$Population<-0
Towns1905$Install<-0

for (i in 1:dim(Towns1905)[1]){
Towns1905$Population[i]<-Population$Y1905[Population$Town==Towns1905$Town[i]]
Towns1905$Population1880[i]<-Population$Y1880[Population$Town==Towns1905$Town[i]]
Towns1905$Install[i]<-Install$M1905[Install$Town==Towns1905$Town[i]]
Towns1905$Region[i]<-Population$Region[Population$Town==Towns1905$Town[i]]
}

Towns1905$Penetration<-Towns1905$MainLines/Towns1905$Population


#################################################  Towns Master

#c("Town","County","Y1840","Y1875","Y1880","Y1885","Y1890","Y1895","Y1900","Y1905","Y1910","Bezirk")
# Install time, Install 1896,1900, Y1896, Lines1896,1900,1905, Penetration1896,1900,195  , Government/Public 1896/1900/1905



Towns<-Population
# Test   drop NA 
Towns<-Towns[is.na(Towns$Y1840)==FALSE,]
Towns<-Towns[is.na(Towns$Y1890)==FALSE,]


Towns$Y1896<-Towns$Y1895+0.2*(Towns$Y1900-Towns$Y1895)
Ivalue<-function(x){Install$M1905[Install$Town==x]}
Towns$InstallTime<- sapply(Towns$Town,Ivalue)
Towns$Install1896<-Towns$InstallTime>108
Towns$Install1900<-Towns$InstallTime>60

NrML96<-function(x){Towns1896$MainLines[Towns1896$Town==x]}
Towns$MainLines1896<-as.numeric(sapply(Towns$Town,NrML96))
NrML00<-function(x){Towns1900$MainLines[Towns1900$Town==x]}
Towns$MainLines1900<-as.numeric(sapply(Towns$Town,NrML00))
NrML05<-function(x){Towns1905$MainLines[Towns1905$Town==x]}
Towns$MainLines1905<-as.numeric(sapply(Towns$Town,NrML05))

NrL96<-function(x){Towns1896$Lines[Towns1896$Town==x]}
Towns$Lines1896<-as.numeric(sapply(Towns$Town,NrL96))
NrL00<-function(x){Towns1900$Lines[Towns1900$Town==x]}
Towns$Lines1900<-as.numeric(sapply(Towns$Town,NrL00))
NrL05<-function(x){Towns1905$Lines[Towns1905$Town==x]}
Towns$Lines1905<-as.numeric(sapply(Towns$Town,NrL05))

Gov96<-function(x){Towns1896$Government[Towns1896$Town==x]}
Towns$Government1896<-as.numeric(sapply(Towns$Town,Gov96))
Gov00<-function(x){Towns1900$Government[Towns1900$Town==x]}
Towns$Government1900<-as.numeric(sapply(Towns$Town,Gov00))
Gov05<-function(x){Towns1905$Government[Towns1905$Town==x]}
Towns$Government1905<-as.numeric(sapply(Towns$Town,Gov05))

Pub96<-function(x){Towns1896$Public[Towns1896$Town==x]}
Towns$Public1896<-as.numeric(sapply(Towns$Town,Pub96))
Pub00<-function(x){Towns1900$Public[Towns1900$Town==x]}
Towns$Public1900<-as.numeric(sapply(Towns$Town,Pub00))
Pub05<-function(x){Towns1905$Public[Towns1905$Town==x]}
Towns$Public1905<-as.numeric(sapply(Towns$Town,Pub05))


Pri96<-function(x){Towns1896$Privat[Towns1896$Town==x]}
Towns$Privat1896<-as.numeric(sapply(Towns$Town,Pri96))
Pri00<-function(x){Towns1900$Privat[Towns1900$Town==x]}
Towns$Privat1900<-as.numeric(sapply(Towns$Town,Pri00))
Pri05<-function(x){Towns1905$Privat[Towns1905$Town==x]}
Towns$Privat1905<-as.numeric(sapply(Towns$Town,Pri05))

Part96<-function(x){Towns1896$Participants[Towns1896$Town==x]}
Towns$Participants1896<-as.numeric(sapply(Towns$Town,Part96))
Part00<-function(x){Towns1900$Participants[Towns1900$Town==x]}
Towns$Participants1900<-as.numeric(sapply(Towns$Town,Part00))
Part05<-function(x){Towns1905$Participants[Towns1905$Town==x]}
Towns$Participants1905<-as.numeric(sapply(Towns$Town,Part05))

Towns[Towns$Install1896==FALSE,c("MainLines1896","Government1896","Public1896","Privat1896")]<-0
Towns[Towns$Install1900==FALSE,c("MainLines1900","Government1900","Public1900", "Privat1900")]<-0
#Towns[Towns$Install1905==FALSE,c("MainLines1905","Government1905","Public1905","Privat1905")]<-0

Towns[is.na(Towns)==TRUE]<-0




# Correct Konradsreuth, Ismaning,Pasing (merging into Hof / Munich / Munich)
Added<-c("Y1840","Y1875", "Y1880", "Y1885","Y1890","Y1895", "Y1900", "Y1905", "Y1910","PostRevenues","TotalRailWeight","SentRailWeight","RailRevenues", "CollectedNachnahme","PaidOutNachnahme","TelegraphRevenues", "Post_1900", "Telegraph_1900", "Y1896","Install1896","Install1900",      "MainLines1896","MainLines1900","MainLines1905", "Lines1896", "Lines1900","Lines1905" , "Government1896",  "Government1900" ,    "Government1905" , "Public1896",  "Public1900","Public1905","Privat1896", "Privat1900" ,"Privat1905",  "Participants1896", "Participants1900", "Participants1905")

 Towns[Towns$Town=="Muenchen",c(Added)]<- Towns[Towns$Town=="Muenchen",c(Added)]+ Towns[Towns$Town=="Pasing",c(Added)] + Towns[Towns$Town=="Ismaning",c(Added)]


 Towns[Towns$Town=="Hof",c(Added)]<- Towns[Towns$Town=="Hof",c(Added)]+ Towns[Towns$Town=="Konradsreuth",c(Added)]


 Towns<-Towns[Towns$Town != 'Pasing',]
 Towns<-Towns[Towns$Town != 'Ismaning',]
 Towns<-Towns[Towns$Town != 'Konradsreuth',]
 


Towns[Towns$Public1905>Towns$MainLines1905,"MainLines1905"]<-1

Towns$Gov1896<-Towns$Government1896/Towns$Lines1896
Towns$Gov1900<-Towns$Government1900/Towns$Lines1900
Towns$Gov1905<-Towns$Government1905/Towns$Lines1905
Towns[is.na(Towns)==TRUE]<-0

Towns$Pub1896<-Towns$Public1896/Towns$Lines1896
Towns$Pub1900<-Towns$Public1900/Towns$Lines1900
Towns$Pub1905<-Towns$Public1905/Towns$Lines1905  
Towns[is.na(Towns)==TRUE]<-0

Towns$Penetration1896<-Towns$MainLines1896/Towns$Y1896
Towns$Penetration1900<-Towns$MainLines1900/Towns$Y1900
Towns$Penetration1905<-Towns$MainLines1905/Towns$Y1905

####################### Nr of Calls
funTowns1896<-function(x,y){Towns1896[Towns1896$Town==x,y]}
Towns$LocalCalls1896<-as.numeric(sapply(Towns$Town,funTowns1896,y="LocalCalls"))

funTowns1900<-function(x,y){Towns1900[Towns1900$Town==x,y]}
Towns$LocalCalls1900<-as.numeric(sapply(Towns$Town,funTowns1900,y="LocalCalls"))
Towns$DistanceCalls1900<-as.numeric(sapply(Towns$Town,funTowns1900,y="DistanceCalls"))



 
############Economic Characteristics


funBezirk<-function(x,y){Bezirke[Bezirke$Bezirk==x,y]}
Towns$EmpRatio82<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="TotalEmp1882"))/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1880"))
Towns$EmpRatio95<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="TotalEmp1895"))/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1895"))
Towns$EmpRatio07<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="TotalEmp1907"))/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1905"))

Towns$IndexDisSim82<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="IndexDisSim82"))
Towns$IndexDisSim95<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="IndexDisSim95"))
Towns$IndexDisSim07<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="IndexDisSim07"))


Towns$PopShare1880<-Towns$Y1880/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1880"))
Towns$PopShare1896<-Towns$Y1895/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1895"))
Towns$PopShare1900<-Towns$Y1900/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1900"))
Towns$PopShare1905<-Towns$Y1905/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1905"))
Towns$City<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="City"))
Towns$Fringe<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Fringe"))
Towns$Border<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Border"))
Towns$StateTax<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="StateTax"))/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1885"))
Towns$LocalTax<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="LocalTax"))/as.numeric(sapply(Towns$Bezirk,funBezirk,y="Y1885"))


Towns$PopShare1880[Towns$PopShare1880>1]<-1
Towns$PopShare1896[Towns$PopShare1896>1]<-1
Towns$PopShare1900[Towns$PopShare1900>1]<-1
Towns$PopShare1905[Towns$PopShare1905>1]<-1

Towns$Agriculture<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="LW"))
Towns$Catholics<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Catholics"))
Towns$Participation<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Participation"))
Towns$Zentrum<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Zentrum"))
Towns$Socialist<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Socialist"))
Towns$Liberal<-as.numeric(sapply(Towns$Bezirk,funBezirk,y="Freisinnige"))+as.numeric(sapply(Towns$Bezirk,funBezirk,y="DVP"))+as.numeric(sapply(Towns$Bezirk,funBezirk,y="Nationalliberal"))
Towns$DifCatholicsZentrum<-Towns$Catholics-Towns$Zentrum
Towns[is.na(Towns)==TRUE]<-0
Towns<-Towns[Towns$BezirkNR>0,]



#rescale population to make coefficients readable
Towns$Y1905<-Towns$Y1905/1000
Towns$Y1900<-Towns$Y1900/1000
Towns$Y1896<-Towns$Y1896/1000
Towns$Y1895<-Towns$Y1895/1000
Towns$Y1890<-Towns$Y1890/1000
Towns$Y1885<-Towns$Y1885/1000
Towns$Y1880<-Towns$Y1880/1000
Towns$Y1875<-Towns$Y1875/1000
Towns$Y1840<-Towns$Y1840/1000


### Rescaling

Towns$Agriculture<-Towns$Agriculture/100
Towns$Participation<-Towns$Participation/100
Towns$Socialist<-Towns$Socialist/100
Towns$Liberal<-Towns$Liberal/100
Towns$Zentrum<-Towns$Zentrum/100
Towns$DifCatholicsZentrum<-Towns$DifCatholicsZentrum/100
Towns$Catholics<-Towns$Catholics/100

##################Verkehrsanstalten#######################

#RankRailWeight
#RankRailRevenues

#Turn variables into Per Capita values
Towns$PostRevenues_pc<-Towns$PostRevenues/(Towns$Y1880*1000)
Towns$TelegraphRevenues_pc<-Towns$TelegraphRevenues/(Towns$Y1880*1000)

Towns$Nachnahme<-(Towns$CollectedNachnahme-Towns$PaidOutNachnahme)/Towns$Y1880
Towns$RailStation<-as.integer(Towns$RankRailRevenues>0)
Towns$RailRevenues<-(1/1000000)*(Towns$RailRevenues/Towns$Y1880-Towns$Nachnahme)
Towns$RailWeight<-(Towns$SentRailWeight-(Towns$TotalRailWeight-Towns$SentRailWeight))/Towns$Y1880


################################################## Market Accesss


MatDist<-matrix(0,nrow=dim(Towns)[1],ncol=dim(Towns)[1])


for (i in 1:dim(Towns)[1]){
for (j in 1:dim(Towns)[1]){
MatDist[i,j]<-distaz(Towns$Latitude[i],Towns$Longitude[i],Towns$Latitude[j],Towns$Longitude[j])$dist
}
}

MatInvDist<-1/MatDist
diag(MatInvDist)<-0

MatDistSq<-MatDist^2
MatInvDistSq<-1/MatDistSq
diag(MatInvDistSq)<-0

MatDistTel<-MatDist^(1.8)
MatInvDistTel<-1/MatDistTel
diag(MatInvDistTel)<-0


MainTowns<-Towns$Region != 'PF'

#Towns$MarketAccess1880<-MatInvDistSq%*%(Towns$Y1880*MainTowns)
#Towns$MarketAccess1896<-MatInvDistSq%*%(Towns$Y1896*MainTowns)
#Towns$MarketAccess1900<-MatInvDistSq%*%(Towns$Y1900*MainTowns)
#Towns$MarketAccess1905<-MatInvDistSq%*%(Towns$Y1905*MainTowns)

Towns$MA_Pop_Out_1880<-MatInvDist%*%(Towns$Y1880*MainTowns)
Towns$MA_Pop_Out_1896<-MatInvDist%*%(Towns$Y1896*MainTowns)
Towns$MA_Pop_Out_1900<-MatInvDist%*%(Towns$Y1900*MainTowns)
Towns$MA_Pop_Out_1905<-MatInvDist%*%(Towns$Y1905*MainTowns)

Towns$MA_Post_Out_1880<-MatInvDistTel%*%(Towns$PostRevenues*MainTowns)*(1/1000)
Towns$MA_Post_Out_1900<-MatInvDistTel%*%(Towns$Post_1900*MainTowns)*(1/1000)


#Towns$MarketSize1880<-rowSums(MatInvDistSq*(Towns$Y1880*MainTowns))
#Towns$MarketSize1896<-rowSums(MatInvDistSq*(Towns$Y1896*MainTowns))
#Towns$MarketSize1900<-rowSums(MatInvDistSq*(Towns$Y1900*MainTowns))
#Towns$MarketSize1905<-rowSums(MatInvDistSq*(Towns$Y1905*MainTowns))


#Use inbound variables for robustness

#Towns$MA_Pop_In_1880<-rowSums(MatInvDist*(Towns$Y1880*MainTowns))
#Towns$MA_Pop_In_1896<-rowSums(MatInvDist*(Towns$Y1896*MainTowns))
#Towns$MA_Pop_In_1900<-rowSums(MatInvDist*(Towns$Y1900*MainTowns))
#Towns$MA_Pop_In_1905<-rowSums(MatInvDist*(Towns$Y1905*MainTowns)

Towns$MA_Pop_In_1880<-MatInvDistTel%*%(Towns$Y1880*MainTowns)
Towns$MA_Pop_In_1896<-MatInvDistTel%*%(Towns$Y1896*MainTowns)
Towns$MA_Pop_In_1900<-MatInvDistTel%*%(Towns$Y1900*MainTowns)
Towns$MA_Pop_In_1905<-MatInvDistTel%*%(Towns$Y1905*MainTowns)



#Towns$MA_Post_In_1880<-rowSums(MatInvDistTel*(Towns$PostRevenues*MainTowns*(1/1000)))
#Towns$MA_Post_In_1900<-rowSums(MatInvDistTel*(Towns$Post_1900*MainTowns*(1/1000)))

Towns$MA_Post_In_1880<-MatInvDist%*%(Towns$PostRevenues*MainTowns)*(1/1000)
Towns$MA_Post_In_1900<-MatInvDist%*%(Towns$Post_1900*MainTowns)*(1/1000)




#Towns$MarketAccessLines1896<-MatInvDistSq%*%(Towns$Lines1896*MainTowns)
#Towns$MarketAccessLines1900<-MatInvDistSq%*%(Towns$Lines1900*MainTowns)
#Towns$MarketAccessLines1905<-MatInvDistSq%*%(Towns$Lines1905*MainTowns)

#Towns$MarketDistance1880<-MatDist%*%MainTowns



###############################################################################################




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







###Save Data files############################################################################################


write.csv(Towns,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv")
write.csv(MatInvDist,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDist.csv") 
write.csv(MatInvDistSq,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistSq.csv") 
write.csv(MatInvDistTel,"C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistTel.csv") 
