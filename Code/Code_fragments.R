
##Analysis 1896###############################################
SpatModel1896<-as.formula(Lines1896~-1+Y1896+I(Y1896*InstallTime)+I(Y1896*MarketAccess1880)+I(Y1896*MarketSize1880)+I(Y1896^2)+I(Y1896*City)+I(Y1896*PopShare1896)+I(Y1896*Fringe)+I(Y1896*Border)+I(Y1896*Gov1896)+I(Y1896*Pub1896)+I(Y1896*Agriculture)+I(Y1896*EmpRatio95)+I(Y1896*IndexDisSim95)+I(Y1896*StateTax)+I(Y1896*LocalTax)+I(Y1896*RailStation)+I(Y1896*RailRevenues)+I(Y1896*RailWeight)+I(Y1896*PostRevenues)+I(Y1896*Participation)+I(Y1896*Zentrum)+I(Y1896*(Catholics-Zentrum))+I(Y1896*Liberal)+I(Y1896*Socialist))

SpatMatrix1896<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1896>0,Main==TRUE & Towns$Lines1896>0]) #make sure it fits
Estimation1896<-lagsarlm(SpatModel1896,data=Towns[Main==TRUE & Towns$Lines1896>0,],SpatMatrix1896,tol.solve=1.0e-24)





#SpatModel1905<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

#SpatModel1900<-as.formula(Lines1900~-1+Y1900+I(Y1900*InstallTime)+I(Y1900*MarketAccess1880)+I(Y1900*MarketSize1880)+I(Y1900^2)+I(Y1900*City)+I(Y1900*PopShare1900)+I(Y1900*Fringe)+I(Y1900*Border)+I(Y1900*Gov1900)+I(Y1900*Pub1900)+I(Y1900*Agriculture)+I(Y1900*EmpRatio95)+I(Y1900*IndexDisSim95)+I(Y1900*StateTax)+I(Y1900*LocalTax)+I(Y1900*RailStation)+I(Y1900*RailRevenues)+I(Y1900*RailWeight)+I(Y1900*PostRevenues)+I(Y1900*Participation)+I(Y1900*Zentrum)+I(Y1900*(Catholics-Zentrum))+I(Y1900*Liberal)+I(Y1900*Socialist))



#SpatModel1905small<-as.formula(Lines1905~-1+Y1905+I(Y1905*InstallTime)+I(Y1905*MarketAccess1880)+I(Y1905*MarketSize1880)+I(Y1905^2)+I(Y1905*City)+I(Y1905*PopShare1905)+I(Y1905*Fringe)+I(Y1905*Border)+I(Y1905*Gov1905)+I(Y1905*Pub1905)+I(Y1905*Agriculture)+I(Y1905*EmpRatio07)+I(Y1905*IndexDisSim07)+I(Y1905*StateTax)+I(Y1905*LocalTax)+I(Y1905*RailStation)+I(Y1905*RailRevenues)+I(Y1905*RailWeight)+I(Y1905*PostRevenues)+I(Y1905*Participation)+I(Y1905*Zentrum)+I(Y1905*(Catholics-Zentrum))+I(Y1905*Liberal)+I(Y1905*Socialist))

#SpatMatrix1905<-mat2listw(MatInvDistSq[Main==TRUE,Main==TRUE]) #make sure it fits
#SpatMatrix1900<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits
#SpatMatrix1905small<-mat2listw(MatInvDistSq[Main==TRUE & Towns$Lines1900>0,Main==TRUE & Towns$Lines1900>0]) #make sure it fits

#City PopShare1905 Fringe Border
#log(InstallTime) Gov1905 Pub1905
#EmpRatio07 IndesDisSim07 Agriculture
#StateTax LocalTax
#PostRevenues RailStation RailWeight RailRevenues Nachnahme
#Participation Socialist Liberal Zentrum (Catholics-Zentrum)





######################################################



Towns$MarketAccess1880Both<-MatInvDistSq%*%Towns$Y1880
Towns$MarketAccess1896Both<-MatInvDistSq%*%Towns$Y1896
Towns$MarketAccess1900Both<-MatInvDistSq%*%Towns$Y1900
Towns$MarketAccess1905Both<-MatInvDistSq%*%Towns$Y1905

Towns$MarketAccessLines1896Both<-MatInvDistSq%*%Towns$Lines1896
Towns$MarketAccessLines1900Both<-MatInvDistSq%*%Towns$Lines1900
Towns$MarketAccessLines1905Both<-MatInvDistSq%*%Towns$Lines1905

Towns$MarketSize1880Both<-rowSums(MatInvDistSq*Towns$Y1880)
Towns$MarketSize1896Both<-rowSums(MatInvDistSq*Towns$Y1896)
Towns$MarketSize1900Both<-rowSums(MatInvDistSq*Towns$Y1900)
Towns$MarketSize1905Both<-rowSums(MatInvDistSq*Towns$Y1905)



##################################################################

Pairs1896<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Matrix1896.csv", header=TRUE)
colnames(Pairs1896)<-c("Town1","Town2","Calls","Connection")

Pairs1896<-as.data.frame(Pairs1896)
Pairs1896$Town1<-as.character(Pairs1896$Town1)
Pairs1896$Town2<-as.character(Pairs1896$Town2)


Pairs1896$Lat1<-0
Pairs1896$Long1<-0
Pairs1896$Lat2<-0
Pairs1896$Long2<-0



for (i in 1:dim(Pairs1896)[1]){
  
  if (sum(Towns$Town==Pairs1896$Town1[i])>0){
    Pairs1896$Lat1[i]<-Towns$Latitude[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Long1[i]<-Towns$Longitude[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Participants1[i]<-Towns$Participants1896[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Lines1[i]<-Towns$Lines1896[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$MainLines1[i]<-Towns$MainLines1896[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Privat1[i]<-Towns$Privat1896[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Lines1[i]<-Towns$Lines1896[Towns$Town==Pairs1896$Town1[i]]
    Pairs1896$Size1[i]<-Towns$Y1896[Towns$Town==Pairs1896$Town1[i]]
    
  }
  
  if (sum(Towns$Town==Pairs1896$Town2[i])>0){
    Pairs1896$Lat2[i]<-Towns$Latitude[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Long2[i]<-Towns$Longitude[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Participants2[i]<-Towns$Participants1896[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Lines2[i]<-Towns$Lines1896[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$MainLines2[i]<-Towns$MainLines1896[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Privat2[i]<-Towns$Privat1896[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Lines2[i]<-Towns$Lines1896[Towns$Town==Pairs1896$Town2[i]]
    Pairs1896$Size2[i]<-Towns$Y1895[Towns$Town==Pairs1896$Town2[i]]
  }
}

Pairs1896<-Pairs1896[Pairs1896$Lat1>0 & Pairs1896$Lat2>0,]
Pairs1896$Distance<-distaz(Pairs1896$Lat1,Pairs1896$Long1,Pairs1896$Lat2,Pairs1896$Long2)[[1]]$dist

matLines1<-matrix(0,dim(Towns1896)[1],dim(Towns1896)[1])
matLines2<-matrix(0,dim(Towns1896)[1],dim(Towns1896)[1])
matDistance<-matrix(0,dim(Towns1896)[1],dim(Towns1896)[1])
matCalls<-matrix(0,dim(Towns1896)[1],dim(Towns1896)[1])

for (i in 1:dim(Towns1896)[1]){
  matLines1[i,]<-Towns1896$Lines[i]
  matLines2[,i]<-Towns1896$Lines[i]
  
  for (j in 1:dim(Towns1896)[1]){
    
    matDistance[i,j]<-distaz(Towns1896$Latitude[i],Towns1896$Longitude[i],Towns1896$Latitude[j],Towns1896$Longitude[j])$dist
    
    if (sum(Pairs1896$Town1==Towns1896$Town[i] & Pairs1896$Town2==Towns1896$Town[j])>0){
      matCalls[i,j]<-Pairs1896$Calls[Pairs1896$Town1==Towns1896$Town[i] & Pairs1896$Town2==Towns1896$Town[j]]
      matCalls[j,i]<-Pairs1896$Calls[Pairs1896$Town1==Towns1896$Town[i] & Pairs1896$Town2==Towns1896$Town[j]]
      
    }
  }
}
matCalls[matCalls<1000]<-1000
FullPairs1896<-cbind(as.vector(matLines1[upper.tri(matLines1)==TRUE]),as.vector(matLines2[upper.tri(matLines2)==TRUE]),as.vector(matDistance[upper.tri(matDistance)==TRUE]),as.vector(matCalls[upper.tri(matCalls)==TRUE]))
FullPairs1896<-data.frame(FullPairs1896)
names(FullPairs1896)<-c("Lines1","Lines2","Distance","Calls")

###############################################################################################################

Pairs1900<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\Matrix1900.csv", header=TRUE)
colnames(Pairs1900)<-c("Town1","Town2","Calls","Connection")

Pairs1900<-as.data.frame(Pairs1900)
Pairs1900$Town1<-as.character(Pairs1900$Town1)
Pairs1900$Town2<-as.character(Pairs1900$Town2)


Pairs1900$Lat1<-0
Pairs1900$Long1<-0
Pairs1900$Lat2<-0
Pairs1900$Long2<-0



for (i in 1:dim(Pairs1900)[1]){
  
  if (sum(Towns$Town==Pairs1900$Town1[i])>0){
    Pairs1900$Lat1[i]<-Towns$Latitude[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Long1[i]<-Towns$Longitude[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Participants1[i]<-Towns$Participants1900[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Lines1[i]<-Towns$Lines1900[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$MainLines1[i]<-Towns$MainLines1900[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Privat1[i]<-Towns$Privat1900[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Lines1[i]<-Towns$Lines1900[Towns$Town==Pairs1900$Town1[i]]
    Pairs1900$Size1[i]<-Towns$Y1900[Towns$Town==Pairs1900$Town1[i]]
    
  }
  
  if (sum(Towns$Town==Pairs1900$Town2[i])>0){
    Pairs1900$Lat2[i]<-Towns$Latitude[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Long2[i]<-Towns$Longitude[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Participants2[i]<-Towns$Participants1900[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Lines2[i]<-Towns$Lines1900[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$MainLines2[i]<-Towns$MainLines1900[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Privat2[i]<-Towns$Privat1900[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Lines2[i]<-Towns$Lines1900[Towns$Town==Pairs1900$Town2[i]]
    Pairs1900$Size2[i]<-Towns$Y1895[Towns$Town==Pairs1900$Town2[i]]
  }
}

Pairs1900<-Pairs1900[Pairs1900$Lat1>0 & Pairs1900$Lat2>0,]
Pairs1900$Distance<-distaz(Pairs1900$Lat1,Pairs1900$Long1,Pairs1900$Lat2,Pairs1900$Long2)[[1]]$dist

matLines1<-matrix(0,dim(Towns1900)[1],dim(Towns1900)[1])
matLines2<-matrix(0,dim(Towns1900)[1],dim(Towns1900)[1])
matDistance<-matrix(0,dim(Towns1900)[1],dim(Towns1900)[1])
matCalls<-matrix(0,dim(Towns1900)[1],dim(Towns1900)[1])

for (i in 1:dim(Towns1900)[1]){
  matLines1[i,]<-Towns1900$Lines[i]
  matLines2[,i]<-Towns1900$Lines[i]
  
  for (j in 1:dim(Towns1900)[1]){
    
    matDistance[i,j]<-distaz(Towns1900$Latitude[i],Towns1900$Longitude[i],Towns1900$Latitude[j],Towns1900$Longitude[j])$dist
    
    if (sum(Pairs1900$Town1==Towns1900$Town[i] & Pairs1900$Town2==Towns1900$Town[j])>0){
      matCalls[i,j]<-Pairs1900$Calls[Pairs1900$Town1==Towns1900$Town[i] & Pairs1900$Town2==Towns1900$Town[j]]
      matCalls[j,i]<-Pairs1900$Calls[Pairs1900$Town1==Towns1900$Town[i] & Pairs1900$Town2==Towns1900$Town[j]]
      
    }
  }
}
matCalls[matCalls<1000]<-1000
FullPairs1900<-cbind(as.vector(matLines1[upper.tri(matLines1)==TRUE]),as.vector(matLines2[upper.tri(matLines2)==TRUE]),as.vector(matDistance[upper.tri(matDistance)==TRUE]),as.vector(matCalls[upper.tri(matCalls)==TRUE]))
FullPairs1900<-data.frame(FullPairs1900)
names(FullPairs1900)<-c("Lines1","Lines2","Distance","Calls")




############################################################3
#MatMAPfalz<-MatInvDist*PfalzTowns
#MarketAccessPfalz<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])
#for (i in 1:(dim(MarketAccessPfalz)[1]-1)){MarketAccessPfalz[i+1,]<-colSums(MatMAPfalz*TeleMonth[i,])}
#MatUsedPfalz<-(t(t(MatInvDist)*PfalzTowns))
#MarketSizePfalz<-matrix(0,nrow=max(Towns$InstallTime),ncol=dim(Towns)[1])
#for (i in 1:dim(MarketSizePfalz)[1]){MarketSizePfalz[i,]<-colSums(t(MatUsedPfalz*PopMonth[i+(301-dim(MarketSizePfalz)[1]),]) *(TeleMonth[i,]>0))}          #Consider replacing telemonth indicator with telemonth amount


TownsHazardMA[j,2]<-MarketAccessPfalz[j-Row[i,1]+1,i]
TownsHazardMA[j,4]<-MarketSizePfalz[j-Row[i,1]+1,i]


