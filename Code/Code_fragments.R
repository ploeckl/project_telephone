
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

