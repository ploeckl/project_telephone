library("foreign")

#Load Bezirke Gewerbe Information ############################################################################
MF<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeMF.csv", header=FALSE)
NB<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeNB.csv", header=FALSE)
OB<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeOB.csv", header=FALSE)
OF<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeOF.csv",header=FALSE)
OP<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeOP.csv", header=FALSE)
PF<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbePF.csv", header=FALSE)
SC<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeSC.csv", header=FALSE)
UF<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\GewerbeUF.csv", header=FALSE)

Gewerbe1882Raw<-cbind(MF[,seq(4,dim(MF)[2],3)],NB[,seq(4,dim(NB)[2],3)],OB[,seq(4,dim(OB)[2],3)],OF[,seq(4,dim(OF)[2],3)],OP[,seq(4,dim(OP)[2],3)],PF[,seq(4,dim(PF)[2],3)],SC[,seq(4,dim(SC)[2],3)],UF[,seq(4,dim(UF)[2],3)])
names1882<-as.character(as.data.frame(t(Gewerbe1882Raw))[,1])
Gewerbe1882<-matrix(0,nrow=dim(Gewerbe1882Raw)[1]-2,ncol=dim(Gewerbe1882Raw)[2])
for (i in 3:25){
Gewerbe1882[i-2,]<-as.integer(as.character(as.data.frame(t(Gewerbe1882Raw))[,i]))
}
Gewerbe1882<-as.data.frame(Gewerbe1882)
names(Gewerbe1882)<-names1882
Gewerbe1882[10,]<-Gewerbe1882[10,]+Gewerbe1882[11,]
Gewerbe1882[14,]<-Gewerbe1882[14,]+Gewerbe1882[15,]
Gewerbe1882<-Gewerbe1882[c(1:10,12:14,16:dim(Gewerbe1882)[1]),]


Gewerbe1895Raw<-cbind(MF[,seq(3,dim(MF)[2],3)],NB[,seq(3,dim(NB)[2],3)],OB[,seq(3,dim(OB)[2],3)],OF[,seq(3,dim(OF)[2],3)],OP[,seq(3,dim(OP)[2],3)],PF[,seq(3,dim(PF)[2],3)],SC[,seq(3,dim(SC)[2],3)],UF[,seq(3,dim(UF)[2],3)])
names1895<-as.character(as.data.frame(t(Gewerbe1895Raw))[,1])
Gewerbe1895<-matrix(0,nrow=dim(Gewerbe1895Raw)[1]-2,ncol=dim(Gewerbe1895Raw)[2])
for (i in 3:25){
Gewerbe1895[i-2,]<-as.integer(as.character(as.data.frame(t(Gewerbe1895Raw))[,i]))
}
Gewerbe1895<-as.data.frame(Gewerbe1895)
names(Gewerbe1895)<-names1895
Gewerbe1895[10,]<-Gewerbe1895[10,]+Gewerbe1895[11,]
Gewerbe1895[14,]<-Gewerbe1895[14,]+Gewerbe1895[15,]
Gewerbe1895<-Gewerbe1895[c(1:10,12:14,16:dim(Gewerbe1895)[1]),]

Gewerbe1907Raw<-cbind(MF[,seq(2,dim(MF)[2],3)],NB[,seq(2,dim(NB)[2],3)],OB[,seq(2,dim(OB)[2],3)],OF[,seq(2,dim(OF)[2],3)],OP[,seq(2,dim(OP)[2],3)],PF[,seq(2,dim(PF)[2],3)],SC[,seq(2,dim(SC)[2],3)],UF[,seq(2,dim(UF)[2],3)])
names1907<-as.character(as.data.frame(t(Gewerbe1907Raw))[,1])
Gewerbe1907<-matrix(0,nrow=dim(Gewerbe1907Raw)[1]-2,ncol=dim(Gewerbe1907Raw)[2])
for (i in 3:25){
Gewerbe1907[i-2,]<-as.integer(as.character(as.data.frame(t(Gewerbe1907Raw))[,i]))
}
Gewerbe1907<-as.data.frame(Gewerbe1907)
names(Gewerbe1907)<-names1907
Gewerbe1907[10,]<-Gewerbe1907[10,]+Gewerbe1907[11,]
Gewerbe1907[14,]<-Gewerbe1907[14,]+Gewerbe1907[15,]
Gewerbe1907<-Gewerbe1907[c(1:10,12:14,16:dim(Gewerbe1907)[1]),]
                                              
#Load BezirkPopulation#########################################################################
BezirkPopulation<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\BezirkPopulation.csv", header=TRUE)
BezirkPopulation<-as.data.frame(BezirkPopulation)
BezirkPopulation<-BezirkPopulation[BezirkPopulation$BezirkNr>0,]

BezirkList<-unique(BezirkPopulation$Bezirk)

Bezirke<-as.data.frame(BezirkList)
names(Bezirke)<-"Bezirk"

funBezirkNr<-function(x){mean(BezirkPopulation[BezirkPopulation$Bezirk==x,"BezirkNr"])}
Bezirke$BezirkNr<-sapply(BezirkList,funBezirkNr)

funBezirkWK<-function(x){mean(BezirkPopulation[BezirkPopulation$Bezirk==x,"Wahlkreis"])}
Bezirke$Wahlkreis<-sapply(BezirkList,funBezirkWK)

funBezirkPop<-function(x,y){sum(BezirkPopulation[BezirkPopulation$Bezirk==x,y])}
Bezirke$Y1840<-sapply(BezirkList,funBezirkPop,y="Y1840")
Bezirke$Y1875<-sapply(BezirkList,funBezirkPop,y="Y1875")
Bezirke$Y1880<-sapply(BezirkList,funBezirkPop,y="Y1880")
Bezirke$Y1885<-sapply(BezirkList,funBezirkPop,y="Y1885")
Bezirke$Y1890<-sapply(BezirkList,funBezirkPop,y="Y1890")
Bezirke$Y1895<-sapply(BezirkList,funBezirkPop,y="Y1895")
Bezirke$Y1900<-sapply(BezirkList,funBezirkPop,y="Y1900")
Bezirke$Y1905<-sapply(BezirkList,funBezirkPop,y="Y1905")
Bezirke$Y1910<-sapply(BezirkList,funBezirkPop,y="Y1910")
Bezirke$StateTax<-sapply(BezirkList,funBezirkPop,y="StateTax")
Bezirke$LocalTax<-sapply(BezirkList,funBezirkPop,y="LocalTax")

RegionNames<-c("MF","NB","OB","OF","OP","PF","SC","UF")
funBezirkReg<-function(x){RegionNames[max(as.integer(as.ordered(BezirkPopulation[,"Region"]))[BezirkPopulation$Bezirk==x])]}
Bezirke$Region<-sapply(BezirkList,funBezirkReg)

funBezirkCity<-function(x){sum(BezirkPopulation[BezirkPopulation$Bezirk==x,"City"])}
Bezirke$City<-sapply(BezirkList,funBezirkCity)
Bezirke$City<-as.numeric(Bezirke$City>0)
funBezirkFringe<-function(x){sum(BezirkPopulation[BezirkPopulation$Bezirk==x,"Fringe"])}
Bezirke$Fringe<-sapply(BezirkList,funBezirkFringe)
Bezirke$Fringe<-as.numeric(Bezirke$Fringe>0)
funBezirkBorder<-function(x){sum(BezirkPopulation[BezirkPopulation$Bezirk==x,"Border"])}
Bezirke$Border<-sapply(BezirkList,funBezirkBorder)
Bezirke$Border<-as.numeric(Bezirke$Border>0)


#Merge Bezirk Population & Gewerbe##########################################################################


funGewerbe82Sum<-function(x,y){sum(Gewerbe1882[y,as.character(x)])}
funGewerbe95Sum<-function(x,y){sum(Gewerbe1895[y,as.character(x)])}
funGewerbe07Sum<-function(x,y){sum(Gewerbe1907[y,as.character(x)])}

Bezirke$TotalEmp1882<-sapply(Bezirke$Bezirk,funGewerbe82Sum,y=c(1:dim(Gewerbe1882)[1]))
Bezirke$TotalEmp1895<-sapply(Bezirke$Bezirk,funGewerbe95Sum,y=c(1:dim(Gewerbe1895)[1]))
Bezirke$TotalEmp1907<-sapply(Bezirke$Bezirk,funGewerbe07Sum,y=c(1:dim(Gewerbe1907)[1]))

Bezirke$AEmp1882<-sapply(Bezirke$Bezirk,funGewerbe82Sum,y=c(1:2))
Bezirke$AEmp1895<-sapply(Bezirke$Bezirk,funGewerbe95Sum,y=c(1:2))
Bezirke$AEmp1907<-sapply(Bezirke$Bezirk,funGewerbe07Sum,y=c(1:2))
Bezirke$BEmp1882<-sapply(Bezirke$Bezirk,funGewerbe82Sum,y=c(3:16))
Bezirke$BEmp1895<-sapply(Bezirke$Bezirk,funGewerbe95Sum,y=c(3:16))
Bezirke$BEmp1907<-sapply(Bezirke$Bezirk,funGewerbe07Sum,y=c(3:16))
Bezirke$CEmp1882<-sapply(Bezirke$Bezirk,funGewerbe82Sum,y=c(17:21))
Bezirke$CEmp1895<-sapply(Bezirke$Bezirk,funGewerbe95Sum,y=c(17:21))
Bezirke$CEmp1907<-sapply(Bezirke$Bezirk,funGewerbe07Sum,y=c(17:21))

BavariaTotalEmp1882<-rowSums(Gewerbe1882)
BavariaTotalEmp1895<-rowSums(Gewerbe1895)
BavariaTotalEmp1907<-rowSums(Gewerbe1907)

IndexDisSim<-function(x,y,z){0.5*sum(abs((z[,x]/sum(z[,x]))-(y/sum(y)))) }
Bezirke$IndexDisSim82<-sapply(Bezirke$BezirkNr,IndexDisSim,y=BavariaTotalEmp1882,z=Gewerbe1882)      #Not sure about this, check whether BezirkNr really picks the right column
Bezirke$IndexDisSim95<-sapply(Bezirke$BezirkNr,IndexDisSim,y=BavariaTotalEmp1895,z=Gewerbe1895) 
Bezirke$IndexDisSim07<-sapply(Bezirke$BezirkNr,IndexDisSim,y=BavariaTotalEmp1907,z=Gewerbe1907) 



#Merge Bezirk & Wahlkreisee##########################################################################


Wahlkreise<-read.csv("C:\\Box\\Research\\Telephone\\Data\\CSV\\Wahlkreise.csv", header=TRUE)
Wahlkreise<-as.data.frame(Wahlkreise)
KreisListe<-Bezirke$Wahlkreis

Bezirke<-as.data.frame(Bezirke)
funBezirkWKinfo<-function(x,y){Wahlkreise[Wahlkreise$Wahlkreis==x,y]}
Bezirke$LW<-sapply(KreisListe,funBezirkWKinfo,y="LW")
Bezirke$IG<-sapply(KreisListe,funBezirkWKinfo,y="IG")
Bezirke$HD<-sapply(KreisListe,funBezirkWKinfo,y="HD")
Bezirke$Catholics<-sapply(KreisListe,funBezirkWKinfo,y="Catholics")
Bezirke$Participation<-sapply(KreisListe,funBezirkWKinfo,y="Participation")
Bezirke$Freisinnige<-sapply(KreisListe,funBezirkWKinfo,y="DF")
Bezirke$DVP<-sapply(KreisListe,funBezirkWKinfo,y="DVP")
Bezirke$Konservative<-sapply(KreisListe,funBezirkWKinfo,y="K")
Bezirke$Nationalliberal<-sapply(KreisListe,funBezirkWKinfo,y="NL")
Bezirke$Reichspartei<-sapply(KreisListe,funBezirkWKinfo,y="RP")
Bezirke$Socialist<-sapply(KreisListe,funBezirkWKinfo,y="S")
Bezirke$Zentrum<-sapply(KreisListe,funBezirkWKinfo,y="Z")
Bezirke$OtherCandidates<-sapply(KreisListe,funBezirkWKinfo,y="ub")
Bezirke$Seat<-sapply(KreisListe,funBezirkWKinfo,y="Seat")

