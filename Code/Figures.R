library("ggplot2")
library("tidyverse")
#############################



##Read in Data sets
Countries<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Input\\AdoptionInternational.csv", header=TRUE) 

Countries<- tibble(Countries )

int<-ggplot(data=Countries, mapping = aes(x = Year, y = Rate, group = Country))
int2<- int+ geom_point(aes(shape=Country)) +  scale_shape_manual(values=c(0,15,3,4,20,6,7))
int3<-int2 + geom_line(aes(linetype=Country)) +  scale_linetype_manual(values=c("dotted","solid", "dotted","twodash","dotted","dotted","dotted"))
int3 + theme_bw() + scale_x_continuous(breaks = c(1896, 1900, 1905),labels = c("1896", "1900", "1905")) +  theme(legend.position = c(0.15,0.75))
ggsave("Figures\\IntAdoption.eps", width = 15, height = 15, units = "cm", dpi=300)
















#########################################

##Read in Data sets
Towns<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv", header=TRUE) 

MatInvDist<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDist.csv", header=TRUE, row.names = 1) 
MatInvDistSq<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistSq.csv", header=TRUE, row.names = 1) 
MatInvDistTel<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistTel.csv", header=TRUE, row.names = 1) 


##clean data
MatInvDist<-as.matrix(MatInvDist)  #confirm data in matrix form
MatInvDistSq<-as.matrix(MatInvDistSq)  #confirm data in matrix form
MatInvDistTel<-as.matrix(MatInvDistTel)  #confirm data in matrix form


#Create months of exchange in service
Towns$InstallMonth<-max(Towns$InstallTime)+1-Towns$InstallTime


Towns$InstallMAPredict<-Towns$InstallMonth*(1/exp(-0.00823*Towns$MA_Post_Out_1880))
Towns$InstallYPredict<-Towns$InstallMonth*(1/exp(-0.02*Towns$Y1880))


###########################
#Plot presentation
#based on Data_diffusion.r


Cumulative<-mat.or.vec(max(Towns$InstallMonth),4)
Cumulative[,1]<-seq(1,max(Towns$InstallMonth),1)
for (i in 1: dim(Cumulative)[1]){
  Cumulative[i,2]<-sum(Towns$InstallMonth<=i)
  Cumulative[i,3]<-sum(Towns$InstallMAPredict<=i)
  Cumulative[i,4]<-sum(Towns$InstallYPredict<=i)
  
}
#matplot(Cumulative[,c(2,3,4)], type="l", ylab="Number of Local Exchanges", xlab="month")

p<-ggplot(data=as.data.frame(Cumulative), mapping=aes(x=Cumulative[,1],y=Cumulative[,2]))
p1<-p+geom_line() + labs(x = "Months", y = "Number of Exchanges") 
p1 + theme_bw() + scale_x_continuous(breaks = c(0, 120, 240), labels = c("Start", "120", "240"))



matplot(Cumulative[,2:4], type="l", ylab="Number of Exchanges", xlab="Months", col="BLACK")
legend(15,250, c("Historical Exchanges", "w/o External MA effect","w/o Town Size effect"),lty=1:3,box.col="white")


#cf<-ggplot(data=as.data.frame(Cumulative), mapping=aes(x=Cumulative[,1],y=Cumulative[,2]))
#cfs<-cf+ theme_bw() + scale_x_continuous(breaks = c(0, 120, 240),              labels = c("Start", "120", "240"))+  theme(legend.position = "bottom")
#cf1<-cfs+geom_line(aes(linetype="solid")) + labs(x = "Months", y = "Number of Exchanges") 
#cf2<-cf1+geom_line(aes(x=Cumulative[,1],y=Cumulative[,3],linetype="twodash"))
#cf3<-cf2+geom_line(aes(x=Cumulative[,1],y=Cumulative[,4],linetype="dotted"))



Gap<-mat.or.vec(dim(Cumulative)[1],2)
Gap[,1]<-Cumulative[,3]/Cumulative[,2]
Gap[,2]<-Cumulative[,4]/Cumulative[,2]

matplot(Gap, type="l", ylab="Share of Local Exchanges", xlab="Months", col="BLACK")
legend(150,0.4, c("w/o External MA effect","w/o Town Size effect"),lty=1:2,box.col="white")

#Gap<-as.data.frame(Gap)
#colnames(Gap)<-c("w/o External MA effect","w/o Town Size effect")

#g<-ggplot(data=Gap, mapping=aes(x=seq(1,dim(Gap)[1],1),y=Gap[,1]))
#g1<-g+geom_line() + labs(x = "Month", y = "Share of Exchanges") 
#g2<-g1+geom_line(aes(y=Gap[,2]),linetype=6)
#g2 + theme_bw() + scale_x_continuous(breaks = c(0, 120, 240),labels = c("Start", "120", "240")) +  theme(legend.position = "bottom")




####################
ExtLines<- 0.29 * (MatInvDistTel%*%Towns$Lines1905)
Shares<- ExtLines/Towns$Lines1905
Shares[Shares>1]<-1
Towns$Shares<-Shares

#plot(log(Towns$Y1880),Shares, ylab="Share of Lines")

s<-ggplot(data=Towns, mapping=aes(x=Y1880,y=Shares))
s1<-s+geom_point() + labs(x = "Population (in thousands)", y = "Share of Lines") 
s1 + theme_bw() + scale_x_log10(breaks = c(0,1,5,10,50,100))


###################################

Adoption<-as.data.frame(Towns$Town)
Adoption$Rate<-Towns$Penetration1896
Adoption$Install<-Towns$InstallTime-108
Adoption$Year<-1896


Adoption00<-as.data.frame(Towns$Town)
Adoption00$Rate<-Towns$Penetration1900
Adoption00$Install<-Towns$InstallTime-60
Adoption00$Year<-1900

Adoption05<-as.data.frame(Towns$Town)
Adoption05$Rate<-Towns$Penetration1905
Adoption05$Install<-Towns$InstallTime
Adoption05$Year<-1905

Adoption<-rbind(Adoption,Adoption00,Adoption05)
Adoption$Year<-as.factor(Adoption$Year)

Ad<-ggplot(data=Adoption[Adoption$Rate>0,], mapping=aes(x=Install,y=Rate, group=Year))
Ad1<-Ad+geom_point(aes(shape=Year)) +  scale_shape_manual(values=c(4,0,20)) 

Ad1 + theme_bw()+ labs(x = "Length of Availability", y = "Adoption rate") +  theme(legend.position = c(0.75,0.75))

ggsave("Figures\\Adoption.eps", width = 15, height = 10, units = "cm", dpi=300)



############################################3

Towns$CoordCol<-"Black"
Towns$CoordCol[Towns$InstallTime<108]<-"gray33"
Towns$CoordCol[Towns$InstallTime<60]<-"gray66"

Towns$CoordPch<-4
Towns$CoordPch[Towns$InstallTime<108]<-0
Towns$CoordPch[Towns$InstallTime<60]<-20



plot(Towns$Longitude,Towns$Latitude,col=Towns$CoordCol,pch=Towns$CoordPch, xlab="Longitude",ylab="Latitude")
addBorders(eu=NA)
legend(12.55,50.2, c("<= 1896", "<= 1900","<= 1905"),pch=c(4,0,20),col=c("Black","gray33","gray66"),box.col="white")




##############################################33

cormat<-cor(Towns[,c(8,28,68,71,74,78,79,80,81,83,85,86,87,89,90,91,93,95,99)])
