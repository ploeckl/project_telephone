#############################
###########################
#Plot presentation
#based on Data_diffusion.r


Cumulative<-mat.or.vec(max(Towns$InstallMonth),2)
Cumulative[,1]<-seq(1,max(Towns$InstallMonth),1)
for (i in 1: dim(Cumulative)[1]){
  Cumulative[i,2]<-sum(Towns$InstallMonth<=i)
}
plot(Cumulative, type="l", ylab="Number of Local Exchanges", xlab="")


Penetration<-as.matrix(Towns[,c("Penetration1896","Penetration1900","Penetration1905")])
Penetration[Penetration==0]<-NA
matplot(c(1896,1900,1905),t(Penetration),col="BLACK",ylab="Phone Lines per Capita", xlab="Year",type="p",pch=19, lty=3)