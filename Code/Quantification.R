###############################
##########################
# Alternative diffusion curve

Delay<- -0.00000693     #Effect on Market Access variable in Diffusion hazard

TownsHazardCons<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardCons.csv") 


AltInstall<-TownsHazardCons$InstallMonth/exp(Delay*TownsHazardCons$MA_Post_Out_1880)

Cumulative<-matrix(0,nrow=max(TownsHazardCons$InstallMonth),ncol=4)
Cumulative[,1]<-seq(1,max(TownsHazardCons$InstallMonth),1)
for (i in 1: dim(Cumulative)[1]){
  Cumulative[i,2]<-sum(TownsHazardCons$InstallMonth<=i)
  Cumulative[i,3]<-sum(AltInstall<=i)
}
Cumulative[,4]<-Cumulative[,2]-Cumulative[,3]
matplot(Cumulative[,2:3], type="l", ylab="Number of Local Exchanges", xlab="")

plot(Cumulative[,c(4)]/Cumulative[,2])






#################
################
#Quantification of shares of phone lines due to long distance phone calls


##Read in Data sets
Towns<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv", header=TRUE) 


MatInvDistTel<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\MatInvDistTel.csv", header=TRUE, row.names = 1) 
MatInvDistTel<-as.matrix(MatInvDistTel)  #confirm data in matrix form

##remove Pfalz from analysis 
Main<-Towns$Region!='PF'                 
Towns<-Towns[Main==TRUE,]
MatInvDistTel<-MatInvDistTel[Main==TRUE,Main==TRUE]

#rescale population to make coefficients readable
Towns$Y1905<-Towns$Y1905/1000
Towns$Y1900<-Towns$Y1900/1000
Towns$Y1896<-Towns$Y1896/1000



#############################################

#
Effect<-0.305   #pull correct effect from spatial regression !!!!!!!!!

Shares<-(Effect*(MatInvDistTel%*%Towns$Lines1905))/Towns$Lines1905