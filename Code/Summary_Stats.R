rm(list = ls(all = TRUE))               
library("foreign")
library("stargazer")

##Read in Data sets
Towns<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Towns.csv", header=TRUE) 



TownsHazardCons<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardCons.csv") 

TownsHazardVary<-read.csv("C:\\Box\\Research\\Telephone\\project_telephone\\Data\\Stata\\TownsHazardVary.csv")


stargazer(Towns[,c(35,99,95,80,8,90,91,83,68,71,81,93,28,85,87,86,89,78,74,79)],digits=2,omit.summary.stat=c("n"))

stargazer(TownsHazardCons[,c(46,17,13,35,30,8,11,38,28,29,36,22,24,39,41,40,44,33,32,34)],digits=2,omit.summary.stat=c("n"))

