
###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)

setwd("C:/Users/andre/Dropbox/HBGDki/WASH Benefits Analysis/0. Data/WBB-primary-outcome-datasets/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
consort<-read.csv("BD-EE-consort.csv")
consort<-subset(consort, select=c(dataid, childno, clusterid, miss1reason_ee))


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")


dim(consort)
d<-left_join(consort,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 
 #Subset to only control and WSH+N for telomere plan
 d<-subset(d, tr=="Control" | tr=="Nutrition + WSH")
 
#Calculate number of clusters
d %>% distinct(clusterid) %>% summarize(N=n())

#Calculate number of households
d %>% distinct(dataid) %>% summarize(N=n())




#Calculate median child ages at time 1 and 2
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 
 median(d$agem2, na.rm=T)
 median(d$agem3, na.rm=T)
 


