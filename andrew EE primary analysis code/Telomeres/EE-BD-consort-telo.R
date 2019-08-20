

#---------------------------------------
# EE-BD-consort.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Tabulating Consort numbers for the EE 
# substudy of the WASH Benefits Bangladesh
# trial.
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
consort<-read.csv("BD-EE-consort.csv")
consort<-subset(consort, select= -tr) #Remove blinded EED treatments

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
#load("washb-BD-telo-blind-tr.Rdata")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Nutrition + WSH"))
levels(treatment$tr)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")



#Merge in treatment data
dim(consort)
table(is.na(consort$clusterid))
d<-left_join(consort,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)

#Drop year 1 data
d<-subset(d, svy!=1)

#Tabulate number of compounds
#d %>% group_by(tr) %>%
#  distinct(dataid) %>%
#  summarize(n.compound=n())

#d %>% distinct(dataid) %>%
#  summarize(n.compound=n())

#-----------------------------
#Subsample target
#-----------------------------

# Compounds

d %>% group_by(tr, svy) %>%
  distinct(clusterid) %>%
  summarize(n.clusters=n())

# Index children (any child in the enrollment record)
d %>% group_by(tr, svy) %>%
  distinct(dataid, childno) %>%
  summarize(n.index=n())
  

#-----------------------------
# Follow-up
#-----------------------------

#Arrange missingness reason
table(d$miss1reason_ee)
d$miss1reason_ee<-factor(d$miss1reason_ee, levels=c("Moved away","Absent","Withdrew","No live birth", "Child death", "Not lost"))

#Drop out non-missing
NotLost<-subset(d, miss1reason_ee=="Not lost")
followup<-subset(d, miss1reason_ee!="Not lost")



#Subset by round
followup1<-subset(followup, svy==2)
followup2<-subset(followup, svy==3)


miss1<-followup1 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss2<-followup2 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss1
miss2


# Number new children added between rounds
#Subset by round
d1<-subset(d, svy==2)
d2<-subset(d, svy==3)

anti_join(d2, d1, by=c("dataid", "childno")) %>%
  group_by(tr) %>%
  summarize(new.children=n())





#-----------------------------
#Subsample enrollment
#-----------------------------

#clusters
NotLost %>% group_by(svy, tr) %>% distinct(clusterid) %>%summarize(N=n())

#Children
NotLost %>% group_by(svy, tr) %>% distinct(dataid, childno) %>%summarize(N=n())








  