
#---------------------------------------
# EE-BD-mathistory.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Analysing the maternal history surveys for 
# EED Bangladesh sub-study
#---------------------------------------

###Load in data
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)
#Load in enrollment data for adjusted analysis
enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

#Load in maternal history data
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
mathist<-read.csv("BD-EE-medhistory.csv")

head(mathist)


#Merge treatment information 
dim(mathist)
d<-left_join(mathist, treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)

#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!is.na(consent1)) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% subset(!is.na(consent1)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 



#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(consent2)) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(consent2)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 



#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na(consent3)) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(consent3)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 

rbind(overallN1, t1[c(1,3,4,2),])
rbind(overallN2, t2[c(1,3,4,2),])
rbind(overallN3, t3[c(1,3,4,2),])

d[d$dataid=="3803"|d$dataid=="16906",]
