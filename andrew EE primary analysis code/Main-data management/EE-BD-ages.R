
#---------------------------------------
# EE-BD-ages.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Calculate child ages at each round
# for the EED Bangladesh substudy
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)


#Load in blinded treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
# levels(treatment$tr)
# treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
# levels(treatment$tr)

#Load in lab outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
outcomes.stool<-read.dta("BD-EE-stool-outcomes-Stata12.dta")
outcomes.urine<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")

#Load in survey data
urine<-read.csv("BD-EE-urine.csv")
stool<-read.csv("BD-EE-stool.csv")


#Extract needed variables from each dataset:
colnames(outcomes.stool)
colnames(outcomes.urine)

colnames(stool)
stool <- stool %>%
      subset(select=c(childid, dataid, childNo,clusterid,agem1,agem2,agem3,sex)) %>%
      rename(st.agem1=agem1,st.agem2=agem2,st.agem3=agem3, st.sex=sex)

colnames(urine)
urine <- urine %>%
      subset(select=c(childid, dataid, childNo,agem1,agem2,agem3,sex)) %>%
      rename(ur.agem1=agem1,ur.agem2=agem2,ur.agem3=agem3, ur.sex=sex)


#merge datasets
dim(outcomes.stool)
dim(outcomes.urine)
outcomes<-merge(outcomes.stool, outcomes.urine, by=c("childid"), all.x =T, all.y =T)
dim(outcomes)


surveys<-merge(stool, urine, by=c("childid","dataid","childNo"), all.x =T, all.y =T)

#Merge L/M outcomes
dim(surveys)
dim(outcomes)
outcomes$childid<-as.numeric(outcomes$childid)
d<-left_join(surveys,outcomes, by="childid")
dim(d)

#Remove empty row
dim(d)
d<-d[!is.na(d$childid),]
dim(d)


#Preferentially select existing urine ages, then stool ages
d$agem1<-d$ur.agem1
d$agem2<-d$ur.agem2
d$agem3<-d$ur.agem3
d$sex<-d$ur.sex

d$agem1[is.na(d$agem1)]<-d$st.agem1[is.na(d$agem1)]
d$agem2[is.na(d$agem2)]<-d$st.agem2[is.na(d$agem2)]
d$agem3[is.na(d$agem3)]<-d$st.agem3[is.na(d$agem3)]
d$sex[is.na(d$sex)]<-d$st.sex[is.na(d$sex)]

#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
table(is.na(d$tr))



table(!(is.na(d$Lact3)))
table(!(is.na(d$Mann3)))
table(!(is.na(d$t3_aat)))
table(!(is.na(d$t3_mpo)))
table(!(is.na(d$t3_neo)))


test<-d%>% subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) 
dim(test)

#Save dataset of all children with either outcome (all children in EED substudy) at baseline
child <- d %>% filter(!is.na(d$Lact1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) %>% subset(., select=c(childid,dataid, clusterid, childNo, tr))
save(child, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/child_df.Rdata")


#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% 
  subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) %>% 
  summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d %>% 
  subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) %>% 
  group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d %>% 
  subset(!is.na(d$Lact2)|!is.na(d$Mann2)|!is.na(d$t2_aat)|!is.na(d$t2_mpo)|!is.na(d$t2_neo)|!is.na(d$t2_reg)) %>% 
  summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% 
  subset(!is.na(d$Lact2)|!is.na(d$Mann2)|!is.na(d$t2_aat)|!is.na(d$t2_mpo)|!is.na(d$t2_neo)|!is.na(d$t2_reg)) %>% 
  group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% 
  subset(!is.na(d$Lact3)|!is.na(d$Mann3)|!is.na(d$t3_aat)|!is.na(d$t3_mpo)|!is.na(d$t3_neo)) %>% 
  summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d %>% 
  subset(!is.na(d$Lact3)|!is.na(d$Mann3)|!is.na(d$t3_aat)|!is.na(d$t3_mpo)|!is.na(d$t3_neo)) %>% 
  group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


age_t1_urine_stool_M<-rbind(overallN1, t1)
age_t2_urine_stool_M<-rbind(overallN2, t2)
age_t3_urine_stool_M<-rbind(overallN3, t3)


#Reorder to match Audrie
age_t1_urine_stool_M<-age_t1_urine_stool_M[,c(1,2,4,3,5,7,6)]
age_t2_urine_stool_M<-age_t2_urine_stool_M[,c(1,2,4,3,5,7,6)]
age_t3_urine_stool_M<-age_t3_urine_stool_M[,c(1,2,4,3,5,7,6)]



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(age_t1_urine_stool_M, 
     age_t2_urine_stool_M, 
     age_t3_urine_stool_M, 
     file="childage_res_N_M.Rdata")


