

#---------------------------------------
# EE-BD-urine-vol-sensitivity.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Calculate the total volume of urine 
# collected from children across the 2h
# and 5hr forms and drop  samples with urine leakage or 
# stool contamination for the sensitivity analysis
#---------------------------------------

rm(list=ls())
library(foreign)
library(tidyverse)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")


#Load the urine analysis dataset to compare the IDs of children with outcome data
# and children with volume data
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/urine_analysis_df.Rdata")


#Load the urine volume data
bl2hr.long<-read.dta("Baseline/Baseline_Urine_2hr_CLEANED_30Sept14_stata12.dta")
bl5hr.long<-read.dta("Baseline/Baseline_Urine_5hr_CLEANED_30Sept14_stata12.dta")
ml2hr.long<-read.dta("Midline/Urine_2hr_Midline_Cleaned_MatchedwEnrollment_27Jan16_stata12.dta")
ml5hr.long<-read.dta("Midline/Urine_5hr_Midline_Cleaned_MatchedwEnrollment_27Jan16_stata12.dta")
el2hr.long<-read.dta("Endline/EE_Endline_Urine_2hr_CLEANED_data_28July2016_stata12.dta")
el5hr.long<-read.dta("Endline/EE_Endline_Urine_5hr_CLEANED_data_28July2016_stata12.dta")

table(bl2hr.long$q17_hour2 >1 |  bl2hr.long$q18_hour2==1)
table(bl5hr.long$q17_hour5 >1 |  bl5hr.long$q18_hour5==1)

#Criteria for exclusion due to leakage or contamination
  # q17_hour2 >1 (urine episodes where urine leakage occurred during 0-2 hours)
  # q18_hour2 =1 (urine episodes where stool contamination occurred during 0-2 hours)
  # q17_hour5 >1 (urine episodes where urine leakage occurred during 2-5 hours)
  # q18_hour5 =1 (urine episodes where stool contamination occurred during 2-5 hours)




bl2hr<-bl2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childno, episodeno, h2.urinevol) %>%
      spread(episodeno, h2.urinevol, fill=NA, sep="_h2_") 

blcont2hr<-bl2hr.long %>%
      mutate(bl_contaminated2hr=as.numeric(q17_hour2 >1 |  q18_hour2==1)) %>%
      select(dataid, childno, episodeno, bl_contaminated2hr) %>%
      spread(episodeno, bl_contaminated2hr, fill=NA, sep="_h2_") %>%
      mutate(bl_contaminated2hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childno, bl_contaminated2hr)

bl2hr<-merge(bl2hr, blcont2hr, by=c("dataid","childno") ,all.x=F, all.y=F) #%>% 
      #filter(contaminated==0) %>% subset(., select= -c(contaminated))
head(bl2hr)

table(blcont2hr$bl_contaminated2hr)
table(bl2hr$bl_contaminated2hr)

head(bl5hr.long)
bl5hr<-bl5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childno, episodeno, h5.urinevol) %>%
      spread(episodeno, h5.urinevol, fill=NA, sep="_h5_") 

blcont5hr<-bl5hr.long %>%
      mutate(bl_contaminated5hr=as.numeric(q17_hour5 >1 |  q18_hour5==1)) %>%
      select(dataid, childno, episodeno, bl_contaminated5hr) %>%
      spread(episodeno, bl_contaminated5hr, fill=NA, sep="_h5_") %>%
      mutate(bl_contaminated5hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childno, bl_contaminated5hr)

bl5hr<-merge(bl5hr, blcont5hr, by=c("dataid","childno") ,all.x=T, all.y=T) #%>% 
      #filter(bl_contaminated==0) %>% subset(., select= -c(bl_contaminated))
head(bl5hr)



ml2hr<-ml2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childno, episodeno, h2.urinevol) %>%
      spread(episodeno, h2.urinevol, fill=NA, sep="_h2_") 

mlcont2hr<-ml2hr.long %>%
      mutate(ml_contaminated2hr=as.numeric(q17_hour2 >1 |  q18_hour2==1)) %>%
      select(dataid, childno, episodeno, ml_contaminated2hr) %>%
      spread(episodeno, ml_contaminated2hr, fill=NA, sep="_h2_") %>%
      mutate(ml_contaminated2hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childno, ml_contaminated2hr)

ml2hr<-merge(ml2hr, mlcont2hr, by=c("dataid","childno") ,all.x=T, all.y=T) #%>% 
      #filter(ml_contaminated2hr==0) %>% subset(., select= -c(ml_contaminated2hr))
head(ml2hr)



head(ml5hr.long)
ml5hr<-ml5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childno, episodeno, h5.urinevol) %>%
      spread(episodeno, h5.urinevol, fill=NA, sep="_h5_") 

mlcont5hr<-ml5hr.long %>%
      mutate(ml_contaminated5hr=as.numeric(q17_hour5 >1 |  q18_hour5==1)) %>%
      select(dataid, childno, episodeno, ml_contaminated5hr) %>%
      spread(episodeno, ml_contaminated5hr, fill=NA, sep="_h5_") %>%
      mutate(ml_contaminated5hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childno, ml_contaminated5hr)

ml5hr<-merge(ml5hr, mlcont5hr, by=c("dataid","childno") ,all.x=T, all.y=T) #%>% 
      #filter(ml_contaminated5hr==0) %>% subset(., select= -c(ml_contaminated5hr))
head(ml5hr)




el2hr<-el2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childNo, EpisodeNo, h2.urinevol) %>%
      spread(EpisodeNo, h2.urinevol, fill=NA, sep="_h2_") 

elcont2hr<-el2hr.long %>%
      mutate(el_contaminated2hr=as.numeric(q17_hour2 >1 |  q18_hour2==1)) %>%
      select(dataid, childNo, EpisodeNo, el_contaminated2hr) %>%
      spread(EpisodeNo, el_contaminated2hr, fill=NA, sep="_h2_") %>%
      mutate(el_contaminated2hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childNo, el_contaminated2hr)

el2hr<-merge(el2hr, elcont2hr, by=c("dataid","childNo") ,all.x=T, all.y=T) #%>% 
     # filter(el_contaminated2hr==0) %>% subset(., select= -c(el_contaminated2hr))
head(el2hr)



head(el5hr.long)
el5hr<-el5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childNo, EpisodeNo, h5.urinevol) %>%
      spread(EpisodeNo, h5.urinevol, fill=NA, sep="_h5_") 

elcont5hr<-el5hr.long %>%
      mutate(el_contaminated5hr=as.numeric(q17_hour5 >1 |  q18_hour5==1)) %>%
      select(dataid, childNo, EpisodeNo, el_contaminated5hr) %>%
      spread(EpisodeNo, el_contaminated5hr, fill=NA, sep="_h5_") %>%
      mutate(el_contaminated5hr= rowSums(.[3:ncol(.)], na.rm=T)) %>%
      select(dataid, childNo, el_contaminated5hr)

el5hr<-merge(el5hr, elcont5hr, by=c("dataid","childNo") ,all.x=T, all.y=T) #%>% 
      #filter(el_contaminated5hr==0) %>% subset(., select= -c(el_contaminated5hr))
head(el5hr)



#Merge urine volume datasets
#baseline
dim(bl2hr)
dim(bl5hr)
bl<-merge(bl2hr, bl5hr, by=c("dataid", "childno"), all.x = T, all.y = T)
dim(bl)

#Replace missing
bl$bl_contaminated2hr[is.na(bl$bl_contaminated2hr)] <- 0
bl$bl_contaminated5hr[is.na(bl$bl_contaminated5hr)] <- 0



dim(ml2hr)
dim(ml5hr)
ml<-merge(ml2hr, ml5hr, by=c("dataid", "childno"), all.x = T, all.y=T)
dim(ml)

#Replace missing
ml$ml_contaminated2hr[is.na(ml$ml_contaminated2hr)] <- 0
ml$ml_contaminated5hr[is.na(ml$ml_contaminated5hr)] <- 0


dim(el2hr)
dim(el5hr)
el<-merge(el2hr, el5hr, by=c("dataid", "childNo"), all.x = T, all.y=T)
dim(el)

#Replace missing
el$el_contaminated2hr[is.na(el$el_contaminated2hr)] <- 0
el$el_contaminated5hr[is.na(el$el_contaminated5hr)] <- 0


#Check for kids with urine volumn but no outcomes
d <- d %>% rename(childno=childNo) 

bl2 <- bl %>% mutate(childno=as.numeric(childno), dataid=as.numeric(dataid))
miss_bl <- anti_join(bl2, d %>% filter(!is.na(Lact1)), by=c("dataid", "childno"))
dim(miss_bl)

ml2 <- ml %>% mutate(childno=as.numeric(childno), dataid=as.numeric(dataid))
miss_ml <- anti_join(ml2, d %>% filter(!is.na(Lact2)), by=c("dataid", "childno"))
dim(miss_ml)

el2 <- el %>% rename(childno=childNo) %>% mutate(childno=as.numeric(childno), dataid=as.numeric(dataid))
miss_el <- anti_join(el2, d %>% filter(!is.na(Lact3)), by=c("dataid", "childno"))
dim(miss_el)

#Drop kids with no outcome information
dim(bl)
dim(ml)
dim(el)
bl <- semi_join(bl2, d %>% filter(!is.na(Lact1)), by=c("dataid", "childno"))
ml <- semi_join(ml2, d %>% filter(!is.na(Lact2)), by=c("dataid", "childno"))
el <- semi_join(el2, d %>% filter(!is.na(Lact3)), by=c("dataid", "childno"))
dim(bl)
table(!is.na(d$Lact1))
dim(ml)
table(!is.na(d$Lact2))
dim(el)
table(!is.na(d$Lact3))


#Tabulate level of contamination
tab1<-table(bl$bl_contaminated2hr>0 | bl$bl_contaminated5hr>0 )
tab1
tab1[2]/(tab1[1]+tab1[2])*100

tab2<-table(ml$ml_contaminated2hr>0 | ml$ml_contaminated5hr>0)
tab2
tab2[2]/(tab2[1]+tab2[2])*100

tab3<-table(el$el_contaminated2hr>0 | el$el_contaminated5hr>0)
tab3
tab3[2]/(tab3[1]+tab3[2])*100


#Audrie's numbers:
#45% of children at age 3 months, 36% of children at age 14 months, and 24%


#Drop contaminates
# dim(bl)
# dim(ml)
# dim(el)
# bl <- bl %>% filter(el_contaminated2hr!=1 & el_contaminated5hr!=1)
# ml <- ml %>% filter(el_contaminated2hr!=1 & el_contaminated5hr!=1)
# el <- el %>% filter(el_contaminated2hr!=1 & el_contaminated5hr!=1)
# dim(bl)
# dim(ml)
# dim(el)

#Sum up urine volumes
bl<-apply(bl, 2, function(x) as.numeric(x))
ml<-apply(ml, 2, function(x) as.numeric(x))
el<-apply(el, 2, function(x) as.numeric(x))

head(bl)
head(ml)
head(el)

# bl<-cbind(bl[,1:2],rowSums(bl[,3:ncol(bl)], na.rm=T)) 
# ml<-cbind(ml[,1:2],rowSums(ml[,3:ncol(ml)], na.rm=T)) 
# el<-cbind(el[,1:2],rowSums(el[,3:ncol(el)], na.rm=T)) 

bl<-cbind(bl[,1:2],rowSums(bl[,c(3:7,9:17)], na.rm=T), bl[,c(8,18)]) 
ml<-cbind(ml[,1:2],rowSums(ml[,c(3:5,7:13)], na.rm=T), ml[,c(6,14)]) 
el<-cbind(el[,1:2],rowSums(el[,c(3:6,8:15)], na.rm=T), el[,c(7,16)]) 
colnames(bl)[2:3]<-c("childNo","urineVol_t1")
colnames(ml)[2:3]<-c("childNo","urineVol_t2")
colnames(el)[3]<-"urineVol_t3"
      
urineVol<-merge(bl, ml, by=c("dataid", "childNo"), all.x = T, all.y=T)

colnames(el)[2]<-"childNo"
urineVol<-merge(urineVol, el, by=c("dataid", "childNo"), all.x = T, all.y=T)
head(urineVol)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
save(urineVol, file="urine_volume_sensitivity.Rdata")












