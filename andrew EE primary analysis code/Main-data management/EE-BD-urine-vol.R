

#---------------------------------------
# EE-BD-urine-vol.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Calculate the total volume of urine 
# collected from children across the 2h
# and 5hr forms
#---------------------------------------

rm(list=ls())
library(foreign)
library(tidyverse)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")

bl2hr.long<-read.dta("Baseline/Baseline_Urine_2hr_CLEANED_30Sept14_stata12.dta")
bl5hr.long<-read.dta("Baseline/Baseline_Urine_5hr_CLEANED_30Sept14_stata12.dta")
ml2hr.long<-read.dta("Midline/Urine_2hr_Midline_Cleaned_MatchedwEnrollment_27Jan16_stata12.dta")
ml5hr.long<-read.dta("Midline/Urine_5hr_Midline_Cleaned_MatchedwEnrollment_27Jan16_stata12.dta")
el2hr.long<-read.dta("Endline/EE_Endline_Urine_2hr_CLEANED_data_28July2016_stata12.dta")
el5hr.long<-read.dta("Endline/EE_Endline_Urine_5hr_CLEANED_data_28July2016_stata12.dta")

pre_ml2hr.long<-read.dta("Midline/Pre-LM2Hr_Urine_Main_Midline_Cleaned_8Jan2017_stata12.dta")
pre_ml5hr.long<-read.dta("Midline/Pre-LM_Urine_Main_Midline_Cleaned_8Jan2017_stata12.dta")

head(bl2hr.long)
bl2hr<-bl2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childno, episodeno, h2.urinevol) %>%
      spread(episodeno, h2.urinevol, fill=NA, sep="_h2_")
      
head(bl5hr.long)
bl5hr<-bl5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childno, episodeno, h5.urinevol) %>%
      spread(episodeno, h5.urinevol, fill=NA, sep="_h5_")      

head(ml2hr.long)
ml2hr<-ml2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childno, episodeno, h2.urinevol) %>%
      spread(episodeno, h2.urinevol, fill=NA, sep="_h2_")
      
head(ml5hr.long)
ml5hr<-ml5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childno, episodeno, h5.urinevol) %>%
      spread(episodeno, h5.urinevol, fill=NA, sep="_h5_")  

head(el2hr.long)
el2hr<-el2hr.long %>%
      rename(h2.urinevol=q16_hour2) %>%
      select(dataid, childNo, EpisodeNo, h2.urinevol) %>%
      spread(EpisodeNo, h2.urinevol, fill=NA, sep="_h2_")
      
head(el5hr.long)
el5hr<-el5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childNo, EpisodeNo, h5.urinevol) %>%
      spread(EpisodeNo, h5.urinevol, fill=NA, sep="_h5_")  

head(pre_ml2hr.long)
pre_ml2hr<-pre_ml2hr.long %>%
      rename(preh2.urinevol=q44_hour2) %>%
      select(dataid, childNo, EpisodeNo, preh2.urinevol) %>%
      spread(EpisodeNo, preh2.urinevol, fill=NA, sep="_preh2_")  

head(pre_ml5hr.long)
pre_ml5hr<-pre_ml5hr.long %>%
      rename(h5.urinevol=q16_hour5) %>%
      select(dataid, childNo, EpisodeNo, h5.urinevol) %>%
      spread(EpisodeNo, h5.urinevol, fill=NA, sep="_preh5_")  

#Merge urine volume datasets
#baseline
dim(bl2hr)
dim(bl5hr)
bl<-merge(bl2hr, bl5hr, by=c("dataid", "childno"), all.x = T, all.y=T)
dim(bl)

dim(ml2hr)
dim(ml5hr)
ml<-merge(ml2hr, ml5hr, by=c("dataid", "childno"), all.x = T, all.y=T)
dim(ml)

dim(el2hr)
dim(el5hr)
el<-merge(el2hr, el5hr, by=c("dataid", "childNo"), all.x = T, all.y=T)
dim(el)


#Sum up urine volumes
bl<-apply(bl, 2, function(x) as.numeric(x))
ml<-apply(ml, 2, function(x) as.numeric(x))
el<-apply(el, 2, function(x) as.numeric(x))

head(bl)
head(ml)
head(el)

bl<-cbind(bl[,1:2],rowSums(bl[,3:ncol(bl)], na.rm=T)) 
ml<-cbind(ml[,1:2],rowSums(ml[,3:ncol(ml)], na.rm=T)) 
el<-cbind(el[,1:2],rowSums(el[,3:ncol(el)], na.rm=T)) 
colnames(bl)[2:3]<-c("childNo","urineVol_t1")
colnames(ml)[2:3]<-c("childNo","urineVol_t2")
colnames(el)[3]<-"urineVol_t3"
      
urineVol<-merge(bl, ml, by=c("dataid", "childNo"), all.x = T, all.y=T)
urineVol<-merge(urineVol, el, by=c("dataid", "childNo"), all.x = T, all.y=T)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
save(urineVol, file="urine_volume.Rdata")












