
#---------------------------------------
# bangladesh-immune-ages-unadj-analysis.R
#
# audrie lin (audrielin@berkeley.edu)
#
# EE in the WASH B package 
# functions and saving output for the replication 
# compare.R script
# 
# input: 
# bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv (from 3-bangladesh-dm-immun-plasma-immun-3.do)
#
# output: 
# immune_N_means.RData (N's and means of immune data)
# immune_unadj_glm.RData (unadj glm of immune data)
#---------------------------------------

#Clear out R environment (remove and loaded data)
rm(list=ls())


######################
###Load in packages
######################

#Load packages
library(devtools)
library(foreign) #Run each time R is started up to load the package into working memory
library(washb) 
library(dplyr)

######################
###Load in data
######################

#Set working directory to load in blinded treatment assignment and enrolment information
setwd("~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/") #Set working directory

#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
lab<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(lab$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in immune
lab$tr <- factor(lab$tr,levels=c("Control","Nutrition + WSH"))

#calculate overall N's and means at t2
igf_t2_N<-lab %>%
  subset(t2_ln_igf!="NA") %>%
  summarize(t2_ln_igf_N_overall=n(), mean=mean(t2_ln_igf, na.rm = T),  sd=sd(t2_ln_igf, na.rm = T))

igf_t2_N

crp_t2_N<-lab %>%
  subset(t2_ln_crp!="NA") %>%
  summarize(t2_ln_crp_N_overall=n(), mean=mean(t2_ln_crp, na.rm = T),  sd=sd(t2_ln_crp, na.rm = T))

crp_t2_N

agp_t2_N<-lab %>%
  subset(t2_ln_agp2="NA") %>%
  summarize(t2_ln_agp_N_overall=n(), mean=mean(t2_ln_agp, na.rm = T),  sd=sd(t2_ln_agp, na.rm = T))

agp_t2_N

gmc_t2_N<-lab %>%
  subset(t2_ln_gmc!="NA") %>%
  summarize(t2_ln_gmc_N_overall=n(), mean=mean(t2_ln_gmc, na.rm = T),  sd=sd(t2_ln_gmc, na.rm = T))

gmc_t2_N

ifn_t2_N<-lab %>%
  subset(t2_ln_ifn!="NA") %>%
  summarize(t2_ln_ifn_N_overall=n(), mean=mean(t2_ln_ifn, na.rm = T),  sd=sd(t2_ln_ifn, na.rm = T))

ifn_t2_N

il10_t2_N<-lab %>%
  subset(t2_ln_il10!="NA") %>%
  summarize(t2_ln_il10_N_overall=n(), mean=mean(t2_ln_il10, na.rm = T),  sd=sd(t2_ln_il10, na.rm = T))

il10_t2_N

il12_t2_N<-lab %>%
  subset(t2_ln_il12!="NA") %>%
  summarize(t2_ln_il12_N_overall=n(), mean=mean(t2_ln_il12, na.rm = T),  sd=sd(t2_ln_il12, na.rm = T))

il12_t2_N

il13_t2_N<-lab %>%
  subset(t2_ln_il13!="NA") %>%
  summarize(t2_ln_il13_N_overall=n(), mean=mean(t2_ln_il13, na.rm = T),  sd=sd(t2_ln_il13, na.rm = T))

il13_t2_N

il17_t2_N<-lab %>%
  subset(t2_ln_il17!="NA") %>%
  summarize(t2_ln_il17_N_overall=n(), mean=mean(t2_ln_il17, na.rm = T),  sd=sd(t2_ln_il17, na.rm = T))

il17_t2_N

il1_t2_N<-lab %>%
  subset(t2_ln_il1!="NA") %>%
  summarize(t2_ln_il1_N_overall=n(), mean=mean(t2_ln_il1, na.rm = T),  sd=sd(t2_ln_il1, na.rm = T))

il1_t2_N

il2_t2_N<-lab %>%
  subset(t2_ln_il2!="NA") %>%
  summarize(t2_ln_il2_N_overall=n(), mean=mean(t2_ln_il2, na.rm = T),  sd=sd(t2_ln_il2, na.rm = T))

il2_t2_N

il21_t2_N<-lab %>%
  subset(t2_ln_il21!="NA") %>%
  summarize(t2_ln_il21_N_overall=n(), mean=mean(t2_ln_il21, na.rm = T),  sd=sd(t2_ln_il21, na.rm = T))

il21_t2_N

il4_t2_N<-lab %>%
  subset(t2_ln_il4!="NA") %>%
  summarize(t2_ln_il4_N_overall=n(), mean=mean(t2_ln_il4, na.rm = T),  sd=sd(t2_ln_il4, na.rm = T))

il4_t2_N

il5_t2_N<-lab %>%
  subset(t2_ln_il5!="NA") %>%
  summarize(t2_ln_il5_N_overall=n(), mean=mean(t2_ln_il5, na.rm = T),  sd=sd(t2_ln_il5, na.rm = T))

il5_t2_N

il6_t2_N<-lab %>%
  subset(t2_ln_il6!="NA") %>%
  summarize(t2_ln_il6_N_overall=n(), mean=mean(t2_ln_il6, na.rm = T),  sd=sd(t2_ln_il6, na.rm = T))

il6_t2_N

tnf_t2_N<-lab %>%
  subset(t2_ln_tnf!="NA") %>%
  summarize(t2_ln_tnf_N_overall=n(), mean=mean(t2_ln_tnf, na.rm = T),  sd=sd(t2_ln_tnf, na.rm = T))

tnf_t2_N





#calculate overall N's and means at t3
igf_t3_N<-lab %>%
  subset(t3_ln_igf!="NA") %>%
  summarize(t3_ln_igf_N_overall=n(), mean=mean(t3_ln_igf, na.rm = T),  sd=sd(t3_ln_igf, na.rm = T))

igf_t3_N

gmc_t3_N<-lab %>%
  subset(t3_ln_gmc!="NA") %>%
  summarize(t3_ln_gmc_N_overall=n(), mean=mean(t3_ln_gmc, na.rm = T),  sd=sd(t3_ln_gmc, na.rm = T))

gmc_t3_N

ifn_t3_N<-lab %>%
  subset(t3_ln_ifn!="NA") %>%
  summarize(t3_ln_ifn_N_overall=n(), mean=mean(t3_ln_ifn, na.rm = T),  sd=sd(t3_ln_ifn, na.rm = T))

ifn_t3_N

il10_t3_N<-lab %>%
  subset(t3_ln_il10!="NA") %>%
  summarize(t3_ln_il10_N_overall=n(), mean=mean(t3_ln_il10, na.rm = T),  sd=sd(t3_ln_il10, na.rm = T))

il10_t3_N

il12_t3_N<-lab %>%
  subset(t3_ln_il12!="NA") %>%
  summarize(t3_ln_il12_N_overall=n(), mean=mean(t3_ln_il12, na.rm = T),  sd=sd(t3_ln_il12, na.rm = T))

il12_t3_N

il13_t3_N<-lab %>%
  subset(t3_ln_il13!="NA") %>%
  summarize(t3_ln_il13_N_overall=n(), mean=mean(t3_ln_il13, na.rm = T),  sd=sd(t3_ln_il13, na.rm = T))

il13_t3_N

il17_t3_N<-lab %>%
  subset(t3_ln_il17!="NA") %>%
  summarize(t3_ln_il17_N_overall=n(), mean=mean(t3_ln_il17, na.rm = T),  sd=sd(t3_ln_il17, na.rm = T))

il17_t3_N

il1_t3_N<-lab %>%
  subset(t3_ln_il1!="NA") %>%
  summarize(t3_ln_il1_N_overall=n(), mean=mean(t3_ln_il1, na.rm = T),  sd=sd(t3_ln_il1, na.rm = T))

il1_t3_N

il2_t3_N<-lab %>%
  subset(t3_ln_il2!="NA") %>%
  summarize(t3_ln_il2_N_overall=n(), mean=mean(t3_ln_il2, na.rm = T),  sd=sd(t3_ln_il2, na.rm = T))

il2_t3_N

il21_t3_N<-lab %>%
  subset(t3_ln_il21!="NA") %>%
  summarize(t3_ln_il21_N_overall=n(), mean=mean(t3_ln_il21, na.rm = T),  sd=sd(t3_ln_il21, na.rm = T))

il21_t3_N

il4_t3_N<-lab %>%
  subset(t3_ln_il4!="NA") %>%
  summarize(t3_ln_il4_N_overall=n(), mean=mean(t3_ln_il4, na.rm = T),  sd=sd(t3_ln_il4, na.rm = T))

il4_t3_N

il5_t3_N<-lab %>%
  subset(t3_ln_il5!="NA") %>%
  summarize(t3_ln_il5_N_overall=n(), mean=mean(t3_ln_il5, na.rm = T),  sd=sd(t3_ln_il5, na.rm = T))

il5_t3_N

il6_t3_N<-lab %>%
  subset(t3_ln_il6!="NA") %>%
  summarize(t3_ln_il6_N_overall=n(), mean=mean(t3_ln_il6, na.rm = T),  sd=sd(t3_ln_il6, na.rm = T))

il6_t3_N

tnf_t3_N<-lab %>%
  subset(t3_ln_tnf!="NA") %>%
  summarize(t3_ln_tnf_N_overall=n(), mean=mean(t3_ln_tnf, na.rm = T),  sd=sd(t3_ln_tnf, na.rm = T))

tnf_t3_N

#calculate N's and mean of biomarkers at t2 by sex

igf_t2_N_sex<-lab %>%
  subset(t2_ln_igf!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_igf_N_sex=n(), mean=mean(t2_ln_igf, na.rm = T),  sd=sd(t2_ln_igf, na.rm = T))

igf_t2_N_sex


crp_t2_N_sex<-lab %>%
  subset(t2_ln_crp!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_crp_N_sex=n(), mean=mean(t2_ln_crp, na.rm = T),  sd=sd(t2_ln_crp, na.rm = T))

crp_t2_N_sex

agp_t2_N_sex<-lab %>%
  subset(t2_ln_agp!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_agp_N_sex=n(), mean=mean(t2_ln_agp, na.rm = T),  sd=sd(t2_ln_agp, na.rm = T))

agp_t2_N_sex

gmc_t2_N_sex<-lab %>%
  subset(t2_ln_gmc!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_gmc_N_sex=n(), mean=mean(t2_ln_gmc, na.rm = T),  sd=sd(t2_ln_gmc, na.rm = T))

gmc_t2_N_sex

ifn_t2_N_sex<-lab %>%
  subset(t2_ln_ifn!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_ifn_N_sex=n(), mean=mean(t2_ln_ifn, na.rm = T),  sd=sd(t2_ln_ifn, na.rm = T))

ifn_t2_N_sex

il10_t2_N_sex<-lab %>%
  subset(t2_ln_il10!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il10_N_sex=n(), mean=mean(t2_ln_il10, na.rm = T),  sd=sd(t2_ln_il10, na.rm = T))

il10_t2_N_sex

il12_t2_N_sex<-lab %>%
  subset(t2_ln_il12!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il12_N_sex=n(), mean=mean(t2_ln_il12, na.rm = T),  sd=sd(t2_ln_il12, na.rm = T))

il12_t2_N_sex

il13_t2_N_sex<-lab %>%
  subset(t2_ln_il13!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il13_N_sex=n(), mean=mean(t2_ln_il13, na.rm = T),  sd=sd(t2_ln_il13, na.rm = T))

il13_t2_N_sex

il17_t2_N_sex<-lab %>%
  subset(t2_ln_il17!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il17_N_sex=n(), mean=mean(t2_ln_il17, na.rm = T),  sd=sd(t2_ln_il17, na.rm = T))

il17_t2_N_sex

il1_t2_N_sex<-lab %>%
  subset(t2_ln_il1!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il1_N_sex=n(), mean=mean(t2_ln_il1, na.rm = T),  sd=sd(t2_ln_il1, na.rm = T))

il1_t2_N_sex

il2_t2_N_sex<-lab %>%
  subset(t2_ln_il2!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il2_N_sex=n(), mean=mean(t2_ln_il2, na.rm = T),  sd=sd(t2_ln_il2, na.rm = T))

il2_t2_N_sex

il21_t2_N_sex<-lab %>%
  subset(t2_ln_il21!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il21_N_sex=n(), mean=mean(t2_ln_il21, na.rm = T),  sd=sd(t2_ln_il21, na.rm = T))

il21_t2_N_sex

il4_t2_N_sex<-lab %>%
  subset(t2_ln_il4!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il4_N_sex=n(), mean=mean(t2_ln_il4, na.rm = T),  sd=sd(t2_ln_il4, na.rm = T))

il4_t2_N_sex

il5_t2_N_sex<-lab %>%
  subset(t2_ln_il5!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il5_N_sex=n(), mean=mean(t2_ln_il5, na.rm = T),  sd=sd(t2_ln_il5, na.rm = T))

il5_t2_N_sex

il6_t2_N_sex<-lab %>%
  subset(t2_ln_il6!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_il6_N_sex=n(), mean=mean(t2_ln_il6, na.rm = T),  sd=sd(t2_ln_il6, na.rm = T))

il6_t2_N_sex

tnf_t2_N_sex<-lab %>%
  subset(t2_ln_tnf!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_tnf_N_sex=n(), mean=mean(t2_ln_tnf, na.rm = T),  sd=sd(t2_ln_tnf, na.rm = T))

tnf_t2_N_sex



  #calculate N's and mean of biomarkers at t3 by sex
igf_t3_N_sex<-lab %>%
  subset(t3_ln_igf!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_igf_N_sex=n(), mean=mean(t3_ln_igf, na.rm = T),  sd=sd(t3_ln_igf, na.rm = T))

igf_t3_N_sex

gmc_t3_N_sex<-lab %>%
  subset(t3_ln_gmc!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_gmc_N_sex=n(), mean=mean(t3_ln_gmc, na.rm = T),  sd=sd(t3_ln_gmc, na.rm = T))

gmc_t3_N_sex

ifn_t3_N_sex<-lab %>%
  subset(t3_ln_ifn!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_ifn_N_sex=n(), mean=mean(t3_ln_ifn, na.rm = T),  sd=sd(t3_ln_ifn, na.rm = T))

ifn_t3_N_sex

il10_t3_N_sex<-lab %>%
  subset(t3_ln_il10!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il10_N_sex=n(), mean=mean(t3_ln_il10, na.rm = T),  sd=sd(t3_ln_il10, na.rm = T))

il10_t3_N_sex

il12_t3_N_sex<-lab %>%
  subset(t3_ln_il12!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il12_N_sex=n(), mean=mean(t3_ln_il12, na.rm = T),  sd=sd(t3_ln_il12, na.rm = T))

il12_t3_N_sex

il13_t3_N_sex<-lab %>%
  subset(t3_ln_il13!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il13_N_sex=n(), mean=mean(t3_ln_il13, na.rm = T),  sd=sd(t3_ln_il13, na.rm = T))

il13_t3_N_sex

il17_t3_N_sex<-lab %>%
  subset(t3_ln_il17!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il17_N_sex=n(), mean=mean(t3_ln_il17, na.rm = T),  sd=sd(t3_ln_il17, na.rm = T))

il17_t3_N_sex

il1_t3_N_sex<-lab %>%
  subset(t3_ln_il1!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il1_N_sex=n(), mean=mean(t3_ln_il1, na.rm = T),  sd=sd(t3_ln_il1, na.rm = T))

il1_t3_N_sex

il2_t3_N_sex<-lab %>%
  subset(t3_ln_il2!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il2_N_sex=n(), mean=mean(t3_ln_il2, na.rm = T),  sd=sd(t3_ln_il2, na.rm = T))

il2_t3_N_sex

il21_t3_N_sex<-lab %>%
  subset(t3_ln_il21!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il21_N_sex=n(), mean=mean(t3_ln_il21, na.rm = T),  sd=sd(t3_ln_il21, na.rm = T))

il21_t3_N_sex

il4_t3_N_sex<-lab %>%
  subset(t3_ln_il4!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il4_N_sex=n(), mean=mean(t3_ln_il4, na.rm = T),  sd=sd(t3_ln_il4, na.rm = T))

il4_t3_N_sex

il5_t3_N_sex<-lab %>%
  subset(t3_ln_il5!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il5_N_sex=n(), mean=mean(t3_ln_il5, na.rm = T),  sd=sd(t3_ln_il5, na.rm = T))

il5_t3_N_sex

il6_t3_N_sex<-lab %>%
  subset(t3_ln_il6!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_il6_N_sex=n(), mean=mean(t3_ln_il6, na.rm = T),  sd=sd(t3_ln_il6, na.rm = T))

il6_t3_N_sex

tnf_t3_N_sex<-lab %>%
  subset(t3_ln_tnf!="NA") %>%
  group_by (sex) %>%
  summarize(t3_ln_tnf_N_sex=n(), mean=mean(t3_ln_tnf, na.rm = T),  sd=sd(t3_ln_tnf, na.rm = T))

tnf_t3_N_sex

  
  
  
  
#calculate N's and mean of immune biomarkers t2 by arm


igf_t2_N_tr<-lab %>%
  subset(t2_ln_igf!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_igf_N_tr=n(), mean=mean(t2_ln_igf, na.rm = T),  sd=sd(t2_ln_igf, na.rm = T))

igf_t2_N_tr


crp_t2_N_tr<-lab %>%
  subset(t2_ln_crp!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_crp_N_tr=n(), mean=mean(t2_ln_crp, na.rm = T),  sd=sd(t2_ln_crp, na.rm = T))

crp_t2_N_tr

agp_t2_N_tr<-lab %>%
  subset(t2_ln_agp!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_agp_N_tr=n(), mean=mean(t2_ln_agp, na.rm = T),  sd=sd(t2_ln_agp, na.rm = T))

agp_t2_N_tr

gmc_t2_N_tr<-lab %>%
  subset(t2_ln_gmc!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_gmc_N_tr=n(), mean=mean(t2_ln_gmc, na.rm = T),  sd=sd(t2_ln_gmc, na.rm = T))

gmc_t2_N_tr

ifn_t2_N_tr<-lab %>%
  subset(t2_ln_ifn!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_ifn_N_tr=n(), mean=mean(t2_ln_ifn, na.rm = T),  sd=sd(t2_ln_ifn, na.rm = T))

ifn_t2_N_tr

il10_t2_N_tr<-lab %>%
  subset(t2_ln_il10!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il10_N_tr=n(), mean=mean(t2_ln_il10, na.rm = T),  sd=sd(t2_ln_il10, na.rm = T))

il10_t2_N_tr

il12_t2_N_tr<-lab %>%
  subset(t2_ln_il12!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il12_N_tr=n(), mean=mean(t2_ln_il12, na.rm = T),  sd=sd(t2_ln_il12, na.rm = T))

il12_t2_N_tr

il13_t2_N_tr<-lab %>%
  subset(t2_ln_il13!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il13_N_tr=n(), mean=mean(t2_ln_il13, na.rm = T),  sd=sd(t2_ln_il13, na.rm = T))

il13_t2_N_tr

il17_t2_N_tr<-lab %>%
  subset(t2_ln_il17!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il17_N_tr=n(), mean=mean(t2_ln_il17, na.rm = T),  sd=sd(t2_ln_il17, na.rm = T))

il17_t2_N_tr

il1_t2_N_tr<-lab %>%
  subset(t2_ln_il1!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il1_N_tr=n(), mean=mean(t2_ln_il1, na.rm = T),  sd=sd(t2_ln_il1, na.rm = T))

il1_t2_N_tr

il2_t2_N_tr<-lab %>%
  subset(t2_ln_il2!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il2_N_tr=n(), mean=mean(t2_ln_il2, na.rm = T),  sd=sd(t2_ln_il2, na.rm = T))

il2_t2_N_tr

il21_t2_N_tr<-lab %>%
  subset(t2_ln_il21!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il21_N_tr=n(), mean=mean(t2_ln_il21, na.rm = T),  sd=sd(t2_ln_il21, na.rm = T))

il21_t2_N_tr

il4_t2_N_tr<-lab %>%
  subset(t2_ln_il4!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il4_N_tr=n(), mean=mean(t2_ln_il4, na.rm = T),  sd=sd(t2_ln_il4, na.rm = T))

il4_t2_N_tr

il5_t2_N_tr<-lab %>%
  subset(t2_ln_il5!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il5_N_tr=n(), mean=mean(t2_ln_il5, na.rm = T),  sd=sd(t2_ln_il5, na.rm = T))

il5_t2_N_tr

il6_t2_N_tr<-lab %>%
  subset(t2_ln_il6!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_il6_N_tr=n(), mean=mean(t2_ln_il6, na.rm = T),  sd=sd(t2_ln_il6, na.rm = T))

il6_t2_N_tr

tnf_t2_N_tr<-lab %>%
  subset(t2_ln_tnf!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_tnf_N_tr=n(), mean=mean(t2_ln_tnf, na.rm = T),  sd=sd(t2_ln_tnf, na.rm = T))

tnf_t2_N_tr



#calculate N's and mean of biomarkers at t3 by arm
igf_t3_N_tr<-lab %>%
  subset(t3_ln_igf!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_igf_N_tr=n(), mean=mean(t3_ln_igf, na.rm = T),  sd=sd(t3_ln_igf, na.rm = T))

igf_t3_N_tr

gmc_t3_N_tr<-lab %>%
  subset(t3_ln_gmc!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_gmc_N_tr=n(), mean=mean(t3_ln_gmc, na.rm = T),  sd=sd(t3_ln_gmc, na.rm = T))

gmc_t3_N_tr

ifn_t3_N_tr<-lab %>%
  subset(t3_ln_ifn!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_ifn_N_tr=n(), mean=mean(t3_ln_ifn, na.rm = T),  sd=sd(t3_ln_ifn, na.rm = T))

ifn_t3_N_tr

il10_t3_N_tr<-lab %>%
  subset(t3_ln_il10!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il10_N_tr=n(), mean=mean(t3_ln_il10, na.rm = T),  sd=sd(t3_ln_il10, na.rm = T))

il10_t3_N_tr

il12_t3_N_tr<-lab %>%
  subset(t3_ln_il12!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il12_N_tr=n(), mean=mean(t3_ln_il12, na.rm = T),  sd=sd(t3_ln_il12, na.rm = T))

il12_t3_N_tr

il13_t3_N_tr<-lab %>%
  subset(t3_ln_il13!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il13_N_tr=n(), mean=mean(t3_ln_il13, na.rm = T),  sd=sd(t3_ln_il13, na.rm = T))

il13_t3_N_tr

il17_t3_N_tr<-lab %>%
  subset(t3_ln_il17!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il17_N_tr=n(), mean=mean(t3_ln_il17, na.rm = T),  sd=sd(t3_ln_il17, na.rm = T))

il17_t3_N_tr

il1_t3_N_tr<-lab %>%
  subset(t3_ln_il1!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il1_N_tr=n(), mean=mean(t3_ln_il1, na.rm = T),  sd=sd(t3_ln_il1, na.rm = T))

il1_t3_N_tr

il2_t3_N_tr<-lab %>%
  subset(t3_ln_il2!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il2_N_tr=n(), mean=mean(t3_ln_il2, na.rm = T),  sd=sd(t3_ln_il2, na.rm = T))

il2_t3_N_tr

il21_t3_N_tr<-lab %>%
  subset(t3_ln_il21!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il21_N_tr=n(), mean=mean(t3_ln_il21, na.rm = T),  sd=sd(t3_ln_il21, na.rm = T))

il21_t3_N_tr

il4_t3_N_tr<-lab %>%
  subset(t3_ln_il4!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il4_N_tr=n(), mean=mean(t3_ln_il4, na.rm = T),  sd=sd(t3_ln_il4, na.rm = T))

il4_t3_N_tr

il5_t3_N_tr<-lab %>%
  subset(t3_ln_il5!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il5_N_tr=n(), mean=mean(t3_ln_il5, na.rm = T),  sd=sd(t3_ln_il5, na.rm = T))

il5_t3_N_tr

il6_t3_N_tr<-lab %>%
  subset(t3_ln_il6!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_il6_N_tr=n(), mean=mean(t3_ln_il6, na.rm = T),  sd=sd(t3_ln_il6, na.rm = T))

il6_t3_N_tr

tnf_t3_N_tr<-lab %>%
  subset(t3_ln_tnf!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_tnf_N_tr=n(), mean=mean(t3_ln_tnf, na.rm = T),  sd=sd(t3_ln_tnf, na.rm = T))

tnf_t3_N_tr







#display

igf_t2_N
igf_t3_N

crp_t2_N

agp_t2_N

gmc_t2_N
ifn_t2_N
il10_t2_N
il12_t2_N
il13_t2_N
il17_t2_N
il1_t2_N
il2_t2_N
il21_t2_N
il4_t2_N
il5_t2_N
il6_t2_N
tnf_t2_N

gmc_t3_N
ifn_t3_N
il10_t3_N
il12_t3_N
il13_t3_N
il17_t3_N
il1_t3_N
il2_t3_N
il21_t3_N
il4_t3_N
il5_t3_N
il6_t3_N
tnf_t3_N

#rename to distinguish mine for R compare


igf_t2_N_L<-igf_t2_N
igf_t3_N_L<-igf_t3_N

crp_t2_N_L<-crp_t2_N

agp_t2_N_L<-agp_t2_N

gmc_t2_N_L<-gmc_t2_N
ifn_t2_N_L<-ifn_t2_N
il10_t2_N_L<-il10_t2_N
il12_t2_N_L<-il12_t2_N
il13_t2_N_L<-il13_t2_N
il17_t2_N_L<-il17_t2_N
il1_t2_N_L<-il1_t2_N
il2_t2_N_L<-il2_t2_N
il21_t2_N_L<-il21_t2_N
il4_t2_N_L<-il4_t2_N
il5_t2_N_L<-il5_t2_N
il6_t2_N_L<-il6_t2_N
tnf_t2_N_L<-tnf_t2_N

gmc_t3_N_L<-gmc_t3_N
ifn_t3_N_L<-ifn_t3_N
il10_t3_N_L<-il10_t3_N
il12_t3_N_L<-il12_t3_N
il13_t3_N_L<-il13_t3_N
il17_t3_N_L<-il17_t3_N
il1_t3_N_L<-il1_t3_N
il2_t3_N_L<-il2_t3_N
il21_t3_N_L<-il21_t3_N
il4_t3_N_L<-il4_t3_N
il5_t3_N_L<-il5_t3_N
il6_t3_N_L<-il6_t3_N
tnf_t3_N_L<-tnf_t3_N



#save as Rdata file

save(igf_t2_N_L, igf_t3_N_L, crp_t2_N_L, agp_t2_N_L, gmc_t2_N_L, ifn_t2_N_L, il10_t2_N_L, il12_t2_N_L, il13_t2_N_L, il17_t2_N_L, il1_t2_N_L, il2_t2_N_L, il21_t2_N_L, il4_t2_N_L, il5_t2_N_L, il6_t2_N_L, tnf_t2_N_L, gmc_t3_N_L, ifn_t3_N_L, il10_t3_N_L, il12_t3_N_L, il13_t3_N_L, il17_t3_N_L, il1_t3_N_L, il2_t3_N_L, il21_t3_N_L, il4_t3_N_L, il5_t3_N_L, il6_t3_N_L, tnf_t3_N_L, file="~/Dropbox/WBB-EE-analysis/Results/Audrie/immune_N_means.RData") #Save as R objects for the compare


ages<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)

# re-order the treatment factor for convenience, dropping the arms not included in EE
ages$tr <- factor(ages$tr,levels=c("Control","Nutrition + WSH"))

#calculate N and mean of ages @ t2 overall
ages_bt2_N<-ages %>%
  summarize(agemth_bt2_N=n(), mean=mean(agemth_bt2, na.rm = T), median=median(agemth_bt2, na.rm =T), sd=sd(agemth_bt2, na.rm = T), female=sum(sex=="female"), male=sum(sex=="male"))


#add column to R obj ages_bt2_N
ages_bt2_N["tr"] <- "Overall" 

#display 
ages_bt2_N

#calculate N and mean of ages @ t2 by arm
ages_bt2_N_tr<-ages %>%
  group_by (tr) %>%
  summarize(agemth_bt2_N=n(), mean=mean(agemth_bt2, na.rm = T), median=median(agemth_bt2, na.rm =T), sd=sd(agemth_bt2, na.rm = T), female=sum(sex=="female"), male=sum(sex=="male"))

#display 
ages_bt2_N_tr

#total <- rbind(data frameA, data frameB)
age_bt2_N_total <-rbind2(ages_bt2_N, ages_bt2_N_tr)
age_t2_blood_L<-age_bt2_N_total[c(7, 1, 2, 3, 4, 5, 6)]

ages<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)

# re-order the treatment factor for convenience, dropping the arms not included in immune
ages$tr <- factor(ages$tr,levels=c("Control", "Nutrition + WSH"))

#calculate N and mean of ages @ t3 overall
ages_bt3_N<-ages %>%
  summarize(agemth_bt3_N=n(), mean=mean(agemth_bt3, na.rm = T), median=median(agemth_bt3, na.rm =T), sd=sd(agemth_bt3, na.rm = T), female=sum(sex=="female"), male=sum(sex=="male"))


#add column to R obj ages_bt3_N
ages_bt3_N["tr"] <- "Overall" 

#display 
ages_bt3_N

#calculate N and mean of ages @ t3 by arm
ages_bt3_N_tr<-ages %>%
  group_by (tr) %>%
  summarize(agemth_bt3_N=n(), mean=mean(agemth_bt3, na.rm = T), median=median(agemth_bt3, na.rm =T), sd=sd(agemth_bt3, na.rm = T), female=sum(sex=="female"), male=sum(sex=="male"))

#display 
ages_bt3_N_tr

#total <- rbind(data frameA, data frameB)
age_bt3_N_total <-rbind2(ages_bt3_N, ages_bt3_N_tr)

age_bt3_N_total

age_t3_blood_L<-age_bt3_N_total[c(7, 1, 2, 3, 4, 5, 6)]





#display

age_t2_blood_L
age_t3_blood_L


#save R objects
save(age_t2_blood_L, age_t3_blood_L, file="~/Dropbox/WBB-EE-analysis/Results/Audrie/immune-age-stats.RData")



                           
#glm t2 undjusted

#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in immune
lab$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

# Set up the WASHB function
# df=data frame

washb_function <- function(df,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=NULL, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('t2_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('t2_', names(d))]

#resulting matrix
list_immune


#to save each matrix separately for comparing with Andrew. 

t2_igf_unadj_L<-list_immune$t2_ln_igf
t2_crp_unadj_L<-list_immune$t2_ln_crp
t2_agp_unadj_L<-list_immune$t2_ln_agp
t2_gmc_unadj_L<-list_immune$t2_ln_gmc
t2_ifn_unadj_L<-list_immune$t2_ln_ifn
t2_il10_unadj_L<-list_immune$t2_ln_il10
t2_il12_unadj_L<-list_immune$t2_ln_il12 
t2_il13_unadj_L<-list_immune$t2_t3_ln_il1
t2_il17_unadj_L<-list_immune$t2_ln_il17
t2_il1_unadj_L<-list_immune$t2_ln_il1
t2_il2_unadj_L<-list_immune$t2_ln_il21
t2_il21_unadj_L<-list_immune$t2_ln_il21
t2_il4_unadj_L<-list_immune$t2_ln_il4
t2_il5_unadj_L<-list_immune$t2_ln_il5
t2_il6_unadj_L<-list_immune$t2_ln_il6
t2_tnf_unadj_L<-list_immune$t2_ln_tnf

t2_ratio_gmc_il10_unadj_L<-list_immune$t2_ratio_gmc_il10 
t2_ratio_ifn_il10_unadj_L<-list_immune$t2_ratio_ifn_il10
t2_ratio_il12_il10_unadj_L<-list_immune$t2_ratio_il12_il10 
t2_ratio_il13_il10_unadj_L<-list_immune$t2_ratio_il13_il10 
t2_ratio_il17_il10_unadj_L<-list_immune$t2_ratio_il17_il10 
t2_ratio_il1_il10_unadj_L<-list_immune$t2_ratio_il1_il10 
t2_ratio_il21_il10_unadj_L<-list_immune$t2_ratio_il21_il10 
t2_ratio_il2_il10_unadj_L<-list_immune$t2_ratio_il2_il10
t2_ratio_il4_il10_unadj_L<-list_immune$t2_ratio_il4_il10
t2_ratio_il5_il10_unadj_L<-list_immune$t2_ratio_il5_il10 
t2_ratio_il6_il10_unadj_L<-list_immune$t2_ratio_il6_il10 
t2_ratio_tnf_il10_unadj_L<-list_immune$t2_ratio_tnf_il10 

t2_ratio_il12_il4_unadj_L<-list_immune$t2_ratio_il12_il4 
t2_ratio_ifn_il4_unadj_L<-list_immune$t2_ratio_ifn_il4 
t2_ratio_il12_il5_unadj_L<-list_immune$t2_ratio_il12_il5 
t2_ratio_ifn_il5_unadj_L<-list_immune$t2_ratio_ifn_il5
t2_ratio_il12_il13_unadj_L<-list_immune$t2_ratio_il12_il13
t2_ratio_ifn_il13_unadj_L<-list_immune$t2_ratio_ifn_il13 

t2_ratio_il12_il17_unadj_L<-list_immune$t2_ratio_il12_il17
t2_ratio_ifn_il17_unadj_L<-list_immune$t2_ratio_ifn_il17
t2_ratio_il12_il21_unadj_L<-list_immune$t2_ratio_il12_il21  
t2_ratio_ifn_il21_unadj_L<-list_immune$t2_ratio_ifn_il21

t2_ratio_pro_il10_unadj_L<-list_immune$t2_ratio_pro_il10
t2_ratio_th1_il10_unadj_L<-list_immune$t2_ratio_th1_il10 
t2_ratio_th2_il10_unadj_L<-list_immune$t2_ratio_th2_il10 
t2_ratio_th17_il10_unadj_L<-list_immune$t2_ratio_th17_il10
t2_ratio_th1_th2_unadj_L<-list_immune$t2_ratio_th1_th2 
t2_ratio_th1_th17_unadj_L<-list_immune$t2_ratio_th1_th17

#glm t3 undjusted


#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in immune
lab$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# Set up the WASHB function
# df=data frame

washb_function <- function(df,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=NULL, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('t3_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('t3_', names(d))]

#resulting matrix
list_immune

#to save each matrix separately for comparing with Andrew. 

t3_igf_unadj_L<-list_immune$t3_ln_igf
t3_gmc_unadj_L<-list_immune$t3_ln_gmc
t3_ifn_unadj_L<-list_immune$t3_ln_ifn
t3_il10_unadj_L<-list_immune$t3_ln_il10
t3_il12_unadj_L<-list_immune$t3_ln_il12
t3_il13_unadj_L<-list_immune$t3_t3_ln_il1
t3_il17_unadj_L<-list_immune$t3_ln_il17
t3_il1_unadj_L<-list_immune$t3_ln_il1
t3_il2_unadj_L<-list_immune$t3_ln_il2
t3_il21_unadj_L<-list_immune$t3_ln_il21
t3_il4_unadj_L<-list_immune$t3_ln_il4
t3_il5_unadj_L<-list_immune$t3_ln_il5
t3_il6_unadj_L<-list_immune$t3_ln_il6
t3_tnf_unadj_L<-list_immune$t3_ln_tnf


t3_ratio_gmc_il10_unadj_L<-list_immune$t3_ratio_gmc_il10
t3_ratio_ifn_il10_unadj_L<-list_immune$t3_ratio_ifn_il10
t3_ratio_il12_il10_unadj_L<-list_immune$t3_ratio_il12_il10
t3_ratio_il13_il10_unadj_L<-list_immune$t3_ratio_il13_il10
t3_ratio_il17_il10_unadj_L<-list_immune$t3_ratio_il17_il10
t3_ratio_il1_il10_unadj_L<-list_immune$t3_ratio_il1_il10
t3_ratio_il21_il10_unadj_L<-list_immune$t3_ratio_il21_il10
t3_ratio_il2_il10_unadj_L<-list_immune$t3_ratio_il2_il10
t3_ratio_il4_il10_unadj_L<-list_immune$t3_ratio_il4_il10
t3_ratio_il5_il10_unadj_L<-list_immune$t3_ratio_il5_il10
t3_ratio_il6_il10_unadj_L<-list_immune$t3_ratio_il6_il10
t3_ratio_tnf_il10_unadj_L<-list_immune$t3_ratio_tnf_il10

t3_ratio_il12_il4_unadj_L<-list_immune$t3_ratio_il12_il4 
t3_ratio_ifn_il4_unadj_L<-list_immune$t3_ratio_ifn_il4
t3_ratio_il12_il5_unadj_L<-list_immune$t3_ratio_il12_il5
t3_ratio_ifn_il5_unadj_L<-list_immune$t3_ratio_ifn_il5
t3_ratio_il12_il13_unadj_L<-list_immune$t3_ratio_il12_il13
t3_ratio_ifn_il13_unadj_L<-list_immune$t3_ratio_ifn_il13

t3_ratio_il12_il17_unadj_L<-list_immune$t3_ratio_il12_il17 
t3_ratio_ifn_il17_unadj_L<-list_immune$t3_ratio_ifn_il17
t3_ratio_il12_il21_unadj_L<-list_immune$t3_ratio_il12_il21
t3_ratio_ifn_il21_unadj_L<-list_immune$t3_ratio_ifn_il21

t3_ratio_pro_il10_unadj_L<-list_immune$t3_ratio_pro_il10
t3_ratio_th1_il10_unadj_L<-list_immune$t3_ratio_th1_il10
t3_ratio_th2_il10_unadj_L<-list_immune$t3_ratio_th2_il10
t3_ratio_th17_il10_unadj_L<-list_immune$t3_ratio_th17_il10
t3_ratio_th1_th2_unadj_L<-list_immune$t3_ratio_th1_th2
t3_ratio_th1_th17_unadj_L<-list_immune$t3_ratio_th1_th17



#delta undjusted


#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in immune
lab$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# Set up the WASHB function
# df=data frame

washb_function <- function(df,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=NULL, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 'd23_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('d23_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('d23_', names(d))]

#resulting matrix
list_immune

#to save each matrix separately for comparing with Andrew. 

d23_ln_igf_unadj_L<-list_immune$d23_ln_igf 
d23_ln_gmc_unadj_L<-list_immune$d23_ln_gmc
d23_ln_ifn_unadj_L<-list_immune$d23_ln_ifn 
d23_ln_il10_unadj_L<-list_immune$d23_ln_il10 
d23_ln_il12_unadj_L<-list_immune$d23_ln_il12 
d23_ln_il13_unadj_L<-list_immune$d23_ln_il13 
d23_ln_il17_unadj_L<-list_immune$d23_ln_il17
d23_ln_il1_unadj_L<-list_immune$d23_ln_il1 
d23_ln_il2_unadj_L<-list_immune$d23_ln_il2 
d23_ln_il21_unadj_L<-list_immune$d23_ln_il21 
d23_ln_il4_unadj_L<-list_immune$d23_ln_il4
d23_ln_il5_unadj_L<-list_immune$d23_ln_il5 
d23_ln_il6_unadj_L<-list_immune$d23_ln_il6 
d23_ln_tnf_unadj_L<-list_immune$d23_ln_tnf

d23_ratio_gmc_il10_unadj_L<-list_immune$d23_ratio_gmc_il10  
d23_ratio_ifn_il10_unadj_L<-list_immune$d23_ratio_ifn_il10  
d23_ratio_il12_il10_unadj_L<-list_immune$d23_ratio_il12_il10 
d23_ratio_il13_il10_unadj_L<-list_immune$d23_ratio_il13_il10 
d23_ratio_il17_il10_unadj_L<-list_immune$d23_ratio_il17_il10 
d23_ratio_il1_il10_unadj_L<-list_immune$d23_ratio_il1_il10 
d23_ratio_il21_il10_unadj_L<-list_immune$d23_ratio_il21_il10 
d23_ratio_il2_il10_unadj_L<-list_immune$d23_ratio_il2_il10 
d23_ratio_il4_il10_unadj_L<-list_immune$d23_ratio_il4_il10
d23_ratio_il5_il10_unadj_L<-list_immune$d23_ratio_il5_il10
d23_ratio_il6_il10_unadj_L<-list_immune$d23_ratio_il6_il10 
d23_ratio_tnf_il10_unadj_L<-list_immune$d23_ratio_tnf_il10 

d23_ratio_il12_il4_unadj_L<-list_immune$d23_ratio_il12_il4 
d23_ratio_ifn_il4_unadj_L<-list_immune$d23_ratio_ifn_il4 
d23_ratio_il12_il5_unadj_L<-list_immune$d23_ratio_il12_il5  
d23_ratio_ifn_il5_unadj_L<-list_immune$d23_ratio_ifn_il5
d23_ratio_il12_il13_unadj_L<-list_immune$d23_ratio_il12_il13 
d23_ratio_ifn_il13_unadj_L<-list_immune$d23_ratio_ifn_il13 

d23_ratio_il12_il17_unadj_L<-list_immune$d23_ratio_il12_il17 
d23_ratio_ifn_il17_unadj_L<-list_immune$d23_ratio_ifn_il17 
d23_ratio_il12_il21_unadj_L<-list_immune$d23_ratio_il12_il21 
d23_ratio_ifn_il21_unadj_L<-list_immune$d23_ratio_ifn_il21 

d23_ratio_pro_il10_unadj_L<-list_immune$d23_ratio_pro_il10 
d23_ratio_th1_il10_unadj_L<-list_immune$d23_ratio_th1_il10 
d23_ratio_th2_il10_unadj_L<-list_immune$d23_ratio_th2_il10_unadj  
d23_ratio_th17_il10_unadj_L<-list_immune$d23_ratio_th17_il10
d23_ratio_th1_th2_unadj_L<-list_immune$d23_ratio_th1_th2
d23_ratio_th1_th17_unadj_L<-list_immune$d23_ratio_th1_th17

#Display results


t2_igf_unadj_L 
t2_crp_unadj_L 
t2_agp_unadj_L 
t2_gmc_unadj_L 
t2_ifn_unadj_L 
t2_il10_unadj_L 
t2_il12_unadj_L 
t2_il13_unadj_L 
t2_il17_unadj_L 
t2_il1_unadj_L 
t2_il2_unadj_L 
t2_il21_unadj_L 
t2_il4_unadj_L 
t2_il5_unadj_L 
t2_il6_unadj_L 
t2_tnf_unadj_L

t3_igf_unadj_L
t3_gmc_unadj_L 
t3_ifn_unadj_L 
t3_il10_unadj_L 
t3_il12_unadj_L 
t3_il13_unadj_L 
t3_il17_unadj_L 
t3_il1_unadj_L 
t3_il2_unadj_L 
t3_il21_unadj_L 
t3_il4_unadj_L 
t3_il5_unadj_L 
t3_il6_unadj_L 
t3_tnf_unadj_L

t2_ratio_gmc_il10_unadj_L 
t2_ratio_ifn_il10_unadj_L
t2_ratio_il12_il10_unadj_L 
t2_ratio_il13_il10_unadj_L 
t2_ratio_il17_il10_unadj_L 
t2_ratio_il1_il10_unadj_L 
t2_ratio_il21_il10_unadj_L 
t2_ratio_il2_il10_unadj_L
t2_ratio_il4_il10_unadj_L
t2_ratio_il5_il10_unadj_L 
t2_ratio_il6_il10_unadj_L
t2_ratio_tnf_il10_unadj_L

t2_ratio_il12_il4_unadj_L
t2_ratio_ifn_il4_unadj_L
t2_ratio_il12_il5_unadj_L
t2_ratio_ifn_il5_unadj_L
t2_ratio_il12_il13_unadj_L
t2_ratio_ifn_il13_unadj_L 

t2_ratio_il12_il17_unadj_L
t2_ratio_ifn_il17_unadj_L
t2_ratio_il12_il21_unadj_L  
t2_ratio_ifn_il21_unadj_L

t2_ratio_pro_il10_unadj_L
t2_ratio_th1_il10_unadj_L
t2_ratio_th2_il10_unadj_L
t2_ratio_th17_il10_unadj_L
t2_ratio_th1_th2_unadj_L
t2_ratio_th1_th17_unadj_L

t3_ratio_gmc_il10_unadj_L
t3_ratio_ifn_il10_unadj_L
t3_ratio_il12_il10_unadj_L
t3_ratio_il13_il10_unadj_L
t3_ratio_il17_il10_unadj_L 
t3_ratio_il1_il10_unadj_L 
t3_ratio_il21_il10_unadj_L 
t3_ratio_il2_il10_unadj_L 
t3_ratio_il4_il10_unadj_L 
t3_ratio_il5_il10_unadj_L 
t3_ratio_il6_il10_unadj_L 
t3_ratio_tnf_il10_unadj_L 

t3_ratio_il12_il4_unadj_L  
t3_ratio_ifn_il4_unadj_L 
t3_ratio_il12_il5_unadj_L 
t3_ratio_ifn_il5_unadj_L 
t3_ratio_il12_il13_unadj_L 
t3_ratio_ifn_il13_unadj_L 

t3_ratio_il12_il17_unadj_L  
t3_ratio_ifn_il17_unadj_L 
t3_ratio_il12_il21_unadj_L 
t3_ratio_ifn_il21_unadj_L  

t3_ratio_pro_il10_unadj_L
t3_ratio_th1_il10_unadj_L 
t3_ratio_th2_il10_unadj_L 
t3_ratio_th17_il10_unadj_L 
t3_ratio_th1_th2_unadj_L 
t3_ratio_th1_th17_unadj_L

d23_ln_igf_unadj_L 
d23_ln_gmc_unadj_L
d23_ln_ifn_unadj_L 
d23_ln_il10_unadj_L 
d23_ln_il12_unadj_L 
d23_ln_il13_unadj_L 
d23_ln_il17_unadj_L
d23_ln_il1_unadj_L 
d23_ln_il2_unadj_L 
d23_ln_il21_unadj_L 
d23_ln_il4_unadj_L 
d23_ln_il5_unadj_L 
d23_ln_il6_unadj_L 
d23_ln_tnf_unadj_L

d23_ratio_gmc_il10_unadj_L  
d23_ratio_ifn_il10_unadj_L  
d23_ratio_il12_il10_unadj_L 
d23_ratio_il13_il10_unadj_L 
d23_ratio_il17_il10_unadj_L 
d23_ratio_il1_il10_unadj_L 
d23_ratio_il21_il10_unadj_L 
d23_ratio_il2_il10_unadj_L 
d23_ratio_il4_il10_unadj_L 
d23_ratio_il5_il10_unadj_L
d23_ratio_il6_il10_unadj_L 
d23_ratio_tnf_il10_unadj_L 

d23_ratio_il12_il4_unadj_L 
d23_ratio_ifn_il4_unadj_L 
d23_ratio_il12_il5_unadj_L  
d23_ratio_ifn_il5_unadj_L 
d23_ratio_il12_il13_unadj_L 
d23_ratio_ifn_il13_unadj_L 

d23_ratio_il12_il17_unadj_L 
d23_ratio_ifn_il17_unadj_L 
d23_ratio_il12_il21_unadj_L 
d23_ratio_ifn_il21_unadj_L 

d23_ratio_pro_il10_unadj_L 
d23_ratio_th1_il10_unadj_L 
d23_ratio_th2_il10_unadj_L  
d23_ratio_th17_il10_unadj_L
d23_ratio_th1_th2_unadj_L
d23_ratio_th1_th17_unadj_L



save (t2_igf_unadj_L, 
      t2_crp_unadj_L, 
      t2_agp_unadj_L, 
      t2_gmc_unadj_L, 
      t2_ifn_unadj_L, 
      t2_il10_unadj_L, 
      t2_il12_unadj_L, 
      t2_il13_unadj_L, 
      t2_il17_unadj_L, 
      t2_il1_unadj_L, 
      t2_il2_unadj_L, 
      t2_il21_unadj_L, 
      t2_il4_unadj_L, 
      t2_il5_unadj_L, 
      t2_il6_unadj_L, 
      t2_tnf_unadj_L,
      
      t3_igf_unadj_L,
      t3_gmc_unadj_L, 
      t3_ifn_unadj_L, 
      t3_il10_unadj_L, 
      t3_il12_unadj_L, 
      t3_il13_unadj_L, 
      t3_il17_unadj_L, 
      t3_il1_unadj_L, 
      t3_il2_unadj_L, 
      t3_il21_unadj_L, 
      t3_il4_unadj_L, 
      t3_il5_unadj_L, 
      t3_il6_unadj_L, 
      t3_tnf_unadj_L, 
      
      t2_ratio_gmc_il10_unadj_L, 
      t2_ratio_ifn_il10_unadj_L,
      t2_ratio_il12_il10_unadj_L, 
      t2_ratio_il13_il10_unadj_L,
      t2_ratio_il17_il10_unadj_L, 
      t2_ratio_il1_il10_unadj_L, 
      t2_ratio_il21_il10_unadj_L, 
      t2_ratio_il2_il10_unadj_L,
      t2_ratio_il4_il10_unadj_L,
      t2_ratio_il5_il10_unadj_L, 
      t2_ratio_il6_il10_unadj_L,
      t2_ratio_tnf_il10_unadj_L,
      
      t2_ratio_il12_il4_unadj_L,
      t2_ratio_ifn_il4_unadj_L,
      t2_ratio_il12_il5_unadj_L,
      t2_ratio_ifn_il5_unadj_L,
      t2_ratio_il12_il13_unadj_L,
      t2_ratio_ifn_il13_unadj_L, 
      
      t2_ratio_il12_il17_unadj_L,
      t2_ratio_ifn_il17_unadj_L,
      t2_ratio_il12_il21_unadj_L,  
      t2_ratio_ifn_il21_unadj_L,
      
      t2_ratio_pro_il10_unadj_L,
      t2_ratio_th1_il10_unadj_L,
      t2_ratio_th2_il10_unadj_L,
      t2_ratio_th17_il10_unadj_L,
      t2_ratio_th1_th2_unadj_L,
      t2_ratio_th1_th17_unadj_L, 
      
      t3_ratio_gmc_il10_unadj_L,
      t3_ratio_ifn_il10_unadj_L,
      t3_ratio_il12_il10_unadj_L,
      t3_ratio_il13_il10_unadj_L,
      t3_ratio_il17_il10_unadj_L, 
      t3_ratio_il1_il10_unadj_L, 
      t3_ratio_il21_il10_unadj_L, 
      t3_ratio_il2_il10_unadj_L, 
      t3_ratio_il4_il10_unadj_L, 
      t3_ratio_il5_il10_unadj_L, 
      t3_ratio_il6_il10_unadj_L, 
      t3_ratio_tnf_il10_unadj_L, 
      
      t3_ratio_il12_il4_unadj_L,  
      t3_ratio_ifn_il4_unadj_L, 
      t3_ratio_il12_il5_unadj_L, 
      t3_ratio_ifn_il5_unadj_L, 
      t3_ratio_il12_il13_unadj_L, 
      t3_ratio_ifn_il13_unadj_L, 
      
      t3_ratio_il12_il17_unadj_L,  
      t3_ratio_ifn_il17_unadj_L, 
      t3_ratio_il12_il21_unadj_L, 
      t3_ratio_ifn_il21_unadj_L,  
      
      t3_ratio_pro_il10_unadj_L,
      t3_ratio_th1_il10_unadj_L, 
      t3_ratio_th2_il10_unadj_L, 
      t3_ratio_th17_il10_unadj_L, 
      t3_ratio_th1_th2_unadj_L, 
      t3_ratio_th1_th17_unadj_L,
      
      d23_ln_igf_unadj_L, 
      d23_ln_gmc_unadj_L, 
      d23_ln_ifn_unadj_L, 
      d23_ln_il10_unadj_L, 
      d23_ln_il12_unadj_L, 
      d23_ln_il13_unadj_L, 
      d23_ln_il17_unadj_L, 
      d23_ln_il1_unadj_L, 
      d23_ln_il2_unadj_L, 
      d23_ln_il21_unadj_L, 
      d23_ln_il4_unadj_L, 
      d23_ln_il5_unadj_L, 
      d23_ln_il6_unadj_L, 
      d23_ln_tnf_unadj_L,
      
      d23_ratio_gmc_il10_unadj_L,  
      d23_ratio_ifn_il10_unadj_L,  
      d23_ratio_il12_il10_unadj_L, 
      d23_ratio_il13_il10_unadj_L, 
      d23_ratio_il17_il10_unadj_L, 
      d23_ratio_il1_il10_unadj_L, 
      d23_ratio_il21_il10_unadj_L, 
      d23_ratio_il2_il10_unadj_L, 
      d23_ratio_il4_il10_unadj_L, 
      d23_ratio_il5_il10_unadj_L, 
      d23_ratio_il6_il10_unadj_L, 
      d23_ratio_tnf_il10_unadj_L, 
      
      d23_ratio_il12_il4_unadj_L, 
      d23_ratio_ifn_il4_unadj_L, 
      d23_ratio_il12_il5_unadj_L,  
      d23_ratio_ifn_il5_unadj_L, 
      d23_ratio_il12_il13_unadj_L, 
      d23_ratio_ifn_il13_unadj_L, 
      
      d23_ratio_il12_il17_unadj_L, 
      d23_ratio_ifn_il17_unadj_L, 
      d23_ratio_il12_il21_unadj_L, 
      d23_ratio_ifn_il21_unadj_L, 
      
      d23_ratio_pro_il10_unadj_L, 
      d23_ratio_th1_il10_unadj_L, 
      d23_ratio_th2_il10_unadj_L,  
      d23_ratio_th17_il10_unadj_L, 
      d23_ratio_th1_th2_unadj_L, 
      d23_ratio_th1_th17_unadj_L,
      
      file="~/Dropbox/WBB-EE-analysis/Results/Audrie/immune_unadj_glm.RData")

