
#---------------------------------------
# bangladesh-stress-ages-unadj-analysis.R
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
# stress_N_means.RData (N's and means of stress data)
# stress_unadj_glm.RData (unadj glm of stress data)
#---------------------------------------

#Clear out R environment (remove and loaded data)
rm(list=ls())


######################
###Load in packages
######################

source(here::here("0-config.R"))


library(washb)
library(arm)
library(lme4)
library(gam)
library(glmnet)
set.seed(12345)

######################
###Load in data
######################

#Set working directory to load in blinded treatment assignment and enrolment information
setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/")) #Set working directory

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


#calculate N's and mean of biomarkers at t2 by sex

igf_t2_N_sex<-lab %>%
  subset(t2_ln_igf!="NA") %>%
  group_by (sex) %>%
  summarize(t2_ln_igf_N_sex=n(), mean=mean(t2_ln_igf, na.rm = T),  sd=sd(t2_ln_igf, na.rm = T))

igf_t2_N_sex


  
#calculate N's and mean of immune biomarkers t2 by arm


igf_t2_N_tr<-lab %>%
  subset(t2_ln_igf!="NA") %>%
  group_by (tr) %>%
  summarize(t2_ln_igf_N_tr=n(), mean=mean(t2_ln_igf, na.rm = T),  sd=sd(t2_ln_igf, na.rm = T))

igf_t2_N_tr



#calculate absolute mean of biomarkers at t2 by arm
absmean<-function(a){
  tbl<-data.frame(var=a, tr=lab$tr)
  ctrl<-tbl%>%subset(tr=="Control")
  wsh<-tbl%>%subset(tr=="Nutrition + WSH")
  data.frame(tr=c("Control", "Nutrition+WSH"), 
             mean=c(mean(ctrl$var, na.rm=TRUE), mean(wsh$var, na.rm=TRUE)),
             sd=c(sd(ctrl$var, na.rm=TRUE), sd(wsh$var, na.rm=TRUE)))
}


abs_igf_t2_N_tr<-absmean(lab$igf_t2)


#calculate absolute means of biomarkers at t3 by arm

abs_igf_t3_N_tr<-absmean(lab$igf_t3)

#calculate N's and mean of biomarkers at t3 by arm
igf_t3_N_tr<-lab %>%
  subset(t3_ln_igf!="NA") %>%
  group_by (tr) %>%
  summarize(t3_ln_igf_N_tr=n(), mean=mean(t3_ln_igf, na.rm = T),  sd=sd(t3_ln_igf, na.rm = T))

igf_t3_N_tr


#display

igf_t2_N
igf_t3_N

#rename to distinguish mine for R compare


igf_t2_N_L<-igf_t2_N
igf_t3_N_L<-igf_t3_N



#save as Rdata file

save(igf_t2_N_L, igf_t3_N_L, crp_t2_N_L, agp_t2_N_L, gmc_t2_N_L, ifn_t2_N_L, il10_t2_N_L, il12_t2_N_L, il13_t2_N_L, il17_t2_N_L, il1_t2_N_L, il2_t2_N_L, il21_t2_N_L, il4_t2_N_L, il5_t2_N_L, il6_t2_N_L, tnf_t2_N_L, gmc_t3_N_L, ifn_t3_N_L, il10_t3_N_L, il12_t3_N_L, il13_t3_N_L, il17_t3_N_L, il1_t3_N_L, il2_t3_N_L, il21_t3_N_L, il4_t3_N_L, il5_t3_N_L, il6_t3_N_L, tnf_t3_N_L, file=here("audrie results/immune_N_means.RData")) #Save as R objects for the compare


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
save(age_t2_blood_L, age_t3_blood_L, file=here("audrie results/stress-age-stats.RData"))



                           
#glm t2 undjusted

#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 
df = d[,c("block", "tr","[insert list of stress variables here]")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=NULL, id=df$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
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




#glm t3 undjusted



#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 
df = d[,c("block", "tr","[insert list of stress variables here]")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=NULL, id=df$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
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




#Display results


t2_igf_unadj_L 

t3_igf_unadj_L





save (t2_igf_unadj_L, 
     
      t3_igf_unadj_L,
      
      
      file=here("audrie results/stress_unadj_glm.RData"))

