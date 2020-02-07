
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
# replication objects/simulated_stress_dataset.rds
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

#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
lab<-readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset.RDS"))
table(lab$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in stress
lab$tr <- factor(lab$tr,levels=c("Control","Nutrition + WSH"))

# functions for overall N and mean, N and mean by sex, N and mean by arm
overall <- function(var) {
  lab %>%
    subset(lab[[var]]!="NA") %>%
    summarize(overall=n(), mean=mean(lab[[var]], na.rm = T),  sd=sd(lab[[var]], na.rm = T))
}

groupsex<-function(var){
  tbl<-data.frame(variable=var, sex=lab$sex)
  female<-tbl%>%subset(sex==0)
  male<-tbl%>%subset(sex==1)
  data.frame(sex=c("female", "male"), 
             n=c(nrow(female), nrow(male)),
             mean=c(mean(female$variable, na.rm=TRUE), mean(male$variable, na.rm=TRUE)),
             sd=c(sd(female$variable, na.rm=TRUE), sd(male$variable, na.rm=TRUE)))
}

grouptr<-function(var){
  tbl<-data.frame(variable=var, tr=lab$tr)
  ctrl<-tbl%>%subset(tr=="Control")
  wsh<-tbl%>%subset(tr=="Nutrition + WSH")
  data.frame(tr=c("Control", "Nutrition+WSH"), 
             n=c(nrow(ctrl), nrow(wsh)),
             mean=c(mean(ctrl$var, na.rm=TRUE), mean(wsh$var, na.rm=TRUE)),
             sd=c(sd(ctrl$var, na.rm=TRUE), sd(wsh$var, na.rm=TRUE)))
}


              "","","","","","")


#calculate overall N's and means at t2 & t3
t2_f2_8ip_N<-overall("t2_f2_8ip")
t2_f2_23d_N<-overall("t2_f2_23d")
t2_f2_VI_N<-overall("t2_f2_VI")
t2_f2_12i_N<-overall("t2_f2_12i")
t3_saa_z01_N<-overall("t3_saa_z01")
t3_cort_z01_N<-overall("t3_cort_z01")
t3_saa_z02_N<-overall("t3_saa_z02")
t3_cort_z03_N<-overall("t3_cort_z03")
t3_map_N<-overall("t3_map")
t3_hr_mean_N<-overall("t3_hr_mean")
t3_gcr_mean_N<-overall("t3_gcr_mean")
t3_gcr_cpg12_N<-overall("t3_gcr_cpg12")

t3_saa_slope_N<-overall("t3_saa_slope")
t3_cort_slope_N<-overall("t3_cort_slope")
t3_residual_saa_N<-overall("t3_residual_saa")
t3_residual_cort_N<-overall("t3_residual_cort")


#calculate N's and mean of biomarkers at t2 & t3 by sex
t2_f2_8ip_N_sex<-groupsex(lab$"t2_f2_8ip")
t2_f2_23d_N_sex<-groupsex(lab$"t2_f2_23d")
t2_f2_VI_N_sex<-groupsex(lab$"t2_f2_VI")
t2_f2_12i_N_sex<-groupsex(lab$"t2_f2_12i")
t3_saa_z01_N_sex<-groupsex(lab$"t3_saa_z01")
t3_cort_z01_N_sex<-groupsex(lab$"t3_cort_z01")
t3_saa_z02_N_sex<-groupsex(lab$"t3_saa_z02")
t3_cort_z03_sex<-groupsex(lab$"t3_cort_z03")
t3_map_N_sex<-groupsex(lab$"t3_map")
t3_dia_N_sex<-groupsex(lab$"t3_dia")
t3_hr_mean_N_sex<-groupsex(lab$"t3_hr_mean")
t3_gcr_mean_N_sex<-groupsex(lab$"t3_gcr_mean")
t3_gcr_cpg12_N_sex<-groupsex(lab$"t3_gcr_cpg12")
t3_saa_slope_N_sex<-groupsex("t3_saa_slope")
t3_cort_slope_N_sex<-groupsex("t3_cort_slope")
t3_residual_saa_N_sex<-groupsex("t3_residual_saa")
t3_residual_cort_N_sex<-groupsex("t3_residual_cort")


#calculate N's and mean of stress biomarkers t2 and t3 by arm
t2_f2_8ip_N_tr<-grouptr(lab$"t2_f2_8ip")
t2_f2_23d_N_tr<-grouptr(lab$"t2_f2_23d")
t2_f2_VI_N_tr<-grouptr(lab$"t2_f2_VI")
t2_f2_12i_N_tr<-grouptr(lab$"t2_f2_12i")
t3_saa_z01_N_tr<-grouptr(lab$"t3_saa_z01")
t3_cort_z01_N_tr<-grouptr(lab$"t3_cort_z01")
t3_saa_z02_N_tr<-grouptr(lab$"t3_saa_z02")
t3_cort_z03_tr<-grouptr(lab$"t3_cort_z03")
t3_map_N_tr<-grouptr(lab$"t3_map")
t3_dia_N_tr<-grouptr(lab$"t3_dia")
t3_hr_mean_N_tr<-grouptr(lab$"t3_hr_mean")
t3_gcr_mean_N_tr<-grouptr(lab$"t3_gcr_mean")
t3_gcr_cpg12_N_tr<-grouptr(lab$"t3_gcr_cpg12")
t3_saa_slope_N_tr<-grouptr("t3_saa_slope")
t3_cort_slope_N_tr<-grouptr("t3_cort_slope")
t3_residual_saa_N_tr<-grouptr("t3_residual_saa")
t3_residual_cort_N_tr<-grouptr("t3_residual_cort")

#display
t2_f2_8ip_N
t2_f2_23d_N
t2_f2_VI_N
t2_f2_12i_N
t3_saa_z01_N
t3_cort_z01_N
t3_saa_z02_N
t3_cort_z03_N
t3_map_N
t3_dia_N
t3_hr_mean_N
t3_gcr_mean_N
t3_gcr_cpg12_N


#rename to distinguish mine for R compare
t2_f2_8ip_N_L<-t2_f2_8ip_N
t2_f2_23d_N_L<-t2_f2_23d_N
t2_f2_VI_N_L<-t2_f2_VI_N
t2_f2_12i_N_L<-t2_f2_12i_N
t3_saa_z01_N_L<-t3_saa_z01_N
t3_cort_z01_N_L<-t3_cort_z01_N
t3_saa_z02_N_L<-t3_saa_z02_N
t3_cort_z03_N_L<-t3_cort_z03_N
t3_map_N_L<-t3_map_N
t3_dia_N_L<-t3_dia_N
t3_hr_mean_N_L<-t3_hr_mean_N
t3_gcr_mean_N_L<-t3_gcr_mean_N
t3_gcr_cpg12_N_L<-t3_gcr_cpg12_N

t3_saa_slope_N_L<-t3_saa_slope
t3_cort_slope_N_L<-t3_cort_slope
t3_residual_saa_N_L<-t3_residual_saa
t3_residual_cort_N_L<-t3_residual_cort


#save as Rdata file

save(t2_f2_8ip_N_L,
     t2_f2_23d_N_L,
     t2_f2_VI_N_L,
     t2_f2_12i_N_L,
     t3_saa_z01_N_L,
     t3_cort_z01_N_L,
     t3_saa_z02_N_L,
     t3_cort_z03_N_L,
     t3_map_N_L,
     t3_dia_N_L,
     t3_hr_mean_N_L,
     t3_gcr_mean_N_L,
     t3_gcr_cpg12_N_L,
     t3_saa_slope_N_L,
     t3_cort_slope_N_L,
     t3_residual_saa_N_L,
     t3_residual_cort_N_L,
     file=here("audrie results/stress_N_means.RData")) #Save as R objects for the compare


ages<-readRDS(here("replication objects/simulated_stress_dataset.rds"))

# re-order the treatment factor for convenience, dropping the arms not included in EE
ages$tr <- factor(ages$tr,levels=c("Control","Nutrition + WSH"))

#calculate N and mean of ages @ t2 overall
ages_bt2_N<-ages %>%
  summarize(agemth_bt2_N=n(), mean=mean(agemth_bt2, na.rm = T), median=median(agemth_bt2, na.rm =T), sd=sd(agemth_bt2, na.rm = T), female=sum(sex==0), male=sum(sex==1))


#add column to R obj ages_bt2_N
ages_bt2_N["tr"] <- "Overall" 

#display 
ages_bt2_N

#calculate N and mean of ages @ t2 by arm
ages_bt2_N_tr<-ages %>%
  group_by (tr) %>%
  summarize(agemth_bt2_N=n(), mean=mean(agemth_bt2, na.rm = T), median=median(agemth_bt2, na.rm =T), sd=sd(agemth_bt2, na.rm = T), female=sum(sex==0), male=sum(sex==1))

#display 
ages_bt2_N_tr

#total <- rbind(data frameA, data frameB)
age_bt2_N_total <-rbind2(ages_bt2_N, ages_bt2_N_tr)
age_t2_blood_L<-age_bt2_N_total[c(7, 1, 2, 3, 4, 5, 6)]

ages<-readRDS(here("replication objects/simulated_stress_dataset.rds"))

# re-order the treatment factor for convenience, dropping the arms not included in stress
ages$tr <- factor(ages$tr,levels=c("Control", "Nutrition + WSH"))

#calculate N and mean of ages @ t3 overall
ages_bt3_N<-ages %>%
  summarize(agemth_bt3_N=n(), mean=mean(agemth_bt3, na.rm = T), median=median(agemth_bt3, na.rm =T), sd=sd(agemth_bt3, na.rm = T), female=sum(sex==0), male=sum(sex==1))


#add column to R obj ages_bt3_N
ages_bt3_N["tr"] <- "Overall" 

#display 
ages_bt3_N

#calculate N and mean of ages @ t3 by arm
ages_bt3_N_tr<-ages %>%
  group_by (tr) %>%
  summarize(agemth_bt3_N=n(), mean=mean(agemth_bt3, na.rm = T), median=median(agemth_bt3, na.rm =T), sd=sd(agemth_bt3, na.rm = T), female=sum(sex==0), male=sum(sex==1))

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




#tmle t2 undjusted

#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-readRDS(here("replication objects/simulated_stress_dataset.rds"))
table(d$tr) #crosstab of numbers in each treatment

# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

# subset to columns needed for unadjusted 
df = d[,c("block", "tr","t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t3_saa_z01", "t3_cort_z01",
          "t3_saa_z02", "t3_cort_z03", "t3_map", "t3_dia", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=NULL, id=df$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  return(temp_metric)
}


#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_stress <- lapply(names(df)[grep('t2_', names(df))],  function(x) washb_function(df,x))

#put names of each of the variables into the matrix
names(list_stress) <- names(df)[grep('t2_', names(df))]

#Compile into data.frame for easier comparison in replication
unadj_stress_t2 <- t(bind_rows(list_stress))
colnames(unadj_stress_t2) <-c("RD","var","ci.lb","ci.ub","P-value")
unadj_stress_t2 <- as.data.frame(unadj_stress_t2)
unadj_stress_t2$var <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")

#view results file
unadj_stress_t2




#tmlet3 undjusted



#Load in enrollment data,blinded tr data, stool data for adjusted analysis. Use read.dta() to read the .dta files, or read.csv() to 
#read .csv files. Use stringAsFactors=TRUE so that any character-based variable will be read in as a factor.
d<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
table(d$tr) #crosstab of numbers in each treatment


# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 
df = d[,c("block", "tr", "t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t3_saa_z01", "t3_cort_z01",
          "t3_saa_z02", "t3_cort_z03", "t3_map", "t3_dia", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")



#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress <- lapply(names(df)[grep('t3_', names(df))],  function(x) washb_function(df,x))

#put names of each of the variables into the matrix
names(list_stress) <- names(df)[grep('t3_', names(df))]

#Compile into data.frame for easier comparison in replication
unadj_stress_t3 <- t(bind_rows(list_stress))
colnames(unadj_stress_t3) <-c("RD","var","ci.lb","ci.ub","P-value")
unadj_stress_t3 <- as.data.frame(unadj_stress_t3)
unadj_stress_t3$var <- c("t3_saa_z01", "t3_cort_z01", "t3_saa_z02", "t3_cort_z03", "t3_map", "t3_dia", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12")

#view results file
unadj_stress_t3




#Save results files
save(unadj_stress_t2, 
     
      unadj_stress_t3,
      
      
      file=here("audrie results/stress_unadj_glm.RData"))

