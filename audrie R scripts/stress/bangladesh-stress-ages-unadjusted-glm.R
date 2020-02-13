
#---------------------------------------
# bangladesh-stress-ur_ages-unadj-analysis.R
#
# audrie lin (audrielin@berkeley.edu)
#
# EE in the WASH B packur_age 
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
###Load in packur_ages
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
lab$tr <- factor(lab$tr,levels=c("Control","Nutrition","WSH" ,"Nutrition + WSH"))

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
t3_hr_mean_N_sex<-groupsex(lab$"t3_hr_mean")
t3_gcr_mean_N_sex<-groupsex(lab$"t3_gcr_mean")
t3_gcr_cpg12_N_sex<-groupsex(lab$"t3_gcr_cpg12")
t3_saa_slope_N_sex<-groupsex(lab$"t3_saa_slope")
t3_cort_slope_N_sex<-groupsex(lab$"t3_cort_slope")
t3_residual_saa_N_sex<-groupsex(lab$"t3_residual_saa")
t3_residual_cort_N_sex<-groupsex(lab$"t3_residual_cort")


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
t3_hr_mean_N_tr<-grouptr(lab$"t3_hr_mean")
t3_gcr_mean_N_tr<-grouptr(lab$"t3_gcr_mean")
t3_gcr_cpg12_N_tr<-grouptr(lab$"t3_gcr_cpg12")
t3_saa_slope_N_tr<-grouptr(lab$"t3_saa_slope")
t3_cort_slope_N_tr<-grouptr(lab$"t3_cort_slope")
t3_residual_saa_N_tr<-grouptr(lab$"t3_residual_saa")
t3_residual_cort_N_tr<-grouptr(lab$"t3_residual_cort")

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
t3_hr_mean_N_L<-t3_hr_mean_N
t3_gcr_mean_N_L<-t3_gcr_mean_N
t3_gcr_cpg12_N_L<-t3_gcr_cpg12_N
t3_saa_slope_N_L<-t3_saa_slope_N
t3_cort_slope_N_L<-t3_cort_slope_N
t3_residual_saa_N_L<-t3_residual_saa_N
t3_residual_cort_N_L<-t3_residual_cort_N


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
     t3_hr_mean_N_L,
     t3_gcr_mean_N_L,
     t3_gcr_cpg12_N_L,
     t3_saa_slope_N_L,
     t3_cort_slope_N_L,
     t3_residual_saa_N_L,
     t3_residual_cort_N_L,
     t2_f2_8ip_N_tr, 
     t2_f2_23d_N_tr, 
     t2_f2_VI_N_tr,
     t2_f2_12i_N_tr, 
     t3_saa_z01_N_tr, 
     t3_cort_z01_N_tr,
     t3_saa_z02_N_tr,
     t3_cort_z03_tr,
     t3_map_N_tr, 
     t3_hr_mean_N_tr, 
     t3_gcr_mean_N_tr, 
     t3_gcr_cpg12_N_tr,
     t3_saa_slope_N_tr, 
     t3_cort_slope_N_tr,
     t3_residual_saa_N_tr, 
     t3_residual_cort_N_tr, 
     file=here::here("audrie results/stress_N_means.RData")) #Save as R objects for the compare


ur_ages<-lab %>% rename(ur_agemth_bt2=ur_agem2, ur_agemth_bt3=ur_agem3)


#calculate N and mean of ur_ages @ t2 overall
ur_ages_bt2_N<-ur_ages %>%
  summarize(ur_agemth_bt2_N=n(), mean=mean(ur_agemth_bt2, na.rm = T), median=median(ur_agemth_bt2, na.rm =T), sd=sd(ur_agemth_bt2, na.rm = T), female=sum(sex==0), male=sum(sex==1))


#add column to R obj ur_ages_bt2_N
ur_ages_bt2_N["tr"] <- "Overall" 

#display 
ur_ages_bt2_N

#calculate N and mean of ur_ages @ t2 by arm
ur_ages_bt2_N_tr<-ur_ages %>%
  group_by (tr) %>%
  summarize(ur_agemth_bt2_N=n(), mean=mean(ur_agemth_bt2, na.rm = T), median=median(ur_agemth_bt2, na.rm =T), sd=sd(ur_agemth_bt2, na.rm = T), female=sum(sex==0), male=sum(sex==1))

#display 
ur_ages_bt2_N_tr

#total <- rbind(data frameA, data frameB)
ur_age_bt2_N_total <-rbind2(ur_ages_bt2_N, ur_ages_bt2_N_tr)
stress_age_t2_L<-ur_age_bt2_N_total[c(7, 1, 2, 3, 4, 5, 6)]


#calculate N and mean of ur_ages @ t3 overall
ur_ages_bt3_N<-ur_ages %>%
  summarize(ur_agemth_bt3_N=n(), mean=mean(ur_agemth_bt3, na.rm = T), median=median(ur_agemth_bt3, na.rm =T), sd=sd(ur_agemth_bt3, na.rm = T), female=sum(sex==0), male=sum(sex==1))


#add column to R obj ur_ages_bt3_N
ur_ages_bt3_N["tr"] <- "Overall" 

#display 
ur_ages_bt3_N

#calculate N and mean of ur_ages @ t3 by arm
ur_ages_bt3_N_tr<-ur_ages %>%
  group_by (tr) %>%
  summarize(ur_agemth_bt3_N=n(), mean=mean(ur_agemth_bt3, na.rm = T), median=median(ur_agemth_bt3, na.rm =T), sd=sd(ur_agemth_bt3, na.rm = T), female=sum(sex==0), male=sum(sex==1))

#display 
ur_ages_bt3_N_tr

#total <- rbind(data frameA, data frameB)
ur_age_bt3_N_total <-rbind2(ur_ages_bt3_N, ur_ages_bt3_N_tr)

ur_age_bt3_N_total

stress_age_t3_L<-ur_age_bt3_N_total[c(7, 1, 2, 3, 4, 5, 6)]


#display
stress_age_t2_L
stress_age_t3_L


#save R objects
save(stress_age_t2_L, stress_age_t3_L, file=here::here("audrie results/stress-age-stats.RData"))




#tmle t2 undjusted
# subset to columns needed for unadjusted 
df = lab[,c("block", "tr","t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t3_saa_z01", "t3_cort_z01",
          "t3_saa_z02", "t3_cort_z03", "t3_map", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12",
          "t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")]
df$block=as.factor(df$block)




# Set up the WASHB function
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
unadj_stress_t2$var <- names(df)[grep('t2_', names(df))]

#view results file
unadj_stress_t2




#tmle t3 undjusted

#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress <- lapply(names(df)[grep('t3_', names(df))],  function(x) washb_function(df,x))

#put names of each of the variables into the matrix
names(list_stress) <- names(df)[grep('t3_', names(df))]

#Compile into data.frame for easier comparison in replication
unadj_stress_t3 <- t(bind_rows(list_stress))
colnames(unadj_stress_t3) <-c("RD","var","ci.lb","ci.ub","P-value")
unadj_stress_t3 <- as.data.frame(unadj_stress_t3)
unadj_stress_t3$var <- names(df)[grep('t3_', names(df))]
#view results file
unadj_stress_t3


saveRDS(df,here::here("replication objects/audrie_stress_object.RDS"))

#Save results files
save(unadj_stress_t2, 
      unadj_stress_t3,
      file=here::here("audrie results/stress_unadj_glm.RData"))

