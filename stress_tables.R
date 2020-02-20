rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

load(here("andrew results/stress_results.RData"))


bonpval <- function(pval){
  bon = round(pval * 2, 2)
  if (pval >= .5)
    bon = 1
  bon 
}

#to be used for formatting ipcw variables for table
ci_interval<-function(str, tbl){
  filter<-tbl[tbl$Y == str,]
  paste(round(filter[1], 2), " (", round(filter[3], 2), ", ", round(filter[4], 2), ")", sep="")
}

#mean
mean <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[3], 2))
}
mean("t2_f2_8ip", "Control", mean_sd_tr)

outcomes2<-c("iPF(2α)-III", "Control", "Nutrition + WSH", "2,3-dinor-iPF(2α)-III", 
              "Control", "Nutrition + WSH", "iPF(2α)-VI", "Control", "Nutrition + WSH", "8,12-iso-iPF(2α)-VI", 
              "Control","Nutrition + WSH")

unadj_diff <-c("","", ci_interval("t2_f2_8ip", res_unadj), "","", 
                          ci_interval("t2_f2_23d", res_unadj), "","",ci_interval("t2_f2_VI", res_unadj), "","",
                          ci_interval("t2_f2_12i", res_unadj))

age_sex_adj <- c("","", ci_interval("t2_f2_8ip", res_sex),"","", 
                 ci_interval("t2_f2_23d", res_sex), "","",ci_interval("t2_f2_VI", res_sex), "","",
                 ci_interval("t2_f2_12i", res_sex))

full_adj <- c("","", ci_interval("t2_f2_8ip", res_adj),"","", 
              ci_interval("t2_f2_23d", res_adj), "","",ci_interval("t2_f2_VI", res_adj), "","",
              ci_interval("t2_f2_12i", res_adj))

mean_tr <- c("", mean("t2_f2_8ip", "Control", mean_sd_tr), mean("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
             mean("t2_f2_23d", "Control", mean_sd_tr), mean("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_VI", "Control", mean_sd_tr),mean("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_12i", "Control", mean_sd_tr), mean("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

tbls2 <- data.table(
  "Outcome" = outcomes2,
  "Unadjusted Analysis" = unadj_diff, 
  "Age and Sex Adjusted Analysis" = age_sex_adj,
  "Fully Adjusted Analysis" = full_adj,
  "Mean" = mean_tr
)

outcomes3<-c("Pre-stressor Salivary alpha-amylase" ,"Control", "Nutrition + WSH",
             "Post-stressor Salivary alpha-amylase","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor alpha-amylase","Control", "Nutrition + WSH",
             "Residualized gain score for alpha-amylase","Control", "Nutrition + WSH",
             "Pre-stressor salivary cortisol","Control", "Nutrition + WSH",
             "Post-stressor salivary cortisol","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor cortisol","Control", "Nutrition + WSH",
             "Residualized gain score for cortisol","Control", "Nutrition + WSH",
             "Mean arterial Pressure","Control", "Nutrition + WSH",
             "Resting heart rate","Control", "Nutrition + WSH",
             "NR3C1 exon 1F promoter methylation","Control", "Nutrition + WSH",
             "NGFI-A transcription factor binding site","Control", "Nutrition + WSH"
)

unadj_diff3 <-c("","", ci_interval("t3_saa_z01", res_unadj), "","", 
               ci_interval("t3_saa_z02", res_unadj), "","",ci_interval("t3_saa_slope", res_unadj), "","",
               ci_interval("t3_residual_saa", res_unadj),
               "","",ci_interval("t3_cort_z01", res_unadj),"","",ci_interval("t3_cort_z03", res_unadj),
               "","",ci_interval("t3_cort_slope", res_unadj),"","",ci_interval("t3_residual_cort", res_unadj),
               "","",ci_interval("t3_map", res_unadj),"","",ci_interval("t3_hr_mean", res_unadj),
               "","",ci_interval("t3_gcr_mean", res_unadj),"","",ci_interval("t3_gcr_cpg12", res_unadj))

age_sex_adj3 <- c("","", ci_interval("t3_saa_z01", res_sex), "","", 
                  ci_interval("t3_saa_z02", res_sex), "","",ci_interval("t3_saa_slope", res_sex), "","",
                  ci_interval("t3_residual_saa", res_sex),
                  "","",ci_interval("t3_cort_z01", res_sex),"","",ci_interval("t3_cort_z03", res_sex),
                  "","",ci_interval("t3_cort_slope", res_sex),"","",ci_interval("t3_residual_cort", res_sex),
                  "","",ci_interval("t3_map", res_sex),"","",ci_interval("t3_hr_mean", res_sex),
                  "","",ci_interval("t3_gcr_mean", res_sex),"","",ci_interval("t3_gcr_cpg12", res_sex))

full_adj3 <- c("","", ci_interval("t3_saa_z01", res_adj), "","", 
               ci_interval("t3_saa_z02", res_adj), "","",ci_interval("t3_saa_slope", res_adj), "","",
               ci_interval("t3_residual_saa", res_adj),
               "","",ci_interval("t3_cort_z01", res_adj),"","",ci_interval("t3_cort_z03", res_adj),
               "","",ci_interval("t3_cort_slope", res_adj),"","",ci_interval("t3_residual_cort", res_adj),
               "","",ci_interval("t3_map", res_adj),"","",ci_interval("t3_hr_mean", res_adj),
               "","",ci_interval("t3_gcr_mean", res_adj),"","",ci_interval("t3_gcr_cpg12", res_adj))

tbls3 <- data.table(
  "Outcome" = outcomes3,
  "Unadjusted Analysis" = unadj_diff3, 
  
  "Age and Sex Adjusted Analysis" = age_sex_adj3,
  "Full Adjusted Analysis" = full_adj3
)





