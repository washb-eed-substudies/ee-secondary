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

#sd
sd <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[4], 2))
}


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

sd_t2 <- c("", sd("t2_f2_8ip", "Control", mean_sd_tr), sd("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
           sd("t2_f2_23d", "Control", mean_sd_tr), sd("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_VI", "Control", mean_sd_tr),sd("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_12i", "Control", mean_sd_tr), sd("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

abs_mean <- c("", mean("t2_f2_8ip_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_8ip_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
              mean("t2_f2_23d_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_23d_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_VI_raw", "Control", absolute_mean_sd_tr),mean("t2_f2_VI_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_12i_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_12i_raw", "Nutrition + WSH", absolute_mean_sd_tr))

tbls2 <- data.table(
  "Outcome" = outcomes2,
  "Absolute Mean" = abs_mean,
  "Mean" = mean_tr,
  "Standard Deviation" = sd_t2,
  "Unadjusted Analysis" = unadj_diff, 
  "Age and Sex Adjusted Analysis" = age_sex_adj,
  "Fully Adjusted Analysis" = full_adj
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

mean_tr3 <- c("", mean("t3_saa_z01", "Control", mean_sd_tr), mean("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
             mean("t3_saa_z02", "Control", mean_sd_tr), mean("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_saa_slope", "Control", mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_residual_saa", "Control", mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_cort_z01", "Control", mean_sd_tr),mean("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_cort_z03", "Control", mean_sd_tr),mean("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_cort_slope", "Control", mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_residual_cort", "Control", mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_map", "Control", mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_hr", "Control", mean_sd_tr),mean("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_gcr", "Control", mean_sd_tr),mean("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
             mean("t3_gcr_cpg12", "Control", mean_sd_tr),mean("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

sd_t3 <- c("", sd("t3_saa_z01", "Control", mean_sd_tr), sd("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
           sd("t3_saa_z02", "Control", mean_sd_tr), sd("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_saa_slope", "Control", mean_sd_tr),sd("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_saa", "Control", mean_sd_tr), sd("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z01", "Control", mean_sd_tr),sd("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z03", "Control", mean_sd_tr),sd("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_slope", "Control", mean_sd_tr),sd("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_cort", "Control", mean_sd_tr),sd("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_map", "Control", mean_sd_tr),sd("t3_map", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_hr", "Control", mean_sd_tr),sd("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr", "Control", mean_sd_tr),sd("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr_cpg12", "Control", mean_sd_tr), sd("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

abs_mean_t3 <- c("", mean("t3_saa_z01_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
                 mean("t3_saa_z02_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z02_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_saa_slope", "Control", absolute_mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_saa", "Control", absolute_mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z01_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z03_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z03_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_slope", "Control", absolute_mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_cort", "Control", absolute_mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_map", "Control", absolute_mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
                 mean("t3_hr", "Control", absolute_mean_sd_tr),mean("t3_hr", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_cpg12_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_cpg12_raw", "Nutrition + WSH", absolute_mean_sd_tr) )


tbls3 <- data.table(
  "Outcome" = outcomes3,
  "Absolute Mean" = abs_mean_t3,
  "Mean" = mean_tr3,
  "Standard Deviation" = sd_t3,
  "Unadjusted Analysis" = unadj_diff3, 
  "Age and Sex Adjusted Analysis" = age_sex_adj3,
  "Full Adjusted Analysis" = full_adj3
)





