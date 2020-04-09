rm(list=ls())
library("xtable")
source(here::here("0-config.R"))


bonpval <- function(pval){
  bon = round(pval * 2, 2)
  if (pval >= .5)
    bon = 1
  bon 
}

#to be used for formatting ipcw variables for table
ci_interval<-function(str, tbl){
  filter<-tbl[tbl$var == str,]
  paste(round(filter[1], 3), " (", round(filter[3], 3), ", ", round(filter[4], 3), ")", sep="")
}

#mean
mean <- function(str, str1, tbl){
  filter <- tbl[tbl$tr == str1,]
  paste(round(filter[3], 2))
}

#sd
sd <- function(str, str1, tbl){
  filter <- tbl[tbl$tr == str1,]
  paste(round(filter[4], 2))
}

#n
n <- function(str, str1, tbl){
  filter <- tbl[tbl$tr == str1,]
  paste(round(filter[2], 2))
}


outcomes4<-c("iPF(2α)-III ", "Control", "Nutrition + WSH", "2,3-dinor-iPF(2α)-III", 
             "Control", "Nutrition + WSH", "iPF(2α)-VI", "Control", "Nutrition + WSH", "8,12-iso-iPF(2α)-VI", 
             "Control","Nutrition + WSH")

load("~/ee-secondary/audrie results/stress_N_means.RData")

n_t4 <- c("", n("t2_f2_8ip", "Control", t2_f2_8ip_N_tr), n("t2_f2_8ip", "Nutrition+WSH", t2_f2_8ip_N_tr),"",
          n("t2_f2_23d", "Control", t2_f2_23d_N_tr), n("t2_f2_23d", "Nutrition+WSH", t2_f2_23d_N_tr), "",
          n("t2_f2_VI", "Control", t2_f2_VI_N_tr),n("t2_f2_VI", "Nutrition+WSH", t2_f2_VI_N_tr), "",
          n("t2_f2_12i", "Control", t2_f2_12i_N_tr), n("t2_f2_12i", "Nutrition+WSH", t2_f2_12i_N_tr))

mean_tr <- c("", mean("t2_f2_8ip", "Control", t2_f2_8ip_N_tr), mean("t2_f2_8ip", "Nutrition+WSH", t2_f2_8ip_N_tr),"",
             mean("t2_f2_23d", "Control", t2_f2_23d_N_tr), mean("t2_f2_23d", "Nutrition+WSH", t2_f2_23d_N_tr), "",
             mean("t2_f2_VI", "Control", t2_f2_VI_N_tr),mean("t2_f2_VI", "Nutrition+WSH", t2_f2_VI_N_tr), "",
             mean("t2_f2_12i", "Control", t2_f2_12i_N_tr), mean("t2_f2_12i", "Nutrition+WSH", t2_f2_12i_N_tr))

sd_t4 <- c("", sd("t2_f2_8ip", "Control", t2_f2_8ip_N_tr), sd("t2_f2_8ip", "Nutrition+WSH", t2_f2_8ip_N_tr),"",
           sd("t2_f2_23d", "Control", t2_f2_23d_N_tr), sd("t2_f2_23d", "Nutrition+WSH", t2_f2_23d_N_tr), "",
           sd("t2_f2_VI", "Control", t2_f2_VI_N_tr),sd("t2_f2_VI", "Nutrition+WSH", t2_f2_VI_N_tr), "",
           sd("t2_f2_12i", "Control", t2_f2_12i_N_tr), sd("t2_f2_12i", "Nutrition+WSH", t2_f2_12i_N_tr))

load("~/ee-secondary/audrie results/stress_unadj_glm.RData")

unadj_diff <-c("","", ci_interval("t2_f2_8ip", unadj_stress_t2), "","", 
               ci_interval("t2_f2_23d", unadj_stress_t2), "","",ci_interval("t2_f2_VI", unadj_stress_t2), "","",
               ci_interval("t2_f2_12i", unadj_stress_t2))

load("~/ee-secondary/audrie results/stress_adj_sex_age_glm.RData")

ci_interval_adj<-function(str, tbl){
  filter<-tbl[str,]
  paste(round(filter[1], 2), " (", round(filter[3], 2), ", ", round(filter[4], 2), ")", sep="")
}

age_sex_adj <- c("","", ci_interval_adj("t2_ipf2a3", adj_age_sex_t2),"","", 
                 ci_interval_adj("t2_23dinor", adj_age_sex_t2), "","",ci_interval_adj("t2_ipf2a6", adj_age_sex_t2), "","",
                 ci_interval_adj("t2_812iso", adj_age_sex_t2))

load("~/ee-secondary/andrew results/stress_results.Rdata")

absmean <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[3], 2))
}



tbls4 <- data.table(
  "Outcome" = outcomes4,
  "N" = n_t4,
  "Absolute Mean" = 0,
  "Mean" = mean_tr,
  "Standard Deviation" = sd_t4,
  "Unadjusted Analysis" = unadj_diff, 
  "Age and Sex Adjusted Analysis" = age_sex_adj,
  "Fully Adjusted Analysis" =0
)

outcomes6<-c("Pre-stressor Salivary alpha-amylase" ,"Control", "Nutrition + WSH",
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

n_t6 <- c("", n("t3_saa_z01", "Control", t3_saa_z01_N_tr), n("t3_saa_z01", "Nutrition+WSH", t3_saa_z01_N_tr),"",
          n("t3_saa_z02", "Control", t3_saa_z02_N_tr), n("t3_saa_z02", "Nutrition+WSH", t3_saa_z02_N_tr), "",
          n("t3_saa_slope", "Control", t3_saa_slope_N_tr),n("t3_saa_slope", "Nutrition+WSH", t3_saa_slope_N_tr), "",
          n("t3_residual_saa", "Control", t3_residual_saa_N_tr), n("t3_residual_saa", "Nutrition+WSH", t3_residual_saa_N_tr), "",
          n("t3_cort_z01", "Control", t3_cort_z01_N_tr),n("t3_cort_z01", "Nutrition+WSH", t3_cort_z01_N_tr), "",
          n("t3_cort_z03", "Control", t3_cort_z03_tr),n("t3_cort_z03", "Nutrition+WSH", t3_cort_z03_tr), "",
          n("t3_cort_slope", "Control", t3_cort_slope_N_tr),n("t3_cort_slope", "Nutrition+WSH", t3_cort_slope_N_tr), "",
          n("t3_residual_cort", "Control", t3_residual_cort_N_tr),n("t3_residual_cort", "Nutrition+WSH", t3_residual_cort_N_tr), "",
          n("t3_map", "Control", t3_map_N_tr),n("t3_map", "Nutrition+WSH", t3_map_N_tr), "",
          n("t3_hr", "Control", t3_hr_mean_N_tr),n("t3_hr", "Nutrition+WSH", t3_hr_mean_N_tr), "",
          n("t3_gcr", "Control", t3_gcr_mean_N_tr),n("t3_gcr", "Nutrition+WSH", t3_gcr_mean_N_tr), "",
          n("t3_gcr_cpg12", "Control", t3_gcr_cpg12_N_tr), n("t3_gcr_cpg12", "Nutrition+WSH", t3_gcr_cpg12_N_tr))

sd_t6 <- c("", sd("t3_saa_z01", "Control", t3_saa_z01_N_tr), sd("t3_saa_z01", "Nutrition+WSH", t3_saa_z01_N_tr),"",
           sd("t3_saa_z02", "Control", t3_saa_z02_N_tr), sd("t3_saa_z02", "Nutrition+WSH", t3_saa_z02_N_tr), "",
           sd("t3_saa_slope", "Control", t3_saa_slope_N_tr), sd("t3_saa_slope", "Nutrition+WSH", t3_saa_slope_N_tr), "",
           sd("t3_residual_saa", "Control", t3_residual_saa_N_tr), sd("t3_residual_saa", "Nutrition+WSH", t3_residual_saa_N_tr), "",
           sd("t3_cort_z01", "Control", t3_cort_z01_N_tr), sd("t3_cort_z01", "Nutrition+WSH", t3_cort_z01_N_tr), "",
           sd("t3_cort_z03", "Control", t3_cort_z03_tr), sd("t3_cort_z03", "Nutrition+WSH", t3_cort_z03_tr), "",
           sd("t3_cort_slope", "Control", t3_cort_slope_N_tr), sd("t3_cort_slope", "Nutrition+WSH", t3_cort_slope_N_tr), "",
           sd("t3_residual_cort", "Control", t3_residual_cort_N_tr),sd("t3_residual_cort", "Nutrition+WSH", t3_residual_cort_N_tr), "",
           sd("t3_map", "Control", t3_map_N_tr), sd("t3_map", "Nutrition+WSH", t3_map_N_tr), "",
           sd("t3_hr", "Control", t3_hr_mean_N_tr), sd("t3_hr", "Nutrition+WSH", t3_hr_mean_N_tr), "",
           sd("t3_gcr", "Control", t3_gcr_mean_N_tr), sd("t3_gcr", "Nutrition+WSH", t3_gcr_mean_N_tr), "",
           sd ("t3_gcr_cpg12", "Control", t3_gcr_cpg12_N_tr), sd("t3_gcr_cpg12", "Nutrition+WSH", t3_gcr_cpg12_N_tr))

mean_t6 <- c("", mean("t3_saa_z01", "Control", t3_saa_z01_N_tr), mean("t3_saa_z01", "Nutrition+WSH", t3_saa_z01_N_tr),"",
             mean("t3_saa_z02", "Control", t3_saa_z02_N_tr), mean("t3_saa_z02", "Nutrition+WSH", t3_saa_z02_N_tr), "",
             mean("t3_saa_slope", "Control", t3_saa_slope_N_tr), mean("t3_saa_slope", "Nutrition+WSH", t3_saa_slope_N_tr), "",
             mean("t3_residual_saa", "Control", t3_residual_saa_N_tr), mean("t3_residual_saa", "Nutrition+WSH", t3_residual_saa_N_tr), "",
             mean("t3_cort_z01", "Control", t3_cort_z01_N_tr),mean("t3_cort_z01", "Nutrition+WSH", t3_cort_z01_N_tr), "",
             mean("t3_cort_z03", "Control", t3_cort_z03_tr), mean("t3_cort_z03", "Nutrition+WSH", t3_cort_z03_tr), "",
             mean("t3_cort_slope", "Control", t3_cort_slope_N_tr), mean("t3_cort_slope", "Nutrition+WSH", t3_cort_slope_N_tr), "",
             mean("t3_residual_cort", "Control", t3_residual_cort_N_tr),mean("t3_residual_cort", "Nutrition+WSH", t3_residual_cort_N_tr), "",
             mean("t3_map", "Control", t3_map_N_tr),mean("t3_map", "Nutrition+WSH", t3_map_N_tr), "",
             mean("t3_hr", "Control", t3_hr_mean_N_tr), mean("t3_hr", "Nutrition+WSH", t3_hr_mean_N_tr), "",
             mean("t3_gcr", "Control", t3_gcr_mean_N_tr),mean("t3_gcr", "Nutrition+WSH", t3_gcr_mean_N_tr), "",
             mean("t3_gcr_cpg12", "Control", t3_gcr_cpg12_N_tr), mean("t3_gcr_cpg12", "Nutrition+WSH", t3_gcr_cpg12_N_tr))

unadj <- c("","", ci_interval("t3_saa_z01", unadj_stress_t3), "","", 
           ci_interval("t3_saa_z02", unadj_stress_t3), "","",
           ci_interval("t3_saa_slope", unadj_stress_t3), "","",
           ci_interval("t3_residual_saa", unadj_stress_t3), "","",
           ci_interval("t3_cort_z01", unadj_stress_t3), "","",
           ci_interval("t3_cort_z03", unadj_stress_t3), "","",
           ci_interval("t3_cort_slope", unadj_stress_t3), "","",
           ci_interval("t3_residual_cort", unadj_stress_t3),"","",
           ci_interval("t3_map", unadj_stress_t3),"","",
           ci_interval("t3_hr_mean", unadj_stress_t3), "","",
           ci_interval("t3_gcr_mean", unadj_stress_t3), "","",
           ci_interval("t3_gcr_cpg12", unadj_stress_t3))

age_sex_adj_6 <- c("","", ci_interval_adj("t3_pre_saa", adj_age_sex_t3), "","", 
                   ci_interval_adj("t3_post_saa", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_saa_slope", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_residual_saa", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_pre_cort", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_post_cort", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_cort_slope", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_residual_cort", adj_age_sex_t3),"","",
                   ci_interval_adj("t3_map", adj_age_sex_t3),"","",
                   ci_interval_adj("t3_heart", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_nr3c1", adj_age_sex_t3), "","",
                   ci_interval_adj("t3_cpg12", adj_age_sex_t3))


tbls6 <- data.table(
  "Outcome" = outcomes6,
  "N" = n_t6,
  "Absolute Mean" = 0,
  "Mean" = mean_t6,
  "Standard Deviation" = sd_t6,
  "Unadjusted Analysis" = unadj, 
  "Age and Sex Adjusted Analysis" = 0,
  "Full Adjusted Analysis" = 0
)

write.csv(tbls4, here('tables/stress/miso9-stress-supplementary-table4.csv'))
write.csv(tbls6, here('tables/stress/miso9-stress-supplementary-table6.csv'))







