
rm(list=ls())
source(here::here("0-config.R"))
library(car)


# #load stress outcomes
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/washb-bangladesh-dm-ee-vital-saa-cortisol-f2-gcr-residuals.RDS"))



#------------------------------------------------------------------------------------------------
# Check distribution of outcomes
# Cutoff of abs(skewness) > 1 for transformation
# https://stats.stackexchange.com/questions/245835/range-of-values-of-skewness-and-kurtosis-for-normal-distribution
#------------------------------------------------------------------------------------------------

plotdf <- bind_rows(
      data.frame(Y=d$t3_map, outcome="Mean arterial pressure"),
      data.frame(Y=d$t3_hr_mean, outcome="mean heart rate"),
      data.frame(Y=d$t3_saa_z01, outcome="Pre-stress SSA"),
      data.frame(Y=d$t3_saa_z02, outcome="Post-stress SSA"),
      data.frame(Y=d$t3_cort_z01, outcome="Pre-stress cortisol"),
      data.frame(Y=d$t3_cort_z03, outcome="Post-stress cortisol"),
      data.frame(Y=d$t2_f2_8ip, outcome="ipf 81p"),
      data.frame(Y=d$t2_f2_23d, outcome="ipf 23d"),
      data.frame(Y=d$t2_f2_VI, outcome="ipf VI"),
      data.frame(Y=d$t2_f2_12i, outcome="ipf l2i"),
      data.frame(Y=d$t3_gcr_mean, outcome="GRC mean"),
      data.frame(Y=d$t3_gcr_cpg12, outcome="GRC cpg12"),
      data.frame(Y=d$t3_saa_slope, outcome="saa slope"),
      data.frame(Y=d$t3_cort_slope, outcome="cort slope"),
      data.frame(Y=d$t3_residual_saa, outcome="ssa residual score"),
      data.frame(Y=d$t3_residual_cort, outcome="cort residual score"))
        

#Check skewness
library(e1071)  
skewness <- plotdf %>% group_by(outcome) %>%
  do(as.data.frame(e1071::skewness(.$Y, na.rm=T)))
skewness

#Plot skewness
p <- ggplot(plotdf, aes(x=Y)) + geom_density() + facet_wrap(~outcome, scales = "free")
                                                        
             
ggsave(p, file = here::here("figures/stress/raw_outcome_distributions.png"), height=6, width=14)                                                  
  

#---------------------------------------------------------------------------------------------
# transform outcome distributions
#---------------------------------------------------------------------------------------------
d <- d %>% 
  mutate(
    t3_saa_z01_raw=t3_saa_z01, 
    t3_saa_z02_raw=t3_saa_z02, 
    t3_cort_z01_raw=t3_cort_z01, 
    t3_cort_z03_raw=t3_cort_z03, 
    t2_f2_8ip_raw=t2_f2_8ip, 
    t2_f2_23d_raw=t2_f2_23d, 
    t2_f2_VI_raw=t2_f2_VI,
    t2_f2_12i_raw=t2_f2_12i, 
    t3_gcr_mean_raw=t3_gcr_mean, 
    t3_gcr_cpg12_raw=t3_gcr_cpg12,
    t3_saa_z01=log(t3_saa_z01), 
    t3_saa_z02=log(t3_saa_z02), 
    t3_cort_z01=log(t3_cort_z01), 
    t3_cort_z03=log(t3_cort_z03), 
    t2_f2_8ip=log(t2_f2_8ip), 
    t2_f2_23d=log(t2_f2_23d), 
    t2_f2_VI=log(t2_f2_VI),
    t2_f2_12i=log(t2_f2_12i), 
    t3_gcr_mean=logit(t3_gcr_mean), 
    t3_gcr_cpg12=logit(t3_gcr_cpg12))
  

plotdf <- bind_rows(
  data.frame(Y=d$t3_map, outcome="Mean arterial pressure"),
  data.frame(Y=d$t3_hr_mean, outcome="mean heart rate"),
  data.frame(Y=d$t3_saa_z01, outcome="Pre-stress SSA"),
  data.frame(Y=d$t3_saa_z02, outcome="Post-stress SSA"),
  data.frame(Y=d$t3_cort_z01, outcome="Pre-stress cortisol"),
  data.frame(Y=d$t3_cort_z03, outcome="Post-stress cortisol"),
  data.frame(Y=d$t2_f2_8ip, outcome="ipf 81p"),
  data.frame(Y=d$t2_f2_23d, outcome="ipf 23d"),
  data.frame(Y=d$t2_f2_VI, outcome="ipf VI"),
  data.frame(Y=d$t2_f2_12i, outcome="ipf l2i"),
  data.frame(Y=d$t3_gcr_mean, outcome="GRC mean"),
  data.frame(Y=d$t3_gcr_cpg12, outcome="GRC cpg12"),
  data.frame(Y=d$t3_saa_slope, outcome="saa slope"),
  data.frame(Y=d$t3_cort_slope, outcome="cort slope"),
  data.frame(Y=d$t3_residual_saa, outcome="ssa residual score"),
  data.frame(Y=d$t3_residual_cort, outcome="cort residual score"))

trans_skewness <- plotdf %>% group_by(outcome) %>%
  do(as.data.frame(e1071::skewness(.$Y, na.rm=T)))
trans_skewness

p <- ggplot(plotdf, aes(x=Y)) + geom_density() + facet_wrap(~outcome, scales = "free")
ggsave(p, file = here::here("figures/stress/transformed_outcome_distributions.png"), height=6, width=14)                                                  


#---------------------------------------------------------------------------------------------
# drop unneeded/intermediate variables
#---------------------------------------------------------------------------------------------

d <- d %>% subset(., select = -c(t3_hr1, t3_hr2, t3_hr3, t3_sysbp1, t3_diasbp1, t3_sysbp2,
                                 t3_diasbp2, t3_sysbp3, t3_diasbp3, t3_sysbp_mean, t3_diasbp_mean,
                                 t3_z01_time, t3_z02_time, t3_z03_time, t3_cort_min_elaps, t3_saa_min_elaps, t3_gcr_stdev))


#---------------------------------------------------------------------------------------------
# merge covariates
#---------------------------------------------------------------------------------------------


d$childid <- as.numeric(d$childid)
fulld <- read.csv(paste0(dropboxDir,"Data/Cleaned/Andrew/EE-BD_fulldata.csv"))

dim(fulld)
dim(d)
df <- left_join(fulld, d, by="childid")
dim(df)


#---------------------------------------------------------------------------------------------
# (temporary) blind treatment assignment
#---------------------------------------------------------------------------------------------
df$tr[1:10]
table(df$tr)
set.seed(12345)
df$tr <- sample(df$tr, nrow(df))
df$tr[1:10]
table(df$tr)

#---------------------------------------------------------------------------------------------
# save data
#---------------------------------------------------------------------------------------------

saveRDS(df, file=paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))


#---------------------------------------------------------------------------------------------
# merge Audrie's covariates to check replication
#---------------------------------------------------------------------------------------------


# dfull <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))
# 
# dim(dfull)
# dim(d)
# df <- left_join(dfull, d, by="childid")
# dim(df)
# 
# 
# df$tr[1:10]
# table(df$tr)
# set.seed(12345)
# df$tr <- sample(df$tr, nrow(df))
# df$tr[1:10]
# table(df$tr)
# 
# 
# saveRDS(df, file=paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))



