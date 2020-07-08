
rm(list=ls())
source(here::here("0-config.R"))
library(car)
library(e1071)  


# #load stress outcomes
#d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/washb-bangladesh-dm-ee-vital-saa-cortisol-f2-gcr-residuals.RDS"))
d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/washb-bangladesh-dm-ee-enrol-vital-saa-cortisol-f2-gcr.csv"))



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
colnames(d)

#---------------------------------------------------------------------------------------------
# merge covariates
#---------------------------------------------------------------------------------------------


d$childid <- as.numeric(d$childid)
fulld <- read.csv(paste0(dropboxDir,"Data/Cleaned/Andrew/EE-BD_fulldata.csv"))

#subset to needed variables
fulld <- fulld %>% subset(., select = c(childid,sex,birthord,tr))#, wall:hfias))

dim(fulld)
dim(d)
df <- left_join(fulld, d, by="childid")
dim(df)

#drop treatment arms where outcome was not samples
df <- df %>% filter(tr %in% c("Control","Nutrition + WSH"))

df$tr <- factor(df$tr, levels = c("Control","Nutrition + WSH"))


#---------------------------------------------------------------------------------------------
# clean time-dependent covariates
#---------------------------------------------------------------------------------------------

# samplecoldate_t3_vital = the date for heart rate and blood pressure measurements
# samplecoldate_t3_oragene = the date for gcr measurements
# samplecoldate_t3_salimetrics = the date for saa and cortisol measurements
# The urine collection date can be used for the 4 urinary f2-isoprostanes biomarkers.

colnames(df)
library(lubridate)
df$samplecoldate_t3_vital <- dmy(df$samplecoldate_t3_vital)
df$samplecoldate_t3_salimetrics <- dmy(df$samplecoldate_t3_salimetrics)
df$samplecoldate_t3_oragene <- dmy(df$samplecoldate_t3_oragene)

summary(as.numeric(df$samplecoldate_t3_vital-df$samplecoldate_t3_oragene))
summary(as.numeric(df$samplecoldate_t3_salimetrics-df$samplecoldate_t3_vital))
summary(as.numeric(df$samplecoldate_t3_oragene-df$samplecoldate_t3_vital))

#Add in measure-specific monsoon status
df <- df %>% mutate(monsoon3_vital = ifelse(month(samplecoldate_t3_vital) > 4 & month(samplecoldate_t3_vital) < 11, "1", "0"),
                    monsoon3_oragene = ifelse(month(samplecoldate_t3_oragene) > 4 & month(samplecoldate_t3_oragene) < 11, "1", "0"),
                    monsoon3_salimetrics = ifelse(month(samplecoldate_t3_salimetrics) > 4 & month(samplecoldate_t3_salimetrics) < 11, "1", "0"),
                    monsoon3_vital = ifelse(is.na(monsoon3_vital),"missing", monsoon3_vital),
                    monsoon3_oragene = ifelse(is.na(monsoon3_oragene),"missing", monsoon3_oragene),
                    monsoon3_salimetrics = ifelse(is.na(monsoon3_salimetrics),"missing", monsoon3_salimetrics),
                    monsoon3_vital = factor(monsoon3_vital),
                    monsoon3_oragene = factor(monsoon3_oragene),
                    monsoon3_salimetrics = factor(monsoon3_salimetrics))

table(df$monsoon3_oragene)
table(df$monsoon3_vital)
table(df$monsoon3_salimetrics)


#Calculate age at measurement time
# df$DOB <- dmy(df$DOB)
# df$vital_aged3 <- (as.numeric(df$samplecoldate_t3_vital-df$DOB))
# df$salimetrics_aged3 <- (as.numeric(df$samplecoldate_t3_salimetrics-df$DOB))
# df$oragene_aged3 <- (as.numeric(df$samplecoldate_t3_oragene-df$DOB))
# 
# summary(df$vital_aged3 - df$ur_aged3)
# summary(df$salimetrics_aged3 - df$ur_aged3)
# summary(df$oragene_aged3 - df$ur_aged3)

#rename variables to match old format
df <- df %>%
  rename(
    vital_aged3=ageday_t3_vital,
    salimetrics_aged3=ageday_t3_salimetrics,
    oragene_aged3=ageday_t3_oragene,
    wall=walls
  )


#---------------------------------------------------------------------------------------------
# (temporary) blind treatment assignment
#---------------------------------------------------------------------------------------------
# df$tr[1:10]
# table(df$tr)
# set.seed(12345)
# df$tr <- sample(df$tr, nrow(df))
# df$tr[1:10]
# table(df$tr)

#---------------------------------------------------------------------------------------------
# save data
#---------------------------------------------------------------------------------------------


saveRDS(df, file=paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))




#---------------------------------------------------------------------------------------------
# save IPCW data
#---------------------------------------------------------------------------------------------

colnames(df)
#df2 <- df %>% subset(., select = -c(sex,birthord,urineVol_t1:mann.rec.MMOL_t3, wall:hfias))
df2 <- df %>% subset(., select = -c(sex,birthord, wall:hfias))
colnames(df2)

saveRDS(df2, file=paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_IPCW_dataset_andrew.RDS"))


