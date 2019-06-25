#---------------------------------------
# quartileTMLE_telo_growth.R
#
# audrie lin (audrielin@berkeley.edu)
#
# Summarize unadjusted and adjusted mean growth outcomes by quartiles of telomere length. 
# Calculate means within each quartile and estimate adjusted means differences 
# between quartiles using TMLE
#---------------------------------------

#---------------------------------------

# input files
# 
# ~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv (from 3-bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.do)
# "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData (from ~/ee-secondary/audrie R scripts/observational/0-file_conversion.R)
#
# output files:
#	
# 
# 
#---------------------------------------




#---------------------------------------
# preamble
#---------------------------------------

rm(list=ls())
source(here::here("0-config.R"))

source("~/ee-secondary/audrie R scripts/observational/0-base-quartileTMLE_functions.R")



#load covariates, exposures, outcomes dataset
load("~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData")

#check if variables are factors/numeric
for(i in 1:ncol(d)){
  cat(colnames(d)[i],"  ",class(d[,i]),"\n")
}
#set variables as factors/numeric
d$sex<-as.factor(d$sex)
d$sex <- factor(d$sex, labels = c("female", "male"))
d$birthord<-as.factor(d$birthord)
d$momage<-as.numeric(d$momage)
d$momheight<-as.numeric(d$momheight)
d$momedu<-as.factor(d$momedu)
d$hfiacat<-as.factor(d$hfiacat)
d$Nlt18<-as.numeric(d$Nlt18)
d$Ncomp<-as.numeric(d$Ncomp)
d$watmin<-as.numeric(d$watmin)
d$floor<-as.factor(d$floor)
d$walls<-as.factor(d$walls)
d$elec<-as.factor(d$elec)
d$asset_wardrobe<-as.factor(d$asset_wardrobe)
d$asset_table<-as.factor(d$asset_table)
d$asset_chair<-as.factor(d$asset_chair)
d$asset_clock<-as.factor(d$asset_clock)
d$asset_khat<-as.factor(d$asset_khat)
d$asset_chouki<-as.factor(d$asset_chouki)
d$asset_radio<-as.factor(d$asset_radio)
d$asset_tv<-as.factor(d$asset_tv)
d$asset_refrig<-as.factor(d$asset_refrig)
d$asset_bike<-as.factor(d$asset_bike)
d$asset_moto<-as.factor(d$asset_moto)
d$asset_sewmach<-as.factor(d$asset_sewmach)
d$asset_mobile<-as.factor(d$asset_mobile)
d$n_cattle<-as.numeric(d$n_cattle)
d$n_goat<-as.numeric(d$n_goat)
d$n_chicken<-as.numeric(d$n_chicken)

d$lenhei_med_t2<-as.numeric(d$lenhei_med_t2)
d$weight_med_t2<-as.numeric(d$weight_med_t2)

d$monsoon_ht2<-as.factor(d$monsoon_ht2)
d$monsoon_ht2<-addNA(d$monsoon_ht2)
levels(d$monsoon_ht2)[length(levels(d$monsoon_ht2))]<-"Missing"

d$monsoon_ht3<-as.factor(d$monsoon_ht3)
d$monsoon_ht3<-addNA(d$monsoon_ht3)
levels(d$monsoon_ht3)[length(levels(d$monsoon_ht3))]<-"Missing"

d$ageday_ht2<-as.numeric(d$ageday_ht2)
d$ageday_ht3<-as.numeric(d$ageday_ht3)

d$anthro_days_btwn_t2_t3<-as.numeric(d$anthro_days_btwn_t2_t3)

d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

d$cesd_sum_t2<-as.numeric(d$cesd_sum_t2)
d$cesd_sum_ee_t3<-as.numeric(d$cesd_sum_ee_t3)
d$pss_sum_mom_t3<-as.numeric(d$pss_sum_mom_t3)

d$diar7d_t2<-as.factor(d$diar7d_t2)
d$diar7d_t2<-addNA(d$diar7d_t2)
levels(d$diar7d_t2)[length(levels(d$diar7d_t2))]<-"Missing"

d$diar7d_t3<-as.factor(d$diar7d_t3)
d$diar7d_t3<-addNA(d$diar7d_t3)
levels(d$diar7d_t3)[length(levels(d$diar7d_t3))]<-"Missing"

d$life_viol_any_t3<-as.factor(d$life_viol_any_t3)
d$life_viol_any_t3<-addNA(d$life_viol_any_t3)
levels(d$life_viol_any_t3)[length(levels(d$life_viol_any_t3))]<-"Missing"



#Hypothesis 1
#The change in telomere length between Year 1 and Year 2 is associated with the change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Year 1 to Year 2.
#Exposure: Quartiles of change in telomere length between Year 1 and Year 2
#Outcome: Change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Year 1 to Year 2

#unadjusted, with continuous outcome

#Null data.frame
h1unadj.res = NULL

h1unadj.res <- tmle_quart(dat=d, 
                          Y="delta_laz_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h1unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h1unadj.res <- tmle_quart(dat=d, 
                          Y="delta_waz_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h1unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h1unadj.res <- tmle_quart(dat=d, 
                          Y="delta_whz_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h1unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h1unadj.res <- tmle_quart(dat=d, 
                          Y="delta_hcz_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h1unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h1unadj.res 

#adjusted



#null dataframe
h1adj.res = NULL 

Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2", "anthro_days_btwn_t2_t3")



h1adj.res <- tmle_quart(dat=d, 
                        Y="delta_laz_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h1adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h1adj.res <- tmle_quart(dat=d, 
                        Y="delta_waz_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h1adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h1adj.res <- tmle_quart(dat=d, 
                        Y="delta_whz_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h1adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h1adj.res <- tmle_quart(dat=d, 
                        Y="delta_hcz_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h1adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h1adj.res



#Hypothesis 2
#The change in telomere length between Year 1 and Year 2 is associated with growth velocity (kg/month or cm/month) between Year 1 and Year 2.
#Exposure: Quartiles of change in telomere length between Year 1 and Year 2
#Outcome: Child weight velocity (in kg/month), length velocity (in cm/month), and head circumference velocity (in cm/month) from Year 1 to Year 2

#unadjusted
#Null data.frame
h2unadj.res = NULL

h2unadj.res <- tmle_quart(dat=d, 
                          Y="len_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h2unadj.res <- tmle_quart(dat=d, 
                          Y="wei_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h2unadj.res <- tmle_quart(dat=d, 
                          Y="hc_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h2unadj.res

#adjusted

#null dataframe
h2adj.res = NULL 

Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2", "anthro_days_btwn_t2_t3")



h2adj.res <- tmle_quart(dat=d, 
                        Y="len_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2adj.res <- tmle_quart(dat=d, 
                        Y="wei_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2adj.res <- tmle_quart(dat=d, 
                        Y="hc_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2adj.res




#Hypothesis 3
#The change in telomere length between Year 1 and Year 2 is associated with child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.
#Exposure: Quartiles of change in telomere length between Year 1 and Year 2
#Outcome: Child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2


#unadjusted, with continuous outcome

#Null data.frame
h3unadj.res = NULL

h3unadj.res <- tmle_quart(dat=d, 
                          Y="laz_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h3unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h3unadj.res <- tmle_quart(dat=d, 
                          Y="waz_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h3unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h3unadj.res <- tmle_quart(dat=d, 
                          Y="whz_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h3unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h3unadj.res <- tmle_quart(dat=d, 
                          Y="hcz_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h3unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h3unadj.res

#adjusted

#null dataframe
h3adj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")



h3adj.res <- tmle_quart(dat=d, 
                        Y="laz_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h3adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h3adj.res <- tmle_quart(dat=d, 
                        Y="waz_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h3adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h3adj.res <- tmle_quart(dat=d, 
                        Y="whz_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h3adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h3adj.res <- tmle_quart(dat=d, 
                        Y="hcz_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h3adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h3adj.res


#Hypothesis 4
#Telomere length measured at Year 1 is associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1.
#Exposure: Quartiles of telomere lengths at Year 1
#Outcome: Child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1

#Unadjusted, with continuous outcome

#Null data.frame
h4unadj.res = NULL

h4unadj.res <- tmle_quart(dat=d, 
                  Y="laz_t2", 
                  W=NULL, 
                  A="TS_t2", 
                  id="block",
                  Alevels=c("Q1","Q2","Q3","Q4"),
                  outputdf = h4unadj.res,
                  family="gaussian", 
                  SLlibrary="SL.gam")


h4unadj.res <- tmle_quart(dat=d, 
                       Y="waz_t2", 
                       W=NULL, 
                       A="TS_t2", 
                       id="block",
                       Alevels=c("Q1","Q2","Q3","Q4"),
                       outputdf = h4unadj.res,
                       family="gaussian", 
                       SLlibrary="SL.gam")

h4unadj.res <- tmle_quart(dat=d, 
                       Y="whz_t2", 
                       W=NULL, 
                       A="TS_t2", 
                       id="block",
                       Alevels=c("Q1","Q2","Q3","Q4"),
                       outputdf = h4unadj.res,
                       family="gaussian", 
                       SLlibrary="SL.gam")

h4unadj.res <- tmle_quart(dat=d, 
                       Y="hcz_t2", 
                       W=NULL, 
                       A="TS_t2", 
                       id="block",
                       Alevels=c("Q1","Q2","Q3","Q4"),
                       outputdf = h4unadj.res,
                       family="gaussian", 
                       SLlibrary="SL.gam")

h4unadj.res




#Adjusted, with continuous outcome
#outcome = Y 
#exposure = A 

#Null data.frame
h4adj.res = NULL

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", "floor", 
         "walls", "elec", "asset_wardrobe", "asset_table", 
         "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach",
         "asset_mobile", "n_cattle", "n_goat", "n_chicken",
         "monsoon_ht2", "ageday_ht2", "tr", "cesd_sum_t2", "diar7d_t2", 
         "life_viol_any_t3")

h4adj.res <- tmle_quart(dat=d, 
                      Y="laz_t2", 
                      W=Wvars, 
                      A="TS_t2", 
                      id="block",
                      Alevels=c("Q1","Q2","Q3","Q4"),
                      outputdf = h4adj.res,
                      family="gaussian", 
                      SLlibrary="SL.gam")



h4adj.res <- tmle_quart(dat=d, 
                           Y="waz_t2", 
                           W=Wvars, 
                           A="TS_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h4adj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h4adj.res <- tmle_quart(dat=d, 
                           Y="whz_t2", 
                           W=Wvars, 
                           A="TS_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h4adj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")



h4adj.res <- tmle_quart(dat=d, 
                           Y="hcz_t2", 
                           W=Wvars, 
                           A="TS_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h4adj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")
h4adj.res

#Hypothesis 5
#Telomere length measured at Year 2 is associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.
#Exposure: Quartiles of telomere lengths at Year 2
#Outcome: Child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2



#unadjusted
#Null data.frame
h5unadj.res = NULL

h5unadj.res <- tmle_quart(dat=d, 
                          Y="laz_t3", 
                          W=NULL, 
                          A="TS_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h5unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h5unadj.res <- tmle_quart(dat=d, 
                          Y="waz_t3", 
                          W=NULL, 
                          A="TS_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h5unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h5unadj.res <- tmle_quart(dat=d, 
                          Y="whz_t3", 
                          W=NULL, 
                          A="TS_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h5unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h5unadj.res <- tmle_quart(dat=d, 
                          Y="hcz_t3", 
                          W=NULL, 
                          A="TS_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h5unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h5unadj.res


#adjusted

#null dataframe
h5adj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")



h5adj.res <- tmle_quart(dat=d, 
                        Y="laz_t3", 
                        W=Wvars, 
                        A="TS_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h5adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h5adj.res <- tmle_quart(dat=d, 
                        Y="waz_t3", 
                        W=Wvars, 
                        A="TS_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h5adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h5adj.res <- tmle_quart(dat=d, 
                        Y="whz_t3", 
                        W=Wvars, 
                        A="TS_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h5adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h5adj.res <- tmle_quart(dat=d, 
                        Y="hcz_t3", 
                        W=Wvars, 
                        A="TS_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h5adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h5adj.res


#Hypothesis 6
#Telomere length measured at Year 1 is negatively associated with subsequent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.
#Exposure: Quartiles of telomere lengths at Year 1
#Outcome: Child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2


#unadjusted
#Null data.frame
h6unadj.res = NULL

h6unadj.res <- tmle_quart(dat=d, 
                       Y="laz_t3", 
                       W=NULL, 
                       A="TS_t2", 
                       id="block",
                       Alevels=c("Q1","Q2","Q3","Q4"),
                       outputdf = h6unadj.res,
                       family="gaussian", 
                       SLlibrary="SL.gam")

h6unadj.res <- tmle_quart(dat=d, 
                          Y="waz_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6unadj.res <- tmle_quart(dat=d, 
                          Y="whz_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6unadj.res <- tmle_quart(dat=d, 
                          Y="hcz_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6unadj.res

#adjusted

#null dataframe
h6adj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")
         


h6adj.res <- tmle_quart(dat=d, 
                      Y="laz_t3", 
                      W=Wvars, 
                      A="TS_t2", 
                      id="block",
                      Alevels=c("Q1","Q2","Q3","Q4"),
                      outputdf = h6adj.res,
                      family="gaussian", 
                      SLlibrary="SL.gam")

h6adj.res <- tmle_quart(dat=d, 
                        Y="waz_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6adj.res <- tmle_quart(dat=d, 
                        Y="whz_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6adj.res <- tmle_quart(dat=d, 
                        Y="hcz_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6adj.res

#Hypothesis 7
#Telomere length measured at Year 1 is associated with child growth velocity (kg/month or cm/month) between the Year 1 and Year 2 visits.
#Exposure: Quartiles of telomere lengths at Year 1
#Outcome: Child weight velocity (in kg/month), length velocity (in cm/month), and head circumference velocity (in cm/month) from Year 1 to Year 2.


#unadjusted
#Null data.frame
h7unadj.res = NULL

h7unadj.res <- tmle_quart(dat=d, 
                          Y="len_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7unadj.res <- tmle_quart(dat=d, 
                          Y="wei_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7unadj.res <- tmle_quart(dat=d, 
                          Y="hc_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h7unadj.res

#adjusted

#null dataframe
h7adj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")



h7adj.res <- tmle_quart(dat=d, 
                        Y="len_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7adj.res <- tmle_quart(dat=d, 
                        Y="wei_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7adj.res <- tmle_quart(dat=d, 
                        Y="hc_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7adj.res

#Hypothesis 8
#Telomere length measured at Year 1 is associated with the change in child LAZ,WAZ, WLZ, and head circumference-for-age Z score from Year 1 to Year 2.
#Exposure: Quartiles of telomere lengths at Year 1
#Outcome: Change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Year 1 to Year 2


#unadjusted
#Null data.frame
h8unadj.res = NULL

h8unadj.res <- tmle_quart(dat=d, 
                          Y="delta_laz_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8unadj.res <- tmle_quart(dat=d, 
                          Y="delta_waz_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8unadj.res <- tmle_quart(dat=d, 
                          Y="delta_whz_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8unadj.res <- tmle_quart(dat=d, 
                          Y="delta_hcz_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8unadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h8unadj.res

#adjusted

#null dataframe
h8adj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")



h8adj.res <- tmle_quart(dat=d, 
                        Y="delta_laz_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8adj.res <- tmle_quart(dat=d, 
                        Y="delta_waz_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8adj.res <- tmle_quart(dat=d, 
                        Y="delta_whz_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8adj.res <- tmle_quart(dat=d, 
                        Y="delta_hcz_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8adj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8adj.res

#------------------
# Ad hoc analyses
# Alternative presentation of growth velocity-related hypotheses
#------------------

#Hypothesis 2b
#The change in telomere length between Year 1 and Year 2 is associated with growth velocity Z-score between Year 1 and Year 2.
#Exposure: Quartiles of change in telomere length between Year 1 and Year 2
#Outcome: Child weight velocity Z-score, length velocity Z-score, and head circumference velocity Z-score from Year 1 to Year 2

#unadjusted
#Null data.frame
h2bunadj.res = NULL

h2bunadj.res <- tmle_quart(dat=d, 
                          Y="z_len_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h2bunadj.res <- tmle_quart(dat=d, 
                          Y="z_wei_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h2bunadj.res <- tmle_quart(dat=d, 
                          Y="z_hc_velocity_t2_t3", 
                          W=NULL, 
                          A="delta_TS", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h2bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h2bunadj.res

#adjusted

#null dataframe
h2badj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2", "anthro_days_btwn_t2_t3")



h2badj.res <- tmle_quart(dat=d, 
                        Y="z_len_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2badj.res <- tmle_quart(dat=d, 
                        Y="z_wei_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2badj.res <- tmle_quart(dat=d, 
                        Y="z_hc_velocity_t2_t3", 
                        W=Wvars, 
                        A="delta_TS", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h2badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h2badj.res

#Hypothesis 7b
#Telomere length measured at Year 1 is associated with child growth velocity Z-score between the Year 1 and Year 2 visits.
#Exposure: Quartiles of telomere lengths at Year 1
#Outcome: Child weight velocity Z-score, length velocity Z-score, and head circumference velocity Z-score from Year 1 to Year 2.


#unadjusted
#Null data.frame
h7bunadj.res = NULL

h7bunadj.res <- tmle_quart(dat=d, 
                          Y="z_len_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7bunadj.res <- tmle_quart(dat=d, 
                          Y="z_wei_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7bunadj.res <- tmle_quart(dat=d, 
                          Y="z_hc_velocity_t2_t3", 
                          W=NULL, 
                          A="TS_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h7bunadj.res

#adjusted

#null dataframe
h7badj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")



h7badj.res <- tmle_quart(dat=d, 
                        Y="z_len_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7badj.res <- tmle_quart(dat=d, 
                        Y="z_wei_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7badj.res <- tmle_quart(dat=d, 
                        Y="z_hc_velocity_t2_t3", 
                        W=Wvars, 
                        A="TS_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7badj.res




#Calculating splines (unadjusted)
#Y=outcome
#X=exposure
#NOTE! I'm currently having trouble getting the ID argument to work, so 
#Currently not using it... will fix, should not greatly affect estimates

#Hypothesis 1
h1_delta_laz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_laz_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h1_delta_waz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_waz_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h1_delta_whz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_whz_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h1_delta_hcz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_hcz_t2_t3, X=d$delta_TS, W = NULL, id = NULL)

#Hypothesis 2
h2_len_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$len_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h2_wei_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$wei_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h2_hc_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$hc_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)

#Hypothesis 3
h3_laz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$laz_t3, X=d$delta_TS, W = NULL, id = NULL)
h3_waz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$waz_t3, X=d$delta_TS, W = NULL, id = NULL)
h3_whz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$whz_t3, X=d$delta_TS, W = NULL, id = NULL)
h3_hcz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$delta_TS, W = NULL, id = NULL)

#Hypothesis 4
h4_laz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$laz_t2, X=d$TS_t2, W = NULL, id = NULL)
h4_waz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$waz_t2, X=d$TS_t2, W = NULL, id = NULL)
h4_whz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$whz_t2, X=d$TS_t2, W = NULL, id = NULL)
h4_hcz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hcz_t2, X=d$TS_t2, W = NULL, id = NULL)

#Hypothesis 5
h5_laz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$laz_t3, X=d$TS_t3, W = NULL, id = NULL)
h5_waz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$waz_t3, X=d$TS_t3, W = NULL, id = NULL)
h5_whz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$whz_t3, X=d$TS_t3, W = NULL, id = NULL)
h5_hcz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$TS_t3, W = NULL, id = NULL)

#Hypothesis 6
h6_laz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$laz_t3, X=d$TS_t2, W = NULL, id = NULL)
h6_waz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$waz_t3, X=d$TS_t2, W = NULL, id = NULL)
h6_whz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$whz_t3, X=d$TS_t2, W = NULL, id = NULL)
h6_hcz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$TS_t2, W = NULL, id = NULL)

#Hypothesis 7
h7_len_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$len_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)
h7_wei_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$wei_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)
h7_hc_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hc_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)

#Hypothesis 8
h8_delta_laz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_laz_t2_t3, X=d$ts_t2, W = NULL, id = NULL)
h8_delta_waz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_waz_t2_t3, X=d$ts_t2, W = NULL, id = NULL)
h8_delta_whz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_whz_t2_t3, X=d$ts_t2, W = NULL, id = NULL)
h8_delta_hcz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_hcz_t2_t3, X=d$ts_t2, W = NULL, id = NULL)

#Hypotheis 2b
h2b_zlen_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$z_len_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h2b_zwei_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$z_wei_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)
h2b_zhc_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$z_hc_velocity_t2_t3, X=d$delta_TS, W = NULL, id = NULL)

#Hypothesis 7b
h7b_zlen_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$z_len_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)
h7b_zwei_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$z_wei_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)
h7b_zhc_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$z_hc_velocity_t2_t3, X=d$TS_t2, W = NULL, id = NULL)


#Example plot (I will write a function for prettier plots, but for initial visualization)
p <- ggplot(h1_delta_laz_v_delta_tsgam.res,aes(x = X)) +
  geom_smooth(aes(y = fit), se = F) +
  geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.1)
p





#Note - the confidence intervals look terrible because I'm not predicting values frequently enough (rather than them being wrong)
#Will fix in the function code.




