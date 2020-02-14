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

source(here("audrie R scripts/observational/0-base-quartileTMLE_functions.R"))



#load covariates, exposures, outcomes dataset
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))



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

#kept life_viol_any_t3 because violence is a pattern and question asked about lifetime violence

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
         "n_cattle", "n_goat", "n_chicken", "monsoon_ht3", 
         "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3", "lenhei_med_t2", "weight_med_t2")

#removed monsoon_ht2, ageday_ht2 (does not influence t3 exposure/outcome)
#kept cesd_sum_t2, diar7d_t2 (could influence t3 exposure/outcome)
#kept lenhei_med_t2, weight_med_t2 (could influence t3 exposure/outcome)

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


#Hypothesis 6b
#Child LAZ, WAZ, WLZ, and HCZ measured at Year 1 is associated with subsequent child telomere length at Year 2.
#Exposure: Quartiles of LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1
#Outcome: Child telomere lengths at Year 2


#unadjusted
#Null data.frame
h6bunadj.res = NULL

h6bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="laz_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="waz_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="whz_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="hcz_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h6bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h6bunadj.res

#adjusted

#null dataframe
h6badj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at2", "monsoon_at3", "ageday_at2",
         "ageday_at3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3")


h6badj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="laz_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6badj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="waz_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6badj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="whz_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6badj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="hcz_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h6badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h6badj.res


#Hypothesis 6c
#Child LAZ, WAZ, WLZ, and head circumference-for-age Z score measured at Month 3 are associated with subsequent telomere lengths at Year 1.
#Exposure: Quartiles of LAZ, WAZ, WLZ, and head circumference-for-age Z score at Month 3
#Outcome: Child telomere lengths at Year 1


#unadjusted
#Null data.frame
h6cunadj.res = NULL

h6cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="laz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="waz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="whz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="hcz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6cunadj.res

#adjusted

#null dataframe
h6cadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "ageday_at1", "ageday_at2", "tr", "cesd_sum_t2", "diar7d_t2", "life_viol_any_t3")


h6cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="laz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="waz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="whz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="hcz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6cadj.res




#Hypothesis 6d
#Child LAZ, WAZ, WLZ, and HCZ measured at Month 3 is associated with subsequent child telomere length at Year 2.
#Exposure: Quartiles of LAZ, WAZ, WLZ, and head circumference-for-age Z score at Month 3
#Outcome: Child telomere lengths at Year 2


#unadjusted
#Null data.frame
h6dunadj.res = NULL

h6dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="laz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="waz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="whz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="hcz_t1", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h6dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h6dunadj.res

#adjusted

#null dataframe
h6dadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "monsoon_at3", "ageday_at1", "ageday_at2", 
         "ageday_at3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3", 
         "life_viol_any_t3")


h6dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="laz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="waz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="whz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="hcz_t1", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h6dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h6dadj.res





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


#Hypothesis 7b
#Child length, weight, head circumference velocity between Month 3 and Year 1 is associated with subsequent child telomere length at Year 1.
#Exposure: Quartiles of length, weight, head circumference velocity between Month 3 and Year 1
#Outcome: Child telomere lengths at Year 1

#unadjusted
#Null data.frame
h7bunadj.res = NULL

h7bunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="len_velocity_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7bunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7bunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="wei_velocity_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7bunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7bunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t2", 
                           W=NULL, 
                           A="hc_velocity_t1_t2", 
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
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "ageday_at1", "ageday_at2",  
         "tr", "cesd_sum_t2", "diar7d_t2", 
         "life_viol_any_t3")

#Removed Year 2 covariates: cesd_sum_ee_t3, diar7d_t3, pss_sum_mom_t3
#Kept life_viol_any_t3 (lifetime exposure to violence affects earlier timepoints)

h7badj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="len_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7badj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7badj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="wei_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7badj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7badj.res <- tmle_quart(dat=d, 
                         Y="TS_t2", 
                         W=Wvars, 
                         A="hc_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7badj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7badj.res



#Hypothesis 7c
#Child length, weight, head circumference velocity between Month 3 and Year 1 is associated with subsequent child telomere length at Year 2.
#Exposure: Quartiles of length, weight, head circumference velocity between Month 3 and Year 1
#Outcome: Child telomere lengths at Year 2

#unadjusted
#Null data.frame
h7cunadj.res = NULL

h7cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="len_velocity_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="wei_velocity_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="hc_velocity_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h7cunadj.res

#adjusted

#null dataframe
h7cadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "monsoon_at3", "ageday_at1", "ageday_at2", "ageday_at3", 
         "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "diar7d_t2", "diar7d_t3", "pss_sum_mom_t3",
         "life_viol_any_t3")


h7cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="len_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="wei_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="hc_velocity_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7cadj.res



#Hypothesis 7d
#Child length, weight, head circumference velocity between Y1 and Y2 is associated with subsequent child telomere length at Year 2.
#Exposure: Quartiles of length, weight, head circumference velocity between Y1 and Y2
#Outcome: Child telomere lengths at Year 2

#unadjusted
#Null data.frame
h7dunadj.res = NULL

h7dunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="len_velocity_t2_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7dunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7dunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="wei_velocity_t2_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7dunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h7dunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t3", 
                          W=NULL, 
                          A="hc_velocity_t2_t3", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h7dunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h7dunadj.res

#adjusted

#null dataframe
h7dadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at2", "monsoon_at3", "ageday_at2", "ageday_at3", 
         "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "diar7d_t2", "diar7d_t3", "pss_sum_mom_t3",
         "life_viol_any_t3")


h7dadj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="len_velocity_t2_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7dadj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7dadj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="wei_velocity_t2_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7dadj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7dadj.res <- tmle_quart(dat=d, 
                        Y="TS_t3", 
                        W=Wvars, 
                        A="hc_velocity_t2_t3", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h7dadj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h7dadj.res

#Hypothesis 7e
#Child length, weight, head circumference velocity between Month 3 and Year 2 is associated with subsequent child telomere length at Year 2.
#Exposure: Quartiles of length, weight, head circumference velocity between Month 3 and Year 2
#Outcome: Child telomere lengths at Year 2


#unadjusted
#Null data.frame
h7eunadj.res = NULL

h7eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="len_velocity_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="wei_velocity_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h7eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="hc_velocity_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h7eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h7eunadj.res

#adjusted

#null dataframe
h7eadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at3", "ageday_at1", "ageday_at3", 
         "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "diar7d_t2", "diar7d_t3", "pss_sum_mom_t3",
         "life_viol_any_t3")


h7eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="len_velocity_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="wei_velocity_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="hc_velocity_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h7eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h7eadj.res







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

#Hypothesis 8b
#Change in child LAZ, WAZ, WLZ, HCZ from Month 3 to Year 1 is associated with telomere length measured at Year 1
#Exposure: Quartiles of change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Month 3 to Year 1
#Outcome:Telomere lengths at Year 1

#unadjusted
#Null data.frame
h8bunadj.res = NULL

h8bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t2", 
                          W=NULL, 
                          A="delta_laz_t1_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t2", 
                          W=NULL, 
                          A="delta_waz_t1_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t2", 
                          W=NULL, 
                          A="delta_whz_t1_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")

h8bunadj.res <- tmle_quart(dat=d, 
                          Y="TS_t2", 
                          W=NULL, 
                          A="delta_hcz_t1_t2", 
                          id="block",
                          Alevels=c("Q1","Q2","Q3","Q4"),
                          outputdf = h8bunadj.res,
                          family="gaussian", 
                          SLlibrary="SL.gam")


h8bunadj.res

#adjusted

#null dataframe
h8badj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "ageday_at1", 
         "ageday_at2", "tr", "cesd_sum_t2", "diar7d_t2", 
         "life_viol_any_t3")

#Removed Year 2 covariates "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3"
#Removed "lenhei_med_t2", "weight_med_t2"
#Kept life_viol_any_t3 (lifetime violence)


h8badj.res <- tmle_quart(dat=d, 
                        Y="TS_t2", 
                        W=Wvars, 
                        A="delta_laz_t1_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8badj.res <- tmle_quart(dat=d, 
                        Y="TS_t2", 
                        W=Wvars, 
                        A="delta_waz_t1_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8badj.res <- tmle_quart(dat=d, 
                        Y="TS_t2", 
                        W=Wvars, 
                        A="delta_whz_t1_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8badj.res <- tmle_quart(dat=d, 
                        Y="TS_t2", 
                        W=Wvars, 
                        A="delta_hcz_t1_t2", 
                        id="block",
                        Alevels=c("Q1","Q2","Q3","Q4"),
                        outputdf = h8badj.res,
                        family="gaussian", 
                        SLlibrary="SL.gam")

h8badj.res

#Hypothesis 8c
#Change in child LAZ, WAZ, WLZ, HCZ from Month 3 to Year 1 is associated with telomere length measured at Year 2
#Exposure: Quartiles of change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Month 3 to Year 1
#Outcome:Telomere lengths at Year 2



#unadjusted
#Null data.frame
h8cunadj.res = NULL

h8cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_laz_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_waz_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_whz_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8cunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_hcz_t1_t2", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8cunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h8cunadj.res

#adjusted

#null dataframe
h8cadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at2", "monsoon_at3", "ageday_at1", 
         "ageday_at2", "ageday_at3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3",
         "life_viol_any_t3")


#Removed "lenhei_med_t2", "weight_med_t2"



h8cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_laz_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_waz_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_whz_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8cadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_hcz_t1_t2", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8cadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8cadj.res


#Hypothesis 8d
#Change in child LAZ, WAZ, WLZ, HCZ from Year 1 to Year 2 is associated with telomere length measured at Year 2
#Exposure: Quartiles of change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Year 1 to Year 2
#Outcome:Telomere lengths at Year 2



#unadjusted
#Null data.frame
h8dunadj.res = NULL

h8dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_laz_t2_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_waz_t2_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_whz_t2_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8dunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_hcz_t2_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8dunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h8dunadj.res

#adjusted

#null dataframe
h8dadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at2", "monsoon_at3", 
         "ageday_at2", "ageday_at3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3",
         "life_viol_any_t3")


#Removed "lenhei_med_t2", "weight_med_t2"
#Removed "monsoon_at1", "ageday_at1"



h8dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_laz_t2_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_waz_t2_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_whz_t2_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8dadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_hcz_t2_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8dadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8dadj.res

#Hypothesis 8e
#Change in child LAZ, WAZ, WLZ, HCZ from Month 3 to Year 2 is associated with telomere length measured at Year 2
#Exposure: Quartiles of change in child length-for-age Z score (LAZ), weight-for-age Z score (WAZ), weight-for-length Z score (WLZ), and head circumference-for-age Z score from Month 3 to Year 2
#Outcome:Telomere lengths at Year 2


#unadjusted
#Null data.frame
h8eunadj.res = NULL

h8eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_laz_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_waz_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_whz_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")

h8eunadj.res <- tmle_quart(dat=d, 
                           Y="TS_t3", 
                           W=NULL, 
                           A="delta_hcz_t1_t3", 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"),
                           outputdf = h8eunadj.res,
                           family="gaussian", 
                           SLlibrary="SL.gam")


h8eunadj.res

#adjusted

#null dataframe
h8eadj.res = NULL 

Wvars<-c("sex","birthord", "momage", "momheight","momedu", 
         "hfiacat", "Nlt18", "Ncomp", "watmin", 
         "floor", "walls", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", "asset_khat", 
         "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
         "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
         "n_cattle", "n_goat", "n_chicken", "monsoon_at1", "monsoon_at3", 
         "ageday_at1", "ageday_at3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t2", "diar7d_t3",
         "life_viol_any_t3")


#Removed "lenhei_med_t2", "weight_med_t2"
#Kept Year 1 (t2) covariates because cesd or diarrhea could influence TS_t3
#Removed "monsoon_at2", "ageday_at2"



h8eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_laz_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_waz_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_whz_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8eadj.res <- tmle_quart(dat=d, 
                         Y="TS_t3", 
                         W=Wvars, 
                         A="delta_hcz_t1_t3", 
                         id="block",
                         Alevels=c("Q1","Q2","Q3","Q4"),
                         outputdf = h8eadj.res,
                         family="gaussian", 
                         SLlibrary="SL.gam")

h8eadj.res




#Calculating splines (unadjusted)
#Y=outcome
#X=exposure

#Hypothesis 1
h1_delta_laz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_laz_t2_t3, X=d$delta_TS, W = NULL)
h1_delta_waz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_waz_t2_t3, X=d$delta_TS, W = NULL)
h1_delta_whz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_whz_t2_t3, X=d$delta_TS, W = NULL)
h1_delta_hcz_v_delta_tsgam.res <- GAM_simulCI(Y=d$delta_hcz_t2_t3, X=d$delta_TS, W = NULL, gamdf = c(1:6)) #drop df failing to converge

#Hypothesis 2
h2_len_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$len_velocity_t2_t3, X=d$delta_TS, W = NULL)
h2_wei_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$wei_velocity_t2_t3, X=d$delta_TS, W = NULL)
h2_hc_velocity_v_delta_tsgam.res <- GAM_simulCI(Y=d$hc_velocity_t2_t3, X=d$delta_TS, W = NULL)

#Hypothesis 3
h3_laz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$laz_t3, X=d$delta_TS, W = NULL)
h3_waz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$waz_t3, X=d$delta_TS, W = NULL)
h3_whz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$whz_t3, X=d$delta_TS, W = NULL)
h3_hcz_t3_vs_delta_tsgam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$delta_TS, W = NULL)

#Hypothesis 4
h4_laz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$laz_t2, X=d$TS_t2, W = NULL)
h4_waz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$waz_t2, X=d$TS_t2, W = NULL)
h4_whz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$whz_t2, X=d$TS_t2, W = NULL)
h4_hcz_t2_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hcz_t2, X=d$TS_t2, W = NULL)

#Hypothesis 5
h5_laz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$laz_t3, X=d$TS_t3, W = NULL)
h5_waz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$waz_t3, X=d$TS_t3, W = NULL)
h5_whz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$whz_t3, X=d$TS_t3, W = NULL)
h5_hcz_t3_vs_ts_t3gam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$TS_t3, W = NULL)

#Hypothesis 6
h6_laz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$laz_t3, X=d$TS_t2, W = NULL)
h6_waz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$waz_t3, X=d$TS_t2, W = NULL)
h6_whz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$whz_t3, X=d$TS_t2, W = NULL)
h6_hcz_t3_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hcz_t3, X=d$TS_t2, W = NULL)

#Hypothesis 7
h7_len_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$len_velocity_t2_t3, X=d$TS_t2, W = NULL)
h7_wei_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$wei_velocity_t2_t3, X=d$TS_t2, W = NULL)
h7_hc_veloc_vs_ts_t2gam.res <- GAM_simulCI(Y=d$hc_velocity_t2_t3, X=d$TS_t2, W = NULL)

#Hypothesis 8
h8_delta_laz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_laz_t2_t3, X=d$TS_t2, W = NULL)
h8_delta_waz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_waz_t2_t3, X=d$TS_t2, W = NULL)
h8_delta_whz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_whz_t2_t3, X=d$TS_t2, W = NULL)
h8_delta_hcz_v_ts_t2gam.res <- GAM_simulCI(Y=d$delta_hcz_t2_t3, X=d$TS_t2, W = NULL, gamdf = 1:5) #drop df failing to converge)





#Save tmle results
save(
  h1unadj.res,
  h1adj.res,
  h2unadj.res,
  h2adj.res,
  h3unadj.res,
  h3adj.res,
  h4unadj.res,
  h4adj.res,
  h5unadj.res,
  h5adj.res,
  h6unadj.res,
  h6adj.res,
  h6bunadj.res,
  h6badj.res,
  h6cunadj.res,
  h6cadj.res,
  h6dunadj.res,
  h6dadj.res,
  h7unadj.res,
  h7adj.res,
  h7bunadj.res,
  h7badj.res,
  h7cunadj.res,
  h7cadj.res,
  h7dunadj.res,
  h7dadj.res,
  h7eunadj.res,
  h7eadj.res,
  h8unadj.res,
  h8adj.res,
  h8bunadj.res,
  h8badj.res,
  h8cunadj.res,
  h8cadj.res,
  h8dunadj.res,
  h8dadj.res,
  h8eunadj.res,
  h8eadj.res,
  file=here("/audrie results/telo_growth_results.Rdata")
)



#Save spline results
save(
  #Hypothesis 1
  h1_delta_laz_v_delta_tsgam.res,
  h1_delta_waz_v_delta_tsgam.res, 
  h1_delta_whz_v_delta_tsgam.res,
  h1_delta_hcz_v_delta_tsgam.res,
  #Hypothesis 2
  h2_len_velocity_v_delta_tsgam.res,
  h2_wei_velocity_v_delta_tsgam.res,
  h2_hc_velocity_v_delta_tsgam.res,
  #Hypothesis 3
  h3_laz_t3_vs_delta_tsgam.res, 
  h3_waz_t3_vs_delta_tsgam.res,
  h3_whz_t3_vs_delta_tsgam.res, 
  h3_hcz_t3_vs_delta_tsgam.res, 
  #Hypothesis 4
  h4_laz_t2_vs_ts_t2gam.res, 
  h4_waz_t2_vs_ts_t2gam.res,
  h4_whz_t2_vs_ts_t2gam.res, 
  h4_hcz_t2_vs_ts_t2gam.res, 
  #Hypothesis 5
  h5_laz_t3_vs_ts_t3gam.res, 
  h5_waz_t3_vs_ts_t3gam.res, 
  h5_whz_t3_vs_ts_t3gam.res, 
  h5_hcz_t3_vs_ts_t3gam.res, 
  #Hypothesis 6
  h6_laz_t3_vs_ts_t2gam.res, 
  h6_waz_t3_vs_ts_t2gam.res, 
  h6_whz_t3_vs_ts_t2gam.res, 
  h6_hcz_t3_vs_ts_t2gam.res, 
  #Hypothesis 7
  h7_len_veloc_vs_ts_t2gam.res, 
  h7_wei_veloc_vs_ts_t2gam.res, 
  h7_hc_veloc_vs_ts_t2gam.res,
  #Hypothesis 8
  h8_delta_laz_v_ts_t2gam.res,
  h8_delta_waz_v_ts_t2gam.res,
  h8_delta_whz_v_ts_t2gam.res, 
  h8_delta_hcz_v_ts_t2gam.res, 
  file=here("/audrie results/telo_growth_spline_fits.Rdata"))
  




