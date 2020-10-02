



rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Andrew/stress_growth_data.RDS"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe",
         "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", 
         "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", 
         "asset_mobile", "n_cattle", "n_goat", "n_chicken", "cesd_sum_t2", "diar7d_t2", "lenhei_med_t2", "weight_med_t2")

Wvars[!(Wvars %in% colnames(d))]



#Add in time varying covariates:

#NOTES
#Does monsoon_ut2 need to be replaced with monsoon_ht2 for growth measures? (and agemth_ut2 with agedays_ht2?)
Wvars2_anthro<-c("agemth_ut2", "monsoon_ut2") 
Wvars3_anthro<-c("ageday_at3", "monsoon_at3") 

Wvars2_F2<-c("agemth_ut2", "monsoon_ut2") 
Wvars3_vital<-c("agemth_t3_vital", "monsoon_t3_vital", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_salimetrics<-c("agemth_t3_salimetrics", "monsoon_t3_salimetrics", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_oragene<-c("agemth_t3_oragene", "monsoon_t3_oragene", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 


#Add in time-varying covariates
# W<- subset(d, select=Wvars)
# W2_anthro<- subset(d, select=Wvars2_anthro)
# W3_anthro<- subset(d, select=Wvars3_anthro)
# 
# W2_F2<- subset(d, select=Wvars2_F2)
# W3_vital<- subset(d, select=Wvars3_vital)
# W3_salimetrics<- subset(d, select=Wvars3_salimetrics)
# W3_oragene<- subset(d, select=Wvars3_oragene)

# W2_F2.W2_anthro <- cbind(W2_F2,W2_anthro) %>% subset(., select = which(!duplicated(names(.))))
# W2_F2.W3_anthro <- cbind(W2_F2,W3_anthro) %>% subset(., select = which(!duplicated(names(.))))
# W2_F2.W2_anthro.W3_anthro <- cbind(W2_F2,W2_anthro,W3_anthro) %>% subset(., select = which(!duplicated(names(.))))
# W3_vital.W3_anthro <- cbind(W3_vital,W3_anthro) %>% subset(., select = which(!duplicated(names(.))))
# W3_salimetrics.W3_anthro <- cbind(W3_salimetrics,W3_anthro) %>% subset(., select = which(!duplicated(names(.))))
# W3_oragene.W3_anthro <- cbind(W3_oragene,W3_anthro) %>% subset(., select = which(!duplicated(names(.))))
# 


W2_F2.W2_anthro <- c(Wvars, Wvars2_F2 ,Wvars2_anthro) %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2 ,Wvars3_anthro) %>% unique(.)
# W3_vital.W3_anthro <- c(Wvars, Wvars3_vital,Wvars3_anthro) %>% unique(.)
# W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics,Wvars3_anthro) %>% unique(.)
# W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene,Wvars3_anthro) %>% unique(.)
W3_vital.W3_anthro <- c(Wvars, Wvars3_vital) %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics) %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene) %>% unique(.)


#Loop over exposure-outcome pairs

##Hypothesis 1a
#Urinary creatine-adjusted F2-isoprostanes isomer score  at Year 1 is negatively associated with 
#concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1.

# Exposure: Quartile of F2-isoprostanes isomer score
# Primary Outcome  : Child LAZ at Year 1
# Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 1
# Tertiary Outcomes: Child WLZ at Year 1

##Hypothesis 1b
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with child growth velocity (kg/month or cm/month) between the Year 1 and Year 2 visits.	

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child length velocity (in cm/month) from Year 1 to Year 2
#Secondary Outcome: Child weight velocity (in kg/month) and head circumference velocity (in cm/month) from Year 1 to Year 2

## Hypothesis 1c
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with subsequent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2. 

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 1d
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with the change in child LAZ, WAZ, WLZ, and head circumference-for-age Z score from Year 1 to Year 2. 

#Exposure: Quartiles of F2-isoprostanes isomer score
#Primary Outcome: Change in child LAZ from Year 1 to Year 2
#Secondary Outcome: Change in child WAZ and head circumference-for-age Z score from Year 1 to Year 2
#Tertiary Outcomes: Change in child WLZ from Year 1 to Year 2

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "iso.pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

pick_covariates_H1 <- function(j){
  if(grepl("_t2", j)){Wset = W2_F2.W2_anthro}
  if(grepl("_t3", j)){Wset = W2_F2.W3_anthro}
  return(Wset)
}
#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset <- pick_covariates_H1(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}



#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}
H1_adj_res$adjusted <- 0

#Make list of plots
H1_adj_plot_list <- NULL
H1_adj_plot_data <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_adj_plot_list[[i]] <-  simul_plot$p
  H1_adj_plot_data <-  rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H1_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_adj_res, here("results/adjusted/H1_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1_adj_spline_data.RDS"))


## Hypothesis 2a
#Change in slope between pre- and post-stressor cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2b
#Residualized gain score for cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 2c
#Change in slope between pre- and post-stressor alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2d
#Residualized gain score for alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

Xvars <- c("t3_cort_slope", "t3_residual_cort", "t3_saa_slope", "t3_residual_saa")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")

#Print adjustment covariates
W3_salimetrics.W3_anthro

#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=W3_salimetrics.W3_anthro)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_models <- bind_rows(H2_models, res)
  }
}

#Get primary contrasts
H2_res <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_res <-  bind_rows(H2_res , preds$res)
}
H2_res$adjusted <- 0

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  print(i)
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H2_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/adjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2_adj_spline_data.RDS"))



##Hypothesis 3a
#Mean arterial pressure measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of mean arterial pressure at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 3b
#Resting heart rate measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of resting heart rate at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_map", "t3_hr_mean")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")


#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=W3_vital.W3_anthro)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}
H3_res$adjusted <- 0

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H3_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/adjusted/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H3_adj_spline_data.RDS"))


##Hypothesis 4a
#Glucocorticoid receptor (NR3C1) exon 1F promoter methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of overall percentage of methylation across the entire promoter region at Year 2 post-intervention
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 4b
#Glucocorticoid receptor NGFI-A transcription factor binding site methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of percentage methylation at NGFI-A transcription factor binding 	site (CpG site #12)
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_gcr_mean", "t3_gcr_cpg12")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")



#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=W3_oragene.W3_anthro)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}
H4_res$adjusted <- 0

#Make list of plots
H4_plot_list <- NULL
H4_plot_data <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_plot_list[[i]] <-  simul_plot$p
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H4_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/adjusted/H4_adj_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4_adj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H4_adj_spline_data.RDS"))
