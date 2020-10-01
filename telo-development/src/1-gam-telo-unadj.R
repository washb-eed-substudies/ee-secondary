rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-immune-growth-analysis-dataset.rds"))


#Example:

#Fit GAM model with random effects for childid
#res_unadj <- fit_RE_gam(d=d, X="t3_cort_z01", Y="laz_t3",  W=NULL)

#Get predictions of differences from the 25th percentile of exposure
#preds_unadj <- predict_gam_diff(fit=res_unadj$fit, d=res_unadj$dat, quantile_diff=c(0.25,0.75), Xvar="delta_TS", Yvar="laz_t3")


#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
#preds_unadj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
#NOTE: not making these plots anymore, just using for diagnostics
#p <- plot_gam_diff(preds_unadj$plotdf)
#print(p)

#Fit spline with simultaneous confidence intervals
#simul_plot <- gam_simul_CI(res_unadj$fit, res_unadj$dat, xlab="delta_TS", ylab="laz_t3", title="example title")
#simul_plot$p


#Loop over exposure-outcome pairs

# all immune ratios at Y1 v. growth outcomes at Y1
Xvars <- c("t2_ratio_pro_il10", "t2_ratio_th1_il10", "t2_ratio_th2_il10",     
           "t2_ratio_th17_il10", "t2_ratio_th1_th2", "t2_ratio_th1_th17", "t2_ln_agp", "t2_ln_crp")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2") 
# "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
# "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
# "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}

#Get primary contrasts
H1_res <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_res <-  bind_rows(H1_res , preds$res)
}
H1_res$adjusted <- 0

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H1_models, here("models/H1_models.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/H1_res.RDS"))


#Save plots
#saveRDS(H1_plot_list, here("figure-objects/H1_unadj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_unadj_spline_data.RDS"))



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

#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
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
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H2_models, here("models/H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/unadjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_unadj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_unadj_spline_data.RDS"))



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
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
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
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H3_models, here("models/H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/unadjusted/H3_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_unadj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_unadj_spline_data.RDS"))


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
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
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
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H4_models, here("models/H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/unadjusted/H4_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, here("figure-objects/H4_unadj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, here("figure-data/H4_unadj_spline_data.RDS"))

