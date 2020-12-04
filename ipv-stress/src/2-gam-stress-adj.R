rm(list=ls())

source(here::here("ipv-stress/0-config.R"))
source(here::here("ipv-stress/src/0-gam-functions.R"))

d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-ipv-cesd-pss-covariates-stresslab.csv"))

# add wealth index
wealth <- read.csv("C:/Users/Sophia/Downloads/WBB-asset-index.csv")
public <- read.csv("C:/Users/Sophia/Downloads/public-ids.csv")

merged <- merge(wealth, public, by.x = 'dataid', by.y = 'dataid_r')
head(merged)
real_ids <- merged %>% select("dataid.y", "HHwealth", "HHwealth_quart", "clusterid", "block") %>% 
  rename(dataid = dataid.y)

d <- left_join(d, real_ids, by=c('dataid', 'clusterid', 'block'))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex", "birthord", "momage","momheight","momedu", 
         "hfiacat", "Ncomp", "watmin", "walls", "floor", "roof", "tr", 
         "HHwealth_quart", "n_cattle", "n_goat", "n_chicken")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
Wvars2_F2<-c("agemth_ut2", "monsoon_ut2") 
Wvars3_vital<-c("agemth_t3_vital", "monsoon_t3_vital") 
Wvars3_salimetrics<-c("agemth_t3_salimetrics", "monsoon_t3_salimetrics") 
Wvars3_oragene<-c("agemth_t3_oragene", "monsoon_t3_oragene") 

W2_F2_total <- c(Wvars, Wvars2_F2) %>% unique(.)
W3_vital_total <- c(Wvars, Wvars3_vital) %>% unique(.)
W3_salimetrics_total <- c(Wvars, Wvars3_salimetrics) %>% unique(.)
W3_oragene_total <- c(Wvars, Wvars3_oragene) %>% unique(.)


#Loop over exposure-outcome pairs
pick_covariates <- function(j){
  if(grepl("t2_f2", j)){Wset = W2_F2_total}
  if(grepl("t3_gcr", j)){Wset = W3_oragene_total}
  if(grepl("map|hr", j)){Wset = W3_vital_total}
  if(grepl("saa|cort", j)){Wset = W3_salimetrics_total}
  return(Wset)
}

#### Hypothesis 1 ####
# Maternal exposure to cumulative lifetime IPV measured at Year 2 is negatively associated with child telomere length measured at Year 2
Xvars <- c("life_viol_any_t3")            
Yvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i",
           "t3_saa_slope", "t3_saa_z01", "t3_saa_z02",
           "t3_cort_slope", "t3_cort_z01", "t3_cort_z03",
           "t3_map", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12") 


#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=pick_covariates(j))
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
saveRDS(H1_adj_models, here("ipv-stress/models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_adj_res, here("ipv-stress/results/adjusted/H1_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_adj_plot_data, here("ipv-stress/figure-data/H1_adj_spline_data.RDS"))




#### Hypothesis 2 ####
Xvars <- c("pss_sum_mom_t3", "pss_sum_dad_t3")            
Yvars <- c("t3_saa_slope", "t3_saa_z01", "t3_saa_z02",
           "t3_cort_slope", "t3_cort_z01", "t3_cort_z03",
           "t3_map", "t3_hr_mean", 
           "t3_gcr_mean", "t3_gcr_cpg12") 

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=pick_covariates(j))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res , preds$res)
}

#Make list of plots
H2_adj_plot_list <- NULL
H2_adj_plot_data <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_adj_plot_list[[i]] <-  simul_plot$p
  H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H2_adj_models, here("ipv-stress/models/H2_adj_models.RDS"))

#Save results
saveRDS(H2_adj_res, here("ipv-stress/results/adjusted/H2_adj_res.RDS"))


#Save plots
#saveRDS(H2_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_splines.RDS"))

#Save plot data
saveRDS(H2_adj_plot_data, here("ipv-stress/figure-data/H2_adj_splint_data.RDS"))




#### Hypothesis 3 ####
Xvars <- c("cesd_sum_t2")            
Yvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i",
           "t3_saa_slope", "t3_saa_z01", "t3_saa_z02",
           "t3_cort_slope", "t3_cort_z01", "t3_cort_z03",
           "t3_map", "t3_hr_mean", 
           "t3_gcr_mean", "t3_gcr_cpg12") 

#Fit models
H3_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=pick_covariates(j))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

Xvars <- c("cesd_sum_ee_t3")            
Yvars <- c("t3_saa_slope", "t3_saa_z01", "t3_saa_z02",
           "t3_cort_slope", "t3_cort_z01", "t3_cort_z03",
           "t3_map", "t3_hr_mean", 
           "t3_gcr_mean", "t3_gcr_cpg12") 

for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=pick_covariates(j))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}


#Get primary contrasts
H3_adj_res <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_adj_res <-  bind_rows(H3_adj_res , preds$res)
}

#Make list of plots
H3_adj_plot_list <- NULL
H3_adj_plot_data <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H3_adj_models$fit[i][[1]], H3_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_adj_plot_list[[i]] <-  simul_plot$p
  H3_adj_plot_data <-  rbind(H3_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H3_adj_models, here("ipv-stress/models/H3_adj_models.RDS"))

#Save results
saveRDS(H3_adj_res, here("ipv-stress/results/adjusted/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))

#Save plot data
saveRDS(H3_adj_plot_data, here("ipv-stress/figure-data/H3_adj_spline_data.RDS"))

