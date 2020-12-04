rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-ee-telo-development-covariates.RDS"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
Wvars2<-c("monsoon_ht2", "ageday_ht2", "tr", "cesd_sum_t2", "diar7d_t2", "midline_stimulation") 
Wvars3<-c("lenhei_med_t2", "weight_med_t2", "monsoon_ht2", "monsoon_ht3", "ageday_ht2", 
          "ageday_ht3", "tr", "cesd_sum_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3", 
          "life_viol_any_t3", "diar7d_t3", "midline_stimulation", "endline_stimulation") 


W2_total <- c(Wvars, Wvars2) %>% unique(.)
W3_total <- c(Wvars, Wvars3) %>% unique(.)

Wvars2[!(Wvars2 %in% colnames(d))]
Wvars3[!(Wvars3 %in% colnames(d))]


#Loop over exposure-outcome pairs

#### Hypothesis 1a ####
# Maternal exposure to cumulative lifetime IPV measured at Year 2 is negatively associated with child telomere length measured at Year 2
Xvars <- c("life_viol_any_t3")            
Yvars <- c("TS_t3_Z") 

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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars3)
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
#saveRDS(H1_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_adj_res, here("results/adjusted/H1_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
#saveRDS(H1_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1_adj_spline_data.RDS"))




#### Hypothesis 2 ####
# Telomere at y1 v. development year 2
Xvars <- c("TS_t2_Z")            
Yvars <- c("endline_communication_score_Z", "endline_gross_motor_score_Z", 
           "endline_personal_social_score_Z", "combined_easq_Z", "endline_A_not_B_score_Z", 
           "endline_tower_test_Z") 

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars3)
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
H2_adj_res$adjusted <- 0

#Make list of plots
H2_adj_plot_list <- NULL
H2_adj_plot_data <- NULL
for(i in 1:nrow(H2_adj_models)){
  print(i)
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_adj_plot_list[[i]] <-  simul_plot$p
  H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H2_adj_models.RDS"))

#Save results
saveRDS(H2_adj_res, here("results/adjusted/H2_adj_res.RDS"))


#Save plots
#saveRDS(H2_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_splines.RDS"))

#Save plot data
#saveRDS(H2_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2_adj_spline_data.RDS"))




#### Hypothesis 3 ####
# telomere length at year 1 v. development at year 1
Xvars <- c("TS_t2_Z")            
Yvars <- c("endline_CDI_understand", "endline_CDI_say")

#Fit models
H3_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars2)
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
H3_adj_res$adjusted <- 0

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
#saveRDS(H3_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H3_adj_models.RDS"))

#Save results
saveRDS(H3_adj_res, here("results/adjusted/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))

#Save plot data
#saveRDS(H3_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H3_adj_spline_data.RDS"))



#### Hypothesis 4 ####
#Telomere length at year 2 v. development at year 2
Xvars <- c("TS_t3_Z")            
Yvars <- c("endline_communication_score_Z", "endline_gross_motor_score_Z", 
           "endline_personal_social_score_Z", "combined_easq_Z", "endline_A_not_B_score_Z", 
           "endline_tower_test_Z") 

#Fit models
H4_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars3)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#Get primary contrasts
H4_adj_res <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_adj_res <-  bind_rows(H4_adj_res , preds$res)
}
H4_adj_res$adjusted <- 0

#Make list of plots
H4_adj_plot_list <- NULL
H4_adj_plot_data <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4_adj_models$fit[i][[1]], H4_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_adj_plot_list[[i]] <-  simul_plot$p
  H4_adj_plot_data <-  rbind(H4_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H4_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H4_adj_models.RDS"))

#Save results
saveRDS(H4_adj_res, here("results/adjusted/H4_adj_res.RDS"))


#Save plots
#saveRDS(H4_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4_adj_splines.RDS"))

#Save plot data
#saveRDS(H4_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H4_adj_spline_data.RDS"))
