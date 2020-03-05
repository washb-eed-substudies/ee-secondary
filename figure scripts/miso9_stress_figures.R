rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


# Load results
load(here::here("andrew results/stress_results.Rdata"))

# Function for helping make table with name and age attached
readjustfunc <- function(data, var){
  data[data$Y == var,]
}

#### FIGURE MEANS ####

d <- rbind(
  data.frame(readjustfunc(mean_sd_tr, "t2_f2_8ip"), name="iPF(2a)-III ", age=14),
  data.frame(readjustfunc(mean_sd_tr, "t2_f2_23d"), name="2,3-dinor-iPF(2a)-III", age=14),
  data.frame(readjustfunc(mean_sd_tr, "t2_f2_VI"), name="iPF(2a)-VI", age=14),
  data.frame(readjustfunc(mean_sd_tr, "t2_f2_12i"), name="8,12-iso-iPF(2a)-VI", age=14),
  data.frame(readjustfunc(mean_sd_tr, "t3_saa_z01"), name="Pre-stressor salivary alpha-amylase", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_saa_z02"), name="Post-stressor salivary alpha-amylase", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_saa_slope"), name="Change in slope between pre- and \n post-stressor alpha-amylase", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_residual_saa"), name="Residualized gain score for alpha-amylase", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_cort_z01"), name="Pre-stressor salivary cortisol", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_cort_z03"), name="Post-stressor salivary cortisol", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_cort_slope"), name="Change in slope between pre- and \n post-stressor cortisol", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_residual_cort"), name="Residualized gain score for cortisol", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_map"), name="Mean arterial pressure", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_hr"), name="Resting heart rate", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_gcr"), name="NR3C1 exon 1F promoter methylation", age=28),
  data.frame(readjustfunc(mean_sd_tr, "t3_gcr_cpg12"), name="NGFI-A transcription factor binding site", age=28)
)

d$age <- as.factor(d$age)

stress_plot_mean <- function(d, var){
  df <- d[d$Y==var,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                  size = 1, width = 0.2, alpha = 0.7, position = pos) +
    labs(y = " ", x =  " ", title=df$name) +
    coord_cartesian(ylim=c(df$mean[1]-max(df$sd)-(0.5*max(df$sd, na.rm=T)), df$mean[1]+max(df$sd)+(0.5*max(df$sd, na.rm=T)))) +
    theme(axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}

stress_plot_mean_wlabel <- function(d, var){
  df <- d[d$Y==var,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                  size = 1, width = 0.2, alpha = 0.7, position = pos) +
    labs(y = " ", x =  "Child age, months", title=df$name) +
    coord_cartesian(ylim=c(df$mean[1]-max(df$sd)-(0.5*max(df$sd, na.rm=T)), df$mean[1]+max(df$sd)+(0.5*max(df$sd, na.rm=T)))) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x = element_text(size=9),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}

f2III <- stress_plot_mean(d, "t2_f2_8ip")
f223d <- stress_plot_mean(d, "t2_f2_23d")
f2VI <- stress_plot_mean(d, "t2_f2_VI")
f212 <- stress_plot_mean(d, "t2_f2_12i")
map <- stress_plot_mean(d, "t3_map")
hr <- stress_plot_mean(d, "t3_hr")
saa1 <- stress_plot_mean(d, "t3_saa_z01")
saa2 <- stress_plot_mean(d, "t3_saa_z02")
cort1 <- stress_plot_mean(d, "t3_cort_z01")
cort2 <- stress_plot_mean(d, "t3_cort_z03")
gcr <- stress_plot_mean(d, "t3_gcr")
cpg12 <- stress_plot_mean(d, "t3_gcr_cpg12")
saam <- stress_plot_mean_wlabel(d, "t3_saa_slope")
cortm <- stress_plot_mean_wlabel(d, "t3_cort_slope")
saar <- stress_plot_mean_wlabel(d, "t3_residual_saa")
cortr <- stress_plot_mean_wlabel(d, "t3_residual_cort")



p1 <- ggarrange(f2III, f223d, f2VI, f212, map, hr, saa1, saa2, cort1, cort2, gcr, cpg12, saam, cortm, saar, cortr,
                ncol=4, nrow=4, common.legend = TRUE, legend="bottom")



ggsave(p1, file =here::here("figures/stress_mean_by_tr.png"), height=9, width=14)



#### FIGURE UNADJUSTED DIFFERENCES ####

d <- rbind(
  data.frame(readjustfunc(res_unadj, "t2_f2_8ip"), name="iPF(2a)-III ", age=14),
  data.frame(readjustfunc(res_unadj, "t2_f2_23d"), name="2,3-dinor-iPF(2a)-III", age=14),
  data.frame(readjustfunc(res_unadj, "t2_f2_VI"), name="iPF(2a)-VI", age=14),
  data.frame(readjustfunc(res_unadj, "t2_f2_12i"), name="8,12-iso-iPF(2a)-VI", age=14),
  data.frame(readjustfunc(res_unadj, "t3_saa_z01"), name="Pre-stressor salivary alpha-amylase", age=28),
  data.frame(readjustfunc(res_unadj, "t3_saa_z02"), name="Post-stressor salivary alpha-amylase", age=28),
  data.frame(readjustfunc(res_unadj, "t3_saa_slope"), name="Change in slope between pre- and \n post-stressor alpha-amylase", age=28),
  data.frame(readjustfunc(res_unadj, "t3_residual_saa"), name="Residualized gain score for alpha-amylase", age=28),
  data.frame(readjustfunc(res_unadj, "t3_cort_z01"), name="Pre-stressor salivary cortisol", age=28),
  data.frame(readjustfunc(res_unadj, "t3_cort_z03"), name="Post-stressor salivary cortisol", age=28),
  data.frame(readjustfunc(res_unadj, "t3_cort_slope"), name="Change in slope between pre- and \n post-stressor cortisol", age=28),
  data.frame(readjustfunc(res_unadj, "t3_residual_cort"), name="Residualized gain score for cortisol", age=28),
  data.frame(readjustfunc(res_unadj, "t3_map"), name="Mean arterial pressure", age=28),
  data.frame(readjustfunc(res_unadj, "t3_hr_mean"), name="Resting heart rate", age=28),
  data.frame(readjustfunc(res_unadj, "t3_gcr_mean"), name="NR3C1 exon 1F promoter methylation", age=28),
  data.frame(readjustfunc(res_unadj, "t3_gcr_cpg12"), name="NGFI-A transcription factor binding site", age=28)
)

d$age <- as.factor(d$age)
d$tr <- c("Control v. Nutrition + Water + Sanitation + Handwashing")

stress_plot_fun <- function(d, var){
  df <- d[d$Y==var,]
  
  p <- ggplot(df, (aes(x=age, y=Mean.difference, fill=tr))) + 
    geom_point(size=3, col="blue") +
    geom_errorbar(aes(ymin=ci.l, ymax=ci.u), col="blue",
                  alpha=0.3, width = 0.3, size = 1.5) +
    labs(y = " ", x = "", title=df$name, fill=" ") +
    coord_cartesian(ylim=c(df$`ci.l`[1]-(0.5*abs(df$ci.l)), df$`ci.u`[1]+(0.5*abs(df$ci.l)))) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  
  return(p)  
}

stress_plot_fun_wlabels <- function(d, var){
  df <- d[d$Y==var,]
  
  p <- ggplot(df, (aes(x=age, y=Mean.difference, fill=tr))) + 
    geom_point(size=3, col="blue") +
    geom_errorbar(aes(ymin=ci.l, ymax=ci.u), col="blue",
                  alpha=0.3, width = 0.3, size = 1.5) +
    labs(y = " ", x = "Child age, months", title=df$name, fill=" ") +
    coord_cartesian(ylim=c(df$`ci.l`[1]-(0.5*abs(df$ci.l)), df$`ci.u`[1]+(0.5*abs(df$ci.l)))) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  
  return(p)  
}


f2III <- stress_plot_fun(d, "t2_f2_8ip")
f223d <- stress_plot_fun(d, "t2_f2_23d")
f2VI <- stress_plot_fun(d, "t2_f2_VI")
f212 <- stress_plot_fun(d, "t2_f2_12i")
map <- stress_plot_fun(d, "t3_map")
hr <- stress_plot_fun(d, "t3_hr_mean")
saa1 <- stress_plot_fun(d, "t3_saa_z01")
saa2 <- stress_plot_fun(d, "t3_saa_z02")
cort1 <- stress_plot_fun(d, "t3_cort_z01")
cort2 <- stress_plot_fun(d, "t3_cort_z03")
gcr <- stress_plot_fun(d, "t3_gcr_mean")
cpg12 <- stress_plot_fun(d, "t3_gcr_cpg12")
saam <- stress_plot_fun_wlabels(d, "t3_saa_slope")
cortm <- stress_plot_fun_wlabels(d, "t3_cort_slope")
saar <- stress_plot_fun_wlabels(d, "t3_residual_saa")
cortr <- stress_plot_fun_wlabels(d, "t3_residual_cort")



p1 <- ggarrange(f2III, f223d, f2VI, f212, map, hr, saa1, saa2, cort1, cort2, gcr, cpg12, saam, cortm, saar, cortr,
                ncol=4, nrow=4, common.legend = TRUE, legend="bottom")



ggsave(p1, file = here::here("figures/stress_unadjusted_diff.png"), height=9, width=14)

