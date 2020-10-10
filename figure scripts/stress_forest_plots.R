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


#split data for multiple plot panels
d14 <- d %>% filter(age==14)
d_res <- d %>% filter(name %in% c(""))

unique(d$var)



  p <- ggplot(d, (aes(x=name, y=Mean.difference))) + 
    geom_point(size=3) +
    geom_errorbar(aes(ymin=ci.l, ymax=ci.u),
                   width = 0.3, size = 1) +
    geom_hline(yintercept = 0) +
    facet_wrap(~age, ncol=2, scales="free_y") +
    coord_flip() +
    #labs(y = " ", x = "", title=d$name, fill=" ") +
    #coord_cartesian(ylim=c(d$`ci.l`[1]-(0.5*abs(dfci.l)), d$`ci.u`[1]+(0.5*abs(d$ci.l)))) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  p

  
  
  
  p <- ggplot(d, (aes(x=name, y=Mean.difference))) + 
    geom_point(size=3) +
    geom_errorbar(aes(ymin=ci.l, ymax=ci.u),
                  width = 0.3, size = 1) +
    geom_hline(yintercept = 0) +
    facet_wrap(name~age, ncol=2, scales="free") +
    coord_flip() +
    #labs(y = " ", x = "", title=d$name, fill=" ") +
    #coord_cartesian(ylim=c(d$`ci.l`[1]-(0.5*abs(dfci.l)), d$`ci.u`[1]+(0.5*abs(d$ci.l)))) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  p
  
  

  #Drop residualized gain score
  
  
#ggsave(p, file = here::here("figures/stress_forest_mean.png"), height=9, width=14)
  #ggsave(p, file = here::here("figures/stress_forest_diff.png"), height=9, width=14)
  
