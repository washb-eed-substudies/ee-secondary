rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
load(here('audrie results/immune_unadj_glm.RData'))

d <- rbind(
  data.frame(t2_gmc_unadj_L, name="Granulocyte-macrophage colony-stimulating factor (pg/ml)", age=14),
  data.frame(t3_gmc_unadj_L, name="Granulocyte-macrophage colony-stimulating factor (pg/ml)", age=28),
  data.frame(t2_ifn_unadj_L, name="Interferon-ç (pg/ml)", age=14),
  data.frame(t3_ifn_unadj_L, name="Interferon-ç (pg/ml)", age=28),
  data.frame(t2_il10_unadj_L, name="Interleukin-10 (pg/ml)", age=14),
  data.frame(t3_il10_unadj_L, name="Interleukin-10 (pg/ml)", age=28),
  data.frame(t2_il12_unadj_L, name="Interleukin-12 (pg/ml)", age=14),
  data.frame(t3_il12_unadj_L, name="Interleukin-12 (pg/ml)", age=28),
  data.frame(t2_il13_unadj_L, name="Interleukin-13 (pg/ml)", age=14),
  data.frame(t3_il13_unadj_L, name="Interleukin-13 (pg/ml)", age=28),
  data.frame(t2_il17_unadj_L, name="Interleukin-17 (pg/ml)", age=14),
  data.frame(t3_il17_unadj_L, name="Interleukin-17 (pg/ml)", age=28),
  data.frame(t2_il1_unadj_L, name="Interleukin-1â (pg/ml)", age=14),
  data.frame(t3_il1_unadj_L, name="Interleukin-1â (pg/ml)", age=28),
  data.frame(t2_il2_unadj_L, name="Interleukin-2 (pg/ml)", age=14),
  data.frame(t3_il2_unadj_L, name="Interleukin-2 (pg/ml)", age=28),
  data.frame(t2_il21_unadj_L, name="Interleukin-21 (pg/ml)", age=14),
  data.frame(t3_il21_unadj_L, name="Interleukin-21 (pg/ml)", age=28),
  data.frame(t2_il4_unadj_L, name="Interleukin-4 (pg/ml)", age=14),
  data.frame(t3_il4_unadj_L, name="Interleukin-4 (pg/ml)", age=28),
  data.frame(t2_il5_unadj_L, name="Interleukin-5 (pg/ml)", age=14),
  data.frame(t3_il5_unadj_L, name="Interleukin-5 (pg/ml)", age=28),
  data.frame(t2_il6_unadj_L, name="Interleukin-6 (pg/ml)", age=14),
  data.frame(t3_il6_unadj_L, name="Interleukin-6 (pg/ml)", age=28),
  data.frame(t2_tnf_unadj_L, name="Tumor necrosis factor-á (pg/ml)", age=14),
  data.frame(t3_tnf_unadj_L, name="Tumor necrosis factor-á (pg/ml)", age=28),
  data.frame(t2_igf_unadj_L, name="Insulin-like growth factor-1 (íg/ml)", age=14),
  data.frame(t3_igf_unadj_L, name="Insulin-like growth factor-1 (íg/ml)", age=28),
  data.frame(t2_crp_unadj_L, name="C-reactive protein (mg/L)", age=14),
  data.frame("RD"=NA, "ci.lb"=NA, "ci.ub"=NA, "SE"=NA, "z"=NA, "P-value"=NA, name = "C-reactive protein (mg/L)", age=28),
  data.frame(t2_agp_unadj_L, name="Alpha-1-acid glycoprotein (g/L)", age=14),
  data.frame("RD"=NA, "ci.lb"=NA, "ci.ub"=NA, "SE"=NA, "z"=NA, "P-value"=NA, name = "Alpha-1-acid glycoprotein (g/L)", age=28)
)

d<-cbind(d, tr="Control vs Nutrition + Water + Sanitation + Handwashing")

d$age <- as.factor(d$age)

immune_plot_fun <- function(d, name){
  df <- d[d$name==name,]
  
  p <- ggplot(df, (aes(x=age, y=RD, group=1, fill=tr))) + 
    geom_point(size=3, col="blue") +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), col="blue",
                   alpha=0.5, width = 0.3, size = 1.5) +
    geom_line(alpha = 0.3, col="blue") +
    labs(y = " ", x =  " ", title=name, fill=" ") +
    coord_cartesian(ylim=c(min(df$`ci.lb`[1], df$`ci.lb`[2], na.rm=TRUE)-0.2, max(df$`ci.ub`[1], df$`ci.ub`[2], na.rm=TRUE)+0.2)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}

immune_plot_fun_wlabels <- function(d, name){
  df <- d[d$name==name,]
  
  p <- ggplot(df, (aes(x=age, y=RD, group=1, fill=tr))) + 
    geom_point(size=3, col="blue") +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), col="blue",
                  alpha=0.5, width = 0.3, size = 1.5) +
    geom_line(alpha = 0.3, col="blue") +
    labs(y = " ", x =  "Child age, months", title=name, fill=" ") +
    coord_cartesian(ylim=c(min(df$`ci.lb`[1], df$`ci.lb`[2], na.rm=TRUE)-0.2, max(df$`ci.ub`[1], df$`ci.ub`[2], na.rm=TRUE)+0.2)) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


gmc<-immune_plot_fun(d, "Granulocyte-macrophage colony-stimulating factor (pg/ml)")
ifn<-immune_plot_fun(d, "Interferon-ç (pg/ml)")
il10<-immune_plot_fun(d, "Interleukin-10 (pg/ml)")
il12<-immune_plot_fun(d, "Interleukin-12 (pg/ml)")
il13<-immune_plot_fun(d, "Interleukin-13 (pg/ml)")
il17<-immune_plot_fun(d, "Interleukin-17 (pg/ml)")
il1<-immune_plot_fun(d, "Interleukin-1â (pg/ml)")
il2<-immune_plot_fun(d, "Interleukin-2 (pg/ml)")
il21<-immune_plot_fun(d, "Interleukin-21 (pg/ml)")
il4<-immune_plot_fun(d, "Interleukin-4 (pg/ml)")
il5<-immune_plot_fun(d, "Interleukin-5 (pg/ml)")
il6<-immune_plot_fun(d, "Interleukin-6 (pg/ml)")
tnf<-immune_plot_fun_wlabels(d, "Tumor necrosis factor-á (pg/ml)")
igf<-immune_plot_fun_wlabels(d, "Insulin-like growth factor-1 (íg/ml)")
crp<-immune_plot_fun_wlabels(d, "C-reactive protein (mg/L)")
agp<-immune_plot_fun_wlabels(d, "Alpha-1-acid glycoprotein (g/L)")




p1 <- ggarrange(gmc, ifn, il10, il12, il13, il17, il1, il2, il21, il4, il5, il6, tnf, igf, crp, agp,
                ncol=4, nrow=4, common.legend = TRUE, legend="bottom")



ggsave(p1, file = here("figures/miso9-figures2-immune.png"), height=9, width=14)
