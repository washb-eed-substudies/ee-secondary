rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
source(here("audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R"))

readjustfunc <- function(data){
  select(data, tr, mean, sd)
}

d <- rbind(
  data.frame(readjustfunc(gmc_t2_N_tr), name="Granulocyte-macrophage colony-stimulating factor (pg/ml)", age=14),
  data.frame(readjustfunc(gmc_t3_N_tr), name="Granulocyte-macrophage colony-stimulating factor (pg/ml)", age=28),
  data.frame(readjustfunc(ifn_t2_N_tr), name="Interferon-γ (pg/ml)", age=14),
  data.frame(readjustfunc(ifn_t3_N_tr), name="Interferon-γ (pg/ml)", age=28),
  data.frame(readjustfunc(il10_t2_N_tr), name="Interleukin-10 (pg/ml)", age=14),
  data.frame(readjustfunc(il10_t3_N_tr), name="Interleukin-10 (pg/ml)", age=28),
  data.frame(readjustfunc(il12_t2_N_tr), name="Interleukin-12 (pg/ml)", age=14),
  data.frame(readjustfunc(il12_t3_N_tr), name="Interleukin-12 (pg/ml)", age=28),
  data.frame(readjustfunc(il13_t2_N_tr), name="Interleukin-13 (pg/ml)", age=14),
  data.frame(readjustfunc(il13_t3_N_tr), name="Interleukin-13 (pg/ml)", age=28),
  data.frame(readjustfunc(il17_t2_N_tr), name="Interleukin-17 (pg/ml)", age=14),
  data.frame(readjustfunc(il17_t3_N_tr), name="Interleukin-17 (pg/ml)", age=28),
  data.frame(readjustfunc(il1_t2_N_tr), name="Interleukin-1β (pg/ml)", age=14),
  data.frame(readjustfunc(il1_t3_N_tr), name="Interleukin-1β (pg/ml)", age=28),
  data.frame(readjustfunc(il2_t2_N_tr), name="Interleukin-2 (pg/ml)", age=14),
  data.frame(readjustfunc(il2_t3_N_tr), name="Interleukin-2 (pg/ml)", age=28),
  data.frame(readjustfunc(il21_t2_N_tr), name="Interleukin-21 (pg/ml)", age=14),
  data.frame(readjustfunc(il21_t3_N_tr), name="Interleukin-21 (pg/ml)", age=28),
  data.frame(readjustfunc(il4_t2_N_tr), name="Interleukin-4 (pg/ml)", age=14),
  data.frame(readjustfunc(il4_t3_N_tr), name="Interleukin-4 (pg/ml)", age=28),
  data.frame(readjustfunc(il5_t2_N_tr), name="Interleukin-5 (pg/ml)", age=14),
  data.frame(readjustfunc(il5_t3_N_tr), name="Interleukin-5 (pg/ml)", age=28),
  data.frame(readjustfunc(il6_t2_N_tr), name="Interleukin-6 (pg/ml)", age=14),
  data.frame(readjustfunc(il6_t3_N_tr), name="Interleukin-6 (pg/ml)", age=28),
  data.frame(readjustfunc(tnf_t2_N_tr), name="Tumor necrosis factor-α (pg/ml)", age=14),
  data.frame(readjustfunc(tnf_t3_N_tr), name="Tumor necrosis factor-α (pg/ml)", age=28),
  data.frame(readjustfunc(igf_t2_N_tr), name="Insulin-like growth factor-1 (μg/ml)", age=14),
  data.frame(readjustfunc(igf_t3_N_tr), name="Insulin-like growth factor-1 (μg/ml)", age=28),
  data.frame(readjustfunc(crp_t2_N_tr), name="C-reactive protein (mg/L)", age=14),
  data.frame(tr = c("Control", "Nutrition + WSH"), mean=c(NA, NA), sd=c(NA, NA), name = "C-reactive protein (mg/L)", age=28),
  data.frame(readjustfunc(agp_t2_N_tr), name="Alpha-1-acid glycoprotein (g/L)", age=14),
  data.frame(tr = c("Control", "Nutrition + WSH"), mean=c(NA, NA), sd=c(NA, NA), name = "Alpha-1-acid glycoprotein (g/L)", age=28)
)

d$age <- as.factor(d$age)

immune_plot_fun <- function(d, name){
  df <- d[d$name==name,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                   size = 1, width = 0.2, alpha = 0.7, position = pos) +
    geom_line(position=pos, alpha = 0.4) +
    scale_color_manual(values=c("blue", "red")) +
    labs(y = " ", x =  " ", title=name) +
    coord_cartesian(ylim=c(df$mean[1]-df$sd[1]-0.5, df$mean[1]+df$sd[1]+0.5)) +
    theme_ki() +
    theme(plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


immune_plot_fun <- function(d, name){
  df <- d[d$name==name,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                  size = 1, width = 0.2, alpha = 0.7, position = pos) +
    geom_line(position=pos, alpha = 0.4) +
    scale_color_manual(values=c("blue", "red")) +
    labs(y = " ", x =  " ", title=name) +
    coord_cartesian(ylim=c(df$mean[1]-df$sd[1]-0.5, df$mean[1]+df$sd[1]+0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}

immune_plot_fun_wlabel <- function(d, name){
  df <- d[d$name==name,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                  size = 1, width = 0.2, alpha = 0.7, position = pos) +
    geom_line(position=pos, alpha = 0.4) +
    scale_color_manual(values=c("blue", "red")) +
    labs(y = " ", x =  "Child age, months", title=name) +
    coord_cartesian(ylim=c(df$mean[1]-df$sd[1]-0.5, df$mean[1]+df$sd[1]+0.5)) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x = element_text(size=9),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


gmc<-immune_plot_fun(d, "Granulocyte-macrophage colony-stimulating factor (pg/ml)")
ifn<-immune_plot_fun(d, "Interferon-γ (pg/ml)")
il10<-immune_plot_fun(d, "Interleukin-10 (pg/ml)")
il12<-immune_plot_fun(d, "Interleukin-12 (pg/ml)")
il13<-immune_plot_fun(d, "Interleukin-13 (pg/ml)")
il17<-immune_plot_fun(d, "Interleukin-17 (pg/ml)")
il1<-immune_plot_fun(d, "Interleukin-1β (pg/ml)")
il2<-immune_plot_fun(d, "Interleukin-2 (pg/ml)")
il21<-immune_plot_fun(d, "Interleukin-21 (pg/ml)")
il4<-immune_plot_fun(d, "Interleukin-4 (pg/ml)")
il5<-immune_plot_fun(d, "Interleukin-5 (pg/ml)")
il6<-immune_plot_fun(d, "Interleukin-6 (pg/ml)")
tnf<-immune_plot_fun_wlabel(d, "Tumor necrosis factor-α (pg/ml)")
igf<-immune_plot_fun_wlabel(d, "Insulin-like growth factor-1 (μg/ml)")
crp<-immune_plot_fun_wlabel(d, "C-reactive protein (mg/L)")
agp<-immune_plot_fun_wlabel(d, "Alpha-1-acid glycoprotein (g/L)")




p1 <- ggarrange(gmc, ifn, il10, il12, il13, il17, il1, il2, il21, il4, il5, il6, tnf, igf, crp, agp,
                ncol=4, nrow=4, common.legend = TRUE, legend="bottom")



ggsave(p1, file = here("figures/miso9-figures2-immune.png"), height=9, width=14)

