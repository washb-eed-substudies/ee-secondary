rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
load(here('audrie results/immune_unadj_glm.RData'))

d <- rbind(
  data.frame(t2_ratio_il1_il10_unadj_L, name="Interleukin-1/Interleukin-10", age=14),
  data.frame(t3_ratio_il1_il10_unadj_L, name="Interleukin-1/Interleukin-10", age=28),
  data.frame(t2_ratio_il6_il10_unadj_L, name="Interleukin-6/Interleukin-10", age=14),
  data.frame(t3_ratio_il6_il10_unadj_L, name="Interleukin-6/Interleukin-10", age=28),
  data.frame(t2_ratio_tnf_il10_unadj_L, name="Tumor necrosis factor-á/Interleukin-10", age=14),
  data.frame(t3_ratio_tnf_il10_unadj_L, name="Tumor necrosis factor-á/Interleukin-10", age=28),
  data.frame(t2_ratio_il12_il10_unadj_L, name="Interleukin-12/Interleukin-10", age=14),
  data.frame(t3_ratio_il12_il10_unadj_L, name="Interleukin-12/Interleukin-10", age=28),
  data.frame(t2_ratio_ifn_il10_unadj_L, name="Interferon-ç/Interleukin-10", age=14),
  data.frame(t3_ratio_ifn_il10_unadj_L, name="Interferon-ç/Interleukin-10", age=28),
  data.frame(t2_ratio_il4_il10_unadj_L, name="Interleukin-4/Interleukin-10", age=14),
  data.frame(t3_ratio_il4_il10_unadj_L, name="Interleukin-4/Interleukin-10", age=28),
  data.frame(t2_ratio_il5_il10_unadj_L, name="Interleukin-5/Interleukin-10", age=14),
  data.frame(t3_ratio_il5_il10_unadj_L, name="Interleukin-5/Interleukin-10", age=28),
  data.frame(t2_ratio_il13_il10_unadj_L, name="Interleukin-13/Interleukin-10", age=14),
  data.frame(t3_ratio_il13_il10_unadj_L, name="Interleukin-13/Interleukin-10", age=28),
  data.frame(t2_ratio_il17_il10_unadj_L, name="Interleukin-17/Interleukin-10", age=14),
  data.frame(t3_ratio_il17_il10_unadj_L, name="Interleukin-17/Interleukin-10", age=28),
  data.frame(t2_ratio_il21_il10_unadj_L, name="Interleukin-21/Interleukin-10", age=14),
  data.frame(t3_ratio_il21_il10_unadj_L, name="Interleukin-21/Interleukin-10", age=28),
  data.frame(t2_ratio_il2_il10_unadj_L, name="Interleukin-2/Interleukin-10", age=14),
  data.frame(t3_ratio_il2_il10_unadj_L, name="Interleukin-2/Interleukin-10", age=28),
  data.frame(t2_ratio_gmc_il10_unadj_L, name="Granulocyte-macrophage colony-stimulating factor/Interleukin-10", age=14),
  data.frame(t3_ratio_gmc_il10_unadj_L, name="Granulocyte-macrophage colony-stimulating factor/Interleukin-10", age=28),
  data.frame(t2_ratio_il12_il4_unadj_L, name="Interleukin-12/Interleukin-4", age=14),
  data.frame(t3_ratio_il12_il4_unadj_L, name="Interleukin-12/Interleukin-4", age=28),
  data.frame(t2_ratio_ifn_il4_unadj_L, name="Interferon-ç/Interleukin-4", age=14),
  data.frame(t3_ratio_ifn_il4_unadj_L, name="Interferon-ç/Interleukin-4", age=28),
  data.frame(t2_ratio_il12_il5_unadj_L, name="Interleukin-12/Interleukin-5", age=14),
  data.frame(t3_ratio_il12_il5_unadj_L, name="Interleukin-12/Interleukin-5", age=28),
  data.frame(t2_ratio_ifn_il5_unadj_L, name="Interferon-ç/Interleukin-5", age=14),
  data.frame(t3_ratio_ifn_il5_unadj_L, name="Interferon-ç/Interleukin-5", age=28),
  data.frame(t2_ratio_il12_il13_unadj_L, name="Interleukin-12/Interleukin-13", age=14),
  data.frame(t3_ratio_il12_il13_unadj_L, name="Interleukin-12/Interleukin-13", age=28),
  data.frame(t2_ratio_ifn_il13_unadj_L, name="Interferon-ç/Interleukin-13", age=14),
  data.frame(t3_ratio_ifn_il13_unadj_L, name="Interferon-ç/Interleukin-13", age=28),
  data.frame(t2_ratio_il12_il17_unadj_L, name="Interleukin-12/Interleukin-17", age=14),
  data.frame(t3_ratio_il12_il17_unadj_L, name="Interleukin-12/Interleukin-17", age=28),
  data.frame(t2_ratio_ifn_il17_unadj_L, name="Interferon-ç/Interleukin-17", age=14),
  data.frame(t3_ratio_ifn_il17_unadj_L, name="Interferon-ç/Interleukin-17", age=28),
  data.frame(t2_ratio_il12_il21_unadj_L, name="Interleukin-12/Interleukin-21", age=14),
  data.frame(t3_ratio_il12_il21_unadj_L, name="Interleukin-12/Interleukin-21", age=28),
  data.frame(t2_ratio_ifn_il21_unadj_L, name="Interferon-ç/Interleukin-21", age=14),
  data.frame(t3_ratio_ifn_il21_unadj_L, name="Interferon-ç/Interleukin-21", age=28),
  data.frame(t2_ratio_pro_il10_unadj_L, name="Pro-inflammatory cytokines/Interleukin-10", age=14),
  data.frame(t3_ratio_pro_il10_unadj_L, name="Pro-inflammatory cytokines/Interleukin-10", age=28),
  data.frame(t2_ratio_th1_il10_unadj_L, name="Th1/Interleukin-10", age=14),
  data.frame(t3_ratio_th1_il10_unadj_L, name="Th1/Interleukin-10", age=28),
  data.frame(t2_ratio_th2_il10_unadj_L, name="Th2/Interleukin-10", age=14),
  data.frame(t3_ratio_th2_il10_unadj_L, name="Th2/Interleukin-10", age=28),
  data.frame(t2_ratio_th17_il10_unadj_L, name="Th17/Interleukin-10", age=14),
  data.frame(t3_ratio_th17_il10_unadj_L, name="Th17/Interleukin-10", age=28),
  data.frame(t2_ratio_th1_th2_unadj_L, name="Th1/Th2", age=14),
  data.frame(t3_ratio_th1_th2_unadj_L, name="Th1/Th2", age=28),
  data.frame(t2_ratio_th1_th17_unadj_L, name="Th1/Th17", age=14),
  data.frame(t3_ratio_th1_th17_unadj_L, name="Th1/Th17", age=28)
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
    coord_cartesian(ylim=c(min(df$`ci.lb`)-0.3, max(df$`ci.ub`)+0.3)) +
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
    coord_cartesian(ylim=c(min(df$`ci.lb`)-0.3, max(df$`ci.ub`)+0.3)) +
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


p1 <-immune_plot_fun(d, "Interleukin-1/Interleukin-10")
p2 <-immune_plot_fun(d, "Interleukin-6/Interleukin-10")
p3 <-immune_plot_fun(d, "Tumor necrosis factor-á/Interleukin-10")
p4 <-immune_plot_fun(d, "Interleukin-12/Interleukin-10")
p5 <-immune_plot_fun(d, "Interferon-ç/Interleukin-10")
p6 <-immune_plot_fun(d, "Interleukin-4/Interleukin-10")
p7 <-immune_plot_fun(d, "Interleukin-5/Interleukin-10")
p8 <-immune_plot_fun(d, "Interleukin-13/Interleukin-10")
p9 <-immune_plot_fun(d, "Interleukin-17/Interleukin-10")
p10 <-immune_plot_fun(d, "Interleukin-21/Interleukin-10")
p11 <-immune_plot_fun(d, "Interleukin-2/Interleukin-10")
p12 <-immune_plot_fun(d, "Granulocyte-macrophage colony-stimulating factor/Interleukin-10")
p13 <-immune_plot_fun(d, "Interleukin-12/Interleukin-4")
p14 <-immune_plot_fun(d, "Interferon-ç/Interleukin-4")
p15 <-immune_plot_fun(d, "Interleukin-12/Interleukin-5")
p16 <-immune_plot_fun(d, "Interferon-ç/Interleukin-5")
p17 <-immune_plot_fun(d, "Interleukin-12/Interleukin-13")
p18 <-immune_plot_fun(d, "Interferon-ç/Interleukin-13")
p19 <-immune_plot_fun(d, "Interleukin-12/Interleukin-17")
p20 <-immune_plot_fun(d, "Interferon-ç/Interleukin-17")
p21 <-immune_plot_fun(d, "Interleukin-12/Interleukin-21")
p22 <-immune_plot_fun(d, "Interferon-ç/Interleukin-21")
p23 <-immune_plot_fun(d, "Pro-inflammatory cytokines/Interleukin-10")
p24 <-immune_plot_fun(d, "Th1/Interleukin-10")
p25 <-immune_plot_fun_wlabels(d, "Th2/Interleukin-10")
p26 <-immune_plot_fun_wlabels(d, "Th17/Interleukin-10")
p27 <-immune_plot_fun_wlabels(d, "Th1/Th2")
p28 <-immune_plot_fun_wlabels(d, "Th1/Th17")



p <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
               p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28,
               ncol=4, nrow=7, common.legend = TRUE, legend="bottom")



ggsave(p, file = here("figures/miso9-figures3-immune.png"), height=15, width=12)
