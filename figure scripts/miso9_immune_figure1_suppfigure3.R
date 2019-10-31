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
  data.frame(readjustfunc(t2_ratio_il1_il10_N_tr), name="Interleukin-1/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il1_il10_N_tr), name="Interleukin-1/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il6_il10_N_tr), name="Interleukin-6/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il6_il10_N_tr), name="Interleukin-6/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_tnf_il10_N_tr), name="Tumor necrosis factor-α/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_tnf_il10_N_tr), name="Tumor necrosis factor-α/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il10_N_tr), name="Interleukin-12/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il10_N_tr), name="Interleukin-12/Interleukin-10", age=28),
  data.frame(readjustfunc(t3_ratio_ifn_il10_N_tr), name="Interferon-γ/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il4_il10_N_tr), name="Interleukin-4/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il4_il10_N_tr), name="Interleukin-4/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il5_il10_N_tr), name="Interleukin-5/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il5_il10_N_tr), name="Interleukin-5/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il13_il10_N_tr), name="Interleukin-13/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il13_il10_N_tr), name="Interleukin-13/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il17_il10_N_tr), name="Interleukin-17/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il17_il10_N_tr), name="Interleukin-17/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il21_il10_N_tr), name="Interleukin-21/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il21_il10_N_tr), name="Interleukin-21/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il2_il10_N_tr), name="Interleukin-2/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_il2_il10_N_tr), name="Interleukin-2/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_gmc_il10_N_tr), name="Granulocyte-macrophage colony-stimulating factor/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_gmc_il10_N_tr), name="Granulocyte-macrophage colony-stimulating factor/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il4_N_tr), name="Interleukin-12/Interleukin-4", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il4_N_tr), name="Interleukin-12/Interleukin-4", age=28),
  data.frame(readjustfunc(t2_ratio_ifn_il4_N_tr), name="Interferon-γ/Interleukin-4", age=14),
  data.frame(readjustfunc(t3_ratio_ifn_il4_N_tr), name="Interferon-γ/Interleukin-4", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il5_N_tr), name="Interleukin-12/Interleukin-5", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il5_N_tr), name="Interleukin-12/Interleukin-5", age=28),
  data.frame(readjustfunc(t2_ratio_ifn_il5_N_tr), name="Interferon-γ/Interleukin-5", age=14),
  data.frame(readjustfunc(t3_ratio_ifn_il5_N_tr), name="Interferon-γ/Interleukin-5", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il13_N_tr), name="Interleukin-12/Interleukin-13", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il13_N_tr), name="Interleukin-12/Interleukin-13", age=28),
  data.frame(readjustfunc(t2_ratio_ifn_il13_N_tr), name="Interferon-γ/Interleukin-13", age=14),
  data.frame(readjustfunc(t3_ratio_ifn_il13_N_tr), name="Interferon-γ/Interleukin-13", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il17_N_tr), name="Interleukin-12/Interleukin-17", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il17_N_tr), name="Interleukin-12/Interleukin-17", age=28),
  data.frame(readjustfunc(t2_ratio_ifn_il17_N_tr), name="Interferon-γ/Interleukin-17", age=14),
  data.frame(readjustfunc(t3_ratio_ifn_il17_N_tr), name="Interferon-γ/Interleukin-17", age=28),
  data.frame(readjustfunc(t2_ratio_il12_il21_N_tr), name="Interleukin-12/Interleukin-21", age=14),
  data.frame(readjustfunc(t3_ratio_il12_il21_N_tr), name="Interleukin-12/Interleukin-21", age=28),
  data.frame(readjustfunc(t2_ratio_ifn_il21_N_tr), name="Interferon-γ/Interleukin-21", age=14),
  data.frame(readjustfunc(t3_ratio_ifn_il21_N_tr), name="Interferon-γ/Interleukin-21", age=28),
  data.frame(readjustfunc(t2_ratio_pro_il10_N_tr), name="Pro-inflammatory cytokines/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_pro_il10_N_tr), name="Pro-inflammatory cytokines/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_th1_il10_N_tr), name="Th1/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_th1_il10_N_tr), name="Th1/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_th2_il10_N_tr), name="Th2/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_th2_il10_N_tr), name="Th2/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_th17_il10_N_tr), name="Th17/Interleukin-10", age=14),
  data.frame(readjustfunc(t3_ratio_th17_il10_N_tr), name="Th17/Interleukin-10", age=28),
  data.frame(readjustfunc(t2_ratio_th1_th2_N_tr), name="Th1/Th2", age=14),
  data.frame(readjustfunc(t3_ratio_th1_th2_N_tr), name="Th1/Th2", age=28),
  data.frame(readjustfunc(t2_ratio_th1_th17_N_tr), name="Th1/Th17", age=14),
  data.frame(readjustfunc(t3_ratio_th1_th17_N_tr), name="Th1/Th17", age=28)
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
    coord_cartesian(ylim=c(min(df$mean)-max(df$sd)-0.5, max(df$mean)+max(df$sd)+0.5)) +
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
    coord_cartesian(ylim=c(min(df$mean)-max(df$sd)-0.5, max(df$mean)+max(df$sd)+0.5)) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x = element_text(size=9),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


p1 <-immune_plot_fun(d, "Interleukin-1/Interleukin-10")
p2 <-immune_plot_fun(d, "Interleukin-6/Interleukin-10")
p3 <-immune_plot_fun(d, "Tumor necrosis factor-α/Interleukin-10")
p4 <-immune_plot_fun(d, "Interleukin-12/Interleukin-10")
p5 <-immune_plot_fun(d, "Interferon-γ/Interleukin-10")
p6 <-immune_plot_fun(d, "Interleukin-4/Interleukin-10")
p7 <-immune_plot_fun(d, "Interleukin-5/Interleukin-10")
p8 <-immune_plot_fun(d, "Interleukin-13/Interleukin-10")
p9 <-immune_plot_fun(d, "Interleukin-17/Interleukin-10")
p10 <-immune_plot_fun(d, "Interleukin-21/Interleukin-10")
p11 <-immune_plot_fun(d, "Interleukin-2/Interleukin-10")
p12 <-immune_plot_fun(d, "Granulocyte-macrophage colony-stimulating factor/Interleukin-10")
p13 <-immune_plot_fun(d, "Interleukin-12/Interleukin-4")
p14 <-immune_plot_fun(d, "Interferon-γ/Interleukin-4")
p15 <-immune_plot_fun(d, "Interleukin-12/Interleukin-5")
p16 <-immune_plot_fun(d, "Interferon-γ/Interleukin-5")
p17 <-immune_plot_fun(d, "Interleukin-12/Interleukin-13")
p18 <-immune_plot_fun(d, "Interferon-γ/Interleukin-13")
p19 <-immune_plot_fun(d, "Interleukin-12/Interleukin-17")
p20 <-immune_plot_fun(d, "Interferon-γ/Interleukin-17")
p21 <-immune_plot_fun(d, "Interleukin-12/Interleukin-21")
p22 <-immune_plot_fun(d, "Interferon-γ/Interleukin-21")
p23 <-immune_plot_fun(d, "Pro-inflammatory cytokines/Interleukin-10")
p24 <-immune_plot_fun_wlabel(d, "Th1/Interleukin-10")
p25 <-immune_plot_fun_wlabel(d, "Th2/Interleukin-10")
p26 <-immune_plot_fun_wlabel(d, "Th17/Interleukin-10")
p27 <-immune_plot_fun_wlabel(d, "Th1/Th2")
p28 <-immune_plot_fun_wlabel(d, "Th1/Th17")




p <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
               p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p28,
                ncol=4, nrow=7, common.legend = TRUE, legend="bottom")


ggsave(p27, file = here("figures/miso9-figure1-immune.png"), height = 10, width = 8)
ggsave(p, file = here("figures/miso9-figures3-immune.png"), height=15, width=12)

