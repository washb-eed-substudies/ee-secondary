rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
load(here('audrie results/immune_unadj_glm.RData'))

d <- rbind(
  data.frame(t2_gmc_unadj_L, name="Granulocyte-macrophage colony-stimulating factor", age=14),
  data.frame(t3_gmc_unadj_L, name="Granulocyte-macrophage colony-stimulating factor", age=28),
  data.frame(t2_ifn_unadj_L, name="Interferon-gamma", age=14),
  data.frame(t3_ifn_unadj_L, name="Interferon-gamma", age=28),
  data.frame(t2_il10_unadj_L, name="Interleukin-10", age=14),
  data.frame(t3_il10_unadj_L, name="Interleukin-10", age=28),
  data.frame(t2_il12_unadj_L, name="Interleukin-12", age=14),
  data.frame(t3_il12_unadj_L, name="Interleukin-12", age=28),
  data.frame(t2_il13_unadj_L, name="Interleukin-13", age=14),
  data.frame(t3_il13_unadj_L, name="Interleukin-13", age=28),
  data.frame(t2_il17_unadj_L, name="Interleukin-17", age=14),
  data.frame(t3_il17_unadj_L, name="Interleukin-17", age=28),
  data.frame(t2_il1_unadj_L, name="Interleukin-1beta", age=14),
  data.frame(t3_il1_unadj_L, name="Interleukin-1beta", age=28),
  data.frame(t2_il2_unadj_L, name="Interleukin-2", age=14),
  data.frame(t3_il2_unadj_L, name="Interleukin-2", age=28),
  data.frame(t2_il21_unadj_L, name="Interleukin-21", age=14),
  data.frame(t3_il21_unadj_L, name="Interleukin-21", age=28),
  data.frame(t2_il4_unadj_L, name="Interleukin-4", age=14),
  data.frame(t3_il4_unadj_L, name="Interleukin-4", age=28),
  data.frame(t2_il5_unadj_L, name="Interleukin-5", age=14),
  data.frame(t3_il5_unadj_L, name="Interleukin-5", age=28),
  data.frame(t2_il6_unadj_L, name="Interleukin-6", age=14),
  data.frame(t3_il6_unadj_L, name="Interleukin-6", age=28),
  data.frame(t2_tnf_unadj_L, name="Tumor necrosis factor-alpha", age=14),
  data.frame(t3_tnf_unadj_L, name="Tumor necrosis factor-alpha", age=28),
  data.frame(t2_igf_unadj_L, name="Insulin-like growth factor-1", age=14),
  data.frame(t3_igf_unadj_L, name="Insulin-like growth factor-1", age=28),
  data.frame(t2_crp_unadj_L, name="C-reactive protein", age=14),
  data.frame(t2_agp_unadj_L, name="Alpha-1-acid glycoprotein", age=14)
)

d$age <- as.factor(d$age)

immune_plot_fun <- function(d, name){
  df <- d[d$name==name,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df) + 
    geom_point(x=age, y=RD, size=3, position = pos) +
    geom_linerange(aes(x = age, ymin=ci.lb, ymax=ci.ub),
                   alpha=0.5, size = 2, position = pos) +
    geom_line(x=age, y=RD, position=pos) +
    labs(y = " ", x =  " ", title=name) +
    coord_cartesian(ylim=c(df$ci.lb[1]-0.5, df$ci.up[1]+0.5)) +
    theme_ki() +
    theme(plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


gmc<-immune_plot_fun(d, "Granulocyte-macrophage colony-stimulating factor")
ifn<-immune_plot_fun(d, "Interferon-gamma")
il10<-immune_plot_fun(d, "Interleukin-10")
il12<-immune_plot_fun(d, "Interleukin-12")
il13<-immune_plot_fun(d, "Interleukin-13")
il17<-immune_plot_fun(d, "Interleukin-17")
il1<-immune_plot_fun(d, "Interleukin-1beta")
il2<-immune_plot_fun(d, "Interleukin-2")
il21<-immune_plot_fun(d, "Interleukin-21")
il4<-immune_plot_fun(d, "Interleukin-4")
il5<-immune_plot_fun(d, "Interleukin-5")
il6<-immune_plot_fun(d, "Interleukin-6")
tnf<-immune_plot_fun(d, "Tumor necrosis factor-alpha")
igf<-immune_plot_fun(d, "Insulin-like growth factor-1")
crp<-immune_plot_fun(d, "C-reactive protein")
agp<-immune_plot_fun(d, "Alpha-1-acid glycoprotein")




p1 <- ggarrange(gmc, ifn, il10, il12, il13, il17, il1, il2, il21, il4, il5, il6, tnf, igf, crp, agp,
                ncol=4, nrow=4, common.legend = TRUE, legend="bottom")



ggsave(p1, file = here("figures/miso9-figure1-immune.png"), height=9, width=14)
