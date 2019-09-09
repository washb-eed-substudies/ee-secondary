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
  data.frame(readjustfunc(gmc_t2_N_tr), name="Granulocyte-macrophage colony-stimulating factor", age=14),
  data.frame(readjustfunc(gmc_t3_N_tr), name="Granulocyte-macrophage colony-stimulating factor", age=28),
  data.frame(readjustfunc(ifn_t2_N_tr), name="Interferon-gamma", age=14),
  data.frame(readjustfunc(ifn_t3_N_tr), name="Interferon-gamma", age=28),
  data.frame(readjustfunc(il10_t2_N_tr), name="Interleukin-10", age=14),
  data.frame(readjustfunc(il10_t3_N_tr), name="Interleukin-10", age=28),
  data.frame(readjustfunc(il12_t2_N_tr), name="Interleukin-12", age=14),
  data.frame(readjustfunc(il12_t3_N_tr), name="Interleukin-12", age=28),
  data.frame(readjustfunc(il13_t2_N_tr), name="Interleukin-13", age=14),
  data.frame(readjustfunc(il13_t3_N_tr), name="Interleukin-13", age=28),
  data.frame(readjustfunc(il17_t2_N_tr), name="Interleukin-17", age=14),
  data.frame(readjustfunc(il17_t3_N_tr), name="Interleukin-17", age=28),
  data.frame(readjustfunc(il1_t2_N_tr), name="Interleukin-1beta", age=14),
  data.frame(readjustfunc(il1_t3_N_tr), name="Interleukin-1beta", age=28),
  data.frame(readjustfunc(il2_t2_N_tr), name="Interleukin-2", age=14),
  data.frame(readjustfunc(il2_t3_N_tr), name="Interleukin-2", age=28),
  data.frame(readjustfunc(il21_t2_N_tr), name="Interleukin-21", age=14),
  data.frame(readjustfunc(il21_t3_N_tr), name="Interleukin-21", age=28),
  data.frame(readjustfunc(il4_t2_N_tr), name="Interleukin-4", age=14),
  data.frame(readjustfunc(il4_t3_N_tr), name="Interleukin-4", age=28),
  data.frame(readjustfunc(il5_t2_N_tr), name="Interleukin-5", age=14),
  data.frame(readjustfunc(il5_t3_N_tr), name="Interleukin-5", age=28),
  data.frame(readjustfunc(il6_t2_N_tr), name="Interleukin-6", age=14),
  data.frame(readjustfunc(il6_t3_N_tr), name="Interleukin-6", age=28),
  data.frame(readjustfunc(tnf_t2_N_tr), name="Tumor necrosis factor-alpha", age=14),
  data.frame(readjustfunc(tnf_t3_N_tr), name="Tumor necrosis factor-alpha", age=28),
  data.frame(readjustfunc(igf_t2_N_tr), name="Insulin-like growth factor-1", age=14),
  data.frame(readjustfunc(igf_t3_N_tr), name="Insulin-like growth factor-1", age=28),
  data.frame(readjustfunc(crp_t2_N_tr), name="C-reactive protein", age=14),
  data.frame(readjustfunc(agp_t2_N_tr), name="Alpha-1-acid glycoprotein", age=14)
)

d$age <- as.factor(d$age)

immune_plot_fun <- function(d, name){
  df <- d[d$name==name,]
  pos <- position_dodge(width=0.3)
  
  p <- ggplot(df, aes(x=age, y=mean, color=tr, group=tr)) + 
    geom_point(size=3, position = pos) +
    geom_linerange(aes(ymin=mean - sd, ymax=mean + sd),
                   alpha=0.5, size = 2, position = pos) +
    geom_line(position=pos) +
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
