<<<<<<< HEAD
rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
load(here("audrie results/immune_N_tr_means.RData"))

readjustfunc <- function(data){
  select(data, tr, mean, sd)
}

d <- rbind(
  data.frame(readjustfunc(gmc_t2_N_tr), name="Granulocyte-macrophage \n colony-stimulating factor (pg/ml)", age=14),
  data.frame(readjustfunc(gmc_t3_N_tr), name="Granulocyte-macrophage \n colony-stimulating factor (pg/ml)", age=28),
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
    coord_cartesian(ylim=c(min(df$mean, na.rm=T)-max(df$sd, na.rm=T)-0.5*max(df$sd, na.rm=T), 
                           max(df$mean, na.rm=T)+max(df$sd, na.rm=T)+0.5*max(df$sd, na.rm=T))) +
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
    coord_cartesian(ylim=c(min(df$mean, na.rm=T)-max(df$sd, na.rm=T)-0.5*max(df$sd, na.rm=T), 
                           max(df$mean, na.rm=T)+max(df$sd, na.rm=T)+0.5*max(df$sd, na.rm=T))) +
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
    coord_cartesian(ylim=c(min(df$mean, na.rm=T)-max(df$sd, na.rm=T)-0.5*max(df$sd, na.rm=T), 
                           max(df$mean, na.rm=T)+max(df$sd, na.rm=T)+0.5*max(df$sd, na.rm=T))) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x = element_text(size=9),
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          legend.position = "right",
          legend.title = element_blank(),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


gmc<-immune_plot_fun(d, "Granulocyte-macrophage \n colony-stimulating factor (pg/ml)")
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



ggsave(p1, file = here("figures/immune/immune_suppfigure1.tiff"), height=9, width=14, dpi=300)
=======
library(tibble)
data <- tibble(x = 1:100, y= 1:100)
head(data)

library(dplyr)
data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_void() ->
  p

p +
  geom_rect(xmin = 25, xmax=75, ymin=96, ymax=100, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=98,label= '13,279 compounds assessed for eligibility', size=2.5) +
  annotate('text', x= 50, y=102,label= 'Figure S1: CONSORT Diagram for the WASH Benefits immune status and growth factor study population', size=3) ->
  p

p +
  geom_rect(xmin = 58, xmax=104, ymin=87, ymax=95, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 81, y=90,label= 'Excluded: 7,728 compounds \n 7,429 compounds excluded to create bu???er zones\n 219 compounds did not meet enrollment criteria\n 80 compounds declined to participate
', size=2.5) +
  annotate('text', x= 3, y=90,label= 'Enrollment', size=4) +
  geom_rect(xmin = 30, xmax=70, ymin=82, ymax=86, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=85,label= '
720 clusters created and randomly allocated \n 5,551 compounds randomly allocated', size=2.5)  +
  annotate('text', x= 2.5, y=84,label= 'Allocation', size=4) +
  geom_rect(xmin = 9, xmax=25, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=80,label= '
Control \n 180 clusters \n 1,382 households', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=80,label= '
Water+Sanitation+Handwashing+Nutrition \n 90 clusters \n 686 households ', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=63, ymax=75, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=70,label= '
Year 1 \n 63 clusters \n 480 children \n Year 2 \n 67 clusters \n 505 children ', size=2.5)+
  geom_rect(xmin = 71, xmax=104, ymin=32, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=48,label= '
Year 1 \n 100 children lost to follow-up \n 9 moved \n 29 absent \n 14 withdrew \n 37 no live birth \n 11 child death \n Year 2 \n 25 new children measured  \n 104 children lost to follow-up \n 28 moved \n 2 absent \n 18 withdrew \n 38 no live birth \n 18 child death ', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=19, ymax=31, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=26,label= '
Year 1 \n 63 clusters \n 380 children \n Year 2 \n 67 clusters \n 401 children ', size=2.5) + 
  geom_rect(xmin = 71, xmax=104, ymin=10, ymax=18, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=15,label= '
Year 1 \n 69 missing outcome \n Year 2 \n 22 missing outcome', size=2.5) + 
    geom_rect(xmin = 71, xmax=104, ymin=-3, ymax=9, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=4,label= '
Year 1 \n 62 clusters \n 311 children \n Year 2 \n 67 clusters \n 379 children', size=2.5) +
  geom_rect(xmin = 9, xmax=25, ymin=63, ymax=75, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=70,label= '
Year 1 \n 68 clusters \n 516 children \n Year 2 \n 68 clusters \n 516 children ', size=2.5) +
  annotate('text', x= 3, y=70,label= 'Subsample \n Target', size=3.5) +
  geom_rect(xmin = 6, xmax=28, ymin=32, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=48,label= '
Year 1 \n 140 children lost to follow-up \n 14 moved \n 16 absent \n 62 withdrew \n 29 no live birth \n 19 child death \n Year 2 \n 0 new children measured  \n 158 children lost to follow-up \n 35 moved \n 3 absent \n 72 withdrew \n 29 no live birth \n 19 child death ', size=2.5) +
  annotate('text', x= 1, y=48,label= 'Follow-up', size=3.3) +
  geom_rect(xmin = 9, xmax=25, ymin=19, ymax=31, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=26,label= '
Year 1 \n 68 clusters \n 376 children \n Year 2 \n 68 clusters \n 358 children ', size=2.5) +
  annotate('text', x= 2.5, y=26,label= 'Subsample \n Enrollment', size=3.5) +
  geom_rect(xmin = 9, xmax=25, ymin=10, ymax=18, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=15,label= '
Year 1 \n 91 missing outcome \n Year 2 \n 33 missing outcome', size=2.5) +
  annotate('text', x= 2.5, y=15,label= 'Specimen \n Collection', size=3.5) +
  annotate('text', x= 2.5, y=4,label= 'Analysis', size=3.5) +
  geom_rect(xmin = 9, xmax=25, ymin=-3, ymax=9, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=4,label= '
Year 1 \n 68 clusters \n 285 children \n Year 2 \n 68 clusters \n 325 children', size=2.5) ->
  p
p

p +
  geom_segment(
    x=50, xend=50, y=96, yend=86, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=58, y=91, yend=91, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=76, yend=75, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=63, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=32, yend=31, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=19, yend=18, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=10, yend=9, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=76, yend=75, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=63, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=32, yend=31, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=19, yend=18, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=10, yend=9, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=30, xend=17, y=85, yend=85, 
    size=0.15, linejoin = "mitre", lineend = "butt") + 
  geom_segment(
    x=17, xend=17, y=85, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=70, xend=88, y=85, yend=85, 
    size=0.15, linejoin = "mitre", lineend = "butt") + 
  geom_segment(
    x=88, xend=88, y=85, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

ggsave(p, file = here("figures/immune/immune_figures1.tiff"), height=14, width=9)


library(rvg)
library(tidyverse)
library(officer)

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- officer::ph_with(doc,  value = p,
                        location = ph_location_type (type = "body"))
print(doc, target = "my_plot.pptx")
>>>>>>> 786f41e9d452f33ad429b5fadd80bfc5061bf32c

