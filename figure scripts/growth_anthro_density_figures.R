

# load packages
rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)


#load covariates, exposures, outcomes dataset
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))


head(d)


#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

color_levels = c("Change in LAZ\nbetween years 1 and 2", "Change in WLZ\nbetween years 1 and 2", "Change in WAZ\nbetween years 1 and 2", "Change in HCZ\nbetween years 1 and 2",  
                 "LAZ - year 1", "WLZ - year 1","WAZ - year q", "HCZ - year 1" ,
                 "LAZ - year 2", "WLZ - year 2","WAZ - year 2", "HCZ - year 2" ,
                 "Length velocity\nbetween years 1 and 2", "Weight velocity\nbetween years 1 and 2", "Head circumference velocity\nbetween years 1 and 2")



TS_density_plot<-function(Y, X, Xlabel, Ylabel, col_i=1){
  
  dat <- data.frame(Y,X)
  #colnames(dat)[1] <- Ylabel
  dat <- dat[complete.cases(dat),]
  
  
  Acuts=as.numeric(summary(dat$X)[c(2,3,5)])


  dat$x<-factor(findInterval(dat$X, Acuts))
  levels(dat$x) = c("Quartile 1","Quartile 2","Quartile 3","Quartile 4")
  
  dat <- dat %>% group_by(x) %>% mutate(Xmedian=median(Y), Xmedian2=ifelse(row_number()==1, Xmedian, NA)) %>% ungroup()
                          
  
  Y_color=rep(tableau10[col_i],4)

  
  p <- ggplot(data=dat, aes(x=Y,group=x, fill=x)) +
    facet_wrap(~x,ncol=1) +
    geom_density(aes(y=..density.. , alpha=x),color=NA) +
    geom_vline(aes(xintercept = Xmedian)) +
    geom_text(aes(x = Xmedian, y=1, label=Xmedian2), hjust=-0.5) +
    scale_alpha_manual(values=c(0.4, 0.6, 0.8, 1),guide=F) +
    #scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
    #scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
    #scale_color_manual(values=Y_color) + 
    scale_fill_manual(values=Y_color) + 
    labs(x=Xlabel, y=paste0("Quartile of ",Ylabel)) +
    theme_minimal(base_size=16) +
    theme(legend.position = "none")
  
  return(p)
  
}




#Hypothesis 1
h1_delta_laz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_laz_t2_t3, X=d$delta_TS, Ylabel="LAZ change", Xlabel="Change in T/S ratio", col_i=1)
h1_delta_waz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_waz_t2_t3, X=d$delta_TS, Ylabel="WAZ change", Xlabel="Change in T/S ratio", col_i=3)
h1_delta_whz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_whz_t2_t3, X=d$delta_TS, Ylabel="WLZ change", Xlabel="Change in T/S ratio", col_i=2)
h1_delta_hcz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_hcz_t2_t3, X=d$delta_TS, Ylabel="HCZ change", Xlabel="Change in T/S ratio", col_i=4) 

#Hypothesis 2
h2_len_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$len_velocity_t2_t3, X=d$delta_TS, Ylabel="Length velocity", Xlabel="Change in T/S ratio", col_i=5)
h2_wei_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$wei_velocity_t2_t3, X=d$delta_TS, Ylabel="Weight velocity", Xlabel="Change in T/S ratio", col_i=6)
h2_hc_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$hc_velocity_t2_t3, X=d$delta_TS, Ylabel="Head circumference velocity", Xlabel="Change in T/S ratio", col_i=7)

#Hypothesis 3
h3_laz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$laz_t3, X=d$delta_TS, Ylabel="Year 2 LAZ", Xlabel="Change in T/S ratio", col_i=1)
h3_waz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$waz_t3, X=d$delta_TS, Ylabel="Year 2 LAZ", Xlabel="Change in T/S ratio", col_i=3)
h3_whz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$whz_t3, X=d$delta_TS, Ylabel="Year 2 WLZ", Xlabel="Change in T/S ratio", col_i=2)
h3_hcz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$hcz_t3, X=d$delta_TS, Ylabel="Year 2 HCZ", Xlabel="Change in T/S ratio", col_i=4)

#Hypothesis 4
h4_laz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$laz_t2, X=d$TS_t2, Ylabel="Year 1 LAZ", Xlabel="Year 1 T/S ratio", col_i=1)
h4_waz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$waz_t2, X=d$TS_t2, Ylabel="Year 1 WAZ", Xlabel="Year 1 T/S ratio", col_i=3)
h4_whz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$whz_t2, X=d$TS_t2, Ylabel="Year 1 WLZ", Xlabel="Year 1 T/S ratio", col_i=2)
h4_hcz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$hcz_t2, X=d$TS_t2, Ylabel="Year 1 HCZ", Xlabel="Year 1 T/S ratio", col_i=4)

#Hypothesis 5
h5_laz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$laz_t3, X=d$TS_t3, Ylabel="Year 2 LAZ", Xlabel="Year 2 T/S ratio", col_i=1)
h5_waz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$waz_t3, X=d$TS_t3, Ylabel="Year 2 WAZ", Xlabel="Year 2 T/S ratio", col_i=3)
h5_whz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$whz_t3, X=d$TS_t3, Ylabel="Year 2 WLZ", Xlabel="Year 2 T/S ratio", col_i=2)
h5_hcz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$hcz_t3, X=d$TS_t3, Ylabel="Year 2 HCZ", Xlabel="Year 2 T/S ratio", col_i=4)

#Hypothesis 6
h6_laz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$laz_t3, X=d$TS_t2, Ylabel="Year 2 LAZ", Xlabel="Year 1 T/S ratio", col_i=1)
h6_waz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$waz_t3, X=d$TS_t2, Ylabel="Year 2 WAZ", Xlabel="Year 1 T/S ratio", col_i=3)
h6_whz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$whz_t3, X=d$TS_t2, Ylabel="Year 2 WLZ", Xlabel="Year 1 T/S ratio", col_i=2)
h6_hcz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$hcz_t3, X=d$TS_t2, Ylabel="Year 2 HCZ", Xlabel="Year 1 T/S ratio", col_i=4)

#Hypothesis 7
h7_len_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$len_velocity_t2_t3, X=d$TS_t2, Ylabel="Length velocity", Xlabel="Year 1 T/S ratio", col_i=5)
h7_wei_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$wei_velocity_t2_t3, X=d$TS_t2, Ylabel="Weight velocity", Xlabel="Year 1 T/S ratio", col_i=6)
h7_hc_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$hc_velocity_t2_t3, X=d$TS_t2, Ylabel="Head circumference velocity", Xlabel="Year 1 T/S ratio", col_i=7)

#Hypothesis 8
h8_delta_laz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_laz_t2_t3, X=d$TS_t2, Ylabel="LAZ change", Xlabel="Year 1 T/S ratio", col_i=1)
h8_delta_waz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_waz_t2_t3, X=d$TS_t2, Ylabel="WAZ change", Xlabel="Year 1 T/S ratio", col_i=3)
h8_delta_whz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_whz_t2_t3, X=d$TS_t2, Ylabel="WLZ change", Xlabel="Year 1 T/S ratio", col_i=2)
h8_delta_hcz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_hcz_t2_t3, X=d$TS_t2, Ylabel="HCZ change", Xlabel="Year 1 T/S ratio", col_i=4)



#Save density plots
save(
  #Hypothesis 1
  h1_delta_laz_v_delta_ts_dens,
  h1_delta_waz_v_delta_ts_dens,
  h1_delta_whz_v_delta_ts_dens,
  h1_delta_hcz_v_delta_ts_dens,
  #Hypothesis 2
  h2_len_velocity_v_delta_ts_dens,
  h2_wei_velocity_v_delta_ts_dens,
  h2_hc_velocity_v_delta_ts_dens,
  #Hypothesis 3
  h3_laz_t3_vs_delta_ts_dens,
  h3_waz_t3_vs_delta_ts_dens,
  h3_whz_t3_vs_delta_ts_dens,
  h3_hcz_t3_vs_delta_ts_dens,
  #Hypothesis 4
  h4_laz_t2_vs_ts_t2_dens,
  h4_waz_t2_vs_ts_t2_dens,
  h4_whz_t2_vs_ts_t2_dens,
  h4_hcz_t2_vs_ts_t2_dens,
  #Hypothesis 5
  h5_laz_t3_vs_ts_t3_dens,
  h5_waz_t3_vs_ts_t3_dens,
  h5_whz_t3_vs_ts_t3_dens,
  h5_hcz_t3_vs_ts_t3_dens,
  #Hypothesis 6
  h6_laz_t3_vs_ts_t2_dens,
  h6_waz_t3_vs_ts_t2_dens,
  h6_whz_t3_vs_ts_t2_dens,
  h6_hcz_t3_vs_ts_t2_dens,
  #Hypothesis 7
  h7_len_veloc_vs_ts_t2_dens,
  h7_wei_veloc_vs_ts_t2_dens,
  h7_hc_veloc_vs_ts_t2_dens,
  #Hypothesis 8
  h8_delta_laz_v_ts_t2_dens,
  h8_delta_waz_v_ts_t2_dens,
  h8_delta_whz_v_ts_t2_dens,
  h8_delta_hcz_v_ts_t2_dens,
  file=here("/audrie results/telo_density_plot_objects.Rdata"))


  p1 <- plot_grid(
  h1_delta_laz_v_delta_ts_dens,
  h1_delta_waz_v_delta_ts_dens,
  h1_delta_whz_v_delta_ts_dens,
  h1_delta_hcz_v_delta_ts_dens,  nrow=1, labels = c("","","",""))
  p2 <- plot_grid(
  h2_len_velocity_v_delta_ts_dens,
  h2_wei_velocity_v_delta_ts_dens,
  h2_hc_velocity_v_delta_ts_dens, nrow=1, labels = c("","",""))
  p3 <- plot_grid(
  h3_laz_t3_vs_delta_ts_dens,
  h3_waz_t3_vs_delta_ts_dens,
  h3_whz_t3_vs_delta_ts_dens,
  h3_hcz_t3_vs_delta_ts_dens, nrow=1, labels = c("","","",""))
  p4 <- plot_grid(
  h4_laz_t2_vs_ts_t2_dens,
  h4_waz_t2_vs_ts_t2_dens,
  h4_whz_t2_vs_ts_t2_dens,
  h4_hcz_t2_vs_ts_t2_dens, nrow=1, labels = c("","","",""))
  p5 <- plot_grid(
  h5_laz_t3_vs_ts_t3_dens,
  h5_waz_t3_vs_ts_t3_dens,
  h5_whz_t3_vs_ts_t3_dens,
  h5_hcz_t3_vs_ts_t3_dens, nrow=1, labels = c("","","",""))
  p6 <- plot_grid(
  h6_laz_t3_vs_ts_t2_dens,
  h6_waz_t3_vs_ts_t2_dens,
  h6_whz_t3_vs_ts_t2_dens,
  h6_hcz_t3_vs_ts_t2_dens, nrow=1, labels = c("","","",""))
  p7 <- plot_grid(
  h7_len_veloc_vs_ts_t2_dens,
  h7_wei_veloc_vs_ts_t2_dens,
  h7_hc_veloc_vs_ts_t2_dens, nrow=1, labels = c("","",""))
  p8 <- plot_grid(
  h8_delta_laz_v_ts_t2_dens,
  h8_delta_waz_v_ts_t2_dens,
  h8_delta_whz_v_ts_t2_dens,
  h8_delta_hcz_v_ts_t2_dens, nrow=1, labels = c("","","",""))



ggsave(p1, file = here("figures/density plots/telo-density_h1.png"), height=6, width=14)
ggsave(p2, file = here("figures/density plots/telo-density_h2.png"), height=6, width=14)
ggsave(p3, file = here("figures/density plots/telo-density_h3.png"), height=6, width=14)
ggsave(p4, file = here("figures/density plots/telo-density_h4.png"), height=6, width=14)
ggsave(p5, file = here("figures/density plots/telo-density_h5.png"), height=6, width=14)
ggsave(p6, file = here("figures/density plots/telo-density_h6.png"), height=6, width=14)
ggsave(p7, file = here("figures/density plots/telo-density_h7.png"), height=6, width=14)
ggsave(p8, file = here("figures/density plots/telo-density_h8.png"), height=6, width=14)
