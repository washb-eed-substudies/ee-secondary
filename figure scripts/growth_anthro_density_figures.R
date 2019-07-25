

# load packages
rm(list=ls())
source(here::here("0-config.R"))


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



TS_density_plot<-function(Y, X){
  
  dat <- data.frame(Y,X)
  #colnames(dat)[1] <- Ylabel
  dat <- dat[complete.cases(dat),]
  
  Acuts=as.numeric(summary(dat$X)[c(2,3,5)])


  dat$x<-factor(findInterval(dat$X, Acuts))
  levels(dat$x) = c("Q1","Q2","Q3","Q4")
  
  
  #TO DO
  # make color by outcome
  # but shade by quartile using scale_alpha_manual
  
  p <- ggplot(data=dat, aes(x=Y,group=x,color=x,fill=x)) +
    facet_wrap(~x,ncol=1) +
    geom_density(aes(y=..density..),color=NA,alpha=0.7) +
    #scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
    #scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
    scale_fill_manual(values=tableau10) + 
    labs(x=Ylabel, y="Density") +
    theme_minimal(base_size=16) +
    theme(legend.position = "none")
  
}




#Hypothesis 1
h1_delta_laz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_laz_t2_t3, X=d$delta_TS, Ylabel="test")
h1_delta_waz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_waz_t2_t3, X=d$delta_TS)
h1_delta_whz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_whz_t2_t3, X=d$delta_TS)
h1_delta_hcz_v_delta_ts_dens <- TS_density_plot(Y=d$delta_hcz_t2_t3, X=d$delta_TS) 

#Hypothesis 2
h2_len_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$len_velocity_t2_t3, X=d$delta_TS)
h2_wei_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$wei_velocity_t2_t3, X=d$delta_TS)
h2_hc_velocity_v_delta_ts_dens <- TS_density_plot(Y=d$hc_velocity_t2_t3, X=d$delta_TS)

#Hypothesis 3
h3_laz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$laz_t3, X=d$delta_TS)
h3_waz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$waz_t3, X=d$delta_TS)
h3_whz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$whz_t3, X=d$delta_TS)
h3_hcz_t3_vs_delta_ts_dens <- TS_density_plot(Y=d$hcz_t3, X=d$delta_TS)

#Hypothesis 4
h4_laz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$laz_t2, X=d$TS_t2)
h4_waz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$waz_t2, X=d$TS_t2)
h4_whz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$whz_t2, X=d$TS_t2)
h4_hcz_t2_vs_ts_t2_dens <- TS_density_plot(Y=d$hcz_t2, X=d$TS_t2)

#Hypothesis 5
h5_laz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$laz_t3, X=d$TS_t3)
h5_waz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$waz_t3, X=d$TS_t3)
h5_whz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$whz_t3, X=d$TS_t3)
h5_hcz_t3_vs_ts_t3_dens <- TS_density_plot(Y=d$hcz_t3, X=d$TS_t3)

#Hypothesis 6
h6_laz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$laz_t3, X=d$TS_t2)
h6_waz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$waz_t3, X=d$TS_t2)
h6_whz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$whz_t3, X=d$TS_t2)
h6_hcz_t3_vs_ts_t2_dens <- TS_density_plot(Y=d$hcz_t3, X=d$TS_t2)

#Hypothesis 7
h7_len_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$len_velocity_t2_t3, X=d$TS_t2)
h7_wei_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$wei_velocity_t2_t3, X=d$TS_t2)
h7_hc_veloc_vs_ts_t2_dens <- TS_density_plot(Y=d$hc_velocity_t2_t3, X=d$TS_t2)

#Hypothesis 8
h8_delta_laz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_laz_t2_t3, X=d$TS_t2)
h8_delta_waz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_waz_t2_t3, X=d$TS_t2)
h8_delta_whz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_whz_t2_t3, X=d$TS_t2)
h8_delta_hcz_v_ts_t2_dens <- TS_density_plot(Y=d$delta_hcz_t2_t3, X=d$TS_t2)



#Save density plots
# save(
#   #Hypothesis 1
#   h1_delta_laz_v_delta_ts_dens,
#   h1_delta_waz_v_delta_ts_dens, 
#   h1_delta_whz_v_delta_ts_dens,
#   h1_delta_hcz_v_delta_ts_dens,
#   #Hypothesis 2
#   h2_len_velocity_v_delta_ts_dens,
#   h2_wei_velocity_v_delta_ts_dens,
#   h2_hc_velocity_v_delta_ts_dens,
#   #Hypothesis 3
#   h3_laz_t3_vs_delta_ts_dens, 
#   h3_waz_t3_vs_delta_ts_dens,
#   h3_whz_t3_vs_delta_ts_dens, 
#   h3_hcz_t3_vs_delta_ts_dens, 
#   #Hypothesis 4
#   h4_laz_t2_vs_ts_t2_dens, 
#   h4_waz_t2_vs_ts_t2_dens,
#   h4_whz_t2_vs_ts_t2_dens, 
#   h4_hcz_t2_vs_ts_t2_dens, 
#   #Hypothesis 5
#   h5_laz_t3_vs_ts_t3_dens, 
#   h5_waz_t3_vs_ts_t3_dens, 
#   h5_whz_t3_vs_ts_t3_dens, 
#   h5_hcz_t3_vs_ts_t3_dens, 
#   #Hypothesis 6
#   h6_laz_t3_vs_ts_t2_dens, 
#   h6_waz_t3_vs_ts_t2_dens, 
#   h6_whz_t3_vs_ts_t2_dens, 
#   h6_hcz_t3_vs_ts_t2_dens, 
#   #Hypothesis 7
#   h7_len_veloc_vs_ts_t2_dens, 
#   h7_wei_veloc_vs_ts_t2_dens, 
#   h7_hc_veloc_vs_ts_t2_dens,
#   #Hypothesis 8
#   h8_delta_laz_v_ts_t2_dens,
#   h8_delta_waz_v_ts_t2_dens,
#   h8_delta_whz_v_ts_t2_dens, 
#   h8_delta_hcz_v_ts_t2_dens, 
#   file=here("/audrie results/telo_growth_spline_fits.Rdata"))





p <- ggplot(data=d,aes(x=whz,group=agecat,color=agecat,fill=agecat)) +
  facet_wrap(~agecat,ncol=1) +
  geom_density(aes(y=..density..),color=NA,alpha=0.7)+
  geom_vline(aes(xintercept= -2), size=2) +
  scale_x_continuous(limits = c(-5,5), breaks = c(-4:4)) +
  scale_fill_manual(values=rep(pcols[4],10)) +
  scale_color_manual(values=rep(pcols[4],10))  +
  labs(x="WLZ",y="Density") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none") +
  ggtitle("WLZ distribution by child age")


#Color by region
df <- d %>% filter(agecat %in% c("Birth","3 months","6 months","12 months","18 months","24 months"))
df$agecat <- gsub("months", "mo.", df$agecat)
df$agecat <- factor(df$agecat, levels=c("Birth","3 mo.","6 mo.","12 mo.","18 mo.","24 mo."))
df$region <- factor(df$region, levels=c("Asia","Africa","Latin America"))

df <- df %>% filter(studyid!="TanzaniaChild2")

p <- ggplot(data=df,aes(x=whz,group=region,color=region,fill=region)) +
  #facet_wrap(~agecat,ncol=1) +
  facet_grid(agecat~.,scales='free_y',space='free_y') +
  geom_density(aes(y=..density..),color=NA,alpha=0.5)+
  geom_vline(aes(xintercept= -2), size=2) +
  geom_vline(aes(xintercept= meanWLZ, group=region, color=region), size=2) +
  scale_x_continuous(limits = c(-4,4), breaks = c(-4:4)) +
  scale_fill_manual(values=rep(pcols,10)) +
  scale_color_manual(values=rep(pcols,10))  +
  labs(x="WLZ",y="Density") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none") +
  ggtitle("WLZ distribution by child age and region")
p



