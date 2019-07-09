

# load packages
rm(list=ls())
source(here::here("0-config.R"))


#Load tmle results
load(here("/audrie results/telo_growth_spline_fits.Rdata"))

head(d)


#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")




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

ggsave(p, file="U:/Figures/Stunting Webinar/WLZ_density_region_age.png", width=7, height=5.5)


