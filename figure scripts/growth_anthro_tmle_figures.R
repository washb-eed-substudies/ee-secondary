

rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)


#Load tmle results
load(here("/audrie results/telo_growth_results.Rdata"))


d <- rbind(
  data.frame(h1adj.res, adjusted=1, hypothesis=1),
  data.frame(h2adj.res, adjusted=1, hypothesis=2),
  data.frame(h3adj.res, adjusted=1, hypothesis=3),
  data.frame(h4adj.res, adjusted=1, hypothesis=4),
  data.frame(h5adj.res, adjusted=1, hypothesis=5),
  data.frame(h6adj.res, adjusted=1, hypothesis=6),
  data.frame(h7adj.res, adjusted=1, hypothesis=7),
  data.frame(h8adj.res, adjusted=1, hypothesis=8)
)

#Clean dataframe
d$ATE[d$level=="Q1"] <- 0

d$level <- paste0(d$level, "\n", d$cutpoints)

unique(d$Y)
unique(d$A)

d$Y <- as.character(d$Y)
d$Y <- gsub("whz", "wlz", d$Y)
d$Y <- factor(d$Y, levels=c("laz_t2", "wlz_t2", "waz_t2", "hcz_t2",
                            "laz_t3", "wlz_t3", "waz_t3", "hcz_t3",
                            "delta_laz_t2_t3", "delta_wlz_t2_t3", "delta_waz_t2_t3", "delta_hcz_t2_t3",    
                            "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3"))

d <- d %>% arrange(Y) %>%
  mutate(
  Ylab = case_when(
    Y=="delta_laz_t2_t3" ~ "Change in LAZ\nbetween years 1 and 2",
    Y=="delta_wlz_t2_t3" ~ "Change in WLZ\nbetween years 1 and 2",
    Y=="delta_waz_t2_t3" ~ "Change in WAZ\nbetween years 1 and 2",
    Y=="delta_hcz_t2_t3" ~ "Change in HCZ\nbetween years 1 and 2",
    Y=="len_velocity_t2_t3" ~ "Length velocity (cm)\nbetween years 1 and 2",
    Y=="wei_velocity_t2_t3" ~ "Weight velocity (kg)\nbetween years 1 and 2",
    Y=="hc_velocity_t2_t3" ~ "Head circumference velocity (cm)\nbetween years 1 and 2",
    Y=="laz_t2" ~ "LAZ at year 1",
    Y=="wlz_t2" ~ "WLZ at year 1",
    Y=="waz_t2" ~ "WAZ at year 1",
    Y=="hcz_t2" ~ "HCZ at year 1",
    Y=="laz_t3" ~ "LAZ at year 2",
    Y=="wlz_t3" ~ "WLZ at year 2",
    Y=="waz_t3" ~ "WAZ at year 2",
    Y=="hcz_t3" ~ "HCZ at year 2"),
  Ylab = factor(Ylab, levels=unique(Ylab)),
  Alab = case_when(
    A=="delta_TS" ~ "Change in telomere length between Years 1 and 2 (T/S ratio)",
    A=="TS_t2" ~ "Telomere length at Year 1 (T/S ratio)",
    A=="TS_t3" ~ "Telomere length at Year 2 (T/S ratio)"),
  anthro = case_when(
    grepl("laz",Y) ~ "LAZ",
    grepl("waz",Y) ~ "WAZ",
    grepl("wlz",Y) ~ "WLZ",
    grepl("hcz",Y) ~ "HCZ",
    grepl("len",Y) ~ "LEN",
    grepl("wei",Y) ~ "WEIGHT",
    grepl("hc_",Y) ~ "HCIR"
  ),
  anthro=factor(anthro,
                levels = c("LAZ","WAZ","WLZ","HCZ",
                           "LEN","WEIGHT","HCIR"))) %>%
  arrange(anthro) %>%
  mutate(Ylab = factor(Ylab, levels=unique(Ylab)))
  



#Seperate out head circumference outcome
dHCZ <- d[grepl("HCZ", d$Ylab),]
d <- d[!grepl("HCZ", d$Ylab),]
d <- droplevels(d)


tmle_plot_fun <- function(d, hypo, title, ylabel="", yrange=NULL, cols=tableau10[c(1,3,2)]){
  
  if(length(hypo)==1){
  df <- d[d$hypothesis==hypo,]
  
  if(is.null(yrange)){
    yrange = c(min(df$CI1, na.rm=T)-0.05, max(df$CI2, na.rm=T)+0.05)
  }

  #add ref label
  df$ref <-"ref."
  df$ref[!grepl("Q1",df$level)] <- NA
  
  p <- ggplot(df, aes(x=level, y=ATE)) + 
    geom_point(aes(color=Ylab), size = 3) +
    geom_linerange(aes(ymin=CI1, ymax=CI2, color=Ylab),
                   alpha=0.5, size = 1) +
    geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
    facet_grid(~Ylab) +
    labs(y = ylabel, x =  df$Alab[1]) +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim=yrange) +
    scale_colour_manual(values=cols) + 
    ggtitle(title) +
    theme_ki() +
    theme(plot.title = element_text(hjust = 0),
          panel.spacing = unit(0, "lines"))
  }else{
    pd <- position_dodge(0.4)
    
    df <- d[d$hypothesis %in% hypo,]
    
    if(is.null(yrange)){
      yrange = c(min(df$CI1, na.rm=T)-0.05, max(df$CI2, na.rm=T)+0.05)
    }
    
    df$Time <- (str_split(df$Y, "_", simplify = TRUE)[,2])
    df$Time[df$Time=="t2"] <- "Year 1"
    df$Time[df$Time=="t3"] <- "Year 2"
    df$Time[grepl("Q1",df$level)] <- "Year 2"
    
    df$CI1[is.na(df$CI1)] <- 0
    df$CI2[is.na(df$CI2)] <- 0
    
    #add ref label
    df$ref <-"ref."
    df$ref[!grepl("Q1",df$level)] <- NA
    
    p <- ggplot(df, aes(x=level, y=ATE)) + 
      geom_pointrange(aes(ymin=CI1, ymax=CI2, color=anthro, group=Time, shape=Time),
                      position = position_dodge(width = 0.4),
                      size = 1) +
      geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
      facet_grid(~anthro) +
      labs(y = ylabel, x =  df$Alab[1]) +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim=yrange) +
      scale_shape_manual(values=c(16,21)) +
      scale_colour_manual(values=cols) + 
      ggtitle(title) +
      theme_ki() +
      theme(plot.title = element_text(hjust = 0),
            panel.spacing = unit(0, "lines"),
            legend.position = "right")+
      guides(color = FALSE)
    p
    
  }
  
  return(p)  
}


pH1 <- tmle_plot_fun(d, 1, title="", ylabel="Difference in\nchange in Z-score")
pH2 <- tmle_plot_fun(d, 2, yrange=c(-0.065, 0.03), title="", cols=tableau10[c(5:7)],  ylabel="Difference in\nvelocity in cm or kg")
pH3 <- tmle_plot_fun(d, 3, title="", ylabel="Z-score difference")
#pH4 <- tmle_plot_fun(d, 4, title="", ylabel="Z-score difference")
pH5 <- tmle_plot_fun(d, 5, title="", ylabel="Z-score difference")
#pH6 <- tmle_plot_fun(d, 6, title="", ylabel="Z-score difference")
pH7 <- tmle_plot_fun(d, 7, yrange=c(-0.03, 0.03), title="", cols=tableau10[c(5:7)], ylabel="Difference in\nvelocity in cm or kg")
pH8 <- tmle_plot_fun(d, 8, title="", ylabel="Difference in\nchange in Z-score")


# p1 <- plot_grid(pH1,
#                pH2,
#                pH3,
#                ncol=1,
#                labels = c("","",""),
#                rel_heights = c(1, 1, 1))
# p2 <- plot_grid(pH4,
#                pH6,
#                pH8,
#                pH7,
#                ncol=1,
#                labels = c("","","",""),
#                rel_heights = c(1, 1, 1, 1))

p2 <- tmle_plot_fun(d, hypo=c(4,6), title="", ylabel="Z-score difference")


ggsave(p2, file = here("figures/telo-growth-quartiles-differences_1.png"), height=3, width=14)
ggsave(pH5, file = here("figures/telo-growth-quartiles-differences_2.png"), height=3, width=14)
ggsave(pH2, file = here("figures/telo-growth-quartiles-differences_3.png"), height=3, width=14)

#Supplimentary figures

ggsave(pH1, file = here("figures/telo-growth-quartiles-differences_supp1.png"), height=3, width=14)


# Z-velocity
ggsave(pH8, file = here("figures/telo-growth-quartiles-differences_supp2.png"), height=3, width=14)
ggsave(pH3, file = here("figures/telo-growth-quartiles-differences_supp3.png"), height=3, width=14)

# Raw anthro velocity

ggsave(pH7, file = here("figures/telo-growth-quartiles-differences_supp4.png"), height=3, width=14)


#Head circumference plots
unique(dHCZ$Alab)
yrange = c(min(dHCZ$CI1, na.rm=T)-0.05, max(dHCZ$CI2, na.rm=T)+0.05)

#add ref label
dHCZ$ref <-"ref."
dHCZ$ref[!grepl("Q1",dHCZ$level)] <- NA

p1 <- ggplot(dHCZ[dHCZ$Alab=="Telomere length at Year 1 (T/S ratio)",], aes(x=level, y=ATE)) + 
  geom_point(aes(color=Ylab), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Ylab),
                 alpha=0.5, size = 1) +
  facet_wrap(~Ylab) +
  labs(y = "Adjusted difference", x = "Quartile of T/S at year 1") +
  geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  geom_hline(yintercept = 0) +
  #coord_cartesian(ylim=yrange) +
  scale_colour_manual(values=rep(tableau10[4],20), drop=FALSE) + 
  ylab("Adjusted difference") +
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))

p2 <- ggplot(dHCZ[dHCZ$Alab=="Change in telomere length between Years 1 and 2 (T/S ratio)",], aes(x=level, y=ATE)) + 
  geom_point(aes(color=Ylab), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Ylab),
                 alpha=0.5, size = 1) +
  facet_wrap(~Ylab) +
  labs(y = "Adjusted difference", x = "Quartile of change in T/S") +
  geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  geom_hline(yintercept = 0) +
  #coord_cartesian(ylim=yrange) +
  scale_colour_manual(values=rep(tableau10[4],20), drop=FALSE) + 
  ylab("Adjusted difference") +
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))

p3 <- ggplot(dHCZ[dHCZ$Alab=="Telomere length at Year 2 (T/S ratio)",], aes(x=level, y=ATE)) + 
  geom_point(aes(color=Ylab), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Ylab),
                 alpha=0.5, size = 1) +
  facet_wrap(~Ylab) +
  labs(y = "Adjusted difference", x = "Quartile of T/S at year 2") +
  geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  geom_hline(yintercept = 0) +
  #coord_cartesian(ylim=yrange) +
  scale_colour_manual(values=rep(tableau10[4],20), drop=FALSE) + 
  ylab("Adjusted difference") +
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))

pLower <- plot_grid(
  p2,
  p3,
  ncol=2,
  labels = c("",""),
  rel_widths = c(2, 1))

p <- plot_grid(
  p1,
  pLower,
  ncol=1,
  labels = c("",""),
  rel_heights = c(1, 1))

ggsave(p, file = here("figures/telo-growth-quartiles-differences_supp5.png"), height=6, width=14)

