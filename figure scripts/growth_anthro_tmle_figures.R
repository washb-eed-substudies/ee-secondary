

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

unique(d$Y)
unique(d$A)

d$Y <- factor(d$Y, levels=c("laz_t2", "whz_t2", "waz_t2", "hcz_t2",
                            "laz_t3", "whz_t3", "waz_t3", "hcz_t3",
                            "delta_laz_t2_t3", "delta_whz_t2_t3", "delta_waz_t2_t3", "delta_hcz_t2_t3",    
                            "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3"))

d <- d %>% arrange(Y) %>%
  mutate(
  Ylab = case_when(
    Y=="delta_laz_t2_t3" ~ "Change in LAZ",
    Y=="delta_whz_t2_t3" ~ "Change in WHZ",
    Y=="delta_waz_t2_t3" ~ "Change in WAZ",
    Y=="delta_hcz_t2_t3" ~ "Change in HCZ",
    Y=="len_velocity_t2_t3" ~ "Length velocity",
    Y=="wei_velocity_t2_t3" ~ "Weight velocity",
    Y=="hc_velocity_t2_t3" ~ "Head circumference velocity",
    Y=="laz_t2" ~ "LAZ at year 1",
    Y=="whz_t2" ~ "WLZ at year 1",
    Y=="waz_t2" ~ "WAZ at year 1",
    Y=="hcz_t2" ~ "HCZ at year 1",
    Y=="laz_t3" ~ "LAZ at year 2",
    Y=="whz_t3" ~ "WLZ at year 2",
    Y=="waz_t3" ~ "WAZ at year 2",
    Y=="hcz_t3" ~ "HCZ at year 2"),
  Ylab = factor(Ylab, levels=unique(Ylab)),
  Alab = case_when(
    A=="delta_TS" ~ "Change in T/S",
    A=="TS_t2" ~ "T/S at year 1",
    A=="TS_t3" ~ "T/S at year 2")) 
  



tmle_plot_fun <- function(d, hypo, title, yrange=c(-0.5, 0.5)){
  df <- d[d$hypothesis==hypo,]
  
  p <- ggplot(df, aes(x=level, y=ATE)) + 
    geom_point(aes(color=Ylab), size = 3) +
    geom_linerange(aes(ymin=CI1, ymax=CI2, color=Ylab),
                   alpha=0.5, size = 1) +
    facet_grid(~Ylab) +
    labs(y = "", x =  df$Alab[1]) +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim=yrange) +
    scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
    ggtitle(title) +
    theme_ki() +
    theme(plot.title = element_text(hjust = 0),
          panel.spacing = unit(0, "lines"))
  
  return(p)  
}


pH1 <- tmle_plot_fun(d, 1, "Hypothesis 1")
pH2 <- tmle_plot_fun(d, 2, "Hypothesis 2", c(-0.03, 0.03))
pH3 <- tmle_plot_fun(d, 3, "Hypothesis 3")
pH4 <- tmle_plot_fun(d, 4, "Hypothesis 4")
pH5 <- tmle_plot_fun(d, 5, "Hypothesis 5")
pH6 <- tmle_plot_fun(d, 6, "Hypothesis 6")
pH7 <- tmle_plot_fun(d, 7, "Hypothesis 7", c(-0.03, 0.03))
pH8 <- tmle_plot_fun(d, 8, "Hypothesis 8")



p <- plot_grid(pH1,
                  pH2,
                  pH3,
                  pH4,
                  pH5,
                  pH6,
                  pH7,
                  pH8,
                  ncol=1,
                  #labels = c(paste0("Hypothesis ", 1:8)),
                  rel_heights = c(1, 1, 1, 1, 1, 1, 1, 1)
                  )


ggsave(p, file = here("figures/telo-growth-quartiles-differences.png"), height=18, width=14)


# To do:
#Order plots, fix color levels by making Alab a factor



