

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

d$Y <- as.character(d$Y)
d$Y <- gsub("whz", "wlz", d$Y)
d$Y <- factor(d$Y, levels=c("laz_t2", "wlz_t2", "waz_t2", "hcz_t2",
                            "laz_t3", "wlz_t3", "waz_t3", "hcz_t3",
                            "delta_laz_t2_t3", "delta_wlz_t2_t3", "delta_waz_t2_t3", "delta_hcz_t2_t3",    
                            "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3"))

d <- d %>% arrange(Y) %>%
  mutate(
  Ylab = case_when(
    Y=="delta_laz_t2_t3" ~ "Change in LAZ",
    Y=="delta_wlz_t2_t3" ~ "Change in WLZ",
    Y=="delta_waz_t2_t3" ~ "Change in WAZ",
    Y=="delta_hcz_t2_t3" ~ "Change in HCZ",
    Y=="len_velocity_t2_t3" ~ "Length velocity",
    Y=="wei_velocity_t2_t3" ~ "Weight velocity",
    Y=="hc_velocity_t2_t3" ~ "Head circumference velocity",
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


# Grouping hypotheses 1,2,3 into 1 figure labeled 
#"Adjusted differences between quartiles of change in telomere length between Years 1 and 2 for each growth outcome."
# 
# Grouping hypotheses 4,6,7,8 into 1 figure labeled 
#"Adjusted differences between quartiles of telomere length at Year 1 for each growth outcome"
# 
# Grouping hypothesis 5 into its own figure labeled 
#"Adjusted differences between quartiles of telomere length at Year 2 for each growth outcome"
# 
# That way, we could get rid of the hypotheses labels entirely and replace each with a figure heading instead. 
#I like that each growth outcome is a different color. Also, need to change WLZ to WLZ to make it consistent across the figure.
# 



pH1 <- tmle_plot_fun(d, 1, title="Adjusted differences between quartiles of change in telomere length between Years 1 and 2 for each growth outcome")
pH2 <- tmle_plot_fun(d, 2, c(-0.03, 0.03), title="")
pH3 <- tmle_plot_fun(d, 3, title="")
pH4 <- tmle_plot_fun(d, 4, title="Adjusted differences between quartiles of telomere length at Year 1 for each growth outcome")
pH5 <- tmle_plot_fun(d, 5, title="Adjusted differences between quartiles of telomere length at Year 2 for each growth outcome")
pH6 <- tmle_plot_fun(d, 6, title="")
pH7 <- tmle_plot_fun(d, 7, c(-0.03, 0.03), title="")
pH8 <- tmle_plot_fun(d, 8, title="")







p1 <- plot_grid(pH1,
               pH2,
               pH3,
               ncol=1,
               labels = c("","",""),
               rel_heights = c(1, 1, 1))
p2 <- plot_grid(pH4,
               pH6,
               pH7,
               pH8,
               ncol=1,
               labels = c("","","",""),
               rel_heights = c(1, 1, 1, 1))



ggsave(p1, file = here("figures/telo-growth-quartiles-differences_1.png"), height=9, width=14)
ggsave(p2, file = here("figures/telo-growth-quartiles-differences_2.png"), height=12, width=14)
ggsave(pH5, file = here("figures/telo-growth-quartiles-differences_3.png"), height=3, width=14)




