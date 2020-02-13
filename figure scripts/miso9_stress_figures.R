rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


#Load tmle results
load(here("andrew results/stress_results.Rdata"))

#### FIGURE MEANS ####



#### FIGURE UNADJUSTED DIFFERENCES ####