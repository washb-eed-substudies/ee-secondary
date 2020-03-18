rm(list=ls())
source(here::here("0-config.R"))

d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv"))

t2 <- sum(d$cesd_sum_t2 >= 16, na.rm=TRUE)/length(d$cesd_sum_t2)
t3 <- sum(d$cesd_sum_ee_t3 >= 16, na.rm=TRUE)/length(d$cesd_sum_ee_t3)
