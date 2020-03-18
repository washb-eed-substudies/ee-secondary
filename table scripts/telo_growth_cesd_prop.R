rm(list=ls())
source(here::here("0-config.R"))

d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv"))

# calculating proportion of mothers that are above the cutoff for clinical depression based on CESD scores
t2 <- sum(d$cesd_sum_t2 >= 16, na.rm=TRUE)/length(d$cesd_sum_t2)
t3 <- sum(d$cesd_sum_ee_t3 >= 16, na.rm=TRUE)/length(d$cesd_sum_ee_t3)

tbl <- data.frame("Year 1 CESD proportion" = c(t2),
           "Year 2 CESD proportion" = c(t3))

write.csv(tbl, file = here("tables/telo_growth/telo_growth_cesdcutoff_prop.csv"))

