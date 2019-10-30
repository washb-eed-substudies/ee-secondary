rm(list=ls())
source(here::here("0-config.R"))



#load stress outcomes
str <- readRDS("~/ee-secondary/replication objects/simulated_stress_outcomes.rds")
str <- as.data.frame(str)
str$childid <- as.numeric(str$childid)
head(str)

#log transform outcomes
table(is.na(str[,-1]))
str[,-1] <- log(str[,-1])
table(is.na(str[,-1]))

fulld <- read.csv(paste0(dropboxDir,"Data/Cleaned/Andrew/EE-BD_fulldata.csv"))
colnames(fulld)
