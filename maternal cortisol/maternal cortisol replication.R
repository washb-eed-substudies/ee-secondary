## maternal cortisol replication ##

rm(list=ls())
source(here::here("0-config.R"))

gene <- read.csv(here('maternal cortisol/Gene/maternal-cortisol.csv'))
soph <- read.csv(here('maternal cortisol/Sophia scripts/cleaned_maternal_cortisol_ST.csv'))
