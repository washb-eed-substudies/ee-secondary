
#Check pre-LM lactulose and mannitol concentrations from subset of children at 14 months

#pre-LM urinary sugar levels were evaluated in a subset of children (N=377) aged 14 months; mean concentrations of 
#mannitol and lactulose were 0.06 mmol/L and 0.002 mmol/L respectively.[AL1] 


#43% of children at age 3 months, 34% of children at age 14 months, and 23%[AL1]  of children at age 28 months 
#experienced at least one episode of urine loss

rm(list=ls())
library(xlsx)
library(tidyverse)

d <- read.xlsx("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-BD-EE-preLM-urine-midline.xlsx", sheetName="Sheet1")

head(d)


#geometric mean function
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x[!is.na(x)]))
}




class(d$lactulose)
d$lactulose <- as.character(d$lactulose)
d$lactulose[d$lactulose=="<0.3"] <- "0.3"
d$lactulose[d$lactulose=="<0.30"] <- "0.3"
d$lactulose <- as.numeric(d$lactulose)

d <- d %>% filter(!is.na(mannitol) | !is.na(lactulose)) %>% 
  mutate(mannitol=mannitol * (1/182.172), lactulose=lactulose * (1/342.296)) 

gm_mean(d$mannitol)
gm_mean(d$lactulose)
