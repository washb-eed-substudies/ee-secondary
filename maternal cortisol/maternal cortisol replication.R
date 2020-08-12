## maternal cortisol replication ##

library(tidyverse)

rm(list=ls())
source(here::here("0-config.R"))

gene <- read.csv(here('maternal cortisol/Gene/maternal-cortisol.csv')) %>% 
  select(-X) %>% 
  rename(lab.tech = lab_tech) %>% 
  drop_na('sample')

soph <- read.csv(here('maternal cortisol/Sophia scripts/cleaned_maternal_cortisol_ST.csv')) %>% 
  select(-X)

`%notin%` <- Negate(`%in%`)
colnames(gene)[colnames(gene) %notin% colnames(soph)]
colnames(soph)[colnames(soph) %notin% colnames(gene)]


gene %>% 
  filter(run == 3) %>% 
  View()

gene$sample[gene$sample %notin% soph$sample]
soph$sample[soph$sample %notin% gene$sample]

