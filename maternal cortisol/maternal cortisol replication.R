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

sum((gene %>% group_by(run) %>% summarise(count = n()))$count != (soph %>% group_by(run) %>% summarise(count = n()))$count)

gene$conc <- as.numeric(gene$conc)
gene$conc_avg <- as.numeric(gene$conc_avg)
soph$conc <- as.numeric(soph$conc)
soph$conc_avg <- as.numeric(soph$conc_avg)
grouped_g <- gene %>% group_by(run) %>% summarise_at(c("raw", "percent", "conc", "conc_avg"), mean, na.rm = TRUE)
grouped_s <- soph %>% group_by(run) %>% summarise_at(c("raw", "percent", "conc", "conc_avg"), mean, na.rm = TRUE)

grouped_g == grouped_s

sum(soph%>%group_by(sample) %>% summarize(count=n()) != gene%>%group_by(sample) %>% summarize(count=n()))
sum(soph%>%group_by(date) %>% summarize(count=n()) != gene%>%group_by(date) %>% summarize(count=n()))


