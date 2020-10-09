library(tidyverse)
library(haven)
library(here)

# source(here::here('0-config.R'))

#######################################################
# data import
#######################################################

motor <- read_dta(here('/gene scripts/eed-dev/washk_motormile_CA_20171121.dta'))
dev <- read_dta(here('/gene scripts/eed-dev/washk_development_allkids_CA_20171121.dta'))
stool <- read_csv(here('/gene scripts/eed-dev/washb-kenya-eed-stool.csv')) %>%  select(-'X1')
urine <- read_csv(here('/gene scripts/eed-dev/washb-kenya-eed-urine.csv')) %>%  select(-'X1')

# colnames(motor)
#   ## childage_devmm
# colnames(dev)
#   ## childage_dev

# select columns from dev data
motor <- motor %>% 
  select("hhid", "childid", "clusterid", "childage_devmm", "vlgid", 'compoundid')

dev <- dev %>% 
  select("hhid", "childid", "clusterid", "childage_dev", "vlgid", 'compoundid')


# select columns from eed data
stool <- stool %>% 
  select("childid", 'clusterid', "aat2", "mpo2", "neo2", "stool_ml_date")

urine <- urine %>% 
  select("childid", "hhid", "Mann2", "Lact2", 'LM2', 'urine_ml_date')


#######################################################
# 1st summary stats???
#######################################################

summary(motor$childage_devmm)[3:4]
summary(dev$childage_dev / 365 * 12)[3:4] # in days

# convert days to months
dev <- dev %>% 
  mutate(childage_mo = childage_dev / 365 * 12)

# indicator variable of whether child was older than midline
motor <- motor %>% 
  mutate(age_midline = case_when(childage_devmm >= 17 ~ 1,
                                 childage_devmm < 17 ~ 0)
  )

dev <- dev %>% 
  mutate(age_midline = case_when(childage_mo >= 22 ~ 1,
                                 childage_mo < 22 ~ 0)
  )

# % of children with development data collected after midline
mean(motor$age_midline)
mean(dev$age_midline, na.rm = TRUE)

#######################################################
# merge datasets
#######################################################
