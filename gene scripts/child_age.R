library(tidyverse)
library(haven)
library(here)
library(lubridate)

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
  select("childid", 'hhid', 'clusterid', "aat2", "mpo2", "neo2", "stool_ml_date", "stool_el_date" ,'DOB')

stool$DOB <- ymd(stool$DOB)
stool$urine_ml_date <- ymd(stool$stool_ml_date)

urine <- urine %>% 
  select("childid", "hhid", 'clusterid',"Mann2", "Lact2", 'LM2', 'urine_ml_date', "urine_el_date", 'DOB')

urine$DOB <- ymd(urine$DOB)
urine$urine_ml_date <- ymd(urine$urine_ml_date)


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
  mutate(age_midline_m = case_when(childage_devmm >= 17 ~ 1,
                                 childage_devmm < 17 ~ 0)
  )

dev <- dev %>% 
  mutate(age_midline_d = case_when(childage_dev >= 22 ~ 1,
                                   childage_dev < 22 ~ 0)
  )

# % of children with development data collected after midline
mean(motor$age_midline_m)
mean(dev$age_midline_d, na.rm = TRUE)


#######################################################
# age at midline (eed)
#######################################################

urine$age_urine_ml <- interval(urine$DOB, urine$urine_ml_date) / months(1)
stool$age_stool_ml <- interval(stool$DOB, stool$stool_ml_date) / months(1)

urine$age_urine_el <- interval(urine$DOB, urine$urine_el_date) / months(1)
stool$age_stool_el <- interval(stool$DOB, stool$stool_el_date) / months(1)

summary(urine$age_urine_ml)
summary(stool$age_urine_ml)

#######################################################
# merge datasets
#######################################################

dim(stool)
dim(urine)
biomarkers <- full_join(stool, urine, by=c("childid","DOB"))
dim(biomarkers)

dim(motor)
dim(dev)
devel <- full_join(motor, dev, by=c("childid"))
dim(devel)

all_dta <- full_join(biomarkers, devel, by=c("childid"))


#######################################################
# numbers
#######################################################
devel %>% 
  filter(!is.na(age_midline_m)) %>% 
  summarize(N=n())

devel %>% 
  filter() %>% 
  summarise_all(N=n())


#######################################################
# comparison
#######################################################
all_dta %>% 
  filter(age_urine_ml < childage_devmm) %>% 
  summarize(N = n())
  

all_dta <- all_dta %>% 
  filter(!is.na(age_urine_ml), 
         !is.na(childage_devmm))

sum(all_dta$childage_devmm > all_dta$age_urine_ml)

summary(all_dta$childage_devmm - all_dta$age_urine_ml)
summary(all_dta$childage_devmm)
summary(all_dta$age_urine_ml)


all_dta$childage_dev <- all_dta$childage_dev/30.4167
summary(all_dta$age_urine_el)
summary(all_dta$childage_dev)


summary(all_dta$childage_dev - all_dta$age_urine_el)
