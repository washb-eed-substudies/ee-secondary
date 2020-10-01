rm(list=ls())
source(here::here("0-config.R"))

# load telo-growth data
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
names(d)

# load in child development datasets
setwd("C:/Users/Sophia/Documents/WASH/WASHB CD data from Kishor/2-child-development-outcomes-datasets")

# create childid
get_childid <- function(v1, v2){
  paste(as.character(v1), as.character(v2), sep="")
}

cdi <- read.csv("washb-bangladesh-cdi-year2.csv") %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, endline_CDI_understand, endline_CDI_say)
easq <- read.csv("washb-bangladesh-easq-year2.csv") %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, endline_communication_score, 
         endline_gross_motor_score, endline_personal_social_score)
efanotb <- read.csv("washb-bangladesh-efanotb-year2.csv")%>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, endline_A_not_B_score)
eftower <- read.csv("washb-bangladesh-eftower-year2.csv")%>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, endline_tower_test)
# home1 <- read.dta("washb-bangladesh-home-year1.dta")%>%
#   mutate(tchild = replace("Target child (first)", tchild, "1")) %>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, midline_stimulation)
# home2 <- read.dta("washb-bangladesh-home-year2.dta")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_stimulation)
# cesd1 <- read.csv("washb-bangladesh-momdepression-year1.csv")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, midline_depression)
# cesd2 <- read.csv("washb-bangladesh-momdepression-year2.csv")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_depression)
motor <- read.csv("washb-bangladesh-motormile-year1.csv")%>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, sit_nosupp, crawl_nosupp, stand_supp, 
         walk_supp, stand_nosupp, walk_nosupp)

development <- motor %>% left_join(cdi, 'childid') %>% left_join(efanotb, 'childid') %>% 
  left_join(eftower, "childid") %>% left_join(easq, 'childid') %>%
  mutate(childid = as.integer(childid))
  
  # cdi %>% inner_join(easq, "childid") %>% inner_join(efanotb, "childid") %>%
  # inner_join(eftower, 'childid') %>% #inner_join(home1, 'childid') %>% inner_join(home2, 'childid') %>%
  # inner_join(cesd1, 'childid') %>% inner_join(cesd2, 'childid') %>% inner_join(motor, 'childid') %>%
  # mutate(childid = as.integer(childid))

telo_dev <- inner_join(d, development, "childid")

write.csv(telo_dev, file=paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-ee-telo-development-covariates.csv"))
