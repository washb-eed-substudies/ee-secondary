rm(list=ls())
source(here::here("0-config.R"))

#load maternal exposure data
#load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))

# load telo-growth data (also includes maternal exposure)
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
names(d)

# create childid
get_childid <- function(v1, v2){
  paste(as.character(v1), as.character(v2), sep="")
}

#subset data

ipv <- d %>%
  select(phys_viol_12m_t3, emot_viol_12m_t3, sex_viol_12m_t3, viol_12m_any_t3, life_phys_viol_t3, life_emot_viol_t3, life_sex_viol_t3, life_viol_any_t3)

stress <- d %>%
  select(pss_sum_mom_t3)

dep <-d %>%
  select(cesd_sum_t2, cesd_sum_ee_t3)

telo <- d%>%
  select(delta_TS, TS_t2, TS_t3)


# cdi <- read.csv("washb-bangladesh-cdi-year2.csv") %>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_CDI_understand, endline_CDI_say)
# easq <- read.csv("washb-bangladesh-easq-year2.csv") %>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_communication_score, 
#          endline_gross_motor_score, endline_personal_social_score, combined) %>%
#   rename(combined_easq = combined)
# efanotb <- read.csv("washb-bangladesh-efanotb-year2.csv")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_A_not_B_score)
# eftower <- read.csv("washb-bangladesh-eftower-year2.csv")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, endline_tower_test)
# home1 <- read.dta("washb-bangladesh-home-year1.dta")%>%
#   mutate(childid = substr(childid, 2, 2)) %>%
#   mutate(childid = get_childid(dataid, childid)) %>%
#   select(childid, midline_stimulation)
# home2 <- read.dta("washb-bangladesh-home-year2.dta")%>%
#   mutate(childid = substr(childid, 2, 2)) %>%
#   mutate(childid = get_childid(dataid, childid)) %>%
#   select(childid, endline_stimulation)
# motor <- read.csv("washb-bangladesh-motormile-year1.csv")%>%
#   mutate(childid = get_childid(dataid, tchild)) %>% 
#   select(childid, sit_nosupp, crawl_nosupp, stand_supp, 
#          walk_supp, stand_nosupp, walk_nosupp)


# development <- motor %>% full_join(cdi, 'childid') %>% full_join(efanotb, 'childid') %>% 
#   full_join(eftower, "childid") %>% full_join(easq, 'childid') %>% left_join(home1, 'childid') %>%
#   left_join(home2, 'childid') %>%
#   mutate(childid = as.integer(childid))
  

#select all exposures and outcomes
# ipv.dep.stress.telo <- d %>%
#   select(phys_viol_12m_t3, emot_viol_12m_t3, sex_viol_12m_t3, viol_12m_any_t3, life_phys_viol_t3, life_emot_viol_t3, life_sex_viol_t3, life_viol_any_t3, pss_sum_mom_t3, cesd_sum_t2, cesd_sum_ee_t3, delta_TS, TS_t2, TS_t3)

#Z-scores of telomere measurements
ipv.dep.stress.telo <- d %>% 
   mutate(TS_t2_Z = scale(TS_t2, center=TRUE, scale=TRUE)[,1]) %>%
   mutate(TS_t3_Z = scale(TS_t3, center=TRUE, scale=TRUE)[,1]) %>%
   mutate(delta_TS_Z = scale(delta_TS, center=TRUE, scale=TRUE)[,1])
#  mutate(endline_CDI_understand_Z = scale(endline_CDI_understand, center=T, scale=T)[,1]) %>%
#   mutate(endline_CDI_say_Z = scale(endline_CDI_say, center=T, scale=T)[,1]) %>%
#   mutate(endline_communication_score_Z = scale(endline_communication_score, center=T, scale=T)[,1]) %>%
#   mutate(endline_gross_motor_score_Z = scale(endline_gross_motor_score, center=T, scale=T)[,1]) %>%
#   mutate(endline_personal_social_score_Z = scale(endline_personal_social_score, center=T, scale=T)[,1]) %>%
#   mutate(combined_easq_Z = scale(combined_easq, center=T, scale=T)[,1]) %>%
#   mutate(endline_A_not_B_score_Z = scale(endline_A_not_B_score, center=T, scale=T)[,1]) %>%
#   mutate(endline_tower_test_Z = scale(endline_tower_test, center=T, scale=T)[,1]) %>%
#   mutate(endline_CDI_say_Z = scale(endline_CDI_say, center=T, scale=T)[,1])

ipv.dep.stress.telo$viol_12m_any_t3_recode <- ifelse(is.na(ipv.dep.stress.telo$viol_12m_any_t3) & 
                                                       ipv.dep.stress.telo$life_viol_any_t3 == 0, 
                                                     0, ipv.dep.stress.telo$viol_12m_any_t3)

saveRDS(ipv.dep.stress.telo, paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-dep-stress-telo-covariates.RDS"))
