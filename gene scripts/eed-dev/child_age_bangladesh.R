library(tidyverse)
library(here)

rm(list = ls())

set_here(path = '..')

motor_1 <- read.csv('../2-child-development-outcomes-datasets 2/washb-bangladesh-motormile-year1.csv')
easq_2 <- read.csv('../2-child-development-outcomes-datasets 2/washb-bangladesh-easq-year2.csv')

urine <- read.csv('../data/BD-EE-urine.csv')
stool <- read.csv('../data/BD-EE-stool.csv')


# ---

motor_1 <- motor_1 %>% 
  select(dataid, childid : ageyears) %>% 
  mutate(motor_agemo = ageyears * 12,
         dataid = as.numeric(dataid), 
         tchild = as.numeric(tchild),
         childid = as.numeric(paste0(dataid,tchild))) 

motor_1 %>% 
  summary()
# mean = 11.45 mo


easq_2 <- easq_2 %>% 
  select(dataid, childid : ageyears) %>% 
  mutate(easq_agemo = ageyears * 12,
         dataid = as.numeric(dataid), 
         tchild = as.numeric(tchild),
         childid = as.numeric(paste0(dataid,tchild))) 

dim(motor_1)
dim(easq_2)

easq_2 %>% 
  summary()
# mean = 25.59 mo

# ---

urine <- urine %>% 
  select('childNo', 'agem2', "agem3", "month3", "DOB", 
         "sex", "clusterid", "birthord", "childid") %>% 
  rename(urine_agemo_ml = 'agem2',
         urine_agemo_el = 'agem3')

stool <- stool %>% 
  select('childNo', 'agem2', "agem3", "month3", "DOB", 
         "sex", "clusterid", "birthord", "childid") %>% 
  rename(stool_agemo_ml = 'agem2',
         stool_agemo_el = 'agem3')

# ---
# YEAR 1
mean(urine$urine_agemo_ml, na.rm = TRUE)
mean(stool$stool_agemo_ml, na.rm = TRUE)
mean(motor_1$motor_agemo)


# YEAR 2
mean(urine$urine_agemo_el, na.rm = TRUE)
mean(stool$stool_agemo_el, na.rm = TRUE)
mean(easq_2$easq_agemo)





# ---

midline <- urine %>% 
  select(c('childid', 'clusterid', 'urine_agemo_ml')) %>% 
  left_join(stool %>% 
              select('childid', 'clusterid', 'stool_agemo_ml'),
            by = c('childid', 'clusterid')) %>% 
  left_join(motor_1 %>% 
              select('childid', 'clusterid', 'motor_agemo'),
            by = c('childid', 'clusterid')) 


endline <- urine %>% 
  select(c('childid', 'clusterid', 'urine_agemo_el')) %>% 
  left_join(stool %>% 
              select('childid', 'clusterid', 'stool_agemo_el'),
            by = c('childid', 'clusterid')) %>% 
  left_join(easq_2 %>% 
              select('childid', 'clusterid', 'easq_agemo'),
            by = c('childid', 'clusterid'))

midline <- midline %>% 
  mutate(eed_dev = ifelse(urine_agemo_ml < motor_agemo, 1, 0))

endline <- endline %>% 
  mutate(eed_dev = ifelse(urine_agemo_el < easq_agemo, 1, 0))


mean(midline$eed_dev, na.rm = TRUE)
mean(endline$eed_dev, na.rm = TRUE)









