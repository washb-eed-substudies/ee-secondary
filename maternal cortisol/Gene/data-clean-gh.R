# Maternal-Infant Stress
# Replication
# Gene Ho


require(openxlsx) # to deal with merged cells
require(tidyverse)

source(here::here("0-config.R"))

# removed plots and calibration data by hand
#

# pilot run 2
# run2_pilot <-
#   read.xlsx(
#     "~/OneDrive/Berkeley/Course Materials/WASH-B/maternal-cortisol/Pilot_RUN 2_Blood Cortisol_Zia_Sunny_Lot^NI6C12b_01JUNE2017.xlsx",
#     fillMergedCells = TRUE
#   ) %>%
#   select(2:11)
# 
# run2_pilot <- cbind(rep("P2", length(run2_pilot)), run2_pilot)
# 
# names(run2_pilot)[ncol(run2_pilot)] <- "Run"


run_import <- function(filename, run_num, lab_tech, date) {
  
  data <- read.xlsx(here(filename),
                    fillMergedCells = TRUE) %>%
    select(2:11) 
  
  data <- cbind(rep(date, nrow(data)), rep("blood_cortisol", nrow(data)), data, rep(run_num, nrow(data)), rep(lab_tech, nrow(data)))
  
  names(data)[ncol(data)] <- "lab_tech"
  names(data)[ncol(data)-1] <- "run"
  names(data)[1] <- "date"
  names(data)[2] <- "assay"
  
  names(data) <- tolower(names(data))
  
  data <- data %>% 
    rename(
      'percent' = '%',
      'conc' = 'conc.', 
      'conc_avg' = 'conc..(average)'
    )
  
  return(data)
}


# to extract lab tech name from pilot sets
# pilot_run1$lab_tech <- gsub("\\w+ \\((\\w+)\\)+", "\\1", pilot_run1$sample)
# pilot_run1$sample <- gsub(" \\((\\w+)\\)+", "", pilot_run1$sample)


lab_tech <- function(data){
  data$lab_tech <- tolower(gsub("\\w+ \\((\\w+)\\)+", "\\1", data$sample))
  data$sample <- gsub(" \\((\\w+)\\)+", "", data$sample)
  
  return(data)
}


pilot_run1 <- run_import("/maternal cortisol/Gene/simple-data/Pilot_RUN 2_Blood Cortisol_Zia_Sunny_Lot^NI6C12b_01JUNE2017.xlsx", 
                         "P1", NA, 170601)

pilot_run1 <- lab_tech(pilot_run1)

pilot_run2 <- run_import("maternal cortisol/Gene/simple-data/Pilot_RUN 2_Blood Cortisol_Zia_Sunny_Lot^NI6C12b_01JUNE2017.xlsx",
           "P2", NA, 170601)

pilot_run2 <- lab_tech(pilot_run2)

pilot_run3 <- run_import("maternal cortisol/Gene/simple-data/Pilot_RUN 3_RABIUL_WASHB_EE_Blood Cortisol_LOT^NI6C12b_170604_134940.xlsx", 
                         "P3", 'rabuil', 170604) %>% 
  drop_na(wells)

pilot_run4 <- run_import("maternal cortisol/Gene/simple-data/Pilot_RUN 4_RABIUL_WASHB_EE_Blood Cortisol_LOT^NI6C12b_170604_134940.xlsx", 
                         "P4", 'rabuil', 170604) %>% 
  drop_na(wells)


run1 <- run_import("maternal cortisol/Gene/simple-data/RUN 1_SUNNY_WASHB_EE_Blood Cortisol_LOT^NI6C12b_170724_134940.xlsx", 
                   1, "sunny", 170724)

run2 <- run_import("maternal cortisol/Gene/simple-data/run 2_sunny_washb_ee_blood cortisol_lot^Ni6c12b_170803_134940.xlsx", 
                   2, 'sunny', 170803)

run3 <- run_import("maternal cortisol/Gene/simple-data/RUN 3_SUNNY_WASHB_EE_Blood Cortisol_LOT^N16C12b_170807_134940.xlsx", 
                   3, 'sunny', 170807)

run4 <- run_import("maternal cortisol/Gene/simple-data/RUN 4_ZIA_WASHB_EE_Blood Cortisol_LOT^N16C12b_170808_161942.xlsx", 
                   4, 'zia', 170808)

run5 <- run_import("maternal cortisol/Gene/simple-data/RUN 5_SUNNY_WASHB_EE_Blood Cortisol_170809_172517.xlsx", 
                   5, 'sunny', 170809)

run6 <- run_import("maternal cortisol/Gene/simple-data/RUN 6_SUNNY_WASHB_EE_Blood Cortisol_LOT^N16C12b_170810_154302.xlsx", 
                   6, 'sunny', 170810)

run7 <- run_import("maternal cortisol/Gene/simple-data/RUN 7_ZIA_WASHB_EE_Blood Cortisol_171017_172753.xlsx", 
                   7, 'zia', 171017)

run8 <- run_import("maternal cortisol/Gene/simple-data/RUN 8_ZIA_WASHB_EE_Blood Cortisol_171018_163445.xlsx", 
                   8, 'zia', 1710018)

run9 <- run_import("maternal cortisol/Gene/simple-data/RUN 9_ZIA_WASHB_EE_Blood Cortisol_LOT^N17C144b_171211_154302_AL (2).xlsx", 
                   9, 'zia', 171211)


full_df <- rbind(pilot_run1, pilot_run2, pilot_run3, pilot_run4,
                 run1, run2, run3, run4, run5, run6, run7, run8, run9) %>% 
  filter(!sample %in%  c("B0", "NSB")) %>% 
  select(-c('%cv', 'sd', 'sem'))


# add lab tech 


write.csv(full_df, here('maternal cortisol/Gene/maternal-cortisol.csv'))

