## Initial maternal Cortisol cleaning ##

rm(list=ls())
source(here::here("0-config.R"))

setwd('C:/Users/Sophia/Box/WASHB - Data Cleaning/Maternal Blood Cortisol/Untouched')

## Read in excel files ##
library('readxl')
p1 <- read_excel('Pilot_RUN 1_Blood Cortisol_Zia_Sunny_Lot#I6C12b_01JUNE2017.xls', range = "B38:K47")
p2 <- read_excel('Pilot_RUN 2_Blood Cortisol_Zia_Sunny_Lot#I6C12b_01JUNE2017.xls', range = "B38:K47")
p3 <- read_excel('Pilot_RUN 3_RABIUL_WASHB_EE_Blood Cortisol_LOT#I6C12b_170604_134940.xls', range = "B38:K41")
p4 <- read_excel('Pilot_RUN 4_RABIUL_WASHB_EE_Blood Cortisol_LOT#I6C12b_170604_134940.xls', range = "B38:K41")
r1 <- read_excel('RUN 1_SUNNY_WASHB_EE_Blood Cortisol_LOT#I6C12b_170724_134940.xls', range = "B38:K118")
r2 <- read_excel('run 2_sunny_washb_ee_blood cortisol_lot#i6c12b_170803_134940.xls', range = "B38:K118")
r3 <- read_excel('RUN 3_SUNNY_WASHB_EE_Blood Cortisol_LOT#16C12b_170807_134940.xls', range = "B38:K118")
r4 <- read_excel('RUN 4_ZIA_WASHB_EE_Blood Cortisol_LOT#16C12b_170808_161942.xls', range = "B38:K118")
r5 <- read_excel('RUN 5_SUNNY_WASHB_EE_Blood Cortisol_170809_172517.xls', range = "B38:K118")
r6 <- read_excel('RUN 6_SUNNY_WASHB_EE_Blood Cortisol_LOT#16C12b_170810_154302.xls', range = "B36:K116")
r7 <- read_excel('RUN 7_ZIA_WASHB_EE_Blood Cortisol_171017_172753.xls', range = "B38:K118")
r8 <- read_excel('RUN 8_ZIA_WASHB_EE_Blood Cortisol_171018_163445.xls', range = "B38:K118")
r9 <- read_excel('RUN 9_ZIA_WASHB_EE_Blood Cortisol_LOT#17C144b_171211_154302_AL (2).xls', range = "B38:K118")
 

## Clean excel files and fill in repeated values ##
cleaning <- function(d, date, run, tech=NULL){
  d <- subset(data.frame(d), select = 1:7)
  names(d) <- c('sample', 'dilution', 'wells', 'raw', 'percent', 'conc', 'conc_avg')
  d$assay <- "blood_cortisol"
  d$date <- date
  d$run <- run
  if (!is.null(tech)) {
    d$`lab tech` <- tech  
  } 
  if (any(is.na(d$sample))) {
    header <- d[1,]
    for (i in 1:nrow(d)){
      row <- d[i,]
      if (is.na(row$'sample')){
        for (j in c('sample', 'dilution', 'percent', 'conc_avg')){
          d[i, j] <- header[,j]
        }
        #d$`lab tech` <- 'f'
      } else {
        header <- d[i,]
      }
    }
  }
  d
}

cleaned_p1 <- cleaning(p1, '170601', 'p1')
cleaned_p2 <- cleaning(p2, '170601', 'p2')
cleaned_p3 <- cleaning(p3, '170604', 'p3', 'Rabiul')
cleaned_p4 <- cleaning(p4, '170604', 'p4', 'Rabiul')

cleaned_r1 <- cleaning(r1, '170724', 1, 'Sunny')
cleaned_r2 <- cleaning(r2, '170803', 2, 'Sunny')
cleaned_r3 <- cleaning(r3, '170807', 3, 'Sunny')
cleaned_r4 <- cleaning(r4, '170808', 4, 'Zia')
cleaned_r5 <- cleaning(r5, '170809', 5, 'Sunny')
cleaned_r6 <- cleaning(r6, '170810', 6, 'Sunny')
cleaned_r7 <- cleaning(r7, '171017', 7, 'Zia')
cleaned_r8 <- cleaning(r8, '171018', 8, 'Zia')
cleaned_r9 <- cleaning(r9, '171211', 9, 'Zia')

## extract lab tech ##
p1_p2 <- rbind(cleaned_p1, cleaned_p2)
name <- str_extract(str_extract(p1_p2$sample, "\\([[:alpha:]]+\\)"), "[[:alpha:]]+")
p1_p2$sample <- str_replace(p1_p2$sample, "\\s\\(.+\\)", '')
p1_p2$`lab tech` <- name

## Merge data, rename, add assay column ##
d <- rbindlist(list(p1_p2, cleaned_r1, cleaned_r2, cleaned_r3, cleaned_r4, cleaned_r5, cleaned_r6, cleaned_r7, cleaned_r8, cleaned_r9))
d <- select(d, c('date', 'assay', 'run', 'sample', 'dilution', 'wells', 'raw', 'percent', 'conc', 'conc_avg', 'lab tech'))

write.csv(d, here('maternal cortisol/Sophia scripts/cleaned_maternal_cortisol_ST.csv'))
