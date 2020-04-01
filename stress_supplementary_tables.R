rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

load(here("andrew results/stress_results.RData"))


bonpval <- function(pval){
  bon = round(pval * 2, 2)
  if (pval >= .5)
    bon = 1
  bon 
}

#to be used for formatting ipcw variables for table
ci_interval<-function(str, tbl){
  filter<-tbl[tbl$Y == str,]
  paste(round(filter[1], 2), " (", round(filter[3], 2), ", ", round(filter[4], 2), ")", sep="")
}

#mean
mean <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[3], 2))
}
#sd
sd <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[4], 2))
}

#n
n <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[5], 2))
}


outcomes4<-c("iPF(2α)-III ", "Control", "Nutrition + WSH", "2,3-dinor-iPF(2α)-III", 
             "Control", "Nutrition + WSH", "iPF(2α)-VI", "Control", "Nutrition + WSH", "8,12-iso-iPF(2α)-VI", 
             "Control","Nutrition + WSH")

tbls4 <- data.table(
  "Outcome" = outcomes4,
  "N" = 0,
  "Absolute Mean" = 0,
  "Mean" =0,
  "Standard Deviation" =0,
  "Unadjusted Analysis" =0, 
  "Age and Sex Adjusted Analysis" =0,
  "Fully Adjusted Analysis" =0
)

outcomes6<-c("Pre-stressor Salivary alpha-amylase" ,"Control", "Nutrition + WSH",
             "Post-stressor Salivary alpha-amylase","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor alpha-amylase","Control", "Nutrition + WSH",
             "Residualized gain score for alpha-amylase","Control", "Nutrition + WSH",
             "Pre-stressor salivary cortisol","Control", "Nutrition + WSH",
             "Post-stressor salivary cortisol","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor cortisol","Control", "Nutrition + WSH",
             "Residualized gain score for cortisol","Control", "Nutrition + WSH",
             "Mean arterial Pressure","Control", "Nutrition + WSH",
             "Resting heart rate","Control", "Nutrition + WSH",
             "NR3C1 exon 1F promoter methylation","Control", "Nutrition + WSH",
             "NGFI-A transcription factor binding site","Control", "Nutrition + WSH"
)

tbls6 <- data.table(
  "Outcome" = outcomes6,
  "N" = 0,
  "Absolute Mean" = 0,
  "Mean" = 0,
  "Standard Deviation" = 0,
  "Unadjusted Analysis" = 0, 
  "Age and Sex Adjusted Analysis" = 0,
  "Full Adjusted Analysis" = 0
)







