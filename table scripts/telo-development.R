rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

#Telo and Child Development Tables
r <- readRDS(here("telo-development/results/H1_res.RDS"))
#d <- load(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-telo.dta"))

#Table 2: Relationship between change in telomere length between Year 1 and Year 2 and child development at Year 2

#Create vectors

outcomes2 <- c("Communication Score", "Gross Motor Score", "Personal Social Score", "A not B Score", "Tower Test Score")

outcomedomains2 <- c("Extended Ages and Stages Questionnaire", "", "", "Executive Function Evalulation", "")

Q1.2 <- as.character(round(r$q1, 2))

Q3.2 <- as.character(round(r$q3, 2))

pointdiff2 <- as.character(round(r$point.diff, 2))

lb2 <- as.character(round(r$lb.diff, 2))

ub2 <- as.character(round(r$ub.diff, 2))

pval2 <- as.character(round(r$Pval, 2))


#Create table

tbl2 <- data.table(
  "Outcome Domain" = outcomedomains2,
  "Outcome" = outcomes2,
  #"N" = n_t2,
  #"Mean" = mean_tr,
  #"Standard Deviation" = sd_t2,
  "25th Percentile" = Q1.2,
  "75th Percentile" = Q3.2,
  "Difference Point Estimate" = pointdiff2, 
  "Upper Bound" = ub2,
    "Lower Bound" = lb2,
  "P-value" = pval2
  #"Fully Adjusted Analysis" = full_adj
)
tbl2



