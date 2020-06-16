rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))
data_y1 <- read.csv(here("tables/stress/stress_table1.csv"))
load(here("andrew results/stress_results.RData"))
load("~/ee-secondary/andrew results/andrew_stress_ipcw.rdata")

#### Table S1 ####
# filtering for children with any t2 measurements
filtering <- function(row){
  any(!is.na(row))
}

y1<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]

# filtering children with any t2 measurements for those lost to follow up at t3
lost<-y1 %>% filter_at(vars(t3_map,t3_hr_mean,t3_saa_z01,t3_saa_z02,t3_cort_z01,t3_cort_z03,
                            t3_gcr_mean,t3_gcr_cpg12,t3_saa_slope,t3_cort_slope,t3_residual_saa,t3_residual_cort), all_vars(is.na(.)))

#calculating overall N by arm
Nlostctrl<-nrow(lost[lost$tr=="Control",])
Nlostwsh<-nrow(lost[lost$tr=="Nutrition + WSH",])

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(tbl, variable) {
  ctrlmean<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE))
  ctrlsd<-round(sd(variable[tbl$tr=="Control"], na.rm=TRUE))
  wshmean<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE))
  wshsd<-round(sd(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE))
  c(ctrlmean, ctrlsd, wshmean, wshsd)
}

npercfunc <- function(tbl, variable) {
  ctrln<-sum(variable[tbl$tr=="Control"], na.rm=TRUE)
  ctrlperc<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE)*100)
  wshn<-sum(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)
  wshperc<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)*100)
  c(ctrln, ctrlperc, wshn, wshperc)
}

momage<-meansdfunc(lost, lost$momage)
momeduy<-meansdfunc(lost, lost$momeduy)
dadeduy<-meansdfunc(lost, lost$dadeduy)
dadagri<-npercfunc(lost, lost$dadagri)
Nhh<-meansdfunc(lost, lost$Nhh)
elec<-npercfunc(lost, lost$elec)
cement<-npercfunc(lost, lost$cement)

acresmctrl<-round(mean(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
acressdctrl<-round(sd(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
acresmwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acressdwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acres<-c(acresmctrl, acressdctrl, acresmwsh, acressdwsh)

tubewell<-npercfunc(lost, lost$tubewell)
storewater<-npercfunc(lost, lost$storewat)
treatwater<-npercfunc(lost, lost$treatwat)
waterdis<-meansdfunc(lost, lost$watmin)
odmen<-npercfunc(lost, lost$odmen)
odwomen<-npercfunc(lost, lost$odwom)
odchild815<-npercfunc(lost, lost$odch815)
odchild38<-npercfunc(lost, lost$odch38)
odchild03<-npercfunc(lost, lost$odchu3)
latowned<-npercfunc(lost, lost$latown)
latslab<-npercfunc(lost, lost$latslab)
latseal<-npercfunc(lost, lost$latseal)

latfctrln<-sum(lost$latfeces[lost$tr=="Control"]==0, na.rm=T)
latfctrlperc<-round(latfctrln/sum(!is.na(lost$latfeces[lost$tr=="Control"]), na.rm=T)*100)
latfwshn<-sum(lost$latfeces[lost$tr=="Nutrition + WSH"]==0, na.rm=T)
latfwshperc<-round(latfwshn/sum(!is.na(lost$latfeces[lost$tr=="Nutrition + WSH"]), na.rm=T)*100)
latfeces<-c(latfctrln, latfctrlperc, latfwshn, latfwshperc)

potty<-npercfunc(lost, lost$potty)
feceshouse<-npercfunc(lost, lost$humfeces)
feceschildarea<-npercfunc(lost, lost$humfecesch)
handlatwater<-npercfunc(lost, lost$hwlatwat)
handlatsoap<-npercfunc(lost, lost$hwlatsoap)
handkitwater<-npercfunc(lost, lost$hwkitwat)
handkitsoap<-npercfunc(lost, lost$hwkitsoap)

fsnctrl<-length(lost$hfiacat[lost$tr=="Control" & lost$hfiacat=="Food Secure"])
fspercctrl<-round(fsnctrl/length(lost$hfiacat[lost$tr=="Control"])*100)
fsnwsh<-length(lost$hfiacat[lost$tr=="Nutrition + WSH" & lost$hfiacat=="Food Secure"])
fspercwsh<-round(fsnwsh/length(lost$hfiacat[lost$tr=="Nutrition + WSH"])*100)
foodsecure<-c(fsnctrl, fspercctrl, fsnwsh, fspercwsh)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

ctrl<-c(paste("Control (N=", Nlostctrl, ")", sep=""), " ", charobject(momage, 1),charobject(momeduy, 1), " ", charobject(dadeduy, 1), charobjectperc(dadagri, 1),
        " ", charobject(Nhh, 1), charobjectperc(elec, 1), charobjectperc(cement, 1), charobject(acres, 1),
        " ", charobjectperc(tubewell, 1), charobjectperc(storewater, 1), charobjectperc(treatwater, 1), charobject(waterdis, 1), 
        " ", " ", charobjectperc(odmen, 1), charobjectperc(odwomen, 1), charobjectperc(odchild815, 1), charobjectperc(odchild38, 1), charobjectperc(odchild03, 1), 
        " ", charobjectperc(latowned, 1), charobjectperc(latslab, 1), charobjectperc(latseal, 1), charobjectperc(latfeces, 1),
        charobjectperc(potty, 1), 
        " ", charobjectperc(feceshouse, 1), charobjectperc(feceschildarea, 1), 
        " ", " ", charobjectperc(handlatwater, 1), charobjectperc(handlatsoap, 1), 
        " ", charobjectperc(handkitwater, 1), charobjectperc(handkitsoap, 1), 
        " ", charobjectperc(foodsecure, 1))
wsh<-c(paste("N + WSH Intervention (N=", Nlostwsh, ")", sep=""), " ", charobject(momage, 3),charobject(momeduy, 3), " ", charobject(dadeduy, 3), charobjectperc(dadagri, 3),
       " ", charobject(Nhh, 3), charobjectperc(elec, 3), charobjectperc(cement, 3), charobject(acres, 3),
       " ", charobjectperc(tubewell, 3), charobjectperc(storewater, 3), charobjectperc(treatwater, 3), charobject(waterdis, 3), 
       " ", " ", charobjectperc(odmen, 3), charobjectperc(odwomen, 3), charobjectperc(odchild815, 3), charobjectperc(odchild38, 3), charobjectperc(odchild03, 3), 
       " ", charobjectperc(latowned, 3), charobjectperc(latslab, 3), charobjectperc(latseal, 3), charobjectperc(latfeces, 3),
       charobjectperc(potty, 3), 
       " ", charobjectperc(feceshouse, 3), charobjectperc(feceschildarea, 3), 
       " ", " ", charobjectperc(handlatwater, 3), charobjectperc(handlatsoap, 3), 
       " ", charobjectperc(handkitwater, 3), charobjectperc(handkitsoap, 3), 
       " ", charobjectperc(foodsecure, 3))

# Table S1/S2
tbls1<-data.table(" "=c("No. of compounds:", "Maternal", "Age(years)", "Years of education", 
                        "Paternal", "Years of education", "Works in agriculture", 
                        "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
                        "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
                        "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
                        "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
                        "Owned a child potty",
                        "Human feces observed in the", "House", "Child's play area",
                        "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
                        "Nutrition", "Household is food secure"), 
                  "WASH Benefits Main Trial"=c("Control (N=1382)"," ", "24 (5)", "6 (3)", " ", "5 (4)", "414 (30%)", 
                                               " ", "5 (2)", "784 (57%)", "145 (10%)", "0.15 (0.21)",
                                               " ", "1038 (75%)", "666 (48%)", "4 (0%)", "1 (3)", 
                                               " ", " ", "97 (7%)", "62 (4%)", "53 (10%)", "267 (38%)", "245 (82%)",
                                               " ", "750 (54%)", "1251 (95%)", "358 (31%)", "625 (48%)", "61 (4%)", 
                                               " ", "114 (8%)", "21 (2%)", 
                                               " ", " ", "178 (14%)", "88 (7%)", " ", "118 (9%)", "33 (3%)", " ", "932 (67%)"),
                  " "=c ("N + WSH Intervention (N=686)", " ", "24 (6)", "6 (3)", " ", "5 (4)", "207 (30%)", 
                         " ", "5 (2)", "412 (60%)", "72 (10%)", "0.14 (0.38)",
                         " ", "504 (73%)", "331 (48%)", "2 (0%)", "1 (2)", 
                         " ", " ", "50 (7%)", "24 (4%)", "28 (10%)", "134 (37%)", "123 (88%)",
                         " ", "367 (53%)", "621 (94%)", "155 (27%)", "298 (46%)", "30 (4%)", 
                         " ", "49 (8%)", "7 (1%)", 
                         " ", " ", "72 (11%)", "36 (6%)", " ", "60 (9%)", "18 (3%)", " ", "485 (71%)"), 
                  "Immune Status Study: Had outcomes at Year 1"=data_y1$Children.measured.at.Year.1,
                  " "=data_y1$X..1, 
                  "Immune Status Study: Lost to follow-up at Year 2"=ctrl,
                  " "=wsh
)

write.csv(tbls1, file=here('tables/stress/stress_supptable1.csv'))
print(xtable(tbls1), type="html", file=here("tables/stress/stress_supptable1.html"))




### Table S2-S6 ####


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


outcomes4<-c("iPF(2α)-III", "Control", "Nutrition + WSH", "2,3-dinor-iPF(2α±)-III", 
             "Control", "Nutrition + WSH", "iPF(2α±)-VI", "Control", "Nutrition + WSH", "8,12-iso-iPF(2α±)-VI", 
             "Control","Nutrition + WSH")

unadj_diff <-c("","", ci_interval("t2_f2_8ip", res_unadj), "","", 
               ci_interval("t2_f2_23d", res_unadj), "","",ci_interval("t2_f2_VI", res_unadj), "","",
               ci_interval("t2_f2_12i", res_unadj))

age_sex_adj <- c("","", ci_interval("t2_f2_8ip", res_sex),"","", 
                 ci_interval("t2_f2_23d", res_sex), "","",ci_interval("t2_f2_VI", res_sex), "","",
                 ci_interval("t2_f2_12i", res_sex))

full_adj <- c("","", ci_interval("t2_f2_8ip", res_adj),"","", 
              ci_interval("t2_f2_23d", res_adj), "","",ci_interval("t2_f2_VI", res_adj), "","",
              ci_interval("t2_f2_12i", res_adj))

n_t4 <- c("", n("t2_f2_8ip", "Control", mean_sd_tr), n("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
          n("t2_f2_23d", "Control", mean_sd_tr), n("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
          n("t2_f2_VI", "Control", mean_sd_tr),n("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
          n("t2_f2_12i", "Control", mean_sd_tr), n("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

mean_tr <- c("", mean("t2_f2_8ip", "Control", mean_sd_tr), mean("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
             mean("t2_f2_23d", "Control", mean_sd_tr), mean("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_VI", "Control", mean_sd_tr),mean("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_12i", "Control", mean_sd_tr), mean("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

sd_t4 <- c("", sd("t2_f2_8ip", "Control", mean_sd_tr), sd("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
           sd("t2_f2_23d", "Control", mean_sd_tr), sd("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_VI", "Control", mean_sd_tr),sd("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_12i", "Control", mean_sd_tr), sd("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

abs_mean <- c("", mean("t2_f2_8ip_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_8ip_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
              mean("t2_f2_23d_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_23d_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_VI_raw", "Control", absolute_mean_sd_tr),mean("t2_f2_VI_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_12i_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_12i_raw", "Nutrition + WSH", absolute_mean_sd_tr))

ipcw_adj <- c("","", ci_interval("t2_f2_8ip", res_ipcw),"","", 
              ci_interval("t2_f2_23d", res_ipcw), "","",ci_interval("t2_f2_VI", res_ipcw), "","",
              ci_interval("t2_f2_12i", res_ipcw))

tbls4 <- data.table(
  "Outcome" = outcomes4,
  "N" = n_t4,
  "Absolute Mean" = abs_mean,
  "Mean" = mean_tr,
  "Standard Deviation" = sd_t4,
  "Unadjusted Analysis" = unadj_diff, 
  "Age and Sex Adjusted Analysis" = age_sex_adj,
  "Fully Adjusted Analysis" = full_adj,
  "IPCW Adjusted Analysis" = ipcw_adj
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

unadj_diff6 <-c("","", ci_interval("t3_saa_z01", res_unadj), "","", 
                ci_interval("t3_saa_z02", res_unadj), "","",ci_interval("t3_saa_slope", res_unadj), "","",
                ci_interval("t3_residual_saa", res_unadj),
                "","",ci_interval("t3_cort_z01", res_unadj),"","",ci_interval("t3_cort_z03", res_unadj),
                "","",ci_interval("t3_cort_slope", res_unadj),"","",ci_interval("t3_residual_cort", res_unadj),
                "","",ci_interval("t3_map", res_unadj),"","",ci_interval("t3_hr_mean", res_unadj),
                "","",ci_interval("t3_gcr_mean", res_unadj),"","",ci_interval("t3_gcr_cpg12", res_unadj))

age_sex_adj6 <- c("","", ci_interval("t3_saa_z01", res_sex), "","", 
                  ci_interval("t3_saa_z02", res_sex), "","",ci_interval("t3_saa_slope", res_sex), "","",
                  ci_interval("t3_residual_saa", res_sex),
                  "","",ci_interval("t3_cort_z01", res_sex),"","",ci_interval("t3_cort_z03", res_sex),
                  "","",ci_interval("t3_cort_slope", res_sex),"","",ci_interval("t3_residual_cort", res_sex),
                  "","",ci_interval("t3_map", res_sex),"","",ci_interval("t3_hr_mean", res_sex),
                  "","",ci_interval("t3_gcr_mean", res_sex),"","",ci_interval("t3_gcr_cpg12", res_sex))

full_adj6 <- c("","", ci_interval("t3_saa_z01", res_adj), "","", 
               ci_interval("t3_saa_z02", res_adj), "","",ci_interval("t3_saa_slope", res_adj), "","",
               ci_interval("t3_residual_saa", res_adj),
               "","",ci_interval("t3_cort_z01", res_adj),"","",ci_interval("t3_cort_z03", res_adj),
               "","",ci_interval("t3_cort_slope", res_adj),"","",ci_interval("t3_residual_cort", res_adj),
               "","",ci_interval("t3_map", res_adj),"","",ci_interval("t3_hr_mean", res_adj),
               "","",ci_interval("t3_gcr_mean", res_adj),"","",ci_interval("t3_gcr_cpg12", res_adj))

mean_tr6 <- c("", mean("t3_saa_z01", "Control", mean_sd_tr), mean("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
              mean("t3_saa_z02", "Control", mean_sd_tr), mean("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_saa_slope", "Control", mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_residual_saa", "Control", mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_z01", "Control", mean_sd_tr),mean("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_z03", "Control", mean_sd_tr),mean("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_slope", "Control", mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_residual_cort", "Control", mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_map", "Control", mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_hr", "Control", mean_sd_tr),mean("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_gcr", "Control", mean_sd_tr),mean("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_gcr_cpg12", "Control", mean_sd_tr),mean("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

n_t6 <- c("", n("t3_saa_z01", "Control", mean_sd_tr), n("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
          n("t3_saa_z02", "Control", mean_sd_tr), n("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_saa_slope", "Control", mean_sd_tr),n("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_residual_saa", "Control", mean_sd_tr), n("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_z01", "Control", mean_sd_tr),n("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_z03", "Control", mean_sd_tr),n("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_slope", "Control", mean_sd_tr),n("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_residual_cort", "Control", mean_sd_tr),n("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_map", "Control", mean_sd_tr),n("t3_map", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_hr", "Control", mean_sd_tr),n("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_gcr", "Control", mean_sd_tr),n("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_gcr_cpg12", "Control", mean_sd_tr), n("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

sd_t6 <- c("", sd("t3_saa_z01", "Control", mean_sd_tr), sd("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
           sd("t3_saa_z02", "Control", mean_sd_tr), sd("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_saa_slope", "Control", mean_sd_tr),sd("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_saa", "Control", mean_sd_tr), sd("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z01", "Control", mean_sd_tr),sd("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z03", "Control", mean_sd_tr),sd("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_slope", "Control", mean_sd_tr),sd("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_cort", "Control", mean_sd_tr),sd("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_map", "Control", mean_sd_tr),sd("t3_map", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_hr", "Control", mean_sd_tr),sd("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr", "Control", mean_sd_tr),sd("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr_cpg12", "Control", mean_sd_tr), sd("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

abs_mean_t6 <- c("", mean("t3_saa_z01_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
                 mean("t3_saa_z02_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z02_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_saa_slope", "Control", absolute_mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_saa", "Control", absolute_mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z01_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z03_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z03_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_slope", "Control", absolute_mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_cort", "Control", absolute_mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_map", "Control", absolute_mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
                 mean("t3_hr", "Control", absolute_mean_sd_tr),mean("t3_hr", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_cpg12_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_cpg12_raw", "Nutrition + WSH", absolute_mean_sd_tr) )

ipcw_adj6 <-c("","", ci_interval("t3_saa_z01", res_ipcw), "","", 
                ci_interval("t3_saa_z02", res_ipcw), "","",ci_interval("t3_saa_slope", res_ipcw), "","",
                ci_interval("t3_residual_saa", res_ipcw),
                "","",ci_interval("t3_cort_z01", res_ipcw),"","",ci_interval("t3_cort_z03", res_ipcw),
                "","",ci_interval("t3_cort_slope", res_ipcw),"","",ci_interval("t3_residual_cort", res_ipcw),
                "","",ci_interval("t3_map", res_ipcw),"","",ci_interval("t3_hr_mean", res_ipcw),
                "","",ci_interval("t3_gcr_mean", res_ipcw),"","",ci_interval("t3_gcr_cpg12", res_ipcw))

tbls6 <- data.table(
  "Outcome" = outcomes6,
  "N" = n_t6,
  "Absolute Mean" = abs_mean_t6,
  "Mean" = mean_tr6,
  "Standard Deviation" = sd_t6,
  "Unadjusted Analysis" = unadj_diff6, 
  "Age and Sex Adjusted Analysis" = age_sex_adj6,
  "Full Adjusted Analysis" = full_adj6,
  "IPCW Adjusted Analysis" = ipcw_adj6
)

write.csv(tbls4, here('tables/stress/miso9-stress-supplementarytable4.csv'))
write.csv(tbls6, here('tables/stress/miso9-stress-supplementarytable6.csv'))
