rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/"))

source(here("table scripts/immune/immune-tables1-6.R"))
load(here("audrie results/immune_ipcw.RData"))

setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/"))
lab<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)

## IMPORTANT FUNCTIONS ##
#to be used for formatting ipcw variables for table
maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}


#### TABLE S1 ####
# filtering for children with no t3 measurements
lost<-y1 %>% filter_at(vars(igf_t3, gmcsf_t3, ifng_t3, il10_t3, il12_t3, il13_t3, il17_t3,
                              il1_t3, il2_t3, il21_t3, il4_t3, il5_t3, il6_t3, tnfa_t3), all_vars(is.na(.)))

#calculating overall N by arm
Nlostctrl<-nrow(lost[lost$tr=="Control",])
Nlostwsh<-nrow(lost[lost$tr=="Nutrition + WSH",])

imomage<-meansdfunc(lost, lost$momage)
imomeduy<-meansdfunc(lost, lost$momeduy)
idadeduy<-meansdfunc(lost, lost$dadeduy)
idadagri<-npercfunc(lost, lost$dadagri)
iNhh<-meansdfunc(lost, lost$Nhh)
ielec<-npercfunc(lost, lost$elec)
icement<-npercfunc(lost, lost$cement)

iacresmctrl<-round(mean(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
iacressdctrl<-round(sd(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
iacresmwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
iacressdwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
iacres<-c(iacresmctrl, iacressdctrl, iacresmwsh, iacressdwsh)

itubewell<-npercfunc(lost, lost$tubewell)
istorewater<-npercfunc(lost, lost$storewat)
itreatwater<-npercfunc(lost, lost$treatwat)
iwaterdis<-meansdfunc(lost, lost$watmin)
iodmen<-npercfunc(lost, lost$odmen)
iodwomen<-npercfunc(lost, lost$odwom)
iodchild815<-npercfunc(lost, lost$odch815)
iodchild38<-npercfunc(lost, lost$odch38)
iodchild03<-npercfunc(lost, lost$odchu3)
ilatowned<-npercfunc(lost, lost$latown)
ilatslab<-npercfunc(lost, lost$latslab)
ilatseal<-npercfunc(lost, lost$latseal)
ilatfeces<-npercfunc(lost, lost$latfeces)
ipotty<-npercfunc(lost, lost$potty)
ifeceshouse<-npercfunc(lost, lost$humfeces)
ifeceschildarea<-npercfunc(lost, lost$humfecesch)
ihandlatwater<-npercfunc(lost, lost$hwlatwat)
ihandlatsoap<-npercfunc(lost, lost$hwlatsoap)
ihandkitwater<-npercfunc(lost, lost$hwkitwat)
ihandkitsoap<-npercfunc(lost, lost$hwkitsoap)

ifsnctrl<-length(lost$hfiacat[lost$tr=="Control" & lost$hfiacat=="Food Secure"])
ifspercctrl<-round(ifsnctrl/length(lost$hfiacat[lost$tr=="Control"])*100)
ifsnwsh<-length(lost$hfiacat[lost$tr=="Nutrition + WSH" & lost$hfiacat=="Food Secure"])
ifspercwsh<-round(ifsnwsh/length(lost$hfiacat[lost$tr=="Nutrition + WSH"])*100)
ifoodsecure<-c(ifsnctrl, ifspercctrl, ifsnwsh, ifspercwsh)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
ctrl<-c(paste("Control (N=", Nlostctrl, ")", sep=""), " ", charobject(imomage, 1),charobject(imomeduy, 1), " ", charobject(idadeduy, 1), charobjectperc(idadagri, 1),
               " ", charobject(iNhh, 1), charobjectperc(ielec, 1), charobjectperc(icement, 1), charobject(iacres, 1),
               " ", charobjectperc(itubewell, 1), charobjectperc(istorewater, 1), charobjectperc(itreatwater, 1), charobject(iwaterdis, 1), 
               " ", " ", charobjectperc(iodmen, 1), charobjectperc(iodwomen, 1), charobjectperc(iodchild815, 1), charobjectperc(iodchild38, 1), charobjectperc(iodchild03, 1), 
               " ", charobjectperc(ilatowned, 1), charobjectperc(ilatslab, 1), charobjectperc(ilatseal, 1), charobjectperc(ilatfeces, 1),
               charobjectperc(ipotty, 1), 
               " ", charobjectperc(ifeceshouse, 1), charobjectperc(ifeceschildarea, 1), 
               " ", " ", charobjectperc(ihandlatwater, 1), charobjectperc(ihandlatsoap, 1), 
               " ", charobjectperc(ihandkitwater, 1), charobjectperc(ihandkitsoap, 1), 
               " ", charobjectperc(ifoodsecure, 1))
wsh<-c(paste("N + WSH Intervention (N=", Nlostwsh, ")", sep=""), " ", charobject(imomage, 3),charobject(imomeduy, 3), " ", charobject(idadeduy, 3), charobjectperc(idadagri, 3),
       " ", charobject(iNhh, 3), charobjectperc(ielec, 3), charobjectperc(icement, 3), charobject(iacres, 3),
       " ", charobjectperc(itubewell, 3), charobjectperc(istorewater, 3), charobjectperc(itreatwater, 3), charobject(iwaterdis, 3), 
       " ", " ", charobjectperc(iodmen, 3), charobjectperc(iodwomen, 3), charobjectperc(iodchild815, 3), charobjectperc(iodchild38, 3), charobjectperc(iodchild03, 3), 
       " ", charobjectperc(ilatowned, 3), charobjectperc(ilatslab, 3), charobjectperc(ilatseal, 3), charobjectperc(ilatfeces, 3),
       charobjectperc(ipotty, 3), 
       " ", charobjectperc(ifeceshouse, 3), charobjectperc(ifeceschildarea, 3), 
       " ", " ", charobjectperc(ihandlatwater, 3), charobjectperc(ihandlatsoap, 3), 
       " ", charobjectperc(ihandkitwater, 3), charobjectperc(ihandkitsoap, 3), 
       " ", charobjectperc(ifoodsecure, 3))

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
                  "Immune Status Study: Had outcomes at Year 1"=ctrly1,
                  " "=wshy1, 
                  "Immune Status Study: Lost to follow-up at Year 2"=ctrl,
                  " "=wsh
)

write.csv(tbls1, file=here('tables/immune/immune_supplementary/immune_supptable1.csv'))
print(xtable(tbls1), type="html", file=here("tables/immune/immune_supplementary/immune_supptable1.html"))



#### TABLE S2 ####

writeqntle<-function(vector) {
  quantiles<-round(quantile(vector, na.rm=TRUE), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

outcomes3<-c("Outcome", "IL-1β (pg/ml)", "Il-6 (pg/ml)", "TNF-α (pg/ml)", "CRP (mg/L)", "IL-12 (pg/ml)",
           "IFN-γ (pg/ml)", "IL-4 (pg/ml)", "IL-5 (pg/ml)", "IL-13 (pg/ml)", "IL-17A (pg/ml)", 
           "IL-21 (pg/ml)", "IL-10 (pg/ml)", "IL-2 (pg/ml)", "GM-CSF (pg/ml)", "AGP (g/L)", "IGF-1 (μg/L)")

t2<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t2), writeqntle(lab$il6_t2), writeqntle(lab$tnfa_t2), writeqntle(lab$crp_t2), writeqntle(lab$il12_t2), writeqntle(lab$ifng_t2), 
      writeqntle(lab$il4_t2), writeqntle(lab$il5_t2), writeqntle(lab$il13_t2), writeqntle(lab$il17_t2), writeqntle(lab$il21_t2), writeqntle(lab$il10_t2), writeqntle(lab$il2_t2), 
      writeqntle(lab$gmcsf_t2), writeqntle(lab$agp_t2), writeqntle(lab$igf_t2))

t3<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t3), writeqntle(lab$il6_t3), writeqntle(lab$tnfa_t3), " ", writeqntle(lab$il12_t3), writeqntle(lab$ifng_t3), 
      writeqntle(lab$il4_t3), writeqntle(lab$il5_t3), writeqntle(lab$il13_t3), writeqntle(lab$il17_t3), writeqntle(lab$il21_t3), writeqntle(lab$il10_t3), writeqntle(lab$il2_t3), 
      writeqntle(lab$gmcsf_t3), " ", writeqntle(lab$igf_t3))

tbls3<-data.table(" "=outcomes3,
                  "Child Age 14 Months"=t2,
                  "Child Age 28 Months"=t3)

write.csv(tbls3, file=here('tables/immune/immune_supplementary/immune_supptable2.csv'))
print(xtable(tbls3), type="html", file=here("tables/immune/immune_supplementary/immune_supptable2.html"))



#### TABLE S3 ####

ipcws4<-c(" ", " ", maketblvalue(il12_t2_adj_ipcw_L$`unlist(il12_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il6_t2_adj_ipcw_L$`unlist(il6_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(tnf_t2_adj_ipcw_L$`unlist(tnf_t2_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(crp_t2_adj_ipcw_L$`unlist(crp_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il12_t2_adj_ipcw_L$`unlist(il12_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ifn_t2_adj_ipcw_L$`unlist(ifn_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il4_t2_adj_ipcw_L$`unlist(il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il5_t2_adj_ipcw_L$`unlist(il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il13_t2_adj_ipcw_L$`unlist(il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il17_t2_adj_ipcw_L$`unlist(il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il21_t2_adj_ipcw_L$`unlist(il21_t2_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(il10_t2_adj_ipcw_L$`unlist(il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il2_t2_adj_ipcw_L$`unlist(il2_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(gmc_t2_adj_ipcw_L$`unlist(gmc_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(agp_t2_adj_ipcw_L$`unlist(agp_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(igf_t2_adj_ipcw_L$`unlist(igf_t2_adj_ipcw$estimates$ATE)`))

tbls4<-cbind(tbl2, ipcws4)
names(tbls4)[10]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls4, file=here('tables/immune/immune_supplementary/immune_supptable3.csv'))
print(xtable(tbls4), type="html", file=here("tables/immune/immune_supplementary/immune_supptable3.html"))


#### TABLE S4 ####

ipcws5<-c(" ", " ", maketblvalue(ratio_il1_il10_t2_adj_ipcw_L$`unlist(ratio_il1_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il6_il10_t2_adj_ipcw_L$`unlist(ratio_il6_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_tnf_il10_t2_adj_ipcw_L$`unlist(ratio_tnf_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il10_t2_adj_ipcw_L$`unlist(ratio_il12_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il10_t2_adj_ipcw_L$`unlist(ratio_ifn_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il4_il10_t2_adj_ipcw_L$`unlist(ratio_il4_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il5_il10_t2_adj_ipcw_L$`unlist(ratio_il5_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il13_il10_t2_adj_ipcw_L$`unlist(ratio_il13_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il17_il10_t2_adj_ipcw_L$`unlist(ratio_il17_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il21_il10_t2_adj_ipcw_L$`unlist(ratio_il21_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il2_il10_t2_adj_ipcw_L$`unlist(ratio_il2_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_gmc_il10_t2_adj_ipcw_L$`unlist(ratio_gmc_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il4_t2_adj_ipcw_L$`unlist(ratio_il12_il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il4_t2_adj_ipcw_L$`unlist(ratio_ifn_il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il5_t2_adj_ipcw_L$`unlist(ratio_il12_il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il5_t2_adj_ipcw_L$`unlist(ratio_ifn_il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il13_t2_adj_ipcw_L$`unlist(ratio_il12_il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il13_t2_adj_ipcw_L$`unlist(ratio_ifn_il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il17_t2_adj_ipcw_L$`unlist(ratio_il12_il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il17_t2_adj_ipcw_L$`unlist(ratio_ifn_il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il21_t2_adj_ipcw_L$`unlist(ratio_il12_il21_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il21_t2_adj_ipcw_L$`unlist(ratio_ifn_il21_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_pro_il10_t2_adj_ipcw_L$`unlist(ratio_pro_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_il10_t2_adj_ipcw_L$`unlist(ratio_th1_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th2_il10_t2_adj_ipcw_L$`unlist(ratio_th2_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th17_il10_t2_adj_ipcw_L$`unlist(ratio_th17_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th2_t2_adj_ipcw_L$`unlist(ratio_th1_th2_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th17_t2_adj_ipcw_L$`unlist(ratio_th1_th17_t2_adj_ipcw$estimates$ATE)`))

tbls5<-cbind(tbl3, ipcws5)
names(tbls5)[10]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls5, file=here('tables/immune/immune_supplementary/immune_supptable4.csv'))
print(xtable(tbls5), type="html", file=here("tables/immune/immune_supplementary/immune_supptable4.html"))


#### TABLE S5 ####

ipcws6<-c(" ", " ", maketblvalue(il1_t3_adj_ipcw_L$`unlist(il1_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il6_t3_adj_ipcw_L$`unlist(il6_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(tnf_t3_adj_ipcw_L$`unlist(tnf_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il12_t3_adj_ipcw_L$`unlist(il12_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ifn_t3_adj_ipcw_L$`unlist(ifn_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il4_t3_adj_ipcw_L$`unlist(il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il5_t3_adj_ipcw_L$`unlist(il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il13_t3_adj_ipcw_L$`unlist(il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il17_t3_adj_ipcw_L$`unlist(il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il21_t3_adj_ipcw_L$`unlist(il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il10_t3_adj_ipcw_L$`unlist(il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il2_t3_adj_ipcw_L$`unlist(il2_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(gmc_t3_adj_ipcw_L$`unlist(gmc_t3_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(igf_t3_adj_ipcw_L$`unlist(igf_t3_adj_ipcw$estimates$ATE)`))

tbls6<-cbind(tbl4, ipcws6)
names(tbls6)[10]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls6, file=here('tables/immune/immune_supplementary/immune_supptable5.csv'))
print(xtable(tbls6), type="html", file=here("tables/immune/immune_supplementary/immune_supptable5.html"))


#### TABLE S6 ####

ipcws7<-c(" ", " ", maketblvalue(ratio_il1_il10_t3_adj_ipcw_L$`unlist(ratio_il1_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il6_il10_t3_adj_ipcw_L$`unlist(ratio_il6_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_tnf_il10_t3_adj_ipcw_L$`unlist(ratio_tnf_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il10_t3_adj_ipcw_L$`unlist(ratio_il12_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il10_t3_adj_ipcw_L$`unlist(ratio_ifn_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il4_il10_t3_adj_ipcw_L$`unlist(ratio_il4_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il5_il10_t3_adj_ipcw_L$`unlist(ratio_il5_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il13_il10_t3_adj_ipcw_L$`unlist(ratio_il13_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il17_il10_t3_adj_ipcw_L$`unlist(ratio_il17_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il21_il10_t3_adj_ipcw_L$`unlist(ratio_il21_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il2_il10_t3_adj_ipcw_L$`unlist(ratio_il2_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_gmc_il10_t3_adj_ipcw_L$`unlist(ratio_gmc_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il4_t3_adj_ipcw_L$`unlist(ratio_il12_il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il4_t3_adj_ipcw_L$`unlist(ratio_ifn_il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il5_t3_adj_ipcw_L$`unlist(ratio_il12_il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il5_t3_adj_ipcw_L$`unlist(ratio_ifn_il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il13_t3_adj_ipcw_L$`unlist(ratio_il12_il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il13_t3_adj_ipcw_L$`unlist(ratio_ifn_il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il17_t3_adj_ipcw_L$`unlist(ratio_il12_il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il17_t3_adj_ipcw_L$`unlist(ratio_ifn_il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il21_t3_adj_ipcw_L$`unlist(ratio_il12_il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il21_t3_adj_ipcw_L$`unlist(ratio_ifn_il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_pro_il10_t3_adj_ipcw_L$`unlist(ratio_pro_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_il10_t3_adj_ipcw_L$`unlist(ratio_th1_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th2_il10_t3_adj_ipcw_L$`unlist(ratio_th2_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th17_il10_t3_adj_ipcw_L$`unlist(ratio_th17_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th2_t3_adj_ipcw_L$`unlist(ratio_th1_th2_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th17_t3_adj_ipcw_L$`unlist(ratio_th1_th17_t3_adj_ipcw$estimates$ATE)`))

tbls7<-cbind(tbl5, ipcws7)
names(tbls7)[10]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls7, file=here('tables/immune/immune_supplementary/immune_supptable6.csv'))
print(xtable(tbls7), type="html", file=here("tables/immune/immune_supplementary/immune_supptable6.html"))



#### TABLE S7 ####

ipcws8<-c(" ", " ", maketblvalue(d23_il1_adj_ipcw_L$`unlist(d23_il1_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il6_adj_ipcw_L$`unlist(d23_il6_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_tnf_adj_ipcw_L$`unlist(d23_tnf_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il12_adj_ipcw_L$`unlist(d23_il12_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_ifn_adj_ipcw_L$`unlist(d23_ifn_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il4_adj_ipcw_L$`unlist(d23_il4_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il5_adj_ipcw_L$`unlist(d23_il5_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il13_adj_ipcw_L$`unlist(d23_il13_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il17_adj_ipcw_L$`unlist(d23_il17_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il21_adj_ipcw_L$`unlist(d23_il21_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il10_adj_ipcw_L$`unlist(d23_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il2_adj_ipcw_L$`unlist(d23_il2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_gmc_adj_ipcw_L$`unlist(d23_gmc_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_igf_adj_ipcw_L$`unlist(d23_igf_adj_ipcw$estimates$ATE)`))

tbls8<-cbind(tbl6, ipcws8)
names(tbls8)[10]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls8, file=here('tables/immune/immune_supplementary/immune_supptable7.csv'))
print(xtable(tbls8), type="html", file=here("tables/immune/immune_supplementary/immune_supptable7.html"))



#### TABLE S8 ####

#calculates N, mean, and sd for each variable and stores as string in vector
meansd<-function(var){
  c(as.character(round(var$mean[1], 2)), as.character(round(var$mean[2], 2)), 
    as.character(round(var$sd[1], 2)), as.character(round(var$sd[2], 2)))
}

#works for confidence intervals except ipcw
makecival<-function(var){
  rounded<-round(var, 2)
  paste(rounded[1], " (", rounded[2], ", ", rounded[3], ")", sep="")
}

makeipcw<-function(var){
  rounded<-round(var, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}

outcomes9<-c("Ln ΔIL-1β/IL-10", "Control", "Nutrition + WSH", 
             "Ln ΔIL-6/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTNF-α/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-4/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-5/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-13/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-17A/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-21/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-2/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔGM-CSF/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-4", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-4", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-5", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-5", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-13", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-13", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-17A", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-17A", "Control", "Nutrition + WSH",
             "Ln ΔIL-12/IL-21", "Control", "Nutrition + WSH",
             "Ln ΔIFN-γ/IL-21", "Control", "Nutrition + WSH",
             "Ln ΔPro-inflammatory cytokines/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh1/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh2/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh17/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh1/Th2", "Control", "Nutrition + WSH",
             "Ln ΔTh1/Th17", "Control", "Nutrition + WSH")

Ns9<-c(" ", as.character(d23_ratio_il1_il10_N_tr$d23_ratio_il1_il10_N_tr[1]), as.character(d23_ratio_il1_il10_N_tr$d23_ratio_il1_il10_N_tr[2]),
       " ", as.character(d23_ratio_il6_il10_N_tr$d23_ratio_il6_il10_N_tr[1]), as.character(d23_ratio_il6_il10_N_tr$d23_ratio_il6_il10_N_tr[2]),
       " ", as.character(d23_ratio_tnf_il10_N_tr$d23_ratio_tnf_il10_N_tr[1]), as.character(d23_ratio_tnf_il10_N_tr$d23_ratio_tnf_il10_N_tr[2]),
       " ", as.character(d23_ratio_il12_il10_N_tr$d23_ratio_il12_il10_N_tr[1]), as.character(d23_ratio_il12_il10_N_tr$d23_ratio_il12_il10_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il10_N_tr$d23_ratio_ifn_il10_N_tr[1]), as.character(d23_ratio_ifn_il10_N_tr$d23_ratio_ifn_il10_N_tr[2]),
       " ", as.character(d23_ratio_il4_il10_N_tr$d23_ratio_il4_il10_N_tr[1]), as.character(d23_ratio_il4_il10_N_tr$d23_ratio_il4_il10_N_tr[2]),
       " ", as.character(d23_ratio_il5_il10_N_tr$d23_ratio_il5_il10_N_tr[1]), as.character(d23_ratio_il5_il10_N_tr$d23_ratio_il5_il10_N_tr[2]),
       " ", as.character(d23_ratio_il13_il10_N_tr$d23_ratio_il13_il10_N_tr[1]), as.character(d23_ratio_il13_il10_N_tr$d23_ratio_il13_il10_N_tr[2]),
       " ", as.character(d23_ratio_il17_il10_N_tr$d23_ratio_il17_il10_N_tr[1]), as.character(d23_ratio_il17_il10_N_tr$d23_ratio_il17_il10_N_tr[2]),
       " ", as.character(d23_ratio_il21_il10_N_tr$d23_ratio_il21_il10_N_tr[1]), as.character(d23_ratio_il21_il10_N_tr$d23_ratio_il21_il10_N_tr[2]),
       " ", as.character(d23_ratio_il2_il10_N_tr$d23_ratio_il2_il10_N_tr[1]), as.character(d23_ratio_il2_il10_N_tr$d23_ratio_il2_il10_N_tr[2]),
       " ", as.character(d23_ratio_gmc_il10_N_tr$d23_ratio_gmc_il10_N_tr[1]), as.character(d23_ratio_gmc_il10_N_tr$d23_ratio_gmc_il10_N_tr[2]),
       " ", as.character(d23_ratio_il12_il4_N_tr$d23_ratio_il12_il4_N_tr[1]), as.character(d23_ratio_il12_il4_N_tr$d23_ratio_il12_il4_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il4_N_tr$d23_ratio_ifn_il4_N_tr[1]), as.character(d23_ratio_ifn_il4_N_tr$d23_ratio_ifn_il4_N_tr[2]),
       " ", as.character(d23_ratio_il12_il5_N_tr$d23_ratio_il12_il5_N_tr[1]), as.character(d23_ratio_il12_il5_N_tr$d23_ratio_il12_il5_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il5_N_tr$d23_ratio_ifn_il5_N_tr[1]), as.character(d23_ratio_ifn_il5_N_tr$d23_ratio_ifn_il5_N_tr[2]),
       " ", as.character(d23_ratio_il12_il13_N_tr$d23_ratio_il12_il13_N_tr[1]), as.character(d23_ratio_il12_il13_N_tr$d23_ratio_il12_il13_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il13_N_tr$d23_ratio_ifn_il13_N_tr[1]), as.character(d23_ratio_ifn_il13_N_tr$d23_ratio_ifn_il13_N_tr[2]),
       " ", as.character(d23_ratio_il12_il17_N_tr$d23_ratio_il12_il17_N_tr[1]), as.character(d23_ratio_il12_il17_N_tr$d23_ratio_il12_il17_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il17_N_tr$d23_ratio_ifn_il17_N_tr[1]), as.character(d23_ratio_ifn_il17_N_tr$d23_ratio_ifn_il17_N_tr[2]),
       " ", as.character(d23_ratio_il12_il21_N_tr$d23_ratio_il12_il21_N_tr[1]), as.character(d23_ratio_il12_il21_N_tr$d23_ratio_il12_il21_N_tr[2]),
       " ", as.character(d23_ratio_ifn_il21_N_tr$d23_ratio_ifn_il21_N_tr[1]), as.character(d23_ratio_ifn_il21_N_tr$d23_ratio_ifn_il21_N_tr[2]),
       " ", as.character(d23_ratio_pro_il10_N_tr$d23_ratio_pro_il10_N_tr[1]), as.character(d23_ratio_pro_il10_N_tr$d23_ratio_pro_il10_N_tr[2]),
       " ", as.character(d23_ratio_th1_il10_N_tr$d23_ratio_th1_il10_N_tr[1]), as.character(d23_ratio_th1_il10_N_tr$d23_ratio_th1_il10_N_tr[2]),
       " ", as.character(d23_ratio_th2_il10_N_tr$d23_ratio_th2_il10_N_tr[1]), as.character(d23_ratio_th2_il10_N_tr$d23_ratio_th2_il10_N_tr[2]),
       " ", as.character(d23_ratio_th17_il10_N_tr$d23_ratio_th17_il10_N_tr[1]), as.character(d23_ratio_th17_il10_N_tr$d23_ratio_th17_il10_N_tr[2]),
       " ", as.character(d23_ratio_th1_th2_N_tr$d23_ratio_th1_th2_N_tr[1]), as.character(d23_ratio_th1_th2_N_tr$d23_ratio_th1_th2_N_tr[2]),
       " ", as.character(d23_ratio_th1_th17_N_tr$d23_ratio_th1_th17_N_tr[1]), as.character(d23_ratio_th1_th17_N_tr$d23_ratio_th1_th17_N_tr[2])
)

absmeantbls9 <- c(" ", as.character(round(abs_d23_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il1_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il6_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_tnf_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il4_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il5_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il13_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il17_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il21_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il2_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_gmc_il10_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il4_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il4_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il5_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il5_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il13_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il13_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il17_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il17_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_il12_il21_N_tr$mean[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(abs_d23_ratio_ifn_il21_N_tr$mean[2], 2)),
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ")

abssdtbls9 <- c(" ", as.character(round(abs_d23_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il1_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il6_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_tnf_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il4_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il5_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il13_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il17_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il21_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il2_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_gmc_il10_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il4_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il4_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il5_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il5_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il13_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il13_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il17_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il17_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_il12_il21_N_tr$sd[2], 2)),
                  " ", as.character(round(abs_d23_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(abs_d23_ratio_ifn_il21_N_tr$sd[2], 2)),
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ",
                  " ", " ", " ")

means9<-c(" ", meansd(d23_ratio_il1_il10_N_tr)[1], meansd(d23_ratio_il1_il10_N_tr)[2],
          " ", meansd(d23_ratio_il6_il10_N_tr)[1], meansd(d23_ratio_il6_il10_N_tr)[2],
          " ", meansd(d23_ratio_tnf_il10_N_tr)[1], meansd(d23_ratio_tnf_il10_N_tr)[2],
          " ", meansd(d23_ratio_il12_il10_N_tr)[1], meansd(d23_ratio_il12_il10_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il10_N_tr)[1], meansd(d23_ratio_ifn_il10_N_tr)[2],
          " ", meansd(d23_ratio_il4_il10_N_tr)[1], meansd(d23_ratio_il4_il10_N_tr)[2],
          " ", meansd(d23_ratio_il5_il10_N_tr)[1], meansd(d23_ratio_il5_il10_N_tr)[2],
          " ", meansd(d23_ratio_il13_il10_N_tr)[1], meansd(d23_ratio_il13_il10_N_tr)[2],
          " ", meansd(d23_ratio_il17_il10_N_tr)[1], meansd(d23_ratio_il17_il10_N_tr)[2],
          " ", meansd(d23_ratio_il21_il10_N_tr)[1], meansd(d23_ratio_il21_il10_N_tr)[2],
          " ", meansd(d23_ratio_il2_il10_N_tr)[1], meansd(d23_ratio_il2_il10_N_tr)[2],
          " ", meansd(d23_ratio_gmc_il10_N_tr)[1], meansd(d23_ratio_gmc_il10_N_tr)[2],
          " ", meansd(d23_ratio_il12_il4_N_tr)[1], meansd(d23_ratio_il12_il4_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il4_N_tr)[1], meansd(d23_ratio_ifn_il4_N_tr)[2],
          " ", meansd(d23_ratio_il12_il5_N_tr)[1], meansd(d23_ratio_il12_il5_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il5_N_tr)[1], meansd(d23_ratio_ifn_il5_N_tr)[2],
          " ", meansd(d23_ratio_il12_il13_N_tr)[1], meansd(d23_ratio_il12_il13_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il13_N_tr)[1], meansd(d23_ratio_ifn_il13_N_tr)[2],
          " ", meansd(d23_ratio_il12_il17_N_tr)[1], meansd(d23_ratio_il12_il17_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il17_N_tr)[1], meansd(d23_ratio_ifn_il17_N_tr)[2],
          " ", meansd(d23_ratio_il12_il21_N_tr)[1], meansd(d23_ratio_il12_il21_N_tr)[2],
          " ", meansd(d23_ratio_ifn_il21_N_tr)[1], meansd(d23_ratio_ifn_il21_N_tr)[2],
          " ", meansd(d23_ratio_pro_il10_N_tr)[1], meansd(d23_ratio_pro_il10_N_tr)[2],
          " ", meansd(d23_ratio_th1_il10_N_tr)[1], meansd(d23_ratio_th1_il10_N_tr)[2],
          " ", meansd(d23_ratio_th2_il10_N_tr)[1], meansd(d23_ratio_th2_il10_N_tr)[2],
          " ", meansd(d23_ratio_th17_il10_N_tr)[1], meansd(d23_ratio_th17_il10_N_tr)[2],
          " ", meansd(d23_ratio_th1_th2_N_tr)[1], meansd(d23_ratio_th1_th2_N_tr)[2],
          " ", meansd(d23_ratio_th1_th17_N_tr)[1], meansd(d23_ratio_th1_th17_N_tr)[2])

sds9<-c(" ", meansd(d23_ratio_il1_il10_N_tr)[3], meansd(d23_ratio_il1_il10_N_tr)[4],
        " ", meansd(d23_ratio_il6_il10_N_tr)[3], meansd(d23_ratio_il6_il10_N_tr)[4],
        " ", meansd(d23_ratio_tnf_il10_N_tr)[3], meansd(d23_ratio_tnf_il10_N_tr)[4],
        " ", meansd(d23_ratio_il12_il10_N_tr)[3], meansd(d23_ratio_il12_il10_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il10_N_tr)[3], meansd(d23_ratio_ifn_il10_N_tr)[4],
        " ", meansd(d23_ratio_il4_il10_N_tr)[3], meansd(d23_ratio_il4_il10_N_tr)[4],
        " ", meansd(d23_ratio_il5_il10_N_tr)[3], meansd(d23_ratio_il5_il10_N_tr)[4],
        " ", meansd(d23_ratio_il13_il10_N_tr)[3], meansd(d23_ratio_il13_il10_N_tr)[4],
        " ", meansd(d23_ratio_il17_il10_N_tr)[3], meansd(d23_ratio_il17_il10_N_tr)[4],
        " ", meansd(d23_ratio_il21_il10_N_tr)[3], meansd(d23_ratio_il21_il10_N_tr)[4],
        " ", meansd(d23_ratio_il2_il10_N_tr)[3], meansd(d23_ratio_il2_il10_N_tr)[4],
        " ", meansd(d23_ratio_gmc_il10_N_tr)[3], meansd(d23_ratio_gmc_il10_N_tr)[4],
        " ", meansd(d23_ratio_il12_il4_N_tr)[3], meansd(d23_ratio_il12_il4_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il4_N_tr)[3], meansd(d23_ratio_ifn_il4_N_tr)[4],
        " ", meansd(d23_ratio_il12_il5_N_tr)[3], meansd(d23_ratio_il12_il5_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il5_N_tr)[3], meansd(d23_ratio_ifn_il5_N_tr)[4],
        " ", meansd(d23_ratio_il12_il13_N_tr)[3], meansd(d23_ratio_il12_il13_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il13_N_tr)[3], meansd(d23_ratio_ifn_il13_N_tr)[4],
        " ", meansd(d23_ratio_il12_il17_N_tr)[3], meansd(d23_ratio_il12_il17_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il17_N_tr)[3], meansd(d23_ratio_ifn_il17_N_tr)[4],
        " ", meansd(d23_ratio_il12_il21_N_tr)[3], meansd(d23_ratio_il12_il21_N_tr)[4],
        " ", meansd(d23_ratio_ifn_il21_N_tr)[3], meansd(d23_ratio_ifn_il21_N_tr)[4],
        " ", meansd(d23_ratio_pro_il10_N_tr)[3], meansd(d23_ratio_pro_il10_N_tr)[4],
        " ", meansd(d23_ratio_th1_il10_N_tr)[3], meansd(d23_ratio_th1_il10_N_tr)[4],
        " ", meansd(d23_ratio_th2_il10_N_tr)[3], meansd(d23_ratio_th2_il10_N_tr)[4],
        " ", meansd(d23_ratio_th17_il10_N_tr)[3], meansd(d23_ratio_th17_il10_N_tr)[4],
        " ", meansd(d23_ratio_th1_th2_N_tr)[3], meansd(d23_ratio_th1_th2_N_tr)[4],
        " ", meansd(d23_ratio_th1_th17_N_tr)[3], meansd(d23_ratio_th1_th17_N_tr)[4])

unadjs9<-c(" ", " ", makecival(d23_ratio_il1_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il6_il10_unadj_L),
           " ", " ", makecival(d23_ratio_tnf_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il10_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il4_il10_unadj_L), 
           " ", " ", makecival(d23_ratio_il5_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il13_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il17_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il21_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il2_il10_unadj_L),
           " ", " ", makecival(d23_ratio_gmc_il10_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il4_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il4_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il5_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il5_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il13_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il13_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il17_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il17_unadj_L),
           " ", " ", makecival(d23_ratio_il12_il21_unadj_L),
           " ", " ", makecival(d23_ratio_ifn_il21_unadj_L),
           " ", " ", makecival(d23_ratio_pro_il10_unadj_L),
           " ", " ", makecival(d23_ratio_th1_il10_unadj_L),
           " ", " ", makecival(d23_ratio_th2_il10_unadj_L),
           " ", " ", makecival(d23_ratio_th17_il10_unadj_L),
           " ", " ", makecival(d23_ratio_th1_th2_unadj_L),
           " ", " ", makecival(d23_ratio_th1_th17_unadj_L))

agesexadjs9<-c(" ", " ", makecival(d23_ratio_il1_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il6_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_tnf_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il4_il10_adj_sex_age_L), 
               " ", " ", makecival(d23_ratio_il5_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il13_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il17_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il21_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il2_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_gmc_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il4_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il4_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il5_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il5_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il13_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il13_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il17_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il17_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_il12_il21_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_ifn_il21_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_pro_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_th1_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_th2_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_th17_il10_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_th1_th2_adj_sex_age_L),
               " ", " ", makecival(d23_ratio_th1_th17_adj_sex_age_L))

adjs9<-c(" ", " ", makecival(d23_ratio_il1_il10_adj_L),
         " ", " ", makecival(d23_ratio_il6_il10_adj_L),
         " ", " ", makecival(d23_ratio_tnf_il10_adj_L),
         " ", " ", makecival(d23_ratio_il12_il10_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il10_adj_L),
         " ", " ", makecival(d23_ratio_il4_il10_adj_L), 
         " ", " ", makecival(d23_ratio_il5_il10_adj_L),
         " ", " ", makecival(d23_ratio_il13_il10_adj_L),
         " ", " ", makecival(d23_ratio_il17_il10_adj_L),
         " ", " ", makecival(d23_ratio_il21_il10_adj_L),
         " ", " ", makecival(d23_ratio_il2_il10_adj_L),
         " ", " ", makecival(d23_ratio_gmc_il10_adj_L),
         " ", " ", makecival(d23_ratio_il12_il4_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il4_adj_L),
         " ", " ", makecival(d23_ratio_il12_il5_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il5_adj_L),
         " ", " ", makecival(d23_ratio_il12_il13_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il13_adj_L),
         " ", " ", makecival(d23_ratio_il12_il17_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il17_adj_L),
         " ", " ", makecival(d23_ratio_il12_il21_adj_L),
         " ", " ", makecival(d23_ratio_ifn_il21_adj_L),
         " ", " ", makecival(d23_ratio_pro_il10_adj_L),
         " ", " ", makecival(d23_ratio_th1_il10_adj_L),
         " ", " ", makecival(d23_ratio_th2_il10_adj_L),
         " ", " ", makecival(d23_ratio_th17_il10_adj_L),
         " ", " ", makecival(d23_ratio_th1_th2_adj_L),
         " ", " ", makecival(d23_ratio_th1_th17_adj_L))

ipcws9<-c(" ", " ", makeipcw(d23_ratio_il1_il10_adj_ipcw_L$`unlist(d23_ratio_il1_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il6_il10_adj_ipcw_L$`unlist(d23_ratio_il6_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_tnf_il10_adj_ipcw_L$`unlist(d23_ratio_tnf_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il10_adj_ipcw_L$`unlist(d23_ratio_il12_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il10_adj_ipcw_L$`unlist(d23_ratio_ifn_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il4_il10_adj_ipcw_L$`unlist(d23_ratio_il4_il10_adj_ipcw$estimates$ATE)`), 
          " ", " ", makeipcw(d23_ratio_il5_il10_adj_ipcw_L$`unlist(d23_ratio_il5_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il13_il10_adj_ipcw_L$`unlist(d23_ratio_il13_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il17_il10_adj_ipcw_L$`unlist(d23_ratio_il17_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il21_il10_adj_ipcw_L$`unlist(d23_ratio_il21_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il2_il10_adj_ipcw_L$`unlist(d23_ratio_il2_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_gmc_il10_adj_ipcw_L$`unlist(d23_ratio_gmc_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il4_adj_ipcw_L$`unlist(d23_ratio_il12_il4_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il4_adj_ipcw_L$`unlist(d23_ratio_ifn_il4_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il5_adj_ipcw_L$`unlist(d23_ratio_il12_il5_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il5_adj_ipcw_L$`unlist(d23_ratio_ifn_il5_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il13_adj_ipcw_L$`unlist(d23_ratio_il12_il13_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il13_adj_ipcw_L$`unlist(d23_ratio_ifn_il13_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il17_adj_ipcw_L$`unlist(d23_ratio_il12_il17_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il17_adj_ipcw_L$`unlist(d23_ratio_ifn_il17_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_il12_il21_adj_ipcw_L$`unlist(d23_ratio_il12_il21_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_ifn_il21_adj_ipcw_L$`unlist(d23_ratio_ifn_il21_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_pro_il10_adj_ipcw_L$`unlist(d23_ratio_pro_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_th1_il10_adj_ipcw_L$`unlist(d23_ratio_th1_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_th2_il10_adj_ipcw_L$`unlist(d23_ratio_th2_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_th17_il10_adj_ipcw_L$`unlist(d23_ratio_th17_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_th1_th2_adj_ipcw_L$`unlist(d23_ratio_th1_th2_adj_ipcw$estimates$ATE)`),
          " ", " ", makeipcw(d23_ratio_th1_th17_adj_ipcw_L$`unlist(d23_ratio_th1_th17_adj_ipcw$estimates$ATE)`))

tbls9<-data.table("Outcome, Arm" = outcomes9,
                  "N" = Ns9, 
                  "Absolute Mean" = absmeantbls9,
                  "Absolute SD" = abssdtbls9,
                  "Mean" = means9,
                  "SD" = sds9,
                  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjs9,
                  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = agesexadjs9,
                  "Fully adjusted difference: Intervention vs. Control (95%)" = adjs9,
                  "IPCW adjusted difference: Intervention vs. Control (95%)" = ipcws9)

write.csv(tbls9, file=here('tables/immune/immune_supplementary/immune_supptable8.csv'))
print(xtable(tbls9), type="html", file=here("tables/immune/immune_supplementary/immune_supptable8.html"))

