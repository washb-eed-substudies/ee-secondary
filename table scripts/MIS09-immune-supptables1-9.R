rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

source(here("table scripts/MIS09-immune-tables1-6.R"))
load(here("audrie results/immune_ipcw.RData"))
lab<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)

## IMPORTANT FUNCTIONS ##
#to be used for formatting ipcw variables for table
maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}


#### TABLE S1 ####
tbls1<-data.table(" "=append(c("No. of compounds:"), tbl1$`No. of compounds:`), 
                  "WASH Benefits Main Trial"=c("Control (N=1382)"," ", "24 (5)", "6 (3)", " ", "5 (4)", "414 (30%)", 
                                               " ", "5 (2)", "784 (57%)", "145 (10%)", "0.15 (0.21)",
                                               " ", "1038 (75%)", "666 (48%)", "4 (0%)", "1 (3)", 
                                               " ", " ", "97 (7%)", "62 (4%)", "53 (10%)", "267 (38%)", "245 (82%)",
                                               " ", "750 (54%)", "1251 (95%)", "358 (31%)", "625 (48%)", "61 (4%)", 
                                               " ", "114 (8%)", "21 (2%)", 
                                               " ", " ", "178 (14%)", "88 (7%)", " ", "118 (9%)", "33 (3%)", " ", "932 (67%)"),
                  " "=c ("N + WSH (N=686)", " ", "24 (6)", "6 (3)", " ", "5 (4)", "207 (30%)", 
                         " ", "5 (2)", "412 (60%)", "72 (10%)", "0.14 (0.38)",
                         " ", "504 (73%)", "331 (48%)", "2 (0%)", "1 (2)", 
                         " ", " ", "50 (7%)", "24 (4%)", "28 (10%)", "134 (37%)", "123 (88%)",
                         " ", "367 (53%)", "621 (94%)", "155 (27%)", "298 (46%)", "30 (4%)", 
                         " ", "49 (8%)", "7 (1%)", 
                         " ", " ", "72 (11%)", "36 (6%)", " ", "60 (9%)", "18 (3%)", " ", "485 (71%)"), 
                  "Immune Status Study"=append(c("Control (N=402)"), tbl1$`Control (N=402)`),
                  " "=append(c("N + WSH (N=404)"), tbl1$`N + WSH (N=404)`)
)

write.csv(tbls1, file=here("tables/miso9-immune-supptable1.csv"))
print(xtable(tbls1), type="html", file=here("tables/miso9-immune-supptable1.html"))


#### TABLE S2 ####

# filtering to include children that have at least one measurement for t2
included<-ages[complete.cases(ages[c(103, 105:119)]),]

# filtering for children with no t3 measurements
lost<-ages %>% filter_at(vars(igf_t3, gmcsf_t3, ifng_t3, il10_t3, il12_t3, il13_t3, il17_t3,
                              il1_t3, il2_t3, il21_t3, il4_t3, il5_t3, il6_t3, tnfa_t3), all_vars(is.na(.)))

#calculating overall N by arm
Nincluded<-nrow(included)
Nlost<-nrow(lost)

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(variable) {
  mean<-round(mean(variable, na.rm=TRUE))
  sd<-round(sd(variable, na.rm=TRUE))
  c(mean, sd)
}

npercfunc <- function(variable) {
  n<-sum(variable, na.rm=TRUE)
  perc<-round(mean(variable, na.rm=TRUE)*100)
  c(n, perc)
}

imomage<-meansdfunc(included$momage)
imomeduy<-meansdfunc(included$momeduy)
idadeduy<-meansdfunc(included$dadeduy)
idadagri<-npercfunc(included$dadagri)
iNhh<-meansdfunc(included$Nhh)
ielec<-npercfunc(included$elec)
icement<-npercfunc(included$cement)

iacresm<-round(mean(included$landacre, na.rm=TRUE), 2)
iacressd<-round(sd(included$landacre, na.rm=TRUE), 2)
iacres<-c(iacresm, iacressd)

itubewell<-npercfunc(included$tubewell)
istorewater<-npercfunc(included$storewat)
itreatwater<-npercfunc(included$treatwat)
iwaterdis<-meansdfunc(included$watmin)
iodmen<-npercfunc(included$odmen)
iodwomen<-npercfunc(included$odwom)
iodchild815<-npercfunc(included$odch815)
iodchild38<-npercfunc(included$odch38)
iodchild03<-npercfunc(included$odchu3)
ilatowned<-npercfunc(included$latown)
ilatslab<-npercfunc(included$latslab)
ilatseal<-npercfunc(included$latseal)
ilatfeces<-npercfunc(included$latfeces)
ipotty<-npercfunc(included$potty)
ifeceshouse<-npercfunc(included$humfeces)
ifeceschildarea<-npercfunc(included$humfecesch)
ihandlatwater<-npercfunc(included$hwlatwat)
ihandlatsoap<-npercfunc(included$hwlatsoap)
ihandkitwater<-npercfunc(included$hwkitwat)
ihandkitsoap<-npercfunc(included$hwkitsoap)

ifsn<-length(included$hfiacat[included$hfiacat=="Food Secure"])
ifsperc<-round(ifsn/length(included$hfiacat)*100)
ifoodsecure<-c(ifsn, ifsperc)


lmomage<-meansdfunc(lost$momage)
lmomeduy<-meansdfunc(lost$momeduy)
ldadeduy<-meansdfunc(lost$dadeduy)
ldadagri<-npercfunc(lost$dadagri)
lNhh<-meansdfunc(lost$Nhh)
lelec<-npercfunc(lost$elec)
lcement<-npercfunc(lost$cement)

lacresm<-round(mean(lost$landacre, na.rm=TRUE), 2)
lacressd<-round(sd(lost$landacre, na.rm=TRUE), 2)
lacres<-c(lacresm, lacressd)

ltubewell<-npercfunc(lost$tubewell)
lstorewater<-npercfunc(lost$storewat)
ltreatwater<-npercfunc(lost$treatwat)
lwaterdis<-meansdfunc(lost$watmin)
lodmen<-npercfunc(lost$odmen)
lodwomen<-npercfunc(lost$odwom)
lodchild815<-npercfunc(lost$odch815)
lodchild38<-npercfunc(lost$odch38)
lodchild03<-npercfunc(lost$odchu3)
llatowned<-npercfunc(lost$latown)
llatslab<-npercfunc(lost$latslab)
llatseal<-npercfunc(lost$latseal)
llatfeces<-npercfunc(lost$latfeces)
lpotty<-npercfunc(lost$potty)
lfeceshouse<-npercfunc(lost$humfeces)
lfeceschildarea<-npercfunc(lost$humfecesch)
lhandlatwater<-npercfunc(lost$hwlatwat)
lhandlatsoap<-npercfunc(lost$hwlatsoap)
lhandkitwater<-npercfunc(lost$hwkitwat)
lhandkitsoap<-npercfunc(lost$hwkitsoap)

lfsn<-length(lost$hfiacat[lost$hfiacat=="Food Secure"])
lfsperc<-round(lfsn/length(lost$hfiacat)*100)
lfoodsecure<-c(lfsn, lfsperc)


#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

includedcol<-c(" ", charobject(imomage, 1),charobject(imomeduy, 1), " ", charobject(idadeduy, 1), charobjectperc(idadagri, 1),
               " ", charobject(iNhh, 1), charobjectperc(ielec, 1), charobjectperc(icement, 1), charobject(iacres, 1),
               " ", charobjectperc(itubewell, 1), charobjectperc(istorewater, 1), charobjectperc(itreatwater, 1), charobject(iwaterdis, 1), 
               " ", " ", charobjectperc(iodmen, 1), charobjectperc(iodwomen, 1), charobjectperc(iodchild815, 1), charobjectperc(iodchild38, 1), charobjectperc(iodchild03, 1), 
               " ", charobjectperc(ilatowned, 1), charobjectperc(ilatslab, 1), charobjectperc(ilatseal, 1), charobjectperc(ilatfeces, 1),
               charobjectperc(ipotty, 1), 
               " ", charobjectperc(ifeceshouse, 1), charobjectperc(ifeceschildarea, 1), 
               " ", " ", charobjectperc(ihandlatwater, 1), charobjectperc(ihandlatsoap, 1), 
               " ", charobjectperc(ihandkitwater, 1), charobjectperc(ihandkitsoap, 1), 
               " ", charobjectperc(ifoodsecure, 1))
lostcol<-c(" ", charobject(lmomage, 1),charobject(lmomeduy, 1), " ", charobject(ldadeduy, 1), charobjectperc(ldadagri, 1),
           " ", charobject(lNhh, 1), charobjectperc(lelec, 1), charobjectperc(lcement, 1), charobject(lacres, 1),
           " ", charobjectperc(ltubewell, 1), charobjectperc(lstorewater, 1), charobjectperc(ltreatwater, 1), charobject(lwaterdis, 1), 
           " ", " ", charobjectperc(lodmen, 1), charobjectperc(lodwomen, 1), charobjectperc(lodchild815, 1), charobjectperc(lodchild38, 1), charobjectperc(lodchild03, 1), 
           " ", charobjectperc(llatowned, 1), charobjectperc(llatslab, 1), charobjectperc(llatseal, 1), charobjectperc(llatfeces, 1),
           charobjectperc(lpotty, 1), 
           " ", charobjectperc(lfeceshouse, 1), charobjectperc(lfeceschildarea, 1), 
           " ", " ", charobjectperc(lhandlatwater, 1), charobjectperc(lhandlatsoap, 1), 
           " ", charobjectperc(lhandkitwater, 1), charobjectperc(lhandkitsoap, 1), 
           " ", charobjectperc(lfoodsecure, 1))

# Table 1: Enrollment characteristics by intervention group
tbls2 <- data.table(
  "No. of compounds:" = c("Maternal", "Age(years)", "Years of education", 
                          "Paternal", "Years of education", "Works in agriculture", 
                          "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
                          "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
                          "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
                          "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
                          "Owned a child potty",
                          "Human feces observed in the", "House", "Child's play area",
                          "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
                          "Nutrition", "Household is food secure"),
  "Included (N=329)" = includedcol,
  "Lost to follow-up at Year 2 (N=96)" = lostcol
)

write.csv(tbls2, file=here('tables/miso9-immune-supptable2.csv'))
print(xtable(tbls2), type="html", file=here("tables/miso9-immune-supptable2.html"))



#### TABLE S3 ####

writeqntle<-function(vector) {
  quantiles<-round(quantile(vector, na.rm=TRUE), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

outcomes3<-c("Outcome", "IL-1β (pg/ml)", "Il-6 (pg/ml)", "TNF-α (pg/ml)", "CRP (mg/L)", "IL-12 (pg/ml)",
           "IFN-γ (pg/ml)", "IL-4 (pg/ml)", "IL-5 (pg/ml)", "IL-13 (pg/ml)", "IL-17A (pg/ml)", 
           "IL-21 (pg/ml)", "IL-10 (pg/ml)", "IL-2 (pg/ml)", "GMCSF (pg/ml)", "AGP (g/L)", "IGF-1 (μg/L)")

t2<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t2), writeqntle(lab$il6_t2), writeqntle(lab$tnfa_t2), writeqntle(lab$crp_t2), writeqntle(lab$il12_t2), writeqntle(lab$ifng_t2), 
      writeqntle(lab$il4_t2), writeqntle(lab$il5_t2), writeqntle(lab$il13_t2), writeqntle(lab$il17_t2), writeqntle(lab$il21_t2), writeqntle(lab$il10_t2), writeqntle(lab$il2_t2), 
      writeqntle(lab$gmcsf_t2), writeqntle(lab$agp_t2), writeqntle(lab$igf_t2))

t3<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t3), writeqntle(lab$il6_t3), writeqntle(lab$tnfa_t3), " ", writeqntle(lab$il12_t3), writeqntle(lab$ifng_t3), 
      writeqntle(lab$il4_t3), writeqntle(lab$il5_t3), writeqntle(lab$il13_t3), writeqntle(lab$il17_t3), writeqntle(lab$il21_t3), writeqntle(lab$il10_t3), writeqntle(lab$il2_t3), 
      writeqntle(lab$gmcsf_t3), " ", writeqntle(lab$igf_t3))

tbls3<-data.table(" "=outcomes3,
                  "Child Age 14 Months"=t2,
                  "Child Age 28 Months"=t3)

write.csv(tbls3, file=here('tables/miso9-immune-supptable3.csv'))
print(xtable(tbls3), type="html", file=here("tables/miso9-immune-supptable3.html"))



#### TABLE S4 ####

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

write.csv(tbls4, file=here('tables/miso9-immune-supptable4.csv'))
print(xtable(tbls4), type="html", file=here("tables/miso9-immune-supptable4.html"))


#### TABLE S5 ####

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

write.csv(tbls5, file=here('tables/miso9-immune-supptable5.csv'))
print(xtable(tbls5), type="html", file=here("tables/miso9-immune-supptable5.html"))


#### TABLE S6 ####

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
names(tbls6)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls6, file=here('tables/miso9-immune-supptable6.csv'))
print(xtable(tbls6), type="html", file=here("tables/miso9-immune-supptable6.html"))


#### TABLE S7 ####

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

write.csv(tbls7, file=here('tables/miso9-immune-supptable7.csv'))
print(xtable(tbls7), type="html", file=here("tables/miso9-immune-supptable7.html"))



#### TABLE S8 ####

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

write.csv(tbls8, file=here('tables/miso9-immune-supptable8.csv'))
print(xtable(tbls8), type="html", file=here("tables/miso9-immune-supptable8.html"))



#### TABLE S9 ####

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
             "Ln ΔPro-inflammatory cytokines*/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh1**/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh2***/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh17****/IL-10", "Control", "Nutrition + WSH",
             "Ln ΔTh1**/Th2***", "Control", "Nutrition + WSH",
             "Ln ΔTh1**/Th17****", "Control", "Nutrition + WSH")

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

write.csv(tbls9, file=here('tables/miso9-immune-supptable9.csv'))
print(xtable(tbls9), type="html", file=here("tables/miso9-immune-supptable9.html"))


