rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/"))

ages<-read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv", stringsAsFactors = TRUE)
load(here('audrie results/immune_N_tr_means.RData'))
load(here('audrie results/immune_unadj_glm.RData'))
load(here('audrie results/immune_adj_sex_age_glm.RData'))
load(here('audrie results/immune_adj_glm.RData'))

#### TABLE 1 ####
filtering <- function(row){
  any(!is.na(row))
}

y1<-ages[apply(select(ages, grep("t2_ln", names(ages), ignore.case=T)), 1, filtering),]

y2<-ages[apply(select(ages, grep("t3_ln", names(ages), ignore.case=T)), 1, filtering),]


#calculating overall N by arm
y1Nctrl<-length(y1$tr[y1$tr=="Control"])
y1Nwsh<-length(y1$tr[y1$tr=="Nutrition + WSH"])
y2Nctrl<-length(y2$tr[y2$tr=="Control"])
y2Nwsh<-length(y2$tr[y2$tr=="Nutrition + WSH"])

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(variable) {
  ctrlmean<-round(mean(variable[ages$tr=="Control"], na.rm=TRUE))
  ctrlsd<-round(sd(variable[ages$tr=="Control"], na.rm=TRUE))
  wshmean<-round(mean(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE))
  wshsd<-round(sd(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE))
  c(ctrlmean, ctrlsd, wshmean, wshsd)
}

npercfunc <- function(variable) {
  ctrln<-sum(variable[ages$tr=="Control"], na.rm=TRUE)
  ctrlperc<-round(mean(variable[ages$tr=="Control"], na.rm=TRUE)*100)
  wshn<-sum(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE)
  wshperc<-round(mean(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE)*100)
  c(ctrln, ctrlperc, wshn, wshperc)
}

y1momage<-meansdfunc(y1$momage)
y1momeduy<-meansdfunc(y1$momeduy)
y1dadeduy<-meansdfunc(y1$dadeduy)
y1dadagri<-npercfunc(y1$dadagri)
y1Nhh<-meansdfunc(y1$Nhh)
y1elec<-npercfunc(y1$elec)
y1cement<-npercfunc(y1$cement)

y2momage<-meansdfunc(y2$momage)
y2momeduy<-meansdfunc(y2$momeduy)
y2dadeduy<-meansdfunc(y2$dadeduy)
y2dadagri<-npercfunc(y2$dadagri)
y2Nhh<-meansdfunc(y2$Nhh)
y2elec<-npercfunc(y2$elec)
y2cement<-npercfunc(y2$cement)

y1acresctrlm<-round(mean(y1$landacre[y1$tr=="Control"], na.rm=TRUE), 2)
y1acresctrlsd<-round(sd(y1$landacre[y1$tr=="Control"], na.rm=TRUE), 2)
y1acreswshm<-round(mean(y1$landacre[y1$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
y1acreswshsd<-round(mean(y1$landacre[y1$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
y1acres<-c(y1acresctrlm, y1acresctrlsd, y1acreswshm, y1acreswshsd)

y2acresctrlm<-round(mean(y2$landacre[y2$tr=="Control"], na.rm=TRUE), 2)
y2acresctrlsd<-round(sd(y2$landacre[y2$tr=="Control"], na.rm=TRUE), 2)
y2acreswshm<-round(mean(y2$landacre[y2$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
y2acreswshsd<-round(mean(y2$landacre[y2$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
y2acres<-c(y2acresctrlm, y2acresctrlsd, y2acreswshm, y2acreswshsd)

y1tubewell<-npercfunc(y1$tubewell)
y1storewater<-npercfunc(y1$storewat)
y1treatwater<-npercfunc(y1$treatwat)
y1waterdis<-meansdfunc(y1$watmin)
y1odmen<-npercfunc(y1$odmen)
y1odwomen<-npercfunc(y1$odwom)
y1odchild815<-npercfunc(y1$odch815)
y1odchild38<-npercfunc(y1$odch38)
y1odchild03<-npercfunc(y1$odchu3)
y1latowned<-npercfunc(y1$latown)
y1latslab<-npercfunc(y1$latslab)
y1latseal<-npercfunc(y1$latseal)
y1latfeces<-npercfunc(y1$latfeces)
y1potty<-npercfunc(y1$potty)
y1feceshouse<-npercfunc(y1$humfeces)
y1feceschildarea<-npercfunc(y1$humfecesch)
y1handlatwater<-npercfunc(y1$hwlatwat)
y1handlatsoap<-npercfunc(y1$hwlatsoap)
y1handkitwater<-npercfunc(y1$hwkitwat)
y1handkitsoap<-npercfunc(y1$hwkitsoap)

y2tubewell<-npercfunc(y2$tubewell)
y2storewater<-npercfunc(y2$storewat)
y2treatwater<-npercfunc(y2$treatwat)
y2waterdis<-meansdfunc(y2$watmin)
y2odmen<-npercfunc(y2$odmen)
y2odwomen<-npercfunc(y2$odwom)
y2odchild815<-npercfunc(y2$odch815)
y2odchild38<-npercfunc(y2$odch38)
y2odchild03<-npercfunc(y2$odchu3)
y2latowned<-npercfunc(y2$latown)
y2latslab<-npercfunc(y2$latslab)
y2latseal<-npercfunc(y2$latseal)
y2latfeces<-npercfunc(y2$latfeces)
y2potty<-npercfunc(y2$potty)
y2feceshouse<-npercfunc(y2$humfeces)
y2feceschildarea<-npercfunc(y2$humfecesch)
y2handlatwater<-npercfunc(y2$hwlatwat)
y2handlatsoap<-npercfunc(y2$hwlatsoap)
y2handkitwater<-npercfunc(y2$hwkitwat)
y2handkitsoap<-npercfunc(y2$hwkitsoap)

y1fsctrln<-length(y1$hfiacat[y1$tr=="Control" & y1$hfiacat=="Food Secure"])
y1fsctrlperc<-round(y1fsctrln/length(y1$hfiacat[y1$tr=="Control"])*100)
y1fswshn<-length(y1$hfiacat[y1$tr=="Nutrition + WSH" & y1$hfiacat=="Food Secure"])
y1fswshperc<-round(y1fswshn/length(y1$hfiacat[y1$tr=="Nutrition + WSH"])*100)
y1foodsecure<-c(y1fsctrln, y1fsctrlperc, y1fswshn, y1fswshperc)

y2fsctrln<-length(y2$hfiacat[y2$tr=="Control" & y2$hfiacat=="Food Secure"])
y2fsctrlperc<-round(y2fsctrln/length(y2$hfiacat[y2$tr=="Control"])*100)
y2fswshn<-length(y2$hfiacat[y2$tr=="Nutrition + WSH" & y2$hfiacat=="Food Secure"])
y2fswshperc<-round(y2fswshn/length(y2$hfiacat[y2$tr=="Nutrition + WSH"])*100)
y2foodsecure<-c(y2fsctrln, y2fsctrlperc, y2fswshn, y2fswshperc)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

ctrly1<-c(paste("Control (N=", y1Nctrl, ")", sep=""), " ", charobject(y1momage, 1),charobject(y1momeduy, 1), " ", charobject(y1dadeduy, 1), charobjectperc(y1dadagri, 1),
        " ", charobject(y1Nhh, 1), charobjectperc(y1elec, 1), charobjectperc(y1cement, 1), charobject(y1acres, 1),
        " ", charobjectperc(y1tubewell, 1), charobjectperc(y1storewater, 1), charobjectperc(y1treatwater, 1), charobject(y1waterdis, 1), 
        " ", " ", charobjectperc(y1odmen, 1), charobjectperc(y1odwomen, 1), charobjectperc(y1odchild815, 1), charobjectperc(y1odchild38, 1), charobjectperc(y1odchild03, 1), 
        " ", charobjectperc(y1latowned, 1), charobjectperc(y1latslab, 1), charobjectperc(y1latseal, 1), charobjectperc(y1latfeces, 1),
        charobjectperc(y1potty, 1), 
        " ", charobjectperc(y1feceshouse, 1), charobjectperc(y1feceschildarea, 1), 
        " ", " ", charobjectperc(y1handlatwater, 1), charobjectperc(y1handlatsoap, 1), 
        " ", charobjectperc(y1handkitwater, 1), charobjectperc(y1handkitsoap, 1), 
        " ", charobjectperc(y1foodsecure, 1))

wshy1<-c(paste("N+WSH Intervention (N=", y1Nwsh, ")", sep=""), " ", charobject(y1momage, 3),charobject(y1momeduy, 3), " ", charobject(y1dadeduy, 3), charobjectperc(y1dadagri, 3),
       " ", charobject(y1Nhh, 3), charobjectperc(y1elec, 3), charobjectperc(y1cement, 3), charobject(y1acres, 3),
       " ", charobjectperc(y1tubewell, 3), charobjectperc(y1storewater, 3), charobjectperc(y1treatwater, 3), charobject(y1waterdis, 3), 
       " ", " ", charobjectperc(y1odmen, 3), charobjectperc(y1odwomen, 3), charobjectperc(y1odchild815, 3), charobjectperc(y1odchild38, 3), charobjectperc(y1odchild03, 3), 
       " ", charobjectperc(y1latowned, 3), charobjectperc(y1latslab, 3), charobjectperc(y1latseal, 3), charobjectperc(y1latfeces, 3),
       charobjectperc(y1potty, 3), 
       " ", charobjectperc(y1feceshouse, 3), charobjectperc(y1feceschildarea, 3), 
       " ", " ", charobjectperc(y1handlatwater, 3), charobjectperc(y1handlatsoap, 3), 
       " ", charobjectperc(y1handkitwater, 3), charobjectperc(y1handkitsoap, 3), 
       " ", charobjectperc(y1foodsecure, 3))

ctrly2<-c(paste("Control (N=", y2Nctrl, ")", sep=""), " ", charobject(y2momage, 1),charobject(y2momeduy, 1), " ", charobject(y2dadeduy, 1), charobjectperc(y2dadagri, 1),
          " ", charobject(y2Nhh, 1), charobjectperc(y2elec, 1), charobjectperc(y2cement, 1), charobject(y2acres, 1),
          " ", charobjectperc(y2tubewell, 1), charobjectperc(y2storewater, 1), charobjectperc(y2treatwater, 1), charobject(y2waterdis, 1), 
          " ", " ", charobjectperc(y2odmen, 1), charobjectperc(y2odwomen, 1), charobjectperc(y2odchild815, 1), charobjectperc(y2odchild38, 1), charobjectperc(y2odchild03, 1), 
          " ", charobjectperc(y2latowned, 1), charobjectperc(y2latslab, 1), charobjectperc(y2latseal, 1), charobjectperc(y2latfeces, 1),
          charobjectperc(y2potty, 1), 
          " ", charobjectperc(y2feceshouse, 1), charobjectperc(y2feceschildarea, 1), 
          " ", " ", charobjectperc(y2handlatwater, 1), charobjectperc(y2handlatsoap, 1), 
          " ", charobjectperc(y2handkitwater, 1), charobjectperc(y2handkitsoap, 1), 
          " ", charobjectperc(y2foodsecure, 1))
  
wshy2<-c(paste("N+WSH Intervention (N=", y2Nwsh, ")", sep=""), " ", charobject(y2momage, 3),charobject(y2momeduy, 3), " ", charobject(y2dadeduy, 3), charobjectperc(y2dadagri, 3),
         " ", charobject(y2Nhh, 3), charobjectperc(y2elec, 3), charobjectperc(y2cement, 3), charobject(y2acres, 3),
         " ", charobjectperc(y2tubewell, 3), charobjectperc(y2storewater, 3), charobjectperc(y2treatwater, 3), charobject(y2waterdis, 3), 
         " ", " ", charobjectperc(y2odmen, 3), charobjectperc(y2odwomen, 3), charobjectperc(y2odchild815, 3), charobjectperc(y2odchild38, 3), charobjectperc(y2odchild03, 3), 
         " ", charobjectperc(y2latowned, 3), charobjectperc(y2latslab, 3), charobjectperc(y2latseal, 3), charobjectperc(y2latfeces, 3),
         charobjectperc(y2potty, 3), 
         " ", charobjectperc(y2feceshouse, 3), charobjectperc(y2feceschildarea, 3), 
         " ", " ", charobjectperc(y2handlatwater, 3), charobjectperc(y2handlatsoap, 3), 
         " ", charobjectperc(y2handkitwater, 3), charobjectperc(y2handkitsoap, 3), 
         " ", charobjectperc(y2foodsecure, 3))

# Table 1: Enrollment characteristics by intervention group
tbl1 <- data.table(
  " " = c("No. of compounds:", "Maternal", "Age(years)", "Years of education", 
          "Paternal", "Years of education", "Works in agriculture", 
          "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
          "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
          "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
          "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
          "Owned a child potty",
          "Human feces observed in the", "House", "Child's play area",
          "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
          "Nutrition", "Household is food secure"),
  "Children measured at Year 1" = ctrly1,
  " " = wshy1,
  "Children measured at Year 2" = ctrly2,
  " " = wshy2
)

write.csv(tbl1, file=here('tables/immune/immune_main/immune_table1.csv'))
print(xtable(tbl1), type="html", file=here("tables/immune/immune_main/immune_table1.html"))


#### TABLE 2 ####
outcometbl2 <- c(paste("Ln IL-1", "Î²", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "Î±", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln CRP (mg/L)", "Control", "Nutrition + WSH", 
                 "Ln IL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln AGP (g/L)", "Control", "Nutrition + WSH",
                 paste("Ln IGF-1 (", "Î¼", "g/L)", sep=""), "Control", "Nutrition + WSH")

Ntbl2 <- c(" ", as.character(il1_t2_N_tr$t2_ln_il1_N_tr[1]), as.character(il1_t2_N_tr$t2_ln_il1_N_tr[2]),
           " ", as.character(il6_t2_N_tr$t2_ln_il6_N_tr[1]), as.character(il6_t2_N_tr$t2_ln_il6_N_tr[2]),
           " ", as.character(tnf_t2_N_tr$t2_ln_tnf_N_tr[1]), as.character(tnf_t2_N_tr$t2_ln_tnf_N_tr[2]),
           " ", as.character(crp_t2_N_tr$t2_ln_crp_N_tr[1]), as.character(crp_t2_N_tr$t2_ln_crp_N_tr[2]),
           " ", as.character(il12_t2_N_tr$t2_ln_il12_N_tr[1]), as.character(il12_t2_N_tr$t2_ln_il12_N_tr[2]),
           " ", as.character(ifn_t2_N_tr$t2_ln_ifn_N_tr[1]), as.character(ifn_t2_N_tr$t2_ln_ifn_N_tr[2]),
           " ", as.character(il4_t2_N_tr$t2_ln_il4_N_tr[1]), as.character(il4_t2_N_tr$t2_ln_il4_N_tr[2]),
           " ", as.character(il5_t2_N_tr$t2_ln_il5_N_tr[1]), as.character(il5_t2_N_tr$t2_ln_il5_N_tr[2]),
           " ", as.character(il13_t2_N_tr$t2_ln_il13_N_tr[1]), as.character(il13_t2_N_tr$t2_ln_il13_N_tr[2]),
           " ", as.character(il17_t2_N_tr$t2_ln_il17_N_tr[1]), as.character(il17_t2_N_tr$t2_ln_il17_N_tr[2]),
           " ", as.character(il21_t2_N_tr$t2_ln_il21_N_tr[1]), as.character(il21_t2_N_tr$t2_ln_il21_N_tr[2]),
           " ", as.character(il10_t2_N_tr$t2_ln_il10_N_tr[1]), as.character(il10_t2_N_tr$t2_ln_il10_N_tr[2]),
           " ", as.character(il2_t2_N_tr$t2_ln_il2_N_tr[1]), as.character(il2_t2_N_tr$t2_ln_il2_N_tr[2]),
           " ", as.character(gmc_t2_N_tr$t2_ln_gmc_N_tr[1]), as.character(gmc_t2_N_tr$t2_ln_gmc_N_tr[2]),
           " ", as.character(agp_t2_N_tr$t2_ln_agp_N_tr[1]), as.character(agp_t2_N_tr$t2_ln_agp_N_tr[2]),
           " ", as.character(igf_t2_N_tr$t2_ln_igf_N_tr[1]), as.character(igf_t2_N_tr$t2_ln_igf_N_tr[2]))

abssdtbl2 <-c(" ", as.character(round(abs_il1_t2_N_tr$sd[1], 2)), as.character(round(abs_il1_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il6_t2_N_tr$sd[1], 2)), as.character(round(abs_il6_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_tnf_t2_N_tr$sd[1], 2)), as.character(round(abs_tnf_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_crp_t2_N_tr$sd[1], 2)), as.character(round(abs_crp_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il12_t2_N_tr$sd[1], 2)), as.character(round(abs_il12_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_ifn_t2_N_tr$sd[1], 2)), as.character(round(abs_ifn_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il4_t2_N_tr$sd[1], 2)), as.character(round(abs_il4_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il5_t2_N_tr$sd[1], 2)), as.character(round(abs_il5_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il13_t2_N_tr$sd[1], 2)), as.character(round(abs_il13_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il17_t2_N_tr$sd[1], 2)), as.character(round(abs_il17_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il21_t2_N_tr$sd[1], 2)), as.character(round(abs_il21_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il10_t2_N_tr$sd[1], 2)), as.character(round(abs_il10_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_il2_t2_N_tr$sd[1], 2)), as.character(round(abs_il2_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_gmc_t2_N_tr$sd[1], 2)), as.character(round(abs_gmc_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_agp_t2_N_tr$sd[1], 2)), as.character(round(abs_agp_t2_N_tr$sd[2], 2)),
                " ", as.character(round(abs_igf_t2_N_tr$sd[1], 2)), as.character(round(abs_igf_t2_N_tr$sd[2], 2)))

absmeantbl2 <- c(" ", as.character(round(abs_il1_t2_N_tr$mean[1], 2)), as.character(round(abs_il1_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il6_t2_N_tr$mean[1], 2)), as.character(round(abs_il6_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_tnf_t2_N_tr$mean[1], 2)), as.character(round(abs_tnf_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_crp_t2_N_tr$mean[1], 2)), as.character(round(abs_crp_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il12_t2_N_tr$mean[1], 2)), as.character(round(abs_il12_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_ifn_t2_N_tr$mean[1], 2)), as.character(round(abs_ifn_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il4_t2_N_tr$mean[1], 2)), as.character(round(abs_il4_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il5_t2_N_tr$mean[1], 2)), as.character(round(abs_il5_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il13_t2_N_tr$mean[1], 2)), as.character(round(abs_il13_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il17_t2_N_tr$mean[1], 2)), as.character(round(abs_il17_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il21_t2_N_tr$mean[1], 2)), as.character(round(abs_il21_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il10_t2_N_tr$mean[1], 2)), as.character(round(abs_il10_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_il2_t2_N_tr$mean[1], 2)), as.character(round(abs_il2_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_gmc_t2_N_tr$mean[1], 2)), as.character(round(abs_gmc_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_agp_t2_N_tr$mean[1], 2)), as.character(round(abs_agp_t2_N_tr$mean[2], 2)),
               " ", as.character(round(abs_igf_t2_N_tr$mean[1], 2)), as.character(round(abs_igf_t2_N_tr$mean[2], 2)))

meantbl2 <- c(" ", as.character(round(il1_t2_N_tr$mean[1], 2)), as.character(round(il1_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il6_t2_N_tr$mean[1], 2)), as.character(round(il6_t2_N_tr$mean[2], 2)),
              " ", as.character(round(tnf_t2_N_tr$mean[1], 2)), as.character(round(tnf_t2_N_tr$mean[2], 2)),
              " ", as.character(round(crp_t2_N_tr$mean[1], 2)), as.character(round(crp_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il12_t2_N_tr$mean[1], 2)), as.character(round(il12_t2_N_tr$mean[2], 2)),
              " ", as.character(round(ifn_t2_N_tr$mean[1], 2)), as.character(round(ifn_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il4_t2_N_tr$mean[1], 2)), as.character(round(il4_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il5_t2_N_tr$mean[1], 2)), as.character(round(il5_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il13_t2_N_tr$mean[1], 2)), as.character(round(il13_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il17_t2_N_tr$mean[1], 2)), as.character(round(il17_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il21_t2_N_tr$mean[1], 2)), as.character(round(il21_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il10_t2_N_tr$mean[1], 2)), as.character(round(il10_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il2_t2_N_tr$mean[1], 2)), as.character(round(il2_t2_N_tr$mean[2], 2)),
              " ", as.character(round(gmc_t2_N_tr$mean[1], 2)), as.character(round(gmc_t2_N_tr$mean[2], 2)),
              " ", as.character(round(agp_t2_N_tr$mean[1], 2)), as.character(round(agp_t2_N_tr$mean[2], 2)),
              " ", as.character(round(igf_t2_N_tr$mean[1], 2)), as.character(round(igf_t2_N_tr$mean[2], 2)))

sdtbl2 <- c(" ", as.character(round(il1_t2_N_tr$sd[1], 2)), as.character(round(il1_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il6_t2_N_tr$sd[1], 2)), as.character(round(il6_t2_N_tr$sd[2], 2)),
            " ", as.character(round(tnf_t2_N_tr$sd[1], 2)), as.character(round(tnf_t2_N_tr$sd[2], 2)),
            " ", as.character(round(crp_t2_N_tr$sd[1], 2)), as.character(round(crp_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il12_t2_N_tr$sd[1], 2)), as.character(round(il12_t2_N_tr$sd[2], 2)),
            " ", as.character(round(ifn_t2_N_tr$sd[1], 2)), as.character(round(ifn_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il4_t2_N_tr$sd[1], 2)), as.character(round(il4_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il5_t2_N_tr$sd[1], 2)), as.character(round(il5_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il13_t2_N_tr$sd[1], 2)), as.character(round(il13_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il17_t2_N_tr$sd[1], 2)), as.character(round(il17_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il21_t2_N_tr$sd[1], 2)), as.character(round(il21_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il10_t2_N_tr$sd[1], 2)), as.character(round(il10_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il2_t2_N_tr$sd[1], 2)), as.character(round(il2_t2_N_tr$sd[2], 2)),
            " ", as.character(round(gmc_t2_N_tr$sd[1], 2)), as.character(round(gmc_t2_N_tr$sd[2], 2)),
            " ", as.character(round(agp_t2_N_tr$sd[1], 2)), as.character(round(agp_t2_N_tr$sd[2], 2)),
            " ", as.character(round(igf_t2_N_tr$sd[1], 2)), as.character(round(igf_t2_N_tr$sd[2], 2)))

t2_agp_unadj_L <- round(t2_agp_unadj_L, 2)
t2_crp_unadj_L <- round(t2_crp_unadj_L, 2)
t2_gmc_unadj_L <- round(t2_gmc_unadj_L, 2)
t2_ifn_unadj_L <- round(t2_ifn_unadj_L, 2)
t2_igf_unadj_L <- round(t2_igf_unadj_L, 2)
t2_il1_unadj_L <- round(t2_il1_unadj_L, 2)
t2_il10_unadj_L <- round(t2_il10_unadj_L, 2)
t2_il12_unadj_L <- round(t2_il12_unadj_L, 2)
t2_il13_unadj_L <- round(t2_il13_unadj_L, 2)
t2_il17_unadj_L <- round(t2_il17_unadj_L, 2)
t2_il2_unadj_L <- round(t2_il2_unadj_L, 2)
t2_il21_unadj_L <- round(t2_il21_unadj_L, 2)
t2_il4_unadj_L <- round(t2_il4_unadj_L, 2)
t2_il5_unadj_L <- round(t2_il5_unadj_L, 2)
t2_il6_unadj_L <- round(t2_il6_unadj_L, 2)
t2_tnf_unadj_L <- round(t2_tnf_unadj_L, 2)

unadjtbl2 <- c(" ", " ", paste(t2_il1_unadj_L[1], " (", t2_il1_unadj_L[2], ", ", t2_il1_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il6_unadj_L[1], " (", t2_il6_unadj_L[2], ", ", t2_il6_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_tnf_unadj_L[1], " (", t2_tnf_unadj_L[2], ", ", t2_tnf_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_crp_unadj_L[1], " (", t2_crp_unadj_L[2], ", ", t2_crp_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il12_unadj_L[1], " (", t2_il12_unadj_L[2], ", ", t2_il12_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ifn_unadj_L[1], " (", t2_ifn_unadj_L[2], ", ", t2_ifn_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il4_unadj_L[1], " (", t2_il4_unadj_L[2], ", ", t2_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il5_unadj_L[1], " (", t2_il5_unadj_L[2], ", ", t2_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il13_unadj_L[1], " (", t2_il13_unadj_L[2], ", ", t2_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il17_unadj_L[1], " (", t2_il17_unadj_L[2], ", ", t2_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il21_unadj_L[1], " (", t2_il21_unadj_L[2], ", ", t2_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il10_unadj_L[1], " (", t2_il10_unadj_L[2], ", ", t2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il2_unadj_L[1], " (", t2_il2_unadj_L[2], ", ", t2_il2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_gmc_unadj_L[1], " (", t2_gmc_unadj_L[2], ", ", t2_gmc_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_agp_unadj_L[1], " (", t2_agp_unadj_L[2], ", ", t2_agp_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_igf_unadj_L[1], " (", t2_igf_unadj_L[2], ", ", t2_igf_unadj_L[3], ")", sep="")) 

t2_agp_adj_sex_age_L <- round(t2_agp_adj_sex_age_L, 2)
t2_crp_adj_sex_age_L <- round(t2_crp_adj_sex_age_L, 2)
t2_gmc_adj_sex_age_L <- round(t2_gmc_adj_sex_age_L, 2)
t2_ifn_adj_sex_age_L <- round(t2_ifn_adj_sex_age_L, 2)
t2_igf_adj_sex_age_L <- round(t2_igf_adj_sex_age_L, 2)
t2_il1_adj_sex_age_L <- round(t2_il1_adj_sex_age_L, 2)
t2_il10_adj_sex_age_L <- round(t2_il10_adj_sex_age_L, 2)
t2_il12_adj_sex_age_L <- round(t2_il12_adj_sex_age_L, 2)
t2_il13_adj_sex_age_L <- round(t2_il13_adj_sex_age_L, 2)
t2_il17_adj_sex_age_L <- round(t2_il17_adj_sex_age_L, 2)
t2_il2_adj_sex_age_L <- round(t2_il2_adj_sex_age_L, 2)
t2_il21_adj_sex_age_L <- round(t2_il21_adj_sex_age_L, 2)
t2_il4_adj_sex_age_L <- round(t2_il4_adj_sex_age_L, 2)
t2_il5_adj_sex_age_L <- round(t2_il5_adj_sex_age_L, 2)
t2_il6_adj_sex_age_L <- round(t2_il6_adj_sex_age_L, 2)
t2_tnf_adj_sex_age_L <- round(t2_tnf_adj_sex_age_L, 2)

asadjtbl2 <- c(" ", " ", paste(t2_il1_adj_sex_age_L[1], " (", t2_il1_adj_sex_age_L[2], ", ", t2_il1_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il6_adj_sex_age_L[1], " (", t2_il6_adj_sex_age_L[2], ", ", t2_il6_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_tnf_adj_sex_age_L[1], " (", t2_tnf_adj_sex_age_L[2], ", ", t2_tnf_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_crp_adj_sex_age_L[1], " (", t2_crp_adj_sex_age_L[2], ", ", t2_crp_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il12_adj_sex_age_L[1], " (", t2_il12_adj_sex_age_L[2], ", ", t2_il12_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ifn_adj_sex_age_L[1], " (", t2_ifn_adj_sex_age_L[2], ", ", t2_ifn_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il4_adj_sex_age_L[1], " (", t2_il4_adj_sex_age_L[2], ", ", t2_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il5_adj_sex_age_L[1], " (", t2_il5_adj_sex_age_L[2], ", ", t2_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il13_adj_sex_age_L[1], " (", t2_il13_adj_sex_age_L[2], ", ", t2_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il17_adj_sex_age_L[1], " (", t2_il17_adj_sex_age_L[2], ", ", t2_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il21_adj_sex_age_L[1], " (", t2_il21_adj_sex_age_L[2], ", ", t2_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il10_adj_sex_age_L[1], " (", t2_il10_adj_sex_age_L[2], ", ", t2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il2_adj_sex_age_L[1], " (", t2_il2_adj_sex_age_L[2], ", ", t2_il2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_gmc_adj_sex_age_L[1], " (", t2_gmc_adj_sex_age_L[2], ", ", t2_gmc_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_agp_adj_sex_age_L[1], " (", t2_agp_adj_sex_age_L[2], ", ", t2_agp_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_igf_adj_sex_age_L[1], " (", t2_igf_adj_sex_age_L[2], ", ", t2_igf_adj_sex_age_L[3], ")", sep=""))  

t2_agp_adj_L <- round(t2_agp_adj_L, 2)
t2_crp_adj_L <- round(t2_crp_adj_L, 2)
t2_gmc_adj_L <- round(t2_gmc_adj_L, 2)
t2_ifn_adj_L <- round(t2_ifn_adj_L, 2)
t2_igf_adj_L <- round(t2_igf_adj_L, 2)
t2_il1_adj_L <- round(t2_il1_adj_L, 2)
t2_il10_adj_L <- round(t2_il10_adj_L, 2)
t2_il12_adj_L <- round(t2_il12_adj_L, 2)
t2_il13_adj_L <- round(t2_il13_adj_L, 2)
t2_il17_adj_L <- round(t2_il17_adj_L, 2)
t2_il2_adj_L <- round(t2_il2_adj_L, 2)
t2_il21_adj_L <- round(t2_il21_adj_L, 2)
t2_il4_adj_L <- round(t2_il4_adj_L, 2)
t2_il5_adj_L <- round(t2_il5_adj_L, 2)
t2_il6_adj_L <- round(t2_il6_adj_L, 2)
t2_tnf_adj_L <- round(t2_tnf_adj_L, 2)

adjtbl2 <- c(" ", " ", paste(t2_il1_adj_L[1], " (", t2_il1_adj_L[2], ", ", t2_il1_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il6_adj_L[1], " (", t2_il6_adj_L[2], ", ", t2_il6_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_tnf_adj_L[1], " (", t2_tnf_adj_L[2], ", ", t2_tnf_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_crp_adj_L[1], " (", t2_crp_adj_L[2], ", ", t2_crp_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il12_adj_L[1], " (", t2_il12_adj_L[2], ", ", t2_il12_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ifn_adj_L[1], " (", t2_ifn_adj_L[2], ", ", t2_ifn_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il4_adj_L[1], " (", t2_il4_adj_L[2], ", ", t2_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il5_adj_L[1], " (", t2_il5_adj_L[2], ", ", t2_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il13_adj_L[1], " (", t2_il13_adj_L[2], ", ", t2_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il17_adj_L[1], " (", t2_il17_adj_L[2], ", ", t2_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il21_adj_L[1], " (", t2_il21_adj_L[2], ", ", t2_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il10_adj_L[1], " (", t2_il10_adj_L[2], ", ", t2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il2_adj_L[1], " (", t2_il2_adj_L[2], ", ", t2_il2_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_gmc_adj_L[1], " (", t2_gmc_adj_L[2], ", ", t2_gmc_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_agp_adj_L[1], " (", t2_agp_adj_L[2], ", ", t2_agp_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_igf_adj_L[1], " (", t2_igf_adj_L[2], ", ", t2_igf_adj_L[3], ")", sep="")) 

# Table 2: Effect of intervention on individual immune status and growth factor measurements at age 14 months
tbl2 <- data.table(
  "Outcome, Arm" = outcometbl2,
  "N" = Ntbl2, 
  "Absolute Mean" = absmeantbl2,
  "Absolute SD" = abssdtbl2,
  "Mean" = meantbl2, 
  "SD" = sdtbl2,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl2,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl2, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl2)

write.csv(tbl2, file=here('tables/immune/immune_main/immune_table2.csv'))
print(xtable(tbl2), type="html", file=here("tables/immune/immune_main/immune_table2.html"))



#### TABLE 3 ####

outcometbl3 <- c(paste("Ln IL-1", "Î²", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "Î±", "/IL-10", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-5/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-13/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-17A/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-21/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-2/IL-10", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF/IL-10", "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-4", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-4", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-5", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-5", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-13", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-13", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-17A", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-17A", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-21", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-21", sep=""), "Control", "Nutrition + WSH",
                 "Ln Pro-inflammatory cytokines*/IL-10", "Control", "Nutrition + WSH",
                 "Ln Th1**/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th2***/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th17****/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th2***", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th17****", "Control", "Nutrition + WSH")

Ntbl3 <- c(" ", as.character(t2_ratio_il1_il10_N_tr$t2_ratio_il1_il10_N_tr[1]), as.character(t2_ratio_il1_il10_N_tr$t2_ratio_il1_il10_N_tr[2]), 
           " ", as.character(t2_ratio_il6_il10_N_tr$t2_ratio_il6_il10_N_tr[1]), as.character(t2_ratio_il6_il10_N_tr$t2_ratio_il6_il10_N_tr[2]),  
           " ", as.character(t2_ratio_tnf_il10_N_tr$t2_ratio_tnf_il10_N_tr[1]), as.character(t2_ratio_tnf_il10_N_tr$t2_ratio_tnf_il10_N_tr[2]), 
           " ", as.character(t2_ratio_il12_il10_N_tr$t2_ratio_il12_il10_N_tr[1]), as.character(t2_ratio_il12_il10_N_tr$t2_ratio_il12_il10_N_tr[2]), 
           " ", as.character(t2_ratio_ifn_il10_N_tr$t2_ratio_ifn_il10_N_tr[1]), as.character(t2_ratio_ifn_il10_N_tr$t2_ratio_ifn_il10_N_tr[2]), 
           " ", as.character(t2_ratio_il4_il10_N_tr$t2_ratio_il4_il10_N_tr[1]), as.character(t2_ratio_il4_il10_N_tr$t2_ratio_il4_il10_N_tr[2]), 
           " ", as.character(t2_ratio_il5_il10_N_tr$t2_ratio_il5_il10_N_tr[1]), as.character(t2_ratio_il5_il10_N_tr$t2_ratio_il5_il10_N_tr[2]),  
           " ", as.character(t2_ratio_il13_il10_N_tr$t2_ratio_il13_il10_N_tr[1]), as.character(t2_ratio_il13_il10_N_tr$t2_ratio_il13_il10_N_tr[2]),    
           " ", as.character(t2_ratio_il17_il10_N_tr$t2_ratio_il17_il10_N_tr[1]), as.character(t2_ratio_il17_il10_N_tr$t2_ratio_il17_il10_N_tr[2]),  
           " ", as.character(t2_ratio_il21_il10_N_tr$t2_ratio_il21_il10_N_tr[1]), as.character(t2_ratio_il21_il10_N_tr$t2_ratio_il21_il10_N_tr[2]),  
           " ", as.character(t2_ratio_il2_il10_N_tr$t2_ratio_il2_il10_N_tr[1]), as.character(t2_ratio_il2_il10_N_tr$t2_ratio_il2_il10_N_tr[2]),  
           " ", as.character(t2_ratio_gmc_il10_N_tr$t2_ratio_gmc_il10_N_tr[1]), as.character(t2_ratio_gmc_il10_N_tr$t2_ratio_gmc_il10_N_tr[2]),  
           " ", as.character(t2_ratio_il12_il4_N_tr$t2_ratio_il12_il4_N_tr[1]), as.character(t2_ratio_il12_il4_N_tr$t2_ratio_il12_il4_N_tr[2]),  
           " ", as.character(t2_ratio_ifn_il4_N_tr$t2_ratio_ifn_il4_N_tr[1]), as.character(t2_ratio_ifn_il4_N_tr$t2_ratio_ifn_il4_N_tr[2]),  
           " ", as.character(t2_ratio_il12_il5_N_tr$t2_ratio_il12_il5_N_tr[1]), as.character(t2_ratio_il12_il5_N_tr$t2_ratio_il12_il5_N_tr[2]),  
           " ", as.character(t2_ratio_ifn_il5_N_tr$t2_ratio_ifn_il5_N_tr[1]), as.character(t2_ratio_ifn_il5_N_tr$t2_ratio_ifn_il5_N_tr[2]), 
           " ", as.character(t2_ratio_il12_il13_N_tr$t2_ratio_il12_il13_N_tr[1]), as.character(t2_ratio_il12_il13_N_tr$t2_ratio_il12_il13_N_tr[2]), 
           " ", as.character(t2_ratio_ifn_il13_N_tr$t2_ratio_ifn_il13_N_tr[1]), as.character(t2_ratio_ifn_il13_N_tr$t2_ratio_ifn_il13_N_tr[2]), 
           " ", as.character(t2_ratio_il12_il17_N_tr$t2_ratio_il12_il17_N_tr[1]), as.character(t2_ratio_il12_il17_N_tr$t2_ratio_il12_il17_N_tr[2]),  
           " ", as.character(t2_ratio_ifn_il17_N_tr$t2_ratio_ifn_il17_N_tr[1]), as.character(t2_ratio_ifn_il17_N_tr$t2_ratio_ifn_il17_N_tr[2]),  
           " ", as.character(t2_ratio_il12_il21_N_tr$t2_ratio_il12_il21_N_tr[1]), as.character(t2_ratio_il12_il21_N_tr$t2_ratio_il12_il21_N_tr[2]), 
           " ", as.character(t2_ratio_ifn_il21_N_tr$t2_ratio_ifn_il21_N_tr[1]), as.character(t2_ratio_ifn_il21_N_tr$t2_ratio_ifn_il21_N_tr[2]),
           " ", as.character(t2_ratio_pro_il10_N_tr$t2_ratio_pro_il10_N_tr[1]), as.character(t2_ratio_pro_il10_N_tr$t2_ratio_pro_il10_N_tr[2]),  
           " ", as.character(t2_ratio_th1_il10_N_tr$t2_ratio_th1_il10_N_tr[1]), as.character(t2_ratio_th1_il10_N_tr$t2_ratio_th1_il10_N_tr[2]),  
           " ", as.character(t2_ratio_th2_il10_N_tr$t2_ratio_th2_il10_N_tr[1]), as.character(t2_ratio_th2_il10_N_tr$t2_ratio_th2_il10_N_tr[2]),    
           " ", as.character(t2_ratio_th17_il10_N_tr$t2_ratio_th17_il10_N_tr[1]), as.character(t2_ratio_th17_il10_N_tr$t2_ratio_th17_il10_N_tr[2]), 
           " ", as.character(t2_ratio_th1_th2_N_tr$t2_ratio_th1_th2_N_tr[1]), as.character(t2_ratio_th1_th2_N_tr$t2_ratio_th1_th2_N_tr[2]),  
           " ", as.character(t2_ratio_th1_th17_N_tr$t2_ratio_th1_th17_N_tr[1]), as.character(t2_ratio_th1_th17_N_tr$t2_ratio_th1_th17_N_tr[2]))

absmeantbl3 <- c(" ", as.character(round(abs_t2_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il1_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il6_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_tnf_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il4_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il5_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il13_il10_N_tr$mean[2], 2)),    
                 " ", as.character(round(abs_t2_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il17_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il21_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il2_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_gmc_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il4_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il4_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il5_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il5_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il13_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il13_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il17_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il17_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_il12_il21_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_ifn_il21_N_tr$mean[2], 2)),
                 " ", " ", " ",  
                 " ", " ", " ",  
                 " ", " ", " ",    
                 " ", " ", " ", 
                 " ", " ", " ",  
                 " ", " ", " ")

abssdtbl3 <- c(" ", as.character(round(abs_t2_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il1_il10_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il6_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_tnf_il10_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il10_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il10_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il4_il10_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il5_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il13_il10_N_tr$sd[2], 2)),    
               " ", as.character(round(abs_t2_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il17_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il21_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il2_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_gmc_il10_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il4_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il4_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il5_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il5_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il13_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il13_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il17_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il17_N_tr$sd[2], 2)),  
               " ", as.character(round(abs_t2_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_il12_il21_N_tr$sd[2], 2)), 
               " ", as.character(round(abs_t2_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(abs_t2_ratio_ifn_il21_N_tr$sd[2], 2)),
               " ", " ", " ",  
               " ", " ", " ",  
               " ", " ", " ",    
               " ", " ", " ", 
               " ", " ", " ",  
               " ", " ", " ")

meantbl3 <- c(" ", as.character(round(t2_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il1_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il6_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_tnf_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il4_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il5_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il13_il10_N_tr$mean[2], 2)),    
              " ", as.character(round(t2_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il17_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il21_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_il2_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_gmc_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il4_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il4_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il5_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il5_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il13_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il13_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il17_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il17_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(t2_ratio_il12_il21_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(t2_ratio_ifn_il21_N_tr$mean[2], 2)),
              " ", as.character(round(t2_ratio_pro_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_pro_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_th1_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_th1_il10_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_th2_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_th2_il10_N_tr$mean[2], 2)),    
              " ", as.character(round(t2_ratio_th17_il10_N_tr$mean[1], 2)), as.character(round(t2_ratio_th17_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t2_ratio_th1_th2_N_tr$mean[1], 2)), as.character(round(t2_ratio_th1_th2_N_tr$mean[2], 2)),  
              " ", as.character(round(t2_ratio_th1_th17_N_tr$mean[1], 2)),  as.character(round(t2_ratio_th1_th17_N_tr$mean[2], 2)))

sdtbl3 <- c(" ", as.character(round(t2_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il1_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il6_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_tnf_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il4_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il5_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il13_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il17_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il21_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_il2_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_gmc_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il4_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il4_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il5_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il5_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il13_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il13_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il17_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il17_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(t2_ratio_il12_il21_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(t2_ratio_ifn_il21_N_tr$sd[2], 2)),
            " ", as.character(round(t2_ratio_pro_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_pro_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_th1_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_th1_il10_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_th2_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_th2_il10_N_tr$sd[2], 2)),    
            " ", as.character(round(t2_ratio_th17_il10_N_tr$sd[1], 2)), as.character(round(t2_ratio_th17_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t2_ratio_th1_th2_N_tr$sd[1], 2)), as.character(round(t2_ratio_th1_th2_N_tr$sd[2], 2)),  
            " ", as.character(round(t2_ratio_th1_th17_N_tr$sd[1], 2)),  as.character(round(t2_ratio_th1_th17_N_tr$sd[2], 2)))

t2_ratio_il1_il10_unadj_L <- round(t2_ratio_il1_il10_unadj_L, 2)
t2_ratio_il6_il10_unadj_L <- round(t2_ratio_il6_il10_unadj_L, 2)
t2_ratio_tnf_il10_unadj_L <- round(t2_ratio_tnf_il10_unadj_L, 2)
t2_ratio_il12_il10_unadj_L <- round(t2_ratio_il12_il10_unadj_L, 2)
t2_ratio_ifn_il10_unadj_L <- round(t2_ratio_ifn_il10_unadj_L, 2)
t2_ratio_il4_il10_unadj_L <- round(t2_ratio_il4_il10_unadj_L, 2)
t2_ratio_il5_il10_unadj_L <- round(t2_ratio_il5_il10_unadj_L, 2)
t2_ratio_il13_il10_unadj_L <- round(t2_ratio_il13_il10_unadj_L, 2)
t2_ratio_il17_il10_unadj_L <- round(t2_ratio_il17_il10_unadj_L, 2)
t2_ratio_il21_il10_unadj_L <- round(t2_ratio_il21_il10_unadj_L, 2)
t2_ratio_il2_il10_unadj_L <- round(t2_ratio_il2_il10_unadj_L, 2)
t2_ratio_gmc_il10_unadj_L <- round(t2_ratio_gmc_il10_unadj_L, 2)
t2_ratio_il12_il4_unadj_L <- round(t2_ratio_il12_il4_unadj_L, 2)
t2_ratio_ifn_il4_unadj_L <- round(t2_ratio_ifn_il4_unadj_L, 2)
t2_ratio_il12_il5_unadj_L <- round(t2_ratio_il12_il5_unadj_L, 2)
t2_ratio_ifn_il5_unadj_L <- round(t2_ratio_ifn_il5_unadj_L, 2)
t2_ratio_il12_il13_unadj_L <- round(t2_ratio_il12_il13_unadj_L, 2)
t2_ratio_ifn_il13_unadj_L <- round(t2_ratio_ifn_il13_unadj_L, 2)
t2_ratio_il12_il17_unadj_L <- round(t2_ratio_il12_il17_unadj_L, 2)
t2_ratio_ifn_il17_unadj_L <- round(t2_ratio_ifn_il17_unadj_L, 2)
t2_ratio_il12_il21_unadj_L <- round(t2_ratio_il12_il21_unadj_L, 2)
t2_ratio_ifn_il21_unadj_L <- round(t2_ratio_ifn_il21_unadj_L, 2)
t2_ratio_pro_il10_unadj_L <- round(t2_ratio_pro_il10_unadj_L, 2)
t2_ratio_th1_il10_unadj_L <- round(t2_ratio_th1_il10_unadj_L, 2)
t2_ratio_th2_il10_unadj_L <- round(t2_ratio_th2_il10_unadj_L, 2)
t2_ratio_th17_il10_unadj_L <- round(t2_ratio_th17_il10_unadj_L, 2)
t2_ratio_th1_th2_unadj_L <- round(t2_ratio_th1_th2_unadj_L, 2)
t2_ratio_th1_th17_unadj_L <- round(t2_ratio_th1_th17_unadj_L, 2)

unadjtbl3 <- c(" ", " ", paste(t2_ratio_il1_il10_unadj_L[1], " (", t2_ratio_il1_il10_unadj_L[2], ", ", t2_ratio_il1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il6_il10_unadj_L[1], " (", t2_ratio_il6_il10_unadj_L[2], ", ", t2_ratio_il6_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_tnf_il10_unadj_L[1], " (", t2_ratio_tnf_il10_unadj_L[2], ", ", t2_ratio_tnf_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il10_unadj_L[1], " (", t2_ratio_il12_il10_unadj_L[2], ", ", t2_ratio_il12_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il10_unadj_L[1], " (", t2_ratio_ifn_il10_unadj_L[2], ", ", t2_ratio_ifn_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il4_il10_unadj_L[1], " (", t2_ratio_il4_il10_unadj_L[2], ", ", t2_ratio_il4_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il5_il10_unadj_L[1], " (", t2_ratio_il5_il10_unadj_L[2], ", ", t2_ratio_il5_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il13_il10_unadj_L[1], " (", t2_ratio_il13_il10_unadj_L[2], ", ", t2_ratio_il13_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il17_il10_unadj_L[1], " (", t2_ratio_il17_il10_unadj_L[2], ", ", t2_ratio_il17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il21_il10_unadj_L[1], " (", t2_ratio_il21_il10_unadj_L[2], ", ", t2_ratio_il21_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il2_il10_unadj_L[1], " (", t2_ratio_il2_il10_unadj_L[2], ", ", t2_ratio_il2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_gmc_il10_unadj_L[1], " (", t2_ratio_gmc_il10_unadj_L[2], ", ", t2_ratio_gmc_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il4_unadj_L[1], " (", t2_ratio_il12_il4_unadj_L[2], ", ", t2_ratio_il12_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il4_unadj_L[1], " (", t2_ratio_ifn_il4_unadj_L[2], ", ", t2_ratio_ifn_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il5_unadj_L[1], " (", t2_ratio_il12_il5_unadj_L[2], ", ", t2_ratio_il12_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il5_unadj_L[1], " (", t2_ratio_ifn_il5_unadj_L[2], ", ", t2_ratio_ifn_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il13_unadj_L[1], " (", t2_ratio_il12_il13_unadj_L[2], ", ", t2_ratio_il12_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il13_unadj_L[1], " (", t2_ratio_ifn_il13_unadj_L[2], ", ", t2_ratio_ifn_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il17_unadj_L[1], " (", t2_ratio_il12_il17_unadj_L[2], ", ", t2_ratio_il12_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il17_unadj_L[1], " (", t2_ratio_ifn_il17_unadj_L[2], ", ", t2_ratio_ifn_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il21_unadj_L[1], " (", t2_ratio_il12_il21_unadj_L[2], ", ", t2_ratio_il12_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il21_unadj_L[1], " (", t2_ratio_ifn_il21_unadj_L[2], ", ", t2_ratio_ifn_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_pro_il10_unadj_L[1], " (", t2_ratio_pro_il10_unadj_L[2], ", ", t2_ratio_pro_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_il10_unadj_L[1], " (", t2_ratio_th1_il10_unadj_L[2], ", ", t2_ratio_th1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th2_il10_unadj_L[1], " (", t2_ratio_th2_il10_unadj_L[2], ", ", t2_ratio_th2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th17_il10_unadj_L[1], " (", t2_ratio_th17_il10_unadj_L[2], ", ", t2_ratio_th17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_th2_unadj_L[1], " (", t2_ratio_th1_th2_unadj_L[2], ", ", t2_ratio_th1_th2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_th17_unadj_L[1], " (", t2_ratio_th1_th17_unadj_L[2], ", ", t2_ratio_th1_th17_unadj_L[3], ")", sep=""))

t2_ratio_il1_il10_adj_sex_age_L <- round(t2_ratio_il1_il10_adj_sex_age_L, 2)
t2_ratio_il6_il10_adj_sex_age_L <- round(t2_ratio_il6_il10_adj_sex_age_L, 2)
t2_ratio_tnf_il10_adj_sex_age_L <- round(t2_ratio_tnf_il10_adj_sex_age_L, 2)
t2_ratio_il12_il10_adj_sex_age_L <- round(t2_ratio_il12_il10_adj_sex_age_L, 2)
t2_ratio_ifn_il10_adj_sex_age_L <- round(t2_ratio_ifn_il10_adj_sex_age_L, 2)
t2_ratio_il4_il10_adj_sex_age_L <- round(t2_ratio_il4_il10_adj_sex_age_L, 2)
t2_ratio_il5_il10_adj_sex_age_L <- round(t2_ratio_il5_il10_adj_sex_age_L, 2)
t2_ratio_il13_il10_adj_sex_age_L <- round(t2_ratio_il13_il10_adj_sex_age_L, 2)
t2_ratio_il17_il10_adj_sex_age_L <- round(t2_ratio_il17_il10_adj_sex_age_L, 2)
t2_ratio_il21_il10_adj_sex_age_L <- round(t2_ratio_il21_il10_adj_sex_age_L, 2)
t2_ratio_il2_il10_adj_sex_age_L <- round(t2_ratio_il2_il10_adj_sex_age_L, 2)
t2_ratio_gmc_il10_adj_sex_age_L <- round(t2_ratio_gmc_il10_adj_sex_age_L, 2)
t2_ratio_il12_il4_adj_sex_age_L <- round(t2_ratio_il12_il4_adj_sex_age_L, 2)
t2_ratio_ifn_il4_adj_sex_age_L <- round(t2_ratio_ifn_il4_adj_sex_age_L, 2)
t2_ratio_il12_il5_adj_sex_age_L <- round(t2_ratio_il12_il5_adj_sex_age_L, 2)
t2_ratio_ifn_il5_adj_sex_age_L <- round(t2_ratio_ifn_il5_adj_sex_age_L, 2)
t2_ratio_il12_il13_adj_sex_age_L <- round(t2_ratio_il12_il13_adj_sex_age_L, 2)
t2_ratio_ifn_il13_adj_sex_age_L <- round(t2_ratio_ifn_il13_adj_sex_age_L, 2)
t2_ratio_il12_il17_adj_sex_age_L <- round(t2_ratio_il12_il17_adj_sex_age_L, 2)
t2_ratio_ifn_il17_adj_sex_age_L <- round(t2_ratio_ifn_il17_adj_sex_age_L, 2)
t2_ratio_il12_il21_adj_sex_age_L <- round(t2_ratio_il12_il21_adj_sex_age_L, 2)
t2_ratio_ifn_il21_adj_sex_age_L <- round(t2_ratio_ifn_il21_adj_sex_age_L, 2)
t2_ratio_pro_il10_adj_sex_age_L <- round(t2_ratio_pro_il10_adj_sex_age_L, 2)
t2_ratio_th1_il10_adj_sex_age_L <- round(t2_ratio_th1_il10_adj_sex_age_L, 2)
t2_ratio_th2_il10_adj_sex_age_L <- round(t2_ratio_th2_il10_adj_sex_age_L, 2)
t2_ratio_th17_il10_adj_sex_age_L <- round(t2_ratio_th17_il10_adj_sex_age_L, 2)
t2_ratio_th1_th2_adj_sex_age_L <- round(t2_ratio_th1_th2_adj_sex_age_L, 2)
t2_ratio_th1_th17_adj_sex_age_L <- round(t2_ratio_th1_th17_adj_sex_age_L, 2)

asadjtbl3 <- c(" ", " ", paste(t2_ratio_il1_il10_adj_sex_age_L[1], " (", t2_ratio_il1_il10_adj_sex_age_L[2], ", ", t2_ratio_il1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il6_il10_adj_sex_age_L[1], " (", t2_ratio_il6_il10_adj_sex_age_L[2], ", ", t2_ratio_il6_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_tnf_il10_adj_sex_age_L[1], " (", t2_ratio_tnf_il10_adj_sex_age_L[2], ", ", t2_ratio_tnf_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il10_adj_sex_age_L[1], " (", t2_ratio_il12_il10_adj_sex_age_L[2], ", ", t2_ratio_il12_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il10_adj_sex_age_L[1], " (", t2_ratio_ifn_il10_adj_sex_age_L[2], ", ", t2_ratio_ifn_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il4_il10_adj_sex_age_L[1], " (", t2_ratio_il4_il10_adj_sex_age_L[2], ", ", t2_ratio_il4_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il5_il10_adj_sex_age_L[1], " (", t2_ratio_il5_il10_adj_sex_age_L[2], ", ", t2_ratio_il5_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il13_il10_adj_sex_age_L[1], " (", t2_ratio_il13_il10_adj_sex_age_L[2], ", ", t2_ratio_il13_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il17_il10_adj_sex_age_L[1], " (", t2_ratio_il17_il10_adj_sex_age_L[2], ", ", t2_ratio_il17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il21_il10_adj_sex_age_L[1], " (", t2_ratio_il21_il10_adj_sex_age_L[2], ", ", t2_ratio_il21_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il2_il10_adj_sex_age_L[1], " (", t2_ratio_il2_il10_adj_sex_age_L[2], ", ", t2_ratio_il2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_gmc_il10_adj_sex_age_L[1], " (", t2_ratio_gmc_il10_adj_sex_age_L[2], ", ", t2_ratio_gmc_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il4_adj_sex_age_L[1], " (", t2_ratio_il12_il4_adj_sex_age_L[2], ", ", t2_ratio_il12_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il4_adj_sex_age_L[1], " (", t2_ratio_ifn_il4_adj_sex_age_L[2], ", ", t2_ratio_ifn_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il5_adj_sex_age_L[1], " (", t2_ratio_il12_il5_adj_sex_age_L[2], ", ", t2_ratio_il12_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il5_adj_sex_age_L[1], " (", t2_ratio_ifn_il5_adj_sex_age_L[2], ", ", t2_ratio_ifn_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il13_adj_sex_age_L[1], " (", t2_ratio_il12_il13_adj_sex_age_L[2], ", ", t2_ratio_il12_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il13_adj_sex_age_L[1], " (", t2_ratio_ifn_il13_adj_sex_age_L[2], ", ", t2_ratio_ifn_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il17_adj_sex_age_L[1], " (", t2_ratio_il12_il17_adj_sex_age_L[2], ", ", t2_ratio_il12_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il17_adj_sex_age_L[1], " (", t2_ratio_ifn_il17_adj_sex_age_L[2], ", ", t2_ratio_ifn_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_il12_il21_adj_sex_age_L[1], " (", t2_ratio_il12_il21_adj_sex_age_L[2], ", ", t2_ratio_il12_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_ifn_il21_adj_sex_age_L[1], " (", t2_ratio_ifn_il21_adj_sex_age_L[2], ", ", t2_ratio_ifn_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_pro_il10_adj_sex_age_L[1], " (", t2_ratio_pro_il10_adj_sex_age_L[2], ", ", t2_ratio_pro_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_il10_adj_sex_age_L[1], " (", t2_ratio_th1_il10_adj_sex_age_L[2], ", ", t2_ratio_th1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th2_il10_adj_sex_age_L[1], " (", t2_ratio_th2_il10_adj_sex_age_L[2], ", ", t2_ratio_th2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th17_il10_adj_sex_age_L[1], " (", t2_ratio_th17_il10_adj_sex_age_L[2], ", ", t2_ratio_th17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_th2_adj_sex_age_L[1], " (", t2_ratio_th1_th2_adj_sex_age_L[2], ", ", t2_ratio_th1_th2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ratio_th1_th17_adj_sex_age_L[1], " (", t2_ratio_th1_th17_adj_sex_age_L[2], ", ", t2_ratio_th1_th17_adj_sex_age_L[3], ")", sep=""))

t2_ratio_il1_il10_adj_L <- round(t2_ratio_il1_il10_adj_L, 2)
t2_ratio_il6_il10_adj_L <- round(t2_ratio_il6_il10_adj_L, 2)
t2_ratio_tnf_il10_adj_L <- round(t2_ratio_tnf_il10_adj_L, 2)
t2_ratio_il12_il10_adj_L <- round(t2_ratio_il12_il10_adj_L, 2)
t2_ratio_ifn_il10_adj_L <- round(t2_ratio_ifn_il10_adj_L, 2)
t2_ratio_il4_il10_adj_L <- round(t2_ratio_il4_il10_adj_L, 2)
t2_ratio_il5_il10_adj_L <- round(t2_ratio_il5_il10_adj_L, 2)
t2_ratio_il13_il10_adj_L <- round(t2_ratio_il13_il10_adj_L, 2)
t2_ratio_il17_il10_adj_L <- round(t2_ratio_il17_il10_adj_L, 2)
t2_ratio_il21_il10_adj_L <- round(t2_ratio_il21_il10_adj_L, 2)
t2_ratio_il2_il10_adj_L <- round(t2_ratio_il2_il10_adj_L, 2)
t2_ratio_gmc_il10_adj_L <- round(t2_ratio_gmc_il10_adj_L, 2)
t2_ratio_il12_il4_adj_L <- round(t2_ratio_il12_il4_adj_L, 2)
t2_ratio_ifn_il4_adj_L <- round(t2_ratio_ifn_il4_adj_L, 2)
t2_ratio_il12_il5_adj_L <- round(t2_ratio_il12_il5_adj_L, 2)
t2_ratio_ifn_il5_adj_L <- round(t2_ratio_ifn_il5_adj_L, 2)
t2_ratio_il12_il13_adj_L <- round(t2_ratio_il12_il13_adj_L, 2)
t2_ratio_ifn_il13_adj_L <- round(t2_ratio_ifn_il13_adj_L, 2)
t2_ratio_il12_il17_adj_L <- round(t2_ratio_il12_il17_adj_L, 2)
t2_ratio_ifn_il17_adj_L <- round(t2_ratio_ifn_il17_adj_L, 2)
t2_ratio_il12_il21_adj_L <- round(t2_ratio_il12_il21_adj_L, 2)
t2_ratio_ifn_il21_adj_L <- round(t2_ratio_ifn_il21_adj_L, 2)
t2_ratio_pro_il10_adj_L <- round(t2_ratio_pro_il10_adj_L, 2)
t2_ratio_th1_il10_adj_L <- round(t2_ratio_th1_il10_adj_L, 2)
t2_ratio_th2_il10_adj_L <- round(t2_ratio_th2_il10_adj_L, 2)
t2_ratio_th17_il10_adj_L <- round(t2_ratio_th17_il10_adj_L, 2)
t2_ratio_th1_th2_adj_L <- round(t2_ratio_th1_th2_adj_L, 2)
t2_ratio_th1_th17_adj_L <- round(t2_ratio_th1_th17_adj_L, 2)

adjtbl3 <- c(" ", " ", paste(t2_ratio_il1_il10_adj_L[1], " (", t2_ratio_il1_il10_adj_L[2], ", ", t2_ratio_il1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il6_il10_adj_L[1], " (", t2_ratio_il6_il10_adj_L[2], ", ", t2_ratio_il6_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_tnf_il10_adj_L[1], " (", t2_ratio_tnf_il10_adj_L[2], ", ", t2_ratio_tnf_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il10_adj_L[1], " (", t2_ratio_il12_il10_adj_L[2], ", ", t2_ratio_il12_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il10_adj_L[1], " (", t2_ratio_ifn_il10_adj_L[2], ", ", t2_ratio_ifn_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il4_il10_adj_L[1], " (", t2_ratio_il4_il10_adj_L[2], ", ", t2_ratio_il4_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il5_il10_adj_L[1], " (", t2_ratio_il5_il10_adj_L[2], ", ", t2_ratio_il5_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il13_il10_adj_L[1], " (", t2_ratio_il13_il10_adj_L[2], ", ", t2_ratio_il13_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il17_il10_adj_L[1], " (", t2_ratio_il17_il10_adj_L[2], ", ", t2_ratio_il17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il21_il10_adj_L[1], " (", t2_ratio_il21_il10_adj_L[2], ", ", t2_ratio_il21_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il2_il10_adj_L[1], " (", t2_ratio_il2_il10_adj_L[2], ", ", t2_ratio_il2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_gmc_il10_adj_L[1], " (", t2_ratio_gmc_il10_adj_L[2], ", ", t2_ratio_gmc_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il4_adj_L[1], " (", t2_ratio_il12_il4_adj_L[2], ", ", t2_ratio_il12_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il4_adj_L[1], " (", t2_ratio_ifn_il4_adj_L[2], ", ", t2_ratio_ifn_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il5_adj_L[1], " (", t2_ratio_il12_il5_adj_L[2], ", ", t2_ratio_il12_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il5_adj_L[1], " (", t2_ratio_ifn_il5_adj_L[2], ", ", t2_ratio_ifn_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il13_adj_L[1], " (", t2_ratio_il12_il13_adj_L[2], ", ", t2_ratio_il12_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il13_adj_L[1], " (", t2_ratio_ifn_il13_adj_L[2], ", ", t2_ratio_ifn_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il17_adj_L[1], " (", t2_ratio_il12_il17_adj_L[2], ", ", t2_ratio_il12_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il17_adj_L[1], " (", t2_ratio_ifn_il17_adj_L[2], ", ", t2_ratio_ifn_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_il12_il21_adj_L[1], " (", t2_ratio_il12_il21_adj_L[2], ", ", t2_ratio_il12_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_ifn_il21_adj_L[1], " (", t2_ratio_ifn_il21_adj_L[2], ", ", t2_ratio_ifn_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_pro_il10_adj_L[1], " (", t2_ratio_pro_il10_adj_L[2], ", ", t2_ratio_pro_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_th1_il10_adj_L[1], " (", t2_ratio_th1_il10_adj_L[2], ", ", t2_ratio_th1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_th2_il10_adj_L[1], " (", t2_ratio_th2_il10_adj_L[2], ", ", t2_ratio_th2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_th17_il10_adj_L[1], " (", t2_ratio_th17_il10_adj_L[2], ", ", t2_ratio_th17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_th1_th2_adj_L[1], " (", t2_ratio_th1_th2_adj_L[2], ", ", t2_ratio_th1_th2_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ratio_th1_th17_adj_L[1], " (", t2_ratio_th1_th17_adj_L[2], ", ", t2_ratio_th1_th17_adj_L[3], ")", sep=""))

# Table 3: Effect of intervention on cytokine ratios at age 14 months
tbl3 <- data.table(
  "Outcome, Arm" = outcometbl3,
  "N" = Ntbl3, 
  "Absolute Mean" = absmeantbl3,
  "Absolute SD" = abssdtbl3,
  "Mean" = meantbl3, 
  "SD" = sdtbl3,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl3,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl3, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl3
)

write.csv(tbl3, file=here('tables/immune/immune_main/immune_table3.csv'))
print(xtable(tbl3), type="html", file=here("tables/immune/immune_main/immune_table3.html"))


#### TABLE 4 ####
outcometbl4 <- c(paste("Ln IL-1", "Î²", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "Î±", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IGF-1 (", "Î¼", "g/L)", sep=""), "Control", "Nutrition + WSH")

Ntbl4 <- c(" ", as.character(il1_t3_N_tr$t3_ln_il1_N_tr[1]), as.character(il1_t3_N_tr$t3_ln_il1_N_tr[2]),
           " ", as.character(il6_t3_N_tr$t3_ln_il6_N_tr[1]), as.character(il6_t3_N_tr$t3_ln_il6_N_tr[2]),
           " ", as.character(tnf_t3_N_tr$t3_ln_tnf_N_tr[1]), as.character(tnf_t3_N_tr$t3_ln_tnf_N_tr[2]),
           " ", as.character(il12_t3_N_tr$t3_ln_il12_N_tr[1]), as.character(il12_t3_N_tr$t3_ln_il12_N_tr[2]),
           " ", as.character(ifn_t3_N_tr$t3_ln_ifn_N_tr[1]), as.character(ifn_t3_N_tr$t3_ln_ifn_N_tr[2]),
           " ", as.character(il4_t3_N_tr$t3_ln_il4_N_tr[1]), as.character(il4_t3_N_tr$t3_ln_il4_N_tr[2]),
           " ", as.character(il5_t3_N_tr$t3_ln_il5_N_tr[1]), as.character(il5_t3_N_tr$t3_ln_il5_N_tr[2]),
           " ", as.character(il13_t3_N_tr$t3_ln_il13_N_tr[1]), as.character(il13_t3_N_tr$t3_ln_il13_N_tr[2]),
           " ", as.character(il17_t3_N_tr$t3_ln_il17_N_tr[1]), as.character(il17_t3_N_tr$t3_ln_il17_N_tr[2]),
           " ", as.character(il21_t3_N_tr$t3_ln_il21_N_tr[1]), as.character(il21_t3_N_tr$t3_ln_il21_N_tr[2]),
           " ", as.character(il10_t3_N_tr$t3_ln_il10_N_tr[1]), as.character(il10_t3_N_tr$t3_ln_il10_N_tr[2]),
           " ", as.character(il2_t3_N_tr$t3_ln_il2_N_tr[1]), as.character(il2_t3_N_tr$t3_ln_il2_N_tr[2]),
           " ", as.character(gmc_t3_N_tr$t3_ln_gmc_N_tr[1]), as.character(gmc_t3_N_tr$t3_ln_gmc_N_tr[2]),
           " ", as.character(igf_t3_N_tr$t3_ln_igf_N_tr[1]), as.character(igf_t3_N_tr$t3_ln_igf_N_tr[2]))

absmeantbl4 <- c(" ", as.character(round(abs_il1_t3_N_tr$mean[1], 2)), as.character(round(abs_il1_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il6_t3_N_tr$mean[1], 2)), as.character(round(abs_il6_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_tnf_t3_N_tr$mean[1], 2)), as.character(round(abs_tnf_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il12_t3_N_tr$mean[1], 2)), as.character(round(abs_il12_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_ifn_t3_N_tr$mean[1], 2)), as.character(round(abs_ifn_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il4_t3_N_tr$mean[1], 2)), as.character(round(abs_il4_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il5_t3_N_tr$mean[1], 2)), as.character(round(abs_il5_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il13_t3_N_tr$mean[1], 2)), as.character(round(abs_il13_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il17_t3_N_tr$mean[1], 2)), as.character(round(abs_il17_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il21_t3_N_tr$mean[1], 2)), as.character(round(abs_il21_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il10_t3_N_tr$mean[1], 2)), as.character(round(abs_il10_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_il2_t3_N_tr$mean[1], 2)), as.character(round(abs_il2_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_gmc_t3_N_tr$mean[1], 2)), as.character(round(abs_gmc_t3_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_igf_t3_N_tr$mean[1], 2)), as.character(round(abs_igf_t3_N_tr$mean[2], 2)))

abssdtbl4 <- c(" ", as.character(round(abs_il1_t3_N_tr$sd[1], 2)), as.character(round(abs_il1_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il6_t3_N_tr$sd[1], 2)), as.character(round(abs_il6_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_tnf_t3_N_tr$sd[1], 2)), as.character(round(abs_tnf_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il12_t3_N_tr$sd[1], 2)), as.character(round(abs_il12_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_ifn_t3_N_tr$sd[1], 2)), as.character(round(abs_ifn_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il4_t3_N_tr$sd[1], 2)), as.character(round(abs_il4_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il5_t3_N_tr$sd[1], 2)), as.character(round(abs_il5_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il13_t3_N_tr$sd[1], 2)), as.character(round(abs_il13_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il17_t3_N_tr$sd[1], 2)), as.character(round(abs_il17_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il21_t3_N_tr$sd[1], 2)), as.character(round(abs_il21_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il10_t3_N_tr$sd[1], 2)), as.character(round(abs_il10_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_il2_t3_N_tr$sd[1], 2)), as.character(round(abs_il2_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_gmc_t3_N_tr$sd[1], 2)), as.character(round(abs_gmc_t3_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_igf_t3_N_tr$sd[1], 2)), as.character(round(abs_igf_t3_N_tr$sd[2], 2)))

meantbl4 <- c(" ", as.character(round(il1_t3_N_tr$mean[1], 2)), as.character(round(il1_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il6_t3_N_tr$mean[1], 2)), as.character(round(il6_t3_N_tr$mean[2], 2)),
              " ", as.character(round(tnf_t3_N_tr$mean[1], 2)), as.character(round(tnf_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il12_t3_N_tr$mean[1], 2)), as.character(round(il12_t3_N_tr$mean[2], 2)),
              " ", as.character(round(ifn_t3_N_tr$mean[1], 2)), as.character(round(ifn_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il4_t3_N_tr$mean[1], 2)), as.character(round(il4_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il5_t3_N_tr$mean[1], 2)), as.character(round(il5_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il13_t3_N_tr$mean[1], 2)), as.character(round(il13_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il17_t3_N_tr$mean[1], 2)), as.character(round(il17_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il21_t3_N_tr$mean[1], 2)), as.character(round(il21_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il10_t3_N_tr$mean[1], 2)), as.character(round(il10_t3_N_tr$mean[2], 2)),
              " ", as.character(round(il2_t3_N_tr$mean[1], 2)), as.character(round(il2_t3_N_tr$mean[2], 2)),
              " ", as.character(round(gmc_t3_N_tr$mean[1], 2)), as.character(round(gmc_t3_N_tr$mean[2], 2)),
              " ", as.character(round(igf_t3_N_tr$mean[1], 2)), as.character(round(igf_t3_N_tr$mean[2], 2)))

sdtbl4 <- c(" ", as.character(round(il1_t3_N_tr$sd[1], 2)), as.character(round(il1_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il6_t3_N_tr$sd[1], 2)), as.character(round(il6_t3_N_tr$sd[2], 2)),
            " ", as.character(round(tnf_t3_N_tr$sd[1], 2)), as.character(round(tnf_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il12_t3_N_tr$sd[1], 2)), as.character(round(il12_t3_N_tr$sd[2], 2)),
            " ", as.character(round(ifn_t3_N_tr$sd[1], 2)), as.character(round(ifn_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il4_t3_N_tr$sd[1], 2)), as.character(round(il4_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il5_t3_N_tr$sd[1], 2)), as.character(round(il5_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il13_t3_N_tr$sd[1], 2)), as.character(round(il13_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il17_t3_N_tr$sd[1], 2)), as.character(round(il17_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il21_t3_N_tr$sd[1], 2)), as.character(round(il21_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il10_t3_N_tr$sd[1], 2)), as.character(round(il10_t3_N_tr$sd[2], 2)),
            " ", as.character(round(il2_t3_N_tr$sd[1], 2)), as.character(round(il2_t3_N_tr$sd[2], 2)),
            " ", as.character(round(gmc_t3_N_tr$sd[1], 2)), as.character(round(gmc_t3_N_tr$sd[2], 2)),
            " ", as.character(round(igf_t3_N_tr$sd[1], 2)), as.character(round(igf_t3_N_tr$sd[2], 2)))

t3_gmc_unadj_L <- round(t3_gmc_unadj_L, 2)
t3_ifn_unadj_L <- round(t3_ifn_unadj_L, 2)
t3_igf_unadj_L <- round(t3_igf_unadj_L, 2)
t3_il1_unadj_L <- round(t3_il1_unadj_L, 2)
t3_il10_unadj_L <- round(t3_il10_unadj_L, 2)
t3_il12_unadj_L <- round(t3_il12_unadj_L, 2)
t3_il13_unadj_L <- round(t3_il13_unadj_L, 2)
t3_il17_unadj_L <- round(t3_il17_unadj_L, 2)
t3_il2_unadj_L <- round(t3_il2_unadj_L, 2)
t3_il21_unadj_L <- round(t3_il21_unadj_L, 2)
t3_il4_unadj_L <- round(t3_il4_unadj_L, 2)
t3_il5_unadj_L <- round(t3_il5_unadj_L, 2)
t3_il6_unadj_L <- round(t3_il6_unadj_L, 2)
t3_tnf_unadj_L <- round(t3_tnf_unadj_L, 2)

unadjtbl4 <- c(" ", " ", paste(t3_il1_unadj_L[1], " (", t3_il1_unadj_L[2], ", ", t3_il1_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il6_unadj_L[1], " (", t3_il6_unadj_L[2], ", ", t3_il6_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_tnf_unadj_L[1], " (", t3_tnf_unadj_L[2], ", ", t3_tnf_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il12_unadj_L[1], " (", t3_il12_unadj_L[2], ", ", t3_il12_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ifn_unadj_L[1], " (", t3_ifn_unadj_L[2], ", ", t3_ifn_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il4_unadj_L[1], " (", t3_il4_unadj_L[2], ", ", t3_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il5_unadj_L[1], " (", t3_il5_unadj_L[2], ", ", t3_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il13_unadj_L[1], " (", t3_il13_unadj_L[2], ", ", t3_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il17_unadj_L[1], " (", t3_il17_unadj_L[2], ", ", t3_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il21_unadj_L[1], " (", t3_il21_unadj_L[2], ", ", t3_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il10_unadj_L[1], " (", t3_il10_unadj_L[2], ", ", t3_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_il2_unadj_L[1], " (", t3_il2_unadj_L[2], ", ", t3_il2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_gmc_unadj_L[1], " (", t3_gmc_unadj_L[2], ", ", t3_gmc_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_igf_unadj_L[1], " (", t3_igf_unadj_L[2], ", ", t3_igf_unadj_L[3], ")", sep=""))

t3_gmc_adj_sex_age_L <- round(t3_gmc_adj_sex_age_L, 2)
t3_ifn_adj_sex_age_L <- round(t3_ifn_adj_sex_age_L, 2)
t3_igf_adj_sex_age_L <- round(t3_igf_adj_sex_age_L, 2)
t3_il1_adj_sex_age_L <- round(t3_il1_adj_sex_age_L, 2)
t3_il10_adj_sex_age_L <- round(t3_il10_adj_sex_age_L, 2)
t3_il12_adj_sex_age_L <- round(t3_il12_adj_sex_age_L, 2)
t3_il13_adj_sex_age_L <- round(t3_il13_adj_sex_age_L, 2)
t3_il17_adj_sex_age_L <- round(t3_il17_adj_sex_age_L, 2)
t3_il2_adj_sex_age_L <- round(t3_il2_adj_sex_age_L, 2)
t3_il21_adj_sex_age_L <- round(t3_il21_adj_sex_age_L, 2)
t3_il4_adj_sex_age_L <- round(t3_il4_adj_sex_age_L, 2)
t3_il5_adj_sex_age_L <- round(t3_il5_adj_sex_age_L, 2)
t3_il6_adj_sex_age_L <- round(t3_il6_adj_sex_age_L, 2)
t3_tnf_adj_sex_age_L <- round(t3_tnf_adj_sex_age_L, 2)

asadjtbl4 <- c(" ", " ", paste(t3_il1_adj_sex_age_L[1], " (", t3_il1_adj_sex_age_L[2], ", ", t3_il1_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il6_adj_sex_age_L[1], " (", t3_il6_adj_sex_age_L[2], ", ", t3_il6_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_tnf_adj_sex_age_L[1], " (", t3_tnf_adj_sex_age_L[2], ", ", t3_tnf_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il12_adj_sex_age_L[1], " (", t3_il12_adj_sex_age_L[2], ", ", t3_il12_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ifn_adj_sex_age_L[1], " (", t3_ifn_adj_sex_age_L[2], ", ", t3_ifn_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il4_adj_sex_age_L[1], " (", t3_il4_adj_sex_age_L[2], ", ", t3_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il5_adj_sex_age_L[1], " (", t3_il5_adj_sex_age_L[2], ", ", t3_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il13_adj_sex_age_L[1], " (", t3_il13_adj_sex_age_L[2], ", ", t3_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il17_adj_sex_age_L[1], " (", t3_il17_adj_sex_age_L[2], ", ", t3_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il21_adj_sex_age_L[1], " (", t3_il21_adj_sex_age_L[2], ", ", t3_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il10_adj_sex_age_L[1], " (", t3_il10_adj_sex_age_L[2], ", ", t3_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_il2_adj_sex_age_L[1], " (", t3_il2_adj_sex_age_L[2], ", ", t3_il2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_gmc_adj_sex_age_L[1], " (", t3_gmc_adj_sex_age_L[2], ", ", t3_gmc_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_igf_adj_sex_age_L[1], " (", t3_igf_adj_sex_age_L[2], ", ", t3_igf_adj_sex_age_L[3], ")", sep=""))

t3_gmc_adj_L <- round(t3_gmc_adj_L, 2)
t3_ifn_adj_L <- round(t3_ifn_adj_L, 2)
t3_igf_adj_L <- round(t3_igf_adj_L, 2)
t3_il1_adj_L <- round(t3_il1_adj_L, 2)
t3_il10_adj_L <- round(t3_il10_adj_L, 2)
t3_il12_adj_L <- round(t3_il12_adj_L, 2)
t3_il13_adj_L <- round(t3_il13_adj_L, 2)
t3_il17_adj_L <- round(t3_il17_adj_L, 2)
t3_il2_adj_L <- round(t3_il2_adj_L, 2)
t3_il21_adj_L <- round(t3_il21_adj_L, 2)
t3_il4_adj_L <- round(t3_il4_adj_L, 2)
t3_il5_adj_L <- round(t3_il5_adj_L, 2)
t3_il6_adj_L <- round(t3_il6_adj_L, 2)
t3_tnf_adj_L <- round(t3_tnf_adj_L, 2)

adjtbl4 <- c(" ", " ", paste(t3_il1_adj_L[1], " (", t3_il1_adj_L[2], ", ", t3_il1_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il6_adj_L[1], " (", t3_il6_adj_L[2], ", ", t3_il6_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_tnf_adj_L[1], " (", t3_tnf_adj_L[2], ", ", t3_tnf_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il12_adj_L[1], " (", t3_il12_adj_L[2], ", ", t3_il12_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ifn_adj_L[1], " (", t3_ifn_adj_L[2], ", ", t3_ifn_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il4_adj_L[1], " (", t3_il4_adj_L[2], ", ", t3_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il5_adj_L[1], " (", t3_il5_adj_L[2], ", ", t3_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il13_adj_L[1], " (", t3_il13_adj_L[2], ", ", t3_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il17_adj_L[1], " (", t3_il17_adj_L[2], ", ", t3_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il21_adj_L[1], " (", t3_il21_adj_L[2], ", ", t3_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il10_adj_L[1], " (", t3_il10_adj_L[2], ", ", t3_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_il2_adj_L[1], " (", t3_il2_adj_L[2], ", ", t3_il2_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_gmc_adj_L[1], " (", t3_gmc_adj_L[2], ", ", t3_gmc_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_igf_adj_L[1], " (", t3_igf_adj_L[2], ", ", t3_igf_adj_L[3], ")", sep=""))

# Table 4: Effect of intervention on individual immune status and growth factor measurements at age 28 months
tbl4 <- data.table(
  "Outcome, Arm" = outcometbl4,
  "N" = Ntbl4, 
  "Absolute Mean" = absmeantbl4,
  "Absolute SD" = abssdtbl4,
  "Mean" = meantbl4, 
  "SD" = sdtbl4,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl4,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl4, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl4
)

write.csv(tbl4, file=here('tables/immune/immune_main/immune_table4.csv'))
print(xtable(tbl4), type="html", file=here("tables/immune/immune_main/immune_table4.html"))



#### TABLE 5 ####
outcometbl5 <- c(paste("Ln IL-1", "Î²", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "Î±", "/IL-10", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-5/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-13/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-17A/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-21/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-2/IL-10", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF/IL-10", "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-4", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-4", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-5", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-5", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-13", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-13", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-17A", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-17A", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-21", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "Î³", "/IL-21", sep=""), "Control", "Nutrition + WSH",
                 "Ln Pro-inflammatory cytokines*/IL-10", "Control", "Nutrition + WSH",
                 "Ln Th1**/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th2***/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th17****/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th2***", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th17****", "Control", "Nutrition + WSH")

Ntbl5 <- c(" ", as.character(t3_ratio_il1_il10_N_tr$t3_ratio_il1_il10_N_tr[1]), as.character(t3_ratio_il1_il10_N_tr$t3_ratio_il1_il10_N_tr[2]), 
           " ", as.character(t3_ratio_il6_il10_N_tr$t3_ratio_il6_il10_N_tr[1]), as.character(t3_ratio_il6_il10_N_tr$t3_ratio_il6_il10_N_tr[2]), 
           " ", as.character(t3_ratio_tnf_il10_N_tr$t3_ratio_tnf_il10_N_tr[1]), as.character(t3_ratio_tnf_il10_N_tr$t3_ratio_tnf_il10_N_tr[2]),
           " ", as.character(t3_ratio_il12_il10_N_tr$t3_ratio_il12_il10_N_tr[1]), as.character(t3_ratio_il12_il10_N_tr$t3_ratio_il12_il10_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il10_N_tr$t3_ratio_ifn_il10_N_tr[1]), as.character(t3_ratio_ifn_il10_N_tr$t3_ratio_ifn_il10_N_tr[2]),
           " ", as.character(t3_ratio_il4_il10_N_tr$t3_ratio_il4_il10_N_tr[1]), as.character(t3_ratio_il4_il10_N_tr$t3_ratio_il4_il10_N_tr[2]),
           " ", as.character(t3_ratio_il5_il10_N_tr$t3_ratio_il5_il10_N_tr[1]), as.character(t3_ratio_il5_il10_N_tr$t3_ratio_il5_il10_N_tr[2]),
           " ", as.character(t3_ratio_il13_il10_N_tr$t3_ratio_il13_il10_N_tr[1]), as.character(t3_ratio_il13_il10_N_tr$t3_ratio_il13_il10_N_tr[2]),
           " ", as.character(t3_ratio_il17_il10_N_tr$t3_ratio_il17_il10_N_tr[1]), as.character(t3_ratio_il17_il10_N_tr$t3_ratio_il17_il10_N_tr[2]),
           " ", as.character(t3_ratio_il21_il10_N_tr$t3_ratio_il21_il10_N_tr[1]), as.character(t3_ratio_il21_il10_N_tr$t3_ratio_il21_il10_N_tr[2]),
           " ", as.character(t3_ratio_il2_il10_N_tr$t3_ratio_il2_il10_N_tr[1]), as.character(t3_ratio_il2_il10_N_tr$t3_ratio_il2_il10_N_tr[2]),
           " ", as.character(t3_ratio_gmc_il10_N_tr$t3_ratio_gmc_il10_N_tr[1]), as.character(t3_ratio_gmc_il10_N_tr$t3_ratio_gmc_il10_N_tr[2]),
           " ", as.character(t3_ratio_il12_il4_N_tr$t3_ratio_il12_il4_N_tr[1]), as.character(t3_ratio_il12_il4_N_tr$t3_ratio_il12_il4_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il4_N_tr$t3_ratio_ifn_il4_N_tr[1]), as.character(t3_ratio_ifn_il4_N_tr$t3_ratio_ifn_il4_N_tr[2]),
           " ", as.character(t3_ratio_il12_il5_N_tr$t3_ratio_il12_il5_N_tr[1]), as.character(t3_ratio_il12_il5_N_tr$t3_ratio_il12_il5_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il5_N_tr$t3_ratio_ifn_il5_N_tr[1]), as.character(t3_ratio_ifn_il5_N_tr$t3_ratio_ifn_il5_N_tr[2]),
           " ", as.character(t3_ratio_il12_il13_N_tr$t3_ratio_il12_il13_N_tr[1]), as.character(t3_ratio_il12_il13_N_tr$t3_ratio_il12_il13_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il13_N_tr$t3_ratio_ifn_il13_N_tr[1]), as.character(t3_ratio_ifn_il13_N_tr$t3_ratio_ifn_il13_N_tr[2]),
           " ", as.character(t3_ratio_il12_il17_N_tr$t3_ratio_il12_il17_N_tr[1]), as.character(t3_ratio_il12_il17_N_tr$t3_ratio_il12_il17_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il17_N_tr$t3_ratio_ifn_il17_N_tr[1]), as.character(t3_ratio_ifn_il17_N_tr$t3_ratio_ifn_il17_N_tr[2]),
           " ", as.character(t3_ratio_il12_il21_N_tr$t3_ratio_il12_il21_N_tr[1]), as.character(t3_ratio_il12_il21_N_tr$t3_ratio_il12_il21_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il21_N_tr$t3_ratio_ifn_il21_N_tr[1]), as.character(t3_ratio_ifn_il21_N_tr$t3_ratio_ifn_il21_N_tr[2]),
           " ", as.character(t3_ratio_pro_il10_N_tr$t3_ratio_pro_il10_N_tr[1]), as.character(t3_ratio_pro_il10_N_tr$t3_ratio_pro_il10_N_tr[2]),
           " ", as.character(t3_ratio_th1_il10_N_tr$t3_ratio_th1_il10_N_tr[1]), as.character(t3_ratio_th1_il10_N_tr$t3_ratio_th1_il10_N_tr[2]),
           " ", as.character(t3_ratio_th2_il10_N_tr$t3_ratio_th2_il10_N_tr[1]), as.character(t3_ratio_th2_il10_N_tr$t3_ratio_th2_il10_N_tr[2]),
           " ", as.character(t3_ratio_th17_il10_N_tr$t3_ratio_th17_il10_N_tr[1]), as.character(t3_ratio_th17_il10_N_tr$t3_ratio_th17_il10_N_tr[2]),
           " ", as.character(t3_ratio_th1_th2_N_tr$t3_ratio_th1_th2_N_tr[1]), as.character(t3_ratio_th1_th2_N_tr$t3_ratio_th1_th2_N_tr[2]),
           " ", as.character(t3_ratio_th1_th17_N_tr$t3_ratio_th1_th17_N_tr[1]), as.character(t3_ratio_th1_th17_N_tr$t3_ratio_th1_th17_N_tr[2])
)

absmeantbl5 <- c(" ", as.character(round(abs_t3_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il1_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t3_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il6_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t3_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_tnf_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il4_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il5_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il13_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il17_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il21_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il2_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_gmc_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il4_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il4_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il5_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il5_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il13_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il13_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il17_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il17_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_il12_il21_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_ifn_il21_N_tr$mean[2], 2)),
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ")

abssdtbl5 <- c(" ", as.character(round(abs_t3_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il1_il10_N_tr$sd[2], 2)), 
                 " ", as.character(round(abs_t3_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il6_il10_N_tr$sd[2], 2)), 
                 " ", as.character(round(abs_t3_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_tnf_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il4_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il5_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il13_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il17_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il21_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il2_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_gmc_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il4_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il4_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il5_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il5_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il13_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il13_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il17_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il17_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_il12_il21_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_t3_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(abs_t3_ratio_ifn_il21_N_tr$sd[2], 2)),
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ")

meantbl5 <- c(" ", as.character(round(t3_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il1_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t3_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il6_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t3_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_tnf_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il4_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il5_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il13_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il17_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il21_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il2_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_gmc_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il4_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il4_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il5_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il5_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il13_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il13_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il17_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il17_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il21_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il21_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_pro_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_pro_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th2_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th2_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th17_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th17_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_th2_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_th2_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_th17_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_th17_N_tr$mean[2], 2)))

sdtbl5 <- c(" ", as.character(round(t3_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il1_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t3_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il6_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t3_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_tnf_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il4_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il5_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il13_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il17_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il21_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il2_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_gmc_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il4_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il4_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il5_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il5_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il13_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il13_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il17_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il17_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il21_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il21_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_pro_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_pro_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th2_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th2_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th17_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th17_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_th2_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_th2_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_th17_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_th17_N_tr$sd[2], 2)))

t3_ratio_il1_il10_unadj_L <- round(t3_ratio_il1_il10_unadj_L, 2)
t3_ratio_il6_il10_unadj_L <- round(t3_ratio_il6_il10_unadj_L, 2)
t3_ratio_tnf_il10_unadj_L <- round(t3_ratio_tnf_il10_unadj_L, 2)
t3_ratio_il12_il10_unadj_L <- round(t3_ratio_il12_il10_unadj_L, 2)
t3_ratio_ifn_il10_unadj_L <- round(t3_ratio_ifn_il10_unadj_L, 2)
t3_ratio_il4_il10_unadj_L <- round(t3_ratio_il4_il10_unadj_L, 2)
t3_ratio_il5_il10_unadj_L <- round(t3_ratio_il5_il10_unadj_L, 2)
t3_ratio_il13_il10_unadj_L <- round(t3_ratio_il13_il10_unadj_L, 2)
t3_ratio_il17_il10_unadj_L <- round(t3_ratio_il17_il10_unadj_L, 2)
t3_ratio_il21_il10_unadj_L <- round(t3_ratio_il21_il10_unadj_L, 2)
t3_ratio_il2_il10_unadj_L <- round(t3_ratio_il2_il10_unadj_L, 2)
t3_ratio_gmc_il10_unadj_L <- round(t3_ratio_gmc_il10_unadj_L, 2)
t3_ratio_il12_il4_unadj_L <- round(t3_ratio_il12_il4_unadj_L, 2)
t3_ratio_ifn_il4_unadj_L <- round(t3_ratio_ifn_il4_unadj_L, 2)
t3_ratio_il12_il5_unadj_L <- round(t3_ratio_il12_il5_unadj_L, 2)
t3_ratio_ifn_il5_unadj_L <- round(t3_ratio_ifn_il5_unadj_L, 2)
t3_ratio_il12_il13_unadj_L <- round(t3_ratio_il12_il13_unadj_L, 2)
t3_ratio_ifn_il13_unadj_L <- round(t3_ratio_ifn_il13_unadj_L, 2)
t3_ratio_il12_il17_unadj_L <- round(t3_ratio_il12_il17_unadj_L, 2)
t3_ratio_ifn_il17_unadj_L <- round(t3_ratio_ifn_il17_unadj_L, 2)
t3_ratio_il12_il21_unadj_L <- round(t3_ratio_il12_il21_unadj_L, 2)
t3_ratio_ifn_il21_unadj_L <- round(t3_ratio_ifn_il21_unadj_L, 2)
t3_ratio_pro_il10_unadj_L <- round(t3_ratio_pro_il10_unadj_L, 2)
t3_ratio_th1_il10_unadj_L <- round(t3_ratio_th1_il10_unadj_L, 2)
t3_ratio_th2_il10_unadj_L <- round(t3_ratio_th2_il10_unadj_L, 2)
t3_ratio_th17_il10_unadj_L <- round(t3_ratio_th17_il10_unadj_L, 2)
t3_ratio_th1_th2_unadj_L <- round(t3_ratio_th1_th2_unadj_L, 2)
t3_ratio_th1_th17_unadj_L <- round(t3_ratio_th1_th17_unadj_L, 2)

unadjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_unadj_L[1], " (", t3_ratio_il1_il10_unadj_L[2], ", ", t3_ratio_il1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il6_il10_unadj_L[1], " (", t3_ratio_il6_il10_unadj_L[2], ", ", t3_ratio_il6_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_tnf_il10_unadj_L[1], " (", t3_ratio_tnf_il10_unadj_L[2], ", ", t3_ratio_tnf_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il10_unadj_L[1], " (", t3_ratio_il12_il10_unadj_L[2], ", ", t3_ratio_il12_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il10_unadj_L[1], " (", t3_ratio_ifn_il10_unadj_L[2], ", ", t3_ratio_ifn_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il4_il10_unadj_L[1], " (", t3_ratio_il4_il10_unadj_L[2], ", ", t3_ratio_il4_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il5_il10_unadj_L[1], " (", t3_ratio_il5_il10_unadj_L[2], ", ", t3_ratio_il5_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il13_il10_unadj_L[1], " (", t3_ratio_il13_il10_unadj_L[2], ", ", t3_ratio_il13_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il17_il10_unadj_L[1], " (", t3_ratio_il17_il10_unadj_L[2], ", ", t3_ratio_il17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il21_il10_unadj_L[1], " (", t3_ratio_il21_il10_unadj_L[2], ", ", t3_ratio_il21_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il2_il10_unadj_L[1], " (", t3_ratio_il2_il10_unadj_L[2], ", ", t3_ratio_il2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_gmc_il10_unadj_L[1], " (", t3_ratio_gmc_il10_unadj_L[2], ", ", t3_ratio_gmc_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il4_unadj_L[1], " (", t3_ratio_il12_il4_unadj_L[2], ", ", t3_ratio_il12_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il4_unadj_L[1], " (", t3_ratio_ifn_il4_unadj_L[2], ", ", t3_ratio_ifn_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il5_unadj_L[1], " (", t3_ratio_il12_il5_unadj_L[2], ", ", t3_ratio_il12_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il5_unadj_L[1], " (", t3_ratio_ifn_il5_unadj_L[2], ", ", t3_ratio_ifn_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il13_unadj_L[1], " (", t3_ratio_il12_il13_unadj_L[2], ", ", t3_ratio_il12_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il13_unadj_L[1], " (", t3_ratio_ifn_il13_unadj_L[2], ", ", t3_ratio_ifn_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il17_unadj_L[1], " (", t3_ratio_il12_il17_unadj_L[2], ", ", t3_ratio_il12_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il17_unadj_L[1], " (", t3_ratio_ifn_il17_unadj_L[2], ", ", t3_ratio_ifn_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il21_unadj_L[1], " (", t3_ratio_il12_il21_unadj_L[2], ", ", t3_ratio_il12_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il21_unadj_L[1], " (", t3_ratio_ifn_il21_unadj_L[2], ", ", t3_ratio_ifn_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_pro_il10_unadj_L[1], " (", t3_ratio_pro_il10_unadj_L[2], ", ", t3_ratio_pro_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_il10_unadj_L[1], " (", t3_ratio_th1_il10_unadj_L[2], ", ", t3_ratio_th1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th2_il10_unadj_L[1], " (", t3_ratio_th2_il10_unadj_L[2], ", ", t3_ratio_th2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th17_il10_unadj_L[1], " (", t3_ratio_th17_il10_unadj_L[2], ", ", t3_ratio_th17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th2_unadj_L[1], " (", t3_ratio_th1_th2_unadj_L[2], ", ", t3_ratio_th1_th2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th17_unadj_L[1], " (", t3_ratio_th1_th17_unadj_L[2], ", ", t3_ratio_th1_th17_unadj_L[3], ")", sep="")
)


t3_ratio_il1_il10_adj_sex_age_L <- round(t3_ratio_il1_il10_adj_sex_age_L, 2)
t3_ratio_il6_il10_adj_sex_age_L <- round(t3_ratio_il6_il10_adj_sex_age_L, 2)
t3_ratio_tnf_il10_adj_sex_age_L <- round(t3_ratio_tnf_il10_adj_sex_age_L, 2)
t3_ratio_il12_il10_adj_sex_age_L <- round(t3_ratio_il12_il10_adj_sex_age_L, 2)
t3_ratio_ifn_il10_adj_sex_age_L <- round(t3_ratio_ifn_il10_adj_sex_age_L, 2)
t3_ratio_il4_il10_adj_sex_age_L <- round(t3_ratio_il4_il10_adj_sex_age_L, 2)
t3_ratio_il5_il10_adj_sex_age_L <- round(t3_ratio_il5_il10_adj_sex_age_L, 2)
t3_ratio_il13_il10_adj_sex_age_L <- round(t3_ratio_il13_il10_adj_sex_age_L, 2)
t3_ratio_il17_il10_adj_sex_age_L <- round(t3_ratio_il17_il10_adj_sex_age_L, 2)
t3_ratio_il21_il10_adj_sex_age_L <- round(t3_ratio_il21_il10_adj_sex_age_L, 2)
t3_ratio_il2_il10_adj_sex_age_L <- round(t3_ratio_il2_il10_adj_sex_age_L, 2)
t3_ratio_gmc_il10_adj_sex_age_L <- round(t3_ratio_gmc_il10_adj_sex_age_L, 2)
t3_ratio_il12_il4_adj_sex_age_L <- round(t3_ratio_il12_il4_adj_sex_age_L, 2)
t3_ratio_ifn_il4_adj_sex_age_L <- round(t3_ratio_ifn_il4_adj_sex_age_L, 2)
t3_ratio_il12_il5_adj_sex_age_L <- round(t3_ratio_il12_il5_adj_sex_age_L, 2)
t3_ratio_ifn_il5_adj_sex_age_L <- round(t3_ratio_ifn_il5_adj_sex_age_L, 2)
t3_ratio_il12_il13_adj_sex_age_L <- round(t3_ratio_il12_il13_adj_sex_age_L, 2)
t3_ratio_ifn_il13_adj_sex_age_L <- round(t3_ratio_ifn_il13_adj_sex_age_L, 2)
t3_ratio_il12_il17_adj_sex_age_L <- round(t3_ratio_il12_il17_adj_sex_age_L, 2)
t3_ratio_ifn_il17_adj_sex_age_L <- round(t3_ratio_ifn_il17_adj_sex_age_L, 2)
t3_ratio_il12_il21_adj_sex_age_L <- round(t3_ratio_il12_il21_adj_sex_age_L, 2)
t3_ratio_ifn_il21_adj_sex_age_L <- round(t3_ratio_ifn_il21_adj_sex_age_L, 2)
t3_ratio_pro_il10_adj_sex_age_L <- round(t3_ratio_pro_il10_adj_sex_age_L, 2)
t3_ratio_th1_il10_adj_sex_age_L <- round(t3_ratio_th1_il10_adj_sex_age_L, 2)
t3_ratio_th2_il10_adj_sex_age_L <- round(t3_ratio_th2_il10_adj_sex_age_L, 2)
t3_ratio_th17_il10_adj_sex_age_L <- round(t3_ratio_th17_il10_adj_sex_age_L, 2)
t3_ratio_th1_th2_adj_sex_age_L <- round(t3_ratio_th1_th2_adj_sex_age_L, 2)
t3_ratio_th1_th17_adj_sex_age_L <- round(t3_ratio_th1_th17_adj_sex_age_L, 2)

asadjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_adj_sex_age_L[1], " (", t3_ratio_il1_il10_adj_sex_age_L[2], ", ", t3_ratio_il1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il6_il10_adj_sex_age_L[1], " (", t3_ratio_il6_il10_adj_sex_age_L[2], ", ", t3_ratio_il6_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_tnf_il10_adj_sex_age_L[1], " (", t3_ratio_tnf_il10_adj_sex_age_L[2], ", ", t3_ratio_tnf_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il10_adj_sex_age_L[1], " (", t3_ratio_il12_il10_adj_sex_age_L[2], ", ", t3_ratio_il12_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il10_adj_sex_age_L[1], " (", t3_ratio_ifn_il10_adj_sex_age_L[2], ", ", t3_ratio_ifn_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il4_il10_adj_sex_age_L[1], " (", t3_ratio_il4_il10_adj_sex_age_L[2], ", ", t3_ratio_il4_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il5_il10_adj_sex_age_L[1], " (", t3_ratio_il5_il10_adj_sex_age_L[2], ", ", t3_ratio_il5_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il13_il10_adj_sex_age_L[1], " (", t3_ratio_il13_il10_adj_sex_age_L[2], ", ", t3_ratio_il13_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il17_il10_adj_sex_age_L[1], " (", t3_ratio_il17_il10_adj_sex_age_L[2], ", ", t3_ratio_il17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il21_il10_adj_sex_age_L[1], " (", t3_ratio_il21_il10_adj_sex_age_L[2], ", ", t3_ratio_il21_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il2_il10_adj_sex_age_L[1], " (", t3_ratio_il2_il10_adj_sex_age_L[2], ", ", t3_ratio_il2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_gmc_il10_adj_sex_age_L[1], " (", t3_ratio_gmc_il10_adj_sex_age_L[2], ", ", t3_ratio_gmc_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il4_adj_sex_age_L[1], " (", t3_ratio_il12_il4_adj_sex_age_L[2], ", ", t3_ratio_il12_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il4_adj_sex_age_L[1], " (", t3_ratio_ifn_il4_adj_sex_age_L[2], ", ", t3_ratio_ifn_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il5_adj_sex_age_L[1], " (", t3_ratio_il12_il5_adj_sex_age_L[2], ", ", t3_ratio_il12_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il5_adj_sex_age_L[1], " (", t3_ratio_ifn_il5_adj_sex_age_L[2], ", ", t3_ratio_ifn_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il13_adj_sex_age_L[1], " (", t3_ratio_il12_il13_adj_sex_age_L[2], ", ", t3_ratio_il12_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il13_adj_sex_age_L[1], " (", t3_ratio_ifn_il13_adj_sex_age_L[2], ", ", t3_ratio_ifn_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il17_adj_sex_age_L[1], " (", t3_ratio_il12_il17_adj_sex_age_L[2], ", ", t3_ratio_il12_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il17_adj_sex_age_L[1], " (", t3_ratio_ifn_il17_adj_sex_age_L[2], ", ", t3_ratio_ifn_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il21_adj_sex_age_L[1], " (", t3_ratio_il12_il21_adj_sex_age_L[2], ", ", t3_ratio_il12_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il21_adj_sex_age_L[1], " (", t3_ratio_ifn_il21_adj_sex_age_L[2], ", ", t3_ratio_ifn_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_pro_il10_adj_sex_age_L[1], " (", t3_ratio_pro_il10_adj_sex_age_L[2], ", ", t3_ratio_pro_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_il10_adj_sex_age_L[1], " (", t3_ratio_th1_il10_adj_sex_age_L[2], ", ", t3_ratio_th1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th2_il10_adj_sex_age_L[1], " (", t3_ratio_th2_il10_adj_sex_age_L[2], ", ", t3_ratio_th2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th17_il10_adj_sex_age_L[1], " (", t3_ratio_th17_il10_adj_sex_age_L[2], ", ", t3_ratio_th17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th2_adj_sex_age_L[1], " (", t3_ratio_th1_th2_adj_sex_age_L[2], ", ", t3_ratio_th1_th2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th17_adj_sex_age_L[1], " (", t3_ratio_th1_th17_adj_sex_age_L[2], ", ", t3_ratio_th1_th17_adj_sex_age_L[3], ")", sep=""))

t3_ratio_il1_il10_adj_L <- round(t3_ratio_il1_il10_adj_L, 2)
t3_ratio_il6_il10_adj_L <- round(t3_ratio_il6_il10_adj_L, 2)
t3_ratio_tnf_il10_adj_L <- round(t3_ratio_tnf_il10_adj_L, 2)
t3_ratio_il12_il10_adj_L <- round(t3_ratio_il12_il10_adj_L, 2)
t3_ratio_ifn_il10_adj_L <- round(t3_ratio_ifn_il10_adj_L, 2)
t3_ratio_il4_il10_adj_L <- round(t3_ratio_il4_il10_adj_L, 2)
t3_ratio_il5_il10_adj_L <- round(t3_ratio_il5_il10_adj_L, 2)
t3_ratio_il13_il10_adj_L <- round(t3_ratio_il13_il10_adj_L, 2)
t3_ratio_il17_il10_adj_L <- round(t3_ratio_il17_il10_adj_L, 2)
t3_ratio_il21_il10_adj_L <- round(t3_ratio_il21_il10_adj_L, 2)
t3_ratio_il2_il10_adj_L <- round(t3_ratio_il2_il10_adj_L, 2)
t3_ratio_gmc_il10_adj_L <- round(t3_ratio_gmc_il10_adj_L, 2)
t3_ratio_il12_il4_adj_L <- round(t3_ratio_il12_il4_adj_L, 2)
t3_ratio_ifn_il4_adj_L <- round(t3_ratio_ifn_il4_adj_L, 2)
t3_ratio_il12_il5_adj_L <- round(t3_ratio_il12_il5_adj_L, 2)
t3_ratio_ifn_il5_adj_L <- round(t3_ratio_ifn_il5_adj_L, 2)
t3_ratio_il12_il13_adj_L <- round(t3_ratio_il12_il13_adj_L, 2)
t3_ratio_ifn_il13_adj_L <- round(t3_ratio_ifn_il13_adj_L, 2)
t3_ratio_il12_il17_adj_L <- round(t3_ratio_il12_il17_adj_L, 2)
t3_ratio_ifn_il17_adj_L <- round(t3_ratio_ifn_il17_adj_L, 2)
t3_ratio_il12_il21_adj_L <- round(t3_ratio_il12_il21_adj_L, 2)
t3_ratio_ifn_il21_adj_L <- round(t3_ratio_ifn_il21_adj_L, 2)
t3_ratio_pro_il10_adj_L <- round(t3_ratio_pro_il10_adj_L, 2)
t3_ratio_th1_il10_adj_L <- round(t3_ratio_th1_il10_adj_L, 2)
t3_ratio_th2_il10_adj_L <- round(t3_ratio_th2_il10_adj_L, 2)
t3_ratio_th17_il10_adj_L <- round(t3_ratio_th17_il10_adj_L, 2)
t3_ratio_th1_th2_adj_L <- round(t3_ratio_th1_th2_adj_L, 2)
t3_ratio_th1_th17_adj_L <- round(t3_ratio_th1_th17_adj_L, 2)

adjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_adj_L[1], " (", t3_ratio_il1_il10_adj_L[2], ", ", t3_ratio_il1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il6_il10_adj_L[1], " (", t3_ratio_il6_il10_adj_L[2], ", ", t3_ratio_il6_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_tnf_il10_adj_L[1], " (", t3_ratio_tnf_il10_adj_L[2], ", ", t3_ratio_tnf_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il10_adj_L[1], " (", t3_ratio_il12_il10_adj_L[2], ", ", t3_ratio_il12_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il10_adj_L[1], " (", t3_ratio_ifn_il10_adj_L[2], ", ", t3_ratio_ifn_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il4_il10_adj_L[1], " (", t3_ratio_il4_il10_adj_L[2], ", ", t3_ratio_il4_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il5_il10_adj_L[1], " (", t3_ratio_il5_il10_adj_L[2], ", ", t3_ratio_il5_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il13_il10_adj_L[1], " (", t3_ratio_il13_il10_adj_L[2], ", ", t3_ratio_il13_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il17_il10_adj_L[1], " (", t3_ratio_il17_il10_adj_L[2], ", ", t3_ratio_il17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il21_il10_adj_L[1], " (", t3_ratio_il21_il10_adj_L[2], ", ", t3_ratio_il21_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il2_il10_adj_L[1], " (", t3_ratio_il2_il10_adj_L[2], ", ", t3_ratio_il2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_gmc_il10_adj_L[1], " (", t3_ratio_gmc_il10_adj_L[2], ", ", t3_ratio_gmc_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il4_adj_L[1], " (", t3_ratio_il12_il4_adj_L[2], ", ", t3_ratio_il12_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il4_adj_L[1], " (", t3_ratio_ifn_il4_adj_L[2], ", ", t3_ratio_ifn_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il5_adj_L[1], " (", t3_ratio_il12_il5_adj_L[2], ", ", t3_ratio_il12_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il5_adj_L[1], " (", t3_ratio_ifn_il5_adj_L[2], ", ", t3_ratio_ifn_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il13_adj_L[1], " (", t3_ratio_il12_il13_adj_L[2], ", ", t3_ratio_il12_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il13_adj_L[1], " (", t3_ratio_ifn_il13_adj_L[2], ", ", t3_ratio_ifn_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il17_adj_L[1], " (", t3_ratio_il12_il17_adj_L[2], ", ", t3_ratio_il12_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il17_adj_L[1], " (", t3_ratio_ifn_il17_adj_L[2], ", ", t3_ratio_ifn_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il21_adj_L[1], " (", t3_ratio_il12_il21_adj_L[2], ", ", t3_ratio_il12_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il21_adj_L[1], " (", t3_ratio_ifn_il21_adj_L[2], ", ", t3_ratio_ifn_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_pro_il10_adj_L[1], " (", t3_ratio_pro_il10_adj_L[2], ", ", t3_ratio_pro_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_il10_adj_L[1], " (", t3_ratio_th1_il10_adj_L[2], ", ", t3_ratio_th1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th2_il10_adj_L[1], " (", t3_ratio_th2_il10_adj_L[2], ", ", t3_ratio_th2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th17_il10_adj_L[1], " (", t3_ratio_th17_il10_adj_L[2], ", ", t3_ratio_th17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_th2_adj_L[1], " (", t3_ratio_th1_th2_adj_L[2], ", ", t3_ratio_th1_th2_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_th17_adj_L[1], " (", t3_ratio_th1_th17_adj_L[2], ", ", t3_ratio_th1_th17_adj_L[3], ")", sep=""))

# Table 5: Effect of intervention on cytokine ratios at age 28 months
tbl5 <- data.table(
  "Outcome, Arm" = outcometbl5,
  "N" = Ntbl5, 
  "Absolute Mean" = absmeantbl5,
  "Absolute SD" = abssdtbl5,
  "Mean" = meantbl5, 
  "SD" = sdtbl5,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl5,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl5, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl5
)

write.csv(tbl5, file=here('tables/immune/immune_main/immune_table5.csv'))
print(xtable(tbl5), type="html", file=here("tables/immune/immune_main/immune_table5.html"))


#### TABLE 6 ####
outcometbl6 <- c(paste("Ln ÎIL-1", "Î²", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln ÎIL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ÎTNF-", "Î±", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln ÎIL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ÎIFN-", "Î³", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln ÎIL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎIL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ÎGM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ÎIGF-1 (", "Î¼", "g/L)", sep=""), "Control", "Nutrition + WSH")

Ntbl6 <- c(" ", as.character(d23_ln_il1_N_tr$d23_ln_il1_N_tr[1]), as.character(d23_ln_il1_N_tr$d23_ln_il1_N_tr[2]), 
           " ", as.character(d23_ln_il6_N_tr$d23_ln_il6_N_tr[1]), as.character(d23_ln_il6_N_tr$d23_ln_il6_N_tr[2]),
           " ", as.character(d23_ln_tnf_N_tr$d23_ln_tnf_N_tr[1]), as.character(d23_ln_tnf_N_tr$d23_ln_tnf_N_tr[2]),
           " ", as.character(d23_ln_il12_N_tr$d23_ln_il12_N_tr[1]), as.character(d23_ln_il12_N_tr$d23_ln_il12_N_tr[2]),
           " ", as.character(d23_ln_ifn_N_tr$d23_ln_ifn_N_tr[1]), as.character(d23_ln_ifn_N_tr$d23_ln_ifn_N_tr[2]),
           " ", as.character(d23_ln_il4_N_tr$d23_ln_il4_N_tr[1]), as.character(d23_ln_il4_N_tr$d23_ln_il4_N_tr[2]),
           " ", as.character(d23_ln_il5_N_tr$d23_ln_il5_N_tr[1]), as.character(d23_ln_il5_N_tr$d23_ln_il5_N_tr[2]),
           " ", as.character(d23_ln_il13_N_tr$d23_ln_il13_N_tr[1]), as.character(d23_ln_il13_N_tr$d23_ln_il13_N_tr[2]),
           " ", as.character(d23_ln_il17_N_tr$d23_ln_il17_N_tr[1]), as.character(d23_ln_il17_N_tr$d23_ln_il17_N_tr[2]),
           " ", as.character(d23_ln_il21_N_tr$d23_ln_il21_N_tr[1]), as.character(d23_ln_il21_N_tr$d23_ln_il21_N_tr[2]),
           " ", as.character(d23_ln_il10_N_tr$d23_ln_il10_N_tr[1]), as.character(d23_ln_il10_N_tr$d23_ln_il10_N_tr[2]),
           " ", as.character(d23_ln_il2_N_tr$d23_ln_il2_N_tr[1]), as.character(d23_ln_il2_N_tr$d23_ln_il2_N_tr[2]),
           " ", as.character(d23_ln_gmc_N_tr$d23_ln_gmc_N_tr[1]), as.character(d23_ln_gmc_N_tr$d23_ln_gmc_N_tr[2]),
           " ", as.character(d23_ln_igf_N_tr$d23_ln_igf_N_tr[1]), as.character(d23_ln_igf_N_tr$d23_ln_igf_N_tr[2]))

absmeantbl6 <- c(" ", as.character(round(abs_d23_il1_N_tr$mean[1], 2)), as.character(round(abs_d23_il1_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_d23_il6_N_tr$mean[1], 2)), as.character(round(abs_d23_il6_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_tnf_N_tr$mean[1], 2)), as.character(round(abs_d23_tnf_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il12_N_tr$mean[1], 2)), as.character(round(abs_d23_il12_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_ifn_N_tr$mean[1], 2)), as.character(round(abs_d23_ifn_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il4_N_tr$mean[1], 2)), as.character(round(abs_d23_il4_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il5_N_tr$mean[1], 2)), as.character(round(abs_d23_il5_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il13_N_tr$mean[1], 2)), as.character(round(abs_d23_il13_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il17_N_tr$mean[1], 2)), as.character(round(abs_d23_il17_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il21_N_tr$mean[1], 2)), as.character(round(abs_d23_il21_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il10_N_tr$mean[1], 2)), as.character(round(abs_d23_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_il2_N_tr$mean[1], 2)), as.character(round(abs_d23_il2_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_gmc_N_tr$mean[1], 2)), as.character(round(abs_d23_gmc_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_d23_igf_N_tr$mean[1], 2)), as.character(round(abs_d23_igf_N_tr$mean[2], 2)))

abssdtbl6 <- c(" ", as.character(round(abs_d23_il1_N_tr$sd[1], 2)), as.character(round(abs_d23_il1_N_tr$sd[2], 2)), 
                 " ", as.character(round(abs_d23_il6_N_tr$sd[1], 2)), as.character(round(abs_d23_il6_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_tnf_N_tr$sd[1], 2)), as.character(round(abs_d23_tnf_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il12_N_tr$sd[1], 2)), as.character(round(abs_d23_il12_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_ifn_N_tr$sd[1], 2)), as.character(round(abs_d23_ifn_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il4_N_tr$sd[1], 2)), as.character(round(abs_d23_il4_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il5_N_tr$sd[1], 2)), as.character(round(abs_d23_il5_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il13_N_tr$sd[1], 2)), as.character(round(abs_d23_il13_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il17_N_tr$sd[1], 2)), as.character(round(abs_d23_il17_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il21_N_tr$sd[1], 2)), as.character(round(abs_d23_il21_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il10_N_tr$sd[1], 2)), as.character(round(abs_d23_il10_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_il2_N_tr$sd[1], 2)), as.character(round(abs_d23_il2_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_gmc_N_tr$sd[1], 2)), as.character(round(abs_d23_gmc_N_tr$sd[2], 2)),
                 " ", as.character(round(abs_d23_igf_N_tr$sd[1], 2)), as.character(round(abs_d23_igf_N_tr$sd[2], 2)))

meantbl6 <- c(" ", as.character(round(d23_ln_il1_N_tr$mean[1], 2)), as.character(round(d23_ln_il1_N_tr$mean[2], 2)), 
              " ", as.character(round(d23_ln_il6_N_tr$mean[1], 2)), as.character(round(d23_ln_il6_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_tnf_N_tr$mean[1], 2)), as.character(round(d23_ln_tnf_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il12_N_tr$mean[1], 2)), as.character(round(d23_ln_il12_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_ifn_N_tr$mean[1], 2)), as.character(round(d23_ln_ifn_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il4_N_tr$mean[1], 2)), as.character(round(d23_ln_il4_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il5_N_tr$mean[1], 2)), as.character(round(d23_ln_il5_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il13_N_tr$mean[1], 2)), as.character(round(d23_ln_il13_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il17_N_tr$mean[1], 2)), as.character(round(d23_ln_il17_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il21_N_tr$mean[1], 2)), as.character(round(d23_ln_il21_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il10_N_tr$mean[1], 2)), as.character(round(d23_ln_il10_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_il2_N_tr$mean[1], 2)), as.character(round(d23_ln_il2_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_gmc_N_tr$mean[1], 2)), as.character(round(d23_ln_gmc_N_tr$mean[2], 2)),
              " ", as.character(round(d23_ln_igf_N_tr$mean[1], 2)), as.character(round(d23_ln_igf_N_tr$mean[2], 2)))

sdtbl6 <- c(" ", as.character(round(d23_ln_il1_N_tr$sd[1], 2)), as.character(round(d23_ln_il1_N_tr$sd[2], 2)), 
            " ", as.character(round(d23_ln_il6_N_tr$sd[1], 2)), as.character(round(d23_ln_il6_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_tnf_N_tr$sd[1], 2)), as.character(round(d23_ln_tnf_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il12_N_tr$sd[1], 2)), as.character(round(d23_ln_il12_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_ifn_N_tr$sd[1], 2)), as.character(round(d23_ln_ifn_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il4_N_tr$sd[1], 2)), as.character(round(d23_ln_il4_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il5_N_tr$sd[1], 2)), as.character(round(d23_ln_il5_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il13_N_tr$sd[1], 2)), as.character(round(d23_ln_il13_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il17_N_tr$sd[1], 2)), as.character(round(d23_ln_il17_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il21_N_tr$sd[1], 2)), as.character(round(d23_ln_il21_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il10_N_tr$sd[1], 2)), as.character(round(d23_ln_il10_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_il2_N_tr$sd[1], 2)), as.character(round(d23_ln_il2_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_gmc_N_tr$sd[1], 2)), as.character(round(d23_ln_gmc_N_tr$sd[2], 2)),
            " ", as.character(round(d23_ln_igf_N_tr$sd[1], 2)), as.character(round(d23_ln_igf_N_tr$sd[2], 2)))

d23_ln_il1_unadj_L <- round(d23_ln_il1_unadj_L, 2)
d23_ln_il6_unadj_L <- round(d23_ln_il6_unadj_L, 2)
d23_ln_tnf_unadj_L <- round(d23_ln_tnf_unadj_L, 2)
d23_ln_il12_unadj_L <- round(d23_ln_il12_unadj_L, 2)
d23_ln_ifn_unadj_L <- round(d23_ln_ifn_unadj_L, 2)
d23_ln_il4_unadj_L <- round(d23_ln_il4_unadj_L, 2)
d23_ln_il5_unadj_L <- round(d23_ln_il5_unadj_L, 2)
d23_ln_il13_unadj_L <- round(d23_ln_il13_unadj_L, 2)
d23_ln_il17_unadj_L <- round(d23_ln_il17_unadj_L, 2)
d23_ln_il21_unadj_L <- round(d23_ln_il21_unadj_L, 2)
d23_ln_il10_unadj_L <- round(d23_ln_il10_unadj_L, 2)
d23_ln_il2_unadj_L <- round(d23_ln_il2_unadj_L, 2)
d23_ln_gmc_unadj_L <- round(d23_ln_gmc_unadj_L, 2)
d23_ln_igf_unadj_L <- round(d23_ln_igf_unadj_L, 2)

unadjtbl6 <- c(" ", " ", paste(d23_ln_il1_unadj_L[1], " (", d23_ln_il1_unadj_L[2], ", ", d23_ln_il1_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il6_unadj_L[1], " (", d23_ln_il6_unadj_L[2], ", ", d23_ln_il6_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_tnf_unadj_L[1], " (", d23_ln_tnf_unadj_L[2], ", ", d23_ln_tnf_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il12_unadj_L[1], " (", d23_ln_il12_unadj_L[2], ", ", d23_ln_il12_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_ifn_unadj_L[1], " (", d23_ln_ifn_unadj_L[2], ", ", d23_ln_ifn_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il4_unadj_L[1], " (", d23_ln_il4_unadj_L[2], ", ", d23_ln_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il5_unadj_L[1], " (", d23_ln_il5_unadj_L[2], ", ", d23_ln_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il13_unadj_L[1], " (", d23_ln_il13_unadj_L[2], ", ", d23_ln_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il17_unadj_L[1], " (", d23_ln_il17_unadj_L[2], ", ", d23_ln_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il21_unadj_L[1], " (", d23_ln_il21_unadj_L[2], ", ", d23_ln_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il10_unadj_L[1], " (", d23_ln_il10_unadj_L[2], ", ", d23_ln_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il2_unadj_L[1], " (", d23_ln_il2_unadj_L[2], ", ", d23_ln_il2_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_gmc_unadj_L[1], " (", d23_ln_gmc_unadj_L[2], ", ", d23_ln_gmc_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_igf_unadj_L[1], " (", d23_ln_igf_unadj_L[2], ", ", d23_ln_igf_unadj_L[3], ")", sep=""))

d23_ln_il1_adj_sex_age_L <- round(d23_ln_il1_adj_sex_age_L, 2)
d23_ln_il6_adj_sex_age_L <- round(d23_ln_il6_adj_sex_age_L, 2)
d23_ln_tnf_adj_sex_age_L <- round(d23_ln_tnf_adj_sex_age_L, 2)
d23_ln_il12_adj_sex_age_L <- round(d23_ln_il12_adj_sex_age_L, 2)
d23_ln_ifn_adj_sex_age_L <- round(d23_ln_ifn_adj_sex_age_L, 2)
d23_ln_il4_adj_sex_age_L <- round(d23_ln_il4_adj_sex_age_L, 2)
d23_ln_il5_adj_sex_age_L <- round(d23_ln_il5_adj_sex_age_L, 2)
d23_ln_il13_adj_sex_age_L <- round(d23_ln_il13_adj_sex_age_L, 2)
d23_ln_il17_adj_sex_age_L <- round(d23_ln_il17_adj_sex_age_L, 2)
d23_ln_il21_adj_sex_age_L <- round(d23_ln_il21_adj_sex_age_L, 2)
d23_ln_il10_adj_sex_age_L <- round(d23_ln_il10_adj_sex_age_L, 2)
d23_ln_il2_adj_sex_age_L <- round(d23_ln_il2_adj_sex_age_L, 2)
d23_ln_gmc_adj_sex_age_L <- round(d23_ln_gmc_adj_sex_age_L, 2)
d23_ln_igf_adj_sex_age_L <- round(d23_ln_igf_adj_sex_age_L, 2)

asadjtbl6 <- c(" ", " ", paste(d23_ln_il1_adj_sex_age_L[1], " (", d23_ln_il1_adj_sex_age_L[2], ", ", d23_ln_il1_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il6_adj_sex_age_L[1], " (", d23_ln_il6_adj_sex_age_L[2], ", ", d23_ln_il6_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_tnf_adj_sex_age_L[1], " (", d23_ln_tnf_adj_sex_age_L[2], ", ", d23_ln_tnf_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il12_adj_sex_age_L[1], " (", d23_ln_il12_adj_sex_age_L[2], ", ", d23_ln_il12_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_ifn_adj_sex_age_L[1], " (", d23_ln_ifn_adj_sex_age_L[2], ", ", d23_ln_ifn_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il4_adj_sex_age_L[1], " (", d23_ln_il4_adj_sex_age_L[2], ", ", d23_ln_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il5_adj_sex_age_L[1], " (", d23_ln_il5_adj_sex_age_L[2], ", ", d23_ln_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il13_adj_sex_age_L[1], " (", d23_ln_il13_adj_sex_age_L[2], ", ", d23_ln_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il17_adj_sex_age_L[1], " (", d23_ln_il17_adj_sex_age_L[2], ", ", d23_ln_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il21_adj_sex_age_L[1], " (", d23_ln_il21_adj_sex_age_L[2], ", ", d23_ln_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il10_adj_sex_age_L[1], " (", d23_ln_il10_adj_sex_age_L[2], ", ", d23_ln_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il2_adj_sex_age_L[1], " (", d23_ln_il2_adj_sex_age_L[2], ", ", d23_ln_il2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_gmc_adj_sex_age_L[1], " (", d23_ln_gmc_adj_sex_age_L[2], ", ", d23_ln_gmc_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_igf_adj_sex_age_L[1], " (", d23_ln_igf_adj_sex_age_L[2], ", ", d23_ln_igf_adj_sex_age_L[3], ")", sep=""))

d23_ln_il1_adj_L <- round(d23_ln_il1_adj_L, 2)
d23_ln_il6_adj_L <- round(d23_ln_il6_adj_L, 2)
d23_ln_tnf_adj_L <- round(d23_ln_tnf_adj_L, 2)
d23_ln_il12_adj_L <- round(d23_ln_il12_adj_L, 2)
d23_ln_ifn_adj_L <- round(d23_ln_ifn_adj_L, 2)
d23_ln_il4_adj_L <- round(d23_ln_il4_adj_L, 2)
d23_ln_il5_adj_L <- round(d23_ln_il5_adj_L, 2)
d23_ln_il13_adj_L <- round(d23_ln_il13_adj_L, 2)
d23_ln_il17_adj_L <- round(d23_ln_il17_adj_L, 2)
d23_ln_il21_adj_L <- round(d23_ln_il21_adj_L, 2)
d23_ln_il10_adj_L <- round(d23_ln_il10_adj_L, 2)
d23_ln_il2_adj_L <- round(d23_ln_il2_adj_L, 2)
d23_ln_gmc_adj_L <- round(d23_ln_gmc_adj_L, 2)
d23_ln_igf_adj_L <- round(d23_ln_igf_adj_L, 2)

adjtbl6 <- c(" ", " ", paste(d23_ln_il1_adj_L[1], " (", d23_ln_il1_adj_L[2], ", ", d23_ln_il1_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il6_adj_L[1], " (", d23_ln_il6_adj_L[2], ", ", d23_ln_il6_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_tnf_adj_L[1], " (", d23_ln_tnf_adj_L[2], ", ", d23_ln_tnf_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il12_adj_L[1], " (", d23_ln_il12_adj_L[2], ", ", d23_ln_il12_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_ifn_adj_L[1], " (", d23_ln_ifn_adj_L[2], ", ", d23_ln_ifn_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il4_adj_L[1], " (", d23_ln_il4_adj_L[2], ", ", d23_ln_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il5_adj_L[1], " (", d23_ln_il5_adj_L[2], ", ", d23_ln_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il13_adj_L[1], " (", d23_ln_il13_adj_L[2], ", ", d23_ln_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il17_adj_L[1], " (", d23_ln_il17_adj_L[2], ", ", d23_ln_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il21_adj_L[1], " (", d23_ln_il21_adj_L[2], ", ", d23_ln_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il10_adj_L[1], " (", d23_ln_il10_adj_L[2], ", ", d23_ln_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il2_adj_L[1], " (", d23_ln_il2_adj_L[2], ", ", d23_ln_il2_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_gmc_adj_L[1], " (", d23_ln_gmc_adj_L[2], ", ", d23_ln_gmc_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_igf_adj_L[1], " (", d23_ln_igf_adj_L[2], ", ", d23_ln_igf_adj_L[3], ")", sep=""))

# Table 6: Effect of intervention on change in individual immune status and growth factor measurements between ages 14 and 28 months
tbl6 <- data.table(
  "Outcome, Arm" = outcometbl6,
  "N" = Ntbl6, 
  "Absolute Mean" = absmeantbl6,
  "Absolute SD" = abssdtbl6,
  "Mean" = meantbl6, 
  "SD" = sdtbl6,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl6,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl6, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl6
)

write.csv(tbl6, file=here('tables/immune/immune_main/immune_table6.csv'))
print(xtable(tbl6), type="html", file=here("tables/immune/immune_main/immune_table6.html"))

