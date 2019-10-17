rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

#### TABLE 1 ####

#calculating overall N by arm
Nctrl<-length(ages$tr[ages$tr=="Control"])
Nwsh<-length(ages$tr[ages$tr=="Nutrition + WSH"])

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

momage<-meansdfunc(ages$momage)
momeduy<-meansdfunc(ages$momeduy)
dadeduy<-meansdfunc(ages$dadeduy)
dadagri<-npercfunc(ages$dadagri)
Nhh<-meansdfunc(ages$Nhh)
elec<-npercfunc(ages$elec)
cement<-npercfunc(ages$cement)

acresctrlm<-round(mean(ages$landacre[ages$tr=="Control"], na.rm=TRUE), 2)
acresctrlsd<-round(sd(ages$landacre[ages$tr=="Control"], na.rm=TRUE), 2)
acreswshm<-round(mean(ages$landacre[ages$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acreswshsd<-round(mean(ages$landacre[ages$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acres<-c(acresctrlm, acresctrlsd, acreswshm, acreswshsd)

tubewell<-npercfunc(ages$tubewell)
storewater<-npercfunc(ages$storewat)
treatwater<-npercfunc(ages$treatwat)
waterdis<-meansdfunc(ages$watmin)
odmen<-npercfunc(ages$odmen)
odwomen<-npercfunc(ages$odwom)
odchild815<-npercfunc(ages$odch815)
odchild38<-npercfunc(ages$odch38)
odchild03<-npercfunc(ages$odchu3)
latowned<-npercfunc(ages$latown)
latslab<-npercfunc(ages$latslab)
latseal<-npercfunc(ages$latseal)
latfeces<-npercfunc(ages$latfeces)
potty<-npercfunc(ages$potty)
feceshouse<-npercfunc(ages$humfeces)
feceschildarea<-npercfunc(ages$humfecesch)
handlatwater<-npercfunc(ages$hwlatwat)
handlatsoap<-npercfunc(ages$hwlatsoap)
handkitwater<-npercfunc(ages$hwkitwat)
handkitsoap<-npercfunc(ages$hwkitsoap)

fsctrln<-length(ages$hfiacat[ages$tr=="Control" & ages$hfiacat=="Food Secure"])
fsctrlperc<-round(fsctrln/length(ages$hfiacat[ages$tr=="Control"])*100)
fswshn<-length(ages$hfiacat[ages$tr=="Nutrition + WSH" & ages$hfiacat=="Food Secure"])
fswshperc<-round(fswshn/length(ages$hfiacat[ages$tr=="Nutrition + WSH"])*100)
foodsecure<-c(fsctrln, fsctrlperc, fswshn, fswshperc)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

ctrl<-c(" ", charobject(momage, 1),charobject(momeduy, 1), " ", charobject(dadeduy, 1), charobjectperc(dadagri, 1),
        " ", charobject(Nhh, 1), charobjectperc(elec, 1), charobjectperc(cement, 1), charobject(acres, 1),
        " ", charobjectperc(tubewell, 1), charobjectperc(storewater, 1), charobjectperc(treatwater, 1), charobject(waterdis, 1), 
        " ", " ", charobjectperc(odmen, 1), charobjectperc(odwomen, 1), charobjectperc(odchild815, 1), charobjectperc(odchild38, 1), charobjectperc(odchild03, 1), 
        " ", charobjectperc(latowned, 1), charobjectperc(latslab, 1), charobjectperc(latseal, 1), charobjectperc(latfeces, 1),
        charobjectperc(potty, 1), 
        " ", charobjectperc(feceshouse, 1), charobjectperc(feceschildarea, 1), 
        " ", " ", charobjectperc(handlatwater, 1), charobjectperc(handlatsoap, 1), 
        " ", charobjectperc(handkitwater, 1), charobjectperc(handkitsoap, 1), 
        " ", charobjectperc(foodsecure, 1))
wsh<-c(" ", charobject(momage, 3),charobject(momeduy, 3), " ", charobject(dadeduy, 3), charobjectperc(dadagri, 3),
       " ", charobject(Nhh, 3), charobjectperc(elec, 3), charobjectperc(cement, 3), charobject(acres, 3),
       " ", charobjectperc(tubewell, 3), charobjectperc(storewater, 3), charobjectperc(treatwater, 3), charobject(waterdis, 3), 
       " ", " ", charobjectperc(odmen, 3), charobjectperc(odwomen, 3), charobjectperc(odchild815, 3), charobjectperc(odchild38, 3), charobjectperc(odchild03, 3), 
       " ", charobjectperc(latowned, 3), charobjectperc(latslab, 3), charobjectperc(latseal, 3), charobjectperc(latfeces, 3),
       charobjectperc(potty, 3), 
       " ", charobjectperc(feceshouse, 3), charobjectperc(feceschildarea, 3), 
       " ", " ", charobjectperc(handlatwater, 3), charobjectperc(handlatsoap, 3), 
       " ", charobjectperc(handkitwater, 3), charobjectperc(handkitsoap, 3), 
       " ", charobjectperc(foodsecure, 3))

# Table 1: Enrollment characteristics by intervention group
tbl1 <- data.table(
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
  "Control (N=402)" = ctrl,
  "N + WSH (N=404)" = wsh
)

write.csv(tbl1, file=here('tables/miso9-immune-table1.csv'))
print(xtable(tbl1), type="html", file=here("tables/miso9-immune-table1.html"))


#### TABLE 2 ####
outcometbl2 <- c(paste("Ln IL-1", "β", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "α", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln CRP (mg/L)", "Control", "Nutrition + WSH", 
                 "Ln IL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln AGP (g/L)", "Control", "Nutrition + WSH",
                 paste("Ln IGF-1 (", "μ", "g/L)", sep=""), "Control", "Nutrition + WSH")

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

absmeantbl2 <-c(" ", as.character(round(abs_il1_t2_N_tr$mean[1], 2)), as.character(round(abs_il1_t2_N_tr$mean[2], 2)),
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
  "Mean" = meantbl2, 
  "SD" = sdtbl2,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl2,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl2, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl2)

write.csv(tbl2, file=here('tables/mis09-immune-table2.csv'))
print(xtable(tbl2), type="html", file=here("tables/miso9-immune-table2.html"))



#### TABLE 3 ####

outcometbl3 <- c(paste("Ln IL-1", "β", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "α", "/IL-10", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-5/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-13/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-17A/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-21/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-2/IL-10", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF/IL-10", "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-4", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-4", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-5", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-5", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-13", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-13", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-17A", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-17A", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-21", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-21", sep=""), "Control", "Nutrition + WSH",
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
                 " ", as.character(round(abs_t2_ratio_pro_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_pro_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_th1_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_th1_il10_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_th2_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_th2_il10_N_tr$mean[2], 2)),    
                 " ", as.character(round(abs_t2_ratio_th17_il10_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_th17_il10_N_tr$mean[2], 2)), 
                 " ", as.character(round(abs_t2_ratio_th1_th2_N_tr$mean[1], 2)), as.character(round(abs_t2_ratio_th1_th2_N_tr$mean[2], 2)),  
                 " ", as.character(round(abs_t2_ratio_th1_th17_N_tr$mean[1], 2)),  as.character(round(abs_t2_ratio_th1_th17_N_tr$mean[2], 2)))

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
  "Mean" = meantbl3, 
  "SD" = sdtbl3,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl3,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl3, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl3
)

write.csv(tbl3, file=here('tables/mis09-immune-table3.csv'))
print(xtable(tbl3), type="html", file=here("tables/miso9-immune-table3.html"))


#### TABLE 4 ####
outcometbl4 <- c(paste("Ln IL-1", "β", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "α", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln IL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln IGF-1 (", "μ", "g/L)", sep=""), "Control", "Nutrition + WSH")

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
  "Mean" = meantbl4, 
  "SD" = sdtbl4,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl4,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl4, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl4
)

write.csv(tbl4, file=here('tables/miso9-immune-table4.csv'))
print(xtable(tbl4), type="html", file=here("tables/miso9-immune-table4.html"))



#### TABLE 5 ####
outcometbl5 <- c(paste("Ln IL-1", "β", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", "α", "/IL-10", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-5/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-13/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-17A/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-21/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-2/IL-10", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF/IL-10", "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-4", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-4", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-5", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-5", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-13", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-13", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-17A", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-17A", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-21", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", "γ", "/IL-21", sep=""), "Control", "Nutrition + WSH",
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
                 " ", as.character(round(abs_t3_ratio_pro_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_pro_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_th1_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_th1_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_th2_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_th2_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_th17_il10_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_th17_il10_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_th1_th2_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_th1_th2_N_tr$mean[2], 2)),
                 " ", as.character(round(abs_t3_ratio_th1_th17_N_tr$mean[1], 2)), as.character(round(abs_t3_ratio_th1_th17_N_tr$mean[2], 2)))

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
  "Mean" = meantbl5, 
  "SD" = sdtbl5,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl5,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl5, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl5
)

write.csv(tbl5, file=here('tables/miso9-immune-table5.csv'))
print(xtable(tbl5), type="html", file=here("tables/miso9-immune-table5.html"))


#### TABLE 6 ####
outcometbl6 <- c(paste("Ln ΔIL-1", "β", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln ΔIL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ΔTNF-", "α", " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln ΔIL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ΔIFN-", "γ", " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln ΔIL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔIL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln ΔGM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln ΔIGF-1 (", "μ", "g/L)", sep=""), "Control", "Nutrition + WSH")

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
  "Mean" = meantbl6, 
  "SD" = sdtbl6,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl6,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl6, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl6
)

write.csv(tbl6, file=here('tables/miso9-immune-table6.csv'))
print(xtable(tbl6), type="html", file=here("tables/miso9-immune-table6.html"))

