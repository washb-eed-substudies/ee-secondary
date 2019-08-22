rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))
source(here('audrie R scripts/immune/bangladesh-immune-subgroup.R'))
source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

#calculating overall N by arm
Nctrl<-length(ages$tr[ages$tr=="Control"])
Nwsh<-length(ages$tr[ages$tr=="Nutrition + WSH"])

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(variable) {
  ctrlmean<-round(mean(variable[ages$tr=="Control"], na.rm=TRUE), 2)
  ctrlsd<-round(sd(variable[ages$tr=="Control"], na.rm=TRUE), 2)
  wshmean<-round(mean(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
  wshsd<-round(sd(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
  c(ctrlmean, ctrlsd, wshmean, wshsd)
}

npercfunc <- function(variable) {
  ctrln<-sum(variable[ages$tr=="Control"], na.rm=TRUE)
  ctrlperc<-round(mean(variable[ages$tr=="Control"], na.rm=TRUE)*100, 2)
  wshn<-sum(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE)
  wshperc<-round(mean(variable[ages$tr=="Nutrition + WSH"], na.rm=TRUE)*100, 2)
  c(ctrln, ctrlperc, wshn, wshperc)
}

momage<-meansdfunc(ages$momage)
momeduy<-meansdfunc(ages$momeduy)
dadeduy<-meansdfunc(ages$dadeduy)
dadagri<-npercfunc(ages$dadagri)
Nhh<-meansdfunc(ages$Nhh)
elec<-npercfunc(ages$elec)
cement<-npercfunc(ages$cement)
acres<-meansdfunc(ages$landacre)
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
handlat<-npercfunc(ages$hwlat)
handlatwater<-npercfunc(ages$hwlatwat)
handlatsoap<-npercfunc(ages$hwlatsoap)
handkit<-npercfunc(ages$hwkit)
handkitwater<-npercfunc(ages$hwkitwat)
handkitsoap<-npercfunc(ages$hwkitsoap)
foodsecure<-meansdfunc(ages$hfias)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], "(", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], "(", variable[num+1], "%)", sep="")
}

ctrl<-c("%/mean", " ", charobject(momage, 1),charobject(momeduy, 1), " ", charobject(dadeduy, 1), charobjectperc(dadagri, 1),
        " ", charobject(Nhh, 1), charobjectperc(elec, 1), charobjectperc(cement, 1), charobject(acres, 1),
        " ", charobjectperc(tubewell, 1), charobjectperc(storewater, 1), charobjectperc(treatwater, 1), charobject(waterdis, 1), 
        " ", " ", charobjectperc(odmen, 1), charobjectperc(odwomen, 1), charobjectperc(odchild815, 1), charobjectperc(odchild38, 1), charobjectperc(odchild03, 1), 
        " ", charobjectperc(latowned, 1), charobjectperc(latslab, 1), charobjectperc(latseal, 1), charobjectperc(latfeces, 1),
        charobjectperc(potty, 1), 
        " ", charobjectperc(feceshouse, 1), charobjectperc(feceschildarea, 1), 
        " ", charobjectperc(handlat, 1), charobjectperc(handlatwater, 1), charobjectperc(handlatsoap, 1), 
        charobjectperc(handkit, 1), charobjectperc(handkitwater, 1), charobjectperc(handkitsoap, 1), 
        " ", charobject(foodsecure, 1))
wsh<-c("%/mean", " ", charobject(momage, 3),charobject(momeduy, 3), " ", charobject(dadeduy, 3), charobjectperc(dadagri, 3),
       " ", charobject(Nhh, 3), charobjectperc(elec, 3), charobjectperc(cement, 3), charobject(acres, 3),
       " ", charobjectperc(tubewell, 3), charobjectperc(storewater, 3), charobjectperc(treatwater, 3), charobject(waterdis, 3), 
       " ", " ", charobjectperc(odmen, 3), charobjectperc(odwomen, 3), charobjectperc(odchild815, 3), charobjectperc(odchild38, 3), charobjectperc(odchild03, 3), 
       " ", charobjectperc(latowned, 3), charobjectperc(latslab, 3), charobjectperc(latseal, 3), charobjectperc(latfeces, 3),
       charobjectperc(potty, 3), 
       " ", charobjectperc(feceshouse, 3), charobjectperc(feceschildarea, 3), 
       " ", charobjectperc(handlat, 3), charobjectperc(handlatwater, 3), charobjectperc(handlatsoap, 3), 
       charobjectperc(handkit, 3), charobjectperc(handkitwater, 3), charobjectperc(handkitsoap, 3), 
       " ", charobject(foodsecure, 3))
  
# Table 1: Enrollment characteristics by intervention group
tbl1 <- data.table(
  "No. of compounds:" = c(" ", "Maternal", "Age(years)", "Years of education", 
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

write.csv(tbl1, file=here('tables/miso9-table1.csv'))
