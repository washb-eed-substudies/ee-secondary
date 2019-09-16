rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))

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


