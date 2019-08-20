
#---------------------------------------
# EE-BD-urine-ipcw.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# EED substudy -IPCW analysis for missing 
# outcomes of urine-based biomarkers
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(tidyverse)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
dim(d)
treatment<-d
# levels(treatment$tr)
# treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
# levels(treatment$tr)
#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)
dim(enrol)

#Load in urine survey and ipcw datasets
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("BD-EE-urine.csv")
ipcw<-read.csv("BD-EE-ipcw.csv", stringsAsFactors = T) %>% select(-c(tr,block))

#Load in L/M outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
outcomes<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")
outcomes$childid<-as.numeric(outcomes$childid)
load("urine_volume.Rdata")

#Load in urine survey data
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
urine<-read.csv("BD-EE-urine.csv")
dim(urine)

#Drop and merge fixed urine volumes
urine<-urine %>% subset(select=-c(urineVol_t1,urineVol_t2,urineVol_t3))
urine<-merge(urine, urineVol, by=c("dataid", "childNo"))



dim(urine)
dim(outcomes)
urine<-left_join(urine,outcomes, by="childid")
dim(urine)





#Merge in urine outcomes
urine_outcomes<-subset(urine, select=c(dataid,childNo, 
                                       staffid1, staffid2, staffid3, 
                                       month1, month2, month3, 
                                       aged1, aged2,aged3,
                                       LMvol_t1,LMvol_t2,LMvol_t3,
                                       urineVol_t1, urineVol_t2, urineVol_t3,
                                       Lact1, Lact2, Lact3, 
                                       Mann1, Mann2, Mann3))
dim(urine_outcomes)
d<-merge(ipcw, urine_outcomes, by=c("dataid", "childNo"), all.x=T, all.y=F)
dim(d)

#Single urine outcome not matched to main trial data
anti_join(urine_outcomes, ipcw, by=c("dataid", "childNo")) %>% subset(select=c(dataid, childNo))

#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
table(d$tr)


#Subset to EED arms
d<-subset(d, tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")
dim(d)



#Impute time varying covariates

#calculate overall median:
month1_median1 <-    median(d$month1, na.rm = T)
month1_median2 <-    median(d$month2, na.rm = T)
month1_median3 <-    median(d$month3, na.rm = T)

#use clusterid to impute median month where possible
table(d$month1)
table(is.na(d$month1))
d$month1[is.na(d$month1)] <-  ave(d$month1, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month1)] 
d$month1 <- ceiling(d$month1)
table(d$month1)
table(is.na(d$month1))

d$month2[is.na(d$month2)] <-  ave(d$month2, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month2)] 
d$month2 <- ceiling(d$month2)

d$month3[is.na(d$month3)] <-  ave(d$month3, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month3)] 
d$month3 <- ceiling(d$month3)


#impute month with overall median for those observations not in a cluster measured in the EED subsample
d$month1[is.na(d$month1)] <-  month1_median1
d$month2[is.na(d$month2)] <-  month1_median2
d$month3[is.na(d$month3)] <-  month1_median3
table(d$month1)


#impute child age with overall median
# d$aged1[is.na(d$aged1)] <- median(d$aged1, na.rm = T)
# d$aged2[is.na(d$aged2)] <- median(d$aged2, na.rm = T)
# d$aged3[is.na(d$aged3)] <- median(d$aged3, na.rm = T)

d$aged1[is.na(d$aged1)] <- 84
d$aged2[is.na(d$aged2)] <- 428
d$aged3[is.na(d$aged3)] <- 857

summary(d$aged1)
table(d$aged1)
summary(d$aged1[d$tr=="Control"])
summary(d$aged1[d$tr=="WSH"])


#Mark missing staffid
d$staffid1[is.na(d$staffid1)] <- "missing"
d$staffid2[is.na(d$staffid2)] <- "missing"
d$staffid3[is.na(d$staffid3)] <- "missing"

#Truncate staffid at <100
table(rbind(d$staffid1,d$staffid2,d$staffid3))
names(table(rbind(d$staffid1,d$staffid2,d$staffid3)))

#Which staff ids had <100 samples collected
inexp_staff_id<-names(which(table(rbind(d$staffid1,d$staffid2,d$staffid3))<100))
inexp_staff_id
#Assign new category to inexperienced IDs across the 3 staffid-round variables
d$staffid1[d$staffid1 %in% inexp_staff_id]<-"inexp"
d$staffid2[d$staffid2 %in% inexp_staff_id]<-"inexp"
d$staffid3[d$staffid3 %in% inexp_staff_id]<-"inexp"



#Clean covariates for adjusted analysis
#Set birthorder to 1, >=2, or missing
class(d$birthord)
d$birthord[d$birthord>1]<-"2+"
d$birthord[is.na(d$birthord)]<-"missing"
d$birthord<-factor(d$birthord)

#Make vectors of adjustment variable names
Wvars<-c('sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
          'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cows', 'n_goats', 'n_chickens')





#subset time-constant W adjustment set
W<- subset(d, select=Wvars)

#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}

#Replace missingness for factors with new level
#in main dataset 
d$sex<-as.factor(d$sex)
#d$birthord[is.na(d$birthord)]<-"99"
d$birthord<-factor(d$birthord)

d$asset_clock[is.na(d$asset_clock)]<-"99"
d$asset_clock<-factor(d$asset_clock)

#Order data to replicate SL
d <- d[order(d$dataid,d$childNo, d$svy),]

#Re-subset W so new missing categories are included
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(class(W[,i])  )
}


#Truncate unrealistic levels of n_chickens to 60
table(d$n_chickens)
d$n_chickens[d$n_chickens>60]<-60
table(d$n_chickens)




#Relevel all factors
table(d$sex)
d$sex<-addNA(d$sex)
  levels(d$sex)[3]<-"missing"
table(d$sex)
d$momedu=relevel(d$momedu,ref="No education")
d$hfiacat=relevel(d$hfiacat,ref="Food Secure")
    d$hfiacat<-addNA(d$hfiacat)
d$wall<-factor(d$wall)
    d$wall<-addNA(d$wall)
    levels(d$wall)<-c("No improved wall","Improved wall","Missing")
    d$wall=relevel(d$wall,ref="No improved wall")
d$floor<-factor(d$floor)
    d$floor<-addNA(d$floor)
    levels(d$floor)<-c("No improved floor","Improved floor","Missing")
    d$floor=relevel(d$floor,ref="No improved floor")
d$elec<-factor(d$elec)
    d$elec<-addNA(d$elec)
    levels(d$elec)<-c("No electricity","Electricity","Missing")
    d$elec=relevel(d$elec,ref="No electricity")
d$asset_wardrobe<-factor(d$asset_wardrobe)
    d$asset_wardrobe<-addNA(d$asset_wardrobe)
    levels(d$asset_wardrobe)<-c("No wardrobe","Wardrobe","Missing")
    d$asset_wardrobe=relevel(d$asset_wardrobe,ref="No wardrobe")
d$asset_table<-factor(d$asset_table)
    d$asset_table<-addNA(d$asset_table)
    levels(d$asset_table)<-c("No table","Improved table","Missing")
    d$asset_table=relevel(d$asset_table,ref="No table")
d$asset_chair<-factor(d$asset_chair)
    d$asset_chair<-addNA(d$asset_chair)
    levels(d$asset_chair)<-c("No chair","Chair","Missing")
    d$asset_chair=relevel(d$asset_chair,ref="No chair")
d$asset_clock[is.na(d$asset_clock)]<-99
    d$asset_clock<-factor(d$asset_clock)
    d$asset_clock<-addNA(d$asset_clock)
    levels(d$asset_clock)<-c("No clock","Clock","Missing", "Missing")
    d$asset_clock=relevel(d$asset_clock,ref="No clock")
d$asset_khat<-factor(d$asset_khat)
    d$asset_khat<-addNA(d$asset_khat)
    levels(d$asset_khat)<-c("No khat","Khat","Missing")
    d$asset_khat=relevel(d$asset_khat,ref="No khat")
d$asset_chouki<-factor(d$asset_chouki)
    d$asset_chouki<-addNA(d$asset_chouki)
    levels(d$asset_chouki)<-c("No chouki","Chouki","Missing")
    d$asset_chouki=relevel(d$asset_chouki,ref="No chouki")
d$asset_tv<-factor(d$asset_tv)
    d$asset_tv<-addNA(d$asset_tv)
    levels(d$asset_tv)<-c("No TV","Improved TV","Missing")
    d$asset_tv=relevel(d$asset_tv,ref="No TV")
d$asset_refrig<-factor(d$asset_refrig)
    d$asset_refrig<-addNA(d$asset_refrig)
    levels(d$asset_refrig)<-c("No refrigerator","Refrigerator","Missing")
    d$asset_refrig=relevel(d$asset_refrig,ref="No refrigerator")
d$asset_bike<-factor(d$asset_bike)
    d$asset_bike<-addNA(d$asset_bike)
    levels(d$asset_bike)<-c("No bicycle","Bicycle","Missing")
    d$asset_bike=relevel(d$asset_bike,ref="No bicycle")
d$asset_moto<-factor(d$asset_moto)
    d$asset_moto<-addNA(d$asset_moto)
    levels(d$asset_moto)<-c("No motorcycle","Motorcycle","Missing")
    d$asset_moto=relevel(d$asset_moto,ref="No motorcycle")
d$asset_sewmach<-factor(d$asset_sewmach)
    d$asset_sewmach<-addNA(d$asset_sewmach)
    levels(d$asset_sewmach)<-c("No sewing machine","Sewing machine","Missing")
    d$asset_sewmach=relevel(d$asset_sewmach,ref="No sewing machine")
d$asset_mobile<-factor(d$asset_mobile)
    d$asset_mobile<-addNA(d$asset_mobile)
    levels(d$asset_mobile)<-c("No mobile phone","Mobile phone","Missing")
    d$asset_mobile=relevel(d$asset_mobile,ref="No mobile phone")    
d$walls<-factor(d$walls)

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)

#Add in time-varying covariates
Wvars1<-c("aged1", "month1", "staffid1") 
Wvars2<-c("aged2", "month2", "staffid2") 
Wvars3<-c("aged3", "month3", "staffid3") 
W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W1$month1[is.na(W1$month1)]<-"missing"
W2$month2[is.na(W2$month2)]<-"missing"
W3$month3[is.na(W3$month3)]<-"missing"
W1$staffid1[is.na(W1$staffid1)]<-"missing"
W2$staffid2[is.na(W2$staffid2)]<-"missing"
W3$staffid3[is.na(W3$staffid3)]<-"missing"


#Set time-varying covariates as factors
W1$month1<-as.factor(W1$month1)
W2$month2<-as.factor(W2$month2)
W3$month3<-as.factor(W3$month3)
W1$staffid1<-factor(W1$staffid1)
W2$staffid2<-factor(W2$staffid2)
W3$staffid3<-factor(W3$staffid3)

W2$month2 <- relevel(W2$month2, ref="1")
W2$staffid2 <- relevel(W2$staffid2, ref="missing")
W2 <- droplevels(W2)

W3$month3 <- relevel(W3$month3, ref="1")
W3$staffid3 <- relevel(W3$staffid3, ref="missing")
W3 <- droplevels(W3)

W1$month1 <- relevel(W1$month1, ref="1")
W1$staffid1 <- relevel(W1$staffid1, ref="missing")
W1 <- droplevels(W1)



#------------------
#Generate LM ratio
#------------------

#Destring urine and LM volume
d$urineVol_t1<-as.numeric(d$urineVol_t1)
d$urineVol_t2<-as.numeric(d$urineVol_t2)
d$urineVol_t3<-as.numeric(d$urineVol_t3)
d$LMvol_t1<-as.numeric(d$LMvol_t1)
d$LMvol_t2<-as.numeric(d$LMvol_t2)
d$LMvol_t3<-as.numeric(d$LMvol_t3)

#To calculate total lactulose dosed (mg) or total mannitol dosed (mg):
 #The children ingest a solution of 250 mg/ml lactulose and 50 mg/ml of mannitol in a dose of 2 ml/kg of weight up to 20 ml maximum.
 #Q9 of the EE urine form is the total volume of LM solution ingested (in ml). For example, a child who ingested 20 ml of LM solution (the maximum dose), would have ingested 1000 mg of mannitol and 5000 mg of lactulose. The 1000 mg and 5000 mg would then be used in the above formula as the "total mannitol dosed (mg) or total lactulose dosed (mg)".
 mean(d$LMvol_t1, na.rm=T)
 mean(d$urineVol_t1, na.rm=T)/1000

d$lact.dose_t1<-d$LMvol_t1*250
d$lact.dose_t2<-d$LMvol_t2*250
d$lact.dose_t3<-d$LMvol_t3*250
d$mann.dose_t1<-d$LMvol_t1*50
d$mann.dose_t2<-d$LMvol_t2*50
d$mann.dose_t3<-d$LMvol_t3*50

mean(d$lact.dose_t1, na.rm=T)
mean(d$mann.dose_t1, na.rm=T)


#% lactulose recovery = (urine concentration lactulose (mg/L) * urine volume (L) * 100 / total lactulose dosed (mg))
d$per.lact.rec_t1<-d$Lact1*(d$urineVol_t1/1000)*100/d$lact.dose_t1
d$per.lact.rec_t2<-d$Lact2*(d$urineVol_t2/1000)*100/d$lact.dose_t2
d$per.lact.rec_t3<-d$Lact3*(d$urineVol_t3/1000)*100/d$lact.dose_t3
mean(d$per.lact.rec_t1, na.rm=T)

#% mannitol recovery = (urine concentration mannitol (mg/L) * urine volume (L) * 100 / total mannitol dosed (mg))
d$per.mann.rec_t1<-d$Mann1*(d$urineVol_t1/1000)*100/d$mann.dose_t1
d$per.mann.rec_t2<-d$Mann2*(d$urineVol_t2/1000)*100/d$mann.dose_t2
d$per.mann.rec_t3<-d$Mann3*(d$urineVol_t3/1000)*100/d$mann.dose_t3


#LM ratio
d$LM1<-d$per.lact.rec_t1/d$per.mann.rec_t1
d$LM2<-d$per.lact.rec_t2/d$per.mann.rec_t2
d$LM3<-d$per.lact.rec_t3/d$per.mann.rec_t3
mean(d$LM1, na.rm=T)

#We also need to report Lactulose recovery and Mannitol recovery in mmol/L (as indicated on our table shells).
    #mmol/L of Lactulose = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/342.296g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 342.296) = mmol/L
    #mmol/L of Mannitol = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/182.172g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 182.172) = mmol/L
mean(d$Lact1, na.rm=T)
mean(d$LMvol_t1, na.rm=T)
mean(d$Mann1, na.rm=T)

d$lact.rec.MMOL_t1<-(d$Lact1/1000)*(1/342.296)
d$lact.rec.MMOL_t2<-(d$Lact2/1000)*(1/342.296)
d$lact.rec.MMOL_t3<-(d$Lact3/1000)*(1/342.296)
d$mann.rec.MMOL_t1<-(d$Mann1/1000)*(1/182.172)
d$mann.rec.MMOL_t2<-(d$Mann2/1000)*(1/182.172)
d$mann.rec.MMOL_t3<-(d$Mann3/1000)*(1/182.172)
mean(d$lact.rec.MMOL_t1, na.rm=T)

############################
#Calculate outcomes:
############################

d$Lact1<-d$Lact1*(1/342.296)
d$Lact2<-d$Lact2*(1/342.296)
d$Lact3<-d$Lact3*(1/342.296)

d$Mann1<-d$Mann1*(1/182.172)
d$Mann2<-d$Mann2*(1/182.172)
d$Mann3<-d$Mann3*(1/182.172)



mean(log(d$Lact1), na.rm=T)
mean(log(d$Lact2), na.rm=T)
mean(log(d$Lact3), na.rm=T)

mean(log(d$Mann1), na.rm=T)
mean(log(d$Mann2), na.rm=T)
mean(log(d$Mann3), na.rm=T)



############################
#Set up ipcw analysis
############################




#Create indicators for missingness
d$Lact1.miss<-ifelse(is.na(d$Lact1),0,1)
d$Lact2.miss<-ifelse(is.na(d$Lact2),0,1)
d$Lact3.miss<-ifelse(is.na(d$Lact3),0,1)

d$Mann1.miss<-ifelse(is.na(d$Mann1),0,1)
d$Mann2.miss<-ifelse(is.na(d$Mann2),0,1)
d$Mann3.miss<-ifelse(is.na(d$Mann3),0,1)

d$LM1.miss<-ifelse(is.na(d$LM1),0,1)
d$LM2.miss<-ifelse(is.na(d$LM2),0,1)
d$LM3.miss<-ifelse(is.na(d$LM3),0,1)



table(d$Lact1.miss)
table(d$Lact2.miss)
table(d$Lact3.miss)

table(d$Mann1.miss)
table(d$Mann2.miss)
table(d$Mann3.miss)

table(d$LM1.miss)
table(d$LM2.miss)
table(d$LM3.miss)

table(d$Lact1.miss)
table(d$Lact2.miss)
table(d$Lact3.miss)


mean(d$Lact1.miss, na.rm=T)


# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$Lact1Delta <- d$Lact1
d$Lact1Delta[d$Lact1.miss==0] <- exp(9)

d$Lact2Delta <- d$Lact2
d$Lact2Delta[d$Lact2.miss==0] <- exp(9)

d$Lact3Delta <- d$Lact3
d$Lact3Delta[d$Lact3.miss==0] <- exp(9)

d$Mann1Delta <- d$Mann1
d$Mann1Delta[d$Mann1.miss==0] <- exp(9)

d$Mann2Delta <- d$Mann2
d$Mann2Delta[d$Mann2.miss==0] <- exp(9)

d$Mann3Delta <- d$Mann3
d$Mann3Delta[d$Mann3.miss==0] <- exp(9)

d$LM1Delta <- d$LM1
d$LM1Delta[d$LM1.miss==0] <- exp(9)

d$LM2Delta <- d$LM2
d$LM2Delta[d$LM2.miss==0] <- exp(9)

d$LM3Delta <- d$LM3
d$LM3Delta[d$LM3.miss==0] <- exp(9)

mean(log(d$Lact1Delta), na.rm=T)



#Order for replication:
d<-d[order(d$block,d$clusterid,d$dataid),]
  
#Run the unadjusted ipcw analysis

#dataframes of urine biomarkers and missingness:
Y<-d %>% select(Lact1Delta,Mann1Delta,LM1Delta,
                Lact2Delta,Mann2Delta,LM2Delta,
                Lact3Delta,Mann3Delta,LM3Delta)
miss<-d %>% select(Lact1.miss,Mann1.miss,LM1.miss,
                Lact2.miss,Mann2.miss,LM2.miss,
                Lact3.miss,Mann3.miss,LM3.miss)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))






#-----------------------
#PCA analysis
#-----------------------


#Time 1 
ret<-as.data.frame(design_matrix(W1))


#Set missingness to zero
table(is.na(ret))
ret[is.na(ret)]<-0
table(is.na(ret))

## Convert the data into matrix ##
ret<-as.matrix(ret)

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(ret) 

## Identifying what components to be used ##
barplot(height=princ.return$sdev[1:10]/princ.return$sdev[1])

## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   
load2 <- loadings(princ.return)[,2]   
load3 <- loadings(princ.return)[,3]   
load4 <- loadings(princ.return)[,4]   

load[which(load==max(load))]
load2[which(load2==max(load2))]
load3[which(load3==max(load3))]
load4[which(load4==max(load4))]

pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
pr.cp2 <- ret %*% load2 
pr.cp3 <- ret %*% load3 
pr.cp4 <- ret %*% load4 
pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
pca2 <- as.numeric(pr.cp2)
pca3 <- as.numeric(pr.cp3)
pca4 <- as.numeric(pr.cp4)


pca_res <- data.frame(tr=d$tr, miss=d$Lact1.miss, pca1=pca, pca2=pca2, pca3=pca3, pca4=pca4)

d$aged1[pca_res$pca1>0]

p <- ggplot(data=pca_res, aes(x=pca1, y=pca2)) +
      geom_point(aes(colour=tr, shape=factor(miss))) + theme_bw()  + ggtitle("Time 1: first 2 principal components, colored by arm")
p

p2 <- ggplot(data=pca_res, aes(x=pca3, y=pca4)) +
      geom_point(aes(colour=tr, shape=factor(miss))) + theme_bw() + coord_cartesian(xlim=c(-20,0))
p2


box1 <- ggplot(pca_res, aes(tr, pca1)) + geom_boxplot() + geom_jitter(width = 0.2) + ggtitle("Time 1: first principal component by arm")
box1
#ggsave("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/urine_ipcw_pca.pdf", p, h=10, w=10)


#Time 2
ret<-as.data.frame(design_matrix(W2))


#Set missingness to zero
table(is.na(ret))
ret[is.na(ret)]<-0
table(is.na(ret))

## Convert the data into matrix ##
ret<-as.matrix(ret)

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(ret) 

## Identifying what components to be used ##
barplot(height=princ.return$sdev[1:10]/princ.return$sdev[1])

## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   
load2 <- loadings(princ.return)[,2]   
load3 <- loadings(princ.return)[,3]   
load4 <- loadings(princ.return)[,4]   

load[which(load==max(load))]
load2[which(load2==max(load2))]
load3[which(load3==max(load3))]
load4[which(load4==max(load4))]

pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
pr.cp2 <- ret %*% load2 
pr.cp3 <- ret %*% load3 
pr.cp4 <- ret %*% load4 
pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
pca2 <- as.numeric(pr.cp2)
pca3 <- as.numeric(pr.cp3)
pca4 <- as.numeric(pr.cp4)


pca_res <- data.frame(tr=d$tr, miss=d$Lact1.miss, pca1=pca, pca2=pca2, pca3=pca3, pca4=pca4)

d$aged1[pca_res$pca1>0]

p_t2 <- ggplot(data=pca_res, aes(x=pca1, y=pca2)) +
      geom_point(aes(colour=tr, shape=factor(miss))) + theme_bw() + ggtitle("Time 2: first 2 principal components, colored by arm")
p_t2


box2 <- ggplot(pca_res, aes(tr, pca1)) + geom_boxplot() + geom_jitter(width = 0.2) + ggtitle("Time 2: first principal component by arm")
box2


#Time 3
ret<-as.data.frame(design_matrix(W3))


#Set missingness to zero
table(is.na(ret))
ret[is.na(ret)]<-0
table(is.na(ret))

## Convert the data into matrix ##
ret<-as.matrix(ret)

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(ret) 

## Identifying what components to be used ##
barplot(height=princ.return$sdev[1:10]/princ.return$sdev[1])

## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   
load2 <- loadings(princ.return)[,2]   
load3 <- loadings(princ.return)[,3]   
load4 <- loadings(princ.return)[,4]   

load[which(load==max(load))]
load2[which(load2==max(load2))]
load3[which(load3==max(load3))]
load4[which(load4==max(load4))]

pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
pr.cp2 <- ret %*% load2 
pr.cp3 <- ret %*% load3 
pr.cp4 <- ret %*% load4 
pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
pca2 <- as.numeric(pr.cp2)
pca3 <- as.numeric(pr.cp3)
pca4 <- as.numeric(pr.cp4)


pca_res <- data.frame(tr=d$tr, miss=d$Lact1.miss, pca1=pca, pca2=pca2, pca3=pca3, pca4=pca4)

d$aged1[pca_res$pca1>0]

p_t3 <- ggplot(data=pca_res, aes(x=pca1, y=pca2)) +
      geom_point(aes(colour=tr, shape=factor(miss))) + theme_bw()  + ggtitle("Time 3: first 2 principal components, colored by arm")
p_t3





box3 <- ggplot(pca_res, aes(tr, pca1)) + geom_boxplot() + geom_jitter(width = 0.2) + ggtitle("Time 3: first principal component by arm")
box3


#-----------------------
#Kmeans analysis
#-----------------------

# library(factoextra)
# 
# set.seed(123)
# km.res <- kmeans(ret, 4, nstart = 25)
# # kmeans_df<-data.frame(SUBJID=ID, kmeans=factor(km.res$cluster))
# # dfull<-left_join(dfull, kmeans_df, by="SUBJID")
# # Visualize
# fviz_cluster(km.res, data = ret,
#              ellipse.type = "convex",
#              palette = "jco",
#              ggtheme = theme_minimal())




#Save plots
pdf("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/urine_ipcw_pca.pdf", h=10, w=10)
box1
p
box2
p_t2
box3
p_t3
dev.off()

