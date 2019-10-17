
#---------------------------------------
# bangladesh-immune-ipcw.R
#
# audrie lin (audrielin@berkeley.edu)
#
# 
# input: 
# "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv" created from do file "/Users/audrie/Desktop/stata/WBB-EED-analysis/dofiles/1-bangladesh-dm-ee-anthro-diar-immun-ipcw.do"
#  "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-enrol.csv" created from do file "/Users/audrie/Desktop/stata/WBB-EED-analysis/dofiles/1-bangladesh-dm-ee-enrol.do"
#  
#  "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/washb-bangladesh-plasma-lab-t2-t3-ipcw.csv" created from do file 3-bangladesh-dm-immun-plasma-immun-3.do
#
# output: 
# ~/Dropbox/WBB-EE-analysis/Results/Audrie/immune_ipcw.RData
# 
#---------------------------------------

#Clear out R environment (remove and loaded data)
rm(list=ls())


######################
###Load in packages
######################

#Load packages
library(devtools)
library(foreign) #Run each time R is started up to load the package into working memory
library(washb) 
library(dplyr)

######################
###Load in data
######################

source(here::here("0-config.R"))

setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/")) #Set working directory


############################
#       TIME POINT 2 
############################

#---------------------------------------
# create a shell of the full data (dfull)
# as if every index child with a live birth
# were measured at year 1 and year 2
#---------------------------------------

#load the immune lab data
washb_bd_immun<- read.csv("washb-bangladesh-plasma-lab-t2-t3-ipcw.csv", stringsAsFactors = TRUE)

#load
dfull<- read.csv("washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv", stringsAsFactors = TRUE)

#Manually set  clusterid and block for childid 61031 so treatments can merge
dfull$clusterid[dfull$childid=="61031" & !is.na(dfull$childid)] <- 61
dfull$block[dfull$childid=="61031" & !is.na(dfull$childid)] <- 8


#load blinded treatment data
#washb_bd_tr <- read.csv(paste0(dropboxDir, "Data/Untouched/Real/washb-bangladesh-tr.csv"), stringsAsFactors = TRUE)
washb_bd_tr <- read.csv(paste0(dropboxDir,"Data/Untouched/Real/washb-bangladesh-tr.csv"))
washb_bd_tr$clusterid <- as.numeric(washb_bd_tr$clusterid)

# merge treatment and enrollment data onto this shell of the full data
dim(dfull)
dim(washb_bd_tr)
dfull <- merge(dfull,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=T)
table(is.na(dfull$tr))

# re-order the treatment factor for convenience, dropping the arms not included in immune
dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

# now merge the observed plasma outcomes onto this full dataset
#idfull <- merge(dfull,washb_bd_immun,by=c("childid","dataid","clusterid","childno"),all.x=T,all.y=T)
washb_bd_immun <- subset(washb_bd_immun, select = -c(dataid, childno))
idfull <- merge(dfull,washb_bd_immun,by=c("childid"), all.x=T, all.y=T)

# sort the data for perfect replication with andrew on the V-fold cross-validation
idfull <- idfull[order(idfull$block,idfull$clusterid,idfull$dataid),]




#Select adjustment covariates 
Wvars<-c("monsoon_bt2","ageday_bt2","sex","birthord", "momage", "momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(idfull, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#Looks good. Use this if any need to be changes:
#deleted ageday, month, faid
#in stata, made momheight 2 decimal places
#in stata, made birthord into 3 categories
#in stata, coded missing as 99


W$monsoon_bt2<-as.factor(W$monsoon_bt2)
#If already a factor:
W$monsoon_bt2<-addNA(W$monsoon_bt2)
levels(W$monsoon_bt2)[length(levels(W$monsoon_bt2))]<-"Missing"

W$monsoon_bt2<-relevel(W$monsoon_bt2, ref="1")



W$ageday_bt2<-as.numeric(W$ageday_bt2)


W$momage<-as.numeric(W$momage)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)

table(idfull$sex)
idfull$sex<-addNA(idfull$sex)
levels(idfull$sex)[length(levels(idfull$sex))]<-"missing"
table(idfull$sex)


W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)


#Save intermediate R objects for replication comparison
da <- idfull
wa <- W
save(da, wa, file = here("replication objects/audrie_immune_ipcw_W.rdata"))



############################
# IGF Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_igf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_igf
idfull$Ydelta[idfull$Delta==0] <- 9

#mean(telodfull$block)
#table(telodfull$tr)
#mean(telodfull$Ydelta)

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

igf_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
igf_t2_unadj_ipcw<-as.data.frame(unlist(igf_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
igf_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
igf_t2_adj_ipcw<-as.data.frame(unlist(igf_t2_adj_ipcw$estimates$ATE))

igf_t2_adj_ipcw
igf_t2_unadj_ipcw

############################
# AGP Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_agp),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_agp
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

agp_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
agp_t2_unadj_ipcw<-as.data.frame(unlist(agp_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
agp_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
agp_t2_adj_ipcw<-as.data.frame(unlist(agp_t2_adj_ipcw$estimates$ATE))

agp_t2_adj_ipcw
agp_t2_unadj_ipcw



############################
# CRP Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_crp),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_crp
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

crp_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
crp_t2_unadj_ipcw<-as.data.frame(unlist(crp_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
crp_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
crp_t2_adj_ipcw<-as.data.frame(unlist(crp_t2_adj_ipcw$estimates$ATE))

crp_t2_adj_ipcw
crp_t2_unadj_ipcw




############################
# GMC Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_gmc),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_gmc
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

gmc_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
gmc_t2_unadj_ipcw<-as.data.frame(unlist(gmc_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
gmc_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
gmc_t2_adj_ipcw<-as.data.frame(unlist(gmc_t2_adj_ipcw$estimates$ATE))

gmc_t2_adj_ipcw
gmc_t2_unadj_ipcw




############################
# IFN Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_ifn),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_ifn
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ifn_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ifn_t2_unadj_ipcw<-as.data.frame(unlist(ifn_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ifn_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ifn_t2_adj_ipcw<-as.data.frame(unlist(ifn_t2_adj_ipcw$estimates$ATE))

ifn_t2_adj_ipcw
ifn_t2_unadj_ipcw



############################
# IL10 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il10_t2_unadj_ipcw<-as.data.frame(unlist(il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il10_t2_adj_ipcw<-as.data.frame(unlist(il10_t2_adj_ipcw$estimates$ATE))

il10_t2_adj_ipcw
il10_t2_unadj_ipcw



############################
# IL12 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il12),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il12
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il12_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il12_t2_unadj_ipcw<-as.data.frame(unlist(il12_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il12_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il12_t2_adj_ipcw<-as.data.frame(unlist(il12_t2_adj_ipcw$estimates$ATE))

il12_t2_adj_ipcw
il12_t2_unadj_ipcw


############################
# IL13 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il13_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il13_t2_unadj_ipcw<-as.data.frame(unlist(il13_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il13_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il13_t2_adj_ipcw<-as.data.frame(unlist(il13_t2_adj_ipcw$estimates$ATE))

il13_t2_adj_ipcw
il13_t2_unadj_ipcw


############################
# IL17 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il17_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il17_t2_unadj_ipcw<-as.data.frame(unlist(il17_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il17_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il17_t2_adj_ipcw<-as.data.frame(unlist(il17_t2_adj_ipcw$estimates$ATE))

il17_t2_adj_ipcw
il17_t2_unadj_ipcw

############################
# IL1 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il1),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il1
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il1_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il1_t2_unadj_ipcw<-as.data.frame(unlist(il1_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il1_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il1_t2_adj_ipcw<-as.data.frame(unlist(il1_t2_adj_ipcw$estimates$ATE))

il1_t2_adj_ipcw
il1_t2_unadj_ipcw

############################
# IL2 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il2_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il2_t2_unadj_ipcw<-as.data.frame(unlist(il2_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il2_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il2_t2_adj_ipcw<-as.data.frame(unlist(il2_t2_adj_ipcw$estimates$ATE))

il2_t2_adj_ipcw
il2_t2_unadj_ipcw

############################
# IL21 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il21_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il21_t2_unadj_ipcw<-as.data.frame(unlist(il21_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il21_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il21_t2_adj_ipcw<-as.data.frame(unlist(il21_t2_adj_ipcw$estimates$ATE))

il21_t2_adj_ipcw
il21_t2_unadj_ipcw


############################
# IL4 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il4_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il4_t2_unadj_ipcw<-as.data.frame(unlist(il4_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il4_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il4_t2_adj_ipcw<-as.data.frame(unlist(il4_t2_adj_ipcw$estimates$ATE))

il4_t2_adj_ipcw
il4_t2_unadj_ipcw

############################
# IL5 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il5_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il5_t2_unadj_ipcw<-as.data.frame(unlist(il5_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il5_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il5_t2_adj_ipcw<-as.data.frame(unlist(il5_t2_adj_ipcw$estimates$ATE))

il5_t2_adj_ipcw
il5_t2_unadj_ipcw

############################
# IL6 Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_il6),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_il6
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il6_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il6_t2_unadj_ipcw<-as.data.frame(unlist(il6_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
il6_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il6_t2_adj_ipcw<-as.data.frame(unlist(il6_t2_adj_ipcw$estimates$ATE))

il6_t2_adj_ipcw
il6_t2_unadj_ipcw

############################
# TNF Control vs N+WSH @T2
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ln_tnf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ln_tnf
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

tnf_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
tnf_t2_unadj_ipcw<-as.data.frame(unlist(tnf_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
tnf_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
tnf_t2_adj_ipcw<-as.data.frame(unlist(tnf_t2_adj_ipcw$estimates$ATE))

tnf_t2_adj_ipcw
tnf_t2_unadj_ipcw




############################
#       TIME POINT 2 ratio
############################


#---------------------------------------
# create a shell of the full data (dfull)
# as if every index child with a live birth
# were measured at year 1 and year 2
#---------------------------------------

#load the immune lab data
washb_bd_immun<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-plasma-lab-t2-t3-ipcw.csv"), stringsAsFactors = TRUE)

#load
dfull<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv"), stringsAsFactors = TRUE)

#load blinded treatment data
washb_bd_tr <- read.csv(paste0(dropboxDir, "Data/Untouched/washb-bangladesh-tr.csv"), stringsAsFactors = TRUE)

# merge treatment and enrollment data onto this shell of the full data
dfull <- merge(dfull,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=F)


# re-order the treatment factor for convenience, dropping the arms not included in immune
dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

# now merge the observed plasma outcomes onto this full dataset
idfull <- merge(dfull,washb_bd_immun,by=c("dataid","clusterid","childno"),all.x=T,all.y=F)


# sort the data for perfect replication with andrew on the V-fold cross-validation
idfull <- idfull[order(idfull$block,idfull$clusterid,idfull$dataid),]




#Select adjustment covariates 
Wvars<-c("monsoon_bt2","ageday_bt2","sex","birthord", "momage", "momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(idfull, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#Looks good. Use this if any need to be changes:
#deleted ageday, month, faid
#in stata, made momheight 2 decimal places
#in stata, made birthord into 3 categories
#in stata, coded missing as 99


W$monsoon_bt2<-as.factor(W$monsoon_bt2)
#If already a factor:
W$monsoon_bt2<-addNA(W$monsoon_bt2)
levels(W$monsoon_bt2)[length(levels(W$monsoon_bt2))]<-"Missing"

W$monsoon_bt2<-relevel(W$monsoon_bt2, ref="1")



W$ageday_bt2<-as.numeric(W$ageday_bt2)


W$momage<-as.numeric(W$momage)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)

table(idfull$sex)
idfull$sex<-addNA(idfull$sex)
levels(idfull$sex)[length(levels(idfull$sex))]<-"missing"
table(idfull$sex)


W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)



############################
# Ratio GMC:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_gmc_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_gmc_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_gmc_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_gmc_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_gmc_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_gmc_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_gmc_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_gmc_il10_t2_adj_ipcw$estimates$ATE))

ratio_gmc_il10_t2_adj_ipcw
ratio_gmc_il10_t2_unadj_ipcw

############################
# Ratio IFN:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il10_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il10_t2_adj_ipcw
ratio_ifn_il10_t2_unadj_ipcw

############################
# Ratio IL12:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il10_t2_adj_ipcw$estimates$ATE))

ratio_il12_il10_t2_adj_ipcw
ratio_il12_il10_t2_unadj_ipcw

############################
# Ratio IL13:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il13_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il13_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il13_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il13_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il13_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il13_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il13_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il13_il10_t2_adj_ipcw$estimates$ATE))

ratio_il13_il10_t2_adj_ipcw
ratio_il13_il10_t2_unadj_ipcw

############################
# Ratio IL17:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il17_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il17_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il17_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il17_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il17_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il17_il10_t2_adj_ipcw$estimates$ATE))

ratio_il17_il10_t2_adj_ipcw
ratio_il17_il10_t2_unadj_ipcw

############################
# Ratio IL1:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il1_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il1_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il1_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il1_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il1_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il1_il10_t2_adj_ipcw$estimates$ATE))

ratio_il1_il10_t2_adj_ipcw
ratio_il1_il10_t2_unadj_ipcw


############################
# Ratio IL21:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il21_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il21_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il21_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il21_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il21_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il21_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il21_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il21_il10_t2_adj_ipcw$estimates$ATE))

ratio_il21_il10_t2_adj_ipcw
ratio_il21_il10_t2_unadj_ipcw

############################
# Ratio IL2:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il2_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il2_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il2_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il2_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il2_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il2_il10_t2_adj_ipcw$estimates$ATE))

ratio_il2_il10_t2_adj_ipcw
ratio_il2_il10_t2_unadj_ipcw

############################
# Ratio IL4:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il4_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il4_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il4_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il4_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il4_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il4_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il4_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il4_il10_t2_adj_ipcw$estimates$ATE))

ratio_il4_il10_t2_adj_ipcw
ratio_il4_il10_t2_unadj_ipcw

############################
# Ratio IL5:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il5_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il5_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il5_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il5_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il5_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il5_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il5_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il5_il10_t2_adj_ipcw$estimates$ATE))

ratio_il5_il10_t2_adj_ipcw
ratio_il5_il10_t2_unadj_ipcw

############################
# Ratio IL6:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il6_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il6_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il6_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il6_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il6_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il6_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il6_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_il6_il10_t2_adj_ipcw$estimates$ATE))

ratio_il6_il10_t2_adj_ipcw
ratio_il6_il10_t2_unadj_ipcw

############################
# Ratio TNF:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_tnf_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_tnf_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_tnf_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_tnf_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_tnf_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_tnf_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_tnf_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_tnf_il10_t2_adj_ipcw$estimates$ATE))

ratio_tnf_il10_t2_adj_ipcw
ratio_tnf_il10_t2_unadj_ipcw

############################
# Ratio IL12:IL4 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il4_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il4_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il4_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il4_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il4_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il4_t2_adj_ipcw$estimates$ATE))

ratio_il12_il4_t2_adj_ipcw
ratio_il12_il4_t2_unadj_ipcw


############################
# Ratio IFN:IL4 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il4_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il4_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il4_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il4_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il4_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il4_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il4_t2_adj_ipcw
ratio_ifn_il4_t2_unadj_ipcw


############################
# Ratio IL12:IL5 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il5_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il5_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il5_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il5_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il5_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il5_t2_adj_ipcw$estimates$ATE))

ratio_il12_il5_t2_adj_ipcw
ratio_il12_il5_t2_unadj_ipcw

############################
# Ratio IFN:IL5 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il5_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il5_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il5_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il5_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il5_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il5_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il5_t2_adj_ipcw
ratio_ifn_il5_t2_unadj_ipcw

############################
# Ratio IL12:IL13 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il13_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il13_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il13_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il13_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il13_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il13_t2_adj_ipcw$estimates$ATE))

ratio_il12_il13_t2_adj_ipcw
ratio_il12_il13_t2_unadj_ipcw

############################
# Ratio IFN:IL13 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il13_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il13_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il13_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il13_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il13_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il13_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il13_t2_adj_ipcw
ratio_ifn_il13_t2_unadj_ipcw

############################
# Ratio IL12:IL17 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il17_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il17_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il17_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il17_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il17_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il17_t2_adj_ipcw$estimates$ATE))

ratio_il12_il17_t2_adj_ipcw
ratio_il12_il17_t2_unadj_ipcw

############################
# Ratio IFN:IL17 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il17_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il17_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il17_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il17_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il17_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il17_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il17_t2_adj_ipcw
ratio_ifn_il17_t2_unadj_ipcw

############################
# Ratio IL12:IL21 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_il12_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_il12_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il21_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il21_t2_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il21_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il21_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il21_t2_adj_ipcw<-as.data.frame(unlist(ratio_il12_il21_t2_adj_ipcw$estimates$ATE))

ratio_il12_il21_t2_adj_ipcw
ratio_il12_il21_t2_unadj_ipcw

############################
# Ratio IFN:IL21 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_ifn_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_ifn_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il21_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il21_t2_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il21_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il21_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il21_t2_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il21_t2_adj_ipcw$estimates$ATE))

ratio_ifn_il21_t2_adj_ipcw
ratio_ifn_il21_t2_unadj_ipcw

############################
# Ratio pro:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_pro_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_pro_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_pro_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_pro_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_pro_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_pro_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_pro_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_pro_il10_t2_adj_ipcw$estimates$ATE))

ratio_pro_il10_t2_adj_ipcw
ratio_pro_il10_t2_unadj_ipcw

############################
# Ratio th1:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_th1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_th1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_th1_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_th1_il10_t2_adj_ipcw$estimates$ATE))

ratio_th1_il10_t2_adj_ipcw
ratio_th1_il10_t2_unadj_ipcw


############################
# Ratio th2:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_th2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_th2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th2_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th2_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_th2_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th2_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th2_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_th2_il10_t2_adj_ipcw$estimates$ATE))

ratio_th2_il10_t2_adj_ipcw
ratio_th2_il10_t2_unadj_ipcw

############################
# Ratio th17:IL10 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_th17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_th17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th17_il10_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th17_il10_t2_unadj_ipcw<-as.data.frame(unlist(ratio_th17_il10_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th17_il10_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th17_il10_t2_adj_ipcw<-as.data.frame(unlist(ratio_th17_il10_t2_adj_ipcw$estimates$ATE))

ratio_th17_il10_t2_adj_ipcw
ratio_th17_il10_t2_unadj_ipcw

############################
# Ratio th1:th2 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_th1_th2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_th1_th2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_th2_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th2_t2_unadj_ipcw<-as.data.frame(unlist(ratio_th1_th2_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_th2_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th2_t2_adj_ipcw<-as.data.frame(unlist(ratio_th1_th2_t2_adj_ipcw$estimates$ATE))

ratio_th1_th2_t2_adj_ipcw
ratio_th1_th2_t2_unadj_ipcw

############################
# Ratio th1:th17 Control vs N+WSH @T2
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t2_ratio_th1_th17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t2_ratio_th1_th17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_th17_t2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th17_t2_unadj_ipcw<-as.data.frame(unlist(ratio_th1_th17_t2_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_th17_t2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th17_t2_adj_ipcw<-as.data.frame(unlist(ratio_th1_th17_t2_adj_ipcw$estimates$ATE))

ratio_th1_th17_t2_adj_ipcw
ratio_th1_th17_t2_unadj_ipcw

############################
#       TIME POINT 3
############################

#---------------------------------------
# create a shell of the full data (dfull)
# as if every index child with a live birth
# were measured at year 1 and year 2
#---------------------------------------

#load the immune lab data
washb_bd_immun<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-plasma-lab-t2-t3-ipcw.csv"), stringsAsFactors = TRUE)

#load
dfull<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv"), stringsAsFactors = TRUE)

#load blinded treatment data
washb_bd_tr <- read.csv(paste0(dropboxDir, "Data/Untouched/washb-bangladesh-tr.csv"), stringsAsFactors = TRUE)

# merge treatment and enrollment data onto this shell of the full data
dfull <- merge(dfull,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=F)


# re-order the treatment factor for convenience, dropping the arms not included in immune
dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

# now merge the observed plasma outcomes onto this full dataset
idfull <- merge(dfull,washb_bd_immun,by=c("dataid","clusterid","childno"),all.x=T,all.y=F)


# sort the data for perfect replication with andrew on the V-fold cross-validation
idfull <- idfull[order(idfull$block,idfull$clusterid,idfull$dataid),]




#Select adjustment covariates 
Wvars<-c("monsoon_bt3","ageday_bt3","sex","birthord", "momage", "momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(idfull, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#Looks good. Use this if any need to be changes:
#deleted ageday, month, faid
#in stata, made momheight 2 decimal places
#in stata, made birthord into 3 categories
#in stata, coded missing as 99


W$monsoon_bt3<-as.factor(W$monsoon_bt3)
#If already a factor:
W$monsoon_bt3<-addNA(W$monsoon_bt3)
levels(W$monsoon_bt3)[length(levels(W$monsoon_bt3))]<-"Missing"

W$monsoon_bt3<-relevel(W$monsoon_bt3, ref="1")



W$ageday_bt3<-as.numeric(W$ageday_bt3)


W$momage<-as.numeric(W$momage)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)


table(idfull$sex)
idfull$sex<-addNA(idfull$sex)
levels(idfull$sex)[length(levels(idfull$sex))]<-"missing"
table(idfull$sex)


W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)

############################
# IGF Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_igf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_igf
idfull$Ydelta[idfull$Delta==0] <- 9

#mean(telodfull$block)
#table(telodfull$tr)
#mean(telodfull$Ydelta)

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

igf_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
igf_t3_unadj_ipcw<-as.data.frame(unlist(igf_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
igf_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
igf_t3_adj_ipcw<-as.data.frame(unlist(igf_t3_adj_ipcw$estimates$ATE))

igf_t3_adj_ipcw
igf_t3_unadj_ipcw


############################
# GMC Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_gmc),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_gmc
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

gmc_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
gmc_t3_unadj_ipcw<-as.data.frame(unlist(gmc_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
gmc_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
gmc_t3_adj_ipcw<-as.data.frame(unlist(gmc_t3_adj_ipcw$estimates$ATE))

gmc_t3_adj_ipcw
gmc_t3_unadj_ipcw




############################
# IFN Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_ifn),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_ifn
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ifn_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ifn_t3_unadj_ipcw<-as.data.frame(unlist(ifn_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ifn_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ifn_t3_adj_ipcw<-as.data.frame(unlist(ifn_t3_adj_ipcw$estimates$ATE))

ifn_t3_adj_ipcw
ifn_t3_unadj_ipcw



############################
# IL10 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il10_t3_unadj_ipcw<-as.data.frame(unlist(il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il10_t3_adj_ipcw<-as.data.frame(unlist(il10_t3_adj_ipcw$estimates$ATE))

il10_t3_adj_ipcw
il10_t3_unadj_ipcw



############################
# IL12 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il12),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il12
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il12_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il12_t3_unadj_ipcw<-as.data.frame(unlist(il12_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il12_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il12_t3_adj_ipcw<-as.data.frame(unlist(il12_t3_adj_ipcw$estimates$ATE))

il12_t3_adj_ipcw
il12_t3_unadj_ipcw


############################
# IL13 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il13_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il13_t3_unadj_ipcw<-as.data.frame(unlist(il13_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il13_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il13_t3_adj_ipcw<-as.data.frame(unlist(il13_t3_adj_ipcw$estimates$ATE))

il13_t3_adj_ipcw
il13_t3_unadj_ipcw


############################
# IL17 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il17_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il17_t3_unadj_ipcw<-as.data.frame(unlist(il17_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il17_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il17_t3_adj_ipcw<-as.data.frame(unlist(il17_t3_adj_ipcw$estimates$ATE))

il17_t3_adj_ipcw
il17_t3_unadj_ipcw

############################
# IL1 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il1),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il1
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il1_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il1_t3_unadj_ipcw<-as.data.frame(unlist(il1_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il1_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il1_t3_adj_ipcw<-as.data.frame(unlist(il1_t3_adj_ipcw$estimates$ATE))

il1_t3_adj_ipcw
il1_t3_unadj_ipcw

############################
# IL2 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il2_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il2_t3_unadj_ipcw<-as.data.frame(unlist(il2_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il2_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il2_t3_adj_ipcw<-as.data.frame(unlist(il2_t3_adj_ipcw$estimates$ATE))

il2_t3_adj_ipcw
il2_t3_unadj_ipcw

############################
# IL21 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il21_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il21_t3_unadj_ipcw<-as.data.frame(unlist(il21_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il21_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il21_t3_adj_ipcw<-as.data.frame(unlist(il21_t3_adj_ipcw$estimates$ATE))

il21_t3_adj_ipcw
il21_t3_unadj_ipcw


############################
# IL4 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il4_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il4_t3_unadj_ipcw<-as.data.frame(unlist(il4_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il4_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il4_t3_adj_ipcw<-as.data.frame(unlist(il4_t3_adj_ipcw$estimates$ATE))

il4_t3_adj_ipcw
il4_t3_unadj_ipcw

############################
# IL5 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il5_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il5_t3_unadj_ipcw<-as.data.frame(unlist(il5_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il5_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il5_t3_adj_ipcw<-as.data.frame(unlist(il5_t3_adj_ipcw$estimates$ATE))

il5_t3_adj_ipcw
il5_t3_unadj_ipcw

############################
# IL6 Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_il6),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_il6
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

il6_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il6_t3_unadj_ipcw<-as.data.frame(unlist(il6_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
il6_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
il6_t3_adj_ipcw<-as.data.frame(unlist(il6_t3_adj_ipcw$estimates$ATE))

il6_t3_adj_ipcw
il6_t3_unadj_ipcw

############################
# TNF Control vs N+WSH @T3
############################


# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ln_tnf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ln_tnf
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

tnf_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
tnf_t3_unadj_ipcw<-as.data.frame(unlist(tnf_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
tnf_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
tnf_t3_adj_ipcw<-as.data.frame(unlist(tnf_t3_adj_ipcw$estimates$ATE))

tnf_t3_adj_ipcw
tnf_t3_unadj_ipcw



############################
#       TIME POINT 3 ratio
############################


#---------------------------------------
# create a shell of the full data (dfull)
# as if every index child with a live birth
# were measured at year 1 and year 2
#---------------------------------------

#load the immune lab data
washb_bd_immun<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-plasma-lab-t2-t3-ipcw.csv"), stringsAsFactors = TRUE)

#load
dfull<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv"), stringsAsFactors = TRUE)

#load blinded treatment data
washb_bd_tr <- read.csv(paste0(dropboxDir, "Data/Untouched/washb-bangladesh-tr.csv"), stringsAsFactors = TRUE)

# merge treatment and enrollment data onto this shell of the full data
dfull <- merge(dfull,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=F)


# re-order the treatment factor for convenience, dropping the arms not included in immune
dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

# now merge the observed plasma outcomes onto this full dataset
idfull <- merge(dfull,washb_bd_immun,by=c("dataid","clusterid","childno"),all.x=T,all.y=F)


# sort the data for perfect replication with andrew on the V-fold cross-validation
idfull <- idfull[order(idfull$block,idfull$clusterid,idfull$dataid),]




#Select adjustment covariates 
Wvars<-c("monsoon_bt3","ageday_bt3","sex","birthord", "momage", "momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(idfull, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#Looks good. Use this if any need to be changes:
#deleted ageday, month, faid
#in stata, made momheight 2 decimal places
#in stata, made birthord into 3 categories
#in stata, coded missing as 99


W$monsoon_bt3<-as.factor(W$monsoon_bt2)
#If already a factor:
W$monsoon_bt3<-addNA(W$monsoon_bt2)
levels(W$monsoon_bt3)[length(levels(W$monsoon_bt3))]<-"Missing"

W$monsoon_bt3<-relevel(W$monsoon_bt3, ref="1")



W$ageday_bt3<-as.numeric(W$ageday_bt3)


W$momage<-as.numeric(W$momage)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)

table(idfull$sex)
idfull$sex<-addNA(idfull$sex)
levels(idfull$sex)[length(levels(idfull$sex))]<-"missing"
table(idfull$sex)


W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)

############################
# Ratio GMC:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_gmc_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_gmc_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_gmc_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_gmc_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_gmc_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_gmc_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_gmc_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_gmc_il10_t3_adj_ipcw$estimates$ATE))

ratio_gmc_il10_t3_adj_ipcw
ratio_gmc_il10_t3_unadj_ipcw

############################
# Ratio IFN:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il10_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il10_t3_adj_ipcw
ratio_ifn_il10_t3_unadj_ipcw

############################
# Ratio IL12:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il10_t3_adj_ipcw$estimates$ATE))

ratio_il12_il10_t3_adj_ipcw
ratio_il12_il10_t3_unadj_ipcw

############################
# Ratio IL13:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il13_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il13_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il13_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il13_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il13_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il13_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il13_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il13_il10_t3_adj_ipcw$estimates$ATE))

ratio_il13_il10_t3_adj_ipcw
ratio_il13_il10_t3_unadj_ipcw

############################
# Ratio IL17:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il17_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il17_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il17_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il17_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il17_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il17_il10_t3_adj_ipcw$estimates$ATE))

ratio_il17_il10_t3_adj_ipcw
ratio_il17_il10_t3_unadj_ipcw

############################
# Ratio IL1:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il1_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il1_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il1_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il1_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il1_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il1_il10_t3_adj_ipcw$estimates$ATE))

ratio_il1_il10_t3_adj_ipcw
ratio_il1_il10_t3_unadj_ipcw


############################
# Ratio IL21:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il21_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il21_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il21_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il21_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il21_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il21_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il21_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il21_il10_t3_adj_ipcw$estimates$ATE))

ratio_il21_il10_t3_adj_ipcw
ratio_il21_il10_t3_unadj_ipcw

############################
# Ratio IL2:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il2_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il2_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il2_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il2_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il2_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il2_il10_t3_adj_ipcw$estimates$ATE))

ratio_il2_il10_t3_adj_ipcw
ratio_il2_il10_t3_unadj_ipcw

############################
# Ratio IL4:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il4_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il4_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il4_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il4_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il4_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il4_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il4_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il4_il10_t3_adj_ipcw$estimates$ATE))

ratio_il4_il10_t3_adj_ipcw
ratio_il4_il10_t3_unadj_ipcw

############################
# Ratio IL5:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il5_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il5_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il5_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il5_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il5_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il5_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il5_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il5_il10_t3_adj_ipcw$estimates$ATE))

ratio_il5_il10_t3_adj_ipcw
ratio_il5_il10_t3_unadj_ipcw

############################
# Ratio IL6:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il6_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il6_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il6_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il6_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il6_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il6_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il6_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_il6_il10_t3_adj_ipcw$estimates$ATE))

ratio_il6_il10_t3_adj_ipcw
ratio_il6_il10_t3_unadj_ipcw

############################
# Ratio TNF:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_tnf_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_tnf_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_tnf_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_tnf_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_tnf_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_tnf_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_tnf_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_tnf_il10_t3_adj_ipcw$estimates$ATE))

ratio_tnf_il10_t3_adj_ipcw
ratio_tnf_il10_t3_unadj_ipcw

############################
# Ratio IL12:IL4 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il4_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il4_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il4_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il4_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il4_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il4_t3_adj_ipcw$estimates$ATE))

ratio_il12_il4_t3_adj_ipcw
ratio_il12_il4_t3_unadj_ipcw


############################
# Ratio IFN:IL4 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il4_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il4_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il4_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il4_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il4_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il4_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il4_t3_adj_ipcw
ratio_ifn_il4_t3_unadj_ipcw


############################
# Ratio IL12:IL5 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il5_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il5_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il5_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il5_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il5_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il5_t3_adj_ipcw$estimates$ATE))

ratio_il12_il5_t3_adj_ipcw
ratio_il12_il5_t3_unadj_ipcw

############################
# Ratio IFN:IL5 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il5_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il5_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il5_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il5_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il5_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il5_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il5_t3_adj_ipcw
ratio_ifn_il5_t3_unadj_ipcw

############################
# Ratio IL12:IL13 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il13_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il13_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il13_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il13_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il13_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il13_t3_adj_ipcw$estimates$ATE))

ratio_il12_il13_t3_adj_ipcw
ratio_il12_il13_t3_unadj_ipcw

############################
# Ratio IFN:IL13 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il13_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il13_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il13_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il13_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il13_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il13_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il13_t3_adj_ipcw
ratio_ifn_il13_t3_unadj_ipcw

############################
# Ratio IL12:IL17 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il17_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il17_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il17_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il17_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il17_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il17_t3_adj_ipcw$estimates$ATE))

ratio_il12_il17_t3_adj_ipcw
ratio_il12_il17_t3_unadj_ipcw

############################
# Ratio IFN:IL17 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il17_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il17_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il17_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il17_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il17_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il17_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il17_t3_adj_ipcw
ratio_ifn_il17_t3_unadj_ipcw

############################
# Ratio IL12:IL21 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_il12_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_il12_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_il12_il21_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il21_t3_unadj_ipcw<-as.data.frame(unlist(ratio_il12_il21_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_il12_il21_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_il12_il21_t3_adj_ipcw<-as.data.frame(unlist(ratio_il12_il21_t3_adj_ipcw$estimates$ATE))

ratio_il12_il21_t3_adj_ipcw
ratio_il12_il21_t3_unadj_ipcw

############################
# Ratio IFN:IL21 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_ifn_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_ifn_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_ifn_il21_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il21_t3_unadj_ipcw<-as.data.frame(unlist(ratio_ifn_il21_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_ifn_il21_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_ifn_il21_t3_adj_ipcw<-as.data.frame(unlist(ratio_ifn_il21_t3_adj_ipcw$estimates$ATE))

ratio_ifn_il21_t3_adj_ipcw
ratio_ifn_il21_t3_unadj_ipcw

############################
# Ratio pro:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_pro_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_pro_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_pro_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_pro_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_pro_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_pro_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_pro_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_pro_il10_t3_adj_ipcw$estimates$ATE))

ratio_pro_il10_t3_adj_ipcw
ratio_pro_il10_t3_unadj_ipcw

############################
# Ratio th1:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_th1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_th1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_th1_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_th1_il10_t3_adj_ipcw$estimates$ATE))

ratio_th1_il10_t3_adj_ipcw
ratio_th1_il10_t3_unadj_ipcw


############################
# Ratio th2:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_th2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_th2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th2_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th2_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_th2_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th2_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th2_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_th2_il10_t3_adj_ipcw$estimates$ATE))

ratio_th2_il10_t3_adj_ipcw
ratio_th2_il10_t3_unadj_ipcw

############################
# Ratio th17:IL10 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_th17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_th17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th17_il10_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th17_il10_t3_unadj_ipcw<-as.data.frame(unlist(ratio_th17_il10_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th17_il10_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th17_il10_t3_adj_ipcw<-as.data.frame(unlist(ratio_th17_il10_t3_adj_ipcw$estimates$ATE))

ratio_th17_il10_t3_adj_ipcw
ratio_th17_il10_t3_unadj_ipcw

############################
# Ratio th1:th2 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_th1_th2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_th1_th2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_th2_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th2_t3_unadj_ipcw<-as.data.frame(unlist(ratio_th1_th2_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_th2_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th2_t3_adj_ipcw<-as.data.frame(unlist(ratio_th1_th2_t3_adj_ipcw$estimates$ATE))

ratio_th1_th2_t3_adj_ipcw
ratio_th1_th2_t3_unadj_ipcw

############################
# Ratio th1:th17 Control vs N+WSH @T3
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$t3_ratio_th1_th17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$t3_ratio_th1_th17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

ratio_th1_th17_t3_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th17_t3_unadj_ipcw<-as.data.frame(unlist(ratio_th1_th17_t3_unadj_ipcw$estimates$ATE))

#here we do adjusted
ratio_th1_th17_t3_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
ratio_th1_th17_t3_adj_ipcw<-as.data.frame(unlist(ratio_th1_th17_t3_adj_ipcw$estimates$ATE))

ratio_th1_th17_t3_adj_ipcw
ratio_th1_th17_t3_unadj_ipcw



############################
#       DELTA
############################

#---------------------------------------
# create a shell of the full data (dfull)
# as if every index child with a live birth
# were measured at year 1 and year 2
#---------------------------------------

#load the immune lab data
washb_bd_immun<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-plasma-lab-t2-t3-ipcw.csv"), stringsAsFactors = TRUE)

#load
dfull<- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-enrol-tracking-immun-ipcw2.csv"), stringsAsFactors = TRUE)

#load unblinded treatment data
washb_bd_tr <- read.csv(paste0(dropboxDir,"Data/Untouched/Real/washb-bangladesh-tr.csv"))
table(washb_bd_tr$tr)


# merge treatment and enrollment data onto this shell of the full data
dfull <- merge(dfull,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=F)


# re-order the treatment factor for convenience, dropping the arms not included in immune
dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

# now merge the observed plasma outcomes onto this full dataset
idfull <- merge(dfull,washb_bd_immun,by=c("dataid","clusterid","childno"),all.x=T,all.y=F)


# sort the data for perfect replication with andrew on the V-fold cross-validation
idfull <- idfull[order(idfull$block,idfull$clusterid,idfull$dataid),]




#Select adjustment covariates 
Wvars<-c("monsoon_bt2", "monsoon_bt3","ageday_bt2", "ageday_bt3","sex","birthord", "momage", "momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(idfull, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#Looks good. Use this if any need to be changes:
#deleted ageday, month, faid
#in stata, made momheight 2 decimal places
#in stata, made birthord into 3 categories
#in stata, coded missing as 99


W$monsoon_bt2<-as.factor(W$monsoon_bt2)
#If already a factor:
W$monsoon_bt2<-addNA(W$monsoon_bt2)
levels(W$monsoon_bt2)[length(levels(W$monsoon_bt2))]<-"Missing"

W$monsoon_bt2<-relevel(W$monsoon_bt3, ref="1")

W$monsoon_bt3<-as.factor(W$monsoon_bt3)
#If already a factor:
W$monsoon_bt3<-addNA(W$monsoon_bt3)
levels(W$monsoon_bt3)[length(levels(W$monsoon_bt3))]<-"Missing"

W$monsoon_bt3<-relevel(W$monsoon_bt3, ref="1")

W$ageday_bt2<-as.numeric(W$ageday_bt2)

W$ageday_bt3<-as.numeric(W$ageday_bt3)


W$momage<-as.numeric(W$momage)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)


table(idfull$sex)
idfull$sex<-addNA(idfull$sex)
levels(idfull$sex)[length(levels(idfull$sex))]<-"missing"
table(idfull$sex)


W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)

############################
# Delta IGF Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_igf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_igf
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_igf_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_igf_unadj_ipcw<-as.data.frame(unlist(d23_igf_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_igf_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_igf_adj_ipcw<-as.data.frame(unlist(d23_igf_adj_ipcw$estimates$ATE))

d23_igf_adj_ipcw
d23_igf_unadj_ipcw

############################
# Delta GMC Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_gmc),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_gmc
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_gmc_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_gmc_unadj_ipcw<-as.data.frame(unlist(d23_gmc_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_gmc_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_gmc_adj_ipcw<-as.data.frame(unlist(d23_gmc_adj_ipcw$estimates$ATE))

d23_gmc_adj_ipcw
d23_gmc_unadj_ipcw

############################
# Delta IFN Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_ifn),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_ifn
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ifn_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ifn_unadj_ipcw<-as.data.frame(unlist(d23_ifn_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ifn_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ifn_adj_ipcw<-as.data.frame(unlist(d23_ifn_adj_ipcw$estimates$ATE))

d23_ifn_adj_ipcw
d23_ifn_unadj_ipcw

############################
# Delta IL10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il10_unadj_ipcw<-as.data.frame(unlist(d23_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il10_adj_ipcw<-as.data.frame(unlist(d23_il10_adj_ipcw$estimates$ATE))

d23_il10_adj_ipcw
d23_il10_unadj_ipcw

############################
# Delta IL12 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il12),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il12
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il12_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il12_unadj_ipcw<-as.data.frame(unlist(d23_il12_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il12_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il12_adj_ipcw<-as.data.frame(unlist(d23_il12_adj_ipcw$estimates$ATE))

d23_il12_adj_ipcw
d23_il12_unadj_ipcw

############################
# Delta IL13 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il13_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il13_unadj_ipcw<-as.data.frame(unlist(d23_il13_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il13_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il13_adj_ipcw<-as.data.frame(unlist(d23_il13_adj_ipcw$estimates$ATE))

d23_il13_adj_ipcw
d23_il13_unadj_ipcw

############################
# Delta IL17 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il17_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il17_unadj_ipcw<-as.data.frame(unlist(d23_il17_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il17_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il17_adj_ipcw<-as.data.frame(unlist(d23_il17_adj_ipcw$estimates$ATE))

d23_il17_adj_ipcw
d23_il17_unadj_ipcw

############################
# Delta IL1 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il1),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il1
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il1_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il1_unadj_ipcw<-as.data.frame(unlist(d23_il1_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il1_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il1_adj_ipcw<-as.data.frame(unlist(d23_il1_adj_ipcw$estimates$ATE))

d23_il1_adj_ipcw
d23_il1_unadj_ipcw

############################
# Delta IL2 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il2_unadj_ipcw<-as.data.frame(unlist(d23_il2_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il2_adj_ipcw<-as.data.frame(unlist(d23_il2_adj_ipcw$estimates$ATE))

d23_il2_adj_ipcw
d23_il2_unadj_ipcw

############################
# Delta IL21 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il21_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il21_unadj_ipcw<-as.data.frame(unlist(d23_il21_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il21_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il21_adj_ipcw<-as.data.frame(unlist(d23_il21_adj_ipcw$estimates$ATE))

d23_il21_adj_ipcw
d23_il21_unadj_ipcw

############################
# Delta IL4 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il4_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il4_unadj_ipcw<-as.data.frame(unlist(d23_il4_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il4_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il4_adj_ipcw<-as.data.frame(unlist(d23_il4_adj_ipcw$estimates$ATE))

d23_il4_adj_ipcw
d23_il4_unadj_ipcw

############################
# Delta IL5 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il5_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il5_unadj_ipcw<-as.data.frame(unlist(d23_il5_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il5_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il5_adj_ipcw<-as.data.frame(unlist(d23_il5_adj_ipcw$estimates$ATE))

d23_il5_adj_ipcw
d23_il5_unadj_ipcw

############################
# Delta IL6 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_il6),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_il6
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_il6_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il6_unadj_ipcw<-as.data.frame(unlist(d23_il6_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_il6_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_il6_adj_ipcw<-as.data.frame(unlist(d23_il6_adj_ipcw$estimates$ATE))

d23_il6_adj_ipcw
d23_il6_unadj_ipcw

############################
# Delta TNF Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ln_tnf),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ln_tnf
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_tnf_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_tnf_unadj_ipcw<-as.data.frame(unlist(d23_tnf_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_tnf_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_tnf_adj_ipcw<-as.data.frame(unlist(d23_tnf_adj_ipcw$estimates$ATE))

d23_tnf_adj_ipcw
d23_tnf_unadj_ipcw

############################
# Delta ratio gmc:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_gmc_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_gmc_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_gmc_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_gmc_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_gmc_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_gmc_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_gmc_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_gmc_il10_adj_ipcw$estimates$ATE))

d23_ratio_gmc_il10_adj_ipcw
d23_ratio_gmc_il10_unadj_ipcw

############################
# Delta ratio ifn:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il10_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il10_adj_ipcw
d23_ratio_ifn_il10_unadj_ipcw

############################
# Delta ratio il12:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il10_adj_ipcw$estimates$ATE))

d23_ratio_il12_il10_adj_ipcw
d23_ratio_il12_il10_unadj_ipcw

############################
# Delta ratio il13:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il13_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il13_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il13_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il13_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il13_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il13_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il13_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il13_il10_adj_ipcw$estimates$ATE))

d23_ratio_il13_il10_adj_ipcw
d23_ratio_il13_il10_unadj_ipcw

############################
# Delta ratio il17:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il17_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il17_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il17_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il17_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il17_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il17_il10_adj_ipcw$estimates$ATE))

d23_ratio_il17_il10_adj_ipcw
d23_ratio_il17_il10_unadj_ipcw


############################
# Delta ratio il1:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il1_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il1_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il1_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il1_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il1_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il1_il10_adj_ipcw$estimates$ATE))

d23_ratio_il1_il10_adj_ipcw
d23_ratio_il1_il10_unadj_ipcw

############################
# Delta ratio il21:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il21_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il21_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il21_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il21_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il21_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il21_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il21_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il21_il10_adj_ipcw$estimates$ATE))

d23_ratio_il21_il10_adj_ipcw
d23_ratio_il21_il10_unadj_ipcw

############################
# Delta ratio il2:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il2_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il2_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il2_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il2_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il2_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il2_il10_adj_ipcw$estimates$ATE))

d23_ratio_il2_il10_adj_ipcw
d23_ratio_il2_il10_unadj_ipcw

############################
# Delta ratio il4:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il4_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il4_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il4_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il4_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il4_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il4_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il4_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il4_il10_adj_ipcw$estimates$ATE))

d23_ratio_il4_il10_adj_ipcw
d23_ratio_il4_il10_unadj_ipcw

############################
# Delta ratio il5:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il5_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il5_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il5_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il5_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il5_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il5_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il5_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il5_il10_adj_ipcw$estimates$ATE))

d23_ratio_il5_il10_adj_ipcw
d23_ratio_il5_il10_unadj_ipcw

############################
# Delta ratio il6:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il6_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il6_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il6_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il6_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il6_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il6_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il6_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_il6_il10_adj_ipcw$estimates$ATE))

d23_ratio_il6_il10_adj_ipcw
d23_ratio_il6_il10_unadj_ipcw

############################
# Delta ratio tnf:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_tnf_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_tnf_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_tnf_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_tnf_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_tnf_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_tnf_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_tnf_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_tnf_il10_adj_ipcw$estimates$ATE))

d23_ratio_tnf_il10_adj_ipcw
d23_ratio_tnf_il10_unadj_ipcw

############################
# Delta ratio il12:il4 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il4_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il4_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il4_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il4_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il4_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il4_adj_ipcw$estimates$ATE))

d23_ratio_il12_il4_adj_ipcw
d23_ratio_il12_il4_unadj_ipcw

############################
# Delta ratio ifn:il4 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il4),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il4
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il4_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il4_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il4_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il4_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il4_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il4_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il4_adj_ipcw
d23_ratio_ifn_il4_unadj_ipcw

############################
# Delta ratio il12:il5 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il5_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il5_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il5_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il5_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il5_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il5_adj_ipcw$estimates$ATE))

d23_ratio_il12_il5_adj_ipcw
d23_ratio_il12_il5_unadj_ipcw

############################
# Delta ratio ifn:il5 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il5),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il5
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il5_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il5_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il5_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il5_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il5_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il5_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il5_adj_ipcw
d23_ratio_ifn_il5_unadj_ipcw

############################
# Delta ratio il12:il13 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il13_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il13_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il13_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il13_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il13_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il13_adj_ipcw$estimates$ATE))

d23_ratio_il12_il13_adj_ipcw
d23_ratio_il12_il13_unadj_ipcw

############################
# Delta ratio ifn:il13 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il13),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il13
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il13_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il13_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il13_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il13_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il13_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il13_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il13_adj_ipcw
d23_ratio_ifn_il13_unadj_ipcw

############################
# Delta ratio il12:il17 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il17_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il17_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il17_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il17_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il17_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il17_adj_ipcw$estimates$ATE))

d23_ratio_il12_il17_adj_ipcw
d23_ratio_il12_il17_unadj_ipcw

############################
# Delta ratio ifn:il17 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il17_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il17_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il17_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il17_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il17_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il17_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il17_adj_ipcw
d23_ratio_ifn_il17_unadj_ipcw

############################
# Delta ratio il12:il21 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_il12_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_il12_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_il12_il21_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il21_unadj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il21_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_il12_il21_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_il12_il21_adj_ipcw<-as.data.frame(unlist(d23_ratio_il12_il21_adj_ipcw$estimates$ATE))

d23_ratio_il12_il21_adj_ipcw
d23_ratio_il12_il21_unadj_ipcw

############################
# Delta ratio ifn:il21 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_ifn_il21),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_ifn_il21
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_ifn_il21_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il21_unadj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il21_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_ifn_il21_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_ifn_il21_adj_ipcw<-as.data.frame(unlist(d23_ratio_ifn_il21_adj_ipcw$estimates$ATE))

d23_ratio_ifn_il21_adj_ipcw
d23_ratio_ifn_il21_unadj_ipcw

############################
# Delta ratio pro:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_pro_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_pro_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_pro_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_pro_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_pro_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_pro_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_pro_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_pro_il10_adj_ipcw$estimates$ATE))

d23_ratio_pro_il10_adj_ipcw
d23_ratio_pro_il10_unadj_ipcw

############################
# Delta ratio th1:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_th1_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_th1_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_th1_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_th1_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_th1_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_th1_il10_adj_ipcw$estimates$ATE))

d23_ratio_th1_il10_adj_ipcw
d23_ratio_th1_il10_unadj_ipcw

############################
# Delta ratio th2:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_th2_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_th2_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_th2_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th2_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_th2_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_th2_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th2_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_th2_il10_adj_ipcw$estimates$ATE))

d23_ratio_th2_il10_adj_ipcw
d23_ratio_th2_il10_unadj_ipcw

############################
# Delta ratio th17:il10 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_th17_il10),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_th17_il10
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_th17_il10_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th17_il10_unadj_ipcw<-as.data.frame(unlist(d23_ratio_th17_il10_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_th17_il10_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th17_il10_adj_ipcw<-as.data.frame(unlist(d23_ratio_th17_il10_adj_ipcw$estimates$ATE))

d23_ratio_th17_il10_adj_ipcw
d23_ratio_th17_il10_unadj_ipcw

############################
# Delta ratio th1:th2 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_th1_th2),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_th1_th2
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_th1_th2_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_th2_unadj_ipcw<-as.data.frame(unlist(d23_ratio_th1_th2_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_th1_th2_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_th2_adj_ipcw<-as.data.frame(unlist(d23_ratio_th1_th2_adj_ipcw$estimates$ATE))

d23_ratio_th1_th2_adj_ipcw
d23_ratio_th1_th2_unadj_ipcw


############################
# Delta ratio th1:th17 Control vs N+WSH 
############################

# create an indicator equal to 1 if the outcome is observed, 0 otherwise
idfull$Delta <- ifelse(is.na(idfull$d23_ratio_th1_th17),0,1)
table(idfull$Delta[idfull$tr=="Control"|idfull$tr=="Nutrition + WSH"])

mean(idfull$Delta,na.rm=T)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
idfull$Ydelta <- idfull$d23_ratio_th1_th17
idfull$Ydelta[idfull$Delta==0] <- 9

# estimate an IPCW-TMLE parameter
#W=NULL if doing unadjusted

d23_ratio_th1_th17_unadj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=NULL,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_th17_unadj_ipcw<-as.data.frame(unlist(d23_ratio_th1_th17_unadj_ipcw$estimates$ATE))

#here we do adjusted
d23_ratio_th1_th17_adj_ipcw <- washb_tmle(Y=idfull$Ydelta,Delta=idfull$Delta,tr=idfull$tr,id=idfull$block,pair=NULL,family="gaussian",contrast=c("Control","Nutrition + WSH"),W=W,Q.SL.library = c("SL.glm"),seed=12345)

#extract items from output
d23_ratio_th1_th17_adj_ipcw<-as.data.frame(unlist(d23_ratio_th1_th17_adj_ipcw$estimates$ATE))

d23_ratio_th1_th17_adj_ipcw
d23_ratio_th1_th17_unadj_ipcw




#Display results


igf_t2_adj_ipcw
crp_t2_adj_ipcw
agp_t2_adj_ipcw
gmc_t2_adj_ipcw
ifn_t2_adj_ipcw
il10_t2_adj_ipcw
il12_t2_adj_ipcw
il13_t2_adj_ipcw
il17_t2_adj_ipcw
il1_t2_adj_ipcw
il2_t2_adj_ipcw
il21_t2_adj_ipcw
il4_t2_adj_ipcw
il5_t2_adj_ipcw
il6_t2_adj_ipcw
tnf_t2_adj_ipcw

ratio_gmc_il10_t2_adj_ipcw
ratio_ifn_il10_t2_adj_ipcw
ratio_il12_il10_t2_adj_ipcw
ratio_il13_il10_t2_adj_ipcw
ratio_il17_il10_t2_adj_ipcw
ratio_il1_il10_t2_adj_ipcw
ratio_il21_il10_t2_adj_ipcw
ratio_il2_il10_t2_adj_ipcw
ratio_il4_il10_t2_adj_ipcw
ratio_il5_il10_t2_adj_ipcw
ratio_il6_il10_t2_adj_ipcw
ratio_tnf_il10_t2_adj_ipcw

ratio_il12_il4_t2_adj_ipcw
ratio_ifn_il4_t2_adj_ipcw
ratio_il12_il5_t2_adj_ipcw
ratio_ifn_il5_t2_adj_ipcw
ratio_il12_il13_t2_adj_ipcw
ratio_ifn_il13_t2_adj_ipcw

ratio_il12_il17_t2_adj_ipcw
ratio_ifn_il17_t2_adj_ipcw
ratio_il12_il21_t2_adj_ipcw
ratio_ifn_il21_t2_adj_ipcw

ratio_pro_il10_t2_adj_ipcw
ratio_th1_il10_t2_adj_ipcw
ratio_th2_il10_t2_adj_ipcw
ratio_th17_il10_t2_adj_ipcw
ratio_th1_th2_t2_adj_ipcw
ratio_th1_th17_t2_adj_ipcw

igf_t3_adj_ipcw
gmc_t3_adj_ipcw
ifn_t3_adj_ipcw
il10_t3_adj_ipcw
il12_t3_adj_ipcw
il13_t3_adj_ipcw
il17_t3_adj_ipcw
il1_t3_adj_ipcw
il2_t3_adj_ipcw
il21_t3_adj_ipcw
il4_t3_adj_ipcw
il5_t3_adj_ipcw
il6_t3_adj_ipcw
tnf_t3_adj_ipcw

ratio_gmc_il10_t3_adj_ipcw
ratio_ifn_il10_t3_adj_ipcw
ratio_il12_il10_t3_adj_ipcw
ratio_il13_il10_t3_adj_ipcw
ratio_il17_il10_t3_adj_ipcw
ratio_il1_il10_t3_adj_ipcw
ratio_il21_il10_t3_adj_ipcw
ratio_il2_il10_t3_adj_ipcw
ratio_il4_il10_t3_adj_ipcw
ratio_il5_il10_t3_adj_ipcw
ratio_il6_il10_t3_adj_ipcw
ratio_tnf_il10_t3_adj_ipcw

ratio_il12_il4_t3_adj_ipcw
ratio_ifn_il4_t3_adj_ipcw
ratio_il12_il5_t3_adj_ipcw
ratio_ifn_il5_t3_adj_ipcw
ratio_il12_il13_t3_adj_ipcw
ratio_ifn_il13_t3_adj_ipcw

ratio_il12_il17_t3_adj_ipcw
ratio_ifn_il17_t3_adj_ipcw
ratio_il12_il21_t3_adj_ipcw
ratio_ifn_il21_t3_adj_ipcw

ratio_pro_il10_t3_adj_ipcw
ratio_th1_il10_t3_adj_ipcw
ratio_th2_il10_t3_adj_ipcw
ratio_th17_il10_t3_adj_ipcw
ratio_th1_th2_t3_adj_ipcw
ratio_th1_th17_t3_adj_ipcw

igf_t2_unadj_ipcw
crp_t2_unadj_ipcw
agp_t2_unadj_ipcw
gmc_t2_unadj_ipcw
ifn_t2_unadj_ipcw
il10_t2_unadj_ipcw
il12_t2_unadj_ipcw
il13_t2_unadj_ipcw
il17_t2_unadj_ipcw
il1_t2_unadj_ipcw
il2_t2_unadj_ipcw
il21_t2_unadj_ipcw
il4_t2_unadj_ipcw
il5_t2_unadj_ipcw
il6_t2_unadj_ipcw
tnf_t2_unadj_ipcw

ratio_gmc_il10_t2_unadj_ipcw
ratio_ifn_il10_t2_unadj_ipcw
ratio_il12_il10_t2_unadj_ipcw
ratio_il13_il10_t2_unadj_ipcw
ratio_il17_il10_t2_unadj_ipcw
ratio_il1_il10_t2_unadj_ipcw
ratio_il21_il10_t2_unadj_ipcw
ratio_il2_il10_t2_unadj_ipcw
ratio_il4_il10_t2_unadj_ipcw
ratio_il5_il10_t2_unadj_ipcw
ratio_il6_il10_t2_unadj_ipcw
ratio_tnf_il10_t2_unadj_ipcw

ratio_il12_il4_t2_unadj_ipcw
ratio_ifn_il4_t2_unadj_ipcw
ratio_il12_il5_t2_unadj_ipcw
ratio_ifn_il5_t2_unadj_ipcw
ratio_il12_il13_t2_unadj_ipcw
ratio_ifn_il13_t2_unadj_ipcw

ratio_il12_il17_t2_unadj_ipcw
ratio_ifn_il17_t2_unadj_ipcw
ratio_il12_il21_t2_unadj_ipcw
ratio_ifn_il21_t2_unadj_ipcw

ratio_pro_il10_t2_unadj_ipcw
ratio_th1_il10_t2_unadj_ipcw
ratio_th2_il10_t2_unadj_ipcw
ratio_th17_il10_t2_unadj_ipcw
ratio_th1_th2_t2_unadj_ipcw
ratio_th1_th17_t2_unadj_ipcw

igf_t3_unadj_ipcw
gmc_t3_unadj_ipcw
ifn_t3_unadj_ipcw
il10_t3_unadj_ipcw
il12_t3_unadj_ipcw
il13_t3_unadj_ipcw
il17_t3_unadj_ipcw
il1_t3_unadj_ipcw
il2_t3_unadj_ipcw
il21_t3_unadj_ipcw
il4_t3_unadj_ipcw
il5_t3_unadj_ipcw
il6_t3_unadj_ipcw
tnf_t3_unadj_ipcw

ratio_gmc_il10_t3_unadj_ipcw
ratio_ifn_il10_t3_unadj_ipcw
ratio_il12_il10_t3_unadj_ipcw
ratio_il13_il10_t3_unadj_ipcw
ratio_il17_il10_t3_unadj_ipcw
ratio_il1_il10_t3_unadj_ipcw
ratio_il21_il10_t3_unadj_ipcw
ratio_il2_il10_t3_unadj_ipcw
ratio_il4_il10_t3_unadj_ipcw
ratio_il5_il10_t3_unadj_ipcw
ratio_il6_il10_t3_unadj_ipcw
ratio_tnf_il10_t3_unadj_ipcw

ratio_il12_il4_t3_unadj_ipcw
ratio_ifn_il4_t3_unadj_ipcw
ratio_il12_il5_t3_unadj_ipcw
ratio_ifn_il5_t3_unadj_ipcw
ratio_il12_il13_t3_unadj_ipcw
ratio_ifn_il13_t3_unadj_ipcw

ratio_il12_il17_t3_unadj_ipcw
ratio_ifn_il17_t3_unadj_ipcw
ratio_il12_il21_t3_unadj_ipcw
ratio_ifn_il21_t3_unadj_ipcw

ratio_pro_il10_t3_unadj_ipcw
ratio_th1_il10_t3_unadj_ipcw
ratio_th2_il10_t3_unadj_ipcw
ratio_th17_il10_t3_unadj_ipcw
ratio_th1_th2_t3_unadj_ipcw
ratio_th1_th17_t3_unadj_ipcw

d23_igf_adj_ipcw
d23_gmc_adj_ipcw
d23_ifn_adj_ipcw
d23_il10_adj_ipcw
d23_il12_adj_ipcw
d23_il13_adj_ipcw
d23_il17_adj_ipcw
d23_il1_adj_ipcw
d23_il2_adj_ipcw
d23_il21_adj_ipcw
d23_il4_adj_ipcw
d23_il5_adj_ipcw
d23_il6_adj_ipcw
d23_tnf_adj_ipcw

d23_ratio_gmc_il10_adj_ipcw
d23_ratio_ifn_il10_adj_ipcw
d23_ratio_il12_il10_adj_ipcw
d23_ratio_il13_il10_adj_ipcw
d23_ratio_il17_il10_adj_ipcw
d23_ratio_il1_il10_adj_ipcw
d23_ratio_il21_il10_adj_ipcw
d23_ratio_il2_il10_adj_ipcw
d23_ratio_il4_il10_adj_ipcw
d23_ratio_il5_il10_adj_ipcw
d23_ratio_il6_il10_adj_ipcw
d23_ratio_tnf_il10_adj_ipcw

d23_ratio_il12_il4_adj_ipcw
d23_ratio_ifn_il4_adj_ipcw
d23_ratio_il12_il5_adj_ipcw
d23_ratio_ifn_il5_adj_ipcw
d23_ratio_il12_il13_adj_ipcw
d23_ratio_ifn_il13_adj_ipcw

d23_ratio_il12_il17_adj_ipcw
d23_ratio_ifn_il17_adj_ipcw
d23_ratio_il12_il21_adj_ipcw
d23_ratio_ifn_il21_adj_ipcw

d23_ratio_pro_il10_adj_ipcw
d23_ratio_th1_il10_adj_ipcw
d23_ratio_th2_il10_adj_ipcw
d23_ratio_th17_il10_adj_ipcw
d23_ratio_th1_th2_adj_ipcw
d23_ratio_th1_th17_adj_ipcw

d23_igf_unadj_ipcw
d23_gmc_unadj_ipcw
d23_ifn_unadj_ipcw
d23_il10_unadj_ipcw
d23_il12_unadj_ipcw
d23_il13_unadj_ipcw
d23_il17_unadj_ipcw
d23_il1_unadj_ipcw
d23_il2_unadj_ipcw
d23_il21_unadj_ipcw
d23_il4_unadj_ipcw
d23_il5_unadj_ipcw
d23_il6_unadj_ipcw
d23_tnf_unadj_ipcw

d23_ratio_gmc_il10_unadj_ipcw
d23_ratio_ifn_il10_unadj_ipcw
d23_ratio_il12_il10_unadj_ipcw
d23_ratio_il13_il10_unadj_ipcw
d23_ratio_il17_il10_unadj_ipcw
d23_ratio_il1_il10_unadj_ipcw
d23_ratio_il21_il10_unadj_ipcw
d23_ratio_il2_il10_unadj_ipcw
d23_ratio_il4_il10_unadj_ipcw
d23_ratio_il5_il10_unadj_ipcw
d23_ratio_il6_il10_unadj_ipcw
d23_ratio_tnf_il10_unadj_ipcw

d23_ratio_il12_il4_unadj_ipcw
d23_ratio_ifn_il4_unadj_ipcw
d23_ratio_il12_il5_unadj_ipcw
d23_ratio_ifn_il5_unadj_ipcw
d23_ratio_il12_il13_unadj_ipcw
d23_ratio_ifn_il13_unadj_ipcw

d23_ratio_il12_il17_unadj_ipcw
d23_ratio_ifn_il17_unadj_ipcw
d23_ratio_il12_il21_unadj_ipcw
d23_ratio_ifn_il21_unadj_ipcw

d23_ratio_pro_il10_unadj_ipcw
d23_ratio_th1_il10_unadj_ipcw
d23_ratio_th2_il10_unadj_ipcw
d23_ratio_th17_il10_unadj_ipcw
d23_ratio_th1_th2_unadj_ipcw
d23_ratio_th1_th17_unadj_ipcw

#rename R objects



igf_t2_adj_ipcw_L <- igf_t2_adj_ipcw
crp_t2_adj_ipcw_L <- crp_t2_adj_ipcw
agp_t2_adj_ipcw_L <- agp_t2_adj_ipcw
gmc_t2_adj_ipcw_L <- gmc_t2_adj_ipcw
ifn_t2_adj_ipcw_L <- ifn_t2_adj_ipcw
il10_t2_adj_ipcw_L <- il10_t2_adj_ipcw
il12_t2_adj_ipcw_L <- il12_t2_adj_ipcw
il13_t2_adj_ipcw_L <- il13_t2_adj_ipcw
il17_t2_adj_ipcw_L <- il17_t2_adj_ipcw
il1_t2_adj_ipcw_L <- il1_t2_adj_ipcw
il2_t2_adj_ipcw_L <- il2_t2_adj_ipcw
il21_t2_adj_ipcw_L <- il21_t2_adj_ipcw
il4_t2_adj_ipcw_L <- il4_t2_adj_ipcw
il5_t2_adj_ipcw_L <- il5_t2_adj_ipcw
il6_t2_adj_ipcw_L <- il6_t2_adj_ipcw
tnf_t2_adj_ipcw_L <- tnf_t2_adj_ipcw


ratio_gmc_il10_t2_adj_ipcw_L <- ratio_gmc_il10_t2_adj_ipcw
ratio_ifn_il10_t2_adj_ipcw_L <- ratio_ifn_il10_t2_adj_ipcw
ratio_il12_il10_t2_adj_ipcw_L <- ratio_il12_il10_t2_adj_ipcw
ratio_il13_il10_t2_adj_ipcw_L <- ratio_il13_il10_t2_adj_ipcw
ratio_il17_il10_t2_adj_ipcw_L <- ratio_il17_il10_t2_adj_ipcw
ratio_il1_il10_t2_adj_ipcw_L <- ratio_il1_il10_t2_adj_ipcw
ratio_il21_il10_t2_adj_ipcw_L <- ratio_il21_il10_t2_adj_ipcw
ratio_il2_il10_t2_adj_ipcw_L <- ratio_il2_il10_t2_adj_ipcw
ratio_il4_il10_t2_adj_ipcw_L <- ratio_il4_il10_t2_adj_ipcw
ratio_il5_il10_t2_adj_ipcw_L <- ratio_il5_il10_t2_adj_ipcw
ratio_il6_il10_t2_adj_ipcw_L <- ratio_il6_il10_t2_adj_ipcw
ratio_tnf_il10_t2_adj_ipcw_L <- ratio_tnf_il10_t2_adj_ipcw

ratio_il12_il4_t2_adj_ipcw_L <- ratio_il12_il4_t2_adj_ipcw
ratio_ifn_il4_t2_adj_ipcw_L <- ratio_ifn_il4_t2_adj_ipcw
ratio_il12_il5_t2_adj_ipcw_L <- ratio_il12_il5_t2_adj_ipcw
ratio_ifn_il5_t2_adj_ipcw_L <- ratio_ifn_il5_t2_adj_ipcw
ratio_il12_il13_t2_adj_ipcw_L <- ratio_il12_il13_t2_adj_ipcw
ratio_ifn_il13_t2_adj_ipcw_L <- ratio_ifn_il13_t2_adj_ipcw

ratio_il12_il17_t2_adj_ipcw_L <- ratio_il12_il17_t2_adj_ipcw
ratio_ifn_il17_t2_adj_ipcw_L <- ratio_ifn_il17_t2_adj_ipcw
ratio_il12_il21_t2_adj_ipcw_L <- ratio_il12_il21_t2_adj_ipcw
ratio_ifn_il21_t2_adj_ipcw_L <- ratio_ifn_il21_t2_adj_ipcw

ratio_pro_il10_t2_adj_ipcw_L <- ratio_pro_il10_t2_adj_ipcw
ratio_th1_il10_t2_adj_ipcw_L <- ratio_th1_il10_t2_adj_ipcw
ratio_th2_il10_t2_adj_ipcw_L <- ratio_th2_il10_t2_adj_ipcw
ratio_th17_il10_t2_adj_ipcw_L <- ratio_th17_il10_t2_adj_ipcw
ratio_th1_th2_t2_adj_ipcw_L <- ratio_th1_th2_t2_adj_ipcw
ratio_th1_th17_t2_adj_ipcw_L <- ratio_th1_th17_t2_adj_ipcw


igf_t3_adj_ipcw_L <- igf_t3_adj_ipcw
gmc_t3_adj_ipcw_L <- gmc_t3_adj_ipcw
ifn_t3_adj_ipcw_L <- ifn_t3_adj_ipcw
il10_t3_adj_ipcw_L <- il10_t3_adj_ipcw
il12_t3_adj_ipcw_L <- il12_t3_adj_ipcw
il13_t3_adj_ipcw_L <- il13_t3_adj_ipcw
il17_t3_adj_ipcw_L <- il17_t3_adj_ipcw
il1_t3_adj_ipcw_L <- il1_t3_adj_ipcw
il2_t3_adj_ipcw_L <- il2_t3_adj_ipcw
il21_t3_adj_ipcw_L <- il21_t3_adj_ipcw
il4_t3_adj_ipcw_L <- il4_t3_adj_ipcw
il5_t3_adj_ipcw_L <- il5_t3_adj_ipcw
il6_t3_adj_ipcw_L <- il6_t3_adj_ipcw
tnf_t3_adj_ipcw_L <- tnf_t3_adj_ipcw


ratio_gmc_il10_t3_adj_ipcw_L <- ratio_gmc_il10_t3_adj_ipcw
ratio_ifn_il10_t3_adj_ipcw_L <- ratio_ifn_il10_t3_adj_ipcw
ratio_il12_il10_t3_adj_ipcw_L <- ratio_il12_il10_t3_adj_ipcw
ratio_il13_il10_t3_adj_ipcw_L <- ratio_il13_il10_t3_adj_ipcw
ratio_il17_il10_t3_adj_ipcw_L <- ratio_il17_il10_t3_adj_ipcw
ratio_il1_il10_t3_adj_ipcw_L <- ratio_il1_il10_t3_adj_ipcw
ratio_il21_il10_t3_adj_ipcw_L <- ratio_il21_il10_t3_adj_ipcw
ratio_il2_il10_t3_adj_ipcw_L <- ratio_il2_il10_t3_adj_ipcw
ratio_il4_il10_t3_adj_ipcw_L <- ratio_il4_il10_t3_adj_ipcw
ratio_il5_il10_t3_adj_ipcw_L <- ratio_il5_il10_t3_adj_ipcw
ratio_il6_il10_t3_adj_ipcw_L <- ratio_il6_il10_t3_adj_ipcw
ratio_tnf_il10_t3_adj_ipcw_L <- ratio_tnf_il10_t3_adj_ipcw

ratio_il12_il4_t3_adj_ipcw_L <- ratio_il12_il4_t3_adj_ipcw
ratio_ifn_il4_t3_adj_ipcw_L <- ratio_ifn_il4_t3_adj_ipcw
ratio_il12_il5_t3_adj_ipcw_L <- ratio_il12_il5_t3_adj_ipcw
ratio_ifn_il5_t3_adj_ipcw_L <- ratio_ifn_il5_t3_adj_ipcw
ratio_il12_il13_t3_adj_ipcw_L <- ratio_il12_il13_t3_adj_ipcw
ratio_ifn_il13_t3_adj_ipcw_L <- ratio_ifn_il13_t3_adj_ipcw

ratio_il12_il17_t3_adj_ipcw_L <- ratio_il12_il17_t3_adj_ipcw
ratio_ifn_il17_t3_adj_ipcw_L <- ratio_ifn_il17_t3_adj_ipcw
ratio_il12_il21_t3_adj_ipcw_L <- ratio_il12_il21_t3_adj_ipcw
ratio_ifn_il21_t3_adj_ipcw_L <- ratio_ifn_il21_t3_adj_ipcw

ratio_pro_il10_t3_adj_ipcw_L <- ratio_pro_il10_t3_adj_ipcw
ratio_th1_il10_t3_adj_ipcw_L <- ratio_th1_il10_t3_adj_ipcw
ratio_th2_il10_t3_adj_ipcw_L <- ratio_th2_il10_t3_adj_ipcw
ratio_th17_il10_t3_adj_ipcw_L <- ratio_th17_il10_t3_adj_ipcw
ratio_th1_th2_t3_adj_ipcw_L <- ratio_th1_th2_t3_adj_ipcw
ratio_th1_th17_t3_adj_ipcw_L <- ratio_th1_th17_t3_adj_ipcw


d23_igf_adj_ipcw_L <- d23_igf_adj_ipcw
d23_gmc_adj_ipcw_L <- d23_gmc_adj_ipcw
d23_ifn_adj_ipcw_L <- d23_ifn_adj_ipcw
d23_il10_adj_ipcw_L <- d23_il10_adj_ipcw
d23_il12_adj_ipcw_L <- d23_il12_adj_ipcw
d23_il13_adj_ipcw_L <- d23_il13_adj_ipcw
d23_il17_adj_ipcw_L <- d23_il17_adj_ipcw
d23_il1_adj_ipcw_L <- d23_il1_adj_ipcw
d23_il2_adj_ipcw_L <- d23_il2_adj_ipcw
d23_il21_adj_ipcw_L <- d23_il21_adj_ipcw
d23_il4_adj_ipcw_L <- d23_il4_adj_ipcw
d23_il5_adj_ipcw_L <- d23_il5_adj_ipcw
d23_il6_adj_ipcw_L <- d23_il6_adj_ipcw
d23_tnf_adj_ipcw_L <- d23_tnf_adj_ipcw

d23_ratio_gmc_il10_adj_ipcw_L <- d23_ratio_gmc_il10_adj_ipcw
d23_ratio_ifn_il10_adj_ipcw_L <- d23_ratio_ifn_il10_adj_ipcw
d23_ratio_il12_il10_adj_ipcw_L <- d23_ratio_il12_il10_adj_ipcw
d23_ratio_il13_il10_adj_ipcw_L <- d23_ratio_il13_il10_adj_ipcw
d23_ratio_il17_il10_adj_ipcw_L <- d23_ratio_il17_il10_adj_ipcw
d23_ratio_il1_il10_adj_ipcw_L <- d23_ratio_il1_il10_adj_ipcw
d23_ratio_il21_il10_adj_ipcw_L <- d23_ratio_il21_il10_adj_ipcw
d23_ratio_il2_il10_adj_ipcw_L <- d23_ratio_il2_il10_adj_ipcw
d23_ratio_il4_il10_adj_ipcw_L <- d23_ratio_il4_il10_adj_ipcw
d23_ratio_il5_il10_adj_ipcw_L <- d23_ratio_il5_il10_adj_ipcw
d23_ratio_il6_il10_adj_ipcw_L <- d23_ratio_il6_il10_adj_ipcw
d23_ratio_tnf_il10_adj_ipcw_L <- d23_ratio_tnf_il10_adj_ipcw

d23_ratio_il12_il4_adj_ipcw_L <- d23_ratio_il12_il4_adj_ipcw
d23_ratio_ifn_il4_adj_ipcw_L <- d23_ratio_ifn_il4_adj_ipcw
d23_ratio_il12_il5_adj_ipcw_L <- d23_ratio_il12_il5_adj_ipcw
d23_ratio_ifn_il5_adj_ipcw_L <- d23_ratio_ifn_il5_adj_ipcw
d23_ratio_il12_il13_adj_ipcw_L <- d23_ratio_il12_il13_adj_ipcw
d23_ratio_ifn_il13_adj_ipcw_L <- d23_ratio_ifn_il13_adj_ipcw

d23_ratio_il12_il17_adj_ipcw_L <- d23_ratio_il12_il17_adj_ipcw
d23_ratio_ifn_il17_adj_ipcw_L <- d23_ratio_ifn_il17_adj_ipcw
d23_ratio_il12_il21_adj_ipcw_L <- d23_ratio_il12_il21_adj_ipcw
d23_ratio_ifn_il21_adj_ipcw_L <- d23_ratio_ifn_il21_adj_ipcw

d23_ratio_pro_il10_adj_ipcw_L <- d23_ratio_pro_il10_adj_ipcw
d23_ratio_th1_il10_adj_ipcw_L <- d23_ratio_th1_il10_adj_ipcw
d23_ratio_th2_il10_adj_ipcw_L <- d23_ratio_th2_il10_adj_ipcw
d23_ratio_th17_il10_adj_ipcw_L <- d23_ratio_th17_il10_adj_ipcw
d23_ratio_th1_th2_adj_ipcw_L <- d23_ratio_th1_th2_adj_ipcw
d23_ratio_th1_th17_adj_ipcw_L <- d23_ratio_th1_th17_adj_ipcw

#-----------------------------------
#save data
#-----------------------------------

save(igf_t2_adj_ipcw_L,
  crp_t2_adj_ipcw_L,
  agp_t2_adj_ipcw_L, 
  gmc_t2_adj_ipcw_L,
  ifn_t2_adj_ipcw_L, 
  il10_t2_adj_ipcw_L, 
  il12_t2_adj_ipcw_L, 
  il13_t2_adj_ipcw_L, 
  il17_t2_adj_ipcw_L,
  il1_t2_adj_ipcw_L, 
  il2_t2_adj_ipcw_L,
  il21_t2_adj_ipcw_L,
  il4_t2_adj_ipcw_L,
  il5_t2_adj_ipcw_L,
  il6_t2_adj_ipcw_L,
  tnf_t2_adj_ipcw_L,
  
  igf_t3_adj_ipcw_L,
  gmc_t3_adj_ipcw_L,
  ifn_t3_adj_ipcw_L,
  il10_t3_adj_ipcw_L,
  il12_t3_adj_ipcw_L,
  il13_t3_adj_ipcw_L,
  il17_t3_adj_ipcw_L,
  il1_t3_adj_ipcw_L,
  il2_t3_adj_ipcw_L,
  il21_t3_adj_ipcw_L,
  il4_t3_adj_ipcw_L,
  il5_t3_adj_ipcw_L,
  il6_t3_adj_ipcw_L,
  tnf_t3_adj_ipcw_L,
  
  ratio_gmc_il10_t2_adj_ipcw_L,
  ratio_ifn_il10_t2_adj_ipcw_L,
  ratio_il12_il10_t2_adj_ipcw_L,
  ratio_il13_il10_t2_adj_ipcw_L,
  ratio_il17_il10_t2_adj_ipcw_L,
  ratio_il1_il10_t2_adj_ipcw_L, 
  ratio_il21_il10_t2_adj_ipcw_L,
  ratio_il2_il10_t2_adj_ipcw_L,
  ratio_il4_il10_t2_adj_ipcw_L,
  ratio_il5_il10_t2_adj_ipcw_L,
  ratio_il6_il10_t2_adj_ipcw_L,
  ratio_tnf_il10_t2_adj_ipcw_L,
  
  ratio_il12_il4_t2_adj_ipcw_L,
  ratio_ifn_il4_t2_adj_ipcw_L,
  ratio_il12_il5_t2_adj_ipcw_L,
  ratio_ifn_il5_t2_adj_ipcw_L, 
  ratio_il12_il13_t2_adj_ipcw_L,
  ratio_ifn_il13_t2_adj_ipcw_L,
  
  ratio_il12_il17_t2_adj_ipcw_L,
  ratio_ifn_il17_t2_adj_ipcw_L, 
  ratio_il12_il21_t2_adj_ipcw_L,
  ratio_ifn_il21_t2_adj_ipcw_L,
  
  ratio_pro_il10_t2_adj_ipcw_L,
  ratio_th1_il10_t2_adj_ipcw_L,
  ratio_th2_il10_t2_adj_ipcw_L,
  ratio_th17_il10_t2_adj_ipcw_L,
  ratio_th1_th2_t2_adj_ipcw_L,
  ratio_th1_th17_t2_adj_ipcw_L, 
  
  ratio_gmc_il10_t3_adj_ipcw_L,
  ratio_ifn_il10_t3_adj_ipcw_L,
  ratio_il12_il10_t3_adj_ipcw_L,
  ratio_il13_il10_t3_adj_ipcw_L,
  ratio_il17_il10_t3_adj_ipcw_L,
  ratio_il1_il10_t3_adj_ipcw_L, 
  ratio_il21_il10_t3_adj_ipcw_L,
  ratio_il2_il10_t3_adj_ipcw_L,
  ratio_il4_il10_t3_adj_ipcw_L,
  ratio_il5_il10_t3_adj_ipcw_L,
  ratio_il6_il10_t3_adj_ipcw_L,
  ratio_tnf_il10_t3_adj_ipcw_L,
  
  ratio_il12_il4_t3_adj_ipcw_L,
  ratio_ifn_il4_t3_adj_ipcw_L,
  ratio_il12_il5_t3_adj_ipcw_L,
  ratio_ifn_il5_t3_adj_ipcw_L, 
  ratio_il12_il13_t3_adj_ipcw_L,
  ratio_ifn_il13_t3_adj_ipcw_L,
  
  ratio_il12_il17_t3_adj_ipcw_L,
  ratio_ifn_il17_t3_adj_ipcw_L, 
  ratio_il12_il21_t3_adj_ipcw_L,
  ratio_ifn_il21_t3_adj_ipcw_L,
  
  ratio_pro_il10_t3_adj_ipcw_L,
  ratio_th1_il10_t3_adj_ipcw_L,
  ratio_th2_il10_t3_adj_ipcw_L,
  ratio_th17_il10_t3_adj_ipcw_L,
  ratio_th1_th2_t3_adj_ipcw_L,
  ratio_th1_th17_t3_adj_ipcw_L, 
  
  d23_igf_adj_ipcw_L,
  d23_gmc_adj_ipcw_L,
  d23_ifn_adj_ipcw_L,
  d23_il10_adj_ipcw_L,
  d23_il12_adj_ipcw_L,
  d23_il13_adj_ipcw_L,
  d23_il17_adj_ipcw_L,
  d23_il1_adj_ipcw_L,
  d23_il2_adj_ipcw_L,
  d23_il21_adj_ipcw_L,
  d23_il4_adj_ipcw_L,
  d23_il5_adj_ipcw_L,
  d23_il6_adj_ipcw_L,
  d23_tnf_adj_ipcw_L,
  
  d23_ratio_gmc_il10_adj_ipcw_L,
  d23_ratio_ifn_il10_adj_ipcw_L,
  d23_ratio_il12_il10_adj_ipcw_L,
  d23_ratio_il13_il10_adj_ipcw_L,
  d23_ratio_il17_il10_adj_ipcw_L,
  d23_ratio_il1_il10_adj_ipcw_L,
  d23_ratio_il21_il10_adj_ipcw_L,
  d23_ratio_il2_il10_adj_ipcw_L,
  d23_ratio_il4_il10_adj_ipcw_L,
  d23_ratio_il5_il10_adj_ipcw_L,
  d23_ratio_il6_il10_adj_ipcw_L,
  d23_ratio_tnf_il10_adj_ipcw_L,
  
  d23_ratio_il12_il4_adj_ipcw_L,
  d23_ratio_ifn_il4_adj_ipcw_L,
  d23_ratio_il12_il5_adj_ipcw_L,
  d23_ratio_ifn_il5_adj_ipcw_L,
  d23_ratio_il12_il13_adj_ipcw_L,
  d23_ratio_ifn_il13_adj_ipcw_L,
  
  d23_ratio_il12_il17_adj_ipcw_L,
  d23_ratio_ifn_il17_adj_ipcw_L,
  d23_ratio_il12_il21_adj_ipcw_L,
  d23_ratio_ifn_il21_adj_ipcw_L,
  
  d23_ratio_pro_il10_adj_ipcw_L,
  d23_ratio_th1_il10_adj_ipcw_L,
  d23_ratio_th2_il10_adj_ipcw_L,
  d23_ratio_th17_il10_adj_ipcw_L,
  d23_ratio_th1_th2_adj_ipcw_L,
  d23_ratio_th1_th17_adj_ipcw_L, file=here("audrie results/immune_ipcw.RData"))

save(idfull, file=here("audrie results/idfull.RData"))

#unadjusted IPCW objects

#d23_igf_unadj_ipcw_L <- d23_igf_unadj_ipcw
#d23_gmc_unadj_ipcw_L <- d23_gmc_unadj_ipcw
#d23_ifn_unadj_ipcw_L <- d23_ifn_unadj_ipcw
#d23_il10_unadj_ipcw_L <- d23_il10_unadj_ipcw
#d23_il12_unadj_ipcw_L <- d23_il12_unadj_ipcw
#d23_il13_unadj_ipcw_L <- d23_il13_unadj_ipcw
#d23_il17_unadj_ipcw_L <- d23_il17_unadj_ipcw
#d23_il1_unadj_ipcw_L <- d23_il1_unadj_ipcw
#d23_il2_unadj_ipcw_L <- d23_il2_unadj_ipcw
#d23_il21_unadj_ipcw_L <- d23_il21_unadj_ipcw
#d23_il4_unadj_ipcw_L <- d23_il4_unadj_ipcw
#d23_il5_unadj_ipcw_L <- d23_il5_unadj_ipcw
#d23_il6_unadj_ipcw_L <- d23_il6_unadj_ipcw
#d23_tnf_unadj_ipcw_L <- d23_tnf_unadj_ipcw

#d23_ratio_gmc_il10_unadj_ipcw_L <- d23_ratio_gmc_il10_unadj_ipcw
#d23_ratio_ifn_il10_unadj_ipcw_L <- d23_ratio_ifn_il10_unadj_ipcw
#d23_ratio_il12_il10_unadj_ipcw_L <- d23_ratio_il12_il10_unadj_ipcw
#d23_ratio_il13_il10_unadj_ipcw_L <- d23_ratio_il13_il10_unadj_ipcw
#d23_ratio_il17_il10_unadj_ipcw_L <- d23_ratio_il17_il10_unadj_ipcw
#d23_ratio_il1_il10_unadj_ipcw_L <- d23_ratio_il1_il10_unadj_ipcw
#d23_ratio_il21_il10_unadj_ipcw_L <- d23_ratio_il21_il10_unadj_ipcw
#d23_ratio_il2_il10_unadj_ipcw_L <- d23_ratio_il2_il10_unadj_ipcw
#d23_ratio_il4_il10_unadj_ipcw_L <- d23_ratio_il4_il10_unadj_ipcw
#d23_ratio_il5_il10_unadj_ipcw_L <- d23_ratio_il5_il10_unadj_ipcw
#d23_ratio_il6_il10_unadj_ipcw_L <- d23_ratio_il6_il10_unadj_ipcw
#d23_ratio_tnf_il10_unadj_ipcw_L <- d23_ratio_tnf_il10_unadj_ipcw

#d23_ratio_il12_il4_unadj_ipcw_L <- d23_ratio_il12_il4_unadj_ipcw
#d23_ratio_ifn_il4_unadj_ipcw_L <- d23_ratio_ifn_il4_unadj_ipcw
#d23_ratio_il12_il5_unadj_ipcw_L <- d23_ratio_il12_il5_unadj_ipcw
#d23_ratio_ifn_il5_unadj_ipcw_L <- d23_ratio_ifn_il5_unadj_ipcw
#d23_ratio_il12_il13_unadj_ipcw_L <- d23_ratio_il12_il13_unadj_ipcw
#d23_ratio_ifn_il13_unadj_ipcw_L <- d23_ratio_ifn_il13_unadj_ipcw

#d23_ratio_il12_il17_unadj_ipcw_L <- d23_ratio_il12_il17_unadj_ipcw
#d23_ratio_ifn_il17_unadj_ipcw_L <- d23_ratio_ifn_il17_unadj_ipcw
#d23_ratio_il12_il21_unadj_ipcw_L <- d23_ratio_il12_il21_unadj_ipcw
#d23_ratio_ifn_il21_unadj_ipcw_L <- d23_ratio_ifn_il21_unadj_ipcw

#d23_ratio_pro_il10_unadj_ipcw_L <- d23_ratio_pro_il10_unadj_ipcw
#d23_ratio_th1_il10_unadj_ipcw_L <- d23_ratio_th1_il10_unadj_ipcw
#d23_ratio_th2_il10_unadj_ipcw_L <- d23_ratio_th2_il10_unadj_ipcw
#d23_ratio_th17_il10_unadj_ipcw_L <- d23_ratio_th17_il10_unadj_ipcw
#d23_ratio_th1_th2_unadj_ipcw_L <- d23_ratio_th1_th2_unadj_ipcw
#d23_ratio_th1_th17_unadj_ipcw_L <- d23_ratio_th1_th17_unadj_ipcw

#igf_t2_unadj_ipcw_L <- igf_t2_unadj_ipcw
#crp_t2_unadj_ipcw_L <- crp_t2_unadj_ipcw
#agp_t2_unadj_ipcw_L <- agp_t2_unadj_ipcw
#gmc_t2_unadj_ipcw_L <- gmc_t2_unadj_ipcw
#ifn_t2_unadj_ipcw_L <- ifn_t2_unadj_ipcw
#il10_t2_unadj_ipcw_L <- il10_t2_unadj_ipcw
#il12_t2_unadj_ipcw_L <- il12_t2_unadj_ipcw
#il13_t2_unadj_ipcw_L <- il13_t2_unadj_ipcw
#il17_t2_unadj_ipcw_L <- il17_t2_unadj_ipcw
#il1_t2_unadj_ipcw_L <- il1_t2_unadj_ipcw
#il2_t2_unadj_ipcw_L <- il2_t2_unadj_ipcw
#il21_t2_unadj_ipcw_L <- il21_t2_unadj_ipcw
#il4_t2_unadj_ipcw_L <- il4_t2_unadj_ipcw
#il5_t2_unadj_ipcw_L <- il5_t2_unadj_ipcw
#il6_t2_unadj_ipcw_L <- il6_t2_unadj_ipcw
#tnf_t2_unadj_ipcw_L <- tnf_t2_unadj_ipcw

#ratio_gmc_il10_t2_unadj_ipcw_L <- ratio_gmc_il10_t2_unadj_ipcw
#ratio_ifn_il10_t2_unadj_ipcw_L <- ratio_ifn_il10_t2_unadj_ipcw
#ratio_il12_il10_t2_unadj_ipcw_L <- ratio_il12_il10_t2_unadj_ipcw
#ratio_il13_il10_t2_unadj_ipcw_L <- ratio_il13_il10_t2_unadj_ipcw
#ratio_il17_il10_t2_unadj_ipcw_L <- ratio_il17_il10_t2_unadj_ipcw
#ratio_il1_il10_t2_unadj_ipcw_L <- ratio_il1_il10_t2_unadj_ipcw
#ratio_il21_il10_t2_unadj_ipcw_L <- ratio_il21_il10_t2_unadj_ipcw
#ratio_il2_il10_t2_unadj_ipcw_L <- ratio_il2_il10_t2_unadj_ipcw
#ratio_il4_il10_t2_unadj_ipcw_L <- ratio_il4_il10_t2_unadj_ipcw
#ratio_il5_il10_t2_unadj_ipcw_L <- ratio_il5_il10_t2_unadj_ipcw
#ratio_il6_il10_t2_unadj_ipcw_L <- ratio_il6_il10_t2_unadj_ipcw
#ratio_tnf_il10_t2_unadj_ipcw_L <- ratio_tnf_il10_t2_unadj_ipcw

#ratio_il12_il4_t2_unadj_ipcw_L <- ratio_il12_il4_t2_unadj_ipcw
#ratio_ifn_il4_t2_unadj_ipcw_L <- ratio_ifn_il4_t2_unadj_ipcw
#ratio_il12_il5_t2_unadj_ipcw_L <- ratio_il12_il5_t2_unadj_ipcw
#ratio_ifn_il5_t2_unadj_ipcw_L <- ratio_ifn_il5_t2_unadj_ipcw
#ratio_il12_il13_t2_unadj_ipcw_L <- ratio_il12_il13_t2_unadj_ipcw
#ratio_ifn_il13_t2_unadj_ipcw_L <- ratio_ifn_il13_t2_unadj_ipcw

#ratio_il12_il17_t2_unadj_ipcw_L <- ratio_il12_il17_t2_unadj_ipcw
#ratio_ifn_il17_t2_unadj_ipcw_L <- ratio_ifn_il17_t2_unadj_ipcw
#ratio_il12_il21_t2_unadj_ipcw_L <- ratio_il12_il21_t2_unadj_ipcw
#ratio_ifn_il21_t2_unadj_ipcw_L <- ratio_ifn_il21_t2_unadj_ipcw

#ratio_pro_il10_t2_unadj_ipcw_L <- ratio_pro_il10_t2_unadj_ipcw
#ratio_th1_il10_t2_unadj_ipcw_L <- ratio_th1_il10_t2_unadj_ipcw
#ratio_th2_il10_t2_unadj_ipcw_L <- ratio_th2_il10_t2_unadj_ipcw
#ratio_th17_il10_t2_unadj_ipcw_L <- ratio_th17_il10_t2_unadj_ipcw
#ratio_th1_th2_t2_unadj_ipcw_L <- ratio_th1_th2_t2_unadj_ipcw
#ratio_th1_th17_t2_unadj_ipcw_L <- ratio_th1_th17_t2_unadj_ipcw

#igf_t3_unadj_ipcw_L <- igf_t3_unadj_ipcw
#gmc_t3_unadj_ipcw_L <- gmc_t3_unadj_ipcw
#ifn_t3_unadj_ipcw_L <- ifn_t3_unadj_ipcw
#il10_t3_unadj_ipcw_L <- il10_t3_unadj_ipcw
#il12_t3_unadj_ipcw_L <- il12_t3_unadj_ipcw
#il13_t3_unadj_ipcw_L <- il13_t3_unadj_ipcw
#il17_t3_unadj_ipcw_L <- il17_t3_unadj_ipcw
#il1_t3_unadj_ipcw_L <- il1_t3_unadj_ipcw
#il2_t3_unadj_ipcw_L <- il2_t3_unadj_ipcw
#il21_t3_unadj_ipcw_L <- il21_t3_unadj_ipcw
#il4_t3_unadj_ipcw_L <- il4_t3_unadj_ipcw
#il5_t3_unadj_ipcw_L <- il5_t3_unadj_ipcw
#il6_t3_unadj_ipcw_L <- il6_t3_unadj_ipcw
#tnf_t3_unadj_ipcw_L <- tnf_t3_unadj_ipcw

#d23_igf_unadj_ipcw_L,
#d23_gmc_unadj_ipcw_L,
#d23_ifn_unadj_ipcw_L,
#d23_il10_unadj_ipcw_L,
#d23_il12_unadj_ipcw_L,
#d23_il13_unadj_ipcw_L,
#d23_il17_unadj_ipcw_L,
#d23_il1_unadj_ipcw_L,
#d23_il2_unadj_ipcw_L,
#d23_il21_unadj_ipcw_L,
#d23_il4_unadj_ipcw_L,
#d23_il5_unadj_ipcw_L,
#d23_il6_unadj_ipcw_L,
#d23_tnf_unadj_ipcw_L,

#d23_ratio_gmc_il10_unadj_ipcw_L,
#d23_ratio_ifn_il10_unadj_ipcw_L,
#d23_ratio_il12_il10_unadj_ipcw_L,
#d23_ratio_il13_il10_unadj_ipcw_L,
#d23_ratio_il17_il10_unadj_ipcw_L,
#d23_ratio_il1_il10_unadj_ipcw_L,
#d23_ratio_il21_il10_unadj_ipcw_L,
#d23_ratio_il2_il10_unadj_ipcw_L,
#d23_ratio_il4_il10_unadj_ipcw_L,
#d23_ratio_il5_il10_unadj_ipcw_L,
#d23_ratio_il6_il10_unadj_ipcw_L,
#d23_ratio_tnf_il10_unadj_ipcw_L,

#d23_ratio_il12_il4_unadj_ipcw_L,
#d23_ratio_ifn_il4_unadj_ipcw_L,
#d23_ratio_il12_il5_unadj_ipcw_L,
#d23_ratio_ifn_il5_unadj_ipcw_L,
#d23_ratio_il12_il13_unadj_ipcw_L,
#d23_ratio_ifn_il13_unadj_ipcw_L,

#d23_ratio_il12_il17_unadj_ipcw_L,
#d23_ratio_ifn_il17_unadj_ipcw_L,
#d23_ratio_il12_il21_unadj_ipcw_L,
#d23_ratio_ifn_il21_unadj_ipcw_L,

#d23_ratio_pro_il10_unadj_ipcw_L,
#d23_ratio_th1_il10_unadj_ipcw_L,
#d23_ratio_th2_il10_unadj_ipcw_L,
#d23_ratio_th17_il10_unadj_ipcw_L,
#d23_ratio_th1_th2_unadj_ipcw_L,
#d23_ratio_th1_th17_unadj_ipcw_L,


#igf_t3_unadj_ipcw_L,
#gmc_t3_unadj_ipcw_L,
#ifn_t3_unadj_ipcw_L,
#il10_t3_unadj_ipcw_L,
#il12_t3_unadj_ipcw_L,
#il13_t3_unadj_ipcw_L,
#il17_t3_unadj_ipcw_L,
#il1_t3_unadj_ipcw_L,
#il2_t3_unadj_ipcw_L,
#il21_t3_unadj_ipcw_L,
#il4_t3_unadj_ipcw_L,
#il5_t3_unadj_ipcw_L,
#il6_t3_unadj_ipcw_L,
#tnf_t3_unadj_ipcw_L,

#igf_t2_unadj_ipcw_L,
#crp_t2_unadj_ipcw_L,
#agp_t2_unadj_ipcw_L,
#gmc_t2_unadj_ipcw_L,
#ifn_t2_unadj_ipcw_L,
#il10_t2_unadj_ipcw_L,
#il12_t2_unadj_ipcw_L,
#il13_t2_unadj_ipcw_L,
#il17_t2_unadj_ipcw_L,
#il1_t2_unadj_ipcw_L,
#il2_t2_unadj_ipcw_L,
#il21_t2_unadj_ipcw_L,
#il4_t2_unadj_ipcw_L,
#il5_t2_unadj_ipcw_L,
#il6_t2_unadj_ipcw_L,
#tnf_t2_unadj_ipcw_L,

#ratio_gmc_il10_t2_unadj_ipcw_L,
#ratio_ifn_il10_t2_unadj_ipcw_L,
#ratio_il12_il10_t2_unadj_ipcw_L,
#ratio_il13_il10_t2_unadj_ipcw_L,
#ratio_il17_il10_t2_unadj_ipcw_L,
#ratio_il1_il10_t2_unadj_ipcw_L,
#ratio_il21_il10_t2_unadj_ipcw_L,
#ratio_il2_il10_t2_unadj_ipcw_L,
#ratio_il4_il10_t2_unadj_ipcw_L,
#ratio_il5_il10_t2_unadj_ipcw_L,
#ratio_il6_il10_t2_unadj_ipcw_L,
#ratio_tnf_il10_t2_unadj_ipcw_L,

#ratio_il12_il4_t2_unadj_ipcw_L,
#ratio_ifn_il4_t2_unadj_ipcw_L,
#ratio_il12_il5_t2_unadj_ipcw_L,
#ratio_ifn_il5_t2_unadj_ipcw_L,
#ratio_il12_il13_t2_unadj_ipcw_L,
#ratio_ifn_il13_t2_unadj_ipcw_L,

#ratio_il12_il17_t2_unadj_ipcw_L,
#ratio_ifn_il17_t2_unadj_ipcw_L,
#ratio_il12_il21_t2_unadj_ipcw_L,
#ratio_ifn_il21_t2_unadj_ipcw_L,

#ratio_pro_il10_t2_unadj_ipcw_L,
#ratio_th1_il10_t2_unadj_ipcw_L,
#ratio_th2_il10_t2_unadj_ipcw_L,
#ratio_th17_il10_t2_unadj_ipcw_L,
#ratio_th1_th2_t2_unadj_ipcw_L,
#ratio_th1_th17_t2_unadj_ipcw_L,

