
#---------------------------------------
# EE-BD-telo-ipcw.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# Telomere substudy -IPCW analysis for missing 
# outcomes
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



 


#Load in telomere outcome data
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv", stringsAsFactors = T)
ipcw<-read.csv("BD-EE-ipcw.csv", stringsAsFactors = T)


#Merge in telomere outcomes
telo_outcomes<-subset(telo, select=c(dataid,childNo, staffid2, staffid3, month2, month3, aged2,aged3,TS2,TS3))
dim(telo_outcomes)
d<-merge(ipcw, telo_outcomes, by=c("dataid", "childNo"), all.x=T, all.y=T)

#Subset to Control and WSH+N
d<-subset(d, tr=="Control" | tr=="Nutrition + WSH")

#Drop if miss1reason is "No live birth"
#d<-subset(d, miss1reason!="No live birth")


#Calculate change in TS between T=2 and T=3
d$TS_delta<-d$TS3-d$TS2


#Check agreement with Audrie's IDs
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Audrie")
load("telodfull.Rdata")


table(is.na(d$childid))
table(is.na(d$dataid))
table(is.na(d$childNo))

#table(is.na(telodfull$childid))
table(is.na(telodfull$dataid))
table(is.na(telodfull$childno))

telodfull$childNo<-telodfull$childno


table(d$dataid %in% telodfull$dataid)
d$dataid[which(!(d$dataid %in% telodfull$dataid))]


table(telodfull$dataid %in% d$dataid)
telodfull$dataid[which(!(telodfull$dataid %in% d$dataid))]

table(telodfull$childid.x %in% d$childid)
telodfull$childid.x[which(!(telodfull$childid.x %in% d$childid))]


table(is.na(telodfull$childid.x))
table(is.na(telodfull$childid.y))
test<-telodfull %>% distinct(dataid, childno)
table(is.na(d$childid))
test2<-d %>% distinct(dataid, childNo)


test<-anti_join(d, telodfull, by=c("dataid", "childNo"))

test2<-anti_join(telodfull, d, by=c("dataid", "childNo"))

cbind(test$dataid, test$childNo, test$TS2, test$TS3)
cbind(test2$dataid, test2$childNo, test2$TS2, test2$TS3)

which(!(test$dataid %in% test2$dataid))
test$dataid[7]
#Clean covariates for 
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

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)





#Create indicator for missingness
d$TS2.miss<-ifelse(is.na(d$TS2),0,1)
d$TS3.miss<-ifelse(is.na(d$TS3),0,1)
d$TS_delta.miss<-ifelse(is.na(d$TS_delta),0,1)

table(d$TS2.miss)
table(d$TS3.miss)

#d$TS2.miss[d$TS2.miss==0]<-9
#d$TS3.miss[d$TS3.miss==0]<-9

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$TS2Delta <- d$TS2
d$TS2Delta[d$TS2.miss==0] <- 9

d$TS3Delta <- d$TS3
d$TS3Delta[d$TS3.miss==0] <- 9


d$TS_deltaDelta <- d$TS_delta  #Bad naming convention!
d$TS_deltaDelta[d$TS_delta.miss==0] <- 9

mean(d$TS2Delta, na.rm=T)
mean(d$TS3Delta, na.rm=T)
mean(d$TS_deltaDelta, na.rm=T)


#Order for replication:
d<-d[order(d$block,d$clusterid,d$dataid),]
  
#Run the ipcw analysis
ts_t2_unadj_ipcw_M<-washb_tmle(Y=d$TS2Delta, tr=d$tr, W=NULL, id=d$block, Delta=d$TS2.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
ts_t3_unadj_ipcw_M<-washb_tmle(Y=d$TS3Delta, tr=d$tr, W=NULL, id=d$block, Delta=d$TS3.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
delta_ts_unadj_ipcw_M<-washb_tmle(Y=d$TS_deltaDelta, tr=d$tr, W=NULL, id=d$block, Delta=d$TS_delta.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)

#Extract estimates
ts_t2_unadj_ipcw_M<-as.data.frame(unlist(ts_t2_unadj_ipcw_M$estimates$ATE))
ts_t3_unadj_ipcw_M<-as.data.frame(unlist(ts_t3_unadj_ipcw_M$estimates$ATE))
delta_ts_unadj_ipcw_M<-as.data.frame(unlist(delta_ts_unadj_ipcw_M$estimates$ATE))
ts_t2_unadj_ipcw_M
ts_t3_unadj_ipcw_M
delta_ts_unadj_ipcw_M

#Run the ipcw analysis
ts_t2_adj_ipcw_M<-washb_tmle(Y=d$TS2Delta, tr=d$tr, W=W, id=d$block, Delta=d$TS2.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
ts_t3_adj_ipcw_M<-washb_tmle(Y=d$TS3Delta, tr=d$tr, W=W, id=d$block, Delta=d$TS3.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
delta_ts_adj_ipcw_M<-washb_tmle(Y=d$TS_deltaDelta, tr=d$tr, W=W, id=d$block, Delta=d$TS_delta.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)

#Extract estimates
ts_t2_adj_ipcw_M<-as.data.frame(unlist(ts_t2_adj_ipcw_M$estimates$ATE))
ts_t3_adj_ipcw_M<-as.data.frame(unlist(ts_t3_adj_ipcw_M$estimates$ATE))
delta_ts_adj_ipcw_M<-as.data.frame(unlist(delta_ts_adj_ipcw_M$estimates$ATE))
ts_t2_adj_ipcw_M
ts_t3_adj_ipcw_M
delta_ts_adj_ipcw_M



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(ts_t2_unadj_ipcw_M, ts_t3_unadj_ipcw_M, delta_ts_unadj_ipcw_M,
     ts_t2_adj_ipcw_M, ts_t3_adj_ipcw_M, delta_ts_adj_ipcw_M, file="telo_ipcw_res.Rdata")




