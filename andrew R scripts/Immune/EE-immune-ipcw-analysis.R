

#---------------------------------------
# EE-immune-ipcw-analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# immune substudy -IPCW analysis for missing 
# outcomes 
#---------------------------------------

###Load in data
rm(list=ls())
source(here::here("0-config.R"))

#load full treatments
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.Rdata")
blind_tr$clusterid<-as.numeric(blind_tr$clusterid)
treatment<-blind_tr

#Load EED 
ipcw <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-ipcw.csv", stringsAsFactors = T) %>% select(-c(tr,block))
ipcw <- left_join(ipcw, treatment, by=c("clusterid"))

#Load in immune analysis dataset
load(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-immune.Rdata"))


#Keep only immune outcomes and time-varying covariates, drop the baseline covariates
colnames(d)
d <- d %>% 
  subset(., select=c(
    childid, dataid, childNo, tr, agemth_bt2, agemth_bt3, ageday_bt2, ageday_bt3, month2, month3,
    igf_t2,          igf_t3,          crp_t2,         
    agp_t2,          gmcsf_t2,        ifng_t2,         il10_t2,         il12_t2,        
    il13_t2,         il17_t2,         il1_t2,          il2_t2,          il21_t2,        
    il4_t2,          il5_t2,          il6_t2,          tnfa_t2,         gmcsf_t3,       
    ifng_t3,         il10_t3,         il12_t3,         il13_t3,         il17_t3,        
    il1_t3,          il2_t3,          il21_t3,         il4_t3,          il5_t3,         
    il6_t3,          tnfa_t3
  ))


#Add in time varying variables above, and impute median where missing




d <- left_join(ipcw, d, by = c("dataid","tr"))

#Subset to immune analysis arms
d <- subset(d, tr=="Control" | tr=="Nutrition + WSH")
d$tr <- factor(d$tr)

#Impute time varying covariates


#calculate overall median:
month2_median <-    median(d$month2, na.rm = T)
month3_median <-    median(d$month3, na.rm = T)

#use clusterid to impute median month where possible
d$month2[is.na(d$month2)] <-  ave(d$month2, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month2)] 
d$month2 <- ceiling(d$month2)

d$month3[is.na(d$month3)] <-  ave(d$month3, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month3)] 
d$month3 <- ceiling(d$month3)


#impute month with overall median for those observations not in a cluster measured in the EED subsample
d$month2[is.na(d$month2)] <-  8
d$month3[is.na(d$month3)] <-  6


d <- d %>% mutate(
                  monsoon2 = ifelse(month2 > 4 & month2 < 11, "1", "0"),
                  monsoon3 = ifelse(month3 > 4 & month3 < 11, "1", "0"),
                  monsoon2 = ifelse(is.na(month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(month3),"missing", monsoon3),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))


#impute child age with overall median
median(d$ageday_bt2, na.rm=T)
median(d$ageday_bt3, na.rm=T)
d$ageday_bt2[is.na(d$ageday_bt2)] <- 441
d$ageday_bt3[is.na(d$ageday_bt3)] <- 862


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
d$birthord<-factor(d$birthord)
table(d$birthord)
table(W$birthord)

d %>% group_by(birthord) %>% summarise(mean=mean(log(aat3), na.rm=T))

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


#Add in time-varying covariates
Wvars2<-c("aged2", "monsoon2") 
Wvars3<-c("aged3", "monsoon3") 
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W2$monsoon2[is.na(W2$monsoon2)]<-"missing"
W3$monsoon3[is.na(W3$monsoon3)]<-"missing"


#Create indicators for missingness
outcomes <- c("igf_t2",          "igf_t3",          "crp_t2",         
              "agp_t2",          "gmcsf_t2",        "ifng_t2",         "il10_t2",         "il12_t2",        
              "il13_t2",         "il17_t2",         "il1_t2",          "il2_t2",          "il21_t2",        
              "il4_t2",          "il5_t2",          "il6_t2",          "tnfa_t2",         "gmcsf_t3",       
              "ifng_t3",         "il10_t3",         "il12_t3",         "il13_t3",         "il17_t3",        
              "il1_t3",          "il2_t3",          "il21_t3",         "il4_t3",          "il5_t3",         
              "il6_t3",          "tnfa_t3")

missingness_list <- list()
for(i in 1:length(outcomes)){
  missingness_list[[i]] <- ifelse(is.na(d[,outcomes[i]]),0,1)
  d[is.na(d[,outcomes[i]]),outcomes[i]] <- 9
  names(missingness_list)[i] <- paste0(outcomes[i],".miss")
}
miss <- bind_rows(missingness_list)
d <- cbind(d, miss)


#Order for replication:
d<-d[order(d$block,d$clusterid,d$dataid),]
  
#Run the adjusted ipcw analysis

#Fully adjusted glm models
res_adj <- NULL
for(i in 1:ncol(Y)){
  if(grepl("t2_", colnames(Y)[i])){
    temp<-washb_glm(Y=(Y[,i]), tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)
  }else{
    temp<-washb_glm(Y=(Y[,i]), tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)
  }
  res_adj<-rbind(res_adj, as.numeric(temp$TR))
}
res_adj <- as.data.frame(res_adj)

colnames(res_adj)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
res_adj$Y <-colnames(Y)

















#dataframe of stool biomarkers:
Y<-d %>% select(neo1Delta,mpo1Delta,aat1Delta,neo2Delta,mpo2Delta,aat2Delta,reg1b2Delta,neo3Delta,mpo3Delta,aat3Delta)

#dataframe of stool missingness:
miss<-d %>% select(neo1.miss,mpo1.miss,aat1.miss,neo2.miss,mpo2.miss,aat2.miss,reg1b2.miss,neo3.miss,mpo3.miss,aat3.miss)


#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))




#Create empty matrix to hold the glm results:


res_adj<-list(neo_t1_adj=matrix(0,5,5), mpo_t1_adj=matrix(0,5,5), aat_t1_adj=matrix(0,5,5), 
                neo_t2_adj=matrix(0,5,5), mpo_t2_adj=matrix(0,5,5), aat_t2_adj=matrix(0,5,5),  reg1b_t2_adj=matrix(0,5,5),
                neo_t3_adj=matrix(0,5,5), mpo_t3_adj=matrix(0,5,5), aat_t3_adj=matrix(0,5,5))

Wlist <- list(W1,W1,W1,W2,W2,W2,W2,W3,W3,W3)


 i<-6
 j <- 3
mean(log(Y[d$tr=="Control",i]), na.rm=T)
sum(miss[d$tr=="Control",i], na.rm=T)
mean(log(Y[d$tr=="WSH",i]), na.rm=T)
sum(miss[d$tr=="WSH",i], na.rm=T)
temp<-washb_tmle(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)


d %>% group_by(tr) %>% summarize(aat2=mean(log(aat2Delta), na.rm=T))


temp<-washb_tmle(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr,
                 W=select(Wlist[[i]], -contains("month")), 
                 id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)



# d <- d[(d$tr=="Control" | d$tr=="Nutrition + WSH") & !is.na(d$tr),]
# summary(d$aged1)


for(i in 1:10){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_tmle(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
    cat(i," : ",j, "\n")
    res_adj[[i]][j,]<-(t(unlist(temp$estimates$ATE)))
    colnames(res_adj[[i]])<-c("psi","var.psi","ci.l","ci.u", "Pval")
    rownames(res_adj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}



#Extract estimates
neo_t1_adj_ipcw_M<-res_adj[[1]]
neo_t2_adj_ipcw_M<-res_adj[[4]]
neo_t3_adj_ipcw_M<-res_adj[[8]]

mpo_t1_adj_ipcw_M<-res_adj[[2]]
mpo_t2_adj_ipcw_M<-res_adj[[5]]
mpo_t3_adj_ipcw_M<-res_adj[[9]]

aat_t1_adj_ipcw_M<-res_adj[[3]]
aat_t2_adj_ipcw_M<-res_adj[[6]]
aat_t3_adj_ipcw_M<-res_adj[[10]]

reg_t2_adj_ipcw_M<-res_adj[[7]]




setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(
aat_t1_adj_ipcw_M,
aat_t2_adj_ipcw_M,
aat_t3_adj_ipcw_M,
mpo_t1_adj_ipcw_M,
mpo_t2_adj_ipcw_M,
mpo_t3_adj_ipcw_M,
neo_t1_adj_ipcw_M,
neo_t2_adj_ipcw_M,

neo_t3_adj_ipcw_M,
reg_t2_adj_ipcw_M, 
file="stool_ipcw_res.Rdata")




