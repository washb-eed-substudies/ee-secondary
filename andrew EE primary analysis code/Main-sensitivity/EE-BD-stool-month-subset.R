
#---------------------------------------
# EE-BD-stool-month-subset.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Bangladesh sub-study - 
# subset to months with treatment arm overlap
#---------------------------------------

## capture all the output to a file.



###Load in data
rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
# levels(treatment$tr)
# treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
# levels(treatment$tr)
#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

#Load in stool survey data
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool<-read.csv("BD-EE-stool.csv")

#Load in lab outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
outcomes<-read.dta("BD-EE-stool-outcomes-Stata12.dta")

#divide the reg value by 1000 to convert it to ug/ml 
outcomes$t2_reg<-outcomes$t2_reg/1000

#divide all the aat values by 1000000 to convert it to mg/g
outcomes$t1_aat<-outcomes$t1_aat/1000000
outcomes$t2_aat<-outcomes$t2_aat/1000000
outcomes$t3_aat<-outcomes$t3_aat/1000000

#Rename outcomes:
outcomes <- outcomes %>%
  rename(aat1=t1_aat,
         aat2=t2_aat,
         aat3=t3_aat,
         mpo1=t1_mpo,
         mpo2=t2_mpo,
         mpo3=t3_mpo,
         neo1=t1_neo,
         neo2=t2_neo,
         neo3=t3_neo,
         reg1b2=t2_reg)


#Merge outcomes
dim(stool)
dim(outcomes)
outcomes$childid<-as.numeric(outcomes$childid)
d<-left_join(stool,outcomes, by="childid")
#d<-cbind(d,outcomes)
dim(d)


#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by="dataid")
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 





#-------------------------
#Covariate cleaning
#-------------------------

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


#Add in time varying covariates:
Wvars1<-c("aged1", "month1", "staffid1") 
Wvars2<-c("aged2", "month2", "staffid2") 
Wvars3<-c("aged3", "month3", "staffid3") 





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

d$birthord[is.na(d$birthord)]<-99
d$birthord<-factor(d$birthord)

d$asset_clock[is.na(d$asset_clock)]<-99
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


#Check that prevalence >5% for all binary variables
for(i in 1:ncol(W)){
  if(class(W[,i])=="factor"){
    for(j in 1:dim(table(W[,i]))){
      flag<-0
      if(sum(W[,i]==levels(W[,i])[j], na.rm=T)/nrow(W)*100<5){
        perc<-sum(W[,i]==levels(W[,i])[j], na.rm=T)/nrow(W)*100
        cat("\n>95% missing: ",colnames(W)[i]," level:",levels(W[,i])[j],"perc:",perc,"\n")
        flag<-1
      }
    }
      if(flag==1){
        print(table(W[,i]))
      }
  }else{
    if(sum(is.na(W[,i]))/nrow(W)*100>95){
      cat("\n>95% missing: ",colnames(W)[i],"\n")
    }
  }
}



#Add in time-varying covariates
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


############################
#Subset to months with 
# treatment arm overlap
############################


head(d)
d <- droplevels(d)
table(d$tr[!is.na(d$neo1)], d$month1[!is.na(d$neo1)])
table(d$tr[!is.na(d$neo2)], d$month2[!is.na(d$neo2)])
table(d$tr[!is.na(d$neo3)], d$month3[!is.na(d$neo3)])


d$neo1[d$month1 > 7 | d$month1 ==5] <- NA
d$mpo1[d$month1 > 7 | d$month1 ==5] <- NA
d$aat1[d$month1 > 7 | d$month1 ==5] <- NA
d$neo2[d$month2==1 | d$month2==7 | d$month2==9] <- NA
d$mpo2[d$month2==1 | d$month2==7 | d$month2==9] <- NA
d$aat2[d$month2==1 | d$month2==7 | d$month2==9] <- NA
d$reg1b2[d$month2==1 | d$month2==7 | d$month2==9] <- NA
d$neo3[d$month3==3 | d$month3==9] <- NA
d$mpo3[d$month3==3 | d$month3==9] <- NA
d$aat3[d$month3==3 | d$month3==9] <- NA


table(d$tr[!is.na(d$neo1)], d$month1[!is.na(d$neo1)])
table(d$tr[!is.na(d$neo2)], d$month2[!is.na(d$neo2)])
table(d$tr[!is.na(d$neo3)], d$month3[!is.na(d$neo3)])


#dataframe of stool biomarkers:
Y <- d %>% select(neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))



############################
#Unadjusted GLMs
############################

#Create empty matrix to hold the glm results:
neo_t1_unadj<-mpo_t1_unadj<-aat_t1_unadj<-matrix(0, nrow=5, ncol=6)
neo_t2_unadj<-mpo_t2_unadj<-aat_t2_unadj<-reg1b_t2_unadj<-matrix(0, nrow=5, ncol=6)
neo_t3_unadj<-mpo_t3_unadj<-aat_t3_unadj<-matrix(0, nrow=5, ncol=6)

res_unadj<-list(neo_t1_unadj=neo_t1_unadj, mpo_t1_unadj=mpo_t1_unadj, aat_t1_unadj=aat_t1_unadj, 
                neo_t2_unadj=neo_t2_unadj, mpo_t2_unadj=mpo_t2_unadj, aat_t2_unadj=aat_t2_unadj, reg1b_t2_unadj=reg1b_t2_unadj, 
                neo_t3_unadj=neo_t3_unadj, mpo_t3_unadj=mpo_t3_unadj, aat_t3_unadj=aat_t3_unadj)




#Unadjusted glm models
for(i in 1:10){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}

############################
#Age and sex adjusted GLMs
############################
d$sex<-as.factor(d$sex)
  d$sex=relevel(d$sex,ref="0")

#Create empty matrix to hold the glm results:
neo_t1_sex<-mpo_t1_sex<-aat_t1_sex<-matrix(0, nrow=5, ncol=6)
neo_t2_sex<-mpo_t2_sex<-aat_t2_sex<-reg1b_t2_sex<-matrix(0, nrow=5, ncol=6)
neo_t3_sex<-mpo_t3_sex<-aat_t3_sex<-matrix(0, nrow=5, ncol=6)

res_sex<-list(neo_t1_sex=neo_t1_sex, mpo_t1_sex=mpo_t1_sex, aat_t1_sex=aat_t1_sex, 
                neo_t2_sex=neo_t2_sex, mpo_t2_sex=mpo_t2_sex, aat_t2_sex=aat_t2_sex, reg1b_t2_sex=reg1b_t2_sex, 
                neo_t3_sex=neo_t3_sex, mpo_t3_sex=mpo_t3_sex, aat_t3_sex=aat_t3_sex)




#Age and sex adjusted glm models
for(i in 1:3){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 4:7){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 8:10){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
            


############################
#Adjusted GLMs
############################



#Create empty matrix to hold the tmle results:
res_adj<-list(neo_t1_adj=matrix(0,5,6), mpo_t1_adj=matrix(0,5,6), aat_t1_adj=matrix(0,5,6), 
                neo_t2_adj=matrix(0,5,6), mpo_t2_adj=matrix(0,5,6), aat_t2_adj=matrix(0,5,6),  reg1b_t2_adj=matrix(0,5,6),
                neo_t3_adj=matrix(0,5,6), mpo_t3_adj=matrix(0,5,6), aat_t3_adj=matrix(0,5,6))

for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W1, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 4:6){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,7]), tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[7]][j,]<-as.numeric(temp$TR)
}
for(i in 8:10){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}




##########################################
#Save objects for replication
##########################################
neo_t1_unadj_sub=res_unadj[[1]]
mpo_t1_unadj_sub=res_unadj[[2]]
aat_t1_unadj_sub=res_unadj[[3]] 
neo_t2_unadj_sub=res_unadj[[4]] 
mpo_t2_unadj_sub=res_unadj[[5]]
aat_t2_unadj_sub=res_unadj[[6]] 
reg1b_t2_unadj_sub=res_unadj[[7]] 
neo_t3_unadj_sub=res_unadj[[8]]
mpo_t3_unadj_sub=res_unadj[[9]]
aat_t3_unadj_sub=res_unadj[[10]]


neo_t1_adj_sex_age_sub=res_sex[[1]]
mpo_t1_adj_sex_age_sub=res_sex[[2]]
aat_t1_adj_sex_age_sub=res_sex[[3]] 
neo_t2_adj_sex_age_sub=res_sex[[4]] 
mpo_t2_adj_sex_age_sub=res_sex[[5]]
aat_t2_adj_sex_age_sub=res_sex[[6]] 
reg1b_t2_adj_sex_age_sub=res_sex[[7]] 
neo_t3_adj_sex_age_sub=res_sex[[8]]
mpo_t3_adj_sex_age_sub=res_sex[[9]]
aat_t3_adj_sex_age_sub=res_sex[[10]]



neo_t1_adj_sub=res_adj[[1]]
mpo_t1_adj_sub=res_adj[[2]]
aat_t1_adj_sub=res_adj[[3]] 
neo_t2_adj_sub=res_adj[[4]] 
mpo_t2_adj_sub=res_adj[[5]]
aat_t2_adj_sub=res_adj[[6]]
reg1b_t2_adj_sub=res_adj[[7]] 
neo_t3_adj_sub=res_adj[[8]]
mpo_t3_adj_sub=res_adj[[9]]
aat_t3_adj_sub=res_adj[[10]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")


save(neo_t1_unadj_sub, mpo_t1_unadj_sub, aat_t1_unadj_sub,
     neo_t2_unadj_sub, mpo_t2_unadj_sub, aat_t2_unadj_sub, reg1b_t2_unadj_sub,
     neo_t3_unadj_sub, mpo_t3_unadj_sub, aat_t3_unadj_sub, 
     file="stool_res_subset_unadj_sub.Rdata")

save(neo_t1_adj_sex_age_sub, mpo_t1_adj_sex_age_sub, aat_t1_adj_sex_age_sub,
     neo_t2_adj_sex_age_sub, mpo_t2_adj_sex_age_sub, aat_t2_adj_sex_age_sub, reg1b_t2_adj_sex_age_sub,
     neo_t3_adj_sex_age_sub, mpo_t3_adj_sex_age_sub, aat_t3_adj_sex_age_sub, 
     file="stool_res_subset_adj_sex_age_sub.Rdata")

save(neo_t1_adj_sub, mpo_t1_adj_sub, aat_t1_adj_sub,
     neo_t2_adj_sub, mpo_t2_adj_sub, aat_t2_adj_sub, reg1b_t2_adj_sub,
     neo_t3_adj_sub, mpo_t3_adj_sub, aat_t3_adj_sub, 
     file="stool_res_subset_adj_sub.Rdata")


