
#---------------------------------------
# EE-BD-telo.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# Telomere substudy
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
table(treatment$tr)


#Load in telomere dataset
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")

#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 

 

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 



#table number of fully collected aliqouts by arm and year
head(d)

t2_N<-d%>% subset(aliquot2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t3_N<-d%>% subset(aliquot3>1)%>%group_by(tr) %>%summarize(sample3=n()) 
cbind(t2_N,t3_N[,2])



#Calculate average age across arms at followup time 1, 2, and 3
#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(TS2)) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(TS2)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na( TS3)) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(TS3)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


rbind(overallN1, t1)
age_t2_blood_M<-rbind(overallN2, t2)
age_t3_blood_M<-rbind(overallN3, t3)

#Reorder columns to match Audrie
age_t2_blood_M<-age_t2_blood_M[,c(1,2,4,3,5,7,6)]
age_t3_blood_M<-age_t3_blood_M[,c(1,2,4,3,5,7,6)]



############################
#Calculate unadjusted outcomes:
############################

#Calculate change in TS between T=2 and T=3
d$TS_delta<-d$TS3-d$TS2


#N's and geometric means
    ts_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(TS2)) %>% summarize(N=n(), mean= mean(TS2, na.rm=T))   
    ts_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(TS3)) %>% summarize(N=n(), mean= mean(TS3, na.rm=T))   
    delta_ts_N_M<-d %>% group_by(tr) %>% subset(!is.na(TS_delta)) %>% summarize(N=n(), mean= mean(TS_delta, na.rm=T))   

#Unadjusted glm models
    ts_t2_unadj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    ts_t3_unadj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    delta_ts_unadj_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR

#Mean basepairs
    d %>% group_by(tr) %>% subset(!is.na(TS2)) %>% summarize(N=n(), mean= mean(3274 + 2413*TS2, na.rm=T))   
    d %>% group_by(tr) %>% subset(!is.na(TS3)) %>% summarize(N=n(), mean= mean(3274 + 2413*TS3, na.rm=T))   
    d %>% group_by(tr) %>% subset(!is.na(TS_delta)) %>% summarize(N=n(), mean= mean(3274 + 2413*TS_delta, na.rm=T))   

    d %>% group_by(tr) %>% subset(!is.na(TS2)&!is.na(TS3)) %>% summarize(N=n(), mean= mean((3274 + 2413*TS3)-(3274 + 2413*TS2), na.rm=T))   

    mean(3274 + 2413*d$TS2[d$tr=="Nutrition + WSH"], na.rm=T)-mean(3274 + 2413*d$TS2[d$tr=="Control"], na.rm=T)
    mean(3274 + 2413*d$TS3[d$tr=="Nutrition + WSH"], na.rm=T)-mean(3274 + 2413*d$TS3[d$tr=="Control"], na.rm=T)
    mean(3274 + 2413*d$TS_delta[d$tr=="Nutrition + WSH"], na.rm=T)-mean(3274 + 2413*d$TS_delta[d$tr=="Control"], na.rm=T)

    

############################
#Adjusted GLMs-sex and age
############################
d$sex<-as.factor(d$sex)

#Run GLMs for the sex/age adjusted parameter estimates
    ts_t2_adj_sex_age_M<-washb_glm(Y=d$TS2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    ts_t3_adj_sex_age_M<-washb_glm(Y=d$TS3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    delta_ts_adj_sex_age_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=cbind(d$sex, d$aged2, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR


    
############################
#Adjusted GLMs-full
############################
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

d$birthord[is.na(d$birthord)]<-"99"
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
d$sex<-as.factor(d$sex)
  levels(d$sex)<-c("female","male")
  d$sex=relevel(d$sex,ref="female")
d$momedu=relevel(d$momedu,ref="No education")
d$hfiacat=relevel(d$hfiacat,ref="Food Secure")
    d$hfiacat<-addNA(d$hfiacat)
    levels(d$hfiacat)[length(levels(d$hfiacat))]<-"missing"
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
d$momheight
    
#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)


#Add in time-varying covariates
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))
W_delta<- cbind(W, subset(d, select=Wvars2), subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W2$month2[is.na(W2$month2)]<-"missing"
W3$month3[is.na(W3$month3)]<-"missing"
W2$staffid2[is.na(W2$staffid2)]<-"missing"
W3$staffid3[is.na(W3$staffid3)]<-"missing"



#Set time-varying covariates as factors
W2$month2<-as.factor(W2$month2)
W3$month3<-as.factor(W3$month3)
W2$staffid2<-factor(W2$staffid2)
W3$staffid3<-factor(W3$staffid3)

W_delta$month2<-as.factor(W_delta$month2)
W_delta$month3<-as.factor(W_delta$month3)
W_delta$staffid2<-factor(W_delta$staffid2)
W_delta$staffid3<-factor(W_delta$staffid3)
W_delta$month2<-addNA(W_delta$month2)
W_delta$month3<-addNA(W_delta$month3)
W_delta$staffid2<-addNA(W_delta$staffid2)
W_delta$staffid3<-addNA(W_delta$staffid3)



#Check missingness:

cbind(d$TS2,W2) %>% subset(!is.na(d$TS2)) %>% apply(., 2, function(x) print(table(is.na(x))[2]))



#Run GLMs for the adjusted parameter estimates
    ts_t2_adj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    ts_t3_adj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    delta_ts_adj_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=W_delta, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR


#Run GLMs for the fully adjusted parameter estimates (no prescreening)
    ts_t2_adj_noscreen_M<-washb_glm(Y=d$TS2, tr=d$tr, W=W2, forcedW=colnames(W2), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    ts_t3_adj_noscreen_M<-washb_glm(Y=d$TS3, tr=d$tr, W=W3, forcedW=colnames(W3), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    delta_ts_adj_noscreen_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=W_delta,  forcedW=colnames(W_delta), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR




    

for(i in 1:ncol(W2)){
  print(colnames(W2)[i])
  print(table(is.na(W2[,i])))
}


    

############################
#Subgroup analysis
############################
    
    #N's and geometric means
    ts_t2_N_subgroup_M<-d %>% group_by(sex, tr) %>% subset(!is.na(TS2)) %>% summarize(N=n(), mean= mean(TS2, na.rm=T))   
    ts_t3_N_subgroup_M<-d %>% group_by(sex, tr) %>% subset(!is.na(TS3)) %>% summarize(N=n(), mean= mean(TS3, na.rm=T))   
    delta_ts_N_subgroup_M<-d %>% group_by(sex, tr) %>% subset(!is.na(TS_delta)) %>% summarize(N=n(), mean= mean(TS_delta, na.rm=T))   
  
    
    
    ts_t2_subgroup_M<-washb_glm(Y=d$TS2, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$lincom
    ts_t3_subgroup_M<-washb_glm(Y=d$TS3, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$lincom
    delta_ts_subgroup_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$lincom

    ts_t2_subgroup_fit<-washb_glm(Y=d$TS2, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$fit
    ts_t3_subgroup_fit<-washb_glm(Y=d$TS3, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$fit
    delta_ts_subgroup_fit<-washb_glm(Y=d$TS_delta, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$fit

    ts_t2_subgroup_fit
    ts_t3_subgroup_fit
    delta_ts_subgroup_fit

    #Strip out "missing" row
    ts_t2_subgroup_M<-ts_t2_subgroup_M[1:2,]
    ts_t3_subgroup_M<-ts_t3_subgroup_M[1:2,]
    delta_ts_subgroup_M<-delta_ts_subgroup_M[1:2,]

    
##########################################
#Save objects for replication
##########################################


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(age_t2_blood_M, age_t3_blood_M, ts_t2_N_M, ts_t3_N_M,ts_t2_unadj_M, ts_t3_unadj_M, ts_t2_adj_sex_age_M, ts_t3_adj_sex_age_M, ts_t2_adj_M, ts_t3_adj_M,
     ts_t2_subgroup_M, ts_t3_subgroup_M, delta_ts_N_M, delta_ts_unadj_M, delta_ts_adj_M, delta_ts_adj_sex_age_M, delta_ts_subgroup_M,
     ts_t2_N_subgroup_M, ts_t3_N_subgroup_M, delta_ts_N_subgroup_M, ts_t2_subgroup_fit,
    ts_t3_subgroup_fit,
    delta_ts_subgroup_fit,
     file="telo_res.Rdata")



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(d, file="telo_figure_data.Rdata")






#Numerical check for elife publication
    library(stargazer)
    stargazer(round(ts_t2_adj_M[c(1:3,6)], 3), type = "text", title="TS2 prescreen", summary=F)
    stargazer(round(ts_t2_adj_noscreen_M[c(1:3,6)], 3), type = "text", title="TS2 no prescreen", summary=F)
    stargazer(round(ts_t3_adj_M[c(1:3,6)], 3), type = "text", title="TS3 prescreen", summary=F)
    stargazer(round(ts_t3_adj_noscreen_M[c(1:3,6)], 3), type = "text", title="TS3 no prescreen", summary=F)
    stargazer(round(delta_ts_adj_M[c(1:3,6)], 3), type = "text", title="TS delta prescreen", summary=F)
    stargazer(round(delta_ts_adj_noscreen_M[c(1:3,6)], 3), type = "text", title="TS delta no prescreen", summary=F)

    
    
    # In the substudy, a quarter of the fathers were engaged in agriculture. 61% of 
    # households reported having electricity available, and only 14% had a cement floor.
    # At enrollment, 72%, 56%, and 9% of households were food secure, owned a latrine, 
    # and had a handwashing station with soap near the latrine respectively. The primary
    # water source for the majority of households (72%) was a shallow tubewell. Respondents
    # reported the occurrence of daily open defecation in 80% of children less than 3 years 
    # of age. The substudy household enrollment characteristics were similar to the overall 
    # trial (Table 2)." 
    
    mean(d$dadagri, na.rm=T) *100
    table(d$elec)[2]/sum( table(d$elec))*100
    table(d$floor) [2]/sum( table(d$floor))*100
    table(d$hfiacat)[1]/sum( table(d$hfiacat))*100 
    mean(d$latown, na.rm=T) *100
    mean(d$hwlatsoap, na.rm=T) *100
    mean(d$tubewell, na.rm=T)
    mean(d$odchu3, na.rm=T) *100





