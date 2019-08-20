
#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------



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



#table number of fully collected aliqouts by arm and year (May not match with the actually available
#outcomes due to mismarked aliquots)
head(d)
table(d$tr)

#aliqout time 1
t1s1<-d%>% subset(aliqout1_t1>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t1s2<-d%>% subset(aliqout2_t1>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t1s3<-d%>% subset(aliqout3_t1>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t1s4<-d%>% subset(aliqout4_t1>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t1s5<-d%>% subset(aliqout5_t1>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2])


#aliqout time 2
t2s1<-d%>% subset(aliqout1_t2>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t2s2<-d%>% subset(aliqout2_t2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t2s3<-d%>% subset(aliqout3_t2>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t2s4<-d%>% subset(aliqout4_t2>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t2s5<-d%>% subset(aliqout5_t2>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2])
aliquotN_t2

#aliqout time 3
t3s1<-d%>% subset(aliqout1_t3>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t3s2<-d%>% subset(aliqout2_t3>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t3s3<-d%>% subset(aliqout3_t3>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t3s4<-d%>% subset(aliqout4_t3>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t3s5<-d%>% subset(aliqout5_t3>1)%>%group_by(tr) %>%summarize(sample5=n()) 

aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2])


aliquotN_t1
aliquotN_t2
aliquotN_t3



#Temporarily generate fake outcome data for aat (0.5, sd=0.12), mpo (10000, sd=2500), and neo (2000, sd=250)
#set.seed(12345)
#d$aat1<-round(rnorm(n=nrow(d), mean=13, sd=1),4)
#d$mpo1<-round(rnorm(n=nrow(d), mean=11000, sd=0.12),4)
#d$neo1<-round(rnorm(n=nrow(d), mean=2000, sd=0.12),4)

#d$aat2<-round(rnorm(n=nrow(d), mean=12, sd=1.5),4)
#d$mpo2<-round(rnorm(n=nrow(d), mean=9000, sd=0.12),4)
#d$neo2<-round(rnorm(n=nrow(d), mean=2100, sd=0.12),4)
#d$reg1b2<-round(rnorm(n=nrow(d), mean=30, sd=5),4)

#d$aat3<-round(rnorm(n=nrow(d), mean=13.5, sd=1.3),4)
#d$mpo3<-round(rnorm(n=nrow(d), mean=10000, sd=0.12),4)
#d$neo3<-round(rnorm(n=nrow(d), mean=1900, sd=0.12),4)


#Create and save dataset for Audrie:
#stool_simulated_outcomes<-d %>%
#    mutate(childid=as.character(dataid*10+childNo)) %>%
#    select(childid, aat1, mpo1, neo1, aat2, mpo2, neo2, reg1b2, aat3, mpo3, neo3)
#library(stringr)
#stool_simulated_outcomes$childid<-str_pad(stool_simulated_outcomes$childid, 6, pad = "0")
#head(stool_simulated_outcomes)    

#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
#save(stool_simulated_outcomes, file="washb-BD-EE-sim-stool-outcomes.Rdata")
#write.dta(stool_simulated_outcomes, "washb-BD-EE-sim-stool-outcomes.dta")

# setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
# outcomes<-read.dta("washb-BD-EE-sim-stool-outcomes-stata12.dta")
# outcomes$childid<-as.numeric(outcomes$childid)
# 
# dim(d)
# dim(outcomes)
# d<-left_join(d,outcomes, by="childid")
# dim(d)

#Calculate average age across arms at followup time 1, 2, and 3
#d$tr <- factor(d$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))

#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% summarize(N=n(), Mean_agem=mean(agem1, na.rm=T) ,Median_agem=median(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem1, na.rm=T), Median_agem=median(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
#subset((!is.na(aat1)|!is.na(mpo1)|!is.na(neo1))&!is.na(agem1)) %>%

#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% summarize(N=n(), Mean_agem=mean(agem2, na.rm=T), Median_agem=median(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>%  group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem2, na.rm=T), Median_agem=median(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% summarize(N=n(), Mean_agem=mean(agem3, na.rm=T), Median_agem=median(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), numfemales=n()-sum(sex, na.rm=T), nummales=sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem3, na.rm=T), Median_agem=median(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), numfemales=n()-sum(sex, na.rm=T), nummales=sum(sex, na.rm=T)) 


age_t1_stool_M<-rbind(overallN1, t1)
age_t2_stool_M<-rbind(overallN2, t2)
age_t3_stool_M<-rbind(overallN3, t3)



############################
#Calculate outcomes:
############################

#dataframe of stool biomarkers:
Y<-d %>% select(neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#Create empty  matrices to hold the Ns and geometric means:
neo_t1_N_M<-mpo_t1_N_M<-aat_t1_N_M<-neo_t2_N_M<-mpo_t2_N_M<-aat_t2_N_M<-reg1b_t2_N_M<-neo_t3_N_M<-mpo_t3_N_M<-aat_t3_N<-matrix(0,4,2)


mean(d$aat1, na.rm=T)

  #N's and geometric means
aat_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat1)) %>% summarize(N=n(), mean= mean(log(aat1), na.rm=T))   
mpo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo1)) %>% summarize(N=n(), mean= mean(log(mpo1), na.rm=T))   
neo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo1)) %>% summarize(N=n(), mean= mean(log(neo1), na.rm=T))   
aat_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat2)) %>% summarize(N=n(), mean= mean(log(aat2), na.rm=T))   
mpo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo2)) %>% summarize(N=n(), mean= mean(log(mpo2), na.rm=T))   
neo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo2)) %>% summarize(N=n(), mean= mean(log(neo2), na.rm=T))
reg1b_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(reg1b2)) %>% summarize(N=n(), mean= mean(log(reg1b2), na.rm=T))   
aat_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat3)) %>% summarize(N=n(), mean= mean(log(aat3), na.rm=T))   
mpo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo3)) %>% summarize(N=n(), mean= mean(log(mpo3), na.rm=T))   
neo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo3)) %>% summarize(N=n(), mean= mean(log(neo3), na.rm=T))   


#Means and 95% CI's for mean by arm plots
aat_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
reg1b2_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$reg1b2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


aat_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
reg1b2_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$reg1b2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


# #Means and 95% CI's not stratified by arm
overall_mn_by_round<-
  d %>% subset(., select=c(dataid, childNo, block.x, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)) %>%
  gather(key, value, -dataid, -childNo, -block.x) %>%
  mutate(biomarker = substr(key, 1,3)) %>%
  group_by(biomarker) %>%
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block.x, print = F))) %>%
  ungroup %>% as.data.frame

overall_mn<-
  d %>% subset(., select=c(dataid, childNo, block.x, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)) %>%
  gather(key, value, -dataid, -childNo, -block.x) %>%
  group_by(key) %>%
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block.x, print = F))) %>%
  ungroup %>% as.data.frame
colnames(overall_mn)[1]<-"biomarker"
stool_overall_mn <- rbind(overall_mn_by_round, overall_mn)
save(stool_overall_mn, file="stool_overall_means.Rdata")



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
d <- d %>% mutate(monsoon1 = ifelse(month1 > 4 & month1 < 11, "1", "0"),
                  monsoon2 = ifelse(month2 > 4 & month2 < 11, "1", "0"),
                  monsoon3 = ifelse(month3 > 4 & month3 < 11, "1", "0"),
                  monsoon1 = ifelse(is.na(month1),"missing", monsoon1),
                  monsoon2 = ifelse(is.na(month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(month3),"missing", monsoon3),
                  monsoon1 = factor(monsoon1),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))


Wvars1<-c("aged1", "monsoon1") 
Wvars2<-c("aged2", "monsoon2") 
Wvars3<-c("aged3", "monsoon3") 

W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))




#Tabulate missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}


#Print means for continious, Ns for factors
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  if(class(W[,i])=="factor"){
    print(table(W[,i]))
  }else{print(mean(W[,i], na.rm=T))}
}



for(i in 1:ncol(W3)){
  print(colnames(W3)[i])
  if(class(W3[,i])=="factor"){
    print(table(W3[,i]))
  }else{print(mean(W3[,i], na.rm=T))}
}








##############################################
#Run GLMs for the adjusted parameter estimates
##############################################

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
neo_t1_unadj_M=res_unadj[[1]]
mpo_t1_unadj_M=res_unadj[[2]]
aat_t1_unadj_M=res_unadj[[3]] 
neo_t2_unadj_M=res_unadj[[4]] 
mpo_t2_unadj_M=res_unadj[[5]]
aat_t2_unadj_M=res_unadj[[6]] 
reg1b_t2_unadj_M=res_unadj[[7]] 
neo_t3_unadj_M=res_unadj[[8]]
mpo_t3_unadj_M=res_unadj[[9]]
aat_t3_unadj_M=res_unadj[[10]]


neo_t1_adj_sex_age_M=res_sex[[1]]
mpo_t1_adj_sex_age_M=res_sex[[2]]
aat_t1_adj_sex_age_M=res_sex[[3]] 
neo_t2_adj_sex_age_M=res_sex[[4]] 
mpo_t2_adj_sex_age_M=res_sex[[5]]
aat_t2_adj_sex_age_M=res_sex[[6]] 
reg1b_t2_adj_sex_age_M=res_sex[[7]] 
neo_t3_adj_sex_age_M=res_sex[[8]]
mpo_t3_adj_sex_age_M=res_sex[[9]]
aat_t3_adj_sex_age_M=res_sex[[10]]



neo_t1_adj_M=res_adj[[1]]
mpo_t1_adj_M=res_adj[[2]]
aat_t1_adj_M=res_adj[[3]] 
neo_t2_adj_M=res_adj[[4]] 
mpo_t2_adj_M=res_adj[[5]]
aat_t2_adj_M=res_adj[[6]]
reg1b_t2_adj_M=res_adj[[7]] 
neo_t3_adj_M=res_adj[[8]]
mpo_t3_adj_M=res_adj[[9]]
aat_t3_adj_M=res_adj[[10]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(neo_t1_N_M, mpo_t1_N_M, aat_t1_N_M,
     neo_t2_N_M, mpo_t2_N_M, aat_t2_N_M, reg1b_t2_N_M,
     neo_t3_N_M, mpo_t3_N_M, aat_t3_N_M, 
     file="stool_res_N_M.Rdata")

save(aat_t1_mn, aat_t2_mn, aat_t3_mn,
     mpo_t1_mn, mpo_t2_mn, mpo_t3_mn, reg1b2_t2_mn,
     neo_t1_mn, neo_t2_mn, neo_t3_mn, 
     aat_t1_absmn, aat_t2_absmn, aat_t3_absmn,
     mpo_t1_absmn, mpo_t2_absmn, mpo_t3_absmn, reg1b2_t2_absmn,
     neo_t1_absmn, neo_t2_absmn, neo_t3_absmn, 
     file="stool_res_means.Rdata")

save(stool_overall_mn, file="stool_overall_means.Rdata")

save(neo_t1_unadj_M, mpo_t1_unadj_M, aat_t1_unadj_M,
     neo_t2_unadj_M, mpo_t2_unadj_M, aat_t2_unadj_M, reg1b_t2_unadj_M,
     neo_t3_unadj_M, mpo_t3_unadj_M, aat_t3_unadj_M, 
     file="stool_res_unadj_M.Rdata")

save(neo_t1_adj_sex_age_M, mpo_t1_adj_sex_age_M, aat_t1_adj_sex_age_M,
     neo_t2_adj_sex_age_M, mpo_t2_adj_sex_age_M, aat_t2_adj_sex_age_M, reg1b_t2_adj_sex_age_M,
     neo_t3_adj_sex_age_M, mpo_t3_adj_sex_age_M, aat_t3_adj_sex_age_M, 
     file="stool_res_adj_sex_age_M.Rdata")

save(neo_t1_adj_M, mpo_t1_adj_M, aat_t1_adj_M,
     neo_t2_adj_M, mpo_t2_adj_M, aat_t2_adj_M, reg1b_t2_adj_M,
     neo_t3_adj_M, mpo_t3_adj_M, aat_t3_adj_M, 
     file="stool_res_adj_M.Rdata")

#save data for figures
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(d, file="stool_figure_data.Rdata")

