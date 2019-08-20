
#---------------------------------------
# EE-BD-urine.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The urine-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------



###Load in data
rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)
library(tidyr)


#Load in blinded treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
# levels(treatment$tr)
# treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
# levels(treatment$tr)

#Load in L/M outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
outcomes<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")
load("urine_volume.Rdata")

#Load in urine survey data
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
urine<-read.csv("BD-EE-urine.csv")

#Drop and merge fixed urine volumes
urine<-urine %>% subset(select=-c(urineVol_t1,urineVol_t2,urineVol_t3))
urine<-merge(urine, urineVol, by=c("dataid", "childNo"))


#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)



#Merge L/M outcomes
dim(urine)
dim(outcomes)
outcomes$childid<-as.numeric(outcomes$childid)
d<-left_join(urine,outcomes, by="childid")
#d<-cbind(d,outcomes)
dim(d)

#Remove empty row
dim(d)
d<-d[!(is.na(d$childNo) & is.na(d$dataid)),]
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
d$dataid<-as.numeric(d$dataid)
d<-left_join(d,enrol, by="dataid")
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 




#table number of fully collected aliqouts by arm and year
head(d)

#aliqout time 1
t1s1<-d%>% subset(h2aliqout1_t1>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t1s2<-d%>% subset(h2aliqout2_t1>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t1s3<-d%>% subset(h2aliqout3_t1>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t1s4<-d%>% subset(h2aliqout4_t1>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t1s5<-d%>% subset(h2aliqout5_t1>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t1s6<-d%>% subset(h2aliqout6_t1>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t1s7<-d%>% subset(h5aliqout7_t1>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t1s8<-d%>% subset(h5aliqout8_t1>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t1s9<-d%>% subset(h5aliqout9_t1>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t1s10<-d%>% subset(h5aliqout10_t1>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t1s11<-d%>% subset(h5aliqout11_t1>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t1s12<-d%>% subset(h5aliqout12_t1>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 

 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2],t1s6[,2],t1s7[,2],t1s8[,2],t1s9[,2],t1s10[,2],t1s11[,2],t1s12[,2])


#aliqout time 2
t2s1<-d%>% subset(h2aliqout1_t2>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t2s2<-d%>% subset(h2aliqout2_t2>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t2s3<-d%>% subset(h2aliqout3_t2>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t2s4<-d%>% subset(h2aliqout4_t2>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t2s5<-d%>% subset(h2aliqout5_t2>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t2s6<-d%>% subset(h2aliqout6_t2>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t2s7<-d%>% subset(h5aliqout7_t2>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t2s8<-d%>% subset(h5aliqout8_t2>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t2s9<-d%>% subset(h5aliqout9_t2>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t2s10<-d%>% subset(h5aliqout10_t2>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t2s11<-d%>% subset(h5aliqout11_t2>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t2s12<-d%>% subset(h5aliqout12_t2>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 
t3s13<-d%>% subset(preLMaliqout13_t2>1)%>%group_by(tr) %>%summarize(preLMsample13_t2=n()) 

 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2],t2s6[,2],t2s7[,2],t2s8[,2],t2s9[,2],t2s10[,2],t2s11[,2],t2s12[,2],t3s13[,2])


#aliqout time 3
t3s1<-d%>% subset(h2aliqout1_t3>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t3s2<-d%>% subset(h2aliqout2_t3>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t3s3<-d%>% subset(h2aliqout3_t3>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t3s4<-d%>% subset(h2aliqout4_t3>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t3s5<-d%>% subset(h2aliqout5_t3>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t3s6<-d%>% subset(h2aliqout6_t3>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t3s7<-d%>% subset(h5aliqout7_t3>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t3s8<-d%>% subset(h5aliqout8_t3>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t3s9<-d%>% subset(h5aliqout9_t3>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t3s10<-d%>% subset(h5aliqout10_t3>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t3s11<-d%>% subset(h5aliqout11_t3>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t3s12<-d%>% subset(h5aliqout12_t3>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 
t3s13<-d%>% subset(preLMaliqout13_t3>1)%>%group_by(tr) %>%summarize(preLMsample13_t3=n()) 

 
aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2],t3s6[,2],t3s7[,2],t3s8[,2],t3s9[,2],t3s10[,2],t3s11[,2],t3s12[,2],t3s13[,2])


aliquotN_t1[c(1,3,4,2),c(1:2,8)]
aliquotN_t2[c(1,3,4,2),c(1:2,8,14)]
aliquotN_t3[c(1,3,4,2),c(1:2,8,14)]



#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!(is.na(d$Lact1)|is.na(d$Mann1))) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"
#subset(!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1) 

#Tabulate N, gender, and age across survey rounds
t1<-d %>% subset(!(is.na(d$Lact1)|is.na(d$Mann1))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d %>% subset(!(is.na(d$Lact2)|is.na(d$Mann2))) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!(is.na(d$Lact2)|is.na(d$Mann2))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!(is.na(d$Lact3)|is.na(d$Mann3))) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d %>% subset(!(is.na(d$Lact3)|is.na(d$Mann3))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


age_t1_urine_M<-rbind(overallN1, t1)
age_t2_urine_M<-rbind(overallN2, t2)
age_t3_urine_M<-rbind(overallN3, t3)


#Reorder to match Audrie
age_t1_urine_M<-age_t1_urine_M[,c(1,2,4,3,5,7,6)]
age_t2_urine_M<-age_t2_urine_M[,c(1,2,4,3,5,7,6)]
age_t3_urine_M<-age_t3_urine_M[,c(1,2,4,3,5,7,6)]



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
mean(d$per.lact.rec_t2, na.rm=T)
mean(d$per.lact.rec_t3, na.rm=T)

summary(d$per.lact.rec_t3)


table(d$lact.dose_t1==0)
table(d$lact.dose_t2==0)
table(d$lact.dose_t3==0)

#% mannitol recovery = (urine concentration mannitol (mg/L) * urine volume (L) * 100 / total mannitol dosed (mg))
d$per.mann.rec_t1<-d$Mann1*(d$urineVol_t1/1000)*100/d$mann.dose_t1
d$per.mann.rec_t2<-d$Mann2*(d$urineVol_t2/1000)*100/d$mann.dose_t2
d$per.mann.rec_t3<-d$Mann3*(d$urineVol_t3/1000)*100/d$mann.dose_t3
mean(d$per.mann.rec_t1, na.rm=T)
mean(d$per.mann.rec_t2, na.rm=T)
mean(d$per.mann.rec_t3, na.rm=T)



table(d$lact.dose_t1==0)
table(d$lact.dose_t2==0)
table(d$lact.dose_t3==0)


#LM ratio
d$LM1<-d$per.lact.rec_t1/d$per.mann.rec_t1
d$LM2<-d$per.lact.rec_t2/d$per.mann.rec_t2
d$LM3<-d$per.lact.rec_t3/d$per.mann.rec_t3
mean(d$LM1, na.rm=T)


#Data check. Why are there less LM than lact or mann?
table(d$per.mann.rec_t1==0)
table(d$per.mann.rec_t2==0)
table(d$per.mann.rec_t3==0)

table(d$urineVol_t1==0)
table(d$urineVol_t2==0)
table(d$urineVol_t3==0)

table(is.na(d$per.mann.rec_t1))
table(is.na(d$Lact1))


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

#------------------
# Save tbe urine 
# analysis dataset
#------------------

save(d, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/urine_analysis_df.Rdata")

#------------------
#N's and geometric means
#------------------


#Create empty  matrices to hold the Ns and geometric means:
lac_t1_N_M<-man_t1_N_M<-lm_t1_N_M<-lac_t2_N_M<-man_t2_N_M<-lm_t2_N_M<-lac_t3_N_M<-man_t3_N_M<-lm_t3_N<-matrix(0,4,2)


  #N's and geometric means
lac_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact1)) %>% summarize(N=n(), mean= mean(log(Lact1), na.rm=T))   
man_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann1)) %>% summarize(N=n(), mean= mean(log(Mann1), na.rm=T))   
lm_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM1)) %>% summarize(N=n(), mean= mean(log(LM1), na.rm=T))   
lac_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact2)) %>% summarize(N=n(), mean= mean(log(Lact2), na.rm=T))   
man_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann2)) %>% summarize(N=n(), mean= mean(log(Mann2), na.rm=T))   
lm_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM2)) %>% summarize(N=n(), mean= mean(log(LM2), na.rm=T))
lac_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact3)) %>% summarize(N=n(), mean= mean(log(Lact3), na.rm=T))   
man_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann3)) %>% summarize(N=n(), mean= mean(log(Mann3), na.rm=T))   
lm_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM3)) %>% summarize(N=n(), mean= mean(log(LM3), na.rm=T))   


#Means and 95% CI's for mean by arm plots
lac_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Lact1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Lact2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Lact3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Mann1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Mann2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$Mann3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$LM1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$LM2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$LM3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

lac_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact1, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact2, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact3, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann1, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann2, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann3, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM1, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM2, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM3, id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

#Means and 95% CI's not stratified by arm
overall_mn_by_round<- 
  d %>% subset(., select=c(dataid, childNo, block.x, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3,per.mann.rec_t1,per.mann.rec_t2,per.mann.rec_t3, per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3)) %>%
  rename(pm1=per.mann.rec_t1, pm2=per.mann.rec_t2, pm3=per.mann.rec_t3, pl1=per.lact.rec_t1, pl2=per.lact.rec_t2,  pl3=per.lact.rec_t3) %>%
  gather(key, value, -dataid, -childNo, -block.x) %>%
  mutate(biomarker = substr(key, 1,2)) %>% 
  group_by(biomarker) %>% 
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block.x, print = F))) %>% 
  ungroup %>% as.data.frame

overall_mn<- 
  d %>% subset(., select=c(dataid, childNo, block.x, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3,per.mann.rec_t1,per.mann.rec_t2,per.mann.rec_t3, per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3)) %>%
  gather(key, value, -dataid, -childNo, -block.x) %>%
  group_by(key) %>% 
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block.x, print = F))) %>% 
  ungroup %>% as.data.frame
colnames(overall_mn)[1]<-"biomarker"
urine_overall_mn <- rbind(overall_mn_by_round, overall_mn)
urine_overall_mn







#geometric mean function
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

gm_mean(d$Lact1)
gm_mean(d$per.lact.rec_t1)

gm_mean(d$per.lact.rec_t1[d$tr=="Control"])


  d %>% subset(., select=c(dataid, childNo, block.x, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3,per.mann.rec_t1,per.mann.rec_t2,per.mann.rec_t3, per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3)) %>%
  gather(key, value, -dataid, -childNo, -block.x) %>%
  group_by(key) %>% 
  do(as.data.frame(gm_mean(.$value))) %>% 
  ungroup %>% as.data.frame







#------------------
#Unadjusted GLM
#------------------

#dataframe of urine biomarkers:
Y<-d %>% select(Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#Create empty matrix to hold the glm results:
lact_t1_unadj<-mann_t1_unadj<-lm_t1_unadj<-matrix(0, nrow=5, ncol=6)
lact_t2_unadj<-mann_t2_unadj<-lm_t2_unadj<-matrix(0, nrow=5, ncol=6)
lact_t3_unadj<-mann_t3_unadj<-lm_t3_unadj<-matrix(0, nrow=5, ncol=6)

res_unadj<-list(lact_t1_unadj=lact_t1_unadj, mann_t1_unadj=mann_t1_unadj, lm_t1_unadj=lm_t1_unadj, 
                lact_t2_unadj=lact_t2_unadj, mann_t2_unadj=mann_t2_unadj, lm_t2_unadj=lm_t2_unadj, 
                lact_t3_unadj=lact_t3_unadj, mann_t3_unadj=mann_t3_unadj, lm_t3_unadj=lm_t3_unadj)


#for(i in 1:ncol(Y)){
#Y[,1] <- gsub("NaN", "NA", Y[,1])
#Y[,1] <- gsub("Inf", "NA", Y[,1])
#Y[,1] <- as.numeric(Y[,1])  
#print(class(Y[,1]))
#}
#head(Y)


#Unadjusted glm models
for(i in 1:ncol(Y)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}


#------------------
#Age and sex adjusted GLMs
#------------------
d$sex<-as.factor(d$sex)
  d$sex=relevel(d$sex,ref="0")

#Create empty matrix to hold the glm results:
lact_t1_sex<-mann_t1_sex<-lm_t1_sex<-matrix(0, nrow=5, ncol=6)
lact_t2_sex<-mann_t2_sex<-lm_t2_sex<-matrix(0, nrow=5, ncol=6)
lact_t3_sex<-mann_t3_sex<-lm_t3_sex<-matrix(0, nrow=5, ncol=6)

res_diff <- res_month <- res_sex <- list(lact_t1_sex=lact_t1_sex, mann_t1_sex=mann_t1_sex, lm_t1_sex=lm_t1_sex, 
                lact_t2_sex=lact_t2_sex, mann_t2_sex=mann_t2_sex, lm_t2_sex=lm_t2_sex, 
                lact_t3_sex=lact_t3_sex, mann_t3_sex=mann_t3_sex, lm_t3_sex=lm_t3_sex)


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
for(i in 4:6){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 7:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}


#Age and sex and month adjusted glm models
for(i in 1:3){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged1, d$month1), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_month[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_month[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_month[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 4:6){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged2, d$month2), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_month[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_month[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_month[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 7:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged3, d$month3), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_month[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_month[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_month[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}

#check change when adding month
for(i in 1:length(res_sex)) res_diff[[i]]<-res_sex[[i]]-res_month[[i]]

#Stack dataframes and add a column for variable and time

#look at mean and max RD change

#list contrasts where significance changes with added month

#------------------
#Adjusted GLM
#------------------



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

d <- d %>% mutate(monsoon1 = ifelse(month1 > 4 & month1 < 11, "1", "0"),
                  monsoon2 = ifelse(month2 > 4 & month2 < 11, "1", "0"),
                  monsoon3 = ifelse(month3 > 4 & month3 < 11, "1", "0"),
                  monsoon1 = ifelse(is.na(month1),"missing", monsoon1),
                  monsoon2 = ifelse(is.na(month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(month3),"missing", monsoon3),
                  monsoon1 = factor(monsoon1),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))

table(is.na(d$monsoon1))
table(d$monsoon1)

Wvars1<-c("aged1", "monsoon1") 
Wvars2<-c("aged2", "monsoon2") 
Wvars3<-c("aged3", "monsoon3") 

table(is.na(d$aged1), is.na(d$LM1))



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
W$sex<-as.factor(W$sex)
  d$sex=relevel(d$sex,ref="0")
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


#dataframe of urine biomarkers:
Y<-d %>% select(Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3)

#Create empty matrix to hold the tmle results:
res_adj<-list(lact_t1_adj=matrix(0,5,6), mann_t1_adj=matrix(0,5,6), lm_t1_adj=matrix(0,5,6), 
                lact_t2_adj=matrix(0,5,6), mann_t2_adj=matrix(0,5,6), lm_t2_adj=matrix(0,5,6),  
                lact_t3_adj=matrix(0,5,6), mann_t3_adj=matrix(0,5,6), lm_t3_adj=matrix(0,5,6))

d %>% group_by(tr) %>%
  summarize(Lac=mean(log(Lact1) ,na.rm=T), N=n())
mean(log(d$Lact1), na.rm=T)
mean(log(Y[,1]), na.rm=T)

table(is.na(d$Lact1))
table(is.na(Y[,1]))



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
for(i in 7:9){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}








#------------------
#Save objects
#------------------
lac_t1_unadj_M=res_unadj[[1]]
man_t1_unadj_M=res_unadj[[2]]
lm_t1_unadj_M=res_unadj[[3]] 
lac_t2_unadj_M=res_unadj[[4]] 
man_t2_unadj_M=res_unadj[[5]]
lm_t2_unadj_M=res_unadj[[6]] 
lac_t3_unadj_M=res_unadj[[7]]
man_t3_unadj_M=res_unadj[[8]]
lm_t3_unadj_M=res_unadj[[9]]

lac_t1_adj_sex_age_M=res_sex[[1]]
man_t1_adj_sex_age_M=res_sex[[2]]
lm_t1_adj_sex_age_M=res_sex[[3]] 
lac_t2_adj_sex_age_M=res_sex[[4]] 
man_t2_adj_sex_age_M=res_sex[[5]]
lm_t2_adj_sex_age_M=res_sex[[6]]
lac_t3_adj_sex_age_M=res_sex[[7]]
man_t3_adj_sex_age_M=res_sex[[8]]
lm_t3_adj_sex_age_M=res_sex[[9]]

lac_t1_adj_M=res_adj[[1]]
man_t1_adj_M=res_adj[[2]]
lm_t1_adj_M=res_adj[[3]] 
lac_t2_adj_M=res_adj[[4]] 
man_t2_adj_M=res_adj[[5]]
lm_t2_adj_M=res_adj[[6]]
lac_t3_adj_M=res_adj[[7]]
man_t3_adj_M=res_adj[[8]]
lm_t3_adj_M=res_adj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(lac_t1_N_M, man_t1_N_M, lm_t1_N_M,
     lac_t2_N_M, man_t2_N_M, lm_t2_N_M,
     lac_t3_N_M, man_t3_N_M,lm_t3_N_M,
     file="urine_res_N_M.Rdata")

save(lm_t1_mn, lm_t2_mn, lm_t3_mn,
     lac_t1_mn, lac_t2_mn, lac_t3_mn, 
     man_t1_mn, man_t2_mn, man_t3_mn, 
     lm_t1_absmn, lm_t2_absmn, lm_t3_absmn,
     lac_t1_absmn, lac_t2_absmn, lac_t3_absmn, 
     man_t1_absmn, man_t2_absmn, man_t3_absmn, 
     file="urine_res_means.Rdata")

save(urine_overall_mn, file="urine_overall_means.Rdata")

save(lac_t1_unadj_M, man_t1_unadj_M, lm_t1_unadj_M,
     lac_t2_unadj_M, man_t2_unadj_M, lm_t2_unadj_M, 
     lac_t3_unadj_M,man_t3_unadj_M, lm_t3_unadj_M,
     file="urine_res_unadj_M.Rdata")


save(lac_t1_adj_sex_age_M, man_t1_adj_sex_age_M, lm_t1_adj_sex_age_M,
     lac_t2_adj_sex_age_M, man_t2_adj_sex_age_M, lm_t2_adj_sex_age_M, 
     lac_t3_adj_sex_age_M,man_t3_adj_sex_age_M, lm_t3_adj_sex_age_M,
     file="urine_res_adj_sex_age_M.Rdata")

save(lac_t1_adj_M, man_t1_adj_M, lm_t1_adj_M,
     lac_t2_adj_M, man_t2_adj_M, lm_t2_adj_M, 
     lac_t3_adj_M,man_t3_adj_M, lm_t3_adj_M,
     file="urine_res_adj_M.Rdata")


#save data for figures
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(d, file="urine_figure_data.Rdata")



#--------------------------------
# Percent L and M recovery
# (for supplimentary table)
#--------------------------------

#  library(EnvStats)
# # d <- read.csv( "C:/Users/andre/Downloads/MyData.csv")
# # 
# #geometric mean function
# gm_mean = function(x, na.rm=TRUE){
#   exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x[!is.na(x)]))
# }
# 
# d <- d[d$tr=="Control",]
# 
# gm_mean(d$per.lact.rec_t1[d$tr=="Control"])
# 
# gm_mean(d$per.lact.rec_t1)
# exp(mean(log(d$per.lact.rec_t1),na.rm=T))
# geoMean(d$per.lact.rec_t1,na.rm=T)
# 
# geoSD(d$per.lact.rec_t1,na.rm=T)
# exp(sd(log(d$per.lact.rec_t1),na.rm=T))
# # 
# # gm_mean(d$per.lact.rec_t1[d$tr=="Control"])
# # exp(mean(log(d$per.lact.rec_t1[d$tr=="Control"]),na.rm=T))
# # 
# # geoSD(d$per.lact.rec_t1)
# # 
# # write.csv(d, file = "C:/Users/andre/Downloads/MyData.csv")



#N's and geometric means
perl1_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t1)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t1), na.rm=T)), sd= exp(sd(log(per.lact.rec_t1), na.rm=T)))  
perl2_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t2)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t2), na.rm=T)), sd= exp(sd(log(per.lact.rec_t2), na.rm=T)))  
perl3_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t3)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t3), na.rm=T)), sd= exp(sd(log(per.lact.rec_t3), na.rm=T)))  

perm1_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t1)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t1), na.rm=T)), sd= exp(sd(log(per.mann.rec_t1), na.rm=T)))   
perm2_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t2)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t2), na.rm=T)), sd= exp(sd(log(per.mann.rec_t2), na.rm=T)))  
perm3_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t3)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t3), na.rm=T)), sd= exp(sd(log(per.mann.rec_t3), na.rm=T)))  



#Means and 95% CI's for mean by arm plots
perl1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perl2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perl3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

perm1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t1), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perm2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t2), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perm3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t3), id=.$block.x, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

#Convert from log to raw scale to get the geometric mean
perl1_mn<-perl1_mn[,-c(3:4)]
perl2_mn<-perl2_mn[,-c(3:4)]
perl3_mn<-perl3_mn[,-c(3:4)]
perm1_mn<-perm1_mn[,-c(3:4)]
perm2_mn<-perm2_mn[,-c(3:4)]
perm3_mn<-perm3_mn[,-c(3:4)]

perl1_mn<-exp(perl1_mn[,c(2:4)])
perl2_mn<-exp(perl2_mn[,c(2:4)])
perl3_mn<-exp(perl3_mn[,c(2:4)])
perm1_mn<-exp(perm1_mn[,c(2:4)])
perm2_mn<-exp(perm2_mn[,c(2:4)])
perm3_mn<-exp(perm3_mn[,c(2:4)])


perl1_unadj_M<-perl2_unadj_M<-perl3_unadj_M<-matrix(0, nrow=5, ncol=6)
perm1_unadj_M<-perm2_unadj_M<-perm3_unadj_M<-matrix(0, nrow=5, ncol=6)
perl1_adj_sex_age_M<-perl2_adj_sex_age_M<-perl3_adj_sex_age_M<-matrix(0, nrow=5, ncol=6)
perm1_adj_sex_age_M<-perm2_adj_sex_age_M<-perm3_adj_sex_age_M<-matrix(0, nrow=5, ncol=6)
perl1_adj_M<-perl2_adj_M<-perl3_adj_M<-matrix(0, nrow=5, ncol=6)
perm1_adj_M<-perm2_adj_M<-perm3_adj_M<-matrix(0, nrow=5, ncol=6)

colnames(perl1_unadj_M)<-colnames(perl2_unadj_M)<-colnames(perl3_unadj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_unadj_M)<-colnames(perm2_unadj_M)<-colnames(perm3_unadj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perl1_adj_sex_age_M)<-colnames(perl2_adj_sex_age_M)<-colnames(perl3_adj_sex_age_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_adj_sex_age_M)<-colnames(perm2_adj_sex_age_M)<-colnames(perm3_adj_sex_age_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perl1_adj_M)<-colnames(perl2_adj_M)<-colnames(perl3_adj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_adj_M)<-colnames(perm2_adj_M)<-colnames(perm3_adj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")

rownames(perl1_unadj_M)<-rownames(perl2_unadj_M)<-rownames(perl3_unadj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_unadj_M)<-rownames(perm2_unadj_M)<-rownames(perm3_unadj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perl1_adj_sex_age_M)<-rownames(perl2_adj_sex_age_M)<-rownames(perl3_adj_sex_age_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_adj_sex_age_M)<-rownames(perm2_adj_sex_age_M)<-rownames(perm3_adj_sex_age_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perl1_adj_M)<-rownames(perl2_adj_M)<-rownames(perl3_adj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_adj_M)<-rownames(perm2_adj_M)<-rownames(perm3_adj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")

  for(j in 1:5){
    perl1_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
 
    perl1_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
 
    perl1_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=W1, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=W1, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
  }






save(
perl1_N_M,
perl2_N_M, 
perl3_N_M, 
perm1_N_M,  
perm2_N_M,
perm3_N_M,  
perl1_mn,
perl2_mn,
perl3_mn,
perm1_mn,
perm2_mn,
perm3_mn,
perl1_unadj_M,perl2_unadj_M,perl3_unadj_M,
perm1_unadj_M,perm2_unadj_M,perm3_unadj_M,
perl1_adj_sex_age_M,perl2_adj_sex_age_M,perl3_adj_sex_age_M,
perm1_adj_sex_age_M,perm2_adj_sex_age_M,perm3_adj_sex_age_M,
perl1_adj_M,perl2_adj_M,perl3_adj_M,
perm1_adj_M,perm2_adj_M,perm3_adj_M,
     file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/pre_recovery_res_M.Rdata")



lac_t1_N_rec_M <- perl1_N_M
lac_t2_N_rec_M <- perl2_N_M
lac_t3_N_rec_M <- perl3_N_M

man_t1_N_rec_M <- perm1_N_M
man_t2_N_rec_M <- perm2_N_M
man_t3_N_rec_M <- perm3_N_M

lac_t1_unadj_rec_M <- perl1_unadj_M 
lac_t2_unadj_rec_M <- perl2_unadj_M
lac_t3_unadj_rec_M <- perl3_unadj_M

man_t1_unadj_rec_M <- perm1_unadj_M
man_t2_unadj_rec_M <- perm2_unadj_M
man_t3_unadj_rec_M <- perm3_unadj_M

lac_t1_adj_rec_M <- perl1_adj_M
lac_t2_adj_rec_M <- perl2_adj_M
lac_t3_adj_rec_M <- perl3_adj_M

man_t1_adj_rec_M <- perm1_adj_M
man_t2_adj_rec_M <- perm2_adj_M
man_t3_adj_rec_M <- perm3_adj_M


save(
lac_t1_N_rec_M,
lac_t2_N_rec_M,
lac_t3_N_rec_M,

man_t1_N_rec_M,
man_t2_N_rec_M,
man_t3_N_rec_M,

lac_t1_unadj_rec_M,
lac_t2_unadj_rec_M,
lac_t3_unadj_rec_M,

man_t1_unadj_rec_M,
man_t2_unadj_rec_M,
man_t3_unadj_rec_M,

lac_t1_adj_rec_M,
lac_t2_adj_rec_M,
lac_t3_adj_rec_M,

man_t1_adj_rec_M,
man_t2_adj_rec_M,
man_t3_adj_rec_M,
file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/pre_recovery_res2_M.Rdata")
