


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
d <- d %>% subset(., select=-c(block))
dim(d)
head(d)
table(d$tr)




#Merge in enrollment information
dim(d)
dim(enrol)
d$dataid<-as.numeric(d$dataid)
d<-left_join(d,enrol, by="dataid")
dim(d)




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


#-------------------------------------------
# stratify below and above 6 
# months of age 
#-------------------------------------------

summary(d$agem1)

d <- d %>% filter(!is.na(agem1))

d$above6mo <- ifelse(d$agem1>=6, 1,0)


d1 <- d %>% filter(above6mo==0)
d2 <- d %>% filter(above6mo==1)


#-------------------------------------------
#Calculate treatment difference:
#-------------------------------------------

#Create empty matrix to hold the glm results:
Lact_t1_under6<-Mann_t1_under6<-LM_t1_under6<-matrix(0, nrow=3, ncol=6)
Lact_t1_above6<-Mann_t1_above6<-LM_t1_above6<-matrix(0, nrow=3, ncol=6)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))


#Unadjusted glm models
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$Lact1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Lact_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(Lact_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Lact_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$Lact1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Lact_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(Lact_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Lact_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$Mann1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Mann_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(Mann_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Mann_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$Mann1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Mann_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(Mann_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Mann_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$LM1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    LM_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(LM_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(LM_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    LM<-washb_glm(Y=log(d2$LM1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    LM_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(LM_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(LM_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }




#-------------------------------------------
# Create plot data.frame
#-------------------------------------------

Lact_t1_under6
Lact_t1_above6
res <- data.frame(rbind(Lact_t1_under6, Lact_t1_above6,
                        Mann_t1_under6 ,Mann_t1_above6,
                        LM_t1_under6, LM_t1_above6))


df <- data.frame(outcome= c(rep("Lact",6),rep("Mann",6),rep("LM",6)),
                 above6mo=rep(c(rep("Under 6 months old",3), rep("6 months or older",3)),3),
                 contrast=rep(c("C v WSH","C v N","C v N+WSH"),6),
                 RD=res$RD,
                 ci.lb=res$ci.l,
                 ci.ub=res$ci.u)
df$above6mo <- factor(df$above6mo)
df$above6mo <- relevel(df$above6mo, ref="Under 6 months old")


#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"

#tr.cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)
tr.cols=c(C=cblack,WSH=cblue,N=cred,"WSH+N"=cgreen)

#cols=c("C v WSH"=corange,"C v N"=cred,"C v N+WSH"=cmagent)
cols=c("C v WSH"=cblue,"C v N"=cred,"C v N+WSH"=cgreen)

theme_set(theme_bw())

p<-ggplot(df, aes(x=contrast)) + 
    geom_point(aes(y=RD, fill=contrast, color=contrast), size = 4) +
    geom_linerange(aes( ymin=ci.lb, ymax=ci.ub, color=contrast),
                   alpha=0.5, size = 3) +
    labs(x = "Contrast", y = "Log Difference") +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values=cols) +
    scale_colour_manual(values=cols) +
    theme(strip.background = element_blank(),
      legend.position="none",
      strip.text.x = element_text(size=12),
      axis.text.x = element_text(size=12)) +
    facet_wrap(outcome~above6mo, ncol=2, scales = "fixed") +
    ggtitle("WBB Intervention Effects on urine biomarkers, stratified by age at measurement round 1")
p


ggsave(p, filename = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/EE-Urine-6mo-diff-plot.pdf",width=10,height=8.5)




