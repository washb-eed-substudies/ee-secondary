



rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d


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
d <- d %>% subset(., select=-c(block))
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


#-------------------------------------------
# stratify below and above 6 
# months of age 
#-------------------------------------------

summary(d$agem1)

d <- d %>% filter(!is.na(agem1))

d$above6mo <- ifelse(d$agem1>=6, 1,0)

d %>% group_by(above6mo) %>% summarize(mn = mean(aat1, na.rm=T), age=mean(agem1))

d %>% group_by(tr,above6mo) %>% summarize(mn = mean(aat1, na.rm=T))

d1 <- d %>% filter(above6mo==0)
d2 <- d %>% filter(above6mo==1)


#-------------------------------------------
#Calculate treatment difference:
#-------------------------------------------

mpo_t1_under6<-mpo_t1_above6<-neo_t1_under6<-neo_t1_above6<-aat_t1_under6<-aat_t1_above6<-matrix(0, nrow=3, ncol=6)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))


#Unadjusted glm models
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$aat1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    aat_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(aat_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(aat_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$aat1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    aat_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(aat_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(aat_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$neo1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    neo_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(neo_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(neo_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$neo1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    neo_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(neo_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(neo_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$mpo1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    mpo_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(mpo_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(mpo_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    mpo<-washb_glm(Y=log(d2$mpo1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    mpo_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(mpo_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(mpo_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }




#-------------------------------------------
# Create plot data.frame
#-------------------------------------------

aat_t1_under6
aat_t1_above6
res <- data.frame(rbind(aat_t1_under6, aat_t1_above6,
                        neo_t1_under6 ,neo_t1_above6,
                        mpo_t1_under6, mpo_t1_above6))


df <- data.frame(outcome= c(rep("AAT",6),rep("NEO",6),rep("MPO",6)),
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
    ggtitle("WBB Intervention Effects on stool biomarkers, stratified by age at measurement round 1")
p


ggsave(p, filename = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/EE-Stool-6mo-diff-plot.pdf",width=10,height=8.5)




