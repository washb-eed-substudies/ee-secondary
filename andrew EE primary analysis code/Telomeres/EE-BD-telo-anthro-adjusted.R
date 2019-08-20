
#---------------------------------------
# EE-BD-telo-anthro.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the anthro-adjusted
# ATE of N+WSH on telomere length
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(caret)
library(tmle)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
#load("washb-BD-telo-blind-tr.Rdata")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
#setwd("C:/Users/andre/Dropbox/HBGDki/WASH Benefits Analysis/0. Data/WBB-primary-outcome-datasets/")
#load("washb-bangladesh-tr.Rdata")
#d$clusterid<-as.numeric(d$clusterid)
#treatment<-d

levels(treatment$tr)
#treatment$tr <- factor(treatment$tr,levels=c("Control","Nutrition + WSH"))
#levels(treatment$tr)
#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)
#enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")
anthro<-read.csv("BD-EE-anthro.csv")
#Only keep variables needed
anthro <- anthro %>% subset(select=c(dataid, childNo,
                                     laz2,	waz2,	whz2,	bmiz2,	hcz2,
                                     laz3,	waz3,	whz3,	bmiz3,	hcz3))



#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 
 #test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

 #Merge in anthro measures
 d<-left_join(d, anthro, by=c("dataid","childNo"))



    
############################
#Adjusted GLMs-full+anthro
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
Wvars2<-c("aged2", "month2", "staffid2",  "laz2",  "waz2",  "whz2",  "bmiz2",  "hcz2") 
Wvars3<-c("aged3", "month3", "staffid3",  "laz3",  "waz3",  "whz3",  "bmiz3",  "hcz3") 





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
    ts_t2_anthro_adj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=W2, forcedW = c("laz2",  "waz2",  "whz2",  "bmiz2",  "hcz2"), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    ts_t3_anthro_adj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=W3, forcedW = c("laz3",  "waz3",  "whz3",  "bmiz3",  "hcz3"), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    delta_ts_anthro_adj_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=W_delta, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR


    
    
    
#Effect of anthro measures on TL     
    


###TMLE by subgroup function
tmle_subgroup<-function(d=d, Y="laz", W, V="sex", ID="block", levelsV=2, SLlibrary, measure="RD", family="gaussian", washb_prescreen=F, continiousV=F, Delta=F){

  # Y="TS2"
  # W=W
  # V="laz2"
  # ID="block.x"
  # levelsV=4
  # SLlibrary=library
  # measure="RD"
  # continiousV=T

  
      Vdat<-subset(d, select=V)

  if(continiousV==T){
      #Divide into quartiles
  V.tertile<-quantile(Vdat, probs = seq(0, 1, 1/levelsV), type=1, na.rm=T) 

  d$V.tertile<-1
  d$V.tertile[Vdat>V.tertile[2]]<-2
  if(levelsV>2){d$V.tertile[Vdat>V.tertile[3]]<-3}
  if(levelsV>3){d$V.tertile[Vdat>V.tertile[4]]<-4}
  if(levelsV>4){d$V.tertile[Vdat>V.tertile[5]]<-5}

  d$V.tertile<-factor(d$V.tertile)
  Vdat<-subset(d, select=V.tertile)
  #Create select covariate list  
  if(sum(Wvars %in% V)>0){
    Wvars<-Wvars[-which(Wvars %in% V)]
    }
  }
  
  #V1<-d[Vdat[,1]==levels(Vdat[,1])[1],]
  V2<-d[Vdat[,1]==levels(Vdat[,1])[2] | Vdat[,1]==levels(Vdat[,1])[1],]
  
  if(levelsV>2) V3<-d[Vdat[,1]==levels(Vdat[,1])[3] | Vdat[,1]==levels(Vdat[,1])[1],]
  if(levelsV>3) V4<-d[Vdat[,1]==levels(Vdat[,1])[4] | Vdat[,1]==levels(Vdat[,1])[1],]
  if(levelsV>4) V5<-d[Vdat[,1]==levels(Vdat[,1])[5] | Vdat[,1]==levels(Vdat[,1])[1],]
    #out1<-tmleV(dat=V1, Y=Y, W=W, V=V, ID=ID, measure=measure, family=family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta)
    out<-tmleV(dat=V2, Y=Y, W=W, V=V, ID=ID, measure=measure, family=family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta)
    #out<-rbind(out1,out2)
  
  if(levelsV>2){
    out3<-tmleV(dat=V3, Y=Y, W=W, V=V, ID=ID, measure=measure, family=family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta)
    out<-rbind(out,out3)
  }
  if(levelsV>3){
    out4<-tmleV(dat=V4, Y=Y, W=W, V=V, ID=ID, measure=measure, family=family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta)
    out<-rbind(out,out4)  
    }
  if(levelsV>4){
    out5<-tmleV(dat=V5, Y=Y, W=W, V=V, ID=ID, measure=measure, family=family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta)
    out<-rbind(out,out5)  
  }
    
    #Format output
    out<-as.data.frame(out)
    #rownames(out)<-unique(Vdat[,1])#[levelsV]
    out$subgroup<-levels(Vdat[,1])[2:levelsV]
    out$variable<-V
    rownames(out)<-NULL
    out<-out[,c(8,7,1:5,6)]
      colnames(out)[8]<-"Mean"
      print(out)

      cat("Mean V across ntiles:\n")
      dmeanV<-d %>% subset(select=c("V.tertile",V))
      colnames(dmeanV)[2]<-"V"
  meanV<-dmeanV %>%
    group_by(V.tertile) %>%
    summarise(meanV=mean(V, na.rm=T))
  print(meanV)
    
      
    return(out) 
}


tmleV<-function(dat, Y, W, V, ID, measure, family, SLlibrary=SLlibrary, washb_prescreen=washb_prescreen, Delta=Delta){
  
  
  
  if(V %in% W){W<-W[-which(W %in% V)]}
  y<-subset(dat, select=Y)
  if(Delta==T){y.miss<-subset(dat, select=laz.miss)}else{
    y.miss<-NULL
  }
  a<-ifelse(dat$V.tertile==1,0,1)
  id<-subset(dat, select=ID)
  
  
  if(washb_prescreen==T){
  screenW <- subset(dat, select = W)
    suppressWarnings(Wscreen <- washb_prescreen(Y = y[,1], Ws = screenW, family = family, pval = 0.2, print = T))
    w <- subset(dat, select = c(Wscreen)) 
  }else{
     w<-subset(dat, select=c(W))
  }

  w<-design_matrix(w)
  #w<-w[,-nearZeroVar(w)] #Remove near zero variance columns
  if(Delta==T){
  dat<-data.frame(y,a,id,y.miss,w)
  dat<-dat[complete.cases(dat),]
  w<-dat[,5:ncol(dat)]
  }else{
    dat<-data.frame(y,a,id,w)
    dat<-dat[!is.na(y),]
    w<-dat[,4:ncol(dat)]
  }
  
  print(table(dat[,2]))
  
  if(Delta==F){
    print(dim(dat))
    dat<-dat[complete.cases(dat),]
    w<-dat[,4:ncol(dat)]
    print(dim(dat))
    set.seed(67890)
    fit<-tmle(Y=dat[,1], A=dat[,2], W=w, family=family, Q.SL.library=SLlibrary, id=dat[,3], g.SL.library =SLlibrary)
  }else{
  set.seed(67890)
  fit<-tmle(Y=dat[,1], A=dat[,2], W=w, Delta=dat[,4], family=family, Q.SL.library=SLlibrary, id=dat[,3], g.SL.library =SLlibrary)
  }
  
  if(measure=="RD"){
    out<-unlist(fit$estimates$ATE)
  }
  if(measure=="RR"){
    out<-unlist(fit$estimates$RR)
  }
  #add in mean Y and A
  out<-c(out,mean(dat[,1], na.rm=T))
  
  return(out)
}
    
 
library=list(c("SL.glm","screen.corP"),c("SL.bayesglm","screen.corP"),
             c("SL.gam","screen.corP"),c("SL.glm.interaction","screen.corP"),
             c("SL.mean","All"), c("SL.stepAIC","All"))  

#d$A<-ifelse(d$tr=="Control",0,1)

     
  LAZ2_quart.C<-tmle_subgroup(d=d[d$tr=="Control",], Y="TS2", W=Wvars, V="laz2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  LAZ2_quart.WSHN<-tmle_subgroup(d=d[d$tr=="Nutrition + WSH",], Y="TS2", W=Wvars, V="laz2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  LAZ2_quart<-tmle_subgroup(d=d, Y="TS2", W=Wvars, V="laz2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
   
     
  LAZ2_quart.C<-tmle_subgroup(d=d[d$tr=="Control",], Y="TS3", W=Wvars, V="laz3", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  LAZ2_quart.WSHN<-tmle_subgroup(d=d[d$tr=="Nutrition + WSH",], Y="TS3", W=Wvars, V="laz3", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  LAZ2_quart<-tmle_subgroup(d=d, Y="TS3", W=Wvars, V="laz3", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
   
          
  TS2_quart.C<-tmle_subgroup(d=d[d$tr=="Control",], Y="laz3", W=Wvars, V="TS2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  TS2_quart.WSHN<-tmle_subgroup(d=d[d$tr=="Nutrition + WSH",], Y="laz3", W=Wvars, V="TS2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
  TS2_quart<-tmle_subgroup(d=d, Y="laz3", W=Wvars, V="TS2", ID="block.x", levelsV=5, SLlibrary=library, measure="RD", continiousV=T)
   
       
 
##########################################
#Save objects for replication
##########################################


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(age_t2_blood_M, age_t3_blood_M, ts_t2_N_M, ts_t3_N_M,ts_t2_unadj_M, ts_t3_unadj_M, ts_t2_adj_sex_age_M, ts_t3_adj_sex_age_M, ts_t2_adj_M, ts_t3_adj_M,
     ts_t2_subgroup_M, ts_t3_subgroup_M, delta_ts_N_M, delta_ts_unadj_M, delta_ts_adj_M, delta_ts_adj_sex_age_M, delta_ts_subgroup_M,
     ts_t2_N_subgroup_M, ts_t3_N_subgroup_M, delta_ts_N_subgroup_M,
     file="telo_anthro_res.Rdata")











