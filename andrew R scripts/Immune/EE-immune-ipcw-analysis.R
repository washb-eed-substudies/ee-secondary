

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

treatment <- read.csv(paste0(dropboxDir,"Data/Untouched/washb-BD-telo-blind-tr.csv"))
treatment$clusterid <- as.numeric(treatment$clusterid)

#Load EED 
ipcw <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-ipcw.csv", stringsAsFactors = T) %>% select(-c(tr,block))
#ipcw <- left_join(ipcw, treatment, by=c("clusterid"))

#Load in immune analysis dataset
load(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-immune.Rdata"))


#Keep only immune outcomes and time-varying covariates, drop the baseline covariates
colnames(d)
d <- d %>% 
  subset(., select=c(
    childid, dataid, childNo, clusterid,
    agemth_bt2, agemth_bt3, ageday_bt2, ageday_bt3, month2, month3,
    igf_t2,          igf_t3,          crp_t2,         
    agp_t2,          gmcsf_t2,        ifng_t2,         il10_t2,         il12_t2,        
    il13_t2,         il17_t2,         il1_t2,          il2_t2,          il21_t2,        
    il4_t2,          il5_t2,          il6_t2,          tnfa_t2,         gmcsf_t3,       
    ifng_t3,         il10_t3,         il12_t3,         il13_t3,         il17_t3,        
    il1_t3,          il2_t3,          il21_t3,         il4_t3,          il5_t3,         
    il6_t3,          tnfa_t3
  ))

dim(d)
dim(ipcw)
d <- merge(ipcw, d, by = c("dataid","childid","clusterid"), all.x = T, all.y = T)
dim(d)

#Merge in treatments
d <- left_join(d, treatment, by = c("clusterid"))
table(is.na(d$tr))

#Subset to immune analysis arms
d <- subset(d, tr=="Control" | tr=="Nutrition + WSH")
d$tr <- factor(d$tr)
table(d$tr)

#Order for replication:
d<-d[order(d$block,d$clusterid,d$dataid),]


#Drop ID missing from Audrie
#d<-d[d$dataid!=72003,]


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

#Temp fix several monsoon numbers to match audrie 
d$monsoon2 <- as.character(d$monsoon2)
d$monsoon2[d$dataid %in% c(42601, 42602, 42603, 42604, 42605, 42606, 42607, 42608, 45407)] <- 1
d$monsoon2 <- factor(d$monsoon2)



#impute child age with overall median
median(d$ageday_bt2, na.rm=T)
median(d$ageday_bt3, na.rm=T)
d$ageday_bt2[is.na(d$ageday_bt2)] <- median(d$ageday_bt2, na.rm=T)
d$ageday_bt3[is.na(d$ageday_bt3)] <- median(d$ageday_bt3, na.rm=T)


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

d$asset_clock[is.na(d$asset_clock)]<-"99"
d$asset_clock<-factor(d$asset_clock)


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
Wvars2<-c("ageday_bt2", "monsoon2") 
Wvars3<-c("ageday_bt3", "monsoon3") 
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W2$monsoon2 <- as.character(W2$monsoon2)
W3$monsoon3 <- as.character(W3$monsoon3)
W2$monsoon2[is.na(W2$monsoon2)]<-"missing"
W3$monsoon3[is.na(W3$monsoon3)]<-"missing"
W2$monsoon2 <- factor(W2$monsoon2)
W3$monsoon3 <- factor(W3$monsoon3)

#Create indicators for missingness

#Update names to match Audrie
colnames(d) <- gsub("gmcsf","gmc",colnames(d))
colnames(d) <- gsub("tnfa","tnf",colnames(d))
colnames(d) <- gsub("ifng","ifn",colnames(d))

#Subset outcomes
# Y<-d %>% rename(t2_igf=igf_t2,          t3_igf=igf_t3,          t2_crp=crp_t2,          t2_agp=agp_t2,          t2_gmc=gmc_t2,       
#                 t2_ifn=ifn_t2,         t2_il10=il10_t2,         t2_il12=il12_t2,         t2_il13=il13_t2,         t2_il17=il17_t2,        
#                 t2_il1=il1_t2,          t2_il2=il2_t2,          t2_il21=il21_t2,         t2_il4=il4_t2,          t2_il5=il5_t2,         
#                 t2_il6=il6_t2,          t2_tnf=tnf_t2,         t3_gmc=gmc_t3,        t3_ifn=ifn_t3,         t3_il10=il10_t3,        
#                 t3_il12=il12_t3,         t3_il13=il13_t3,         t3_il17=il17_t3,         t3_il1=il1_t3,          t3_il2=il2_t3,         
#                 t3_il21=il21_t3,         t3_il4=il4_t3,          t3_il5=il5_t3,          t3_il6=il6_t3,          t3_tnf=tnf_t3) %>%
#   select(t2_igf,          t3_igf,          t2_crp,          t2_agp,          t2_gmc,       
#          t2_ifn,         t2_il10,         t2_il12,         t2_il13,         t2_il17,        
#          t2_il1,          t2_il2,          t2_il21,         t2_il4,          t2_il5,         
#          t2_il6,          t2_tnf,         t3_gmc,        t3_ifn,         t3_il10,        
#          t3_il12,         t3_il13,         t3_il17,         t3_il1,          t3_il2,         
#          t3_il21,         t3_il4,          t3_il5,          t3_il6,          t3_tnf)
Y<-d %>% select(igf_t2,          igf_t3,          crp_t2,          agp_t2,          gmc_t2,       
                ifn_t2,         il10_t2,         il12_t2,         il13_t2,         il17_t2,        
                il1_t2,          il2_t2,          il21_t2,         il4_t2,          il5_t2,         
                il6_t2,          tnf_t2,         gmc_t3,        ifn_t3,         il10_t3,        
                il12_t3,         il13_t3,         il17_t3,         il1_t3,          il2_t3,         
                il21_t3,         il4_t3,          il5_t3,          il6_t3,          tnf_t3) 


missingness_list <- list()
for(i in 1:ncol(Y)){
  missingness_list[[i]] <- ifelse(is.na(Y[,i]),0,1)
  Y[is.na(Y[,i]),i] <- 9
  names(missingness_list)[i] <- paste0(colnames(Y)[i],".miss")
}
miss <- as.data.frame(bind_rows(missingness_list))
d <- cbind(d, miss)





  
#Run the adjusted ipcw analysis


#Fully adjusted glm models
res_ipcw <- NULL
for(i in 1:ncol(Y)){
  if(grepl("_t2", colnames(Y)[i])){
    temp<-washb_tmle(Y=(Y[,i]), Delta=miss[,i], tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
  }else{
    temp<-washb_tmle(Y=(Y[,i]), Delta=miss[,i], tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
  }
  res_ipcw<-rbind(res_ipcw, as.numeric((t(unlist(temp$estimates$ATE)))))
}
res_ipcw <- as.data.frame(res_ipcw)

colnames(res_ipcw)<-c("RD","var.psi","ci.l","ci.u", "Pval")
res_ipcw$Y <-colnames(Y)






#Compare to Audrie's objects
#Function to load and compile Audrie's objects
load_aud <- function(name.pattern, object_list, subgroup=F){
  print(object_list)
  df <- lapply(object_list, get)
  for(i in which(sapply(df, is.null))){
    print(object_list[i])
    if(subgroup){
      df[[i]] <- data.frame(subgroup=rep(NA,2), RD=rep(NA,2), ci.lb=rep(NA,2), ci.ub=rep(NA,2), SE=rep(NA,2), z=rep(NA,2), `P-value`=rep(NA,2))
    }else{
      df[[i]] <- data.frame(RD=NA, ci.lb=NA, ci.ub=NA, SE=NA, z=NA, `P-value`=NA)
    }
  }
  if(subgroup){
    df <- data.frame(rbindlist(lapply(df, as.data.frame), fill=TRUE),Y = rep(gsub(name.pattern,"",object_list), each=2))
  }else{
    df <- data.frame(rbindlist(lapply(lapply(df, t), as.data.frame), fill=TRUE),Y = gsub(name.pattern,"",object_list))
  }
  return(df)
}
load(paste0(dropboxDir,"Results/Audrie/immune_ipcw.RData"))
name.pattern="_adj_ipcw_L"
object_list=ls(pattern=name.pattern)
aud_ipcw <- load_aud(name.pattern, object_list)


dim(res_ipcw)
dim(aud_ipcw)
comp_ipcw <- full_join(res_ipcw, aud_ipcw, by="Y")
dim(comp_ipcw)
comp_ipcw$RD - comp_ipcw$psi
comp_ipcw$Pval - comp_ipcw$pvalue


save(d, res_ipcw, comp_ipcw, Y, miss, W2, W3, file = here("replication objects/andrew_immune_ipcw_W.rdata"))









