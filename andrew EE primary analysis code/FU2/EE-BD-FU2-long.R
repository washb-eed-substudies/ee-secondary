


rm(list=ls())
library(tidyverse)
library(xlsx)
library(stringr)
library(washb)
library(knitr)
library(lubridate)
library(reshape2)
library(data.table)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Endline/")

#Saliva lab data
child <- read.xlsx("EE_Endline_Child_Saliva_FU2.xlsx", 1)
mother <- read.xlsx("EE_Endline_Mother_Saliva_FU2.xlsx", 1)



#Load primary EED dataset
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
d <- read.csv("EE-BD_fulldata.csv")


#Load anthropometry outcomes and only keep ID and outcome variables
anthro<-read.csv("BD-EE-anthro.csv")
anthro <- anthro %>% rename(an_aged1= aged1, an_aged2= aged2, an_aged3= aged3,
                            an_month1=month1, an_month2=month2, an_month3=month3) %>%
                      subset(select=c(dataid, childNo,
                                     laz1,	waz1,	whz1,	hcz1,
                                     laz2,	waz2,	whz2,	hcz2,
                                     laz3,	waz3,	whz3,	hcz3,
                                     an_aged1, an_aged2, an_aged3,
                                     an_month1, an_month2, an_month3))

#Merge in anthro measures
 d<-left_join(d, anthro, by=c("dataid","childNo"))

 #Create binary anthropometry outcomes
 d$laz1[d$laz1 < -5 | d$laz1 > 5] <- NA
 d$laz2[d$laz2 < -5 | d$laz2 > 5] <- NA
 d$laz3[d$laz3 < -5 | d$laz3 > 5] <- NA

 d$whz1[d$whz1 < -5 | d$whz1 > 5] <- NA
 d$whz2[d$whz2 < -5 | d$whz2 > 5] <- NA
 d$whz3[d$whz3 < -5 | d$whz3 > 5] <- NA
 
 d$waz1[d$waz1 < -5 | d$waz1 > 5] <- NA
 d$waz2[d$waz2 < -5 | d$waz2 > 5] <- NA
 d$waz3[d$waz3 < -5 | d$waz3 > 5] <- NA
 
 d$hcz1[d$hcz1 < -5 | d$hcz1 > 5] <- NA
 d$hcz2[d$hcz2 < -5 | d$hcz2 > 5] <- NA
 d$hcz3[d$hcz3 < -5 | d$hcz3 > 5] <- NA

 d$stunt1 <- ifelse(d$laz1 < -2, 1, 0)
     d$stunt1[is.na(d$laz1)] <- NA 
 d$stunt2 <- ifelse(d$laz2 < -2, 1, 0)
     d$stunt2[is.na(d$laz2)] <- NA
 d$stunt3 <- ifelse(d$laz3 < -2, 1, 0)
     d$stunt3[is.na(d$laz3)] <- NA

 d$wast1 <- ifelse(d$whz1 < -2, 1, 0)
     d$wast1[is.na(d$whz1)] <- NA
 d$wast2 <- ifelse(d$whz2 < -2, 1, 0)
     d$wast2[is.na(d$whz2)] <- NA
 d$wast3 <- ifelse(d$whz3 < -2, 1, 0)
     d$wast3[is.na(d$whz3)] <- NA      

 d$sstunt1 <- ifelse(d$laz1 < -3, 1, 0)
     d$sstunt1[is.na(d$laz1)] <- NA 
 d$sstunt2 <- ifelse(d$laz2 < -3, 1, 0)
     d$sstunt2[is.na(d$laz2)] <- NA
 d$sstunt3 <- ifelse(d$laz3 < -3, 1, 0)
     d$sstunt3[is.na(d$laz3)] <- NA

 d$swast1 <- ifelse(d$whz1 < -3, 1, 0)
     d$swast1[is.na(d$whz1)] <- NA
 d$swast2 <- ifelse(d$whz2 < -3, 1, 0)
     d$swast2[is.na(d$whz2)] <- NA
 d$swast3 <- ifelse(d$whz3 < -3, 1, 0)
     d$swast3[is.na(d$whz3)] <- NA     

 d$underwt1 <- ifelse(d$waz1 < -2, 1, 0)
     d$underwt1[is.na(d$waz1)] <- NA
 d$underwt2 <- ifelse(d$waz2 < -2, 1, 0)
     d$underwt2[is.na(d$waz2)] <- NA
 d$underwt3 <- ifelse(d$waz3 < -2, 1, 0)
     d$underwt3[is.na(d$waz3)] <- NA      

 d$sunderwt1 <- ifelse(d$waz1 < -3, 1, 0)
     d$sunderwt1[is.na(d$waz1)] <- NA
 d$sunderwt2 <- ifelse(d$waz2 < -3, 1, 0)
     d$sunderwt2[is.na(d$waz2)] <- NA
 d$sunderwt3 <- ifelse(d$waz3 < -3, 1, 0)
     d$sunderwt3[is.na(d$waz3)] <- NA       
     

#Reshape from wide to long     
colnames(d)
 
time_var_list <- list()
time_vars <-c("st_staffid", "st_date", "st_agem", "st_month", "st_aged", "st_monsoon", "ur_staffid", "ur_date", "ur_aged", "ur_agem", "ur_month", "an_aged", "an_month", "LMvol_t",
  "urineVol_t", "ur_monsoon", "aat", "mpo", "neo", "reg1b", "Mann", "Lact", "LM", "lact.dose_t", "mann.dose_t", "per.lact.rec_t", 
  "per.mann.rec_t", "lact.rec.MMOL_t", "mann.rec.MMOL_t", "laz", "waz", "whz", "hcz", "stunt", "wast", "sstunt", "swast", "underwt", "sunderwt")
for(i in 1:length(time_vars)){
  time_var_list[[i]] <- paste(time_vars[i], 1:3, sep = "")
  names(time_var_list)[i] <- time_vars[i]
}

d$reg1b1 <- d$reg1b3 <- NA

d = melt(data.table(d), measure = time_var_list, value.name = names(time_var_list))
d <- d %>% rename(round = variable) %>% as.data.frame()
     

#Process child lab data

      #   There were 2 typo's in the sample IDs of the child FU2 lab results:
      # 26703E1Z01 instead of 25703E1Z01
      # 65303E1Z01 instead of 65603E1Z01
  child$sample_id <- as.character(child$sample_id)
  child$sample_id[child$sample_id=="25703E1Z01"] <- "26703E1Z01"
  child$sample_id[child$sample_id=="65603E1Z01"] <- "65303E1Z01"

    
  #Grab dataid from the sample ID
  child$dataid <- as.numeric(sapply(strsplit(child$sample_id, "E", fixed=T), `[`, 1))
  child$childNo <- as.numeric(substr(sapply(strsplit(child$sample_id, "E", fixed=T), `[`, 2),1,1))
  
  #mark FU2 status
  child$cFU2 <- NA
  child$cFU2[child$lea_result=="Positive" & child$leb_result=="Positive"] <- "Positive"
  child$cFU2[child$lea_result=="Negative" & child$leb_result=="Positive"] <- "Positive"
  child$cFU2[child$lea_result=="Positive" & child$leb_result=="Negative"] <- "Negative"
  child$cFU2[child$lea_result=="Negative" & child$leb_result=="Negative"] <- "Inconclusive"
  table(child$cFU2)
  
  child <- child %>% subset(., select=c(dataid, childNo, cFU2))
  


#Process mother lab data

  #Grab dataid from the sample ID
  mother$dataid <- as.numeric(sapply(strsplit(as.character(mother$sample_id), "E", fixed=T), `[`, 1))

  #mark FU2 status
  mother$mFU2 <- NA
  mother$mFU2[mother$lea_result=="Positive" & mother$leb_result=="Positive"] <- "Positive"
  mother$mFU2[mother$lea_result=="Negative" & mother$leb_result=="Positive"] <- "Positive"
  mother$mFU2[mother$lea_result=="Positive" & mother$leb_result=="Negative"] <- "Negative"
  mother$mFU2[mother$lea_result=="Negative" & mother$leb_result=="Negative"] <- "Inconclusive"
  table(mother$mFU2)
  
  mother <- mother %>% subset(., select=c(dataid, mFU2))
  
  
#Merge saliva results with the primary EED dataset
d <- left_join(d, child, by=c("dataid","childNo"))
d <- left_join(d, mother, by=c("dataid"))




#Summary statistics on FU2

overall_c <- d %>% group_by(cFU2) %>% summarize(N_child=n())
overall_m <- d %>% group_by(mFU2) %>% filter(childNo==1) %>% summarize(N_mother=n()) # filter to childNo==1 to remove duplicated
overall_c <- data.frame(tr="Overall", overall_c)
overall_m <- data.frame(tr="Overall", overall_m)

tr_c <- d %>% group_by(tr, cFU2) %>% summarize(N_child=n())
tr_m <- d %>% group_by(tr, mFU2) %>% filter(childNo==1) %>% summarize(N_mother=n()) # filter to childNo==1 to remove duplicated

summary_c <- bind_rows(overall_c, tr_c) %>% rename(FU2_status = cFU2)
summary_m <- bind_rows(overall_m, tr_m) %>% rename(FU2_status = mFU2)
summary_res <- merge(summary_c, summary_m, by=c("tr","FU2_status"))
summary_res$tr <- factor(summary_res$tr, levels=c("Overall", "Control", "WSH", "Nutrition", "Nutrition + WSH"))
summary_res <- summary_res %>% arrange(tr)
summary_res




#Order data to replicate SL
d <- d[order(d$dataid,d$childNo, d$svy),]

d$tr <- as.character(d$tr)
d$tr[d$tr=="Nutrition"] <- "N"
d$tr[d$tr=="Nutrition + WSH"] <- "N + WSH"






#Create mother and child FU2 analysis datasets
dc <- d %>% filter(!is.na(cFU2) & cFU2!="Inconclusive")
dm <- d %>% filter(!is.na(mFU2) & mFU2!="Inconclusive")


#dataframe of EED biomarkers:
Yc <- dc %>% subset(., select=c(Lact,Mann,LM, neo,mpo,aat,reg1b))
Ym <- dm %>% subset(., select=c(Lact,Mann,LM, neo,mpo,aat,reg1b))
Y <- d %>% subset(., select=c(Lact,Mann,LM, neo,mpo,aat,reg1b))

#dataframe of growth:
Yc_cont <- dc %>% subset(., select=c(laz, whz, waz, hcz))
Ym_cont <- dm %>% subset(., select=c(laz, whz, waz, hcz))

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","N"), c("Control","N + WSH"), c("WSH","N + WSH"), c("N","N + WSH"))



#-------------------------------------------------------
# Analyze if age is an effect modifier
#-------------------------------------------------------

#Create empty matrix to hold the glm results:
EM_pval_TR<- EM_pval_anthro_C <- EM_pval_anthro_M <- EM_pval_EED_C <- EM_pval_EED_M <- NULL


#Unadjusted glm models -FU2 as an EM
for(i in 1:ncol(Y)){
  if(colnames(Y)[i]!="reg1b"){
  for(j in 1:5){

    temp <- washb_glm(Y=log(Y[,i]), tr=d$tr, W=data.frame(round=d$round), V="round", id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    #Get interaction p-val
    pvals<-temp$fit$`Pr(>|z|)`[5:6]
    temp <- data.frame(interaction="TR x Round", contrast= paste0(contrasts[[j]][1]," v ",contrasts[[j]][2]), Y=colnames(Y)[i], TRxRound2=pvals[1], TRxRound3=pvals[2])
    EM_pval_TR <- rbind(EM_pval_TR, temp)
  }
  }
}

for(i in 1:ncol(Yc)){
  if(colnames(Yc)[i]!="reg1b"){
    temp <- washb_glm(Y=log(Yc[,i]), tr=dc$cFU2, W=data.frame(round=dc$round), V="round", id=dc$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F)
    #Get interaction p-val
    pvals<-temp$fit$`Pr(>|z|)`[5:6]
    temp <- data.frame(interaction="Child FU2 x Round", contrast="", Y=colnames(Yc)[i], TRxRound2=pvals[1], TRxRound3=pvals[2])
    EM_pval_EED_C <- rbind(EM_pval_EED_C, temp)
  }
}

for(i in 1:ncol(Ym)){
  if(colnames(Ym)[i]!="reg1b"){
    temp <- washb_glm(Y=log(Ym[,i]), tr=dm$cFU2, W=data.frame(round=dm$round), V="round", id=dm$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F)
    #Get interaction p-val
    pvals<-temp$fit$`Pr(>|z|)`[5:6]
    temp <- data.frame(interaction="Mom FU2 x Round",contrast="", Y=colnames(Ym)[i], TRxRound2=pvals[1], TRxRound3=pvals[2])
    EM_pval_EED_M <- rbind(EM_pval_EED_M, temp)
  }
}



for(i in 1:ncol(Yc_cont)){
    temp <- washb_glm(Y=(Yc_cont[,i]), tr=dc$cFU2, W=data.frame(round=dc$round), V="round", id=dc$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F)
    #Get interaction p-val
    pvals<-temp$fit$`Pr(>|z|)`[5:6]
    temp <- data.frame(interaction="Child FU2 x Round", contrast="", Y=colnames(Yc_cont)[i], TRxRound2=pvals[1], TRxRound3=pvals[2])
    EM_pval_anthro_C <- rbind(EM_pval_anthro_C, temp)
  
}


for(i in 1:ncol(Ym_cont)){
    temp <- washb_glm(Y=(Ym_cont[,i]), tr=dm$cFU2, W=data.frame(round=dm$round), V="round", id=dm$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F)
    #Get interaction p-val
    pvals<-temp$fit$`Pr(>|z|)`[5:6]
    temp <- data.frame(interaction="Mom FU2 x Round",contrast="", Y=colnames(Ym_cont)[i], TRxRound2=pvals[1], TRxRound3=pvals[2])
    EM_pval_anthro_M <- rbind(EM_pval_anthro_M, temp)
}


#Check if the proportion of singificant interaction values are 
mean(c(EM_pval_TR$TRxRound2, EM_pval_TR$TRxRound3) < 0.05)
mean(c(EM_pval_EED_C$TRxRound2, EM_pval_EED_C$TRxRound3) < 0.05)
mean(c(EM_pval_EED_M$TRxRound2, EM_pval_EED_M$TRxRound3) < 0.05)
mean(c(EM_pval_anthro_C$TRxRound2, EM_pval_anthro_C$TRxRound3) < 0.05)
mean(c(EM_pval_anthro_M$TRxRound2, EM_pval_anthro_M$TRxRound3) < 0.05)

mean(c(EM_pval_TR$TRxRound2, EM_pval_TR$TRxRound3) < 0.2)
mean(c(EM_pval_EED_C$TRxRound2, EM_pval_EED_C$TRxRound3) < 0.2)
mean(c(EM_pval_EED_M$TRxRound2, EM_pval_EED_M$TRxRound3) < 0.2)
mean(c(EM_pval_anthro_C$TRxRound2, EM_pval_anthro_C$TRxRound3) < 0.2)
mean(c(EM_pval_anthro_M$TRxRound2, EM_pval_anthro_M$TRxRound3) < 0.2)

EMdat <- rbind(
  EM_pval_TR, EM_pval_EED_C, EM_pval_EED_M, EM_pval_anthro_C, EM_pval_anthro_M
)
mean(c(EMdat$TRxRound2, EMdat$TRxRound3) < 0.2)
mean(c(EMdat$TRxRound2, EMdat$TRxRound3) < 0.05)

knitr::kable(EMdat)



#-------------------------------------------------------
# FU2 as a risk factor
#-------------------------------------------------------



#Make vectors of adjustment variable names
Wvars<-c('tr', #Include treatment 
         'sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cows', 'n_goats', 'n_chickens')

#Clean adjustment variables 

#Set all factors as characters to allow level replacement
for(i in 1:ncol(d)){
  if(class(d[,i])=="factor"){
    d[,i] <- as.character(d[,i])
  }
}



#Clean covariates
d$sex<-as.factor(d$sex)
d$sex=relevel(d$sex,ref="0")





#Truncate unrealistic levels of n_chickens to 60
d$n_cows[is.na(d$n_cows)] <- 0
d$n_goats[is.na(d$n_goats)] <- 0
d$n_chickens[is.na(d$n_chickens)] <- 0

#Relevel all factors
d$tr <- factor(d$tr)

d$momedu=relevel(factor(d$momedu),ref="No education")
d$hfiacat=relevel(factor(d$hfiacat),ref="Food Secure")
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

    
#Calculate anthro monsoon season
d <- d %>% mutate(an_monsoon = ifelse(an_month > 4 & an_month < 11, "1", "0"),
                  an_monsoon = ifelse(is.na(an_month),"missing", an_monsoon),
                  an_monsoon = factor(an_monsoon))
    
#Re-subset d so new re-leveled factors are included
dc <- d %>% filter(!is.na(cFU2) & cFU2!="Inconclusive")
dm <- d %>% filter(!is.na(mFU2) & mFU2!="Inconclusive")

cW <- subset(dc, select=c(Wvars, "st_aged", "st_monsoon"))
mW<- subset(dm, select=c(Wvars, "st_aged", "st_monsoon"))

ur_cW<- subset(dc, select=c(Wvars, "ur_aged", "ur_monsoon"))
ur_mW<- subset(dm, select=c(Wvars, "ur_aged", "ur_monsoon"))

an_cW<- subset(dc, select=c(Wvars, "an_aged", "an_monsoon"))
an_mW<- subset(dm, select=c(Wvars, "an_aged", "an_monsoon"))



#Create lists of adjustment covariates
cWlist <- list(ur_cW,ur_cW,ur_cW,cW,cW,cW,cW)
mWlist <- list(ur_mW,ur_mW,ur_mW,mW,mW,mW,mW)




#-------------------------------------------------------
# Associations with EED outcomes
#-------------------------------------------------------

library=c("SL.glm")

#Set up SL library

#Create empty matrix to hold the glm results:
res_childFU2_RF_adj <- NULL
res_motherFU2_RF_adj <- NULL

#TMLE models -FU2 as a RF
for(i in  1:ncol(Yc)){
    #note the log transformation of the outcome prior to running TMLE model:
      set.seed(12345)
    temp<-washb_tmle(Y=log(Yc[,i]), tr=dc$cFU2, W=cWlist[[i]], id=dc$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_childFU2_RF_adj <- rbind(res_childFU2_RF_adj , temp) 
} 
rownames(res_childFU2_RF_adj)<-colnames(Yc)


for(i in  1:ncol(Ym)){
    #note the log transformation of the outcome prior to running TMLE model:
    set.seed(12345)
    temp<-washb_tmle(Y=log(Ym[,i]), tr=dm$mFU2, W=mWlist[[i]], id=dm$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=T, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_motherFU2_RF_adj <- rbind(res_motherFU2_RF_adj , temp) 
} 
rownames(res_motherFU2_RF_adj) <- colnames(Ym)
  


#-------------------------------------------------------
# Associations with growth outcomes
#-------------------------------------------------------


#dataframe of EED biomarkers:
Yc_cont <- dc %>% subset(., select=c(laz, whz, waz, hcz))
Ym_cont <- dm %>% subset(., select=c(laz, whz, waz, hcz))
Yc_bin <- dc %>% subset(., select=c( stunt, wast,
                                     sstunt, swast, 
                                     underwt, sunderwt))
Ym_bin <- dm %>% subset(., select=c( stunt, wast,
                                     sstunt, swast, 
                                     underwt, sunderwt))

    
     


#Create empty matrix to hold the glm results:
res_childFU2_growthRF_cont <- NULL
res_motherFU2_growthRF_cont <- NULL
res_childFU2_growthRF_bin <- NULL
res_motherFU2_growthRF_bin <- NULL

#TMLE models -continious outcomes 
for(i in  1:ncol(Yc_cont)){
    #note the log transformation of the outcome prior to running TMLE model:
        set.seed(12345)
    temp<-washb_tmle(Y=Yc_cont[,i], tr=dc$cFU2, W=an_cW, id=dc$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_childFU2_growthRF_cont <- rbind(res_childFU2_growthRF_cont , temp) 
} 
rownames(res_childFU2_growthRF_cont)<-colnames(Yc_cont)


for(i in  1:ncol(Ym_cont)){
    #note the log transformation of the outcome prior to running TMLE model:
        set.seed(12345)
    temp<-washb_tmle(Y=Ym_cont[,i], tr=dm$mFU2, W=an_mW, id=dm$block, pair=NULL, family="gaussian", contrast=c("Positive","Negative"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_motherFU2_growthRF_cont <- rbind(res_motherFU2_growthRF_cont , temp) 
} 
rownames(res_motherFU2_growthRF_cont) <- colnames(Ym_cont)
  
#TMLE models -binary outcomes 
for(i in  1:ncol(Yc_bin)){
    #note the log transformation of the outcome prior to running TMLE model:
        set.seed(12345)
    temp<-washb_tmle(Y=Yc_bin[,i], tr=dc$cFU2, W=an_cW, id=dc$block, pair=NULL, family="binomial", contrast=c("Positive","Negative"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$RR)))
    colnames(temp)<-c("RR","ci.l","ci.u", "Pvalue", "logRR","logRR_var")
    res_childFU2_growthRF_bin <- rbind(res_childFU2_growthRF_bin , temp) 
} 
rownames(res_childFU2_growthRF_bin)<-colnames(Yc_bin)


for(i in  1:ncol(Ym_bin)){
    #note the log transformation of the outcome prior to running TMLE model:
        set.seed(12345)
    temp<-washb_tmle(Y=Ym_bin[,i], tr=dm$mFU2, W=an_mW, id=dm$block, pair=NULL, family="binomial", contrast=c("Positive","Negative"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$RR)))
    colnames(temp)<-c("RR","ci.l","ci.u", "Pvalue", "logRR","logRR_var")
    res_motherFU2_growthRF_bin <- rbind(res_motherFU2_growthRF_bin , temp) 
} 
rownames(res_motherFU2_growthRF_bin) <- colnames(Ym_bin)
  





# Format results
rownames(res_childFU2_RF_adj) <- rownames(res_motherFU2_RF_adj) <- c("Lactulose",
                                   "Mannitol",
                                   "L/M ratio",
                                   "Neopterin",
                                   "Myeloperoxidase",
                                   "Alpha-1 antitrypsin",
                                   "Regenerating gene 1B" )


rownames(res_motherFU2_growthRF_bin) <- rownames(res_childFU2_growthRF_bin) <- c(
                                         "Stunted",
                                         "Wasted",
                                         "Severe stunted",
                                         "Severe wasted",
                                         "Underweight",
                                         "Severe underweight"
                                         )


rownames(res_motherFU2_growthRF_cont) <- rownames(res_childFU2_growthRF_cont) <- c(
                                         "LAZ", "WHZ", "WAZ", "HCZ"
                                         )



     pooled_childFU2_RF_adj <- res_childFU2_RF_adj
     pooled_motherFU2_RF_adj <- res_motherFU2_RF_adj
     pooled_childFU2_growthRF_cont <- res_childFU2_growthRF_cont
     pooled_motherFU2_growthRF_cont <- res_motherFU2_growthRF_cont 
     pooled_childFU2_growthRF_bin <- res_childFU2_growthRF_bin 
     pooled_motherFU2_growthRF_bin <- res_motherFU2_growthRF_bin 



# Save results
save(
     pooled_childFU2_RF_adj, pooled_motherFU2_RF_adj,
     pooled_childFU2_growthRF_cont, pooled_motherFU2_growthRF_cont,
     pooled_childFU2_growthRF_bin,pooled_motherFU2_growthRF_bin,
     file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_pooled_results.Rdata")






