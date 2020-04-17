###Load in data
rm(list=ls())
source(here::here("0-config.R"))

# #load full treatments
load(paste0(dropboxDir,"/Data/Untouched/washb-bangladesh-tr (real).Rdata"))

treatment <- d
treatment$clusterid<-as.numeric(treatment$clusterid)
rm(d)

#Load EED 
ipcw <- read.csv(paste0(dropboxDir,"/Data/Cleaned/Andrew/BD-EE-ipcw.csv"), stringsAsFactors = T) %>% select(-c(tr,block))

#Load in stress analysis dataset
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_IPCW_dataset_andrew.RDS"))





#Keep only stress outcomes and time-varying covariates, drop the baseline covariates
colnames(d)
d <- d %>% 
  subset(., select=c(
    childid, dataid, childNo, clusterid,
    ur_aged2, ur_monsoon2, 
    vital_aged3, monsoon3_vital,
    salimetrics_aged3, monsoon3_salimetrics, 
    oragene_aged3, monsoon3_oragene,
    t2_f2_8ip,t2_f2_23d,t2_f2_VI, t2_f2_12i,
    t3_map,t3_hr_mean,
    t3_saa_z01,t3_saa_z02,t3_cort_z01,t3_cort_z03,
    t3_gcr_mean,t3_gcr_cpg12,t3_saa_slope,t3_cort_slope,t3_residual_saa,t3_residual_cort
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
d$ur_monsoon2 <- as.character(d$ur_monsoon2)
d$ur_monsoon2[is.na(d$ur_monsoon2)] <- "missing"
d$monsoon3_vital <- as.character(d$monsoon3_vital)
d$monsoon3_vital[is.na(d$monsoon3_vital)] <- "missing"
d$monsoon3_salimetrics <- as.character(d$monsoon3_salimetrics)
d$monsoon3_salimetrics[is.na(d$monsoon3_salimetrics)] <- "missing"
d$monsoon3_oragene <- as.character(d$monsoon3_oragene)
d$monsoon3_oragene[is.na(d$monsoon3_oragene)] <- "missing"

#calculate overall medians/modes:
ur_aged2_median <-    median(d$ur_aged2, na.rm = T)
vital_aged3_median <-    median(d$vital_aged3, na.rm = T)
salimetrics_aged3_median <-    median(d$salimetrics_aged3, na.rm = T)
oragene_aged3_median <-    median(d$oragene_aged3, na.rm = T)


#impute child age with overall median
d$ur_aged2[is.na(d$ur_aged2)] <-  ur_aged2_median
d$vital_aged3[is.na(d$vital_aged3)] <-  vital_aged3_median
d$salimetrics_aged3[is.na(d$salimetrics_aged3)] <-  salimetrics_aged3_median
d$oragene_aged3[is.na(d$oragene_aged3)] <-  oragene_aged3_median


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



#Add in time varying covariates:
Wvars2<-c("ur_aged2", "ur_monsoon2") 
Wvars3_vital<-c("vital_aged3", "monsoon3_vital") 
Wvars3_salimetrics<-c("salimetrics_aged3", "monsoon3_salimetrics") 
Wvars3_oragene<-c("oragene_aged3", "monsoon3_oragene") 



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
#Add in time-varying covariates
W2<- cbind(W, subset(d, select=Wvars2))
W3_vital<- cbind(W, subset(d, select=Wvars3_vital))
W3_salimetrics<- cbind(W, subset(d, select=Wvars3_salimetrics))
W3_oragene<- cbind(W, subset(d, select=Wvars3_oragene))




#Create indicators for missingness
outcomes <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
              "t3_map","t3_hr_mean",
              "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

Y <- d %>% select(all_of(outcomes)) 


missingness_list <- list()
for(i in 1:ncol(Y)){
  missingness_list[[i]] <- ifelse(is.na(Y[,i]),0,1)
  Y[is.na(Y[,i]),i] <- 9
  names(missingness_list)[i] <- paste0(colnames(Y)[i],".miss")
}
miss <- as.data.frame(bind_rows(missingness_list))
d <- cbind(d, miss)






#Run the adjusted ipcw analysis
res_ipcw <- NULL
for(i in outcomes){
  if(grepl("t2_", i)){
    temp<-washb_tmle(Y=(d[,i]), Delta=miss[,paste0(i,".miss")], tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  }else{
    if(i %in% c("t3_map","t3_hr_mean" )){
      temp<-washb_tmle(Y=(d[,i]), Delta=miss[,paste0(i,".miss")], tr=d$tr, W=W3_vital, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
    if(i %in% c("t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03","t3_saa_slope","t3_cort_slope","t3_residual_saa",  "t3_residual_cort")){
      temp<-washb_tmle(Y=(d[,i]), Delta=miss[,paste0(i,".miss")], tr=d$tr, W=W3_salimetrics, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
    if(i %in% c("t3_gcr_mean", "t3_gcr_cpg12")){
      temp<-washb_tmle(Y=(d[,i]), Delta=miss[,paste0(i,".miss")], tr=d$tr, W=W3_oragene, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
  }
  res_ipcw<-rbind(res_ipcw, unlist(temp$estimates$ATE))
}
res_ipcw <- as.data.frame(res_ipcw)


colnames(res_ipcw)<-c("RD","var.psi","ci.l","ci.u", "Pval")
res_ipcw$Y <-colnames(Y)




save(res_ipcw,  file = here::here("andrew results/andrew_stress_ipcw.rdata"))









