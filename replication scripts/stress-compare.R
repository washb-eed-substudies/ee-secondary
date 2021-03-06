

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))
da <- readRDS(here::here("replication objects/audrie_stress_object.RDS"))
dm <- readRDS(here::here("replication objects/andrew_stress_object.RDS"))

a <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Audrie/washb-bangladesh-dm-ee-vital-saa-cortisol-f2-gcr.csv")

#check time-varying covariates
head(a)



table(d$monsoon3_oragene)
table(d$monsoon3_vital)
table(d$monsoon3_salimetrics)

summary(d$oragene_aged3)

#oragene is for gcr methylation, salimetrics for saa/cortisol, vitals for heart rate and blood pressure.
summary(d$vital_aged3)
summary(a$ageday_t3_vital)

summary(d$salimetrics_aged3)
summary(a$ageday_t3_salimetrics)

summary(d$oragene_aged3)
summary(a$ageday_t3_oragene)













da <- da %>% filter(tr %in% c("Control","Nutrition + WSH"))

head(da)

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")


summary(d$t2_f2_8ip)

summary(da$t2_f2_8ip)
summary(dm$t2_f2_8ip)
table(da$tr)
table(dm$tr)
table(da$block)
table(dm$block)

da %>% group_by(tr) %>% summarize(mean(t2_f2_8ip, na.rm=T))
dm %>% group_by(tr) %>% summarize(mean(t2_f2_8ip, na.rm=T))

table(da$tr, da$block)
table(dm$tr, dm$block)


  temp <- washb_tmle(Y=da$t2_f2_8ip   , tr=da$tr, pair=NULL, W=NULL, id=da$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  temp_metric




#Unadjusted glm models
  temp<-washb_tmle(Y=(dm$t2_f2_8ip  ), tr=dm$tr, W=NULL, id=dm$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T, seed=12345)
  temp2 <- washb_tmle(Y=da$t2_f2_8ip   , tr=da$tr, pair=NULL, W=NULL, id=da$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp3 <- washb_tmle(Y=d$t2_f2_8ip   , tr=d$tr, pair=NULL, W=NULL, id=d$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                      Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  unlist(temp$estimates$ATE)
  unlist(temp2$estimates$ATE)
  unlist(temp3$estimates$ATE)
  temp_metric
  
  
  
  
  temp <- washb_tmle(Y=da$t2_f2_8ip  , tr=da$tr, pair=NULL, W=NULL, id=da$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  temp_metric

  #Unadjusted glm models
  temp<-washb_tmle(Y=(dm$t2_f2_8ip ), tr=dm$tr, W=NULL, id=dm$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T, seed=12345)
  unlist(temp$estimates$ATE)
  temp_metric


  
  #Check covariates
  Wvars<-c('sex', 'birthord',
           'momage', 'momheight','momedu','hfiacat',
           'Nlt18','Ncomp','watmin',
           'walls', 'floor',
           'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
           'asset_khat', 'asset_chouki', 'asset_radio', 
           'asset_tv', 'asset_refrig', 'asset_bike',
           'asset_moto', 'asset_sewmach', 'asset_mobile',
           'n_cows', 'n_goats', 'n_chickens',
           "ur_agem2", "ur_monsoon2")
  
  
  
da <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-immun.csv")
da <- da %>% filter(tr %in% c("Control","Nutrition + WSH"))

for(i in Wvars){
  cat(i, "\n")
  print(summary(dm[!is.na(dm$t2_f2_VI_raw),i]))
}



Wvars<-c('sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cows', 'n_goats', 'n_chickens',
         "oragene_aged3", "monsoon3_oragene")



#Add in time varying covariates:
# Wvars2<-c("ur_agem2", "ur_monsoon2") 
# Wvars3_vital<-c("vital_aged3", "monsoon3_vital") 
# Wvars3_salimetrics<-c("salimetrics_aged3", "monsoon3_salimetrics") 
# Wvars3_oragene<-c("oragene_aged3", "monsoon3_oragene") 
for(i in Wvars){
  cat(i, "\n")
  print(summary(dm[!is.na(dm$t3_gcr_mean_raw),i]))
}
