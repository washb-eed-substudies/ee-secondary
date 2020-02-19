

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset.RDS"))
da <- readRDS(here::here("replication objects/audrie_stress_object.RDS"))
dm <- readRDS(here::here("replication objects/andrew_stress_object.RDS"))

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

for(i in Wvars){
  cat(i, "\n")
  summary(dm[,i]) - summary(da[,i])
}
