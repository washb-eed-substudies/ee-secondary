
rm(list=ls())
source(here::here("0-config.R"))

#load the fake dataset
# d <- readRDS("~/ee-secondary/replication objects/simulated_stress_dataset.rds")
# head(d)

# #load stress outcomes dataset
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

d$tr <- factor(d$tr, levels = c("Control","Nutrition","WSH","Nutrition + WSH"))






#------------------------------------------------------------------------------------------------
# Age stats
#------------------------------------------------------------------------------------------------


stress_age2_overall <- d %>% filter(!is.na(ur_agem2)) %>%
  summarize(tr="Overall",N=n(), mean=mean(ur_agem2), median=median(ur_agem2), sd=sd(ur_agem2), female=sum(sex==0), male=sum(sex))
stress_age2_tr <- d %>% group_by(tr) %>% filter(!is.na(ur_agem2)) %>%
  summarize(N=n(), mean=mean(ur_agem2), median=median(ur_agem2), sd=sd(ur_agem2), female=sum(sex==0), male=sum(sex))
stress_age_t2_M <- rbind(stress_age2_overall, stress_age2_tr)

stress_age3_overall <- d %>% filter(!is.na(ur_agem3)) %>%
  summarize(tr="Overall",N=n(), mean=mean(ur_agem3), median=median(ur_agem3), sd=sd(ur_agem3), female=sum(sex==0), male=sum(sex))
stress_age3_tr <- d %>% group_by(tr) %>% filter(!is.na(ur_agem3)) %>%
  summarize(N=n(), mean=mean(ur_agem3), median=median(ur_agem3), sd=sd(ur_agem3), female=sum(sex==0), male=sum(sex))
stress_age_t3_M <- rbind(stress_age3_overall, stress_age3_tr)


# #Compare to Audrie's
load(here::here("audrie results/stress-age-stats.RData"))
stress_age_t2_L[,-1] - stress_age_t2_M[,-1]
stress_age_t3_L[,-1] - stress_age_t3_M[,-1]


#------------------------------------------------------------------------------------------------
# N's and absolute means
#------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------
# N's and transformed means
#------------------------------------------------------------------------------------------------

raw_outcomes <- c("t2_f2_8ip_raw","t2_f2_23d_raw","t2_f2_VI_raw", "t2_f2_12i_raw",
              "t3_map","t3_hr_mean",
              "t3_saa_z01_raw","t3_saa_z02_raw","t3_cort_z01_raw","t3_cort_z03_raw",
              "t3_gcr_mean_raw","t3_gcr_cpg12_raw","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

absolute_mean_sd <- d %>% subset(., select=c(raw_outcomes)) %>% 
  summarise_all(tibble::lst(mean, sd), na.rm=T) %>% 
  gather() %>% as.data.frame()
n <-nrow(absolute_mean_sd)/2
#split mean and SD into different columns
absolute_mean_sd <- data.frame(Y=gsub("_mean","",absolute_mean_sd[1:n,1]), mean=absolute_mean_sd[1:n,2], sd=absolute_mean_sd[(n+1):(2*n),2]) 

#Mean and SD by treatment arm
absolute_mean_sd_tr <- d %>% group_by(tr) %>% subset(., select=c("tr",raw_outcomes))%>% 
  summarise_all(tibble::lst(mean, sd), na.rm=T) %>% 
  gather(.,  stat, measurement, t2_f2_8ip_raw_mean:t3_residual_cort_sd, factor_key=TRUE) %>%
  as.data.frame()

n <-nrow(absolute_mean_sd_tr)/2
#split mean and SD into different columns
absolute_mean_sd_tr <- data.frame(tr=absolute_mean_sd_tr[1:n,1], Y=gsub("_mean","",absolute_mean_sd_tr[1:n,2]), mean=absolute_mean_sd_tr[1:n,3], sd=absolute_mean_sd_tr[(n+1):(2*n),3]) 



outcomes <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
              "t3_map","t3_hr_mean",
              "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

mean_sd <- d %>% subset(., select=c(outcomes)) %>% 
  summarise_all(tibble::lst(mean, sd), na.rm=T) %>% 
  gather() %>% as.data.frame()
n <-nrow(mean_sd)/2
#split mean and SD into different columns
mean_sd <- data.frame(Y=gsub("_mean","",mean_sd[1:n,1]), mean=mean_sd[1:n,2], sd=mean_sd[(n+1):(2*n),2]) 

#Mean and SD by treatment arm
mean_sd_tr <- d %>% group_by(tr) %>% subset(., select=c("tr",outcomes))%>% 
  summarise_all(tibble::lst(mean, sd), na.rm=T) %>% 
  gather(.,  stat, measurement, t2_f2_8ip_mean:t3_residual_cort_sd, factor_key=TRUE) %>%
  as.data.frame()

n <-nrow(mean_sd_tr)/2
#split mean and SD into different columns
mean_sd_tr <- data.frame(tr=mean_sd_tr[1:n,1], Y=gsub("_mean","",mean_sd_tr[1:n,2]), mean=mean_sd_tr[1:n,3], sd=mean_sd_tr[(n+1):(2*n),3]) 


# #Compare to Audrie's
 load(here::here("audrie results/stress_N_means.RData"))
 ls()
#
 aud_N <- as.data.frame(rbindlist(lapply(ls(pattern="_N_L"), get)))
 aud_N$Y = gsub("_N_L","",ls(pattern="_N_L"))
#
# #merge and compare
 N_comp <- merge(aud_N, mean_sd, by="Y")
 dim(N_comp)
 N_comp$mean.diff <- N_comp$mean.x - N_comp$mean.y
 N_comp$sd.diff <- N_comp$sd.x - N_comp$sd.y
 max(N_comp$mean.diff)
 max(N_comp$sd.diff)


#------------------------------------------------------------------------------------------------
# Unadjusted tmle
#------------------------------------------------------------------------------------------------


#dataframe of stress outcomes:
colnames(d)


#Unadjusted glm models
res_unadj <- NULL
for(i in outcomes){
  temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  res_unadj<-rbind(res_unadj, unlist(temp$estimates$ATE))
}
res_unadj <- as.data.frame(res_unadj)
res_unadj

colnames(res_unadj)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_unadj$Y <- outcomes
res_unadj

# #Compare to Audrie's objects
load(here::here("audrie results/stress_unadj_glm.RData"))

name.pattern="unadj_"
object_list=ls(pattern=name.pattern)
aud_unadj <- rbind(get(object_list[1]), get(object_list[2]))
names(aud_unadj)[names(aud_unadj) == 'var'] <- "Y"

#
dim(res_unadj)
dim(aud_unadj)
comp_unadj <- full_join(res_unadj, aud_unadj, by="Y")
dim(comp_unadj)
#
comp_unadj$`Mean difference` - comp_unadj$RD
comp_unadj$`P-value` - comp_unadj$Pval

saveRDS(d, here::here("replication objects/andrew_stress_object.RDS"))



#------------------------------------------------------------------------------------------------
# Age and sex adjusted GLMs
#------------------------------------------------------------------------------------------------

d$sex<-as.factor(d$sex)
d$sex=relevel(d$sex,ref="0")

#Age and sex adjusted glm models
res_sex <- NULL
for(i in outcomes){
  if(grepl("t2_", outcomes[i])){
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, agem2=d$ur_agem2), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)
  }else{
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, agem2=d$ur_agem2), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)
  }
  res_sex<-rbind(res_sex, unlist(temp$estimates$ATE))
}
res_sex <- as.data.frame(res_sex)

colnames(res_sex)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_sex$Y <- outcomes




# #Compare to Audrie's objects
# load(here("audrie results/immune_adj_sex_age_glm.RData"))
# 
# aud_sex <- as.data.frame(rbindlist(lapply(lapply(ls(pattern="_adj_sex_age_L"), get), as.data.frame)))
# aud_sex$Y = gsub("_adj_sex_age_L","",ls(pattern="_adj_sex_age_L"))
# 
# dim(res_sex)
# dim(aud_sex)
# comp_sex <- full_join(res_sex, aud_sex, by="Y")
# dim(comp_sex)
# 
# comp_sex$RD.x - comp_sex$RD.y


#------------------
# Clean adjustment variables
# Note: no need ot change this code
#------------------

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
d <- d %>% mutate(monsoon2 = ifelse(month2 > 4 & month2 < 11, "1", "0"),
                  monsoon3 = ifelse(month3 > 4 & month3 < 11, "1", "0"),
                  monsoon2 = ifelse(is.na(month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(month3),"missing", monsoon3),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))

Wvars2<-c("ur_agem2", "ur_monsoon2") 
Wvars3<-c("ur_agem3", "ur_monsoon3") 


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

d$asset_clock <- as.character(d$asset_clock)
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
d$sex<-as.factor(d$sex)
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
d$asset_clock<-factor(d$asset_clock)
d$asset_clock=relevel(d$asset_clock,ref="No clock")
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
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))






##############################################
#Run GLMs for the adjusted parameter estimates
##############################################

#Fully adjusted glm models
res_adj <- NULL
for(i in outcomes){
  if(grepl("t2_", outcomes[i])){
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  }else{
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  }
  res_adj<-rbind(res_adj, unlist(temp$estimates$ATE))
}
res_adj <- as.data.frame(res_adj)

colnames(res_adj)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_adj$Y <- outcomes





# #Compare to Audrie's objects
# load(here("audrie results/immune_adj_glm.RData"))
# name.pattern="_adj_L"
# object_list=ls(pattern=name.pattern)
# aud_adj <- load_aud(name.pattern, object_list)
# 
# 
# dim(res_adj)
# dim(aud_adj)
# comp_adj <- full_join(res_adj, aud_adj, by="Y")
# dim(comp_adj)
# comp_adj$RD.x - comp_adj$RD.y
# comp_adj$Pval - comp_adj$P.value

#Save intermediate R objects for replication comparison
dm <- d
save(res_adj, W, W2, W3, dm,  file = here("replication objects/lisa_stress_W.rdata"))



##############################################
#Run GLMs for the sex-stratified subgroup analysis
##############################################

#sex stratified glm models
res_sub <- NULL
for(i in outcomes){
  temp<-washb_glm(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex), V="sex", id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)
  res_sub<-rbind(res_sub, temp$lincom)
}
res_sub <- as.data.frame(res_sub)

colnames(res_sub)<-c("sex","RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
res_sub$Y <-rep(outcomes, each=2)
res_sub <- res_sub %>% mutate(subgroup = case_when(sex==1 ~ "male", sex==0 ~ "female", TRUE~""), subgroup=factor(subgroup))

# #Compare to Audrie's objects
# load(here("audrie results/immune_subgroup.RData"))
# 
# name.pattern="_subgroup_L"
# object_list=ls(pattern=name.pattern)
# aud_sub <- load_aud(name.pattern, object_list, subgroup = T)
# 
# dim(res_sub)
# dim(aud_sub)
# comp_sub <- full_join(res_sub, aud_sub, by=c("Y","subgroup"))
# dim(comp_sub)
# 
# comp_sub <- filter(comp_sub, !is.na(RD.x))
# 
# comp_sub$RD.x - comp_sub$RD.y



##############################################
#Run GLMs for the sex-stratified subgroup analysis
##############################################

#save results
save(stress_age_t2_M, stress_age_t3_M, mean_sd, mean_sd_tr, absolute_mean_sd, absolute_mean_sd_tr, res_unadj, res_sex, res_adj, res_sub, file=here::here("andrew results/stress_results.Rdata"))
