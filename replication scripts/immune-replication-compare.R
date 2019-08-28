


rm(list=ls())
source(here::here("0-config.R"))

load(here("replication objects/audrie_immune_W.rdata"))
load(here("replication objects/andrew_immune_W.rdata"))
load(here("replication objects/andrew_immune_W3.rdata"))

# da
# wa
# list_immune
# 
# res_adj
# W
# W2
# W3
# dm




for(i in 1:ncol(W)){
  cat(colnames(W)[i], "\n")
  print(mean(as.numeric(W[,i]),na.rm=T))
}
for(i in 1:ncol(W2)){
  cat(colnames(W2)[i], "\n")
  print(mean(as.numeric(W2[,i]),na.rm=T))
}

mean(W$momheight,na.rm=T)
mean(W2$momheight,na.rm=T)
mean(W$momage,na.rm=T)
mean(W2$momage,na.rm=T)


list_immune$t2_ln_igf
res_adj[res_adj$Y=="igf_t2",]
 

head(W2)
head(wa)


d1=dm
d2=da
var1="n_chickens"
var2="n_chicken"
subset1=NULL
subset2=NULL

covariate_compare <- function(d1, d2, var1, var2, subset1=NULL, subset2=NULL){
  if(is.null(subset1)){
    d1 <- d1 %>% filter(!is.na(subset1))
  }
  if(is.null(subset2)){
    d2 <- d2 %>% filter(!is.na(subset2))
  }
  d1 <- d1 %>% subset(., select = c("childid",var1))
  d2 <- d2 %>% subset(., select = c("childid",var2))
  
}


washb_function <- function(d,x, Wvars, logtrans=F, print=F) {
  if(logtrans){d[,x]=log(d[,x])}
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=Wvars, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=print)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

summary(da$igf_t2)
summary(dm$igf_t2)

summary(log(da$igf_t2))
summary(log(dm$igf_t2))


washb_function(d=da, x="igf_t2", Wvars=wa, logtrans=T)
washb_function(d=dm, x="igf_t2", Wvars=W2)


washb_function(d=da, x="igf_t2", Wvars=NULL)
washb_function(d=dm, x="igf_t2", Wvars=NULL)
washb_function(d=da, x="t2_ln_igf", Wvars=NULL)


table(da$tr)
table(dm$tr)

mean(da$igf_t2, na.rm=T)
mean(dm$igf_t2, na.rm=T)

mean(log(da$igf_t2), na.rm=T)
dm$ln_igf_t2 <- log(dm$igf_t2)


mean(da$igf_t2, na.rm=T)
mean(dm$ln_igf_t2, na.rm=T)
mean(log(da$igf_t2), na.rm=T)
mean(log(dm$igf_t2), na.rm=T)

summary(dm$igf_t2)
summary(da$t2_ln_igf)

summary(dm$igf_t3)
summary(da$t3_ln_igf)


res_a <- washb_function(da, "t3_ln_igf", wa)
res_m <- washb_function(dm, "igf_t3", W2)
res_a
res_m

res_a <- washb_function(da, "t3_ln_igf", Wa3)
res_m <- washb_function(dm, "igf_t3", W3)
res_a
res_m


# Compare T3 covariates
colnames(Wa3)
colnames(W3)

#Our monsoons are off
summary(wa$monsoon_bt2)
summary(W2$monsoon2)

summary(Wa3$monsoon_bt3)
summary(W3$monsoon3)

#Our ages are off at both times
summary(wa$ageday_bt2)
summary(W2$aged2)

summary(Wa3$ageday_bt3)
summary(W3$aged3)


summary(Wa3$sex)
summary(W3$sex)
summary(Wa3$birthord)
summary(W3$birthord)
summary(Wa3$hfiacat)
summary(W3$hfiacat)

res_a <- washb_function(da, "t3_ln_igf", Wa3)
res_m <- washb_function(dm, "igf_t3", W3)
res_a
res_m



# Compare child differences
dim(dm)
dim(da)
d <- full_join(dm, da, by = c("childid"))
dim(d)

d$childid[as.numeric(d$monsoon2)!=as.numeric(d$monsoon_bt2)]
d$monsoon2[as.numeric(d$monsoon2)!=as.numeric(d$monsoon_bt2)]
d$monsoon_bt2[as.numeric(d$monsoon2)!=as.numeric(d$monsoon_bt2)]


# da %>% group_by(tr) %>% summarise(N=n(), mean(as.numeric(factor(sex))), mean(igf_t2, na.rm=T))
# d %>% group_by(tr) %>% summarise(N=n(), mean(sex), mean(igf_t2, na.rm=T))
# 
# da <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv")
# table(is.na(da$agemth_bt1))
# table(is.na(da$agemth_bt2))
# table(is.na(da$agemth_bt3))
# 
# mean(da$agemth_bt2, na.rm=T)
# 
# table(d$mons)
# 
# 
# #load immune outcomes
# imm<-read_dta("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/immune/washb-bangladesh-immun-lab-t2-t3.dta")
# imm <- as.data.frame(imm)
# imm$childid <- as.numeric(imm$childid)
# head(imm)
# 
# #log transform outcomes
# table(is.na(imm[,-1]))
# imm[,-1] <- log(imm[,-1])
# table(is.na(imm[,-1]))
# 
# fulld <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/EE-BD_fulldata.csv")
# colnames(fulld)
# 
# #Subset to needed variables
# 
# #Merge in immune outcomes
# dim(imm)
# dim(fulld)
# fulld$childid <- as.numeric(fulld$childid)
# d <- semi_join(imm, fulld, by="childid")
# dim(d)
# 
# table(is.na(d$sex))
# 
# 
# 
# 
# #Covariate comparison
# Wvars<-c("monsoon_bt2","ageday_bt2", "sex","birthord", "momage","momheight","momedu", 
#          "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", 
#          "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", 
#          "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", 
#          "n_cattle", "n_goat", "n_chicken")
# 
# 
# #Monsoon is off for the two of us
# table(W2$monsoon2)
# table(da$monsoon_bt2)
# 
# #I'm missing one age
# summary(W2$aged2)
# summary(da$ageday_bt2)
# 
# # table(W2$sex)
# # table(da$sex)
# 
# # table(W2$birthord)
# # table(da$birthord)
# 
# # summary(W2$momage)
# # summary(da$momage)
# 
# # summary(W2$momheight)
# # summary(da$momheight)
# 
# # summary(W2$momedu)
# # summary(da$momedu)
# 
# # summary(W2$momedu)
# # summary(da$momedu)
# 
# # summary(W2$Nlt18)
# # summary(da$Nlt18)
# 
# # summary(W2$Ncomp)
# # summary(da$Ncomp)
# 
# # summary(W2$watmin)
# # summary(da$watmin)
# 
# wvars <- c("hfiacat", "walls", "floor", "elec", "asset_wardrobe", 
# "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", 
# "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile")
# 
# for(i in 1:length(wvars)){
#   print(wvars[i])
#   print(table(W2[,wvars[i]]))
#   print(table(da[,wvars[i]]))
# }
# 
# #All matching
# 
# 
# #Missing 7 obs
# # summary(W2$n_cows)
# # summary(da$n_cattle)
# 
# #Missing 7 obs
# # summary(W2$n_goats)
# # summary(da$n_goat)
# 
# #Missing 7 obs
# # summary(W2$n_chickens)
# # summary(da$n_chicken)
# 
# # miss_ids <- d$childid[is.na(d$n_chickens)]
# # miss_ids
# # 
# # da$n_cattle[da$childid %in% miss_ids]
# # da$n_goat[da$childid %in% miss_ids]
# # da$n_chicken[da$childid %in% miss_ids]
# #Audrie's assets are numeric rather than factors