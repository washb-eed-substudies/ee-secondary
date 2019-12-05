#---------------------------------------
# bangladesh-stress-adj.R
#
# audrie lin (audrielin@berkeley.edu)
#
# calculate adjusted differences
# between treatment arms for stress assays
#---------------------------------------

#---------------------------------------
# input files:
# replication objects/simulated_stress_dataset.rds
#
# output files:
#	bangladesh-stress-adj.Rdata
# 
# 
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
#rm(list=ls())
source(here::here("0-config.R"))

setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/")) #Set working directory


##########################
#   Adjusted glm @t2 
##########################

#---------------------------------------
# Load the analysis dataset,
# the baseline covariate dataset
#---------------------------------------

d <- readRDS(here("replication objects/simulated_stress_dataset.rds"))


#---------------------------------------
# subset to the relevant measurement
# T2 
#---------------------------------------

dim(d)


# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 
df = d[,c("block", "tr", "t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso", "t3_pre_saa", "t3_pre_cort",
          "t3_post_saa", "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")


# ensure that variables are coded as factors
d$monsoon_bt2 <- factor(d$monsoon_bt2)
d$sex <- factor(d$sex, labels = c("female", "male"))


# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]

#---------------------------------------
# Select covariates with univariate
# associations with the outcome of
# P<0.2 based on a liklihood ratio test
#---------------------------------------


# drop due to so many missing values
# asset_clock
# birthord
###################
#Adjusted GLM analysis
###################


#Select adjustment covariates 
Wvars<-c("monsoon_bt2","ageday_bt2", "sex","birthord", "momage","momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}

#Replace missingness in time varying covariates as a new level
#W1$month1[is.na(W1$month1)]<-"missing"



#Looks good. Use this if any need to be changes:

W$monsoon_bt2<-as.factor(W$monsoon_bt2)
#If already a factor:
d$monsoon_bt2<-addNA(d$monsoon_bt2)
levels(d$monsoon_bt2)[length(levels(d$monsoon_bt2))]<-"Missing"


W$momage<-as.numeric(W$momage)
W$ageday_bt2<-as.numeric(W$ageday_bt2)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)
W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)

# Set up the WASHB function
washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=W, id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#washb_function(d, "igf_t2")

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_stress <- lapply(names(d)[grep('t2_', names(d))],  function(x) washb_function(d,x))

list_stress

#put names of each of the variables into the matrix
names(list_stress) <- names(d)[grep('t2_', names(d))]

#Compile into data.frame for easier comparison in replication
adj_stress_t2 <- t(bind_rows(list_stress))
colnames(adj_stress_t2) <-c("RD","var","ci.lb","ci.ub","P-value")
adj_stress_t2 <- as.data.frame(adj_stress_t2)
adj_stress_t2$var <- c("t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso")

#view results file
adj_stress_t2

#Save intermediate R objects for replication comparison
da <- d
wa <-W
save(da, wa, list_stress, file = here("replication objects/audrie_stress_W.rdata"))


#to save each matrix separately for comparing with Andrew. 
t2_igf_adj_L<-list_stress$t2_ln_igf



#---------------------------------------
# Load the analysis dataset,
# the baseline covariate dataset
#---------------------------------------

d <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))

#---------------------------------------
# subset to the relevant measurement
# T3
#---------------------------------------

dim(d)



# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 
df = d[,c("block", "tr", "t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso", "t3_pre_saa", "t3_pre_cort",
          "t3_post_saa", "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

# ensure that variables are coded as factors
d$monsoon_bt3 <- factor(d$monsoon_bt3)
d$sex <- factor(d$sex, labels = c("female", "male"))


# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]

#---------------------------------------
# Select covariates with univariate
# associations with the outcome of
# P<0.2 based on a liklihood ratio test
#---------------------------------------


# drop due to so many missing values
# asset_clock
# birthord
###################
#Adjusted GLM analysis
###################


#Select adjustment covariates 
Wvars<-c("monsoon_bt3","ageday_bt3", "sex","birthord", "momage","momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}

#Replace missingness in time varying covariates as a new level
#W1$month1[is.na(W1$month1)]<-"missing"



#Looks good. Use this if any need to be changes:

W$monsoon_bt3<-as.factor(W$monsoon_bt3)
#If already a factor:
d$monsoon_bt3<-addNA(d$monsoon_bt3)
levels(d$monsoon_bt3)[length(levels(d$monsoon_bt3))]<-"Missing"


W$momage<-as.numeric(W$momage)
W$ageday_bt3<-as.numeric(W$ageday_bt3)
W$momheight<-as.numeric(W$momheight)
W$sex<-as.factor(W$sex)
W$birthord<-as.factor(W$birthord)
W$momedu<-as.factor(W$momedu)
W$hfiacat<-as.factor(W$hfiacat)
W$Nlt18<-as.numeric(W$Nlt18)
W$Ncomp<-as.numeric(W$Ncomp)
W$watmin<-as.numeric(W$watmin)
W$walls<-as.factor(W$walls)
W$floor<-as.factor(W$floor)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$n_cattle<-as.numeric(W$n_cattle)
W$n_goat<-as.numeric(W$n_goat)
W$n_chicken<-as.numeric(W$n_chicken)


# Save t3 covariates
Wa3 <- W
save(Wa3, file=here("replication objects/andrew_stress_W3.rdata"))


# Set up the WASHB function
washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=W, id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}


#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress <- lapply(names(d)[grep('t3_', names(d))],  function(x) washb_function(d,x))

list_stress

#put names of each of the variables into the matrix
names(list_stress) <- names(d)[grep('t3_', names(d))]

#Compile into data.frame for easier comparison in replication
adj_stress_t3 <- t(bind_rows(list_stress))
colnames(adj_stress_t3) <-c("RD","var","ci.lb","ci.ub","P-value")
adj_stress_t3 <- as.data.frame(adj_stress_t3)
adj_stress_t3$var <- c("t3_pre_saa", "t3_pre_cort", "t3_post_saa", "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")

#view results file
adj_stress_t3

#resulting matrix
list_stress


#to save each matrix separately for comparing with Andrew. 
t3_igf_adj_L<-list_stress$t3_ln_igf





#Display results




t2_igf_adj_L


t3_igf_adj_L


#-----------------------------------
#save data
#-----------------------------------

save (t2_igf_adj_L,

      t3_igf_adj_L,
       file=here("audrie results/stress_adj_glm.RData"))

# example function
#sample_mean_fun <- function(df, x) {
#  result_sample <- mean(df[,x], na.rm=T)
#  return(result_sample)
#}


# WASHB function

#washb_function <- function(df,x) {
  
#  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=W, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
#  temp_metric <-as.matrix(temp$TR)
#  rownames(temp_metric) <- c("Nutrition + WSH v C")
#  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
#  return(temp_metric)
#}

#list_example <- lapply(names(d)[grep('ln_', names(d))],  function(x) washb_function(d,x))


#names(list_example) <- names(d)[grep('ln_', names(d))]

#list_example

#to save each matrix separately for comparing with Andrew.
#ln_tnf3 <- list_example$ln_tnf3
#ln_tnf3

 # unlist does not work
#unlist_mylist_function <- function(list,name) {
#  name <- list$name
#}

