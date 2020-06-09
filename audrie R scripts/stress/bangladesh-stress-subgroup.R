#---------------------------------------
# bangladesh-stress-subgroup.R
#
# audrie lin (audrielin@berkeley.edu)
#
# calculate subgroup analyses for stress function
#---------------------------------------

#---------------------------------------
# input files:
#	bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv
#
# output files:
#	bangladesh-stress-subgroup.Rdata
# 
# 
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
#rm(list=ls())
source(here::here("0-config.R"))


setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/")) #Set working directory

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
df = d[,c("block", "tr","t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso", "t3_pre_saa", "t3_pre_cort",
          "t3_post_saa", "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]

###################
#Subgroup Analyses @T2
###################


#Select adjustment covariates 
Wvars<-c("sex")

#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}

#Looks good. Use this if any need to be changes:

W$sex<-as.factor(W$sex)

# Set up the WASHB function
# df=data frame

# stratified by "sex"

washb_function <- function(df,x) {
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=df["sex"], id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  return(temp_metric)
}

#Create df t2 subset

df_t2 = d[,c("block", "tr", "sex", "t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso")]
df_t2$block=as.factor(df_t2$block)
#Subset females 
d_t2_f <- df_t2 %>%
  filter(sex == 0)

#Subset males
d_t2_m <- df_t2 %>%
  filter(sex == 1)

##Female 

list_stress_t2_f2 <- lapply(names(d_f)[grep('t2_', names(d_f))],  function(x) washb_function(d_f,x))

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function

list_stress_t2_f <- lapply(names(d_t2_f)[grep('t2_', names(d_t2_f))],  function(x) washb_function(d_t2_f,x))

list_stress_t2_f

#put names of each of the variables into the matrix
names(list_stress_t2_f) <- names(d_t2_f)[grep('t2_', names(d_t2_f))]

#resulting matrix
list_stress_t2_f

#to save each matrix separately for comparing with Andrew. 

t2_igf_subgroup_L_f<-list_stress_t2_f$t2_ln_igf

##Male 

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function


list_stress_m <- lapply(names(d_t2_m)[grep('t2_', names(d_t2_m))],  function(x) washb_function(d_t2_m,x))

list_stress_m

#put names of each of the variables into the matrix
names(list_stress_m) <- names(d_t2_m)[grep('t2_', names(d_t2_m))]

#resulting matrix
list_stress_m

#to save each matrix separately for comparing with Andrew. 

t2_igf_subgroup_L_m<-list_stress_m$t2_ln_igf


#---------------------------------------
# Load the analysis dataset,
# the baseline covariate dataset
#---------------------------------------

d <- readRDS(here("replication objects/simulated_stress_dataset.rds"))


#---------------------------------------
# subset to the relevant measurement
# T3
#---------------------------------------

dim(d)


# re-order the treatment factor for convenience, dropping the arms not included in stress
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))


# subset to columns needed for unadjusted 

#### Should this be same as df defined above for T2? #####
df = d[,c("block", "tr","t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso", "t3_pre_saa", "t3_pre_cort",
          "t3_post_saa", "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]


###################
#Subgroup Analyses @T3
###################


#Select adjustment covariates 
Wvars<-c("sex")

#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}

#Looks good. Use this if any need to be changes:


W$sex<-as.factor(W$sex)

# Set up the WASHB function
# df=data frame

# stratified by "sex"

washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=df["sex"], V="sex", id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, verbose=FALSE)
  
  temp_metric<-as.data.frame(temp$lincom)
  
  colnames(temp_metric) <-c("subgroup", "RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress <- lapply(names(d)[grep('t3_', names(d))],  function(x) washb_function(d,x))

list_stress

#put names of each of the variables into the matrix
names(list_stress) <- names(d)[grep('t3_', names(d))]

#resulting matrix
list_stress


#to save each matrix separately for comparing with Andrew. 

####These two seem to be immune (rather than stress) outcomes, but I am unsure how to ammend without the list_stress output

t3_igf_subgroup_L<-list_stress$t3_ln_igf


#Display results

t2_igf_subgroup_L


t3_igf_subgroup_L

#-----------------------------------
#save data
#-----------------------------------

save (t2_igf_subgroup_L,
  
      t3_igf_subgroup_L,file=here("audrie results/stress_subgroup.RData"))

