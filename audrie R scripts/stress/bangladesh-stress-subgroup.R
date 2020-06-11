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

#####Unadjusted subgroup analyses at t2####

#Create df t2 subset

df_t2 = d[,c("block", "tr", "sex", "t2_ipf2a3", "t2_23dinor", "t2_ipf2a6", "t2_812iso")]
df_t2$block=as.factor(df_t2$block)


#Subset females 
df_t2_f <- df_t2 %>%
  filter(sex == 0)

#Subset males
df_t2_m <- df_t2 %>%
  filter(sex == 1)

##Female 
#grab the variables with prefix 't2_' from the data frame and then apply the washb_function

list_stress_t2_f2 <- lapply(names(df_t2_f)[grep('t2_', names(df_t2_f))],  function(x) washb_function(df_t2_f,x))

list_stress_t2_f

#Compile into data.frame for easier comparison in replication
subgroup_stress_t2_f <- t(bind_rows(list_stress_t2_f))
colnames(subgroup_stress_t2_f) <-c("RD","var","ci.lb","ci.ub","P-value")
subgroup_stress_t2_f <- as.data.frame(subgroup_stress_t2_f)


#View results
subgroup_stress_t2_f

##Male 

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function


list_stress_t2_m <- lapply(names(df_t2_m)[grep('t2_', names(df_t2_m))],  function(x) washb_function(df_t2_m,x))

list_stress_t2_m

#Compile into data.frame for easier comparison in replication
subgroup_stress_t2_m <- t(bind_rows(list_stress_t2_m))
colnames(subgroup_stress_t2_m) <-c("RD","var","ci.lb","ci.ub","P-value")
subgroup_stress_t2_m <- as.data.frame(subgroup_stress_t2_m)

#View results
subgroup_stress_t2_m
###################
#Subgroup Analyses @T3
###################

#####Unadjusted analyses at t3####

#Create df t3 subset

df_t3 = d[,c("block", "tr", "sex", "t3_pre_saa", "t3_pre_cort", "t3_post_saa", 
             "t3_post_cort", "t3_sys", "t3_dia", "t3_heart", "t3_nr3c1", "t3_cpg12")]
df_t3$block=as.factor(df_t3$block)


#Subset females 
df_t3_f <- df_t3 %>%
  filter(sex == 0)

#Subset males
df_t3_m <- df_t3 %>%
  filter(sex == 1)

###Female###

#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress_t3_f <- lapply(names(df_t3_f)[grep('t3_', names(df_t3_f))],  function(x) washb_function(df_t3_f,x))

list_stress_t3_f

#Compile into data.frame for easier comparison in replication
subgroup_stress_t3_f <- t(bind_rows(list_stress_t3_f))
colnames(subgroup_stress_t3_f) <-c("RD","var","ci.lb","ci.ub","P-value")
subgroup_stress_t3_f <- as.data.frame(subgroup_stress_t3_f)

#View results
subgroup_stress_t3_f

###Male###

#grab the variables with prefix 't3_' from the data frame and then apply the washb_function
list_stress_t3_m <- lapply(names(df_t3_m)[grep('t3_', names(df_t3_m))],  function(x) washb_function(df_t3_m,x))

list_stress_t3_m

#Compile into data.frame for easier comparison in replication
subgroup_stress_t3_m <- t(bind_rows(list_stress_t3_m))
colnames(subgroup_stress_t3_m) <-c("RD","var","ci.lb","ci.ub","P-value")
subgroup_stress_t3_m <- as.data.frame(subgroup_stress_t3_m)

#View results
subgroup_stress_t3_m
#-----------------------------------
#save data
#-----------------------------------

save (subgroup_stress_t2_f, subgroup_stress_t2_m,
      subgroup_stress_t3_f,  subgroup_stress_t3_m,
      file=here::here("audrie results/stress_subgroup.RData"))

