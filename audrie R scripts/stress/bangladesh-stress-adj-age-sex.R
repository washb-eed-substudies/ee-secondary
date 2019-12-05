#---------------------------------------
# bangladesh-stress-adj-age-sex.R
#
# audrie lin (audrielin@berkeley.edu)
#
# 
# calculate adjusted age sex differences
# between treatment arms for stress assays
#---------------------------------------

#---------------------------------------
# 
#	input files:
# replication objects/simulated_stress_dataset.rds
#
#
# output files:
#	bangladesh-stress-adj-age-sex.Rdata
# 
# 
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
#rm(list=ls())
source(here::here("0-config.R"))

setwd(paste0(dropboxDir,"Data/Cleaned/Audrie/")) #Set working directory

#t2

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
d$sex <- factor(d$sex, labels = c("female", "male"))


# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]

#---------------------------------------
# Select covariates with univariate
# associations with the outcome of
# P<0.2 based on a liklihood ratio test
#---------------------------------------

###################
#Adjusted GLM analysis
###################


#Select adjustment covariates 
Wvars<-c("sex", "ageday_bt2")


#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#if already a factor
W$ageday_bt2<-as.numeric(W$ageday_bt2)
W$sex<-as.factor(W$sex)



# df=data frame
# Set up the WASHB function
washb_function <- function(df,x) {
  
  temp <- washb_tmle(Y=df[,x], tr=d$tr, pair=NULL, W=W, id=df$block, contrast= c("Control","Nutrition + WSH"), family="gaussian",
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  return(temp_metric)
  
  #temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=W, id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
   #                  Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  #temp_metric <-as.matrix(temp$TR)
  #rownames(temp_metric) <- c("Nutrition + WSH v C")
  #colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  #return(temp_metric)
}

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_stress <- lapply(names(d)[grep('t2_', names(d))],  function(x) washb_function(d,x))

list_stress

#put names of each of the variables into the matrix
names(list_stress) <- names(d)[grep('t2_', names(d))]

#resulting matrix
list_stress

#to save each matrix separately for comparing with Andrew. 

t2_igf_adj_sex_age_L<-list_stress$t2_ln_igf




#t3

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
df = d[,c("block", "tr","[insert list of stress variables here]")]
df$block=as.factor(df$block)

# Set up the WASHB function
# df=data frame

#trlist=c("Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")


# ensure that variables are coded as factors
d$sex <- factor(d$sex, labels = c("female", "male"))


# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]

#---------------------------------------
# Select covariates with univariate
# associations with the outcome of
# P<0.2 based on a liklihood ratio test
#---------------------------------------

###################
#Adjusted GLM analysis
###################


#Select adjustment covariates 
Wvars<-c("sex", "ageday_bt3")


#subset the main dataframe and create a new W dataframe
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  cat(colnames(W)[i],"  ",class(W[,i]),"\n")
}


#if already a factor
W$ageday_bt3<-as.numeric(W$ageday_bt3)
W$sex<-as.factor(W$sex)

# df=data frame
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

#resulting matrix
list_stress

#to save each matrix separately for comparing with Andrew. 
t3_igf_adj_sex_age_L<-list_stress$t3_ln_igf


#display results

t2_igf_adj_sex_age_L

t3_igf_adj_sex_age_L





#-----------------------------------
#save data
#-----------------------------------


save (t2_igf_adj_sex_age_L,
     
      
      t3_igf_adj_sex_age_L,
      
      
     file=here("audrie results/stress_adj_sex_age_glm.RData"))


