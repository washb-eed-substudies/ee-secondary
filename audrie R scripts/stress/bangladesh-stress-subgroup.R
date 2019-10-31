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

d <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))


#---------------------------------------
# subset to the relevant measurement
# T2 
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
  
  temp <- washb_tmle(Y=df[,x], tr=df$tr, pair=NULL, W=df["sex"], V="sex", id=df$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", 
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, verbose=FALSE)
  
  temp_metric<-as.data.frame(temp$lincom)

  colnames(temp_metric) <-c("subgroup", "RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('t2_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('t2_', names(d))]

#resulting matrix
list_immune

#to save each matrix separately for comparing with Andrew. 



t2_igf_subgroup_L<-list_immune$t2_ln_igf



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
df = d[,c("block", "tr","[insert list of stress variables here]")]
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
list_immune <- lapply(names(d)[grep('t3_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('t3_', names(d))]

#resulting matrix
list_immune


#to save each matrix separately for comparing with Andrew. 
t3_igf_subgroup_L<-list_immune$t3_ln_igf


#Display results

t2_igf_subgroup_L


t3_igf_subgroup_L

#-----------------------------------
#save data
#-----------------------------------

save (t2_igf_subgroup_L,
  
      t3_igf_subgroup_L,file=here("audrie results/stress_subgroup.RData"))

