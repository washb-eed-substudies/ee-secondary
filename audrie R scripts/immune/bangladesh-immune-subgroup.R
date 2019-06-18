#---------------------------------------
# bangladesh-immune-subgroup.R
#
# audrie lin (audrielin@berkeley.edu)
#
# calculate subgroup analyses for immune function
#---------------------------------------

#---------------------------------------
# input files:
#	bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv
#
# output files:
#	bangladesh-immune-subgroup.Rdata
# 
# 
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(tmle)
library(SuperLearner)

library(devtools)
library(foreign) #Run each time R is started up to load the package into working memory
library(washb) 
library(dplyr)

setwd("~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/") #Set working directory

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

# Exclude children with missing data (none)

#table(is.na(d$t2_ln_igf))
#table(is.na(d$t2_ln_agp))
#table(is.na(d$t2_ln_crp))
#table(is.na(d$t2_ln_gmc))
#table(is.na(d$t2_ln_ifn))
#table(is.na(d$t2_ln_il10))
#table(is.na(d$t2_ln_il12))
#table(is.na(d$t2_ln_il13))
#table(is.na(d$t2_ln_il17))
#table(is.na(d$t2_ln_il1))
#table(is.na(d$t2_ln_il2))
#table(is.na(d$t2_ln_il21))
#table(is.na(d$t2_ln_il4))
#table(is.na(d$t2_ln_il5))
#table(is.na(d$t2_ln_il6))
#table(is.na(d$t2_ln_tnf))


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

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
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=d["sex"], V="sex", id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", verbose=FALSE)
  
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
t2_crp_subgroup_L<-list_immune$t2_ln_crp
t2_agp_subgroup_L<-list_immune$t2_ln_agp
t2_gmc_subgroup_L<-list_immune$t2_ln_gmc
t2_ifn_subgroup_L<-list_immune$t2_ln_ifn
t2_il10_subgroup_L<-list_immune$t2_ln_il10
t2_il12_subgroup_L<-list_immune$t2_ln_il12 
t2_il13_subgroup_L<-list_immune$t2_ln_il13
t2_il17_subgroup_L<-list_immune$t2_ln_il17
t2_il1_subgroup_L<-list_immune$t2_ln_il1
t2_il2_subgroup_L<-list_immune$t2_ln_il21
t2_il21_subgroup_L<-list_immune$t2_ln_il21
t2_il4_subgroup_L<-list_immune$t2_ln_il4
t2_il5_subgroup_L<-list_immune$t2_ln_il5
t2_il6_subgroup_L<-list_immune$t2_ln_il6
t2_tnf_subgroup_L<-list_immune$t2_ln_tnf

t2_ratio_gmc_il10_subgroup_L<-list_immune$t2_ratio_gmc_il10 
t2_ratio_ifn_il10_subgroup_L<-list_immune$t2_ratio_ifn_il10
t2_ratio_il12_il10_subgroup_L<-list_immune$t2_ratio_il12_il10 
t2_ratio_il13_il10_subgroup_L<-list_immune$t2_ratio_il13_il10 
t2_ratio_il17_il10_subgroup_L<-list_immune$t2_ratio_il17_il10 
t2_ratio_il1_il10_subgroup_L<-list_immune$t2_ratio_il1_il10 
t2_ratio_il21_il10_subgroup_L<-list_immune$t2_ratio_il21_il10 
t2_ratio_il2_il10_subgroup_L<-list_immune$t2_ratio_il2_il10
t2_ratio_il4_il10_subgroup_L<-list_immune$t2_ratio_il4_il10
t2_ratio_il5_il10_subgroup_L<-list_immune$t2_ratio_il5_il10 
t2_ratio_il6_il10_subgroup_L<-list_immune$t2_ratio_il6_il10 
t2_ratio_tnf_il10_subgroup_L<-list_immune$t2_ratio_tnf_il10 

t2_ratio_il12_il4_subgroup_L<-list_immune$t2_ratio_il12_il4 
t2_ratio_ifn_il4_subgroup_L<-list_immune$t2_ratio_ifn_il4 
t2_ratio_il12_il5_subgroup_L<-list_immune$t2_ratio_il12_il5 
t2_ratio_ifn_il5_subgroup_L<-list_immune$t2_ratio_ifn_il5
t2_ratio_il12_il13_subgroup_L<-list_immune$t2_ratio_il12_il13
t2_ratio_ifn_il13_subgroup_L<-list_immune$t2_ratio_ifn_il13 

t2_ratio_il12_il17_subgroup_L<-list_immune$t2_ratio_il12_il17
t2_ratio_ifn_il17_subgroup_L<-list_immune$t2_ratio_ifn_il17
t2_ratio_il12_il21_subgroup_L<-list_immune$t2_ratio_il12_il21  
t2_ratio_ifn_il21_subgroup_L<-list_immune$t2_ratio_ifn_il21

t2_ratio_pro_il10_subgroup_L<-list_immune$t2_ratio_pro_il10
t2_ratio_th1_il10_subgroup_L<-list_immune$t2_ratio_th1_il10 
t2_ratio_th2_il10_subgroup_L<-list_immune$t2_ratio_th2_il10 
t2_ratio_th17_il10_subgroup_L<-list_immune$t2_ratio_th17_il10
t2_ratio_th1_th2_subgroup_L<-list_immune$t2_ratio_th1_th2 
t2_ratio_th1_th17_subgroup_L<-list_immune$t2_ratio_th1_th17


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

# Exclude children with missing data (none)

#table(is.na(d$t3_ln_igf))
#table(is.na(d$t3_ln_gmc))
#table(is.na(d$t3_ln_ifn))
#table(is.na(d$t3_ln_il10))
#table(is.na(d$t3_ln_il12))
#table(is.na(d$t3_ln_il13))
#table(is.na(d$t3_ln_il17))
#table(is.na(d$t3_ln_il1))
#table(is.na(d$t3_ln_il2))
#table(is.na(d$t3_ln_il21))
#table(is.na(d$t3_ln_il4))
#table(is.na(d$t3_ln_il5))
#table(is.na(d$t3_ln_il6))
#table(is.na(d$t3_ln_tnf))


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

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
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=d["sex"], V="sex", id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", verbose=FALSE)
  
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
t3_gmc_subgroup_L<-list_immune$t3_ln_gmc
t3_ifn_subgroup_L<-list_immune$t3_ln_ifn
t3_il10_subgroup_L<-list_immune$t3_ln_il10
t3_il12_subgroup_L<-list_immune$t3_ln_il12
t3_il13_subgroup_L<-list_immune$t3_ln_il13
t3_il17_subgroup_L<-list_immune$t3_ln_il17
t3_il1_subgroup_L<-list_immune$t3_ln_il1
t3_il2_subgroup_L<-list_immune$t3_ln_il2
t3_il21_subgroup_L<-list_immune$t3_ln_il21
t3_il4_subgroup_L<-list_immune$t3_ln_il4
t3_il5_subgroup_L<-list_immune$t3_ln_il5
t3_il6_subgroup_L<-list_immune$t3_ln_il6
t3_tnf_subgroup_L<-list_immune$t3_ln_tnf


t3_ratio_gmc_il10_subgroup_L<-list_immune$t3_ratio_gmc_il10
t3_ratio_ifn_il10_subgroup_L<-list_immune$t3_ratio_ifn_il10
t3_ratio_il12_il10_subgroup_L<-list_immune$t3_ratio_il12_il10
t3_ratio_il13_il10_subgroup_L<-list_immune$t3_ratio_il13_il10
t3_ratio_il17_il10_subgroup_L<-list_immune$t3_ratio_il17_il10
t3_ratio_il1_il10_subgroup_L<-list_immune$t3_ratio_il1_il10
t3_ratio_il21_il10_subgroup_L<-list_immune$t3_ratio_il21_il10
t3_ratio_il2_il10_subgroup_L<-list_immune$t3_ratio_il2_il10
t3_ratio_il4_il10_subgroup_L<-list_immune$t3_ratio_il4_il10
t3_ratio_il5_il10_subgroup_L<-list_immune$t3_ratio_il5_il10
t3_ratio_il6_il10_subgroup_L<-list_immune$t3_ratio_il6_il10
t3_ratio_tnf_il10_subgroup_L<-list_immune$t3_ratio_tnf_il10

t3_ratio_il12_il4_subgroup_L<-list_immune$t3_ratio_il12_il4 
t3_ratio_ifn_il4_subgroup_L<-list_immune$t3_ratio_ifn_il4
t3_ratio_il12_il5_subgroup_L<-list_immune$t3_ratio_il12_il5
t3_ratio_ifn_il5_subgroup_L<-list_immune$t3_ratio_ifn_il5
t3_ratio_il12_il13_subgroup_L<-list_immune$t3_ratio_il12_il13
t3_ratio_ifn_il13_subgroup_L<-list_immune$t3_ratio_ifn_il13

t3_ratio_il12_il17_subgroup_L<-list_immune$t3_ratio_il12_il17 
t3_ratio_ifn_il17_subgroup_L<-list_immune$t3_ratio_ifn_il17
t3_ratio_il12_il21_subgroup_L<-list_immune$t3_ratio_il12_il21
t3_ratio_ifn_il21_subgroup_L<-list_immune$t3_ratio_ifn_il21

t3_ratio_pro_il10_subgroup_L<-list_immune$t3_ratio_pro_il10
t3_ratio_th1_il10_subgroup_L<-list_immune$t3_ratio_th1_il10
t3_ratio_th2_il10_subgroup_L<-list_immune$t3_ratio_th2_il10
t3_ratio_th17_il10_subgroup_L<-list_immune$t3_ratio_th17_il10
t3_ratio_th1_th2_subgroup_L<-list_immune$t3_ratio_th1_th2
t3_ratio_th1_th17_subgroup_L<-list_immune$t3_ratio_th1_th17

#Delta

#---------------------------------------
# Load the analysis dataset,
# the baseline covariate dataset
#---------------------------------------

d <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))


#---------------------------------------
# subset to the relevant measurement
# Delta T2 - T3
#---------------------------------------

dim(d)


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

# sort the data for perfect replication with andrew on the V-fold cross-validation
d <- d[order(d$block,d$clusterid,d$dataid,d$childid),]


###################
#Subgroup Analyses @Delta T2-T3
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
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=d["sex"], V="sex", id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", verbose=FALSE)
  
  temp_metric<-as.data.frame(temp$lincom)
  
  colnames(temp_metric) <-c("subgroup", "RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#grab the variables with prefix 'd23_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('d23_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('d23_', names(d))]

#resulting matrix
list_immune


#to save each matrix separately for comparing with Andrew. 

d23_ln_igf_subgroup_L<-list_immune$d23_ln_igf 
d23_ln_gmc_subgroup_L<-list_immune$d23_ln_gmc
d23_ln_ifn_subgroup_L<-list_immune$d23_ln_ifn 
d23_ln_il10_subgroup_L<-list_immune$d23_ln_il10 
d23_ln_il12_subgroup_L<-list_immune$d23_ln_il12 
d23_ln_il13_subgroup_L<-list_immune$d23_ln_il13 
d23_ln_il17_subgroup_L<-list_immune$d23_ln_il17
d23_ln_il1_subgroup_L<-list_immune$d23_ln_il1 
d23_ln_il2_subgroup_L<-list_immune$d23_ln_il2 
d23_ln_il21_subgroup_L<-list_immune$d23_ln_il21 
d23_ln_il4_subgroup_L<-list_immune$d23_ln_il4
d23_ln_il5_subgroup_L<-list_immune$d23_ln_il5 
d23_ln_il6_subgroup_L<-list_immune$d23_ln_il6 
d23_ln_tnf_subgroup_L<-list_immune$d23_ln_tnf

d23_ratio_gmc_il10_subgroup_L<-list_immune$d23_ratio_gmc_il10  
d23_ratio_ifn_il10_subgroup_L<-list_immune$d23_ratio_ifn_il10  
d23_ratio_il12_il10_subgroup_L<-list_immune$d23_ratio_il12_il10 
d23_ratio_il13_il10_subgroup_L<-list_immune$d23_ratio_il13_il10 
d23_ratio_il17_il10_subgroup_L<-list_immune$d23_ratio_il17_il10 
d23_ratio_il1_il10_subgroup_L<-list_immune$d23_ratio_il1_il10 
d23_ratio_il21_il10_subgroup_L<-list_immune$d23_ratio_il21_il10 
d23_ratio_il2_il10_subgroup_L<-list_immune$d23_ratio_il2_il10 
d23_ratio_il4_il10_subgroup_L<-list_immune$d23_ratio_il4_il10
d23_ratio_il5_il10_subgroup_L<-list_immune$d23_ratio_il5_il10
d23_ratio_il6_il10_subgroup_L<-list_immune$d23_ratio_il6_il10 
d23_ratio_tnf_il10_subgroup_L<-list_immune$d23_ratio_tnf_il10 

d23_ratio_il12_il4_subgroup_L<-list_immune$d23_ratio_il12_il4 
d23_ratio_ifn_il4_subgroup_L<-list_immune$d23_ratio_ifn_il4 
d23_ratio_il12_il5_subgroup_L<-list_immune$d23_ratio_il12_il5  
d23_ratio_ifn_il5_subgroup_L<-list_immune$d23_ratio_ifn_il5
d23_ratio_il12_il13_subgroup_L<-list_immune$d23_ratio_il12_il13 
d23_ratio_ifn_il13_subgroup_L<-list_immune$d23_ratio_ifn_il13 

d23_ratio_il12_il17_subgroup_L<-list_immune$d23_ratio_il12_il17 
d23_ratio_ifn_il17_subgroup_L<-list_immune$d23_ratio_ifn_il17 
d23_ratio_il12_il21_subgroup_L<-list_immune$d23_ratio_il12_il21 
d23_ratio_ifn_il21_subgroup_L<-list_immune$d23_ratio_ifn_il21 

d23_ratio_pro_il10_subgroup_L<-list_immune$d23_ratio_pro_il10 
d23_ratio_th1_il10_subgroup_L<-list_immune$d23_ratio_th1_il10 
d23_ratio_th2_il10_subgroup_L<-list_immune$d23_ratio_th2_il10_unadj  
d23_ratio_th17_il10_subgroup_L<-list_immune$d23_ratio_th17_il10
d23_ratio_th1_th2_subgroup_L<-list_immune$d23_ratio_th1_th2
d23_ratio_th1_th17_subgroup_L<-list_immune$d23_ratio_th1_th17

#Display results

t2_igf_subgroup_L
t2_crp_subgroup_L
t2_agp_subgroup_L
t2_gmc_subgroup_L
t2_ifn_subgroup_L
t2_il10_subgroup_L
t2_il12_subgroup_L
t2_il13_subgroup_L
t2_il17_subgroup_L
t2_il1_subgroup_L
t2_il2_subgroup_L
t2_il21_subgroup_L
t2_il4_subgroup_L
t2_il5_subgroup_L
t2_il6_subgroup_L
t2_tnf_subgroup_L

t2_ratio_gmc_il10_subgroup_L
t2_ratio_ifn_il10_subgroup_L
t2_ratio_il12_il10_subgroup_L 
t2_ratio_il13_il10_subgroup_L 
t2_ratio_il17_il10_subgroup_L
t2_ratio_il1_il10_subgroup_L
t2_ratio_il21_il10_subgroup_L
t2_ratio_il2_il10_subgroup_L
t2_ratio_il4_il10_subgroup_L
t2_ratio_il5_il10_subgroup_L
t2_ratio_il6_il10_subgroup_L
t2_ratio_tnf_il10_subgroup_L

t2_ratio_il12_il4_subgroup_L
t2_ratio_ifn_il4_subgroup_L
t2_ratio_il12_il5_subgroup_L 
t2_ratio_ifn_il5_subgroup_L
t2_ratio_il12_il13_subgroup_L
t2_ratio_ifn_il13_subgroup_L 

t2_ratio_il12_il17_subgroup_L
t2_ratio_ifn_il17_subgroup_L
t2_ratio_il12_il21_subgroup_L
t2_ratio_ifn_il21_subgroup_L

t2_ratio_pro_il10_subgroup_L
t2_ratio_th1_il10_subgroup_L 
t2_ratio_th2_il10_subgroup_L
t2_ratio_th17_il10_subgroup_L
t2_ratio_th1_th2_subgroup_L
t2_ratio_th1_th17_subgroup_L



t3_igf_subgroup_L
t3_gmc_subgroup_L
t3_ifn_subgroup_L
t3_il10_subgroup_L
t3_il12_subgroup_L
t3_il13_subgroup_L
t3_il17_subgroup_L
t3_il1_subgroup_L
t3_il2_subgroup_L
t3_il21_subgroup_L
t3_il4_subgroup_L
t3_il5_subgroup_L
t3_il6_subgroup_L
t3_tnf_subgroup_L


t3_ratio_gmc_il10_subgroup_L
t3_ratio_ifn_il10_subgroup_L
t3_ratio_il12_il10_subgroup_L
t3_ratio_il13_il10_subgroup_L
t3_ratio_il17_il10_subgroup_L
t3_ratio_il1_il10_subgroup_L
t3_ratio_il21_il10_subgroup_L
t3_ratio_il2_il10_subgroup_L
t3_ratio_il4_il10_subgroup_L
t3_ratio_il5_il10_subgroup_L
t3_ratio_il6_il10_subgroup_L
t3_ratio_tnf_il10_subgroup_L

t3_ratio_il12_il4_subgroup_L 
t3_ratio_ifn_il4_subgroup_L
t3_ratio_il12_il5_subgroup_L
t3_ratio_ifn_il5_subgroup_L
t3_ratio_il12_il13_subgroup_L
t3_ratio_ifn_il13_subgroup_L

t3_ratio_il12_il17_subgroup_L 
t3_ratio_ifn_il17_subgroup_L
t3_ratio_il12_il21_subgroup_L
t3_ratio_ifn_il21_subgroup_L

t3_ratio_pro_il10_subgroup_L
t3_ratio_th1_il10_subgroup_L
t3_ratio_th2_il10_subgroup_L
t3_ratio_th17_il10_subgroup_L
t3_ratio_th1_th2_subgroup_L
t3_ratio_th1_th17_subgroup_L


d23_ln_igf_subgroup_L 
d23_ln_gmc_subgroup_L
d23_ln_ifn_subgroup_L
d23_ln_il10_subgroup_L
d23_ln_il12_subgroup_L
d23_ln_il13_subgroup_L
d23_ln_il17_subgroup_L
d23_ln_il1_subgroup_L
d23_ln_il2_subgroup_L
d23_ln_il21_subgroup_L
d23_ln_il4_subgroup_L
d23_ln_il5_subgroup_L
d23_ln_il6_subgroup_L
d23_ln_tnf_subgroup_L

d23_ratio_gmc_il10_subgroup_L  
d23_ratio_ifn_il10_subgroup_L 
d23_ratio_il12_il10_subgroup_L
d23_ratio_il13_il10_subgroup_L 
d23_ratio_il17_il10_subgroup_L 
d23_ratio_il1_il10_subgroup_L
d23_ratio_il21_il10_subgroup_L 
d23_ratio_il2_il10_subgroup_L
d23_ratio_il4_il10_subgroup_L
d23_ratio_il5_il10_subgroup_L
d23_ratio_il6_il10_subgroup_L
d23_ratio_tnf_il10_subgroup_L

d23_ratio_il12_il4_subgroup_L
d23_ratio_ifn_il4_subgroup_L
d23_ratio_il12_il5_subgroup_L
d23_ratio_ifn_il5_subgroup_L
d23_ratio_il12_il13_subgroup_L
d23_ratio_ifn_il13_subgroup_L

d23_ratio_il12_il17_subgroup_L
d23_ratio_ifn_il17_subgroup_L
d23_ratio_il12_il21_subgroup_L
d23_ratio_ifn_il21_subgroup_L

d23_ratio_pro_il10_subgroup_L 
d23_ratio_th1_il10_subgroup_L
d23_ratio_th2_il10_subgroup_L 
d23_ratio_th17_il10_subgroup_L
d23_ratio_th1_th2_subgroup_L
d23_ratio_th1_th17_subgroup_L



#-----------------------------------
#save data
#-----------------------------------

save (t2_igf_subgroup_L,
      t2_crp_subgroup_L,
      t2_agp_subgroup_L,
      t2_gmc_subgroup_L,
      t2_ifn_subgroup_L,
      t2_il10_subgroup_L,
      t2_il12_subgroup_L,
      t2_il13_subgroup_L,
      t2_il17_subgroup_L,
      t2_il1_subgroup_L,
      t2_il2_subgroup_L,
      t2_il21_subgroup_L,
      t2_il4_subgroup_L,
      t2_il5_subgroup_L,
      t2_il6_subgroup_L,
      t2_tnf_subgroup_L,
      
      t2_ratio_gmc_il10_subgroup_L,
      t2_ratio_ifn_il10_subgroup_L,
      t2_ratio_il12_il10_subgroup_L, 
      t2_ratio_il13_il10_subgroup_L,
      t2_ratio_il17_il10_subgroup_L,
      t2_ratio_il1_il10_subgroup_L,
      t2_ratio_il21_il10_subgroup_L,
      t2_ratio_il2_il10_subgroup_L,
      t2_ratio_il4_il10_subgroup_L,
      t2_ratio_il5_il10_subgroup_L,
      t2_ratio_il6_il10_subgroup_L,
      t2_ratio_tnf_il10_subgroup_L,
      
      t2_ratio_il12_il4_subgroup_L,
      t2_ratio_ifn_il4_subgroup_L,
      t2_ratio_il12_il5_subgroup_L, 
      t2_ratio_ifn_il5_subgroup_L,
      t2_ratio_il12_il13_subgroup_L,
      t2_ratio_ifn_il13_subgroup_L, 
      
      t2_ratio_il12_il17_subgroup_L,
      t2_ratio_ifn_il17_subgroup_L,
      t2_ratio_il12_il21_subgroup_L,
      t2_ratio_ifn_il21_subgroup_L,
      
      t2_ratio_pro_il10_subgroup_L,
      t2_ratio_th1_il10_subgroup_L, 
      t2_ratio_th2_il10_subgroup_L,
      t2_ratio_th17_il10_subgroup_L,
      t2_ratio_th1_th2_subgroup_L,
      t2_ratio_th1_th17_subgroup_L,
      
      
      
      t3_igf_subgroup_L,
      t3_gmc_subgroup_L,
      t3_ifn_subgroup_L,
      t3_il10_subgroup_L,
      t3_il12_subgroup_L,
      t3_il13_subgroup_L,
      t3_il17_subgroup_L,
      t3_il1_subgroup_L,
      t3_il2_subgroup_L,
      t3_il21_subgroup_L,
      t3_il4_subgroup_L,
      t3_il5_subgroup_L,
      t3_il6_subgroup_L,
      t3_tnf_subgroup_L,
      
      
      t3_ratio_gmc_il10_subgroup_L,
      t3_ratio_ifn_il10_subgroup_L,
      t3_ratio_il12_il10_subgroup_L,
      t3_ratio_il13_il10_subgroup_L,
      t3_ratio_il17_il10_subgroup_L,
      t3_ratio_il1_il10_subgroup_L,
      t3_ratio_il21_il10_subgroup_L,
      t3_ratio_il2_il10_subgroup_L,
      t3_ratio_il4_il10_subgroup_L,
      t3_ratio_il5_il10_subgroup_L,
      t3_ratio_il6_il10_subgroup_L,
      t3_ratio_tnf_il10_subgroup_L,
      
      t3_ratio_il12_il4_subgroup_L, 
      t3_ratio_ifn_il4_subgroup_L,
      t3_ratio_il12_il5_subgroup_L,
      t3_ratio_ifn_il5_subgroup_L,
      t3_ratio_il12_il13_subgroup_L,
      t3_ratio_ifn_il13_subgroup_L,
      
      t3_ratio_il12_il17_subgroup_L, 
      t3_ratio_ifn_il17_subgroup_L,
      t3_ratio_il12_il21_subgroup_L,
      t3_ratio_ifn_il21_subgroup_L,
      
      t3_ratio_pro_il10_subgroup_L,
      t3_ratio_th1_il10_subgroup_L,
      t3_ratio_th2_il10_subgroup_L,
      t3_ratio_th17_il10_subgroup_L,
      t3_ratio_th1_th2_subgroup_L,
      t3_ratio_th1_th17_subgroup_L,
      
      
      d23_ln_igf_subgroup_L,
      d23_ln_gmc_subgroup_L,
      d23_ln_ifn_subgroup_L,
      d23_ln_il10_subgroup_L,
      d23_ln_il12_subgroup_L,
      d23_ln_il13_subgroup_L,
      d23_ln_il17_subgroup_L,
      d23_ln_il1_subgroup_L,
      d23_ln_il2_subgroup_L,
      d23_ln_il21_subgroup_L,
      d23_ln_il4_subgroup_L,
      d23_ln_il5_subgroup_L,
      d23_ln_il6_subgroup_L,
      d23_ln_tnf_subgroup_L,
      
      d23_ratio_gmc_il10_subgroup_L,  
      d23_ratio_ifn_il10_subgroup_L,
      d23_ratio_il12_il10_subgroup_L,
      d23_ratio_il13_il10_subgroup_L, 
      d23_ratio_il17_il10_subgroup_L, 
      d23_ratio_il1_il10_subgroup_L,
      d23_ratio_il21_il10_subgroup_L, 
      d23_ratio_il2_il10_subgroup_L,
      d23_ratio_il4_il10_subgroup_L,
      d23_ratio_il5_il10_subgroup_L,
      d23_ratio_il6_il10_subgroup_L,
      d23_ratio_tnf_il10_subgroup_L,
      
      d23_ratio_il12_il4_subgroup_L,
      d23_ratio_ifn_il4_subgroup_L,
      d23_ratio_il12_il5_subgroup_L,
      d23_ratio_ifn_il5_subgroup_L,
      d23_ratio_il12_il13_subgroup_L,
      d23_ratio_ifn_il13_subgroup_L,
      
      d23_ratio_il12_il17_subgroup_L,
      d23_ratio_ifn_il17_subgroup_L,
      d23_ratio_il12_il21_subgroup_L,
      d23_ratio_ifn_il21_subgroup_L,
      
      d23_ratio_pro_il10_subgroup_L, 
      d23_ratio_th1_il10_subgroup_L,
      d23_ratio_th2_il10_subgroup_L,
      d23_ratio_th17_il10_subgroup_L,
      d23_ratio_th1_th2_subgroup_L,
      d23_ratio_th1_th17_subgroup_L, file="~/Dropbox/WBB-EE-analysis/Results/Audrie/immune_subgroup.RData")

