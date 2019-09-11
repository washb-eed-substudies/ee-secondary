#---------------------------------------
# bangladesh-immune-adj.R
#
# audrie lin (audrielin@berkeley.edu)
#
# calculate adjusted differences
# between treatment arms for immune assays
#---------------------------------------

#---------------------------------------
# input files:
#	bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv (from 3-bangladesh-dm-immun-plasma-immun-3.do)
#
# output files:
#	bangladesh-immune-adj.Rdata
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

d <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))


#---------------------------------------
# subset to the relevant measurement
# T2 
#---------------------------------------

dim(d)


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

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
washb_function <- function(d,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=W, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
  return(temp_metric)
}

#washb_function(d, "igf_t2")

#grab the variables with prefix 't2_' from the data frame and then apply the washb_function
list_immune <- lapply(names(d)[grep('t2_', names(d))],  function(x) washb_function(d,x))

list_immune

#put names of each of the variables into the matrix
names(list_immune) <- names(d)[grep('t2_', names(d))]

#resulting matrix
list_immune

#Save intermediate R objects for replication comparison
da <- d
wa <-W
save(da, wa, list_immune, file = here("replication objects/audrie_immune_W.rdata"))


#to save each matrix separately for comparing with Andrew. 
t2_igf_adj_L<-list_immune$t2_ln_igf
t2_crp_adj_L<-list_immune$t2_ln_crp
t2_agp_adj_L<-list_immune$t2_ln_agp
t2_gmc_adj_L<-list_immune$t2_ln_gmc
t2_ifn_adj_L<-list_immune$t2_ln_ifn
t2_il10_adj_L<-list_immune$t2_ln_il10
t2_il12_adj_L<-list_immune$t2_ln_il12 
t2_il13_adj_L<-list_immune$t2_ln_il13
t2_il17_adj_L<-list_immune$t2_ln_il17
t2_il1_adj_L<-list_immune$t2_ln_il1
t2_il2_adj_L<-list_immune$t2_ln_il2
t2_il21_adj_L<-list_immune$t2_ln_il21
t2_il4_adj_L<-list_immune$t2_ln_il4
t2_il5_adj_L<-list_immune$t2_ln_il5
t2_il6_adj_L<-list_immune$t2_ln_il6
t2_tnf_adj_L<-list_immune$t2_ln_tnf

t2_ratio_gmc_il10_adj_L<-list_immune$t2_ratio_gmc_il10 
t2_ratio_ifn_il10_adj_L<-list_immune$t2_ratio_ifn_il10
t2_ratio_il12_il10_adj_L<-list_immune$t2_ratio_il12_il10 
t2_ratio_il13_il10_adj_L<-list_immune$t2_ratio_il13_il10 
t2_ratio_il17_il10_adj_L<-list_immune$t2_ratio_il17_il10 
t2_ratio_il1_il10_adj_L<-list_immune$t2_ratio_il1_il10 
t2_ratio_il21_il10_adj_L<-list_immune$t2_ratio_il21_il10 
t2_ratio_il2_il10_adj_L<-list_immune$t2_ratio_il2_il10
t2_ratio_il4_il10_adj_L<-list_immune$t2_ratio_il4_il10
t2_ratio_il5_il10_adj_L<-list_immune$t2_ratio_il5_il10 
t2_ratio_il6_il10_adj_L<-list_immune$t2_ratio_il6_il10 
t2_ratio_tnf_il10_adj_L<-list_immune$t2_ratio_tnf_il10 

t2_ratio_il12_il4_adj_L<-list_immune$t2_ratio_il12_il4 
t2_ratio_ifn_il4_adj_L<-list_immune$t2_ratio_ifn_il4 
t2_ratio_il12_il5_adj_L<-list_immune$t2_ratio_il12_il5 
t2_ratio_ifn_il5_adj_L<-list_immune$t2_ratio_ifn_il5
t2_ratio_il12_il13_adj_L<-list_immune$t2_ratio_il12_il13
t2_ratio_ifn_il13_adj_L<-list_immune$t2_ratio_ifn_il13 

t2_ratio_il12_il17_adj_L<-list_immune$t2_ratio_il12_il17
t2_ratio_ifn_il17_adj_L<-list_immune$t2_ratio_ifn_il17
t2_ratio_il12_il21_adj_L<-list_immune$t2_ratio_il12_il21  
t2_ratio_ifn_il21_adj_L<-list_immune$t2_ratio_ifn_il21

t2_ratio_pro_il10_adj_L<-list_immune$t2_ratio_pro_il10
t2_ratio_th1_il10_adj_L<-list_immune$t2_ratio_th1_il10 
t2_ratio_th2_il10_adj_L<-list_immune$t2_ratio_th2_il10 
t2_ratio_th17_il10_adj_L<-list_immune$t2_ratio_th17_il10
t2_ratio_th1_th2_adj_L<-list_immune$t2_ratio_th1_th2 
t2_ratio_th1_th17_adj_L<-list_immune$t2_ratio_th1_th17

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


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

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
save(Wa3, file=here("replication objects/andrew_immune_W3.rdata"))


# Set up the WASHB function
# df=data frame

washb_function <- function(df,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=W, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
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
t3_igf_adj_L<-list_immune$t3_ln_igf
t3_gmc_adj_L<-list_immune$t3_ln_gmc
t3_ifn_adj_L<-list_immune$t3_ln_ifn
t3_il10_adj_L<-list_immune$t3_ln_il10
t3_il12_adj_L<-list_immune$t3_ln_il12
t3_il13_adj_L<-list_immune$t3_ln_il13
t3_il17_adj_L<-list_immune$t3_ln_il17
t3_il1_adj_L<-list_immune$t3_ln_il1
t3_il2_adj_L<-list_immune$t3_ln_il2
t3_il21_adj_L<-list_immune$t3_ln_il21
t3_il4_adj_L<-list_immune$t3_ln_il4
t3_il5_adj_L<-list_immune$t3_ln_il5
t3_il6_adj_L<-list_immune$t3_ln_il6
t3_tnf_adj_L<-list_immune$t3_ln_tnf


t3_ratio_gmc_il10_adj_L<-list_immune$t3_ratio_gmc_il10
t3_ratio_ifn_il10_adj_L<-list_immune$t3_ratio_ifn_il10
t3_ratio_il12_il10_adj_L<-list_immune$t3_ratio_il12_il10
t3_ratio_il13_il10_adj_L<-list_immune$t3_ratio_il13_il10
t3_ratio_il17_il10_adj_L<-list_immune$t3_ratio_il17_il10
t3_ratio_il1_il10_adj_L<-list_immune$t3_ratio_il1_il10
t3_ratio_il21_il10_adj_L<-list_immune$t3_ratio_il21_il10
t3_ratio_il2_il10_adj_L<-list_immune$t3_ratio_il2_il10
t3_ratio_il4_il10_adj_L<-list_immune$t3_ratio_il4_il10
t3_ratio_il5_il10_adj_L<-list_immune$t3_ratio_il5_il10
t3_ratio_il6_il10_adj_L<-list_immune$t3_ratio_il6_il10
t3_ratio_tnf_il10_adj_L<-list_immune$t3_ratio_tnf_il10

t3_ratio_il12_il4_adj_L<-list_immune$t3_ratio_il12_il4 
t3_ratio_ifn_il4_adj_L<-list_immune$t3_ratio_ifn_il4
t3_ratio_il12_il5_adj_L<-list_immune$t3_ratio_il12_il5
t3_ratio_ifn_il5_adj_L<-list_immune$t3_ratio_ifn_il5
t3_ratio_il12_il13_adj_L<-list_immune$t3_ratio_il12_il13
t3_ratio_ifn_il13_adj_L<-list_immune$t3_ratio_ifn_il13

t3_ratio_il12_il17_adj_L<-list_immune$t3_ratio_il12_il17 
t3_ratio_ifn_il17_adj_L<-list_immune$t3_ratio_ifn_il17
t3_ratio_il12_il21_adj_L<-list_immune$t3_ratio_il12_il21
t3_ratio_ifn_il21_adj_L<-list_immune$t3_ratio_ifn_il21

t3_ratio_pro_il10_adj_L<-list_immune$t3_ratio_pro_il10
t3_ratio_th1_il10_adj_L<-list_immune$t3_ratio_th1_il10
t3_ratio_th2_il10_adj_L<-list_immune$t3_ratio_th2_il10
t3_ratio_th17_il10_adj_L<-list_immune$t3_ratio_th17_il10
t3_ratio_th1_th2_adj_L<-list_immune$t3_ratio_th1_th2
t3_ratio_th1_th17_adj_L<-list_immune$t3_ratio_th1_th17


###Delta

#---------------------------------------
# Load the analysis dataset,
# the baseline covariate dataset
#---------------------------------------

d <- read.csv("bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv",colClasses=c("dataid"="character"))

#---------------------------------------
# subset to the relevant measurement
#---------------------------------------

dim(d)


# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Nutrition + WSH"))

# ensure that variables are coded as factors
d$monsoon_bt2 <- factor(d$monsoon_bt2)
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
Wvars<-c("monsoon_bt2","monsoon_bt3","ageday_bt2","ageday_bt3", "sex","birthord", "momage","momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

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

W$monsoon_bt2<-as.factor(W$monsoon_bt2)
d$monsoon_bt2<-addNA(d$monsoon_bt2)
levels(d$monsoon_bt2)[length(levels(d$monsoon_bt2))]<-"Missing"


W$momage<-as.numeric(W$momage)
W$ageday_bt3<-as.numeric(W$ageday_bt3)
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
# df=data frame

washb_function <- function(df,x) {
  
  temp <- washb_glm(Y=d[,x], tr=d$tr, pair=NULL, W=W, id=d$block, contrast = c("Control","Nutrition + WSH"), family="gaussian", print=TRUE)
  temp_metric <-as.matrix(temp$TR)
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  colnames(temp_metric) <-c("RD","ci.lb","ci.ub","SE","z","P-value")
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

d23_ln_igf_adj_L<-list_immune$d23_ln_igf 
d23_ln_gmc_adj_L<-list_immune$d23_ln_gmc
d23_ln_ifn_adj_L<-list_immune$d23_ln_ifn 
d23_ln_il10_adj_L<-list_immune$d23_ln_il10 
d23_ln_il12_adj_L<-list_immune$d23_ln_il12 
d23_ln_il13_adj_L<-list_immune$d23_ln_il13 
d23_ln_il17_adj_L<-list_immune$d23_ln_il17
d23_ln_il1_adj_L<-list_immune$d23_ln_il1 
d23_ln_il2_adj_L<-list_immune$d23_ln_il2 
d23_ln_il21_adj_L<-list_immune$d23_ln_il21 
d23_ln_il4_adj_L<-list_immune$d23_ln_il4
d23_ln_il5_adj_L<-list_immune$d23_ln_il5 
d23_ln_il6_adj_L<-list_immune$d23_ln_il6 
d23_ln_tnf_adj_L<-list_immune$d23_ln_tnf

d23_ratio_gmc_il10_adj_L<-list_immune$d23_ratio_gmc_il10  
d23_ratio_ifn_il10_adj_L<-list_immune$d23_ratio_ifn_il10  
d23_ratio_il12_il10_adj_L<-list_immune$d23_ratio_il12_il10 
d23_ratio_il13_il10_adj_L<-list_immune$d23_ratio_il13_il10 
d23_ratio_il17_il10_adj_L<-list_immune$d23_ratio_il17_il10 
d23_ratio_il1_il10_adj_L<-list_immune$d23_ratio_il1_il10 
d23_ratio_il21_il10_adj_L<-list_immune$d23_ratio_il21_il10 
d23_ratio_il2_il10_adj_L<-list_immune$d23_ratio_il2_il10 
d23_ratio_il4_il10_adj_L<-list_immune$d23_ratio_il4_il10
d23_ratio_il5_il10_adj_L<-list_immune$d23_ratio_il5_il10
d23_ratio_il6_il10_adj_L<-list_immune$d23_ratio_il6_il10 
d23_ratio_tnf_il10_adj_L<-list_immune$d23_ratio_tnf_il10 

d23_ratio_il12_il4_adj_L<-list_immune$d23_ratio_il12_il4 
d23_ratio_ifn_il4_adj_L<-list_immune$d23_ratio_ifn_il4 
d23_ratio_il12_il5_adj_L<-list_immune$d23_ratio_il12_il5  
d23_ratio_ifn_il5_adj_L<-list_immune$d23_ratio_ifn_il5
d23_ratio_il12_il13_adj_L<-list_immune$d23_ratio_il12_il13 
d23_ratio_ifn_il13_adj_L<-list_immune$d23_ratio_ifn_il13 

d23_ratio_il12_il17_adj_L<-list_immune$d23_ratio_il12_il17 
d23_ratio_ifn_il17_adj_L<-list_immune$d23_ratio_ifn_il17 
d23_ratio_il12_il21_adj_L<-list_immune$d23_ratio_il12_il21 
d23_ratio_ifn_il21_adj_L<-list_immune$d23_ratio_ifn_il21 

d23_ratio_pro_il10_adj_L<-list_immune$d23_ratio_pro_il10 
d23_ratio_th1_il10_adj_L<-list_immune$d23_ratio_th1_il10 
d23_ratio_th2_il10_adj_L<-list_immune$d23_ratio_th2_il10  
d23_ratio_th17_il10_adj_L<-list_immune$d23_ratio_th17_il10
d23_ratio_th1_th2_adj_L<-list_immune$d23_ratio_th1_th2
d23_ratio_th1_th17_adj_L<-list_immune$d23_ratio_th1_th17



#Display results




t2_igf_adj_L
t2_crp_adj_L
t2_agp_adj_L
t2_gmc_adj_L
t2_ifn_adj_L
t2_il10_adj_L
t2_il12_adj_L
t2_il13_adj_L
t2_il17_adj_L
t2_il1_adj_L
t2_il2_adj_L
t2_il21_adj_L
t2_il4_adj_L
t2_il5_adj_L
t2_il6_adj_L
t2_tnf_adj_L

t2_ratio_gmc_il10_adj_L 
t2_ratio_ifn_il10_adj_L
t2_ratio_il12_il10_adj_L
t2_ratio_il13_il10_adj_L
t2_ratio_il17_il10_adj_L
t2_ratio_il1_il10_adj_L 
t2_ratio_il21_il10_adj_L
t2_ratio_il2_il10_adj_L
t2_ratio_il4_il10_adj_L
t2_ratio_il5_il10_adj_L
t2_ratio_il6_il10_adj_L
t2_ratio_tnf_il10_adj_L

t2_ratio_il12_il4_adj_L
t2_ratio_ifn_il4_adj_L 
t2_ratio_il12_il5_adj_L
t2_ratio_ifn_il5_adj_L
t2_ratio_il12_il13_adj_L
t2_ratio_ifn_il13_adj_L

t2_ratio_il12_il17_adj_L
t2_ratio_ifn_il17_adj_L
t2_ratio_il12_il21_adj_L
t2_ratio_ifn_il21_adj_L

t2_ratio_pro_il10_adj_L
t2_ratio_th1_il10_adj_L
t2_ratio_th2_il10_adj_L
t2_ratio_th17_il10_adj_L
t2_ratio_th1_th2_adj_L
t2_ratio_th1_th17_adj_L


t3_igf_adj_L
t3_gmc_adj_L
t3_ifn_adj_L
t3_il10_adj_L
t3_il12_adj_L
t3_il13_adj_L
t3_il17_adj_L
t3_il1_adj_L
t3_il2_adj_L
t3_il21_adj_L
t3_il4_adj_L
t3_il5_adj_L
t3_il6_adj_L
t3_tnf_adj_L


t3_ratio_gmc_il10_adj_L
t3_ratio_ifn_il10_adj_L
t3_ratio_il12_il10_adj_L
t3_ratio_il13_il10_adj_L
t3_ratio_il17_il10_adj_L
t3_ratio_il1_il10_adj_L
t3_ratio_il21_il10_adj_L
t3_ratio_il2_il10_adj_L
t3_ratio_il4_il10_adj_L
t3_ratio_il5_il10_adj_L
t3_ratio_il6_il10_adj_L
t3_ratio_tnf_il10_adj_L

t3_ratio_il12_il4_adj_L 
t3_ratio_ifn_il4_adj_L
t3_ratio_il12_il5_adj_L
t3_ratio_ifn_il5_adj_L
t3_ratio_il12_il13_adj_L
t3_ratio_ifn_il13_adj_L

t3_ratio_il12_il17_adj_L 
t3_ratio_ifn_il17_adj_L
t3_ratio_il12_il21_adj_L
t3_ratio_ifn_il21_adj_L

t3_ratio_pro_il10_adj_L
t3_ratio_th1_il10_adj_L
t3_ratio_th2_il10_adj_L
t3_ratio_th17_il10_adj_L
t3_ratio_th1_th2_adj_L
t3_ratio_th1_th17_adj_L

d23_ln_igf_adj_L
d23_ln_gmc_adj_L
d23_ln_ifn_adj_L
d23_ln_il10_adj_L 
d23_ln_il12_adj_L
d23_ln_il13_adj_L
d23_ln_il17_adj_L
d23_ln_il1_adj_L
d23_ln_il2_adj_L
d23_ln_il21_adj_L 
d23_ln_il4_adj_L
d23_ln_il5_adj_L 
d23_ln_il6_adj_L 
d23_ln_tnf_adj_L

d23_ratio_gmc_il10_adj_L
d23_ratio_ifn_il10_adj_L  
d23_ratio_il12_il10_adj_L
d23_ratio_il13_il10_adj_L
d23_ratio_il17_il10_adj_L
d23_ratio_il1_il10_adj_L
d23_ratio_il21_il10_adj_L
d23_ratio_il2_il10_adj_L
d23_ratio_il4_il10_adj_L
d23_ratio_il5_il10_adj_L
d23_ratio_il6_il10_adj_L
d23_ratio_tnf_il10_adj_L

d23_ratio_il12_il4_adj_L 
d23_ratio_ifn_il4_adj_L 
d23_ratio_il12_il5_adj_L  
d23_ratio_ifn_il5_adj_L
d23_ratio_il12_il13_adj_L 
d23_ratio_ifn_il13_adj_L 

d23_ratio_il12_il17_adj_L 
d23_ratio_ifn_il17_adj_L
d23_ratio_il12_il21_adj_L
d23_ratio_ifn_il21_adj_L 

d23_ratio_pro_il10_adj_L 
d23_ratio_th1_il10_adj_L 
d23_ratio_th2_il10_adj_L
d23_ratio_th17_il10_adj_L
d23_ratio_th1_th2_adj_L
d23_ratio_th1_th17_adj_L

#-----------------------------------
#save data
#-----------------------------------

save (t2_igf_adj_L,
      t2_crp_adj_L,
      t2_agp_adj_L,
      t2_gmc_adj_L,
      t2_ifn_adj_L,
      t2_il10_adj_L,
      t2_il12_adj_L,
      t2_il13_adj_L,
      t2_il17_adj_L,
      t2_il1_adj_L,
      t2_il2_adj_L,
      t2_il21_adj_L,
      t2_il4_adj_L,
      t2_il5_adj_L,
      t2_il6_adj_L,
      t2_tnf_adj_L,
      
      t2_ratio_gmc_il10_adj_L, 
      t2_ratio_ifn_il10_adj_L,
      t2_ratio_il12_il10_adj_L,
      t2_ratio_il13_il10_adj_L,
      t2_ratio_il17_il10_adj_L,
      t2_ratio_il1_il10_adj_L, 
      t2_ratio_il21_il10_adj_L,
      t2_ratio_il2_il10_adj_L,
      t2_ratio_il4_il10_adj_L,
      t2_ratio_il5_il10_adj_L,
      t2_ratio_il6_il10_adj_L,
      t2_ratio_tnf_il10_adj_L,
      
      t2_ratio_il12_il4_adj_L,
      t2_ratio_ifn_il4_adj_L, 
      t2_ratio_il12_il5_adj_L,
      t2_ratio_ifn_il5_adj_L,
      t2_ratio_il12_il13_adj_L,
      t2_ratio_ifn_il13_adj_L,
      
      t2_ratio_il12_il17_adj_L,
      t2_ratio_ifn_il17_adj_L,
      t2_ratio_il12_il21_adj_L,
      t2_ratio_ifn_il21_adj_L,
      
      t2_ratio_pro_il10_adj_L,
      t2_ratio_th1_il10_adj_L,
      t2_ratio_th2_il10_adj_L,
      t2_ratio_th17_il10_adj_L,
      t2_ratio_th1_th2_adj_L,
      t2_ratio_th1_th17_adj_L,
      
      
      t3_igf_adj_L,
      t3_gmc_adj_L,
      t3_ifn_adj_L,
      t3_il10_adj_L,
      t3_il12_adj_L,
      t3_il13_adj_L,
      t3_il17_adj_L,
      t3_il1_adj_L,
      t3_il2_adj_L,
      t3_il21_adj_L,
      t3_il4_adj_L,
      t3_il5_adj_L,
      t3_il6_adj_L,
      t3_tnf_adj_L,
      
      
      t3_ratio_gmc_il10_adj_L,
      t3_ratio_ifn_il10_adj_L,
      t3_ratio_il12_il10_adj_L,
      t3_ratio_il13_il10_adj_L,
      t3_ratio_il17_il10_adj_L,
      t3_ratio_il1_il10_adj_L,
      t3_ratio_il21_il10_adj_L,
      t3_ratio_il2_il10_adj_L,
      t3_ratio_il4_il10_adj_L,
      t3_ratio_il5_il10_adj_L,
      t3_ratio_il6_il10_adj_L,
      t3_ratio_tnf_il10_adj_L,
      
      t3_ratio_il12_il4_adj_L, 
      t3_ratio_ifn_il4_adj_L,
      t3_ratio_il12_il5_adj_L,
      t3_ratio_ifn_il5_adj_L,
      t3_ratio_il12_il13_adj_L,
      t3_ratio_ifn_il13_adj_L,
      
      t3_ratio_il12_il17_adj_L,
      t3_ratio_ifn_il17_adj_L,
      t3_ratio_il12_il21_adj_L,
      t3_ratio_ifn_il21_adj_L,
      
      t3_ratio_pro_il10_adj_L,
      t3_ratio_th1_il10_adj_L,
      t3_ratio_th2_il10_adj_L,
      t3_ratio_th17_il10_adj_L,
      t3_ratio_th1_th2_adj_L,
      t3_ratio_th1_th17_adj_L,
      
      d23_ln_igf_adj_L,
      d23_ln_gmc_adj_L,
      d23_ln_ifn_adj_L,
      d23_ln_il10_adj_L, 
      d23_ln_il12_adj_L,
      d23_ln_il13_adj_L,
      d23_ln_il17_adj_L,
      d23_ln_il1_adj_L,
      d23_ln_il2_adj_L,
      d23_ln_il21_adj_L, 
      d23_ln_il4_adj_L,
      d23_ln_il5_adj_L,
      d23_ln_il6_adj_L, 
      d23_ln_tnf_adj_L,
      
      d23_ratio_gmc_il10_adj_L,
      d23_ratio_ifn_il10_adj_L,  
      d23_ratio_il12_il10_adj_L,
      d23_ratio_il13_il10_adj_L,
      d23_ratio_il17_il10_adj_L,
      d23_ratio_il1_il10_adj_L,
      d23_ratio_il21_il10_adj_L,
      d23_ratio_il2_il10_adj_L,
      d23_ratio_il4_il10_adj_L,
      d23_ratio_il5_il10_adj_L,
      d23_ratio_il6_il10_adj_L,
      d23_ratio_tnf_il10_adj_L,
      
      d23_ratio_il12_il4_adj_L, 
      d23_ratio_ifn_il4_adj_L,
      d23_ratio_il12_il5_adj_L,  
      d23_ratio_ifn_il5_adj_L,
      d23_ratio_il12_il13_adj_L, 
      d23_ratio_ifn_il13_adj_L, 
      
      d23_ratio_il12_il17_adj_L, 
      d23_ratio_ifn_il17_adj_L,
      d23_ratio_il12_il21_adj_L,
      d23_ratio_ifn_il21_adj_L, 
      
      d23_ratio_pro_il10_adj_L, 
      d23_ratio_th1_il10_adj_L, 
      d23_ratio_th2_il10_adj_L,
      d23_ratio_th17_il10_adj_L,
      d23_ratio_th1_th2_adj_L,
      d23_ratio_th1_th17_adj_L, file=here("audrie results/immune_adj_glm.RData"))

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

