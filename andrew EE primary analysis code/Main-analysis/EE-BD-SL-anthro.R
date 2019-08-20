
###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
levels(treatment$tr)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")
anthro<-read.csv("BD-EE-anthro.csv",stringsAsFactors = TRUE)



#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 

 

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 


#Merge in anthropometry measures
head(anthro)
dim(d)
dim(anthro)
d<-left_join(d, anthro, by=c("dataid", "childNo"))
dim(d)


#9.5 Observational analyses nested within the existing trial
#Each specific association that we measure between an exposure (e.g., telomere length at
#12 months after intervention) and an outcome (e.g., length-for-age Z-scores (LAZ) measured 
#at 24 months after intervention) will require its own, unique analysis, which could be 
#complicated by non-linear or other complex relationships between the exposure and outcome. 
#We will adopt the following general approach in each case, recognizing that it will be tailored
#to each specific analysis. First, we will conduct exploratory data analyses that plot the relationship 
#between telomere exposures and length outcomes and summarize the patterns between them using non-parametric 
#smoothed fits with an ensemble approach called Ssuper learner18 that uses cross-validation to optimally 
#combine different models in a library, and we will include the following library in the ensemble: the simple mean, 
#linear models, locally weighted regression (lowess), and natural smoothing splines (generalized additive models).
#In concert with this visual examination of each exposure/outcome relationship, we will test for the bivariate 
#association between the exposure and outcome using a non-parametric Spearman's rank correlation test with 
#a permutation-based test to determine if it differs from zero. Since the relationship between telomere 
#length and subsequent growth could be nonlinear, we will summarize unadjusted and adjusted mean LAZ by quartiles 
#of telomere length.  We will estimated adjusted means in each quartile and their difference using targeted 
#maximum likelihood estimation (TMLE), which will allow us to flexibly adjust for potential confounding covariates
#in section 9.2 using the super learner ensemble, described above19. We will first stratify this analysis by 
#intervention arm, and then combine the data across arms if the relationships are similar. 




