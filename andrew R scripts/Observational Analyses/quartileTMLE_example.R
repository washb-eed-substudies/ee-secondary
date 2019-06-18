

rm(list=ls())
library(tidyverse)
library(haven)
library(washb)
library(data.table)
library(tmle)
library(tmleAb)

source("C:/Users/andre/Documents/WASHB_EE_Analysis/WBB EE Analysis/R EE outcome code/Observational Analyses/quartileTMLE_functions.R")

#load immune outcomes
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-immune.Rdata")


#Unadjusted, with continious outcome

res <- tmle_quart(dat=d, 
                    Y="igf_t3", 
                    W=NULL, 
                    A="momage", 
                    id="block",
                    Alevels=c("Q1","Q2","Q3","Q4"), 
                    family="gaussian", 
                    SLlibrary="SL.gam")
res

#Unadjusted, with binary outcome (Calculates both risk difference and risk ratio)

res_bin <- tmle_quart(dat=d, 
                  Y="roof", 
                  W=NULL, 
                  A="momage", 
                  id="block",
                  Alevels=c("Q1","Q2","Q3","Q4"), 
                  family="binomial", 
                  SLlibrary="SL.gam")
res_bin




#Unadjusted, with continious outcome
Wvars<-c("sex","birthord", "momheight","momedu", 
         "hfiacat", "Nlt18")

res_adj <- tmle_quart(dat=d, 
                  Y="igf_t3", 
                  W=Wvars, 
                  A="momage", 
                  id="block",
                  Alevels=c("Q1","Q2","Q3","Q4"), 
                  family="gaussian", 
                  SLlibrary="SL.gam")
res_adj




#For saving and plotting many analyses, I think it's way easier to deal with a single well organized dataframe. 
#(Previously for plotting, I was writing seperate code to bind together all the different objects)
#You can pass a data.frame to the function through the `outputdf` argument and append the results to it so that it is easy to concatenate all results
#(If all the outcomes are the same family: gaussian or binomial)

#Null data.frame
full.res = NULL

#First estimate
full.res <- tmle_quart(dat=d, 
                  Y="igf_t3", 
                  W=NULL, 
                  A="momage", 
                  id="block",
                  Alevels=c("Q1","Q2","Q3","Q4"), 
                  outputdf = full.res,
                  family="gaussian", 
                  SLlibrary="SL.gam")

#Append second estimate
full.res <- tmle_quart(dat=d, 
                       Y="il1_t3", 
                       W=NULL, 
                       A="momage", 
                       id="block",
                       Alevels=c("Q1","Q2","Q3","Q4"), 
                       outputdf = full.res,
                       family="gaussian", 
                       SLlibrary="SL.gam")

full.res



#for-loop approach to looping through outcomes and exposures
Avars <- c("momage", "momheight")
Yvars <- c("igf_t3", "il1_t3")
full.res2 = NULL

for(i in Avars){
  for(j in Yvars){
    full.res2 <- tmle_quart(dat=d, 
                           Y=i, 
                           W=NULL, 
                           A=j, 
                           id="block",
                           Alevels=c("Q1","Q2","Q3","Q4"), 
                           outputdf = full.res2,
                           family="gaussian", 
                           SLlibrary="SL.gam")
  }  
}

full.res2






#Calculating splines (unadjusted)
#NOTE! I'm currently having trouble getting the ID argument to work, so 
#Currently not using it... will fix, should not greatly affect estimates
gam.res <- GAM_simulCI(Y=d$igf_t2, X=d$momage, W = NULL, id = "block")



#Example plot (I will write a function for prettier plots, but for initial visualization)
p <- ggplot(gam.res,aes(x = X)) +
  geom_smooth(aes(y = fit), se = F) +
  geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.1)
p


#Note - the confidence intervals look terrible because I'm not predicting values frequently enough (rather than them being wrong)
#Will fix in the function code.

