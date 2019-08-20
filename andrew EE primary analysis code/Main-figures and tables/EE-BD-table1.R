
#---------------------------------------
# EE-BD-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# manuscript table 1
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
# load("washb-BD-EE-blind-tr.Rdata")
 levels(treatment$tr)
 treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
 levels(treatment$tr)

#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

#Load in urine, stool, and medhistory datasets to track all children 
#who participated in the eed substudy
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("BD-EE-urine.csv", stringsAsFactors = TRUE)
stool<-read.csv("BD-EE-stool.csv", stringsAsFactors = TRUE)
medhistory<-read.csv("BD-EE-medhistory.csv", stringsAsFactors = TRUE)

#Select non-duplicate childids:
urine<-urine %>%
  mutate(urine_data1=!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1 | !is.na(preLMaliqout13_t1) & preLMaliqout13_t1>1) %>%
  mutate(urine_data2=!is.na(h2aliqout1_t2) & h2aliqout1_t2>1 | !is.na(h5aliqout7_t2) & h5aliqout7_t2>1 | !is.na(preLMaliqout13_t2) & preLMaliqout13_t2>1) %>%
  mutate(urine_data3=!is.na(h2aliqout1_t3) & h2aliqout1_t3>1 | !is.na(h5aliqout7_t3) & h5aliqout7_t3>1 | !is.na(preLMaliqout13_t3) & preLMaliqout13_t3>1) %>%
  mutate(urine_data= urine_data1 | urine_data2 | urine_data3) %>%
  subset(urine_data==T) %>%
  select(childid, dataid, clusterid, childNo) %>%
  distinct(childid, dataid, clusterid, childNo)

stool<-stool %>% 
  mutate(stool_data=!is.na(aliqout1_t1) & aliqout1_t1>1 | !is.na(aliqout1_t2) & aliqout1_t2>1 | !is.na(aliqout1_t3) & aliqout1_t3>1) %>%
  subset(stool_data==T) %>%
  select(childid, dataid, clusterid, childNo) %>%
  distinct(childid, dataid, clusterid, childNo)

medhistory<-medhistory %>%
  #subset(!is.na(consent1) | !is.na(consent2) | !is.na(consent3)) %>%
  subset((consent1==1) | (consent2==1) | (consent3==1)) %>%
  select(childid, dataid, clusterid, childNo) %>%
  distinct(childid, dataid, clusterid, childNo)

childid<-union(urine, stool)
childid<-union(childid, medhistory)


#Merge treatment information 
dim(childid)
d<-left_join(childid,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by="dataid")
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 


#Generate table 1
colnames(d)


#Drop twins so HH characteristics aren't duplicates
d<-subset(d, childNo!=2)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#Generate table 1
colnames(d)
d$foodsecure<-ifelse(d$hfiacat=="Food Secure", 1,0)


vlist <- c("momage","momeduy","dadeduy","dadagri","Nhh","elec","cement","landacre","tubewell","storewat","treatwat","watmin","odmen","odwom","odch815","odch38","odchu3",
           "latown","latslab","latseal","latfeces","potty","humfeces","humfecesch", "hwlatwat","hwlatsoap","hwkitwat","hwkitsoap","foodsecure")


table(vlist %in% colnames(d))

table.dat<-subset(d, select=c("tr", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")


#Change factors to indicators
for(i in 1:ncol(table.dat)){
  cat(colnames(table.dat)[i]," : ",class((table.dat[,i])),"\n")
}

#Flip the latfeces variables so that it indicates visible feces rather than no visible feces
table(table.dat$latfeces)
table.dat$latfeces[table.dat$latfeces==1 & !is.na(table.dat$latfeces)] <- table.dat$latfeces[table.dat$latfeces==1 & !is.na(table.dat$latfeces)] - 2
table.dat$latfeces <- table.dat$latfeces +1
table(table.dat$latfeces)


#Calculate number of compounds

table1_mu<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_N<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_sd<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

Ns<-table(table.dat$tr)
balance.tab.mu_M<-rbind(Ns,t((table1_mu[,2:ncol(table1_mu)])))
balance.tab.n_M<-rbind(Ns,t((table1_N[,2:ncol(table1_N)])))
balance.tab.sd_M<-rbind(Ns,t((table1_sd[,2:ncol(table1_sd)])))


#save objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(balance.tab.mu_M, balance.tab.n_M, balance.tab.sd_M, 
     file="EE-BD-table1.Rdata")




#Create supplimentary table 1
#Merge in outcomes


#Create supplimentary table 1
#Merge in outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine.outcomes<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")
stool.outcomes<-read.dta("BD-EE-stool-outcomes-Stata12.dta")
urine.outcomes$childid<-as.numeric(urine.outcomes$childid)
stool.outcomes$childid<-as.numeric(stool.outcomes$childid)



d<-left_join(d,urine.outcomes, by="childid")
d<-left_join(d,stool.outcomes, by="childid")


overallN1<-d%>% 
  subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) %>% 
  summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

suppd1<-d %>% 
    subset(is.na(d$Lact2) & is.na(d$Mann2)&is.na(d$t2_aat)&is.na(d$t2_mpo)&is.na(d$t2_neo))
dim(suppd1)


suppd2<-d %>% 
    subset(is.na(d$Lact3) & is.na(d$Mann3) & is.na(d$t3_aat) & is.na(d$t3_mpo) & is.na(d$t3_neo))
dim(suppd2) 
  

dim(suppd1)
dim(suppd2)

d$col<-1
suppd1$col<-2
suppd2$col<-3


table.dat<-subset(d, select=c("tr","col",vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Telomere substudy enrolled at Year 1
s1.table.dat<-subset(suppd1, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Telomere substudy lost to follow-up at Year 2
s2.table.dat<-subset(suppd2, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)

#combine:
s.table.dat<-rbind(table.dat,s1.table.dat,s2.table.dat)
head(s.table.dat)

#Change factors to indicators
for(i in 1:ncol(s.table.dat)){
  cat(colnames(s.table.dat)[i]," : ",class((s.table.dat[,i])),"\n")
}

s.table.dat$hfiacat<-ifelse(s.table.dat$hfiacat=="Severely Food Insecure" | s.table.dat$hfiacat=="Moderately Food Insecure", 1,0)

#Supplimentary table 1 
s.table1_mu<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_N<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_sd<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame


s.Ns<-table(s.table.dat$col)
s.Ns<-c(s.Ns[1],s.Ns[2],s.Ns[3])


s.balance.tab.mu_M<-s.table1_mu
s.balance.tab.n_M<-s.table1_N
s.balance.tab.sd_M<-s.table1_sd
s.balance.tab.mu_M[,1]<-s.Ns
s.balance.tab.n_M[,1]<-s.Ns
s.balance.tab.sd_M[,1]<-s.Ns

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.balance.tab.mu_M, s.balance.tab.n_M, s.balance.tab.sd_M, 
     file="EE-BD_s.table1.Rdata")


