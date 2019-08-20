
#---------------------------------------
# EE-BD-telo-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# telomere manuscript table 1
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Nutrition + WSH"))
levels(treatment$tr)

#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

#Load in telomere datasets to track all children 
#who participated in the telomere substudy
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")

#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)

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

table.dat<-subset(d, select=c("tr", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH")


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
     file="telo_table1.Rdata")




#Create supplimentary table 1
#Merge in outcomes

suppd1<-subset(d, !is.na(TS2))
suppd2<-subset(d, !is.na(TS2) & is.na(TS3))
dim(suppd1)
dim(suppd2)

suppd1$col<-1
suppd2$col<-2



#Telomere substudy enrolled at Year 1
s1.table.dat<-subset(suppd1, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH") #%>% subset(., select= -tr)
#Telomere substudy lost to follow-up at Year 2
s2.table.dat<-subset(suppd2, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH") #%>% subset(., select= -tr)

#combine:
s.table.dat<-rbind(s1.table.dat,s2.table.dat)
head(s.table.dat)

#Change factors to indicators
for(i in 1:ncol(s.table.dat)){
  cat(colnames(s.table.dat)[i]," : ",class((s.table.dat[,i])),"\n")
}


#Supplimentary table 1 column 2
s.table1_mu<-s.table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_N<-s.table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_sd<-s.table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame


s.Ns<-table(s.table.dat$tr, s.table.dat$col)
s.Ns<-c(s.Ns[1,1],s.Ns[2,1],s.Ns[1,2],s.Ns[2,2])


s.balance.tab.mu_M<-s.table1_mu
s.balance.tab.n_M<-s.table1_N
s.balance.tab.sd_M<-s.table1_sd
s.balance.tab.mu_M[,1]<-s.Ns
s.balance.tab.n_M[,1]<-s.Ns
s.balance.tab.sd_M[,1]<-s.Ns
save(s.balance.tab.mu_M, s.balance.tab.n_M, s.balance.tab.sd_M, 
     file="telo_s.table1.Rdata")



ls()
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Audrie")
load("telomere_enrol_supp_baseline_char_tst2.Rdata")
load("telomere_enrol_supp_baseline_char_lost_tst3.Rdata")
tst2.supp.balance.tab.mu_L[-1,]-t(s.balance.tab.mu_M[1:2,-c(1:2)])
tst2.supp.balance.tab.n_L[-1,]-t(s.balance.tab.n_M[1:2,-c(1:2)])     
tst2.supp.balance.tab.sd_L[-1,]-t(s.balance.tab.sd_M[1:2,-c(1:2)]) 

lost.tst3.supp.balance.tab.mu_L[-1,]-t(s.balance.tab.mu_M[3:4,-c(1:2)])
lost.tst3.supp.balance.tab.n_L[-1,]-t(s.balance.tab.n_M[3:4,-c(1:2)])
lost.tst3.supp.balance.tab.sd_L[-1,]-t(s.balance.tab.sd_M[3:4,-c(1:2)])


