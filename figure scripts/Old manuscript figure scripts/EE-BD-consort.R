

#---------------------------------------
# EE-BD-consort.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Tabulating Consort numbers for the EE 
# substudy of the WASH Benefits Bangladesh
# trial.
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
options(dplyr.print_max = 1e9)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
consort<-read.csv("BD-EE-consort.csv")
colnames(consort)
consort<-consort %>% select(
miss1_ee,miss1reason_ee,svy,baseline_origin,dataid,          
childno,midline_origin,endline_origin,clusterid,       
block,maintrial_origin
)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr.Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d
# levels(treatment$tr)
# treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
# levels(treatment$tr)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool<-read.csv("BD-EE-stool.csv",stringsAsFactors = TRUE)
urine<-read.csv("BD-EE-urine.csv",stringsAsFactors = TRUE)

#Load in lab outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool.outcomes<-read.dta("BD-EE-stool-outcomes-Stata12.dta")
urine.outcomes<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")


#Merge in treatment data
dim(consort)
table(is.na(consort$clusterid))
d<-left_join(consort,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr, d$svy)



#Load in Audrie's data for comparison
setwd("C:/Users/andre/Downloads/")
aud<-read.dta("Audrie-EED-Consort.dta")

head(aud)
dim(d)
dim(aud)



#Tabulate number of compounds
d %>% group_by(svy, tr) %>%
  distinct(dataid) %>%
  summarize(n.compound=n())

#Tabulate number of clusterid
d %>% group_by(svy, tr) %>%
  distinct(clusterid) %>%
  summarize(n.clusterid=n())


#-----------------------------
#Subsample target
#-----------------------------

# Compounds
d %>% group_by(tr, svy) %>%
  distinct(clusterid) %>%
  summarize(n.clusters=n())

# Index children (any child in the enrollment record)
d %>% group_by(tr, svy) %>%
  #distinct(dataid, childno) %>%
  summarize(n.index=n())


aud %>% group_by(tr) %>%
  distinct(clusterid) %>%
  summarize(n.clusters=n())
aud %>% group_by(tr) %>%
  #distinct(dataid, childno) %>%
  summarize(n.index=n())

  

#-----------------------------
# Follow-up
#-----------------------------

#Arrange missingness reason
table(d$miss1reason_ee)
d$miss1reason_ee<-factor(d$miss1reason_ee, levels=c("Moved away","Absent","Withdrew","No live birth", "Child death", "Not lost"))

#Drop out non-missing
NotLost<-subset(d, miss1reason_ee=="Not lost")
followup<-subset(d, miss1reason_ee!="Not lost")


#Subset by round
followup.m3<-subset(followup, svy==1)
followup1<-subset(followup, svy==2)
followup2<-subset(followup, svy==3)

head(followup.m3)
miss.m3<-followup.m3 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss1<-followup1 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss2<-followup2 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())

print.data.frame(miss.m3)
print.data.frame(miss1)
print.data.frame(miss2)

#total lost to followup
followup.m3 %>% group_by(tr)  %>%
      summarize(N=n())
followup1 %>% group_by(tr)  %>%
      summarize(N=n())
followup2 %>% group_by(tr)  %>%
      summarize(N=n())


# Number new children added for each round

#Subset by round
dm3<-subset(d, svy==1)
d1<-subset(d, svy==2)
d2<-subset(d, svy==3)


anti_join(d1, dm3, by=c("dataid", "childno")) %>%
  group_by(tr) %>%
  summarize(new.children1=n())


anti_join(d2, d1, by=c("dataid", "childno")) %>%
  group_by(tr) %>%
  summarize(new.children2=n())

#-----------------------------
#Subsample enrollment
#-----------------------------

#clusters
NotLost %>% group_by(svy, tr) %>% distinct(clusterid) %>%summarize(N=n())

#Children
enrollchild<-NotLost %>% group_by(svy, tr) %>% distinct(dataid, childno) %>%summarize(N=n())
enrollchild




  
  
#-----------------------------
#Missing outcomes
#-----------------------------
  
# Index children with stool
stool.outcomes.m3<-  stool.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t1_aat) | !is.na(t1_mpo)| !is.na(t1_neo)) %>%
  group_by(tr) %>%
  summarize(index.N=n())

stool.outcomes1<-   stool.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t2_aat) | !is.na(t2_mpo) | !is.na(t2_neo) | !is.na(t2_reg)) %>%
  group_by(tr) %>%
  summarize(index.N=n())
  
stool.outcomes2<-   stool.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t3_aat) | !is.na(t3_mpo) | !is.na(t3_neo)) %>%
  group_by(tr) %>%
  summarize(index.N=n()) 
  
# Index children with urine
urine.outcomes.m3<-  urine.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(Mann1) | !is.na(Lact1)) %>%
  group_by(tr) %>%
  summarize(index.N=n())

urine.outcomes1<-  urine.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(Mann2) | !is.na(Lact2)) %>%
  group_by(tr) %>%
  summarize(index.N=n())

  
urine.outcomes2<-  urine.outcomes %>% mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid") %>%
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(Mann3) | !is.na(Lact3)) %>%
  group_by(tr) %>%
  summarize(index.N=n())
 


#Missing stool
enrollchild[,3]-c(stool.outcomes.m3$index.N, stool.outcomes1$index.N,stool.outcomes2$index.N)

#Missing urine
enrollchild[,3]-c(urine.outcomes.m3$index.N, urine.outcomes1$index.N,urine.outcomes2$index.N)




#-----------------------------
# Analysis row 
#-----------------------------
stool.d<-stool.outcomes %>%
  mutate(childid= as.numeric(childid)) %>%
  left_join(stool, ., by="childid")

urine.d<-urine.outcomes %>%
  mutate(childid= as.numeric(childid)) %>%
  left_join(urine, ., by="childid")

outcomes.m3<-  full_join(stool.d, urine.d, by=c("dataid", "childNo", "clusterid")) %>% 
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t1_aat) | !is.na(t1_mpo) | !is.na(t1_neo) | !is.na(Mann1) | !is.na(Lact1))

outcomes1<-  full_join(stool.d, urine.d, by=c("dataid", "childNo", "clusterid")) %>% 
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t2_aat) | !is.na(t2_mpo) | !is.na(t2_neo) | !is.na(Mann2) | !is.na(Lact2))

outcomes2<-  full_join(stool.d, urine.d, by=c("dataid", "childNo", "clusterid")) %>% 
  left_join(.,treatment, by="clusterid") %>%
  filter(!is.na(t3_aat) | !is.na(t3_mpo) | !is.na(t3_neo) | !is.na(Mann3) | !is.na(Lact3))

# Compounds
outcomes.m3 %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds.N=n())

outcomes1 %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds.N=n())

outcomes2 %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds.N=n())


# Index children
outcomes.m3 %>%
  group_by(tr) %>% 
  summarize(compounds.N=n())

outcomes1 %>%
  group_by(tr) %>% 
  summarize(compounds.N=n())

outcomes2 %>%
  group_by(tr) %>% 
  summarize(compounds.N=n())


  