
#---------------------------------------
# EE-BD-telo-table1-elife.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# telomere manuscript table 1 - elife format
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)
library(reshape2)
library(xtable)



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

d %>% group_by(tr) %>% distinct(dataid) %>% summarise(N=n())


vlist <- c("momage","momeduy","dadeduy","dadagri","Nhh","elec","cement","landacre","tubewell","storewat","treatwat","watmin","odmen","odwom","odch815","odch38","odchu3",
           "latown","latslab","latseal","latfeces","potty","humfeces","humfecesch", "hwlatwat","hwlatsoap","hwkitwat","hwkitsoap","foodsecure")


d1<-subset(d, !is.na(TS2))
d2<-subset(d, !is.na(TS3))
d1$col<-1
d2$col<-2

#Number of children in each subset
d1 %>% group_by(tr) %>% distinct(dataid) %>% summarise(N=n())
d2 %>% group_by(tr) %>% distinct(dataid) %>% summarise(N=n())


table(vlist %in% colnames(d))

table.dat1<-subset(d1, select=c("tr", "col", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH")
table.dat2<-subset(d2, select=c("tr", "col", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH")

#combine:
table.dat<-rbind(table.dat1,table.dat2)
head(table.dat)

#Change factors to indicators
for(i in 1:ncol(table.dat)){
  cat(colnames(table.dat)[i]," : ",class((table.dat[,i])),"\n")
}


#Calculate number of compounds

table1_mu<-table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_N<-table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_sd<-table.dat%>%
        group_by(col, tr) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

Ns<-table(table.dat$tr, table.dat$col)
Ns<-c(Ns[1,1],Ns[2,1],Ns[1,2],Ns[2,2])


balance.tab.mu_M<-rbind(Ns,t((table1_mu[,3:ncol(table1_mu)])))
balance.tab.n_M<-rbind(Ns,t((table1_N[,3:ncol(table1_N)])))
balance.tab.sd_M<-rbind(Ns,t((table1_sd[,3:ncol(table1_sd)])))


#save objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(balance.tab.mu_M, balance.tab.n_M, balance.tab.sd_M, 
     file="telo_table1.Rdata")


#---------------------------------------
# Table printing
#---------------------------------------


# Table functions
cleantable <- function(x,digits) {
 print( xtable(x,digits=digits),
        sanitize.text.function=function(y){y},
        floating=FALSE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        only.contents=TRUE,
        hline.after=NULL
 )
}

pt.est.n.f=function(obj,decimals,scale){
  pts=obj[1]*scale
  pt=sprintf(paste("%0.0",decimals,"f",sep=""),pts)
  a=paste(pt, " (",obj[4],")",sep="")
  return(a)
}

pt.est.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  return(a)
}

per.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  a=paste(as.numeric(a),"\\%",sep="")
  return(a)
}


ci.f=function(obj,decimals,scale){
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste("(",b,", ",c,")",sep=""))
}

pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}


table1_create<-function(mu, n, sd, mean.ind, vargroup, vargroup.row, Rownames, round){
  dat<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2])
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],(mu[j,1])*100,n[j,2],(mu[j,2])*100)
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)

  col1<-NULL
  col2<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],")",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)           
        }else{
                  #Drop N's
      #temp1<-cbind(paste(dat[j,1]," (",dat[j,2],"\\%)",sep=""))
      #temp2<-cbind(paste(dat[j,3]," (",dat[j,4],"\\%)",sep=""))
      
      temp1<-cbind(paste(dat[j,2],"\\%",sep=""))
      temp2<-cbind(paste(dat[j,4],"\\%",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
        }
      }
  } 
  dat<-cbind(Rownames, col1, col2)
  colnames(dat)=c(" ","Control","Nutrition + WSH")
  return(dat)
}


#Function to round 0.5 away from zero (differs from R's default behavior)
round2 <- function(x, n=3) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

rnd<- function(x, n=2) {
  x<-round2(x,3)
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#Test function 
x<-0.034833066 
rnd(1.555)
rnd(x)

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_table1.Rdata")


balance.tab.mu_M*100 
balance.tab.n_M 
balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

rownames(balance.tab.mu_M)[which(mean.ind==1)]
rownames(balance.tab.mu_M)[which(mean.ind!=1)]
balance.tab.n_M[which(mean.ind!=1),]
#vargroup<-c(0)
#vargroup.row<-c(2,5,8,13,20,21,27,32,33,36,37,40,43)
Rownames<-c("No. of compounds",
            "Age (years)",
  "Years of education",
  "Years of education",
  "Works in agriculture",
  "Number of persons"
  ,"Has electricity"
  ,"Has a cement floor"
  ,"Acres of agricultural land owned",
      "Shallow tubewell primary water source",
    "Stored water observed at home",
    "Reported treating water yesterday",
    "Distance (mins) to primary water source",
        "Adult men",
        "Adult women",
        "Children: 8-\\textless 15 years",
        "Children: 3-\\textless 8 years",
        "Children: 0-\\textless 3 years",
        "Owned",
        "Concrete slab",
        "Functional water seal",
        "Visible stool on slab or floor",
        "Owned a potty",
        "House",
        "Child's play area",
        "Has water",
        "Has soap",
        "Has water",
        "Has soap",
    "Household is food secure"
  )



table1_create<-function(mu, n, sd, mean.ind, vargroup, vargroup.row, Rownames, round){
  dat<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2])
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],(mu[j,1])*100,n[j,2],(mu[j,2])*100)
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)

  col1<-NULL
  col2<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],")",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)           
        }else{
                        #Drop N's
      #temp1<-cbind(paste(dat[j,1]," (",dat[j,2],"\\%)",sep=""))
      #temp2<-cbind(paste(dat[j,3]," (",dat[j,4],"\\%)",sep=""))
      
      temp1<-cbind(paste(dat[j,2],"\\%",sep=""))
      temp2<-cbind(paste(dat[j,4],"\\%",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
        }
      }
  } 
  dat<-cbind(Rownames, col1, col2)
  colnames(dat)=c(" ","Control","Nutrition + WSH")
  return(dat)
}


#Telomere substudy: Year 1
tab<-table1_create(mu=balance.tab.mu_M, 
                   n=balance.tab.n_M, 
                   sd=balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab

#Telomere substudy: Year 2 
tab2<-table1_create(mu=balance.tab.mu_M[,3:4], 
                   n=balance.tab.n_M[,3:4], 
                   sd=balance.tab.sd_M[,3:4], 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab2


tab<-cbind(tab,tab2[,2:3])


#Add in variable group labels
blank=rep("",8)

n.comp.f<-tab[1,]
tab<-tab[-1,]

table1_f=   rbind(
               c("\\textbf{Maternal}",blank,blank),
               tab[c(1:2),],
               c( "\\textbf{Paternal}",blank,blank),
               tab[c(3:4),],
               c("\\textbf{Household}",blank,blank),
               tab[c(5:8),],
               c("\\textbf{Drinking Water}",blank,blank),
               tab[c(9:12),],
               c("\\textbf{Sanitation}",blank,blank),
               c("Reported daily open defecation",blank,blank),
               tab[c(13:17),],
               c("Latrine",blank,blank),
               tab[c(18:22),],
               c("Human feces observed in the",blank,blank),
               tab[c(23:24),],
               c("\\textbf{Handwashing}",blank,blank),
               c("Within 6 steps of latrine",blank,blank),
               tab[c(25:26),],
               c("Within 6 steps of kitchen",blank,blank),
               tab[c(27:28),],
               c("\\textbf{Nutrition}",blank,blank),
               tab[c(29),])

rownames(table1_f)=NULL

table1_f
n.comp.f

n.comp.f[2]=paste(" (N=",n.comp.f[2],")",sep="")
n.comp.f[3]=paste(" (N=",n.comp.f[3],")",sep="")
n.comp.f<-n.comp.f[2:3]
colnames(n.comp.f)<-NULL


for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  table1_f[i,1]=paste("~~~",table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,31,32,35:36,38:39)){
  table1_f[i,1]=paste("~~~~~",table1_f[i,1],sep="")
}





setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(n.comp.f, table1_f, file="table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(table1_f,digits=0)


