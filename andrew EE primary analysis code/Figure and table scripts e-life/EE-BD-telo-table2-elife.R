
#---------------------------------------
# EE-BD-telo-table2-elife.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# telomere manuscript table 2 - elife format
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


#Merge in outcomes

suppd1<-subset(d, !is.na(TS2))
suppd2<-subset(d, !is.na(TS2) & is.na(TS3))
dim(suppd1)
dim(suppd2)

suppd1$col<-1
suppd2$col<-2

#Number of children in each subset
suppd1 %>% group_by(tr) %>% distinct(dataid) %>% summarise(N=n())
suppd2 %>% group_by(tr) %>% distinct(dataid) %>% summarise(N=n())

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




#--------------------------------
#Table S1 (eLife table 2)
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_s.table1.Rdata")


s.balance.tab.mu_M 
s.balance.tab.n_M 
s.balance.tab.sd_M
s.balance.tab.mu_M<-t(s.balance.tab.mu_M[,-2])
s.balance.tab.n_M <-t(s.balance.tab.n_M[,-2])
s.balance.tab.sd_M<-t(s.balance.tab.sd_M[,-2]) 
s.balance.tab.mu_M 
s.balance.tab.n_M 
s.balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

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




#Telomere substudy: Had telomere outcomes at Year 1.
tab<-table1_create(mu=s.balance.tab.mu_M, 
                   n=s.balance.tab.n_M, 
                   sd=s.balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab

#Telomere substudy: Lost to follow-up at Year 2 (from those who had telomere outcomes at Year 1)
tab2<-table1_create(mu=s.balance.tab.mu_M[,3:4], 
                   n=s.balance.tab.n_M[,3:4], 
                   sd=s.balance.tab.sd_M[,3:4], 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)

# 
# main.control<-c("1382",
#                 "24 (5)",
#                 "6 (3)",
#                 "5 (4)",
#                 "414 (30\\%)",
#                 "5 (2)",
#                 "784 (57\\%)",
#                 "145 (10\\%)",
#                 "0.15 (0.21)",
#                 "1038 (75\\%)",
#                 "666 (48\\%)",
#                 "4 (0\\%)",
#                 "1 (1)",
#                 "97 (7\\%)",
#                 "62 (4\\%)",
#                 "53 (10\\%)",
#                 "267 (38\\%)",
#                 "245 (82\\%)",
#                 "750 (54\\%)",
#                 "1251 (95\\%)",
#                 "358 (31\\%)",
#                 "625 (48\\%)",
#                 "61 (4\\%)",
#                 "114 (8\\%)",
#                 "21 (2\\%)",
#                 "178 (14\\%)",
#                 "88 (7\\%)",
#                 "118 (9\\%)",
#                 "33 (3\\%)",
#                 "932 (67\\%)"
#                 ) 
# 
# main.WSHN<-c("686",
#                 "24 (6)",
#                 "6 (3)",
#                 "5 (4)",
#                 "207 (30\\%)",
#                 "5 (2)",
#                 "412 (60\\%)",
#                 "72 (10\\%)",
#                 "0.14 (0.38)",
#                 "504 (73\\%)",
#                 "331 (48\\%)",
#                 "2 (0\\%)",
#                 "1 (2)",
#                 "50 (7\\%)",
#                 "24 (4\\%)",
#                 "28 (10\\%)",
#                 "134 (37\\%)",
#                 "123 (88\\%)",
#                 "367 (53\\%)",
#                 "621 (94\\%)",
#                 "155 (27\\%)",
#                 "298 (46\\%)",
#                 "30 (4\\%)",
#                 "49 (7\\%)",
#                 "7 (1\\%)",
#                 "72 (11\\%)",
#                 "36 (6\\%)",
#                 "60 (9\\%)",
#                 "18 (3\\%)",
#                 "485 (71\\%)"
#                 ) 


main.control<-c("1382",
                "24 (5)",
                "6 (3)",
                "5 (4)",
                "30\\%",
                "5 (2)",
                "57\\%",
                "10\\%",
                "0.15 (0.21)",
                "75\\%",
                "48\\%",
                "0\\%",
                "1 (1)",
                "7\\%",
                "4\\%",
                "10\\%",
                "38\\%",
                "82\\%",
                "54\\%",
                "95\\%",
                "31\\%",
                "48\\%",
                "4\\%",
                "8\\%",
                "2\\%",
                "14\\%",
                "7\\%",
                "9\\%",
                "3\\%",
                "67\\%"
                ) 

main.WSHN<-c("686",
                "24 (6)",
                "6 (3)",
                "5 (4)",
                "30\\%",
                "5 (2)",
                "60\\%",
                "10\\%",
                "0.14 (0.38)",
                "73\\%",
                "48\\%",
                "0\\%",
                "1 (2)",
                "7\\%",
                "4\\%",
                "10\\%",
                "37\\%",
                "88\\%",
                "53\\%",
                "94\\%",
                "27\\%",
                "46\\%",
                "4\\%",
                "7\\%",
                "1\\%",
                "11\\%",
                "6\\%",
                "9\\%",
                "3\\%",
                "71\\%"
                ) 

dim(tab)
dim(tab2)
length(main.control)
length(main.WSHN)

tab<-cbind(tab[,1],main.control,main.WSHN,tab[,2:3],tab2[,2:3])


#Add in variable group labels
blank=rep("",8)

s.n.comp.f<-tab[1,]
tab<-tab[-1,]

s.table1_f=   rbind(
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

rownames(s.table1_f)=NULL

s.table1_f


for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  s.table1_f[i,1]=paste("~~~",s.table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,31,32,35:36,38:39)){
  s.table1_f[i,1]=paste("~~~~~",s.table1_f[i,1],sep="")
}



#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.table1_f, file="s.table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(s.table1_f,digits=0)



