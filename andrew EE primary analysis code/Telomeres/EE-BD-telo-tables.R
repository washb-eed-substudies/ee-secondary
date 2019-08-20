
#---------------------------------------
# EE-BD-telo-tables.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Create R-objects for xtable printing
#---------------------------------------




rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)
library(xtable)


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
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],"\\%)",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],"\\%)",sep=""))
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

tab<-table1_create(mu=balance.tab.mu_M, 
                   n=balance.tab.n_M, 
                   sd=balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab



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


#--------------------------------
#Table 2 (eLife table 3)
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_res.Rdata")
ts_t2_N_M
ts_t3_N_M
ts_t2_unadj_M
ts_t3_unadj_M
ts_t2_adj_sex_age_M
ts_t3_adj_sex_age_M
ts_t2_adj_M
ts_t3_adj_M
delta_ts_N_M
delta_ts_unadj_M
delta_ts_adj_sex_age_M
delta_ts_adj_M

age_t2_blood_M
age_t3_blood_M
 
glm_print<-function(obj, t=F){
  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }
  flag=F
  if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
                              flag=T}
  if(flag==F){
  obj<-t(as.matrix(c(sprintf("%1.2f",obj[c(1:3)]),
                     sprintf("%1.3f",obj[c(6)]))))    
  }else{
   obj<-t(as.matrix(c(sprintf("%1.2f",obj[c(1:3)]),
                     obj[c(6)])))       
  }

  #rownames(obj)<-colnames(obj)<-NULL
  out<-paste(obj[1],"(",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(out)
}

tab2<-data.frame(rbind(ts_t2_N_M,ts_t3_N_M,delta_ts_N_M))
tab2[,3]<-rnd(tab2[,3],2)
tab2[,4:6]<-matrix("",6,3)
tab2[2,4]<-glm_print(ts_t2_unadj_M)
tab2[4,4]<-glm_print(ts_t3_unadj_M)
tab2[6,4]<-glm_print(delta_ts_unadj_M)
tab2[2,5]<-glm_print(ts_t2_adj_sex_age_M)
tab2[4,5]<-glm_print(ts_t3_adj_sex_age_M)
tab2[6,5]<-glm_print(delta_ts_adj_sex_age_M)
tab2[2,6]<-glm_print(ts_t2_adj_M)
tab2[4,6]<-glm_print(ts_t3_adj_M)
tab2[6,6]<-glm_print(delta_ts_adj_M)
colnames(tab2)<-NULL

tab2[,1]<-as.character(tab2[,1])
tab2[c(1,3,5),1]<-rep("~~~Control",3)
tab2[c(2,4,6),1]<-rep("~~~N+WSH",3)

blank=rep("",5)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("\\textbf{After 1 year of intervention}",blank)),
            t(c("\\textbf{(age \\textasciitilde 14 months)}",blank)),
            tab2[1:2,],
            t(c("\\textbf{After 2 years of intervention}",blank)),
            t(c("\\textbf{(age \\textasciitilde 28 months)}",blank)),
            tab2[3:4,],
            t(c("\\textbf{Change in Telomere length}",blank)),
            t(c("\\textbf{between year 1 and 2}",blank)),
            tab2[5:6,])

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab2, file="table2.RData")

cleantable(tab2, 2)


#--------------------------------
#Table 3 -Effect modification by gender
# (eLife table 4)
#--------------------------------

ts_t2_N_subgroup_M
ts_t3_N_subgroup_M
delta_ts_N_subgroup_M
delta_ts_subgroup_M
ts_t2_subgroup_M
ts_t3_subgroup_M




tab3<-data.frame(rbind(ts_t2_N_subgroup_M,ts_t3_N_subgroup_M,delta_ts_N_subgroup_M))
tab3[,4]<-rnd(tab3[,4],2)
tab3[,5]<-matrix("",12,1)
tab3[2,5]<-glm_print(ts_t2_subgroup_M[1,-c(1)])
tab3[4,5]<-glm_print(ts_t2_subgroup_M[2,-c(1)])
tab3[6,5]<-glm_print(ts_t3_subgroup_M[1,-c(1)])
tab3[8,5]<-glm_print(ts_t3_subgroup_M[2,-c(1)])
tab3[10,5]<-glm_print(delta_ts_subgroup_M[1,-c(1)])
tab3[12,5]<-glm_print(delta_ts_subgroup_M[2,-c(1)])
colnames(tab3)<-NULL

tab3[,1]<-as.character(tab3[,1])
tab3[,2]<-as.character(tab3[,2])
tab3[c(2,4,6,8,10,12),2]<-rep("N + WSH",3)
#tab3[c(1,3,5,7,8,11),1]<-rep("~~~Control",3)


tab3[tab3[,1]=="female",1]<-"Female, "
tab3[tab3[,1]=="male",1]<-"Male, "

tab3[,2]<-paste0(tab3[,1],tab3[,2])
tab3[,1]<-""

blank=rep("",4)
tab3<-as.matrix(tab3)
tab3<-rbind(t(c("\\textbf{After 1 year of intervention (age \\textasciitilde 14 months)}",blank)),
            tab3[1:4,],
            t(c("\\textbf{After 2 years of intervention (age \\textasciitilde 28 months)}",blank)),
            tab3[5:8,],
            t(c("\\textbf{Change in telomere length}",blank)),
            tab3[9:12,])

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab3, file="table3.RData")

cleantable(tab3, 2)


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


main.control<-c("1382",
                "24 (5)",
                "6 (3)",
                "5 (4)",
                "414 (30\\%)",
                "5 (2)",
                "784 (57\\%)",
                "145 (10\\%)",
                "0.15 (0.21)",
                "1038 (75\\%)",
                "666 (48\\%)",
                "4 (0\\%)",
                "1 (1)",
                "97 (7\\%)",
                "62 (4\\%)",
                "53 (10\\%)",
                "267 (38\\%)",
                "245 (82\\%)",
                "750 (54\\%)",
                "1251 (95\\%)",
                "358 (31\\%)",
                "625 (48\\%)",
                "61 (4\\%)",
                "114 (8\\%)",
                "21 (2\\%)",
                "178 (14\\%)",
                "88 (7\\%)",
                "118 (9\\%)",
                "33 (3\\%)",
                "932 (67\\%)"
                ) 

main.WSHN<-c("686",
                "24 (6)",
                "6 (3)",
                "5 (4)",
                "207 (30\\%)",
                "5 (2)",
                "412 (60\\%)",
                "72 (10\\%)",
                "0.14 (0.38)",
                "504 (73\\%)",
                "331 (48\\%)",
                "2 (0\\%)",
                "1 (2)",
                "50 (7\\%)",
                "24 (4\\%)",
                "28 (10\\%)",
                "134 (37\\%)",
                "123 (88\\%)",
                "367 (53\\%)",
                "621 (94\\%)",
                "155 (27\\%)",
                "298 (46\\%)",
                "30 (4\\%)",
                "49 (7\\%)",
                "7 (1\\%)",
                "72 (11\\%)",
                "36 (6\\%)",
                "60 (9\\%)",
                "18 (3\\%)",
                "485 (71\\%)"
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
s.n.comp.f

n.comp.f[2]=paste("(N=",n.comp.f[2],")",sep="")
n.comp.f[3]=paste("(N=",n.comp.f[3],")",sep="")
n.comp.f<-n.comp.f[2:3]
colnames(n.comp.f)<-NULL


#FIX:
for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  s.table1_f[i,1]=paste("~~~",table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,31,32,35:36,38:39)){
  s.table1_f[i,1]=paste("~~~~~",table1_f[i,1],sep="")
}



#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.n.comp.f, s.table1_f, file="s.table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(s.table1_f,digits=0)






#--------------------------------
#Table S2 (eLife table 5)
#--------------------------------
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_ipcw_res.Rdata")

ts_t2_adj_ipcw_M
ts_t3_adj_ipcw_M
delta_ts_adj_ipcw_M

#Drop out var.psi from ipcw objects so that glm_print() works 
ts_t2_adj_ipcw_M<-ts_t2_adj_ipcw_M[-2,]
ts_t3_adj_ipcw_M<-ts_t3_adj_ipcw_M[-2,]
delta_ts_adj_ipcw_M<-delta_ts_adj_ipcw_M[-2,]


tab2<-data.frame(rbind(ts_t2_N_M,ts_t3_N_M,delta_ts_N_M))
tab2[,3]<-rnd(tab2[,3],2)
tab2[,4:6]<-matrix("",6,3)
tab2[2,4]<-glm_print(ts_t2_unadj_M)
tab2[4,4]<-glm_print(ts_t3_unadj_M)
tab2[6,4]<-glm_print(delta_ts_unadj_M)
tab2[2,5]<-glm_print(ts_t2_adj_M)
tab2[4,5]<-glm_print(ts_t3_adj_M)
tab2[6,5]<-glm_print(delta_ts_adj_M)
tab2[2,6]<-glm_print((c(ts_t2_adj_ipcw_M[1:3],NA,NA,ts_t2_adj_ipcw_M[4])), t=T)
tab2[4,6]<-glm_print((c(ts_t3_adj_ipcw_M[1:3],NA,NA,ts_t3_adj_ipcw_M[4])), t=T)
tab2[6,6]<-glm_print((c(delta_ts_adj_ipcw_M[1:3],NA,NA,delta_ts_adj_ipcw_M[4])), t=T)
colnames(tab2)<-NULL

tab2[,1]<-as.character(tab2[,1])
tab2[c(1,3,5),1]<-rep("~~~Control",3)
tab2[c(2,4,6),1]<-rep("~~~N+WSH",3)

blank=rep("",5)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("\\textbf{After 1 year of intervention}",blank)),
            t(c("\\textbf{(age \\textasciitilde 14 months)}",blank)),
            tab2[1:2,],
            t(c("\\textbf{After 2 years of intervention}",blank)),
            t(c("\\textbf{(age \\textasciitilde 28 months)}",blank)),
            tab2[3:4,],
            t(c("\\textbf{Change in Telomere length}",blank)),
            t(c("\\textbf{between year 1 and 2}",blank)),
            tab2[5:6,])
s.tab2<-tab2
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.tab2, file="s.table2.RData")

cleantable(s.tab2, 2)

