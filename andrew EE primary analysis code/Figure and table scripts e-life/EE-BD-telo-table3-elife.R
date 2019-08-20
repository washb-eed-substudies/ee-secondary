
#---------------------------------------
# EE-BD-telo-table3-elife.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Creation of
# telomere manuscript table 3 - elife format
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

#--------------------------------
#Table 2 (eLife table 3)
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_res.Rdata")
load("telo_ipcw_res.Rdata")

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
  out<-paste(obj[1]," (",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(out)
}




#Drop out var.psi from ipcw objects so that glm_print() works 
ts_t2_adj_ipcw_M<-ts_t2_adj_ipcw_M[-2,]
ts_t3_adj_ipcw_M<-ts_t3_adj_ipcw_M[-2,]
delta_ts_adj_ipcw_M<-delta_ts_adj_ipcw_M[-2,]



tab2<-data.frame(rbind(ts_t2_N_M,ts_t3_N_M,delta_ts_N_M))
tab2[,3]<-rnd(tab2[,3],2)
tab2[,4:7]<-matrix("",6,4)
tab2[2,4]<-glm_print(ts_t2_unadj_M)
tab2[4,4]<-glm_print(ts_t3_unadj_M)
tab2[6,4]<-glm_print(delta_ts_unadj_M)
tab2[2,5]<-glm_print(ts_t2_adj_sex_age_M)
tab2[4,5]<-glm_print(ts_t3_adj_sex_age_M)
tab2[6,5]<-glm_print(delta_ts_adj_sex_age_M)
tab2[2,6]<-glm_print(ts_t2_adj_M)
tab2[4,6]<-glm_print(ts_t3_adj_M)
tab2[6,6]<-glm_print(delta_ts_adj_M)
tab2[2,7]<-glm_print((c(ts_t2_adj_ipcw_M[1:3],NA,NA,ts_t2_adj_ipcw_M[4])), t=T)
tab2[4,7]<-glm_print((c(ts_t3_adj_ipcw_M[1:3],NA,NA,ts_t3_adj_ipcw_M[4])), t=T)
tab2[6,7]<-glm_print((c(delta_ts_adj_ipcw_M[1:3],NA,NA,delta_ts_adj_ipcw_M[4])), t=T)

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





















