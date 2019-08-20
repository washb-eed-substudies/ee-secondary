
#---------------------------------------
# EE-BD-telo-table4-elife.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# telomere manuscript table 4 - elife format
#---------------------------------------


rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)
library(xtable)

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_res.Rdata")

ts_t2_N_subgroup_M
ts_t3_N_subgroup_M
delta_ts_N_subgroup_M
delta_ts_subgroup_M
ts_t2_subgroup_M
ts_t3_subgroup_M



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



glm_print<-function(obj, t=F){
  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }
  flag=F
  if(as.numeric(obj[4])<0.05){obj[4]<-paste0(sprintf("%1.3f",obj[c(4)]),"*")
                              flag=T}
  if(flag==F){
  obj<-t(as.matrix(c(sprintf("%1.2f",obj[c(1:3)]),
                     sprintf("%1.3f",obj[c(4)]))))    
  }else{
   obj<-t(as.matrix(c(sprintf("%1.2f",obj[c(1:3)]),
                     obj[c(4)])))       
  }

  #rownames(obj)<-colnames(obj)<-NULL
  out<-paste(obj[1]," (",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(out)
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






tab3_long<-data.frame(rbind(ts_t2_N_subgroup_M,ts_t3_N_subgroup_M,delta_ts_N_subgroup_M))
tab3 <-cbind(tab3_long[c(1,2,5,6,9,10),], tab3_long[c(3,4,7,8,11,12),])
tab3 <- tab3[,c(2,3,4,7,8)]
tab3[,c(3,5)]<-rnd(tab3[,c(3,5)],2)
tab3[,6:8]<-matrix("",6,3)


tab3[2,6]<-glm_print(ts_t2_subgroup_M[1,-c(1,3,6)])
tab3[4,6]<-glm_print(ts_t3_subgroup_M[1,-c(1,3,6)])
tab3[6,6]<-glm_print(delta_ts_subgroup_M[1,-c(1,3,6)])

tab3[2,7]<-glm_print(ts_t2_subgroup_M[2,-c(1,3,6)])
tab3[4,7]<-glm_print(ts_t3_subgroup_M[2,-c(1,3,6)])
tab3[6,7]<-glm_print(delta_ts_subgroup_M[2,-c(1,3,6)])

tab3[2,8]<-glm_print(ts_t2_subgroup_fit[4,-c(4:5)])
tab3[4,8]<-glm_print(ts_t3_subgroup_fit[4,-c(4:5)])
tab3[6,8]<-glm_print(delta_ts_subgroup_fit[4,-c(4:5)])

colnames(tab3)<-NULL

tab3[,1]<-as.character(tab3[,1])
tab3[c(2,4,6),1]<-rep("N + WSH",3)

# tab3[tab3[,1]=="female",1]<-"Female, "
# tab3[tab3[,1]=="male",1]<-"Male, "
# tab3[,2]<-paste0(tab3[,1],tab3[,2])
# tab3[,1]<-""

blank=rep("",7)
tab3<-as.matrix(tab3)
tab3<-rbind(t(c("\\textbf{After 1 year of intervention (age \\textasciitilde 14 months)}",blank)),
            tab3[1:2,],
            t(c("\\textbf{After 2 years of intervention (age \\textasciitilde 28 months)}",blank)),
            tab3[3:4,],
            t(c("\\textbf{Change in telomere length}",blank)),
            tab3[5:6,])

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab3, file="table3.RData")

cleantable(tab3, 2)



