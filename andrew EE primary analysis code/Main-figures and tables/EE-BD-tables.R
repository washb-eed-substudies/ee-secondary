
#---------------------------------------
# EE-BD-tables.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Create R-objects for xtable printing
# for the WASH Benefits Bangladesh EED 
# substudy
#---------------------------------------




rm(list=ls())
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
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2],mu[j,3],sd[j,3],mu[j,4],sd[j,4])
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],mu[j,1]*100,
                  n[j,2],mu[j,2]*100,
                  n[j,3],mu[j,3]*100,
                  n[j,4],mu[j,4]*100)
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)
  #dat<-rnd(dat, round)
  #dat<-cbind(Rownames,dat)
  col1<-NULL
  col2<-NULL
  col3<-NULL
  col4<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],")",sep=""))
      temp3<-cbind(paste(dat[j,5]," (",dat[j,6],")",sep=""))
      temp4<-cbind(paste(dat[j,7]," (",dat[j,8],")",sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      col3<-rbind(col3, temp3)
      col4<-rbind(col4, temp4)

      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      temp3<-cbind(paste(dat[j,5],sep=""))
      temp4<-cbind(paste(dat[j,7],sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)  
      col3<-rbind(col3, temp3)           
      col4<-rbind(col4, temp4)           
        }else{
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],"\\%)",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],"\\%)",sep=""))
      temp3<-cbind(paste(dat[j,5]," (",dat[j,6],"\\%)",sep=""))
      temp4<-cbind(paste(dat[j,7]," (",dat[j,8],"\\%)",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
      col3<-rbind(col3, temp3) 
      col4<-rbind(col4, temp4) 
          }
      }
  } 
  dat<-cbind(Rownames, col1, col2, col3, col4)
  colnames(dat)=c(" ","Control","WSH","Nutrition","Nutrition + WSH")
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


#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("EE-BD-table1.Rdata")


balance.tab.mu_M 
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
blank=rep("",12)

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
n.comp.f[4]=paste(" (N=",n.comp.f[4],")",sep="")
n.comp.f[5]=paste(" (N=",n.comp.f[5],")",sep="")
n.comp.f<-n.comp.f[2:5]
colnames(n.comp.f)<-NULL


for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  table1_f[i,1]=paste("~~~",table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,26:27,31,32,35:36,38:39)){
  table1_f[i,1]=paste("~~~~~",table1_f[i,1],sep="")
}

table1_f

#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(n.comp.f, table1_f, file="EE-BD-table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(table1_f,digits=0)


#--------------------------------
#Table 2
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_res_unadj_M.Rdata")
load("stool_res_adj_M.Rdata")
load("stool_res_N_M.Rdata")
load("stool_res_means.Rdata")
load("urine_res_unadj_M.Rdata")
load("urine_res_adj_M.Rdata")
load("urine_res_N_M.Rdata")
load("urine_res_means.Rdata")

aat_t1_N_M
aat_t2_N_M
aat_t3_N_M
aat_t1_mn
aat_t2_mn
aat_t3_mn
aat_t1_unadj_M
aat_t2_unadj_M
aat_t3_unadj_M
mpo_t1_unadj_M
mpo_t2_unadj_M
mpo_t3_unadj_M
neo_t1_unadj_M
neo_t2_unadj_M
neo_t3_unadj_M
reg1b_t2_unadj_M


 
glm_print<-function(n, mean, glm, t=F){
# n<-aat_t1_N_M
# mean<-aat_t1_mn
# glm<-aat_t1_unadj_M

rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
#adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
glmh2a<-rbind(rep(NA, 3),rep(NA, 3),rep(NA, 3), glm[5,1:3])
glmh2b<-rbind(rep(NA, 3),rep(NA, 3),rep(NA, 3), glm[4,1:3])


obj<-cbind(n[,1:2], mean[,2:3], glmh1, glmh2a, glmh2b)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[4,8]<-sprintf("%1.2f",obj[4,8])
obj[4,9]<-sprintf("%1.2f",obj[4,9])
obj[4,10]<-sprintf("%1.2f",obj[4,10])
obj[4,11]<-sprintf("%1.2f",obj[4,11])
obj[4,12]<-sprintf("%1.2f",obj[4,12])
obj[4,13]<-sprintf("%1.2f",obj[4,13])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,14]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,15]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,16]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")

  obj<-obj[,-c(5:13)]
  obj[1,5]<-""
  obj[1:3,6:7]<-""
  #out<-paste(obj[1],"(",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(obj)
}

tab2<-rbind(
  glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M),
  glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M),
  glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M),
  glm_print(lac_t1_N_M, lac_t1_mn, lac_t1_unadj_M),
  glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M),
  glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M)
)




# tab2[,1]<-as.character(tab2[,1])
# tab2[c(1,3,5),1]<-rep("~~~Control",3)
# tab2[c(2,4,6),1]<-rep("~~~N+WSH",3)

blank=rep("",6)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            tab2[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            tab2[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            tab2[9:12,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            tab2[13:16,],
            t(c("\\textbf{Ln mannitol (mmol/L)l}",blank)),
            tab2[17:20,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            tab2[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab2, file="EE-BD-table2.RData")

cleantable(tab2, 2)






#--------------------------------
#Table 3
#--------------------------------

tab3<-rbind(
  glm_print(mpo_t2_N_M, mpo_t2_mn, mpo_t2_unadj_M),
  glm_print(aat_t2_N_M, aat_t2_mn, aat_t2_unadj_M),
  glm_print(neo_t2_N_M, neo_t2_mn, neo_t2_unadj_M),
  glm_print(reg1b_t2_N_M, reg1b2_t2_mn, reg1b_t2_unadj_M),
  glm_print(lac_t2_N_M, lac_t2_mn, lac_t2_unadj_M),
  glm_print(man_t2_N_M, man_t2_mn, man_t2_unadj_M),
  glm_print(lm_t2_N_M, lm_t2_mn, lm_t2_unadj_M)
)



blank=rep("",6)
tab3<-as.matrix(tab3)
tab3<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            tab3[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            tab3[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            tab3[9:12,],
            t(c("\\textbf{Ln regenerating gene 1$\\beta$ ($\\mu$g/ml)}",blank)),
            tab3[13:16,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            tab3[17:20,],
            t(c("\\textbf{Ln mannitol (mmol/L)l}",blank)),
            tab3[21:24,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            tab3[25:28,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab3, file="EE-BD-table3.RData")

cleantable(tab3, 2)




#--------------------------------
#Table 4
#--------------------------------

tab4<-rbind(
  glm_print(mpo_t3_N_M, mpo_t3_mn, mpo_t3_unadj_M),
  glm_print(aat_t3_N_M, aat_t3_mn, aat_t3_unadj_M),
  glm_print(neo_t3_N_M, neo_t3_mn, neo_t3_unadj_M),
  glm_print(lac_t3_N_M, lac_t3_mn, lac_t3_unadj_M),
  glm_print(man_t3_N_M, man_t3_mn, man_t3_unadj_M),
  glm_print(lm_t3_N_M, lm_t3_mn, lm_t3_unadj_M)
)



blank=rep("",6)
tab4<-as.matrix(tab4)
tab4<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            tab4[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            tab4[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            tab4[9:12,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            tab4[13:16,],
            t(c("\\textbf{Ln mannitol (mmol/L)l}",blank)),
            tab4[17:20,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            tab4[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab4, file="EE-BD-table4.RData")

cleantable(tab4, 2)


#--------------------------------
# Supplimentary table 1: 
# Enrollment characteristics included in the follow-up and children lost to follow-up 
#--------------------------------


s.table1_create<-function(mu, n, sd, mean.ind, vargroup, vargroup.row, Rownames, round){
  dat<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2],mu[j,3],sd[j,3])
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],mu[j,1]*100,
                  n[j,2],mu[j,2]*100,
                  n[j,3],mu[j,3]*100)
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)
  #dat<-rnd(dat, round)
  #dat<-cbind(Rownames,dat)
  col1<-NULL
  col2<-NULL
  col3<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],")",sep=""))
      temp3<-cbind(paste(dat[j,5]," (",dat[j,6],")",sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      col3<-rbind(col3, temp3)

      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      temp3<-cbind(paste(dat[j,5],sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)  
      col3<-rbind(col3, temp3)           
        }else{
      temp1<-cbind(paste(dat[j,1]," (",dat[j,2],"\\%)",sep=""))
      temp2<-cbind(paste(dat[j,3]," (",dat[j,4],"\\%)",sep=""))
      temp3<-cbind(paste(dat[j,5]," (",dat[j,6],"\\%)",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
      col3<-rbind(col3, temp3) 
          }
      }
  } 
  dat<-cbind(Rownames, col1, col2, col3)
  colnames(dat)=c(" ","Included","Lost to follow-up at Year 1","Lost to follow-up at Year 2")
  return(dat)
}


	 	 
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
load("EE-BD_s.table1.Rdata")

s.balance.tab.mu_M
s.balance.tab.n_M
s.balance.tab.sd_M


s.balance.tab.mu_M<-t(s.balance.tab.mu_M)
s.balance.tab.n_M <-t(s.balance.tab.n_M)
s.balance.tab.sd_M<-t(s.balance.tab.sd_M) 
s.balance.tab.mu_M 
s.balance.tab.n_M 
s.balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
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




#EE substudy table S1
tab<-s.table1_create(mu=s.balance.tab.mu_M, 
                   n=s.balance.tab.n_M, 
                   sd=s.balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab


#Add in variable group labels
blank=rep("",8)

s.n.comp.f<-tab[1,]
tab<-tab[-1,]

s.table1_f=   rbind(
               c("\\textbf{Maternal}",blank,blank,blank),
               tab[c(1:2),],
               c( "\\textbf{Paternal}",blank,blank,blank),
               tab[c(3:4),],
               c("\\textbf{Household}",blank,blank,blank),
               tab[c(5:8),],
               c("\\textbf{Drinking Water}",blank,blank,blank),
               tab[c(9:12),],
               c("\\textbf{Sanitation}",blank,blank,blank),
               c("Reported daily open defecation",blank,blank,blank),
               tab[c(13:17),],
               c("Latrine",blank,blank,blank),
               tab[c(18:22),],
               c("Human feces observed in the",blank,blank,blank),
               tab[c(23:24),],
               c("\\textbf{Handwashing}",blank,blank,blank),
               c("Within 6 steps of latrine",blank,blank,blank),
               tab[c(25:26),],
               c("Within 6 steps of kitchen",blank,blank,blank),
               tab[c(27:28),],
               c("\\textbf{Nutrition}",blank,blank,blank),
               tab[c(29),])

rownames(s.table1_f)=NULL

s.table1_f
s.n.comp.f

n.comp.f[2]=paste("(N=",n.comp.f[2],")",sep="")
n.comp.f[3]=paste("(N=",n.comp.f[3],")",sep="")
n.comp.f<-n.comp.f[2:3]
colnames(n.comp.f)<-NULL


#FIX:
for(i in c(2:3,5:6,8:11,13:16,18,24,29,30,34,37,41)){
  s.table1_f[i,1]=paste("~~~",s.table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,31,32,35:36,38:39)){
  s.table1_f[i,1]=paste("~~~~~",s.table1_f[i,1],sep="")
}


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.n.comp.f, s.table1_f, file="EE-BD-s.table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(s.table1_f,digits=0)
















#--------------------------------
#Supplementary Table 2: 
#   Effect of intervention on environmental enteric dysfunction measurements 
# at follow-up 1 
#--------------------------------

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_ipcw_res.Rdata")
load("urine_ipcw_res.Rdata")
load("stool_res_adj_sex_age_M.Rdata")
load("urine_res_adj_sex_age_M.Rdata")



###NOTE! Need to fix to print the 


s.glm_print<-function(n, mean, glm, age, adj,ipcw, t=F){
 # n<-aat_t1_N_M
 # mean<-aat_t1_mn
 # glm<-aat_t1_unadj_M
 # adj<-aat_t1_adj_M
 # ipcw<-aat_t1_adj_ipcw_M
 
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])


obj<-cbind(n[,1:2], mean[,2:3], glmh1 ,ageh1, adjh1, ipcwh1)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[2:4,8]<-sprintf("%1.2f",obj[2:4,8])
obj[2:4,9]<-sprintf("%1.2f",obj[2:4,9])
obj[2:4,10]<-sprintf("%1.2f",obj[2:4,10])
obj[2:4,11]<-sprintf("%1.2f",obj[2:4,11])
obj[2:4,12]<-sprintf("%1.2f",obj[2:4,12])
obj[2:4,13]<-sprintf("%1.2f",obj[2:4,13])
obj[2:4,14]<-sprintf("%1.2f",obj[2:4,14])
obj[2:4,15]<-sprintf("%1.2f",obj[2:4,15])
obj[2:4,16]<-sprintf("%1.2f",obj[2:4,16])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,17]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,18]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,19]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")
  obj[,20]<-paste(obj[,14]," (",obj[,15],",",obj[,16],")", sep="")

  obj<-obj[,-c(5:16)]
  obj[1,5]<-""
  obj[1,6]<-""
  obj[1,7]<-""
  obj[1,8]<-""

  return(obj)
}

s.tab2<-rbind(
  s.glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M, mpo_t1_adj_sex_age_M, mpo_t1_adj_M, mpo_t1_adj_ipcw_M),
  s.glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M, aat_t1_adj_sex_age_M, aat_t1_adj_M, aat_t1_adj_ipcw_M),
  s.glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M, neo_t1_adj_sex_age_M, neo_t1_adj_M, neo_t1_adj_ipcw_M),
  s.glm_print(lac_t1_N_M, lac_t1_mn, lac_t1_unadj_M, lac_t1_adj_sex_age_M, lac_t1_adj_M, l1_adj_ipcw_M),
  s.glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M, man_t1_adj_sex_age_M, man_t1_adj_M, m1_adj_ipcw_M),
  s.glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M, lm_t1_adj_sex_age_M, lm_t1_adj_M, lmr1_adj_ipcw_M)
)




blank=rep("",7)
s.tab2<-as.matrix(s.tab2)
s.tab2<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            s.tab2[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            s.tab2[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            s.tab2[9:12,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            s.tab2[13:16,],
            t(c("\\textbf{Ln mannitol (mmol/L)}",blank)),
            s.tab2[17:20,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            s.tab2[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.tab2, file="EE-BD-s.table2.RData")

cleantable(s.tab2, 2)





#--------------------------------
# Supplementary Table 3:
#   Effect of intervention on environmental enteric dysfunction measurements 
# after 1 year of intervention 
#--------------------------------
s.tab3<-rbind(
    s.glm_print(mpo_t2_N_M, mpo_t2_mn, mpo_t2_unadj_M, mpo_t2_adj_sex_age_M, mpo_t2_adj_M, mpo_t2_adj_ipcw_M),
  s.glm_print(aat_t2_N_M, aat_t2_mn, aat_t2_unadj_M, aat_t2_adj_sex_age_M, aat_t2_adj_M, aat_t2_adj_ipcw_M),
  s.glm_print(neo_t2_N_M, neo_t2_mn, neo_t2_unadj_M, neo_t2_adj_sex_age_M, neo_t2_adj_M, neo_t2_adj_ipcw_M),
    s.glm_print(reg1b_t2_N_M, reg1b2_t2_mn, reg1b_t2_unadj_M, reg1b_t2_adj_sex_age_M, reg1b_t2_adj_M, reg_t2_adj_ipcw_M),
  s.glm_print(lac_t2_N_M, lac_t2_mn, lac_t2_unadj_M, lac_t2_adj_sex_age_M, lac_t2_adj_M, l2_adj_ipcw_M),
  s.glm_print(man_t2_N_M, man_t2_mn, man_t2_unadj_M, man_t2_adj_sex_age_M, man_t2_adj_M, m2_adj_ipcw_M),
  s.glm_print(lm_t2_N_M, lm_t2_mn, lm_t2_unadj_M, lm_t2_adj_sex_age_M, lm_t2_adj_M, lmr2_adj_ipcw_M)
)

  



blank=rep("",7)
s.tab3<-as.matrix(s.tab3)
s.tab3<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            s.tab3[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            s.tab3[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            s.tab3[9:12,],
            t(c("\\textbf{Ln regenerating gene 1$\\beta$ ($\\mu$g/ml)}",blank)),
            s.tab3[13:16,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            s.tab3[17:20,],
            t(c("\\textbf{Ln mannitol (mmol/L)}",blank)),
            s.tab3[21:24,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            s.tab3[25:28,]
            )


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.tab3, file="EE-BD-s.table3.RData")

cleantable(s.tab3, 2)

#--------------------------------
# Supplementary Table 4: 
#   Effect of intervention on environmental enteric dysfunction measurements 
# after 2 years of intervention 
#--------------------------------
s.tab4<-rbind(
  s.glm_print(mpo_t3_N_M, mpo_t3_mn, mpo_t3_unadj_M, mpo_t3_adj_sex_age_M, mpo_t3_adj_M, mpo_t3_adj_ipcw_M),
  s.glm_print(aat_t3_N_M, aat_t3_mn, aat_t3_unadj_M, aat_t3_adj_sex_age_M, aat_t3_adj_M, aat_t3_adj_ipcw_M),
  s.glm_print(neo_t3_N_M, neo_t3_mn, neo_t3_unadj_M, neo_t3_adj_sex_age_M, neo_t3_adj_M, neo_t3_adj_ipcw_M),
  s.glm_print(lac_t3_N_M, lac_t3_mn, lac_t3_unadj_M, lac_t3_adj_sex_age_M, lac_t3_adj_M, l3_adj_ipcw_M),
  s.glm_print(man_t3_N_M, man_t3_mn, man_t3_unadj_M, man_t3_adj_sex_age_M, man_t3_adj_M, m3_adj_ipcw_M),
  s.glm_print(lm_t3_N_M, lm_t3_mn, lm_t3_unadj_M, lm_t3_adj_sex_age_M, lm_t3_adj_M, lmr3_adj_ipcw_M)
)

blank=rep("",7)
s.tab4<-as.matrix(s.tab4)
s.tab4<-rbind(t(c("\\textbf{Ln myeloperoxidase (ng/ml)}",blank)),
            s.tab4[1:4,],
            t(c("\\textbf{Ln alpha-1 antitrypsin (mg/g)}",blank)),
            s.tab4[5:8,],
            t(c("\\textbf{Ln neopterin (nmol/L)}",blank)),
            s.tab4[9:12,],
            t(c("\\textbf{Ln lactulose (mmol/L)}",blank)),
            s.tab4[13:16,],
            t(c("\\textbf{Ln mannitol (mmol/L)}",blank)),
            s.tab4[17:20,],
            t(c("\\textbf{Ln L:M ratio}",blank)),
            s.tab4[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.tab4, file="EE-BD-s.table4.RData")

cleantable(s.tab4, 2)




#--------------------------------
# Supplementary Table 6: 
#   Effect of intervention on environmental enteric dysfunction measurements 
# after 1 year of intervention using inverse probability weighting
#--------------------------------


ipcw.glm_print<-function(n, mean, glm, adj, ipcw, t=F){
 # n<-aat_t1_N_M
 # mean<-aat_t1_mn
 # glm<-aat_t1_unadj_M
 # adj<-aat_t1_adj_M
 # ipcw<-aat_t1_adj_ipcw_M
 
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,1:3])


obj<-cbind(n[,1:2], mean[,2:3], glmh1, adjh1, ipcwh1)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[2:4,8]<-sprintf("%1.2f",obj[2:4,8])
obj[2:4,9]<-sprintf("%1.2f",obj[2:4,9])
obj[2:4,10]<-sprintf("%1.2f",obj[2:4,10])
obj[2:4,11]<-sprintf("%1.2f",obj[2:4,11])
obj[2:4,12]<-sprintf("%1.2f",obj[2:4,12])
obj[2:4,13]<-sprintf("%1.2f",obj[2:4,13])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,14]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,15]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,16]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")

  obj<-obj[,-c(5:13)]
  obj[1,5]<-""
  obj[1,6]<-""
  obj[1,7]<-""

  return(obj)
}


ipcw.tab2<-rbind(
  s.glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M, mpo_t1_adj_M, mpo_t1_adj_ipcw_M),
  s.glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M, aat_t1_adj_M, aat_t1_adj_ipcw_M),
  s.glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M, neo_t1_adj_M, neo_t1_adj_ipcw_M),
  s.glm_print(lac_t1_N_M, lac_t1_mn, lac_t1_unadj_M, lac_t1_adj_M, l1_adj_ipcw_M),
  s.glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M, man_t1_adj_M, m1_adj_ipcw_M),
  s.glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M, lm_t1_adj_M, lmr1_adj_ipcw_M)
)



#--------------------------------
# Supplementary Table 6: 
#   L and M %s
#--------------------------------   
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("pre_recovery_res_M.Rdata")
load("pre_recovery_ipcw_res_M.Rdata")

s6.glm_print <- function(n, mean, glm, age, adj,ipcw, t=F){
 # n<-perl1_N_M
 # mean<-perl1_mn
 # glm<-perl1_unadj_M
 # adj<-perl1_adj_M
 # ipcw<-perl1_adj_ipcw_M
 
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])


obj<-cbind(n[,1:4], glmh1 ,ageh1, adjh1, ipcwh1)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[2:4,8]<-sprintf("%1.2f",obj[2:4,8])
obj[2:4,9]<-sprintf("%1.2f",obj[2:4,9])
obj[2:4,10]<-sprintf("%1.2f",obj[2:4,10])
obj[2:4,11]<-sprintf("%1.2f",obj[2:4,11])
obj[2:4,12]<-sprintf("%1.2f",obj[2:4,12])
obj[2:4,13]<-sprintf("%1.2f",obj[2:4,13])
obj[2:4,14]<-sprintf("%1.2f",obj[2:4,14])
obj[2:4,15]<-sprintf("%1.2f",obj[2:4,15])
obj[2:4,16]<-sprintf("%1.2f",obj[2:4,16])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,17]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,18]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,19]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")
  obj[,20]<-paste(obj[,14]," (",obj[,15],",",obj[,16],")", sep="")

  obj<-obj[,-c(5:16)]
  obj[1,5]<-""
  obj[1,6]<-""
  obj[1,7]<-""
  obj[1,8]<-""

  return(obj)
}



s.tab6<-rbind(
  s6.glm_print(perl1_N_M, perl1_mn, perl1_unadj_M, perl1_adj_sex_age_M, perl1_adj_M, perl1_adj_ipcw_M),
  s6.glm_print(perm1_N_M, perm1_mn, perm1_unadj_M, perm1_adj_sex_age_M, perm1_adj_M, perm1_adj_ipcw_M),
  s6.glm_print(perl2_N_M, perl2_mn, perl2_unadj_M, perl2_adj_sex_age_M, perl2_adj_M, perl2_adj_ipcw_M),
  s6.glm_print(perm2_N_M, perm2_mn, perm2_unadj_M, perm2_adj_sex_age_M, perm2_adj_M, perm2_adj_ipcw_M),
  s6.glm_print(perl3_N_M, perl3_mn, perl3_unadj_M, perl3_adj_sex_age_M, perl3_adj_M, perl3_adj_ipcw_M),
  s6.glm_print(perm3_N_M, perm3_mn, perm3_unadj_M, perm3_adj_sex_age_M, perm3_adj_M, perm3_adj_ipcw_M)
)

blank=rep("",7)
s.tab6<-as.matrix(s.tab6)
s.tab6<-rbind(t(c("\\textbf{Lactulose recovery (\\%) at child age 3 months}",blank)),
            s.tab6[1:4,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 3 months}",blank)),
            s.tab6[5:8,],
            t(c("\\textbf{Lactulose recovery (\\%) at child age 14 months}",blank)),
            s.tab6[9:12,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 14 months}",blank)),
            s.tab6[13:16,],
            t(c("\\textbf{Lactulose recovery (\\%) at child age 28 months}",blank)),
            s.tab6[17:20,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 28 months}",blank)),
            s.tab6[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.tab6, file="EE-BD-s.table8.RData")

cleantable(s.tab6, 2)













#--------------------------------
#Supplementary Table 7: 
#   Effect of intervention on environmental enteric dysfunction measurements 
#   when excluding contaminated samples
#--------------------------------

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("urine_sens_res_N_M.Rdata")
load("urine_sens_res_means.Rdata")
load("urine_sens_res_unadj_M.Rdata")
load("urine_sens_res_adj_sex_age_M.Rdata")
load("urine_sens_res_adj_M.Rdata")
load("urine_ipcw_sens_res.Rdata")




s.glm_print<-function(n, mean, glm, age, adj,ipcw, t=F){
 # n<-aat_t1_N_M
 # mean<-aat_t1_mn
 # glm<-aat_t1_unadj_M
 # adj<-aat_t1_adj_M
 # ipcw<-aat_t1_adj_ipcw_M
 
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])


obj<-cbind(n[,1:2], mean[,2:3], glmh1 ,ageh1, adjh1, ipcwh1)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[2:4,8]<-sprintf("%1.2f",obj[2:4,8])
obj[2:4,9]<-sprintf("%1.2f",obj[2:4,9])
obj[2:4,10]<-sprintf("%1.2f",obj[2:4,10])
obj[2:4,11]<-sprintf("%1.2f",obj[2:4,11])
obj[2:4,12]<-sprintf("%1.2f",obj[2:4,12])
obj[2:4,13]<-sprintf("%1.2f",obj[2:4,13])
obj[2:4,14]<-sprintf("%1.2f",obj[2:4,14])
obj[2:4,15]<-sprintf("%1.2f",obj[2:4,15])
obj[2:4,16]<-sprintf("%1.2f",obj[2:4,16])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,17]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,18]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,19]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")
  obj[,20]<-paste(obj[,14]," (",obj[,15],",",obj[,16],")", sep="")

  obj<-obj[,-c(5:16)]
  obj[1,5]<-""
  obj[1,6]<-""
  obj[1,7]<-""
  obj[1,8]<-""

  return(obj)
}

sens.tab<-rbind(
  s.glm_print(sens_lac_t1_N_M, sens_lac_t1_mn, sens_lac_t1_unadj_M, sens_lac_t1_adj_sex_age_M, sens_lac_t1_adj_M, sens_l1_adj_ipcw_M),
  s.glm_print(sens_man_t1_N_M, sens_man_t1_mn, sens_man_t1_unadj_M, sens_man_t1_adj_sex_age_M, sens_man_t1_adj_M, sens_m1_adj_ipcw_M),
  s.glm_print(sens_lm_t1_N_M, sens_lm_t1_mn, sens_lm_t1_unadj_M, sens_lm_t1_adj_sex_age_M, sens_lm_t1_adj_M, sens_lmr1_adj_ipcw_M),
  
  s.glm_print(sens_lac_t2_N_M, sens_lac_t2_mn, sens_lac_t2_unadj_M, sens_lac_t2_adj_sex_age_M, sens_lac_t2_adj_M, sens_l2_adj_ipcw_M),
  s.glm_print(sens_man_t2_N_M, sens_man_t2_mn, sens_man_t2_unadj_M, sens_man_t2_adj_sex_age_M, sens_man_t2_adj_M, sens_m2_adj_ipcw_M),
  s.glm_print(sens_lm_t2_N_M, sens_lm_t2_mn, sens_lm_t2_unadj_M, sens_lm_t2_adj_sex_age_M, sens_lm_t2_adj_M, sens_lmr2_adj_ipcw_M),

  s.glm_print(sens_lac_t3_N_M, sens_lac_t3_mn, sens_lac_t3_unadj_M, sens_lac_t3_adj_sex_age_M, sens_lac_t3_adj_M, sens_l3_adj_ipcw_M),
  s.glm_print(sens_man_t3_N_M, sens_man_t3_mn, sens_man_t3_unadj_M, sens_man_t3_adj_sex_age_M, sens_man_t3_adj_M, sens_m3_adj_ipcw_M),
  s.glm_print(sens_lm_t3_N_M, sens_lm_t3_mn, sens_lm_t3_unadj_M, sens_lm_t3_adj_sex_age_M, sens_lm_t3_adj_M, sens_lmr3_adj_ipcw_M)
  )




blank=rep("",7)
sens.tab<-as.matrix(sens.tab)
sens.tab<-rbind(
            t(c("\\textbf{Ln lactulose (mmol/L) at child age 3 months}",blank)),
            sens.tab[1:4,],
            t(c("\\textbf{Ln mannitol (mmol/L) at child age 3 months}",blank)),
            sens.tab[5:8,],
            t(c("\\textbf{Ln L:M ratio at child age 3 months}",blank)),
            sens.tab[9:12,],
            t(c("\\textbf{Ln lactulose (mmol/L) at child age 14 months}",blank)),
            sens.tab[13:16,],
            t(c("\\textbf{Ln mannitol (mmol/L) at child age 14 months}",blank)),
            sens.tab[17:20,],
            t(c("\\textbf{Ln L:M ratio at child age 14 months}",blank)),
            sens.tab[21:24,],
             t(c("\\textbf{Ln lactulose (mmol/L) at child age 28 months}",blank)),
            sens.tab[25:28,],
            t(c("\\textbf{Ln mannitol (mmol/L) at child age 28 months}",blank)),
            sens.tab[29:32,],
            t(c("\\textbf{Ln L:M ratio at child age 28 months}",blank)),
            sens.tab[33:36,]
            )



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(sens.tab, file="EE-BD-sens.table.RData")

cleantable(sens.tab, 2)









#--------------------------------
# Supplementary Table 8: 
#   L and M %s sensitivity analysis
#--------------------------------   
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("pre_recovery_sens_res_M.Rdata")
load("pre_recovery_ipcw_sens_res_M.Rdata")


sens.tab8<-rbind(
  s6.glm_print(sens_perl1_N_M, sens_perl1_mn, sens_perl1_unadj_M, sens_perl1_adj_sex_age_M, sens_perl1_adj_M, sens_perl1_adj_ipcw_M),
  s6.glm_print(sens_perm1_N_M, sens_perm1_mn, sens_perm1_unadj_M, sens_perm1_adj_sex_age_M, sens_perm1_adj_M, sens_perm1_adj_ipcw_M),
  s6.glm_print(sens_perl2_N_M, sens_perl2_mn, sens_perl2_unadj_M, sens_perl2_adj_sex_age_M, sens_perl2_adj_M, sens_perl2_adj_ipcw_M),
  s6.glm_print(sens_perm2_N_M, sens_perm2_mn, sens_perm2_unadj_M, sens_perm2_adj_sex_age_M, sens_perm2_adj_M, sens_perm2_adj_ipcw_M),
  s6.glm_print(sens_perl3_N_M, sens_perl3_mn, sens_perl3_unadj_M, sens_perl3_adj_sex_age_M, sens_perl3_adj_M, sens_perl3_adj_ipcw_M),
  s6.glm_print(sens_perm3_N_M, sens_perm3_mn, sens_perm3_unadj_M, sens_perm3_adj_sex_age_M, sens_perm3_adj_M, sens_perm3_adj_ipcw_M)
)

blank=rep("",7)
sens.tab8<-as.matrix(sens.tab8)
sens.tab8<-rbind(t(c("\\textbf{Lactulose recovery (\\%) at child age 3 months}",blank)),
            sens.tab8[1:4,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 3 months}",blank)),
            sens.tab8[5:8,],
            t(c("\\textbf{Lactulose recovery (\\%) at child age 14 months}",blank)),
            sens.tab8[9:12,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 14 months}",blank)),
            sens.tab8[13:16,],
            t(c("\\textbf{Lactulose recovery (\\%) at child age 28 months}",blank)),
            sens.tab8[17:20,],
            t(c("\\textbf{Mannitol recovery (\\%) at child age 28 months}",blank)),
            sens.tab8[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(sens.tab8, file="EE-BD-sens.table8.RData")

cleantable(sens.tab8, 2)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("pre_recovery_sens_res_M.Rdata")
load("pre_recovery_ipcw_sens_res_M.Rdata")






# 
# #--------------------------------
# #Table S10:
# # Can you create another supplementary table with geometric means and 95%CI of each biomarker at each age/time point?
# # biomarkers in the table: mpo, neo, reg, aat, lactulose concentration, mannitol concentration, %L recovery, %M recovery, LMR
# # And it'd be best to include an overall geo mean of each biomarker across all time points in the table, in addition to a means 
# # at each time point individually (other EED studies report these absolute concentrations). 
# #--------------------------------
# 
# #load objects
# setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
# load("urine_overall_means.Rdata")
# load("stool_overall_means.Rdata")
# 
# d<-rbind(stool_overall_mn[,-c(4:5)], urine_overall_mn[,-c(4:5)])
# tab_overall<-d[c(1:4,15:19),]
# 
# tab<-data.frame(biomarker=tab_overall[,1],
#                 N=rep(NA,9),
#                 mean=rep(NA,9),
#                 ci.lb=rep(NA,9),
#                 ci.ub=rep(NA,9),
#                 N1=rep(NA,9),
#                 mean1=rep(NA,9),
#                 ci.lb1=rep(NA,9),
#                 ci.ub1=rep(NA,9),
#                 N2=rep(NA,9),
#                 mean2=rep(NA,9),
#                 ci.lb2=rep(NA,9),
#                 ci.ub2=rep(NA,9),
#                 N3=rep(NA,9),
#                 mean3=rep(NA,9),
#                 ci.lb3=rep(NA,9),
#                 ci.ub3=rep(NA,9))
# tab[,c(2:5)]<-d[c(1:4,15:19),2:5]                
# tab[c(1:3,5:9),c(6:9)]<-d[c(5,8,11,20,23,26,29,32),2:5]                
# tab[,c(10:13)]<-d[c(6,9,12,14,21,24,27,30,33),2:5]                
# tab[c(1:3,5:9),c(14:17)]<-d[c(7,10,12,20,25,28,31,34),2:5]                
# 
# #convert back to raw scale from log scale
# for(i in c(3,4,5,7,8,9,11,12,13,15,16,17)){
#   tab[,i]<-exp(tab[,i])
# }
# 
# 
# tab[,3] <- paste(sprintf("%1.2f",tab[,3])," (",sprintf("%1.2f",tab[,4]),",",sprintf("%1.2f",tab[,5]),")", sep="")
# tab[,7] <- paste(sprintf("%1.2f",tab[,7])," (",sprintf("%1.2f",tab[,8]),",",sprintf("%1.2f",tab[,9]),")", sep="")
# tab[,11] <- paste(sprintf("%1.2f",tab[,11])," (",sprintf("%1.2f",tab[,12]),",",sprintf("%1.2f",tab[,13]),")", sep="")
# tab[,15] <- paste(sprintf("%1.2f",tab[,15])," (",sprintf("%1.2f",tab[,16]),",",sprintf("%1.2f",tab[,17]),")", sep="")
# 
# tab<-tab[,-c(4,5,8,9,12,13,16,17)]
# tab[4,c(4,5,8,9)]<-"---"
# tab<-tab[c(2,1,3,4,5,7,8,9,6),]
# tab
# 
# tab[,1]<-c(
# "Myeloperoxidase (ng/ml)",
# "Alpha-1 antitrypsin (mg/g)",
# "Neopterin (nmol/L)",
# "Regenerating gene 1$\\beta$ ($\\mu$g/ml)",
# "Lactulose (mmol/L)",
# "Mannitol (mmol/L)",
# "Lactulose (\\%)",
# "Mannitol (\\%)",
# "Ln L:M ratio"
# )
# 
# 
# 
# cleantable(tab,digits=0)






#--------------------------------
# Supplementary Table 9-11: 
#   Unadjusted and adjusted p-values 
# across all time points
#--------------------------------

load("stool_res_unadj_M.Rdata")
load("stool_res_adj_M.Rdata")
load("stool_ipcw_res.Rdata")
load("urine_res_unadj_M.Rdata")
load("urine_res_adj_M.Rdata")
load("urine_ipcw_res.Rdata")

s.pval_print<-function(t, sample ,glm, age, adj,ipcw){
 
contrasts<-c("C v WSH", "C v N", "C v N+WSH", "WSH v N+WSH", "N v N+WSH")
  
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])

glm<-data.frame(glm[,c(1,6)])
age<-data.frame(age[,c(1,6)])
adj<-data.frame(adj[,c(1,6)])
ipcw<-data.frame(ipcw[,c(1,5)])

colnames(glm)<-colnames(age)<-colnames(adj)<-colnames(ipcw)<-c("diff","pval")

glm$adjp<-glm[,2]*3
age$adjp<-age[,2]*3
adj$adjp<-adj[,2]*3
ipcw$adjp<-ipcw[,2]*3

glm$adjp<-ifelse(glm$adjp>0.99,0.99,glm$adjp)
age$adjp<-ifelse(age$adjp>0.99,0.99,age$adjp)
adj$adjp<-ifelse(adj$adjp>0.99,0.99,adj$adjp)
ipcw$adjp<-ifelse(ipcw$adjp>0.99,0.99,ipcw$adjp)

time<-ifelse(t>6,"Year 1", "3 month")
time<-ifelse(t>13,"Year 2", time)

obj<-data.frame((rep(paste0(sample[t]),5)), (rep(time,5)), contrasts, glm ,age, adj, ipcw)
for(i in 4:ncol(obj)){
  sig<-rep("", nrow(obj))
  obj[,i]<-sprintf("%1.2f",obj[,i])
    obj[,i]<-as.character(obj[,i])
    
  if(i==5|i==6|i==8|i==9|i==11|i==12|i==14|i==15){
  sig<-ifelse(obj[,i]<0.05, paste0(sig, "*"), sig)
  sig<-ifelse(obj[,i]<0.01, paste0(sig, "*"), sig)
  sig<-ifelse(obj[,i]<0.001, paste0(sig, "*"), sig)
  obj[,i]<-paste0(obj[,i],sig)
    }
}

  return(obj)
}

sample<-c("Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln lactulose (mmol/L)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio",
"Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln regenerating gene 1$\\beta$ ($\\mu$g/ml)",
"Ln lactulose (mmol/L)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio",
"Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln lactulose (mmol/L)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio"
)

s.tab7<-rbind(
  s.pval_print(1, sample, mpo_t1_unadj_M, mpo_t1_adj_sex_age_M, mpo_t1_adj_M, mpo_t1_adj_ipcw_M),
  s.pval_print(2, sample,aat_t1_unadj_M, aat_t1_adj_sex_age_M, aat_t1_adj_M, aat_t1_adj_ipcw_M),
  s.pval_print(3, sample,neo_t1_unadj_M, neo_t1_adj_sex_age_M, neo_t1_adj_M, neo_t1_adj_ipcw_M),
  s.pval_print(4, sample,lac_t1_unadj_M, lac_t1_adj_sex_age_M, lac_t1_adj_M, l1_adj_ipcw_M),
  s.pval_print(5, sample,man_t1_unadj_M, man_t1_adj_sex_age_M, man_t1_adj_M, m1_adj_ipcw_M),
  s.pval_print(6, sample,lm_t1_unadj_M, lm_t1_adj_sex_age_M, lm_t1_adj_M, lmr1_adj_ipcw_M),
  s.pval_print(7, sample,mpo_t2_unadj_M, mpo_t2_adj_sex_age_M, mpo_t2_adj_M, mpo_t2_adj_ipcw_M),
  s.pval_print(8, sample,aat_t2_unadj_M, aat_t2_adj_sex_age_M, aat_t2_adj_M, aat_t2_adj_ipcw_M),
  s.pval_print(9, sample,neo_t2_unadj_M, neo_t2_adj_sex_age_M, neo_t2_adj_M, neo_t2_adj_ipcw_M),
  s.pval_print(10, sample,reg1b_t2_unadj_M, reg1b_t2_adj_sex_age_M, reg1b_t2_adj_M, reg_t2_adj_ipcw_M),
  s.pval_print(11, sample,lac_t2_unadj_M, lac_t2_adj_sex_age_M, lac_t2_adj_M, l2_adj_ipcw_M),
  s.pval_print(12, sample,man_t2_unadj_M, man_t2_adj_sex_age_M, man_t2_adj_M, m2_adj_ipcw_M),
  s.pval_print(13, sample,lm_t2_unadj_M, lm_t2_adj_sex_age_M, lm_t2_adj_M, lmr2_adj_ipcw_M),
  s.pval_print(14, sample,mpo_t3_unadj_M, mpo_t3_adj_sex_age_M, mpo_t3_adj_M, mpo_t3_adj_ipcw_M),
  s.pval_print(15, sample,aat_t3_unadj_M, aat_t3_adj_sex_age_M, aat_t3_adj_M, aat_t3_adj_ipcw_M),
  s.pval_print(16, sample,neo_t3_unadj_M, neo_t3_adj_sex_age_M, neo_t3_adj_M, neo_t3_adj_ipcw_M),
  s.pval_print(17, sample,lac_t3_unadj_M, lac_t3_adj_sex_age_M, lac_t3_adj_M, l3_adj_ipcw_M),
  s.pval_print(18, sample,man_t3_unadj_M, man_t3_adj_sex_age_M, man_t3_adj_M, m3_adj_ipcw_M),
  s.pval_print(19, sample,lm_t3_unadj_M, lm_t3_adj_sex_age_M, lm_t3_adj_M, lmr3_adj_ipcw_M)
)

#Drop out age_sex and ipcw

s.tab7<-s.tab7[,c(1,2,3,4,5,6,10,11,12)]

  cleantable(s.tab7,2)
  
  #Split by time
  s.tab7.m3<-s.tab7[s.tab7[,2]=="3 month",c(1,3:9)]
  s.tab7.1<-s.tab7[s.tab7[,2]=="Year 1",c(1,3:9)]
  s.tab7.2<-s.tab7[s.tab7[,2]=="Year 2",c(1,3:9)]
  
      cleantable(s.tab7.m3,2)
      cleantable(s.tab7.1,2)
      cleantable(s.tab7.2,2)






