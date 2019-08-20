

#-------------------------------------------
# Telo-trajectory-figure.R
# Andrew Mertens
# Adapted from code byBen Arnold
#
# plot Wb123 antibody levels for individuals
# measured at both time points
#
#-------------------------------------------

#-------------------------------------------
# input files:
#   
#
# output files:
#   Telo-trajectory-figure.pdf
#-------------------------------------------



#-------------------------------------------
# preamble
#-------------------------------------------

rm(list=ls())
library(RColorBrewer)
library(scales)
library(dplyr)
library(washb)

#-------------------------------------------
# load the full telomere data
#-------------------------------------------
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
load("telo_figure_data.Rdata")


#Rename Nutrition + WSH
levels(d$tr)[7]<-"N+WSH"


#subset to only children with measurements at both time points
#d<-subset(d, !is.na(TS_delta))
#table(is.na(d$TS2))
#table(is.na(d$TS3))

#Calculate means and 95%CI's at each time point
#meansT2<- d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$TS2 , id=.$block.x, print=F)))
#meansT3<- d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$TS3 , id=.$block.x, print=F)))
#overall.meansT2<- d %>% do(as.data.frame(washb_mean(Y=.$TS2 , id=.$block.x, print=F)))
#overall.meansT3<- d %>% do(as.data.frame(washb_mean(Y=.$TS3 , id=.$block.x, print=F)))

#T2.mu.ci<-rbind(overall.meansT2[,c(2,5,6)],meansT2[,c(3,6,7)])
#T3.mu.ci<-rbind(overall.meansT3[,c(2,5,6)],meansT3[,c(3,6,7)])
#diff=T3.mu.ci[,1]-T2.mu.ci[,1]

#-------------------------------
# Plot schema that will be
# repeated for each Ab group in
# the composite figure
#-------------------------------

age.scatter.plot <- function(d,labels=c("Control","WSH+N"),letter="",ylabel=FALSE, ysize=c(1,2)) {

  
  op <- par(mar=c(3,6,4,0)+0.1)
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cols <- cbPalette[c(7,6,2,3)]
  #mus <- cbind(mu1,mu2)
  
  plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim=c(0.5,2),ylim=c(ysize[1],ysize[2]))
  xs <- c(0.5,1.5)
  # axes
  #mtext(labels,side=3,line=0.5,at=xs,col=cols[1:2])
  #mtext(c("Year 1","Year 2"),side=1,line=0,at=xs,col=cols[1:2])
  if (ylabel==TRUE) mtext(expression(paste(italic(''), "T/S Ratio")),side=2,line=3.5,cex=1.25)
  axis(1,at=c(0.5,1,1.5,2),labels=c(
    # expression(10^-1),
    expression(0.5),
    expression(1),
    expression(1.5),
    expression(2)
    #expression(10^4),
    #expression(10^5),
    #expression(10^6)
  ), las=1,cex.axis=1.5
  )
  axis(2,at=c(0,0.5,1,1.5,2,2.5,3),labels=c(
    # expression(10^-1),
    expression(0),
    expression(0.5),
    expression(1),
    expression(1.5),
    expression(2),
    expression(2.5),
    expression(3)
    #expression(10^4),
    #expression(10^5),
    #expression(10^6)
  ), las=1,cex.axis=1.5
  )
  
  # header
  mtext(letter,side=3,line=1.75,font=2,at=-0.3,cex=1.75)

  points(d$agey2[d$tr=="Control"],d$TS2[d$tr=="Control"],col=alpha(cols[1],.3),pch=16)
  points(d$agey2[d$tr=="N+WSH"],d$TS2[d$tr=="N+WSH"],col=alpha(cols[2],.3),pch=16)

  reg1 <- lm(d$TS2[d$tr=="Control"]~d$agey2[d$tr=="Control"],data=d) 
	summary(reg1)
	abline(reg1, lwd = 2.5, col=alpha(cols[3]))
	
	reg2 <- lm(d$TS2[d$tr=="N+WSH"]~d$agey2[d$tr=="N+WSH"],data=d) 
	summary(reg2)
	abline(reg2, lwd = 2.5, col=alpha(cols[4]))
  
  par(op)
}



ts_t2_unadj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
ts_t3_unadj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
delta_ts_unadj_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
ts_t2_unadj_M
ts_t3_unadj_M
delta_ts_unadj_M


#Subset data to 1:20 rank ordered lines
nrow(d)
dsub1<-d %>%
      subset(tr=="Control") %>%
      arrange(TS2) %>%
      slice(rep(1:floor(n()/5))*5)
      #filter(row_number())
dsub2<-d %>%
      subset(tr=="Nutrition + WSH") %>%
      arrange(TS2) %>%
      slice(rep(1:floor(n()/5))*5)
dsub<-rbind(dsub1,dsub2)


#-------------------------------
# make the plot
#-------------------------------


lo <- layout(mat=matrix(1:2,nrow=1,ncol=2,byrow=TRUE))

SLAb.plotLong(Ab1=(dsub$TS2[dsub$tr=="Control"]),Ab2=(dsub$TS3[dsub$tr=="Control"]),mu1=t(T2.mu.ci[2,]),mu2=t(T3.mu.ci[2,]),diff=diff[2],labels=c("",""),letter="Control",ylabel=TRUE, ysize=c(.8,3))
SLAb.plotLong(Ab1=(dsub$TS2[dsub$tr=="Nutrition + WSH"]),Ab2=(dsub$TS3[dsub$tr=="Nutrition + WSH"]),mu1=t(T2.mu.ci[3,]),mu2=t(T3.mu.ci[3,]),diff=diff[3],labels=c("",""),letter="WSH+N",ylabel=TRUE, ysize=c(.8,3))

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")
pdf("Telo-trajectories.pdf",width=10,height=4)

#Full data
lo <- layout(mat=matrix(1:2,nrow=1,ncol=2,byrow=TRUE))

SLAb.plotLong(Ab1=(d$TS2[d$tr=="Control"]),Ab2=(d$TS3[d$tr=="Control"]),mu1=t(T2.mu.ci[2,]),mu2=t(T3.mu.ci[2,]),diff=diff[2],labels=c("",""),letter="Control",ylabel=TRUE, ysize=c(.5,3))
SLAb.plotLong(Ab1=(d$TS2[d$tr=="Nutrition + WSH"]),Ab2=(d$TS3[d$tr=="Nutrition + WSH"]),mu1=t(T2.mu.ci[3,]),mu2=t(T3.mu.ci[3,]),diff=diff[3],labels=c("",""),letter="WSH+N",ylabel=TRUE, ysize=c(.5,3))

dev.off()













