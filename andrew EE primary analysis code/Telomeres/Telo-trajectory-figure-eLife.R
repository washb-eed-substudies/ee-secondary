

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


#subset to only children with measurements at both time points
d<-subset(d, !is.na(TS_delta))
table(is.na(d$TS2))
table(is.na(d$TS3))
dim(d)

#Calculate means and 95%CI's at each time point
meansT2<- d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$TS2 , id=.$block.x, print=F)))
meansT3<- d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$TS3 , id=.$block.x, print=F)))
overall.meansT2<- d %>% do(as.data.frame(washb_mean(Y=.$TS2 , id=.$block.x, print=F)))
overall.meansT3<- d %>% do(as.data.frame(washb_mean(Y=.$TS3 , id=.$block.x, print=F)))

T2.mu.ci<-rbind(overall.meansT2[,c(2,5,6)],meansT2[,c(3,6,7)])
T3.mu.ci<-rbind(overall.meansT3[,c(2,5,6)],meansT3[,c(3,6,7)])
diff=T3.mu.ci[,1]-T2.mu.ci[,1]


#Set up  colors
black = "#000004FF"
blue = "#3366AA"
teal = "#11AA99"
green = "#3cc93c"
chartr = "#CCCC55"
magent = "#992288"
red = "#EE3333"
orange = "#EEA722"
yellow = "#FFEE33"
grey = "#777777"
pink = "#f442d4"
greenblue="#009e73"


#-------------------------------
# Plot schema that will be
# repeated for each Ab group in
# the composite figure
#-------------------------------

SLAb.plotLong <- function(Ab1,Ab2,mu1,mu2,diff,labels=c("Control","N+WSH Intervention"),letter="",ylabel=FALSE, ysize=c(1,2), point_col=1, linecols=c(greenblue,black)) {
  # plot individual level trajectories between measurements
  
  # Ab1 : log10 antibody level for each individual at measurement 1
  # Ab2 : log10 antibody level for each individual at measurement 2
  # mu1 : geometric mean and 95% CIs at measurement 1
  # mu2 : geometric mean and 95% CIs at measurement 2
  # diff: difference between geometric means at measurement 1 and measurement 2
  # labels: Antibody status group labels, length 2. e.g., c("Ab-","Ab+")
  # letter: letter for multi-panel plots (e.g., "a")
  # ylabel: logical. print a label for the Y-axis
  
  op <- par(mar=c(3,6,4,0)+0.1, ps = 7, cex = 1, cex.main = 1)
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#3feac2")
  cols <- cbPalette[c(7,6,2,3)]
  #cols <- cbPalette[c(7,6,9,3)]

    mus <- cbind(mu1,mu2)
  
  plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim=c(0,2),ylim=c(ysize[1],ysize[2]))
  xs <- c(0.5,1.5)
  # axes
  mtext(labels,side=3,line=0.5,at=xs,col=cols[point_col])
  
  mtext(c("Year 1","Year 2"),side=1,line=0,at=xs,col=cols[point_col])
  if (ylabel==TRUE) mtext(expression(paste(italic(''), "T/S Ratio")),side=2,line=3.5,cex=1)
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
  ), las=1,cex.axis=1
  )
  
  # header
  mtext(letter,side=3,line=1.75,font=1,at=ifelse(letter=="Control",-0.3,.2),cex=1)
  
  set.seed(12345)
  xs<-cbind(rnorm(1000,mean=0.5, sd=.02),rnorm(1000,mean=1.5, sd=.02))
  
  # plot individual trajectories
  segments(x0=xs[which(Ab2<Ab1),1],x1=xs[which(Ab2<Ab1),2],y0=Ab1[Ab2<Ab1],y1=Ab2[Ab2<Ab1],col=alpha(linecols[1],.2), lwd=2)
  segments(x0=xs[which(Ab2>=Ab1),1],x1=xs[which(Ab2>=Ab1),2],y0=Ab1[Ab2>=Ab1],y1=Ab2[Ab2>=Ab1],col=alpha(linecols[2],.2), lwd=2)
  points(((xs[which(Ab2>=Ab1),1])),Ab1[Ab2>=Ab1],col=alpha(cols[point_col],.3),pch=16)
  points(((xs[which(Ab2<Ab1),1])),Ab1[Ab2<Ab1],col=alpha(cols[point_col],.3),pch=16)
  points(((xs[which(Ab2>=Ab1),2])),Ab2[Ab2>=Ab1],col=alpha(cols[point_col],.3),pch=16)
  points(((xs[which(Ab2<Ab1),2])),Ab2[Ab2<Ab1],col=alpha(cols[point_col],.3),pch=16)
    
  par(op, ps = 7, cex = 1, cex.main = 1)
}



ts_t2_unadj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
ts_t3_unadj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
delta_ts_unadj_M<-washb_glm(Y=d$TS_delta, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
ts_t2_unadj_M
ts_t3_unadj_M
delta_ts_unadj_M



#PDF parameters 
# mm to inch
#setWidth = 183*0.039370 
setWidth = 89*0.039370 

# font size in pt
setFontSize = 7

# 1 in R = 0.75pt, so 0.25pt is specified as 
setLwd <- 0.25/0.75    




setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")
pdf("Telo-trajectories-subsample.pdf",width=setWidth,height=2.5,pointsize=setFontSize)
#Subset data to 1:20 rank ordered lines
i<-5

d %>% group_by(tr) %>%
  summarize(N=n())

nrow(d)
table(d$tr)
dsub1<-d %>%
      subset(tr=="Control") %>%
      arrange(TS2) %>%
      slice(rep(1:floor(n()/i))*i)
      #filter(row_number())
dsub2<-d %>%
      subset(tr=="Nutrition + WSH") %>%
      arrange(TS2) %>%
      slice(rep(1:floor(n()/i))*i)
dsub<-rbind(dsub1,dsub2)
dim(dsub1)
dim(dsub2)
dim(dsub)


#-------------------------------
# make the plot
#-------------------------------


lo <- layout(mat=matrix(1:2,nrow=1,ncol=2,byrow=TRUE))

SLAb.plotLong(Ab1=(dsub$TS2[dsub$tr=="Control"]),Ab2=(dsub$TS3[dsub$tr=="Control"]),mu1=t(T2.mu.ci[2,]),mu2=t(T3.mu.ci[2,]),diff=diff[2],labels=c("",""),letter="Control",ylabel=TRUE, ysize=c(.5,3))
SLAb.plotLong(Ab1=(dsub$TS2[dsub$tr=="Nutrition + WSH"]),Ab2=(dsub$TS3[dsub$tr=="Nutrition + WSH"]),mu1=t(T2.mu.ci[3,]),mu2=t(T3.mu.ci[3,]),diff=diff[3],labels=c("",""),letter="N+WSH Intervention",ylabel=TRUE, ysize=c(.5,3), point_col = 2)

dev.off()







