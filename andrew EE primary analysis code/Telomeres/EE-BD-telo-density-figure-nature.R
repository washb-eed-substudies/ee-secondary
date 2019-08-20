


##########################################
#Plot telomere length densities
##########################################


# --------------------------------------
# input files:
#
# output files:
#	BD-telo-t2.pdf
# BD-telo-t3.pdf
# --------------------------------------

rm(list=ls())
try(detach(package:plyr))
library(RColorBrewer)
library(scales)
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes) 
library(gridExtra)


# --------------------------------------
# load the analysis data and output
# --------------------------------------
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
load("telo_figure_data.Rdata")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_res.Rdata")


# --------------------------------------
# rename analysis output objects
# for convenience
# --------------------------------------
t2sum  <- ts_t2_N_M
t2diff <- ts_t2_unadj_M

t3sum  <- ts_t3_N_M
t3diff <- ts_t3_unadj_M


levels(t2sum$tr)[7]<-levels(t3sum$tr)[7]<-"N + WSH"



#---------------------------------------
# clean the analysis data
#---------------------------------------

#Rename Nutrition + WSH
levels(d$tr)[7]<-"N+WSH"

# subset to the relevant measurement
ad2 <- subset(d, !is.na(TS2) & !is.na(tr))
dim(ad2)
ad3 <- subset(d, !is.na(TS3) & !is.na(tr))
dim(ad3)

ad2$bp<-3274 + 2413*ad2$TS2
ad3$bp<-3274 + 2413*ad3$TS3


x=ad2$TS2[ad2$tr=="Control"]
y=ad2$TS2[ad2$tr=="N+WSH"]
namex="Control"
namey="N + WSH"
dstats=NULL
main="Telomere length (T/S ratio), Year 1"
letter="a"
cols=cols[c(1,2)]
mulab=TRUE


# --------------------------------------
# plotting and analysis function
# for comparison of 2 groups using
# kernel densities
# --------------------------------------
lazdenplot <- function(x,y,namex,namey,dstats,main,letter,cols,ylab=TRUE,mulab=TRUE) {
	# x : a series of LAZ data from the first group
	# y : a series of LAZ data from the second group
	# namex : string. name for x group
	# namey : string. name for y group
	# dstats : a vector of length 5 with t-test statistics (diff, min, max, t-stat, P)
	# main : string. plot title
	# letter : letter for multipanel plots
	# cols  : colors used for plotting
	# ylab : Label the Y-axis?
  # mulab : logical. label group mean dots?


	# format the difference and 95% CI
	diff <- paste(sprintf("%1.2f",dstats[1])," (",sprintf("%1.2f",dstats[2]),", ",sprintf("%1.2f",dstats[3]),")",sep="")


	# make the empty plot
	op <- par(xpd=FALSE,mar=c(4,5,3,2)+0.1, ps = 7, cex = 1, cex.main = 1)
	ytics <- seq(0,3,by=0.5)
	xtics <- seq(0,3,by=0.5)
	plot(density(x),type="n",
		main="",
		ylim=c(0,3),yaxt="n",ylab="",
		xlim=c(0,2),xaxt="n",xlab="",
		las=1,bty="n"
		)
		axis(1,at=xtics,las=1,cex.axis=1)
		mtext("T/S Ratio",side=1,line=2.5)
		if(ylab==TRUE) {
		  axis(2,at=ytics,las=1,cex.axis=1,lwd=1.5,lwd.ticks=1)
		  mtext("Kernel Density",side=2,line=3.5)
		}
		
		mtext(main,side=3,line=1,cex=1,adj=0)
		#mtext(letter,side=3,line=2,cex=1,adj=0,at=-6)

		# draw shaded regions for stunted and severely stunted (not used)
		minx <- min(xtics)
		miny <- min(ytics)
		maxy <- max(ytics)
		# polygon(x=c(-3,-2,-2,-3),y=c(miny,miny,maxy,maxy),border=NA,col="gray95")
		# polygon(x=c(minx,-3,-3,minx),y=c(miny,miny,maxy,maxy),border=NA,col="gray90")
		# mtext(c("Severe","Stunted"),at=c(-4,-2.5),side=3,line=-0.5,cex=0.8,col="gray30")

		# add a marker at -2
		#segments(x0=-2,y0=0,y1=0.25,lty=1,col="gray40")

		# draw kernal density distributions
		dx <- density(x)
		dy <- density(y)
		lines(dx,col=cols[1],lwd=1.5,lty=2)
		lines(dy,col=cols[2],lwd=1.5)

		# label group means
		if(mulab==TRUE) {
			segments(x0=mean(x),y0=miny-0.015,y1=0.05,col="gray40",lty=2)
			segments(x0=mean(y),y0=miny-0.015,y1=0.05,col="gray40",lty=2)
			text(x=mean(c(x,y)),y=0.4,"Group Means",col="gray40",cex=1)

		}
		# segments(x0=mean(x),y0=miny-0.015,y1=max(dx$y)+0.05,col="gray40",lty=2)
		# segments(x0=mean(x),x1=mean(x)+0.2,y0=max(dx$y)+0.05,col="gray40",lty=2)
		op <- par(xpd=TRUE, ps = 7, cex = 1, cex.main = 1)
		points(mean(x),miny-0.015,pch=21,cex=1.75,col=cols[1],bg=alpha(cols[1],alpha=0.5))

		# segments(x0=mean(y),y0=miny-0.015,y1=max(dy$y)+0.02,col="gray40",lty=2)
		# segments(x0=mean(y),x1=mean(y)+0.2,y0=max(dy$y)+0.02,col="gray40",lty=2)
		points(mean(y),miny-0.015,pch=21,cex=1.75,col=cols[2],bg=alpha(cols[2],alpha=0.5))


		# # add labels
		# text(x=mean(x)+0.3,y=max(dx$y)+0.05,paste("Mean (SD) in Control: ",muform(x)),adj=0,cex=0.8,col="gray40")
		# text(x=mean(y)+0.3,y=max(dy$y)+0.02,paste("Mean (SD) in WSH: ",muform(y)),adj=0,cex=0.8,col="gray40")


		# draw a small table in the upper right
		#txs <- c(-1,0,1.1,2)
		txs <- c(1,1.25,1.5,1.75)
		#		txs <- c(1.3,1.4,1.5,1.6)
		txs2 <- c(2.2)
		# mtext("LAZ",side=3,line=0,at=txs[1],adj=1)
			mtext(c("","N","Mean","SD"),side=3,line=0,at=txs,cex=1,adj=1)
		mtext(namex,side=3,line=-1.2,at=txs[1],adj=1,col=cols[1])
			mtext(format(length(x),big.mark=","),side=3,line=-1.2,at=txs[2],adj=1,cex=1,col=cols[1])
			mtext(sprintf("%1.2f",mean(x)),side=3,line=-1.2,at=txs[3],adj=1,cex=1,col=cols[1])
			mtext(sprintf("%1.2f",sd(x))  ,side=3,line=-1.2,at=txs[4],adj=1,cex=1,col=cols[1])
		mtext(namey,side=3,line=-2.2,at=txs[1],adj=1,col=cols[2])
		mtext("Intervention",side=3,line=-2.9,at=txs[1],adj=1,col=cols[2])
			mtext(format(length(y),big.mark=","),side=3,line=-2.4,at=txs[2],adj=1,cex=1,col=cols[2])
			mtext(sprintf("%1.2f",mean(y)),side=3,line=-2.4,at=txs[3],adj=1,cex=1,col=cols[2])
			mtext(sprintf("%1.2f",sd(y))  ,side=3,line=-2.4,at=txs[4],adj=1,cex=1,col=cols[2])

		#mtext("Diff. (95% CI)",side=3,line=0,at=txs2,adj=1)
		#	mtext(diff,side=3,line=-2.4,at=txs2,adj=1,col="gray20",cex=0.9)

		#mtext(paste("t-test p =",sprintf("%1.3f",dstats[5])),side=3,line=-4,at=txs2,adj=1,col="gray20",cex=0.9)

		par(op)
}

# --------------------------------------
#  make a multi-panel density plot - year 2
# --------------------------------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442","#6e016b", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette <- c("#999999","#9970ab" ,"#E69F00", "#56B4E9", "#009E73", "#F0E442", "#6e016b", "#0072B2", "#D55E00")

cols = cbPalette
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# 
black = "#000004FF"
blue = "#3366AA"
teal = "#11AA99"
green = "#66AA55"
chartr = "#CCCC55"
magent = "#992288"
red = "#EE3333"
orange = "#EEA722"
yellow = "#FFEE33"
grey = "#777777"
cols=c(black,teal,green,chartr,orange,red,magent,blue,yellow)
cols=c(orange,blue)



#PDF parameters for Nature
# mm to inch
setWidth = 183*0.039370 
setWidth = 89*0.039370 

# font size in pt
setFontSize = 7

# 1 in R = 0.75pt, so 0.25pt is specified as 
setLwd <- 0.25/0.75    



# set up a layout
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")
pdf("telo-density.pdf",width=setWidth,height=3.5,pointsize=setFontSize)
lo <- layout(mat=matrix(1:2,nrow=2,ncol=1,byrow=F))
lazdenplot(x=ad2$TS2[ad2$tr=="Control"],y=ad2$TS2[ad2$tr=="N+WSH"],namex="Control",namey="N + WSH", dstats=NULL, main="Telomere length (T/S ratio), Year 1",letter="a",cols=cols[c(1,2)],mulab=TRUE)
lazdenplot(x=ad3$TS3[ad3$tr=="Control"],y=ad3$TS3[ad3$tr=="N+WSH"],namex="Control",namey="N + WSH", dstats=NULL, main="Telomere length (T/S ratio), Year 2",letter="b",cols=cols[c(1,2)],mulab=TRUE)


dev.off()



