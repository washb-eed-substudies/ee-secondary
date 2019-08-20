
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(grid)
library(gridExtra)
library(scales)
library(lattice)

#Useful links
#Facet options:
     #http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
#Coloring X-axis text
     #https://stackoverflow.com/questions/22972478/color-axis-text-by-variable-in-ggplot
#Arranging ggplots:
     #https://github.com/baptiste/gridextra/wiki/arranging-ggplot
#Arranging grobs
     #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#Scaling in grid.arrange
     #https://stackoverflow.com/questions/16298599/keep-or-set-the-ratio-between-text-labels-and-size-of-plot-in-grid-arrange


#---------------------------------------
# Load data
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_res_N_M.Rdata")
load("stool_res_unadj_M.Rdata")
load("urine_res_N_M.Rdata")
load("urine_res_unadj_M.Rdata")
load("stool_res_means.Rdata")
load("urine_res_means.Rdata")

ls()

#-----------------------------------
# Unadjusted difference processing
#-----------------------------------

#Compile N's
aat_N1<-cbind("AAT","T1", rownames(aat_t1_mn), as.data.frame(aat_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N1<-cbind("NEO","T1", rownames(neo_t1_mn), as.data.frame(neo_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N1<-cbind("MPO","T1", rownames(mpo_t1_mn), as.data.frame(mpo_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N1<-cbind("LM","T1", rownames(lm_t1_mn), as.data.frame(lm_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N1<-cbind("Lact","T1", rownames(lac_t1_mn), as.data.frame(lac_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N1<-cbind("Mann","T1", rownames(man_t1_mn), as.data.frame(man_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N2<-cbind("AAT","T2", rownames(aat_t2_mn), as.data.frame(aat_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N2<-cbind("NEO","T2", rownames(neo_t2_mn), as.data.frame(neo_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N2<-cbind("MPO","T2", rownames(mpo_t2_mn), as.data.frame(mpo_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N2<-cbind("LM","T2", rownames(lm_t2_mn), as.data.frame(lm_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
reg_N2<-cbind("REG","T2", rownames(reg1b2_t2_mn), as.data.frame(reg1b2_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N2<-cbind("Lact","T2", rownames(lac_t2_mn), as.data.frame(lac_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N2<-cbind("Mann","T2", rownames(man_t2_mn), as.data.frame(man_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N3<-cbind("AAT","T3", rownames(aat_t3_mn), as.data.frame(aat_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N3<-cbind("NEO","T3", rownames(neo_t3_mn), as.data.frame(neo_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N3<-cbind("MPO","T3", rownames(mpo_t3_mn), as.data.frame(mpo_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N3<-cbind("LM","T3", rownames(lm_t3_mn), as.data.frame(lm_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N3<-cbind("Lact","T3", rownames(lac_t3_mn), as.data.frame(lac_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N3<-cbind("Mann","T3", rownames(man_t3_mn), as.data.frame(man_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 






#Compile differences
aat_dif1<-cbind("AAT","T1", rownames(aat_t1_unadj_M), as.data.frame(aat_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif1<-cbind("NEO","T1", rownames(neo_t1_unadj_M), as.data.frame(neo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif1<-cbind("MPO","T1", rownames(mpo_t1_unadj_M), as.data.frame(mpo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif1<-cbind("LM","T1", rownames(lm_t1_unadj_M), as.data.frame(lm_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif1<-cbind("Lact","T1", rownames(lac_t1_unadj_M), as.data.frame(lac_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif1<-cbind("Mann","T1", rownames(man_t1_unadj_M), as.data.frame(man_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 



aat_dif2<-cbind("AAT","T2", rownames(aat_t2_unadj_M), as.data.frame(aat_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif2<-cbind("NEO","T2", rownames(neo_t2_unadj_M), as.data.frame(neo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif2<-cbind("MPO","T2", rownames(mpo_t2_unadj_M), as.data.frame(mpo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif2<-cbind("LM","T2", rownames(lm_t2_unadj_M), as.data.frame(lm_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
reg_dif2<-cbind("REG","T2", rownames(reg1b_t2_unadj_M), as.data.frame(reg1b_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif2<-cbind("Lact","T2", rownames(lac_t2_unadj_M), as.data.frame(lac_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif2<-cbind("Mann","T2", rownames(man_t2_unadj_M), as.data.frame(man_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_dif3<-cbind("AAT","T3", rownames(aat_t3_unadj_M), as.data.frame(aat_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif3<-cbind("NEO","T3", rownames(neo_t3_unadj_M), as.data.frame(neo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif3<-cbind("MPO","T3", rownames(mpo_t3_unadj_M), as.data.frame(mpo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif3<-cbind("LM","T3", rownames(lm_t3_unadj_M), as.data.frame(lm_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif3<-cbind("Lact","T3", rownames(lac_t3_unadj_M), as.data.frame(lac_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif3<-cbind("Mann","T3", rownames(man_t3_unadj_M), as.data.frame(man_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 





#Construct dataframes of results
dif_df<-(rbind(
aat_dif1,
neo_dif1,
mpo_dif1,
l_dif1,
m_dif1,
lm_dif1,
aat_dif2,
neo_dif2,
mpo_dif2,
l_dif2,
m_dif2,
lm_dif2,
reg_dif2,
aat_dif3,
neo_dif3,
mpo_dif3,
l_dif3,
m_dif3,
lm_dif3
))
rownames(dif_df)<-NULL
#dif_df<-cbind(dif_df[,1:3],rep(9999, nrow(dif_df)),dif_df[,4:8])
colnames(dif_df)<-c("Location","round","TR", "Dif","lower.ci","upper.ci", "SD","Robust SE", "Pval")
levels(dif_df$TR)
levels(dif_df$TR)<-c("C v N", "C v N+WSH", "C v WSH", "N v N + WSH", "WSH v N+WSH")

dif_df$Location<-factor(dif_df$Location)
dif_df$round<-factor(dif_df$round)
dif_df$TR<-factor(dif_df$TR)
#dif_df$TR = factor(dif_df$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Drop H2 comparisons
dif_df<-subset(dif_df, TR!="WSH v N+WSH" & TR!="N v N + WSH")

#Drop unneeded columns
dif_df<-dif_df[,c(1:6,9)]

#Round numeric columns
# dif_df[,4]<-as.numeric(sprintf("%1.2f",dif_df[,4]))
# dif_df[,5]<-as.numeric(sprintf("%1.2f",dif_df[,5]))
# dif_df[,6]<-as.numeric(sprintf("%1.2f",dif_df[,6]))
 dif_df[,7]<-as.numeric(sprintf("%1.3f",dif_df[,7]))

#Add comparison group for colors
dif_df$comp.TR<-rep(c("WSH","N","N+WSH"),19)


#Formated comparison for x-axis printing
dif_df$TR.format<-paste0("C vs.\n",dif_df$comp.TR)




N_df<-(rbind(
aat_N1,
neo_N1,
mpo_N1,
l_N1,
m_N1,
lm_N1,
aat_N2,
neo_N2,
mpo_N2,
l_N2,
m_N2,
lm_N2,
reg_N2,
aat_N3,
neo_N3,
mpo_N3,
l_N3,
m_N3,
lm_N3
))
rownames(N_df)<-NULL
colnames(N_df)<-c("Location","round","TR", "N" ,"Dif","SD","Robust SE","lower.ci","upper.ci")
levels(N_df$TR)
levels(N_df$TR)<-c("C", "N", "N+WSH", "WSH")

N_df$Location<-factor(N_df$Location)
N_df$round<-factor(N_df$round)
N_df$comp.TR<-N_df$TR<-factor(N_df$TR)

#Drop unneeded columns
N_df<-N_df[,c(1:5,8,9)]

#Round the numeric columns that are printed
# N_df[,4]<-as.numeric(sprintf("%1.2f",N_df[,4]))
# N_df[,5]<-as.numeric(sprintf("%1.2f",N_df[,5]))
# N_df[,6]<-as.numeric(sprintf("%1.2f",N_df[,6]))
# N_df[,7]<-as.numeric(sprintf("%1.2f",N_df[,7]))

#Add formatted treatment for x-acis label
#N_df$TR.format<-N_df$TR  
N_df$TR.format<-ifelse(N_df$TR=="N+WSH",
  paste0("N+\nWSH"),
    paste0(N_df$TR,"\n"))
N_df$TR.format<-ifelse(N_df$TR=="WSH",
  paste0("\nWSH"),
    N_df$TR.format)

#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"

#tr.cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)
tr.cols=c(C=cblack,WSH=cblue,N=cred,"WSH+N"=cgreen)

#cols=c("C v WSH"=corange,"C v N"=cred,"C v N+WSH"=cmagent)
cols=c("C v WSH"=cblue,"C v N"=cred,"C v N+WSH"=cgreen)



arms=c("C", "W", "S", "H", "WSH")


# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


#Order data
# unique(prev.dat$Location)
# unique(prev.dat$TR)
# unique(prev.dat$round)
# prev.dat$Location<-factor(prev.dat$Location)
# table(prev.dat$Location)
# prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies"))
# table(prev.dat$Location)
# prev.dat$TR
# j<-2
# k<-3
# d=N_df[N_df$Location==levels(N_df$Location)[k] & N_df$round==levels(N_df$round)[j],]
# i=3 
# yrange="auto"
# cols=tr.cols
# n=4
# lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
# op <- par(mar=c(4,1,3,0.5)+0.1)

#for(i in 1:24){
diffplot<-function(d, i, cols=cols, yrange="auto", n=3){
  
  if(nrow(d)==0){
       op <- par(mar=c(3,1,2,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
  ytics <- if(yrange[1]=="auto"){
    seq(min(d$lower.ci),max(d$upper.ci),by=(max(d$upper.ci)-min(d$lower.ci))/10)
    }else{
    seq(yrange[1],yrange[2],by=.2) #<----------Set the Y-axis range here
      }  


if(i==1 | i==9 | i==17){
   op <- par(mar=c(3,0,2,1)+0.1)

	if(i==1){ulabplot("3 Month\nDifference")}
	if(i==9){ulabplot("Year 1\nDifference")}
	if(i==17){ulabplot("Year 2\nDifference")}

   	#,side=2,line=3,las=1)

	}else{
   #op <- par(mar=c(4,1,3,0.5)+0.1)
   op <- par(mar=c(3,1,2,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:n, names.arg=NA,col=NA,
                  border=NA, 
	  	ylim= if(yrange[1]=="auto"){c(min(d$lower.ci),max(d$upper.ci))}else{c(range(yrange)[1],range(yrange)[2])},
	  	ylab="",yaxt="n",
      las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	segments(x0=0,x1=max(MidPts+0.5),y0=0,lty=1,lwd=1,col="black")

	
	if(i==2 | i==10 | i==18){
	    axis(2,at=ytics,las=1)
	}

	
	# plot estimates
	arrows(x0=MidPts, y0=d$lower.ci, y1=d$upper.ci, col=cols,lwd=2,length=0.05,angle=90,code=3)
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=1,col=cols,bg="white")
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))
	text(x=MidPts,y=d$upper.ci, labels=d$Pval,pos=3,cex=1,col=cols,font=1)
	  # X-axis labels
  mtext(d$TR.format,side=1,line=2,at=MidPts,col=cols,cex=0.8,las=1)
  #mtext(d$Location,side=3,line=0.25,col="gray20",cex=1)
           mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
             box()
  }
}
  
	# print header and footer labels
	# mtext(glab,at=MidPts,side=3,line=6,col=cols,font=1  )
	# hx <- MidPts[1]-0.5
	# prform <- function(pr,lb,ub) {
	# 	paste(sprintf("%1.2f",pr)," (",sprintf("%1.2f",lb),", ",sprintf("%1.2f",ub),")",sep="")
	# }
}

#------------------------------------------
# Outcome means function
#------------------------------------------

#Figure out Y-axis range for different sample types
AAT<-N_df[N_df$Location=="AAT",]
NEO<-N_df[N_df$Location=="NEO",]
MPO<-N_df[N_df$Location=="MPO",]
Lact<-N_df[N_df$Location=="Lact",]
Mann<-N_df[N_df$Location=="Mann",]
LM<-N_df[N_df$Location=="LM",]
REG<-N_df[N_df$Location=="REG",]

seq(min(AAT$lower.ci),max(AAT$upper.ci),by=diff(range(min(AAT$lower.ci),max(AAT$upper.ci)))/10)
c(min(AAT$lower.ci),max(AAT$upper.ci))
c(min(NEO$lower.ci),max(NEO$upper.ci))
c(min(MPO$lower.ci),max(MPO$upper.ci))
c(min(Lact$lower.ci),max(Lact$upper.ci))
c(min(Mann$lower.ci),max(Mann$upper.ci))
c(min(LM$lower.ci),max(LM$upper.ci))
c(min(REG$lower.ci),max(REG$upper.ci))



meanplot<-function(d, i, cols=cols, yrange, n=3){
#   l<-2
#   j<-2
#   i<-3
# d=N_df[N_df$Location==levels(N_df$Location)[l] & N_df$round==levels(N_df$round)[j],]
# i=i
# yrange=c(0,1.5)
# cols=tr.cols
# n=4

  if(nrow(d)==0){
       op <- par(mar=c(3,3,2,0.5)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
  }else{
  
  if(d$Location=="AAT"){yrange<-c(-1.5,-0.5)
    ytics <- seq(-1.5,-0.5,by=0.1)}
  if(d$Location=="NEO"){yrange<-c(6,8)
    ytics <- seq(6,8,by=.2)}
  if(d$Location=="MPO"){yrange<-c(7.25,9.75)
    ytics <- seq(7,10,by=.25)}
  if(d$Location=="Lact"){yrange<-c(-1.8,0.2)
    ytics <- seq(-1.8,0.2,by=0.2)}
  if(d$Location=="Mann"){yrange<-c(0.2,2.2)
    ytics <- seq(0.2,2.2,by=.2)}
  if(d$Location=="LM"){yrange<-c(-4,-2)
    ytics <- seq(-4,-2,by=0.2)}
  if(d$Location=="REG"){yrange<-c(4.8,5.2)
    ytics <- seq(4.8,5.2,by=.05)}

  
  #seq(min(d$lower.ci),max(d$upper.ci),by=(max(d$upper.ci)-min(d$lower.ci))/10)
  # ytics <- if(yrange[1]=="auto"){
  #   seq(min(d$lower.ci),max(d$upper.ci),by=(max(d$upper.ci)-min(d$lower.ci))/10)
  #   }else{
  #   seq(yrange[1],yrange[2],by=.1) #<----------Set the Y-axis range here
  #     }  
  # ytics <- seq(min(d$lower.ci),max(d$upper.ci),by=diff(range(d$Dif))/10)
  # ytics <- round(ytics,2)

if(i==1 | i==9 | i==17){
   op <- par(mar=c(3,0,2,1)+0.1)

	if(i==1){ulabplot("3 Month\nMean")}
	if(i==9){ulabplot("Year 1\nMean")}
	if(i==17){ulabplot("Year 2\nMean")}

   	#,side=2,line=3,las=1)

	}else{
   #op <- par(mar=c(4,1,3,0.5)+0.1)
   op <- par(mar=c(3,3,2,0.5)+0.1)

   # set up an empty plot
MidPts <- barplot(1:n, names.arg=NA,col=NA,
                  border=NA, 
	  	#ylim= if(yrange[1]=="auto"){c(min(d$lower.ci),max(d$upper.ci))}else{c(range(yrange)[1],range(yrange)[2])},
	  	ylim= yrange,
	  	ylab="",yaxt="n",
      las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	
	#if(i==2 | i==10 | i==18){
	    axis(2,at=ytics,las=1)
	#}

	
	# plot estimates
	arrows(x0=MidPts, y0=d$lower.ci, y1=d$upper.ci, col=cols,lwd=2,length=0.05,angle=90,code=3)
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=1,col=cols,bg="white")
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))
	text(x=MidPts,y=d$upper.ci, labels=d$Pval,pos=3,cex=1,col=cols,font=1)
	  # X-axis labels
  mtext(d$TR.format,side=1,line=2,at=MidPts,col=cols,cex=0.8,las=1)
  #mtext(d$Location,side=3,line=0.25,col="gray20",cex=1)
           mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
             box()
  }
}
  

}


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE-Difference-Plot.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
op <- par(mar=c(4,0,3,0)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:8){
    ifelse(k==1,l<-1,l<-k-1)
    diffplot(d=dif_df[dif_df$Location==levels(dif_df$Location)[l] & dif_df$round==levels(dif_df$round)[j],], i, yrange=c(-1,1), cols=cols, n=3)
    i<-i+1
    }
}

dev.off()

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE-Mean-Plot.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
op <- par(mar=c(4,0,3,0)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:8){
    ifelse(k==1,l<-1,l<-k-1)
    meanplot(d=N_df[N_df$Location==levels(N_df$Location)[l] & N_df$round==levels(N_df$round)[j],], i=i, yrange=c(0,1.5), cols=tr.cols, n=4)
    i<-i+1
    }
  }

 
dev.off()

  



#------------------------------------------
# Outcome absolute (not-log transformed)
# means function
#------------------------------------------



abs.meanplot<-function(d, i, cols=cols, yrange, n=3){

  if(nrow(d)==0){
       op <- par(mar=c(3,3,2,0.5)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
  }else{
  
  if(d$Location=="AAT"){yrange<-c(0.3,1.3)
    ytics <- seq(0.3,1.3,by=0.1)}
  if(d$Location=="NEO"){yrange<-c(500,2500)
    ytics <- seq(500,2500,by=200)}
  if(d$Location=="MPO"){yrange<-c(3000,23000)
    ytics <- seq(3000,23000,by=2000)}
  if(d$Location=="Lact"){yrange<-c(0.2,2.7)
    ytics <- seq(0.2,2.7,by=0.25)}
  if(d$Location=="Mann"){yrange<-c(0,20)
    ytics <- seq(0,20,by=2)}
  if(d$Location=="LM"){yrange<-c(0,0.25)
    ytics <- seq(0,0.25,by=0.025)}
  if(d$Location=="REG"){yrange<-c(150,250)
    ytics <- seq(150,250,by=10)}


if(i==1 | i==9 | i==17){
   op <- par(mar=c(3,0,2,1)+0.1)

	if(i==1){ulabplot("3 Month\nMean")}
	if(i==9){ulabplot("Year 1\nMean")}
	if(i==17){ulabplot("Year 2\nMean")}

   	#,side=2,line=3,las=1)

	}else{
   #op <- par(mar=c(4,1,3,0.5)+0.1)
   op <- par(mar=c(3,3,2,0.5)+0.1)

   # set up an empty plot
MidPts <- barplot(1:n, names.arg=NA,col=NA,
                  border=NA, 
	  	#ylim= if(yrange[1]=="auto"){c(min(d$lower.ci),max(d$upper.ci))}else{c(range(yrange)[1],range(yrange)[2])},
	  	ylim= yrange,
	  	ylab="",yaxt="n",
      las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	
	#if(i==2 | i==10 | i==18){
	    axis(2,at=ytics,las=1)
	#}

	
	# plot estimates
	arrows(x0=MidPts, y0=d$lower.ci, y1=d$upper.ci, col=cols,lwd=2,length=0.05,angle=90,code=3)
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=1,col=cols,bg="white")
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))
	text(x=MidPts,y=d$upper.ci, labels=d$Pval,pos=3,cex=1,col=cols,font=1)
	  # X-axis labels
  mtext(d$TR.format,side=1,line=2,at=MidPts,col=cols,cex=0.8,las=1)
  #mtext(d$Location,side=3,line=0.25,col="gray20",cex=1)
           mtext(ifelse(i<9,
                      c("AAT", "NEO", "MPO","Lactulose","Mannitol", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
             box()
    }
  }
}




#Compile N's
aat_absN1<-cbind("AAT","T1", rownames(aat_t1_absmn), as.data.frame(aat_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_absN1<-cbind("NEO","T1", rownames(neo_t1_absmn), as.data.frame(neo_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_absN1<-cbind("MPO","T1", rownames(mpo_t1_absmn), as.data.frame(mpo_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_absN1<-cbind("LM","T1", rownames(lm_t1_absmn), as.data.frame(lm_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_absN1<-cbind("Lact","T1", rownames(lac_t1_absmn), as.data.frame(lac_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_absN1<-cbind("Mann","T1", rownames(man_t1_absmn), as.data.frame(man_t1_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_absN2<-cbind("AAT","T2", rownames(aat_t2_absmn), as.data.frame(aat_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_absN2<-cbind("NEO","T2", rownames(neo_t2_absmn), as.data.frame(neo_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_absN2<-cbind("MPO","T2", rownames(mpo_t2_absmn), as.data.frame(mpo_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_absN2<-cbind("LM","T2", rownames(lm_t2_absmn), as.data.frame(lm_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
reg_absN2<-cbind("REG","T2", rownames(reg1b2_t2_absmn), as.data.frame(reg1b2_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_absN2<-cbind("Lact","T2", rownames(lac_t2_absmn), as.data.frame(lac_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_absN2<-cbind("Mann","T2", rownames(man_t2_absmn), as.data.frame(man_t2_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_absN3<-cbind("AAT","T3", rownames(aat_t3_absmn), as.data.frame(aat_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_absN3<-cbind("NEO","T3", rownames(neo_t3_absmn), as.data.frame(neo_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_absN3<-cbind("MPO","T3", rownames(mpo_t3_absmn), as.data.frame(mpo_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_absN3<-cbind("LM","T3", rownames(lm_t3_absmn), as.data.frame(lm_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_absN3<-cbind("Lact","T3", rownames(lac_t3_absmn), as.data.frame(lac_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_absN3<-cbind("Mann","T3", rownames(man_t3_absmn), as.data.frame(man_t3_absmn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

absN_df<-(rbind(
aat_absN1,
neo_absN1,
mpo_absN1,
l_absN1,
m_absN1,
lm_absN1,
aat_absN2,
neo_absN2,
mpo_absN2,
l_absN2,
m_absN2,
lm_absN2,
reg_absN2,
aat_absN3,
neo_absN3,
mpo_absN3,
l_absN3,
m_absN3,
lm_absN3
))
rownames(absN_df)<-NULL
colnames(absN_df)<-c("Location","round","TR", "N" ,"Dif","SD","Robust SE","lower.ci","upper.ci")
levels(absN_df$TR)
levels(absN_df$TR)<-c("C", "N", "N+WSH", "WSH")

absN_df$Location<-factor(absN_df$Location)
absN_df$round<-factor(absN_df$round)
absN_df$comp.TR<-absN_df$TR<-factor(absN_df$TR)

#Drop unneeded columns
absN_df<-absN_df[,c(1:5,8,9)]

#Add formatted treatment for x-acis label
absN_df$TR.format<-ifelse(absN_df$TR=="N+WSH",
  paste0("N+\nWSH"),
    paste0(absN_df$TR,"\n"))
absN_df$TR.format<-ifelse(absN_df$TR=="WSH",
  paste0("\nWSH"),
    absN_df$TR.format)


#Figure out Y-axis range for different sample types
AAT<-absN_df[absN_df$Location=="AAT",]
NEO<-absN_df[absN_df$Location=="NEO",]
MPO<-absN_df[absN_df$Location=="MPO",]
Lact<-absN_df[absN_df$Location=="Lact",]
Mann<-absN_df[absN_df$Location=="Mann",]
LM<-absN_df[absN_df$Location=="LM",]
REG<-absN_df[absN_df$Location=="REG",]

c(min(AAT$lower.ci),max(AAT$upper.ci))
c(min(NEO$lower.ci),max(NEO$upper.ci))
c(min(MPO$lower.ci),max(MPO$upper.ci))
c(min(Lact$lower.ci),max(Lact$upper.ci))
c(min(Mann$lower.ci),max(Mann$upper.ci))
c(min(LM$lower.ci),max(LM$upper.ci))
c(min(REG$lower.ci),max(REG$upper.ci))





setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE-AbsMean-Plot.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
op <- par(mar=c(4,0,3,0)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:8){
    ifelse(k==1,l<-1,l<-k-1)
    abs.meanplot(d=absN_df[absN_df$Location==levels(absN_df$Location)[l] & absN_df$round==levels(absN_df$round)[j],], i=i, yrange=c(0,1.5), cols=tr.cols, n=4)
    i<-i+1
    }
  }

 
dev.off()


	
#------------------------------------------
# Adj. difference function
#------------------------------------------

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_res_adj_M.Rdata")
load("urine_res_adj_M.Rdata")

contrasts<-c("Control v WSH","Control v Nutrition","Control v Nutrition + WSH","WSH v Nutrition + WSH","Nutrition v Nutrition + WSH")

#Compile differences
aat_adjdiff1<-cbind("AAT","T1", contrasts, as.data.frame(aat_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_adjdiff1<-cbind("NEO","T1", contrasts, as.data.frame(neo_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_adjdiff1<-cbind("MPO","T1", contrasts, as.data.frame(mpo_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_adjdiff1<-cbind("LM","T1", contrasts, as.data.frame(lm_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_adjdiff1<-cbind("Lact","T1", contrasts, as.data.frame(lac_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_adjdiff1<-cbind("Mann","T1", contrasts, as.data.frame(man_t1_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_adjdiff2<-cbind("AAT","T2", contrasts, as.data.frame(aat_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_adjdiff2<-cbind("NEO","T2", contrasts, as.data.frame(neo_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_adjdiff2<-cbind("MPO","T2", contrasts, as.data.frame(mpo_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_adjdiff2<-cbind("LM","T2", contrasts, as.data.frame(lm_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
reg_adjdiff2<-cbind("REG","T2", contrasts, as.data.frame(reg1b_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_adjdiff2<-cbind("Lact","T2", contrasts, as.data.frame(lac_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_adjdiff2<-cbind("Mann","T2", contrasts, as.data.frame(man_t2_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_adjdiff3<-cbind("AAT","T3", contrasts, as.data.frame(aat_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_adjdiff3<-cbind("NEO","T3", contrasts, as.data.frame(neo_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_adjdiff3<-cbind("MPO","T3", contrasts, as.data.frame(mpo_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_adjdiff3<-cbind("LM","T3", contrasts, as.data.frame(lm_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_adjdiff3<-cbind("Lact","T3", contrasts, as.data.frame(lac_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_adjdiff3<-cbind("Mann","T3", contrasts, as.data.frame(man_t3_adj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 


#Construct dataframes of results
adjdif_df<-(rbind(
aat_adjdiff1,
neo_adjdiff1,
mpo_adjdiff1,
l_adjdiff1,
m_adjdiff1,
lm_adjdiff1,
aat_adjdiff2,
neo_adjdiff2,
mpo_adjdiff2,
l_adjdiff2,
m_adjdiff2,
lm_adjdiff2,
reg_adjdiff2,
aat_adjdiff3,
neo_adjdiff3,
mpo_adjdiff3,
l_adjdiff3,
m_adjdiff3,
lm_adjdiff3
))
rownames(adjdif_df)<-NULL
#adjdif_df<-cbind(adjdif_df[,1:3],rep(9999, nrow(adjdif_df)),adjdif_df[,4:8])
colnames(adjdif_df)<-c("Location","round","TR", "Dif","lower.ci","upper.ci", "SD","Robust SE", "Pval")
levels(adjdif_df$TR)
levels(adjdif_df$TR)<-c("C v N", "C v N+WSH", "C v WSH", "N v N + WSH", "WSH v N+WSH")

adjdif_df$Location<-factor(adjdif_df$Location)
adjdif_df$round<-factor(adjdif_df$round)
adjdif_df$TR<-factor(adjdif_df$TR)

#Drop H2 comparisons
adjdif_df<-subset(adjdif_df, TR!="WSH v N+WSH" & TR!="N v N + WSH")

#Drop unneeded columns
adjdif_df<-adjdif_df[,c(1:6,9)]

#Round numeric columns
adjdif_df[,7]<-as.numeric(sprintf("%1.3f",adjdif_df[,7]))

#Add comparison group for colors
adjdif_df$comp.TR<-rep(c("WSH","N","N+WSH"),19)


#Formated comparison for x-axis printing
adjdif_df$TR.format<-paste0("C vs.\n",adjdif_df$comp.TR)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE-AdjDifference-Plot.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
op <- par(mar=c(4,0,3,0)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:8){
    ifelse(k==1,l<-1,l<-k-1)
    diffplot(d=dif_df[dif_df$Location==levels(dif_df$Location)[l] & dif_df$round==levels(dif_df$round)[j],], i, yrange=c(-1.1,1), cols=cols, n=3)
    i<-i+1
    }
}

dev.off()

#------------------------------------------
# IPCW difference function
#------------------------------------------

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_ipcw_res.Rdata")
load("urine_ipcw_res.Rdata")

#Compile differences
aat_ipcw1<-cbind("AAT","T1", rownames(aat_t1_adj_ipcw_M), as.data.frame(aat_t1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
neo_ipcw1<-cbind("NEO","T1", rownames(neo_t1_adj_ipcw_M), as.data.frame(neo_t1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mpo_ipcw1<-cbind("MPO","T1", rownames(mpo_t1_adj_ipcw_M), as.data.frame(mpo_t1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
lm_ipcw1<-cbind("LM","T1", rownames(lmr1_adj_ipcw_M), as.data.frame(lmr1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
l_ipcw1<-cbind("Lact","T1", rownames(l1_adj_ipcw_M), as.data.frame(l1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
m_ipcw1<-cbind("Mann","T1", rownames(m1_adj_ipcw_M), as.data.frame(m1_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

aat_ipcw2<-cbind("AAT","T2", rownames(aat_t2_adj_ipcw_M), as.data.frame(aat_t2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
neo_ipcw2<-cbind("NEO","T2", rownames(neo_t2_adj_ipcw_M), as.data.frame(neo_t2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mpo_ipcw2<-cbind("MPO","T2", rownames(mpo_t2_adj_ipcw_M), as.data.frame(mpo_t2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
lm_ipcw2<-cbind("LM","T2", rownames(lmr2_adj_ipcw_M), as.data.frame(lmr2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
reg_ipcw2<-cbind("REG","T2", rownames(reg_t2_adj_ipcw_M), as.data.frame(reg_t2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
l_ipcw2<-cbind("Lact","T2", rownames(l2_adj_ipcw_M), as.data.frame(l2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
m_ipcw2<-cbind("Mann","T2", rownames(m2_adj_ipcw_M), as.data.frame(m2_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

aat_ipcw3<-cbind("AAT","T3", rownames(aat_t3_adj_ipcw_M), as.data.frame(aat_t3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
neo_ipcw3<-cbind("NEO","T3", rownames(neo_t3_adj_ipcw_M), as.data.frame(neo_t3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mpo_ipcw3<-cbind("MPO","T3", rownames(mpo_t3_adj_ipcw_M), as.data.frame(mpo_t3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
lm_ipcw3<-cbind("LM","T3", rownames(lmr3_adj_ipcw_M), as.data.frame(lmr3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
l_ipcw3<-cbind("Lact","T3", rownames(l3_adj_ipcw_M), as.data.frame(l3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
m_ipcw3<-cbind("Mann","T3", rownames(m3_adj_ipcw_M), as.data.frame(m3_adj_ipcw_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 


#Construct dataframes of results
ipcwdif_df<-(rbind(
aat_ipcw1,
neo_ipcw1,
mpo_ipcw1,
l_ipcw1,
m_ipcw1,
lm_ipcw1,
aat_ipcw2,
neo_ipcw2,
mpo_ipcw2,
l_ipcw2,
m_ipcw2,
lm_ipcw2,
reg_ipcw2,
aat_ipcw3,
neo_ipcw3,
mpo_ipcw3,
l_ipcw3,
m_ipcw3,
lm_ipcw3
))
rownames(ipcwdif_df)<-NULL
#ipcwdif_df<-cbind(ipcwdif_df[,1:3],rep(9999, nrow(ipcwdif_df)),ipcwdif_df[,4:8])
colnames(ipcwdif_df)<-c("Location","round","TR", "Dif", "var","lower.ci","upper.ci", "Pval")
levels(ipcwdif_df$TR)
levels(ipcwdif_df$TR)<-c("C v N", "C v N+WSH", "C v WSH", "N v N + WSH", "WSH v N+WSH")

ipcwdif_df$Location<-factor(ipcwdif_df$Location)
ipcwdif_df$round<-factor(ipcwdif_df$round)
ipcwdif_df$TR<-factor(ipcwdif_df$TR)

#Drop H2 comparisons
ipcwdif_df<-subset(ipcwdif_df, TR!="WSH v N+WSH" & TR!="N v N + WSH")

#Drop unneeded columns
ipcwdif_df<-ipcwdif_df[,c(1:4,6:8)]

#Round numeric columns
ipcwdif_df[,7]<-as.numeric(sprintf("%1.3f",ipcwdif_df[,7]))

#Add comparison group for colors
ipcwdif_df$comp.TR<-rep(c("WSH","N","N+WSH"),19)


#Formated comparison for x-axis printing
ipcwdif_df$TR.format<-paste0("C vs.\n",ipcwdif_df$comp.TR)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE-ipcwDifference-Plot.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:24,ncol=8,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1))
op <- par(mar=c(4,0,3,0)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:8){
    ifelse(k==1,l<-1,l<-k-1)
    diffplot(d=ipcwdif_df[ipcwdif_df$Location==levels(ipcwdif_df$Location)[l] & ipcwdif_df$round==levels(ipcwdif_df$round)[j],], i, yrange=c(-1.1,1), cols=cols, n=3)
    i<-i+1
    }
}

dev.off()



