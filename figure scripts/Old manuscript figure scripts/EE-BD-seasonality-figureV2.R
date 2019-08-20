
##########################################
#Plot EED biormarker over time to examine seasonality
##########################################



rm(list=ls())
library(plyr)
library(RColorBrewer)
library(scales)
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes) 
library(gridExtra)
library(lubridate)
library(reshape2)
library(tmleAb)
theme_set(theme_bw())


# --------------------------------------
# load the analysis data and output
# --------------------------------------
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
load("stool_figure_data.Rdata")
stool <- d %>% select(dataid, childNo, tr, date1, date2, date3, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)
#add empty variables for reg1b at time 1 and 3 to allow reshape to work
stool$reg1b1 <- stool$reg1b3 <- NA

varying <- list(c("date1", "date2", "date3"),c("neo1","neo2","neo3"),c("mpo1","mpo2","mpo3"),c("aat1","aat2","aat3"),c("reg1b1","reg1b2","reg1b3"))
stool<-reshape(stool, idvar=c("dataid", "childNo", "tr"), varying =varying , times=c(1:3), sep = "", direction = 'long')
stool <- stool %>% filter(!is.na(neo1) | !is.na(mpo1) | !is.na(aat1) | !is.na(reg1b1))
colnames(stool)[5:9] <-c("date","neo","mpo","aat","reg1b")                     
stool<-melt(stool, id.vars=c("dataid", "childNo", "tr","time","date"))

load("urine_figure_data.Rdata")
urine <- d %>% select(dataid, childNo, tr, date1, date2, date3, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3)
varying <- list(c("date1", "date2", "date3"),c("Lact1","Lact2","Lact3"),c("Mann1","Mann2","Mann3"),c("LM1","LM2","LM3"))
urine<-reshape(urine, idvar=c("dataid", "childNo", "tr"), varying =varying , times=c(1:3), sep = "", direction = 'long')
urine <- urine %>% filter(!is.na(Lact1) | !is.na(Mann1) | !is.na(LM1))
colnames(urine)[5:8] <-c("date","Lact","Mann","LM")                     
urine<-melt(urine, id.vars=c("dataid", "childNo", "tr","time","date"))

#combine urine and stool data
df<-rbind(stool, urine)

#Set calendar date variable
df$caldate <- as.Date(df$date,format="%d%b%Y")

#create month and year variables
df$month <-  as.numeric(month(df$caldate))
df$year <- as.numeric(year(df$caldate))
df$study_month <- df$month + (df$year - min(df$year, na.rm=T))*12

#log transform outcome
df$logvalue <- log(df$value)

#drop empty tr factor levels
df <- droplevels(df)

# df <- df %>% group_by(variable) %>%
#     mutate(logvalue=scale(logvalue))

#Set colors:
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")




df1 <- df %>% filter(time==1 & variable!="reg1b")
df2 <- df %>% filter(time==2)
df3 <- df %>% filter(time==3 & variable!="reg1b")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")

# pdf("EED-seasonality-plots.pdf",width=9,height=9)
# pR1<- ggplot(df1, aes(x = caldate)) +
#     geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
#     #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
#     geom_segment(data=df1[df1$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
#     geom_segment(data=df1[df1$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
#     geom_segment(data=df1[df1$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
#     geom_segment(data=df1[df1$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
#       facet_wrap(~variable, ncol=3) +
#     scale_colour_manual( values=tableau10[1:4]) +
#     labs(x = "Date",
#          y = "z-scored ln-value",
#          title = "Z-scored Ln biomarker conc. over calendar time - round 1",
#          color="Intervention Arm") +
#     theme(#panel.border = element_blank(), 
#           strip.background = element_blank() )#,
#           #legend.position="none")
# pR1
# 
# pR2<- ggplot(df2, aes(x = caldate)) +
#     geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
#     #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
#     geom_segment(data=df2[df2$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
#     geom_segment(data=df2[df2$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
#     geom_segment(data=df2[df2$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
#     geom_segment(data=df2[df2$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
#       facet_wrap(~variable, ncol=4) +
#     scale_colour_manual( values=tableau10[1:4]) +
#     labs(x = "Date",
#          y = "z-scored ln-value",
#          title = "Z-scored Ln biomarker conc. over calendar time - round 2",
#          color="Intervention Arm") +
#     theme(#panel.border = element_blank(), 
#           strip.background = element_blank() )#,
#           #legend.position="none")
# pR2
# 
# pR3<- ggplot(df3, aes(x = caldate)) +
#     geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
#     #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
#     geom_segment(data=df3[df3$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
#     geom_segment(data=df3[df3$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
#     geom_segment(data=df3[df3$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
#     geom_segment(data=df3[df3$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
#       facet_wrap(~variable, ncol=3) +
#     scale_colour_manual( values=tableau10[1:4]) +
#     labs(x = "Date",
#          y = "z-scored ln-value",
#          title = "Z-scored Ln biomarker conc. over calendar time - round 3",
#          color="Intervention Arm") +
#     theme(#panel.border = element_blank(), 
#           strip.background = element_blank() )#,
#           #legend.position="none")
# pR3
# dev.off()






rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}



GAM_simulCI<-function (Y, Age, W = NULL, id = NULL, SL.library = c( "SL.gam"), cvControl = list(V=5), 
                       gamdf = NULL, imputeX=F){
  
  require(SuperLearner)
  if(is.null(id)) 
    id <- 1:length(Y)
  if(is.null(W)){
    nullW <- TRUE
    fulld <- data.frame(id, Y, Age)
  }else{
    nullW <- FALSE
    Wdesign <- design_matrix(W)
    fulld <- data.frame(id, Y, Age, Wdesign)
  }
  if(imputeX==T){
    As <- seq(0, max(fulld$Age), by=0.1)
  }else{
    As <- unique(fulld$Age)
  }
  pY <- rep(NA, length(As))
  fitd <- fulld[complete.cases(fulld), ]
  n.orig <- dim(fulld)[1]
  n.fit <- dim(fitd)[1]
  
  if (n.orig > n.fit) 
    warning(paste("\n\n", n.orig - n.fit, "observations were dropped due to missing values\n in the outcome, age, or adjustement covariates. \n The original dataset contained", 
                  n.orig, "observations,\n but GAM_simulCI is fitting the curve using", 
                  n.fit, "observations."))
  X <- subset(fitd, select = -c(1:2))
  if (length(grep("SL.gam", SL.library)) > 0) {
    set.seed(123456)
    cvGAM <- ab_cvGAM(Y = fitd$Y, X = X, id = fitd$id, SL.library = SL.library, 
                      cvControl = cvControl, df = gamdf)
    SL.library <- cvGAM$SL.library
  }
  
  try(detach(package:gam))
  require(mgcv)
  m <- gam(Y ~ s(Age, k = cvGAM$df_opt), data = fitd,  method = "REML")
  pval<- unlist(summary(m))$s.pv
  
  
  Vb <- vcov(m)
  newd <- seq(min(Age), max(Age), length = nrow(fitd))
  pred <- predict(m, data.frame(Age = newd),  se.fit = TRUE)
  se.fit <- pred$se.fit
  
  
  set.seed(123456)
  N <- 10000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  Cg <- predict(m, data.frame(Age = newd), type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- transform(cbind(data.frame(pred), newd),
                    uprP = fit + (2 * se.fit),
                    lwrP = fit - (2 * se.fit),
                    uprS = fit + (crit * se.fit),
                    lwrS = fit - (crit * se.fit))  
  
  pred<-data.frame(Y=fitd$Y, X=fitd$Age, pred, Pval=rep(pval, nrow(fitd)))  
  
  
  return(pred)    
}




set.seed(12345)




#Plot stratified by time and by arm. Maybe add an actual mean of observations, 
#and an expected mean if the observations had been balanced by arm

# d <- df1 %>% filter(variable=="neo" & (tr=="Nutrition + WSH" | tr=="Control"))
# 
# d$childid<-d$dataid*10+d$childNo
# test <- GAM_simulCI(Y=d$value,Age=as.numeric(d$caldate),id=d$childid, SL.library=SL.library,  gamdf = c(1:10))
# 
# test <- d %>%
#   group_by(tr) %>%
#   do(GAM_simulCI(Y=.$value,Age=as.numeric(.$caldate),id=.$childid, SL.library=SL.library,  gamdf = c(1:10)))


set.seed(2134)
margpred <- df %>% filter(!is.na(caldate) & !is.na(logvalue)) %>% #filter(variable!="variable") %>%
  group_by(variable, time,  tr) %>%
  do(GAM_simulCI(Y=.$logvalue,Age=as.numeric(.$caldate),id=.$childid, SL.library=c("SL.gam"),  gamdf = c(1:10)))

#add mean lines
d<- margpred %>% mutate(meanY=mean(Y), meanFit=mean(fit))

#Create treatment contrast groups (replicate Control 3 times)
Cgroup2<-Cgroup1<-d[d$tr=="Control",]
d$contrast<-NA
d$contrast[d$tr=="Control"|d$tr=="WSH"]<-rep("CvWSH",length(d$contrast[d$tr=="Control"|d$tr=="WSH"]))
d$contrast[d$tr=="Nutrition"]<-rep("CvN",length(d$contrast[d$tr=="Nutrition"]))
d$contrast[d$tr=="Nutrition + WSH"]<-rep("CvWSHN",length(d$contrast[d$tr=="Nutrition + WSH"]))
Cgroup1$contrast <- "CvN"
Cgroup2$contrast <- "CvWSHN"
d<-rbind(d, Cgroup1,Cgroup2)


d1 <- d[d$time==1,]
d2 <- d[d$time==2,]
d3 <- d[d$time==3,]

d1 <- droplevels(d1)
d2 <- droplevels(d2)
d3 <- droplevels(d3)

range(d2$X)
range(d3$X)


pdf("EED-seasonality-plots-TRstrat.pdf",width=12,height=9)

p1<- ggplot(d1, aes(x = newd)) +
   geom_smooth(aes(x=newd, y=fit,  color=tr, group=tr), se = FALSE) +
   geom_ribbon(aes(x=newd, ymin = lwrS , ymax = uprS, color=tr, fill=tr),alpha=0.25, colour=NA) +
   geom_point(aes(y=meanY, x=16125,  color=tr, group=tr), size=3) +
   geom_point(aes(y=meanFit,  x=16175,  color=tr, group=tr), size=3, shape=2) +
   coord_cartesian(xlim=c(15700, 16200)) +
   geom_rug(data=subset(d1,tr=="Control"),aes(x=X,  color=tr)) +
   geom_rug(data=subset(d1,tr!="Control"),aes(x=X,  color=tr),sides="t") +
    facet_grid(variable~contrast,  scales = "free") +
    scale_colour_manual( values=tableau10[1:4]) +
    scale_fill_manual( values=tableau10[1:4], guide=FALSE) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker conc. over calendar time - round 1",
         color="Intervention Arm") +
    theme(strip.background = element_blank())
p1

p2<- ggplot(d2, aes(x = newd)) +
   geom_smooth(aes(x=newd, y=fit,  color=tr, group=tr), se = FALSE) +
   geom_ribbon(aes(x=newd, ymin = lwrS , ymax = uprS, color=tr, fill=tr),alpha=0.25, colour=NA) +
   geom_point(aes(y=meanY, x=16425,  color=tr, group=tr), size=3) +
   geom_point(aes(y=meanFit,  x=16475,  color=tr, group=tr), size=3, shape=2) +
   coord_cartesian(xlim=c(16010, 16500)) +
   geom_rug(data=subset(d2,tr=="Control"),aes(x=X,  color=tr)) +
   geom_rug(data=subset(d2,tr!="Control"),aes(x=X,  color=tr),sides="t") +
    facet_grid(variable~contrast,  scales = "free") +
    scale_colour_manual( values=tableau10[1:4]) +
    scale_fill_manual( values=tableau10[1:4], guide=FALSE) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker conc. over calendar time - round 2",
         color="Intervention Arm") +
    theme(strip.background = element_blank())
p2

p3<- ggplot(d3, aes(x = newd)) +
   geom_smooth(aes(x=newd, y=fit,  color=tr, group=tr), se = FALSE) +
   geom_ribbon(aes(x=newd, ymin = lwrS , ymax = uprS, color=tr, fill=tr),alpha=0.25, colour=NA) +
   geom_point(aes(y=meanY, x=16925,  color=tr, group=tr), size=3) +
   geom_point(aes(y=meanFit,  x=16975,  color=tr, group=tr), size=3, shape=2) +
   coord_cartesian(xlim=c(16500, 17000)) +
   geom_rug(data=subset(d3,tr=="Control"),aes(x=X,  color=tr)) +
   geom_rug(data=subset(d3,tr!="Control"),aes(x=X,  color=tr),sides="t") +
    facet_grid(variable~contrast,  scales = "free") +
    scale_colour_manual( values=tableau10[1:4]) +
    scale_fill_manual( values=tableau10[1:4], guide=FALSE) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker conc. over calendar time - round 3",
         color="Intervention Arm") +
    theme(strip.background = element_blank())
p3


dev.off()