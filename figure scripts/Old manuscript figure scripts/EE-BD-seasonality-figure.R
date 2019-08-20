
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

df <- df %>% group_by(variable) %>%
    mutate(logvalue=scale(logvalue))

#Set colors:
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

p1<- ggplot(df, aes(x = caldate)) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
    #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
    geom_segment(data=df[df$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.1) +
    geom_segment(data=df[df$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.1) +
    geom_segment(data=df[df$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.1) +
    geom_segment(data=df[df$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.1) +
      facet_wrap(~variable, ncol=4) +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "z-scored ln-value",
         title = "Z-scored Ln biomarker conc. over calendar time with lines marking measurements",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")


#Add density plots to margins
ggplot(df, aes(x = caldate)) +
  geom_density(aes(group=tr, color=tr, fill=tr), alpha=0.2) +
  facet_wrap(~variable, scale="free_y") +
  scale_colour_manual( values=tableau10[1:4])

p2<- ggplot(df, aes(x = caldate)) +
    geom_point(aes(x=caldate, y=logvalue, group=tr, color=tr),  alpha = 0.1, size=2) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
      facet_wrap(~variable, scale="free_y") +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker concentration over calendar time",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
ggsave(filename="EED-biomarker-seasonality-smooth.pdf",plot = p1,device='pdf',width=9,height=9)
ggsave(filename="EED-biomarker-seasonality-smoothpoints.pdf",plot = p2,device='pdf',width=9,height=9)


#median studymonth br round and arm
df %>% group_by(time,tr) %>% summarize(mean=mean(study_month, na.rm=T), median=median(study_month, na.rm=T))


#Histogram of campling times
ggplot(df, aes(x=study_month, fill=tr)) +
    facet_wrap(~variable) +
    geom_histogram( position="dodge")

ggplot(df, aes(x=study_month, fill=tr)) +
    facet_wrap(~variable) +
    geom_histogram( alpha=.5, position="identity")

#Arm sampling proportion by day (should switch to week or month)
plot_df <- ddply(df,.(study_month, variable),summarise,
              percentage=as.numeric(prop.table(table(tr))),
              tr=names(table(tr)))
ggplot(plot_df) + geom_bar(aes(y = percentage, x = study_month, fill = tr),stat="identity") + facet_wrap(~variable) 






p3<- ggplot(df, aes(x = caldate)) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
    #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter", sixe=12) +
    #coord_cartesian(ylim = c()) +
      facet_wrap(~variable, scale="free_y", ncol = 7) +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker concentration over calendar time",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank(),
          legend.position="none")


#Add density plots to margins
p4<-ggplot(df, aes(x = caldate)) +
  geom_density(aes(group=tr, color=tr, fill=tr), alpha=0.2) +
  facet_wrap(~variable, scale="free_y", ncol = 7) +
  scale_colour_manual( values=tableau10[1:4]) +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank(),
          legend.position="none")



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p3,p4, cols=1)


df1 <- df %>% filter(time==1 & variable!="reg1b")
df2 <- df %>% filter(time==2)
df3 <- df %>% filter(time==3 & variable!="reg1b")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")

pdf("EED-seasonality-plots.pdf",width=9,height=9)
pR1<- ggplot(df1, aes(x = caldate)) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
    #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
    geom_segment(data=df1[df1$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
    geom_segment(data=df1[df1$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
    geom_segment(data=df1[df1$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
    geom_segment(data=df1[df1$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
      facet_wrap(~variable, ncol=3) +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "z-scored ln-value",
         title = "Z-scored Ln biomarker conc. over calendar time - round 1",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")
pR1

pR2<- ggplot(df2, aes(x = caldate)) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
    #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
    geom_segment(data=df2[df2$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
    geom_segment(data=df2[df2$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
    geom_segment(data=df2[df2$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
    geom_segment(data=df2[df2$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
      facet_wrap(~variable, ncol=4) +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "z-scored ln-value",
         title = "Z-scored Ln biomarker conc. over calendar time - round 2",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")
pR2

pR3<- ggplot(df3, aes(x = caldate)) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
    #geom_rug(aes(y=logvalue, group=tr, color=tr),sides="b",alpha = 1/2, position = "jitter") +
    geom_segment(data=df3[df3$tr=="Control",], aes(x=caldate,y=-2,xend =caldate, yend = -1.75), color=tableau10[1], alpha=0.25) +
    geom_segment(data=df3[df3$tr=="WSH",], aes(x=caldate,y=-1.75,xend =caldate, yend = -1.5), color=tableau10[2], alpha=0.25) +
    geom_segment(data=df3[df3$tr=="Nutrition",], aes(x=caldate,y=-1.5,xend =caldate, yend = -1.25), color=tableau10[3], alpha=0.25) +
    geom_segment(data=df3[df3$tr=="Nutrition + WSH",], aes(x=caldate,y=-1.25,xend =caldate, yend = -1), color=tableau10[4], alpha=0.25) +
      facet_wrap(~variable, ncol=3) +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "z-scored ln-value",
         title = "Z-scored Ln biomarker conc. over calendar time - round 3",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")
pR3
dev.off()









#Plot stratified by time and by arm. Maybe add an actual mean of observations, 
#and an expected mean if the observations had been balanced by arm


