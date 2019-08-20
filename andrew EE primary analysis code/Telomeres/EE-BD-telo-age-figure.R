
##########################################
#Plot telomere/age scatters
##########################################


# --------------------------------------
# input files:
#
# output files:
#	BD-telo-t2.pdf
# BD-telo-t3.pdf
# --------------------------------------

rm(list=ls())
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


#---------------------------------------
# clean the analysis data
#---------------------------------------

#Rename Nutrition + WSH
levels(d$tr)[7]<-"N+WSH"

#Make longform data
dlong2 <- d %>% subset(!is.na(TS2)) %>%
  select(dataid, tr, TS2, agey2, sex) %>%
  rename(TS=TS2, agey=agey2)
dlong2$year<-"Year 1"
dlong3 <- d %>% subset(!is.na(TS3)) %>%
  select(dataid, tr, TS3, agey3, sex) %>%
  rename(TS=TS3, agey=agey3)
dlong3$year<-"Year 2"

dlong <- rbind(dlong2, dlong3)
dlong %>% group_by(year) %>%
  summarize(meanAge=mean(agey*12, na.rm=T), SD_Age=sd(agey*12, na.rm=T),
             meanTS=mean(TS, na.rm=T), SD_TS=sd(TS, na.rm=T),
            meanTSbp=mean(3274 + 2413*TS, na.rm=T), SD_TSbp=sd(3274 + 2413*TS, na.rm=T))


#Make new blank category to control axis in facet_wrap
head(dlong)
temp<-data.frame(dataid=0,tr="", TS=1.5, agey=3, sex="male", year="Year 2")
temp2<-data.frame(dataid=0,tr="", TS=1.5, agey=2, sex="male", year="Year 1")
dlong2<-rbind(dlong, temp, temp2)




# --------------------------------------
#  Plot age by telomere length
# --------------------------------------


#set up colors
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cols <- cbPalette[c(7,6,2,3)]
  col_legend=c("Control"=cols[1],"N+WSH"=cols[2], "b"="white")

  
age_scatter<-ggplot(data = dlong2) + 
  geom_point(mapping = aes(x = agey, y = TS, color=tr), size=3, alpha=0.2) +
  geom_smooth(method = "lm", mapping = aes(x = agey, y = TS, color=tr),size=1.25, se=FALSE) +
  #scale_x_continuous(limits = ifelse(year=="Year 1", c(0.5,2), c(1.5,3)))+
  #scale_x_continuous(limits = range(dlong$agey, na.rm=T)) +
  facet_wrap(~year, scales = "free_x", ncol = 2)+
  #facet_grid(~year, scales = "free_x")+
  scale_colour_manual(values = col_legend) +
  #theme_bw() +
  #theme_light() +
  #theme_minimal() +
  theme_classic() +
  #theme_tufte() +
  labs(x="Child Age in Years", y="T/S Ratio", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        axis.title.x = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        strip.text.x = element_text(color="#666666", face="bold", size=11),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.spacing = unit(3, "lines")) 
age_scatter


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")
pdf("telo-age-scatter.pdf",width=10,height=4)  

age_scatter

dev.off()

#-------------------------------------------------------------------
#- Old/alternative age/telomere code
#-------------------------------------------------------------------

y1<-ggplot(data = dlong[dlong$year=="Year 1",]) + 
  geom_point(mapping = aes(x = agey, y = TS, color=tr), size=3, alpha=0.2) +
  geom_smooth(method = "lm", mapping = aes(x = agey, y = TS, color=tr),size=1.5, se=FALSE) +
  scale_colour_manual(values = col_legend) +
  scale_x_continuous(limits = c(0.5,2))+
  theme_light() +
  labs(x="Child Age in Years", y="T/S Ratio", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        #axis.title.y = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        #axis.title.x = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        strip.text.x = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        strip.background = element_rect( fill=NA),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.position="none") 

y2<-ggplot(data = dlong[dlong$year=="Year 2",]) + 
  geom_point(mapping = aes(x = agey, y = TS, color=tr), size=3, alpha=0.2) +
  geom_smooth(method = "lm", mapping = aes(x = agey, y = TS, color=tr),size=1.5, se=FALSE) +
  scale_colour_manual(values = col_legend) +
  scale_x_continuous(limits = c(1.5,3))+
  theme_light() +
  labs(x="Child Age in Years", y="T/S Ratio", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        #axis.title.y = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        #axis.title.x = element_text(angle=0, vjust = 0.5, color="#666666", face="bold"),
        strip.text.x = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        strip.background = element_rect( fill=NA),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(2, "lines")) 



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, 
                      labs=list(), labpos=list(c(0.5,0.03), c(0.03,0.5))) {
  require(grid)
  
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
    
    if(!length(labs) == 0){
      grid.text(labs[1], x=labpos[[1]][1], y=labpos[[1]][2], gp=gpar(fontsize=16))
      grid.text(labs[2], x=labpos[[2]][1], y=labpos[[2]][2], rot=90, gp=gpar(fontsize=16))
    }
  }
}


multiplot(y1, y2, cols=2, labs=list("cool x label", "cool y label"))




setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures/")
#pdf("telo-age-scatter.pdf",width=8,height=10)
lo <- layout(mat=matrix(1:2,nrow=2,ncol=1,byrow=F))

ggplot(data = dlong) + 
  geom_point(mapping = aes(x = agey, y = TS, color=tr)) +
  #geom_smooth(mapping = aes(x = agey, y = TS, color=tr)) 
  geom_smooth(method = "lm", mapping = aes(x = agey, y = TS, color=tr)) 


ggplot(data = d) + 
  geom_point(mapping = aes(x = agey2, y = TS_delta, color=tr)) +
  #geom_smooth(mapping = aes(x = agey2, y = TS_delta, color=tr)) 
  geom_smooth(method = "lm", mapping = aes(x = agey2, y = TS_delta, color=tr)) 


ggplot(data = d) + 
  geom_point(mapping = aes(x = agey2, y = TS2, color=tr)) +
  #geom_smooth(mapping = aes(x = agey2, y = TS2, color=tr)) 
  geom_smooth(method = "lm", mapping = aes(x = agey2, y = TS2, color=tr)) 

ggplot(data = d[d$agey3>1.75,]) + 
  geom_point(mapping = aes(x = agey3, y = TS3, color=tr)) +
  #geom_smooth(mapping = aes(x = agey3, y = TS3, color=tr)) +
  geom_smooth(method = "lm", mapping = aes(x = agey3, y = TS3, color=tr)) 


dev.off()