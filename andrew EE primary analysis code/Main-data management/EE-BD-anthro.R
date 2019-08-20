
#---------------------------------------
# EE-BD-anthro.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Examining child anthro over main trial 
# and EE followups
#---------------------------------------

#Clean environment
rm(list=ls())

#Useful functions:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title="",hjust=0,vjust=-7) {
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
    #mtext("Title for Two Plots", outer = TRUE, cex = 1.5)
grid.text(title, gp = gpar(fontsize = 18), hjust=hjust,vjust=vjust)
  }









###Load in data
library(foreign)
library(dplyr)
library(washb)
library(tidyverse)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)

#Load in main study anthro
main_anthro<-read.csv("washb-bangladesh-anthro.csv",stringsAsFactors = TRUE)
head(main_anthro)
main_anthro<-main_anthro %>%
  rename(main_svy=svy) %>%
  select(dataid, childid, clusterid, main_svy, anthrodate, month, dob, aged, agem, agey, laz, laz_x, lazminus2, lazminus3, waz, waz_x, wazminus2, wazminus3, whz, whz_x, whzminus2, whzminus3,  bmiz, bmiz_x, hcz, hcz_x)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")
anthro<-read.csv("BD-EE-anthro.csv",stringsAsFactors = TRUE)

#Merge treatment information 
telo <- telo %>% subset(., select = c(dataid, childNo, clusterid))
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
table(is.na(d$tr))




#Merge in anthropometry measures
head(anthro)
dim(d)
dim(anthro)
d<-left_join(d, anthro, by=c("dataid", "childNo"))
dim(d)
head(d)



#Merge treatment information 
dim(anthro)
d<-left_join(anthro,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Append main study anthro data to EE anthro data
    #First, drop main study children not in EE
EEchildren<-d %>% select(dataid, childNo, svy) %>% distinct(dataid, childNo)

dim(EEchildren)
dim(main_anthro)
anthrochildren<-semi_join(main_anthro, EEchildren, by = "dataid", "childNo") 
dim(anthrochildren)

#Clean EE and main study datasets into appendable forms
head(anthrochildren)

anthrochildren<- anthrochildren %>%
    mutate(childNo=as.integer(substr(childid,2,2)), svy=main_svy+0.5, EEdata=0) %>%
    rename(date=anthrodate) %>%
    select(dataid, childNo, clusterid, svy, date, dob, aged,agem,agey,laz,laz_x, lazminus2, lazminus3, waz, waz_x, wazminus2, wazminus3, whz, whz_x, whzminus2, whzminus3)

head(d)
d<- d %>% 
  mutate(EEdata=1) %>% 
  rename(dob=DOB) %>% 
  select(dataid, childNo, clusterid, svy, date, dob, aged,agem,agey,laz,laz_x, lazminus2, lazminus3, waz, waz_x, wazminus2, wazminus3, whz, whz_x, whzminus2, whzminus3)

#Next, append main  study data to EE data
d<-bind_rows(d, anthrochildren)

#create unique child identifier
d$childid<-d$dataid*10+d$childNo
d$childid<-factor(d$childid)


#Crosstab wasting percentages over each survey round
d %>% group_by(svy) %>% summarize(wast_perc=mean(lazminus2, na.rm=T), severe_wast_perc=mean(wazminus3,  na.rm=T))


#Order children by sample dates and create new measurement number variable
d<-d %>%
  arrange(childid, childNo, date) %>%
  group_by(childid, childNo) %>%
  mutate(meas_num=min_rank(date))  %>%
  ungroup

#Vizualize wasting:

#All:
ggplot(data=d, aes(x=meas_num, y=whz, group=childid, color=childid)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none")

#Only those children who had wasting at any time point 
anywast<-d %>% group_by(childid)  %>%   mutate(num_was=sum(lazminus2, na.rm=T))  %>% subset(num_was>0) %>% ungroup 
ggplot(data=anywast, aes(x=meas_num, y=whz, group=childid, color=childid)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none")

#Only those children who had severe wasting at any time point
anysevwast<-d %>% group_by(childid)  %>%   mutate(num_sevwas=sum(lazminus3, na.rm=T))  %>% subset(num_sevwas>0) %>% ungroup 
ggplot(data=anysevwast, aes(x=meas_num, y=whz, group=childid, color=childid)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none")


#Only those children who had severe wasting at any time point but recover
anysevwast<-d %>% group_by(childid)  %>%   mutate(num_sevwas=sum(lazminus3, na.rm=T))  %>% subset(num_sevwas>0 & num_sevwas<5) %>% ungroup 
ggplot(data=anysevwast, aes(x=meas_num, y=whz, group=childid, color=childid)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none")

ggplot(data=anysevwast, aes(x=date, y=whz, group=childid, color=childid)) +
    geom_line() +
    theme(legend.position = "none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Make line graphs of kids who have switched from wasted to non-wasted or vice-versa at least twice.
d2<- d %>% arrange(childid, meas_num) %>% mutate(wast_lag1=lag(whzminus2), swast_lag1=lag(whzminus3)) 
d2$wast_lag1[d$meas_num==1]<-NA
d2$swast_lag1[d$meas_num==1]<-NA
d2$wastchange<-d2$whzminus2-d2$wast_lag1
d2$swastchange<-d2$whzminus3-d2$swast_lag1

print(d2, n = 10, width = Inf)

wastchange<-d2 %>% group_by(childid) %>%  mutate(num_wastchange=sum(abs(wastchange), na.rm=T)) %>% subset(num_wastchange>0 & !is.na(wast_lag1)) %>% ungroup
wastchange_plot<-ggplot(data=wastchange, aes(x=agey, y=whz, group=childid, color=childid)) +
    geom_line() +
    stat_smooth(aes(group = 1)) + 
    theme(legend.position = "none",axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
    coord_cartesian(xlim=c(0,3), ylim = c(-4,0))


swastchange<-d2 %>% group_by(childid) %>%  mutate(num_swastchange=sum(abs(swastchange), na.rm=T)) %>% subset(num_swastchange>0 & !is.na(swast_lag1)) %>% ungroup
swastchange_plot<-ggplot(data=swastchange, aes(x=agey, y=whz, group=childid, color=childid)) +
    geom_line() +
    stat_smooth(aes(group = 1)) + 
    theme(legend.position = "none",axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
    coord_cartesian(xlim=c(0,3), ylim = c(-4,0))




#Compare to stunting
d3<- d %>% arrange(childid, meas_num) %>% mutate(stunt_lag1=lag(lazminus2), sstunt_lag1=lag(lazminus3)) 
d3$stunt_lag1[d$meas_num==1]<-NA
d3$sstunt_lag1[d$meas_num==1]<-NA
d3$stuntchange<-d3$lazminus2-d3$stunt_lag1
d3$sstuntchange<-d3$lazminus3-d3$sstunt_lag1

stuntchange<-d3 %>% group_by(childid) %>%  mutate(num_stuntchange=sum(abs(stuntchange), na.rm=T)) %>% subset(num_stuntchange>0 & !is.na(stunt_lag1)) %>% ungroup
stuntchange_plot<-ggplot(data=stuntchange, aes(x=agey, y=laz, group=childid, color=childid)) +
    geom_line() +
    stat_smooth(aes(group = 1)) + 
    theme(legend.position = "none", axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
    coord_cartesian(xlim=c(0,3), ylim = c(-4,0))

sstuntchange<-d3 %>% group_by(childid) %>%  mutate(num_sstuntchange=sum(abs(sstuntchange), na.rm=T)) %>% subset(num_sstuntchange>0 & !is.na(sstunt_lag1)) %>% ungroup
sstuntchange_plot<-ggplot(data=sstuntchange, aes(x=agey, y=laz, group=childid, color=childid)) +
    geom_line() +
    stat_smooth(aes(group = 1)) +
    theme(legend.position = "none", axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
    coord_cartesian(xlim=c(0,3), ylim = c(-4,0))


multiplot(wastchange_plot, stuntchange_plot, swastchange_plot, sstuntchange_plot, cols=2)








#Make table of, at each time, percentage of kids who become wasted, who recover from wasting, or who stay wasted

wast_change_summary<-d2 %>% group_by(meas_num) %>% subset(!is.na(whzminus2)) %>%
  summarize(n_wast=sum(whzminus2, na.rm=T),per_wast=mean(whzminus2,na.rm=T),n_become_wast=sum(wastchange>0, na.rm=T), per_become_wast=mean(wastchange>0, na.rm=T)/mean(whzminus2,na.rm=T),n_recover_from_wast=sum(wastchange<0, na.rm=T), per_recover_from_wast=mean(wastchange<0, na.rm=T)/mean(whzminus2,na.rm=T),n_stay_wast=sum(wastchange==0 & whzminus2==1, na.rm=T), per_stay_wast=mean(wast_lag1==1 & whzminus2==1, na.rm=T)/mean(whzminus2,na.rm=T))
wast_change_summary

stunt_change_summary<-d3 %>% group_by(meas_num) %>% subset(!is.na(lazminus2)) %>%
  summarize(n_stunt=sum(lazminus2, na.rm=T),per_stunt=mean(lazminus2,na.rm=T),n_become_stunt=sum(stuntchange>0, na.rm=T), per_become_stunt=mean(stuntchange>0, na.rm=T)/mean(lazminus2,na.rm=T),n_recover_from_stunt=sum(stuntchange<0, na.rm=T), per_recover_from_stunt=mean(stuntchange<0, na.rm=T)/mean(lazminus2,na.rm=T),n_stay_stunt=sum(stuntchange==0 & lazminus2==1, na.rm=T), per_stay_stunt=mean(stunt_lag1==1 & lazminus2==1, na.rm=T)/mean(lazminus2,na.rm=T))
stunt_change_summary


