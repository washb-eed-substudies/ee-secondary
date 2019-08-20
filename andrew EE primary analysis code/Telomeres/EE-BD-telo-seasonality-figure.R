
##########################################
#Plot telomere over time to examine seasonality
##########################################



rm(list=ls())
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
load("telo_figure_data.Rdata")

#Reshape to long format
head(d)
d2 <- d %>% select(date2, TS2, tr) %>% filter(!is.na(TS2)) %>% rename(TS=TS2, date=date2)
d3 <- d %>% select(date3, TS3, tr) %>% filter(!is.na(TS3)) %>% rename(TS=TS3, date=date3)
df<-rbind(d2,d3)
head(df)

#Set calendar date variable
df$caldate <- as.Date(df$date,format="%d%b%Y")


ggplot(df) +
  geom_smooth(aes(x=caldate, y=TS, group=tr, color=tr)) +
  geom_point(aes(x=caldate, y=TS, group=tr, color=tr))


ggplot(df, aes(x = caldate)) +
    geom_smooth(aes(y=TS, group=tr, color=tr)) +
    geom_point(aes(x=caldate, y=TS, group=tr, color=tr),  alpha = 0.5, size=1) +
    #scale_y_continuous(limits = c(1.4,1.6)) +
    labs(x = "Date",
         y = "T/S Ratio",
         title = "Telomere length over calendar time") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")



