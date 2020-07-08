
#-------------------------------------
# EE substudies analysis 

# configure data directories
# source base functions
# load libraries
#-------------------------------------

library(tidyverse)
library(haven)
library(washb)
library(foreign)
library(data.table)
library(tmle)
library(SuperLearner)
library(devtools)
library(kableExtra)
library(here)

dropboxDir <- NULL
if(dir.exists("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/"
}
if(dir.exists("/Users/audrielin/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "/Users/audrielin/Dropbox/WBB-EE-analysis/"
}
if(dir.exists("C:/Users/Sophia/Dropbox/WASH/")){ 
  dropboxDir <- "C:/Users/Sophia/Dropbox/WASH/"
}
if(dir.exists("/Users/lisa/Dropbox/WASH/")){ 
  dropboxDir <- "/Users/lisa/Dropbox/WASH/"
}
if(dir.exists("/Users/caitlinhemlock/Dropbox/")){ 
  dropboxDir <- "/Users/caitlinhemlock/"
}
if(dir.exists("/Users/zbutzindozier/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "/Users/zbutzindozier/Dropbox/WBB-EE-analysis/"
}




theme_ki<-function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

theme_set(theme_ki())

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F",
               "#BCBD22","#17BECF")


