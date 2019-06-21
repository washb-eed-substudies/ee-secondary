
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
library(here)

dropboxDir <- NULL
if(dir.exists("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/"
}
if(dir.exists("C:/Users/audrielin/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "C:/Users/audrielin/Dropbox/WBB-EE-analysis/"
}
if(dir.exists("C:/Users/Sophie/Dropbox/WASH/")){ 
  dropboxDir <- "C:/Users/Sophie/Dropbox/WASH/"
}




theme_set(theme_bw())
