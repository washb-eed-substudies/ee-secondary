
rm(list=ls())
source(here::here("0-config.R"))

load(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
da <- d

d<-read.csv(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-anthro.csv"))

sex <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-ipcw.csv")
sex$childid <- as.numeric(paste0(sex$dataid, sex$childNo))
sex <- sex %>% subset(., select = c(childid, sex))
d$childid<-as.numeric(paste0(d$dataid, d$childNo))

dim(d)
d <- left_join(d, sex, by="childid")
dim(d)

head(d)

head(da)


table(d$laz_x1)
table(da$laz_x_t1)


summary(d$laz1)
summary(da$laz_t1)


dm <- d


dim(dm)
d  <- merge(dm, da, by=c("childid"), all.x=T, all.y=T)
dim(d)



sum(d$laz1 - d$laz_t1, na.rm=T)
sum(d$laz2 - d$laz_t2, na.rm=T)

summary(d$laz1)
summary(d$laz_t1)


sum((d$laz1 - d$laz_t1)!=0, na.rm=T)
sum((d$laz2 - d$laz_t2)!=0, na.rm=T)
sum((d$laz3 - d$laz_t3)!=0, na.rm=T)

sum((d$whz1 - d$whz_t1)!=0, na.rm=T)
sum((d$whz2 - d$whz_t2)!=0, na.rm=T)
sum((d$whz3 - d$whz_t3)!=0, na.rm=T)

sum((d$bmiz1 - d$bmi_t1)!=0, na.rm=T)
sum((d$bmiz2 - d$bmi_t2)!=0, na.rm=T)
sum((d$bmiz3 - d$bmi_t3)!=0, na.rm=T)

sum((d$hcz1 - d$hcz_t1)!=0, na.rm=T)
sum((d$hcz2 - d$hcz_t2)!=0, na.rm=T)
sum((d$hcz3 - d$hcz_t3)!=0, na.rm=T)

table(is.na(d$laz1))
table(is.na(d$laz_t1))

table(is.na(d$laz1) & !is.na(d$laz_t1))
table(!is.na(d$laz1) & is.na(d$laz_t1))




#Childid's that have unmatched Z-scores at each time
d$childid[(d$laz1 - d$laz_t1)!=0 & !is.na(d$laz1) & !is.na(d$laz_t1)]
d$childid[(d$laz2 - d$laz_t2)!=0 & !is.na(d$laz2) & !is.na(d$laz_t2)]
d$childid[(d$laz3 - d$laz_t3)!=0 & !is.na(d$laz3) & !is.na(d$laz_t3)]

ids <- d$childid[(d$laz3 - d$laz_t3)!=0 & !is.na(d$laz3) & !is.na(d$laz_t3)]

dfull<-d
d<-dfull[d$childid %in% ids,]
d

d$laz1
d$laz_t1

d$laz2
d$laz_t2

d$laz3
d$laz_t3

#Compare raw anthro
rowMedian <- function(x, na.rm = FALSE) apply(x, 1, median, na.rm = na.rm) 


d$length1
d$lenhei_med_t1
d$length2
d$lenhei_med_t2
d$length3
d$lenhei_med_t3

#compare ages
d$aged1
d$ageday_ht1

d$aged2
d$ageday_ht2

d$aged3
d$ageday_ht3

d$sex.x
d$sex.y



#check with growthstandards package
library(growthstandards)

#Andrew's input
d$sex_a <- ifelse(d$sex.x==1, "Male", "Female")
who_htcm2zscore(d$aged1, d$length1, sex = d$sex_a)
who_htcm2zscore(d$aged2, d$length2, sex = d$sex_a)
who_htcm2zscore(d$aged3, d$length3, sex = d$sex_a)

#Andrew's input
d$sex_a <- ifelse(d$sex.y=="male", "Male", "Female")
who_htcm2zscore(d$ageday_ht1, d$lenhei_med_t1, sex = d$sex_a)
who_htcm2zscore(d$ageday_ht2, d$lenhei_med_t2, sex = d$sex_a)
who_htcm2zscore(d$ageday_ht3, d$lenhei_med_t3, sex = d$sex_a)

