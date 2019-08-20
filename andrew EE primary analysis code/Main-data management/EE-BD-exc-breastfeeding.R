
# 
# Hi Andrew,
# 
# I've uploaded the baseline infant FFQ dataset to Dropbox:
# Dropbox/WBB-EE-analysis/Data/Untouched/Baseline/EE_Baseline_FFQ_Raw_data_13Sep2017.dta
# 
# To determine exclusive breastfeeding using the 24 hour definition,  
# 1. c607>0 & c607a==5
# 2. c608 series of questions all need to be "no"
# 3. c609 series of questions all need to be "no"
# 
# I've attached my do file for calculating exclusive breastfeeding using the infant FFQ data.

rm(list=ls())
library(foreign)
library(tidyverse)

#load in urine outcome dataset
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/child_df.Rdata")


#load BF dataset
d <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Baseline/EE_Baseline_FFQ_Raw_data.csv")

d <- d %>% rename(childNo =childId)

#merge
d <- left_join(child, d, by=c("dataid","childNo"))


head(d)
colnames(d)

table(is.na(d$c605))
table(is.na(d$c607))

#drop those with no ffq breastfeeding data collected
d <- d %>%  filter(!is.na(c605) | !is.na(c607))


#3 step process to define exclusive breastfeeding

# 1. c607>0 & c607a==5

table(d$c607)
table(d$c607a)
table(is.na(d$c607))
table(is.na(d$c607a))

exc_step1 <- d$c607>0 & d$c607a==5
table(exc_step1)

# 2. c608 series of questions all need to be "no"

c8df <- d %>% select(starts_with("c608")) %>% select(ends_with("_3"))
dim(c8df)

for(i in 1:ncol(c8df)){
  c8df[,i] <- as.numeric(c8df[,i] == 2) #2 is no
}

exc_step2 <- c8df %>% mutate(exc_step2 = rowSums(.)==ncol(.)) %>% select(exc_step2) %>% as.vector()
table(exc_step2)



# 3. c609 series of questions all need to be "no"

c9df <- d %>% select(starts_with("c609")) %>% select(., -ends_with("other"))

for(i in 1:ncol(c9df)){
  print(class(c9df[,i]))
}

# drop c609_19_1
c9df <- c9df %>%  subset(., select= -c(c609_19_1))

for(i in 1:ncol(c9df)){
  c9df[,i] <- as.numeric(c9df[,i] == 2)
}


exc_step3 <- c9df %>% mutate(exc_step3 = rowSums((.))==ncol(.)) %>% select(exc_step3) %>% as.vector()
table(exc_step3)


exc_feeding = exc_step1 & exc_step2 & exc_step3

table(exc_feeding)
#matches Audrie

#make exclusive breastfeeding dataset
bf_df <- data.frame(dataid=d$dataid, childNo=d$childNo, exc_feeding=exc_feeding[,1], tr=d$tr)

# #load in urine outcome dataset
# load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/child_df.Rdata")
# 
# #merge
# d <- left_join(child, bf_df, by=c("dataid","childNo")) %>% filter(!is.na(exc_feeding))
# d <- droplevels(d)

d <- bf_df
d <- droplevels(d)

table(d$tr, d$exc_feeding)


tab<- table(d$tr, d$exc_feeding)
tab[,1]+tab[,2]

tab[,2]/(tab[,1]+tab[,2])*100

#128 WSH


df<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Audrie/Aud_FFQ.csv")

d$childid <-as.numeric(paste0(d$dataid, d$childNo))

test <- anti_join(d,df,  by="childid")

test$exc_feeding

test2 <- anti_join(df, d,  by="childid")
test2[,1:40]

table(d$tr, d$exc_feeding)
table(df$tr, df$EBF)


table(d$exc_feeding, df$EBF)
table(is.na(d$exc_feeding), is.na(df$EBF))

id <- d$childid[is.na(d$exc_feeding) & !is.na(df$EBF)]
id

d$exc_feeding[d$childid==id]
df$EBF[df$childid==id]

d %>% group_by(tr) %>% summarize(mean(exc_feeding, na.rm=T))
df %>% group_by(tr) %>% summarize(mean(EBF, na.rm=T))




#load BF dataset
d <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Baseline/EE_Baseline_FFQ_Raw_data.csv")

d <- d %>% rename(childNo =childId)

#merge
d <- left_join(child, d, by=c("dataid","childNo"))


#Subset to unreplicated id
d <- d %>% filter(childid==id)

head(d)
colnames(d)

table(is.na(d$c605))
table(is.na(d$c607))

#drop those with no ffq breastfeeding data collected
d <- d %>%  filter(!is.na(c605) | !is.na(c607))


#3 step process to define exclusive breastfeeding

# 1. c607>0 & c607a==5

table(d$c607)
table(d$c607a)
table(is.na(d$c607))
table(is.na(d$c607a))

exc_step1 <- d$c607>0 & d$c607a==5
table(exc_step1)

# 2. c608 series of questions all need to be "no"

c8df <- d %>% select(starts_with("c608")) %>% select(ends_with("_3"))
dim(c8df)

for(i in 1:ncol(c8df)){
  c8df[,i] <- as.numeric(c8df[,i] == 2) #2 is no
}

exc_step2 <- c8df %>% mutate(exc_step2 = rowSums(.)==ncol(.)) %>% select(exc_step2) %>% as.vector()
table(exc_step2)



# 3. c609 series of questions all need to be "no"

c9df <- d %>% select(starts_with("c609")) %>% select(., -ends_with("other"))

for(i in 1:ncol(c9df)){
  print(class(c9df[,i]))
}

# drop c609_19_1
c9df <- c9df %>%  subset(., select= -c(c609_19_1))

for(i in 1:ncol(c9df)){
  c9df[,i] <- as.numeric(c9df[,i] == 2)
}


exc_step3 <- c9df %>% mutate(exc_step3 = rowSums((.))==ncol(.)) %>% select(exc_step3) %>% as.vector()
table(exc_step3)


exc_feeding = exc_step1 & exc_step2 & exc_step3

table(exc_feeding)

