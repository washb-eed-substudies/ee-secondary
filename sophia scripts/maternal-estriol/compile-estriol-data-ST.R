## Initial maternal estriol data cleaning ##

rm(list=ls())
source(here::here("0-config.R"))

setwd('C:/Users/Sophia/Box/WASHB - Data Cleaning/Maternal Estriol/Untouched')

library(readxl)

# list of files names 
files <- list.files(getwd())[4:15]
files

# create vector of ranges of cells for each file
ranges = c("A32:J51", "A32:J51", "A35:J57", "A38:J63", "A109:J205", "A109:J205",
           "A109:J205", "A109:J205", "A109:J205", "A109:J205", "A109:J205", "A93:J173")

# empty data frame to hold cleaned data
d <- data.frame()

for (i in 1:length(files)){
  # extract date, assay, run number
  date <- str_extract(files[i], "[0-9]{6}")
  assay <- "blood_estriol"
  lab_tech <- gsub(".* [0-9]+_([A-Z_]+)_(WASHB).*|(Estriols).+", "\\1", files[i])
  if(i==4){lab_tech="ZIA_SUNNY"}
  print(lab_tech)
  
  if(grepl("Pilot", files[i])){run <- paste("P",str_extract(files[i], "[0-9]+"), sep="")}
  else{run <- str_extract(files[i], "[0-9]+")}
  
  # read in data
  temp <- read_excel(files[i], range=ranges[i])
  print(dim(temp))
  print(head(temp))
  temp <- temp %>% select('Name', 'Conc/Dil', 'Well', '450', '[Concentration]')
  
  # fill in values for cells that were merged
  if (any(is.na(temp$Name))) {
    name <- temp[1,]
    for(j in 1:nrow(temp)){
      if(is.na(temp[j, 1])){
        temp[j, 1] <- name
      }else{
        name <- temp[j, 1]
      }
    }
  }
  print(head(temp))
  
  # filter out control and standard curve samples
  temp <- temp %>% filter(!grepl("Control|Standard Curve", Name)) 
  temp <- temp %>% 
    mutate(`[Concentration]`=as.numeric(`[Concentration]`)) %>%
    group_by(Name) %>% 
    mutate(conc_avg = mean(`[Concentration]`))
  print(head(temp))
  
  # add lab tech, date, run
  if(grepl("_", lab_tech)){
    if(grepl("sunny|rabiul|zia", temp$Name, ignore.case=T)){
      lab_tech <- gsub(".*\\(([A-Za-z]+)\\).*", "\\1", temp$Name)
      temp$Name <- str_extract(temp$Name, "[0-9A-Z]+")
    }else{
      lab_tech <- gsub(".*\\(([A-Za-z]+)\\).*", "\\1", temp$Well)
      temp$Name <- str_extract(temp$Name, "[0-9A-Z]+")
    }
  }
  
  temp$date <- date
  temp$assay <- assay
  temp$run <- run
  temp$lab_tech <- lab_tech
  
  temp <- temp %>% rename(sample=`Name`, cond_dil=`Conc/Dil`, abs_450=`450`,
                          conc=`[Concentration]`) %>%
    select(date, assay, run, sample, cond_dil, abs_450, conc, conc_avg, lab_tech)
  
  print(head(temp))
  # bind to old data
  d <- rbind(d, temp)
}

d$lab_tech<-str_to_title(d$lab_tech)

write.csv(d, "C:/Users/Sophia/Documents/WASH/Maternal Estriol/compiled-estriol-ST.csv", row.names=F)
