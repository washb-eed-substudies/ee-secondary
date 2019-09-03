rm(list=ls())
source(here::here("0-config.R"))
source(here('table scripts/MIS09-Table5.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], "(", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws7<-c()

tbls7<-cbind(tbl5, ipcws7)
names(tbls7)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls7, file=here('tables/miso9-supptable7.csv'))
