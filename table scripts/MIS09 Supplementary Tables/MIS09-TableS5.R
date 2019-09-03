rm(list=ls())
source(here::here("0-config.R"))
source(here('table scripts/MIS09-Table3.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], "(", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws5<-c()

tbls5<-cbind(tbl3, ipcws5)
names(tbls5)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls5, file=here('tables/miso9-supptable5.csv'))
