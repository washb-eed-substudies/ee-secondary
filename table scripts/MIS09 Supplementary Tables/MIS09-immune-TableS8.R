rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
source(here('table scripts/MIS09-immune-Table6.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws8<-c(" ", " ", maketblvalue(d23_il1_adj_ipcw_L$`unlist(d23_il1_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il6_adj_ipcw_L$`unlist(d23_il6_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_tnf_adj_ipcw_L$`unlist(d23_tnf_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il12_adj_ipcw_L$`unlist(d23_il12_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_ifn_adj_ipcw_L$`unlist(d23_ifn_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il4_adj_ipcw_L$`unlist(d23_il4_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il5_adj_ipcw_L$`unlist(d23_il5_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il13_adj_ipcw_L$`unlist(d23_il13_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il17_adj_ipcw_L$`unlist(d23_il17_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il21_adj_ipcw_L$`unlist(d23_il21_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il10_adj_ipcw_L$`unlist(d23_il10_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_il2_adj_ipcw_L$`unlist(d23_il2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_gmc_adj_ipcw_L$`unlist(d23_gmc_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(d23_igf_adj_ipcw_L$`unlist(d23_igf_adj_ipcw$estimates$ATE)`))

tbls8<-cbind(tbl6, ipcws8)
names(tbls8)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls8, file=here('tables/miso9-immune-supptable8.csv'))
print(xtable(tbls8), type="html", file=here("tables/miso9-immune-supptable8.html"))

 