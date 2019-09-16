rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
source(here('table scripts/MIS09-immune-Table4.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws6<-c(" ", " ", maketblvalue(il1_t3_adj_ipcw_L$`unlist(il1_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il6_t3_adj_ipcw_L$`unlist(il6_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(tnf_t3_adj_ipcw_L$`unlist(tnf_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il12_t3_adj_ipcw_L$`unlist(il12_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ifn_t3_adj_ipcw_L$`unlist(ifn_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il4_t3_adj_ipcw_L$`unlist(il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il5_t3_adj_ipcw_L$`unlist(il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il13_t3_adj_ipcw_L$`unlist(il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il17_t3_adj_ipcw_L$`unlist(il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il21_t3_adj_ipcw_L$`unlist(il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il10_t3_adj_ipcw_L$`unlist(il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il2_t3_adj_ipcw_L$`unlist(il2_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(gmc_t3_adj_ipcw_L$`unlist(gmc_t3_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(igf_t3_adj_ipcw_L$`unlist(igf_t3_adj_ipcw$estimates$ATE)`))

tbls6<-cbind(tbl4, ipcws6)
names(tbls6)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls6, file=here('tables/miso9-immune-supptable6.csv'))
print(xtable(tbls6), type="html", file=here("tables/miso9-immune-supptable6.html"))
