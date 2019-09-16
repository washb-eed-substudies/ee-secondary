rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
source(here('table scripts/MIS09-immune-Table2.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws4<-c(" ", " ", maketblvalue(il12_t2_adj_ipcw_L$`unlist(il12_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il6_t2_adj_ipcw_L$`unlist(il6_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(tnf_t2_adj_ipcw_L$`unlist(tnf_t2_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(crp_t2_adj_ipcw_L$`unlist(crp_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il12_t2_adj_ipcw_L$`unlist(il12_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ifn_t2_adj_ipcw_L$`unlist(ifn_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il4_t2_adj_ipcw_L$`unlist(il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il5_t2_adj_ipcw_L$`unlist(il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il13_t2_adj_ipcw_L$`unlist(il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il17_t2_adj_ipcw_L$`unlist(il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il21_t2_adj_ipcw_L$`unlist(il21_t2_adj_ipcw$estimates$ATE)`), 
          " ", " ", maketblvalue(il10_t2_adj_ipcw_L$`unlist(il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(il2_t2_adj_ipcw_L$`unlist(il2_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(gmc_t2_adj_ipcw_L$`unlist(gmc_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(agp_t2_adj_ipcw_L$`unlist(agp_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(igf_t2_adj_ipcw_L$`unlist(igf_t2_adj_ipcw$estimates$ATE)`))

tbls4<-cbind(tbl2, ipcws4)
names(tbls4)[9]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls4, file=here('tables/miso9-immune-supptable4.csv'))
print(xtable(tbls4), type="html", file=here("tables/miso9-immune-supptable4.html"))

