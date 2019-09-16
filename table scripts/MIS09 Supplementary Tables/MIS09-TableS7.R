rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
source(here('table scripts/MIS09-immune-Table5.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], " (", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws7<-c(" ", " ", maketblvalue(ratio_il1_il10_t3_adj_ipcw_L$`unlist(ratio_il1_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il6_il10_t3_adj_ipcw_L$`unlist(ratio_il6_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_tnf_il10_t3_adj_ipcw_L$`unlist(ratio_tnf_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il10_t3_adj_ipcw_L$`unlist(ratio_il12_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il10_t3_adj_ipcw_L$`unlist(ratio_ifn_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il4_il10_t3_adj_ipcw_L$`unlist(ratio_il4_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il5_il10_t3_adj_ipcw_L$`unlist(ratio_il5_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il13_il10_t3_adj_ipcw_L$`unlist(ratio_il13_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il17_il10_t3_adj_ipcw_L$`unlist(ratio_il17_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il21_il10_t3_adj_ipcw_L$`unlist(ratio_il21_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il2_il10_t3_adj_ipcw_L$`unlist(ratio_il2_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_gmc_il10_t3_adj_ipcw_L$`unlist(ratio_gmc_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il4_t3_adj_ipcw_L$`unlist(ratio_il12_il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il4_t3_adj_ipcw_L$`unlist(ratio_ifn_il4_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il5_t3_adj_ipcw_L$`unlist(ratio_il12_il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il5_t3_adj_ipcw_L$`unlist(ratio_ifn_il5_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il13_t3_adj_ipcw_L$`unlist(ratio_il12_il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il13_t3_adj_ipcw_L$`unlist(ratio_ifn_il13_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il17_t3_adj_ipcw_L$`unlist(ratio_il12_il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il17_t3_adj_ipcw_L$`unlist(ratio_ifn_il17_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il21_t3_adj_ipcw_L$`unlist(ratio_il12_il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il21_t3_adj_ipcw_L$`unlist(ratio_ifn_il21_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_pro_il10_t3_adj_ipcw_L$`unlist(ratio_pro_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_il10_t3_adj_ipcw_L$`unlist(ratio_th1_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th2_il10_t3_adj_ipcw_L$`unlist(ratio_th2_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th17_il10_t3_adj_ipcw_L$`unlist(ratio_th17_il10_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th2_t3_adj_ipcw_L$`unlist(ratio_th1_th2_t3_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th17_t3_adj_ipcw_L$`unlist(ratio_th1_th17_t3_adj_ipcw$estimates$ATE)`))

tbls7<-cbind(tbl5, ipcws7)
names(tbls7)[8]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls7, file=here('tables/miso9-immune-supptable7.csv'))
print(xtable(tbls7), type="html", file=here("tables/miso9-immune-supptable7.html"))

