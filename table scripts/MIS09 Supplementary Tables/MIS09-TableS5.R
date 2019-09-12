rm(list=ls())
source(here::here("0-config.R"))
source(here('table scripts/MIS09-Table3.R'))

load(here("audrie results/immune_ipcw.RData"))

maketblvalue<-function(ipcwadjvar){
  rounded<-round(ipcwadjvar, 2)
  paste(rounded[1], "(", rounded[3], ", ", rounded[4], ")", sep="")
}

ipcws5<-c(" ", " ", maketblvalue(ratio_il1_il10_t2_adj_ipcw_L$`unlist(ratio_il1_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il6_il10_t2_adj_ipcw_L$`unlist(ratio_il6_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_tnf_il10_t2_adj_ipcw_L$`unlist(ratio_tnf_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il10_t2_adj_ipcw_L$`unlist(ratio_il12_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il10_t2_adj_ipcw_L$`unlist(ratio_ifn_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il4_il10_t2_adj_ipcw_L$`unlist(ratio_il4_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il5_il10_t2_adj_ipcw_L$`unlist(ratio_il5_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il13_il10_t2_adj_ipcw_L$`unlist(ratio_il13_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il17_il10_t2_adj_ipcw_L$`unlist(ratio_il17_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il21_il10_t2_adj_ipcw_L$`unlist(ratio_il21_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il2_il10_t2_adj_ipcw_L$`unlist(ratio_il2_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_gmc_il10_t2_adj_ipcw_L$`unlist(ratio_gmc_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il4_t2_adj_ipcw_L$`unlist(ratio_il12_il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il4_t2_adj_ipcw_L$`unlist(ratio_ifn_il4_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il5_t2_adj_ipcw_L$`unlist(ratio_il12_il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il5_t2_adj_ipcw_L$`unlist(ratio_ifn_il5_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il13_t2_adj_ipcw_L$`unlist(ratio_il12_il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il13_t2_adj_ipcw_L$`unlist(ratio_ifn_il13_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il17_t2_adj_ipcw_L$`unlist(ratio_il12_il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il17_t2_adj_ipcw_L$`unlist(ratio_ifn_il17_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_il12_il21_t2_adj_ipcw_L$`unlist(ratio_il12_il21_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_ifn_il21_t2_adj_ipcw_L$`unlist(ratio_ifn_il21_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_pro_il10_t2_adj_ipcw_L$`unlist(ratio_pro_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_il10_t2_adj_ipcw_L$`unlist(ratio_th1_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th2_il10_t2_adj_ipcw_L$`unlist(ratio_th2_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th17_il10_t2_adj_ipcw_L$`unlist(ratio_th17_il10_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th2_t2_adj_ipcw_L$`unlist(ratio_th1_th2_t2_adj_ipcw$estimates$ATE)`),
          " ", " ", maketblvalue(ratio_th1_th17_t2_adj_ipcw_L$`unlist(ratio_th1_th17_t2_adj_ipcw$estimates$ATE)`))

tbls5<-cbind(tbl3, ipcws5)
names(tbls5)[8]<-"IPCW adjusted difference: Intervention vs. Control (95% CI)"

write.csv(tbls5, file=here('tables/miso9-supptable5.csv'))
