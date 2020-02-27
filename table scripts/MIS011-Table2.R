rm(list=ls())
source(here::here("0-config.R"))
load(here("audrie results/telo_growth_results.Rdata"))
load(here("audrie results/telo_growth_spline_fits.Rdata"))

# round all summary values from data tables for input
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
h4adj.res <- round_df(h4adj.res, 2)
h4unadj.res <- round_df(h4unadj.res, 2)
h6adj.res <- round_df(h6adj.res, 2)
h6unadj.res <- round_df(h6unadj.res, 2)
h8adj.res <- round_df(h8adj.res, 2)
h8unadj.res <- round_df(h8unadj.res, 2)
h7adj.res <- round_df(h7adj.res, 2)
h7unadj.res <- round_df(h7unadj.res, 2)

# filtered tables for all variables
# hypothesis 4
lazy1unadj <- filter(h4unadj.res, Y == "laz_t2")
lazy1adj <- filter(h4adj.res, Y == "laz_t2")
wazy1unadj <- filter(h4unadj.res, Y == "waz_t2")
wazy1adj <- filter(h4adj.res, Y == "waz_t2")
wlzy1unadj <- filter(h4unadj.res, Y == "whz_t2")
wlzy1adj <- filter(h4adj.res, Y == "whz_t2")
hczy1unadj <- filter(h4unadj.res, Y == "hcz_t2")
hczy1adj <- filter(h4adj.res, Y == "hcz_t2")
# hypothesis 6
lazy2unadj <- filter(h6unadj.res, Y == "laz_t3")
lazy2adj <- filter(h6adj.res, Y == "laz_t3")
wazy2unadj <- filter(h6unadj.res, Y == "waz_t3")
wazy2adj <- filter(h6adj.res, Y == "waz_t3")
wlzy2unadj <- filter(h6unadj.res, Y == "whz_t3")
wlzy2adj <- filter(h6adj.res, Y == "whz_t3")
hczy2unadj <- filter(h6unadj.res, Y == "hcz_t3")
hczy2adj <- filter(h6adj.res, Y == "hcz_t3")
# hypothesis 8
lazunadj <- filter(h8unadj.res, Y == "delta_laz_t2_t3")
lazadj <- filter(h8adj.res, Y == "delta_laz_t2_t3")
wazunadj <- filter(h8unadj.res, Y == "delta_waz_t2_t3")
wazadj <- filter(h8adj.res, Y == "delta_waz_t2_t3")
wlzunadj <- filter(h8unadj.res, Y == "delta_whz_t2_t3")
wlzadj <- filter(h8adj.res, Y == "delta_whz_t2_t3")
hczunadj <- filter(h8unadj.res, Y == "delta_hcz_t2_t3")
hczadj <- filter(h8adj.res, Y == "delta_hcz_t2_t3")
# hypothesis 7
lenvunadj <- filter(h7unadj.res, Y == "len_velocity_t2_t3")
lenvadj <- filter(h7adj.res, Y == "len_velocity_t2_t3")
weivunadj <- filter(h7unadj.res, Y == "wei_velocity_t2_t3")
weivadj <- filter(h7adj.res, Y == "wei_velocity_t2_t3")
hcvunadj <- filter(h7unadj.res, Y == "hc_velocity_t2_t3")
hcvadj <- filter(h7adj.res, Y == "hc_velocity_t2_t3")

# calculating N for all variables
Nlazy1 <- as.character(nrow(h4_laz_t2_vs_ts_t2gam.res))
Nwazy1 <- as.character(nrow(h4_waz_t2_vs_ts_t2gam.res))
Nwlzy1 <- as.character(nrow(h4_whz_t2_vs_ts_t2gam.res))
Nhczy1 <- as.character(nrow(h4_hcz_t2_vs_ts_t2gam.res))
Nlazy2 <- as.character(nrow(h6_laz_t3_vs_ts_t2gam.res))
Nwazy2 <- as.character(nrow(h6_waz_t3_vs_ts_t2gam.res))
Nwlzy2 <- as.character(nrow(h6_whz_t3_vs_ts_t2gam.res))
Nhczy2 <- as.character(nrow(h6_hcz_t3_vs_ts_t2gam.res))
Nlaz <- as.character(nrow(h8_delta_laz_v_ts_t2gam.res))
Nwaz <- as.character(nrow(h8_delta_waz_v_ts_t2gam.res))
Nwlz <- as.character(nrow(h8_delta_whz_v_ts_t2gam.res))
Nhcz <- as.character(nrow(h8_delta_hcz_v_ts_t2gam.res))
Nlenv <- as.character(nrow(h7_len_veloc_vs_ts_t2gam.res))
Nweiv <- as.character(nrow(h7_wei_veloc_vs_ts_t2gam.res))
Nhcv <- as.character(nrow(h7_hc_veloc_vs_ts_t2gam.res))
               
tbl2 <- data.table(
  "Outcome"=c(" ", " ", "LAZ, Year 1", "WAZ, Year 1", "WLZ, Year 1", "HCZ, Year 1",
                 " ", "LAZ, Year 2", "WAZ, Year 2", "WLZ, Year 2", "HCZ, Year 2", 
                 " ", "Change in LAZ between Year 1 and Year 2", "Change in WAZ between Year 1 and Year 2", "Change in WLZ between Year 1 and Year 2", "Change in HCZ between Year 1 and Year 2", 
                 " ", "Length velocity (cm/month) between Year 1 and Year 2", "Weight velocity (kg/month) between Year 1 and Year 2", "Head circumference velocity (cm/month) between Year 1 and Year 2"),
  "N"=c(" ", " ", Nlazy1, Nwazy1, Nwlzy1, Nhczy1, " ", Nlazy2, Nwazy2, Nwlzy2, Nhczy2, " ", Nlaz, Nwaz, Nwlz, Nhcz, " ", Nlenv, Nweiv, Nhcv), 
  "Q1 Mean"=c(" ", " ", as.character(lazy1unadj$meanY[1]), as.character(wazy1unadj$meanY[1]), as.character(wlzy1unadj$meanY[1]), as.character(hczy1unadj$meanY[1]), " ",
                 as.character(lazy2unadj$meanY[1]), as.character(wazy2unadj$meanY[1]), as.character(wlzy2unadj$meanY[1]), as.character(hczy2unadj$meanY[1]), " ",
                 as.character(lazunadj$meanY[1]), as.character(wazunadj$meanY[1]), as.character(wlzunadj$meanY[1]), as.character(hczunadj$meanY[1]), " ",
                 as.character(lenvunadj$meanY[1]), as.character(weivunadj$meanY[1]), as.character(hcvunadj$meanY[1])), 
  "Q2 Mean"=c(" ", " ", as.character(lazy1unadj$meanY[2]), as.character(wazy1unadj$meanY[2]), as.character(wlzy1unadj$meanY[2]), as.character(hczy1unadj$meanY[2]), " ",
                 as.character(lazy2unadj$meanY[2]), as.character(wazy2unadj$meanY[2]), as.character(wlzy2unadj$meanY[2]), as.character(hczy2unadj$meanY[2]), " ",
                 as.character(lazunadj$meanY[2]), as.character(wazunadj$meanY[2]), as.character(wlzunadj$meanY[2]), as.character(hczunadj$meanY[2]), " ",
                 as.character(lenvunadj$meanY[2]), as.character(weivunadj$meanY[2]), as.character(hcvunadj$meanY[2])), 
  "Q3 Mean"=c(" ", " ", as.character(lazy1unadj$meanY[3]), as.character(wazy1unadj$meanY[3]), as.character(wlzy1unadj$meanY[3]), as.character(hczy1unadj$meanY[3]), " ",
                 as.character(lazy2unadj$meanY[3]), as.character(wazy2unadj$meanY[3]), as.character(wlzy2unadj$meanY[3]), as.character(hczy2unadj$meanY[3]), " ",
                 as.character(lazunadj$meanY[3]), as.character(wazunadj$meanY[3]), as.character(wlzunadj$meanY[3]), as.character(hczunadj$meanY[3]), " ",
                 as.character(lenvunadj$meanY[3]), as.character(weivunadj$meanY[3]), as.character(hcvunadj$meanY[3])), 
  "Q4 Mean"=c(" ", " ", as.character(lazy1unadj$meanY[4]), as.character(wazy1unadj$meanY[4]), as.character(wlzy1unadj$meanY[4]), as.character(hczy1unadj$meanY[4]), " ",
                 as.character(lazy2unadj$meanY[4]), as.character(wazy2unadj$meanY[4]), as.character(wlzy2unadj$meanY[4]), as.character(hczy2unadj$meanY[4]), " ",
                 as.character(lazunadj$meanY[4]), as.character(wazunadj$meanY[4]), as.character(wlzunadj$meanY[4]), as.character(hczunadj$meanY[4]), " ",
                 as.character(lenvunadj$meanY[4]), as.character(weivunadj$meanY[4]), as.character(hcvunadj$meanY[4])), 
  "T/S Ratio Year 1, Q1 vs. Q2"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy1unadj$ATE[2], " (", lazy1unadj$CI1[2], ", ", lazy1unadj$CI2[2], ")", sep=""),
                                  paste(wazy1unadj$ATE[2], " (", wazy1unadj$CI1[2], ", ", wazy1unadj$CI2[2], ")", sep=""),
                                  paste(wlzy1unadj$ATE[2], " (", wlzy1unadj$CI1[2], ", ", wlzy1unadj$CI2[2], ")", sep=""),
                                  paste(hczy1unadj$ATE[2], " (", hczy1unadj$CI1[2], ", ", hczy1unadj$CI2[2], ")", sep=""),
                                  " ",
                                  paste(lazy2unadj$ATE[2], " (", lazy2unadj$CI1[2], ", ", lazy2unadj$CI2[2], ")", sep=""),
                                  paste(wazy2unadj$ATE[2], " (", wazy2unadj$CI1[2], ", ", wazy2unadj$CI2[2], ")", sep=""),
                                  paste(wlzy2unadj$ATE[2], " (", wlzy2unadj$CI1[2], ", ", wlzy2unadj$CI2[2], ")", sep=""),
                                  paste(hczy2unadj$ATE[2], " (", hczy2unadj$CI1[2], ", ", hczy2unadj$CI2[2], ")", sep=""),
                                  " ",
                                  paste(lazunadj$ATE[2], " (", lazunadj$CI1[2], ", ", lazunadj$CI2[2], ")", sep=""),
                                  paste(wazunadj$ATE[2], " (", wazunadj$CI1[2], ", ", wazunadj$CI2[2], ")", sep=""),
                                  paste(wlzunadj$ATE[2], " (", wlzunadj$CI1[2], ", ", wlzunadj$CI2[2], ")", sep=""),
                                  paste(hczunadj$ATE[2], " (", hczunadj$CI1[2], ", ", hczunadj$CI2[2], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[2], " (", lenvunadj$CI1[2], ", ", lenvunadj$CI2[2], ")", sep=""),
                                  paste(weivunadj$ATE[2], " (", weivunadj$CI1[2], ", ", weivunadj$CI2[2], ")", sep=""),
                                  paste(hcvunadj$ATE[2], " (", hcvunadj$CI1[2], ", ", hcvunadj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy1unadj$Pval[2]), as.character(wazy1unadj$Pval[2]), as.character(wlzy1unadj$Pval[2]), as.character(hczy1unadj$Pval[2]), " ",
        as.character(lazy2unadj$Pval[2]), as.character(wazy2unadj$Pval[2]), as.character(wlzy2unadj$Pval[2]), as.character(hczy2unadj$Pval[2]), " ",
        as.character(lazunadj$Pval[2]), as.character(wazunadj$Pval[2]), as.character(wlzunadj$Pval[2]), as.character(hczunadj$Pval[2]), " ",
        as.character(lenvunadj$Pval[2]), as.character(weivunadj$Pval[2]), as.character(hcvunadj$Pval[2])), 
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy1adj$ATE[2], " (", lazy1adj$CI1[2], ", ", lazy1adj$CI2[2], ")", sep=""),
        paste(wazy1adj$ATE[2], " (", wazy1adj$CI1[2], ", ", wazy1adj$CI2[2], ")", sep=""),
        paste(wlzy1adj$ATE[2], " (", wlzy1adj$CI1[2], ", ", wlzy1adj$CI2[2], ")", sep=""),
        paste(hczy1adj$ATE[2], " (", hczy1adj$CI1[2], ", ", hczy1adj$CI2[2], ")", sep=""),
        " ",
        paste(lazy2adj$ATE[2], " (", lazy2adj$CI1[2], ", ", lazy2adj$CI2[2], ")", sep=""),
        paste(wazy2adj$ATE[2], " (", wazy2adj$CI1[2], ", ", wazy2adj$CI2[2], ")", sep=""),
        paste(wlzy2adj$ATE[2], " (", wlzy2adj$CI1[2], ", ", wlzy2adj$CI2[2], ")", sep=""),
        paste(hczy2adj$ATE[2], " (", hczy2adj$CI1[2], ", ", hczy2adj$CI2[2], ")", sep=""),
        " ",
        paste(lazadj$ATE[2], " (", lazadj$CI1[2], ", ", lazadj$CI2[2], ")", sep=""),
        paste(wazadj$ATE[2], " (", wazadj$CI1[2], ", ", wazadj$CI2[2], ")", sep=""),
        paste(wlzadj$ATE[2], " (", wlzadj$CI1[2], ", ", wlzadj$CI2[2], ")", sep=""),
        paste(hczadj$ATE[2], " (", hczadj$CI1[2], ", ", hczadj$CI2[2], ")", sep=""),
        " ",
        paste(lenvadj$ATE[2], " (", lenvadj$CI1[2], ", ", lenvadj$CI2[2], ")", sep=""),
        paste(weivadj$ATE[2], " (", weivadj$CI1[2], ", ", weivadj$CI2[2], ")", sep=""),
        paste(hcvadj$ATE[2], " (", hcvadj$CI1[2], ", ", hcvadj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy1adj$Pval[2]), as.character(wazy1adj$Pval[2]), as.character(wlzy1adj$Pval[2]), as.character(hczy1adj$Pval[2]), " ",
        as.character(lazy2adj$Pval[2]), as.character(wazy2adj$Pval[2]), as.character(wlzy1adj$Pval[2]), as.character(hczy2adj$Pval[2]), " ",
        as.character(lazadj$Pval[2]), as.character(wazadj$Pval[2]), as.character(wlzadj$Pval[2]), as.character(hczadj$Pval[2]), " ",
        as.character(lenvadj$Pval[2]), as.character(weivadj$Pval[2]), as.character(hcvadj$Pval[2])), 
  "T/S Ratio Year 1, Q1 vs. Q3"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy1unadj$ATE[3], " (", lazy1unadj$CI1[3], ", ", lazy1unadj$CI2[3], ")", sep=""),
                                  paste(wazy1unadj$ATE[3], " (", wazy1unadj$CI1[3], ", ", wazy1unadj$CI2[3], ")", sep=""),
                                  paste(wlzy1unadj$ATE[3], " (", wlzy1unadj$CI1[3], ", ", wlzy1unadj$CI2[3], ")", sep=""),
                                  paste(hczy1unadj$ATE[3], " (", hczy1unadj$CI1[3], ", ", hczy1unadj$CI2[3], ")", sep=""),
                                  " ",
                                  paste(lazy2unadj$ATE[3], " (", lazy2unadj$CI1[3], ", ", lazy2unadj$CI2[3], ")", sep=""),
                                  paste(wazy2unadj$ATE[3], " (", wazy2unadj$CI1[3], ", ", wazy2unadj$CI2[3], ")", sep=""),
                                  paste(wlzy2unadj$ATE[3], " (", wlzy2unadj$CI1[3], ", ", wlzy2unadj$CI2[3], ")", sep=""),
                                  paste(hczy2unadj$ATE[3], " (", hczy2unadj$CI1[3], ", ", hczy2unadj$CI2[3], ")", sep=""),
                                  " ",
                                  paste(lazunadj$ATE[3], " (", lazunadj$CI1[3], ", ", lazunadj$CI2[3], ")", sep=""),
                                  paste(wazunadj$ATE[3], " (", wazunadj$CI1[3], ", ", wazunadj$CI2[3], ")", sep=""),
                                  paste(wlzunadj$ATE[3], " (", wlzunadj$CI1[3], ", ", wlzunadj$CI2[3], ")", sep=""),
                                  paste(hczunadj$ATE[3], " (", hczunadj$CI1[3], ", ", hczunadj$CI2[3], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[3], " (", lenvunadj$CI1[3], ", ", lenvunadj$CI2[3], ")", sep=""),
                                  paste(weivunadj$ATE[3], " (", weivunadj$CI1[3], ", ", weivunadj$CI2[3], ")", sep=""),
                                  paste(hcvunadj$ATE[3], " (", hcvunadj$CI1[3], ", ", hcvunadj$CI2[3], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy1unadj$Pval[3]), as.character(wazy1unadj$Pval[3]), as.character(wlzy1unadj$Pval[3]), as.character(hczy1unadj$Pval[3]), " ",
        as.character(lazy2unadj$Pval[3]), as.character(wazy2unadj$Pval[3]), as.character(wlzy2unadj$Pval[3]), as.character(hczy2unadj$Pval[3]), " ",
        as.character(lazunadj$Pval[3]), as.character(wazunadj$Pval[3]), as.character(wlzunadj$Pval[3]), as.character(hczunadj$Pval[3]), " ",
        as.character(lenvunadj$Pval[3]), as.character(weivunadj$Pval[3]), as.character(hcvunadj$Pval[3])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy1adj$ATE[3], " (", lazy1adj$CI1[3], ", ", lazy1adj$CI2[3], ")", sep=""),
        paste(wazy1adj$ATE[3], " (", wazy1adj$CI1[3], ", ", wazy1adj$CI2[3], ")", sep=""),
        paste(wlzy1adj$ATE[3], " (", wlzy1adj$CI1[3], ", ", wlzy1adj$CI2[3], ")", sep=""),
        paste(hczy1adj$ATE[3], " (", hczy1adj$CI1[3], ", ", hczy1adj$CI2[3], ")", sep=""),
        " ",
        paste(lazy2adj$ATE[3], " (", lazy2adj$CI1[3], ", ", lazy2adj$CI2[3], ")", sep=""),
        paste(wazy2adj$ATE[3], " (", wazy2adj$CI1[3], ", ", wazy2adj$CI2[3], ")", sep=""),
        paste(wlzy2adj$ATE[3], " (", wlzy2adj$CI1[3], ", ", wlzy2adj$CI2[3], ")", sep=""), 
        paste(hczy2adj$ATE[3], " (", hczy2adj$CI1[3], ", ", hczy2adj$CI2[3], ")", sep=""),
        " ",
        paste(lazadj$ATE[3], " (", lazadj$CI1[3], ", ", lazadj$CI2[3], ")", sep=""),
        paste(wazadj$ATE[3], " (", wazadj$CI1[3], ", ", wazadj$CI2[3], ")", sep=""),
        paste(wlzadj$ATE[3], " (", wlzadj$CI1[3], ", ", wlzadj$CI2[3], ")", sep=""),
        paste(hczadj$ATE[3], " (", hczadj$CI1[3], ", ", hczadj$CI2[3], ")", sep=""),
        " ",
        paste(lenvadj$ATE[3], " (", lenvadj$CI1[3], ", ", lenvadj$CI2[3], ")", sep=""),
        paste(weivadj$ATE[3], " (", weivadj$CI1[3], ", ", weivadj$CI2[3], ")", sep=""),
        paste(hcvadj$ATE[3], " (", hcvadj$CI1[3], ", ", hcvadj$CI2[3], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy1adj$Pval[3]), as.character(wazy1adj$Pval[3]), as.character(wlzy1adj$Pval[3]), as.character(hczy1adj$Pval[3]), " ",
        as.character(lazy2adj$Pval[3]), as.character(wazy2adj$Pval[3]), as.character(wlzy2adj$Pval[3]), as.character(hczy2adj$Pval[3]), " ",
        as.character(lazadj$Pval[3]), as.character(wazadj$Pval[3]), as.character(wlzadj$Pval[3]), as.character(hczadj$Pval[3]), " ",
        as.character(lenvadj$Pval[3]), as.character(weivadj$Pval[3]), as.character(hcvadj$Pval[3])),
  "T/S Ratio Year 1, Q1 vs. Q4"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy1unadj$ATE[4], " (", lazy1unadj$CI1[4], ", ", lazy1unadj$CI2[4], ")", sep=""),
                                  paste(wazy1unadj$ATE[4], " (", wazy1unadj$CI1[4], ", ", wazy1unadj$CI2[4], ")", sep=""),
                                  paste(wlzy1unadj$ATE[4], " (", wlzy1unadj$CI1[4], ", ", wlzy1unadj$CI2[4], ")", sep=""),
                                  paste(hczy1unadj$ATE[4], " (", hczy1unadj$CI1[4], ", ", hczy1unadj$CI2[4], ")", sep=""),
                                  " ",
                                  paste(lazy2unadj$ATE[4], " (", lazy2unadj$CI1[4], ", ", lazy2unadj$CI2[4], ")", sep=""),
                                  paste(wazy2unadj$ATE[4], " (", wazy2unadj$CI1[4], ", ", wazy2unadj$CI2[4], ")", sep=""),
                                  paste(wlzy2unadj$ATE[4], " (", wlzy2unadj$CI1[4], ", ", wlzy2unadj$CI2[4], ")", sep=""),
                                  paste(hczy2unadj$ATE[4], " (", hczy2unadj$CI1[4], ", ", hczy2unadj$CI2[4], ")", sep=""),
                                  " ",
                                  paste(lazunadj$ATE[4], " (", lazunadj$CI1[4], ", ", lazunadj$CI2[4], ")", sep=""),
                                  paste(wazunadj$ATE[4], " (", wazunadj$CI1[4], ", ", wazunadj$CI2[4], ")", sep=""),
                                  paste(wlzunadj$ATE[4], " (", wlzunadj$CI1[4], ", ", wlzunadj$CI2[4], ")", sep=""),
                                  paste(hczunadj$ATE[4], " (", hczunadj$CI1[4], ", ", hczunadj$CI2[4], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[4], " (", lenvunadj$CI1[4], ", ", lenvunadj$CI2[4], ")", sep=""),
                                  paste(weivunadj$ATE[4], " (", weivunadj$CI1[4], ", ", weivunadj$CI2[4], ")", sep=""),
                                  paste(hcvunadj$ATE[4], " (", hcvunadj$CI1[4], ", ", hcvunadj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy1unadj$Pval[4]), as.character(wazy1unadj$Pval[4]), as.character(wlzy1unadj$Pval[4]), as.character(hczy1unadj$Pval[4]), " ",
        as.character(lazy2unadj$Pval[4]), as.character(wazy2unadj$Pval[4]), as.character(wlzy2unadj$Pval[4]), as.character(hczy2unadj$Pval[4]), " ",
        as.character(lazunadj$Pval[4]), as.character(wazunadj$Pval[4]), as.character(wlzunadj$Pval[4]), as.character(hczunadj$Pval[4]), " ",
        as.character(lenvunadj$Pval[4]), as.character(weivunadj$Pval[4]), as.character(hcvunadj$Pval[4])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy1adj$ATE[4], " (", lazy1adj$CI1[4], ", ", lazy1adj$CI2[4], ")", sep=""),
        paste(wazy1adj$ATE[4], " (", wazy1adj$CI1[4], ", ", wazy1adj$CI2[4], ")", sep=""),
        paste(wlzy1adj$ATE[4], " (", wlzy1adj$CI1[4], ", ", wlzy1adj$CI2[4], ")", sep=""),
        paste(hczy1adj$ATE[4], " (", hczy1adj$CI1[4], ", ", hczy1adj$CI2[4], ")", sep=""),
        " ",
        paste(lazy2adj$ATE[4], " (", lazy2adj$CI1[4], ", ", lazy2adj$CI2[4], ")", sep=""),
        paste(wazy2adj$ATE[4], " (", wazy2adj$CI1[4], ", ", wazy2adj$CI2[4], ")", sep=""),
        paste(wlzy2adj$ATE[4], " (", wlzy2adj$CI1[4], ", ", wlzy2adj$CI2[4], ")", sep=""),
        paste(hczy2adj$ATE[4], " (", hczy2adj$CI1[4], ", ", hczy2adj$CI2[4], ")", sep=""),
        " ",
        paste(lazadj$ATE[4], " (", lazadj$CI1[4], ", ", lazadj$CI2[4], ")", sep=""),
        paste(wazadj$ATE[4], " (", wazadj$CI1[4], ", ", wazunadj$CI2[4], ")", sep=""),
        paste(wlzadj$ATE[4], " (", wlzadj$CI1[4], ", ", wlzadj$CI2[4], ")", sep=""),
        paste(hczadj$ATE[4], " (", hczadj$CI1[4], ", ", hczadj$CI2[4], ")", sep=""),
        " ",
        paste(lenvadj$ATE[4], " (", lenvadj$CI1[4], ", ", lenvadj$CI2[4], ")", sep=""),
        paste(weivadj$ATE[4], " (", weivadj$CI1[4], ", ", weivadj$CI2[4], ")", sep=""),
        paste(hcvadj$ATE[4], " (", hcvadj$CI1[4], ", ", hcvadj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy1adj$Pval[4]), as.character(wazy1adj$Pval[4]), as.character(wlzy1adj$Pval[4]), as.character(hczy1adj$Pval[4]), " ",
        as.character(lazy2adj$Pval[4]), as.character(wazy2adj$Pval[4]), as.character(wlzy2adj$Pval[4]), as.character(hczy2adj$Pval[4]), " ",
        as.character(lazadj$Pval[4]), as.character(wazadj$Pval[4]), as.character(wlzadj$Pval[4]), as.character(hczadj$Pval[4]), " ",
        as.character(lenvadj$Pval[4]), as.character(weivadj$Pval[4]), as.character(hcvadj$Pval[4]))
)


write.csv(tbl2, file=here("tables/mis011-table2.csv"))

