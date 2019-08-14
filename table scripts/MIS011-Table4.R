rm(list=ls())
source(here::here("0-config.R"))
source(here("audrie R scripts/observational/quartileTMLE_telo_growth.R"))

# round all summary values from data tables for input
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
h3unadj.res <- round_df(h3unadj.res, 2)
h3adj.res <- round_df(h3adj.res, 2)
h1adj.res <- round_df(h1adj.res, 2)
h1unadj.res <- round_df(h1unadj.res, 2)
h2adj.res <- round_df(h2adj.res, 2)
h2unadj.res <- round_df(h2unadj.res, 2)

lazy2h3unadj <- filter(h3unadj.res, Y == "laz_t3")
lazy2h3adj <- filter(h3adj.res, Y == "laz_t3")
wazy2h3unadj <- filter(h3unadj.res, Y == "waz_t3")
wazy2h3adj <- filter(h3adj.res, Y == "waz_t3")
wlzy2h3unadj <- filter(h3unadj.res, Y == "whz_t3")
wlzy2h3adj <- filter(h3adj.res, Y == "whz_t3")
hczy2h3unadj <- filter(h3unadj.res, Y == "hcz_t3")
hczy2h3adj <- filter(h3adj.res, Y == "hcz_t3")
lazh1unadj <- filter(h1unadj.res, Y == "delta_laz_t2_t3")
lazh1adj <- filter(h1adj.res, Y == "delta_laz_t2_t3")
wazh1unadj <- filter(h1unadj.res, Y == "delta_waz_t2_t3")
wazh1adj <- filter(h1adj.res, Y == "delta_waz_t2_t3")
wlzh1unadj <- filter(h1unadj.res, Y == "delta_whz_t2_t3")
wlzh1adj <- filter(h1adj.res, Y == "delta_whz_t2_t3")
hczh1unadj <- filter(h1unadj.res, Y == "delta_hcz_t2_t3")
hczh1adj <- filter(h1adj.res, Y == "delta_hcz_t2_t3")
lenvunadj <- filter(h2unadj.res, Y == "len_velocity_t2_t3")
lenvadj <- filter(h2adj.res, Y == "len_velocity_t2_t3")
weivunadj <- filter(h2unadj.res, Y == "wei_velocity_t2_t3")
weivadj <- filter(h2adj.res, Y == "wei_velocity_t2_t3")
hcvunadj <- filter(h2unadj.res, Y == "hc_velocity_t2_t3")
hcvadj <- filter(h2adj.res, Y == "hc_velocity_t2_t3")

Nlazy2h3 <- as.character(nrow(h3_laz_t3_vs_delta_tsgam.res))
Nwazy2h3<- as.character(nrow(h3_waz_t3_vs_delta_tsgam.res))
Nwlzy2h3 <- as.character(nrow(h3_whz_t3_vs_delta_tsgam.res))
Nhczy2h3 <- as.character(nrow(h3_hcz_t3_vs_delta_tsgam.res))
Nlazh1 <- as.character(nrow(h1_delta_laz_v_delta_tsgam.res))
Nwazh1 <- as.character(nrow(h1_delta_waz_v_delta_tsgam.res))
Nwlzh1 <- as.character(nrow(h1_delta_whz_v_delta_tsgam.res))
Nhczh1 <- as.character(nrow(h1_delta_hcz_v_delta_tsgam.res))
Nlenv <- as.character(nrow(h2_len_velocity_v_delta_tsgam.res))
Nweiv <- as.character(nrow(h2_wei_velocity_v_delta_tsgam.res))
Nhcv <- as.character(nrow(h2_hc_velocity_v_delta_tsgam.res))

tbl4 <- data.table(
  "Outcome"=c(" ", " ", "LAZ, Year 2", "WAZ, Year 2", "WLZ, Year 2", "HCZ, Year 2",
              " ", "Change in LAZ between Year 1 to Year 2", "Change in WAZ between Year 1 to Year 2", "Change in WLZ between Year 1 to Year 2", "Change in HCZ between Year 1 to Year 2", 
              " ", "Length velocity (cm/month)", "Weight velocity (kg/month)", "Head circumference velocity (cm/month)"),
  "N"=c(" ", " ", Nlazy2h3, Nwazy2h3, Nwlzy2h3, Nhczy2h3, " ", Nlazh1, Nwazh1, Nwlzh1, Nhczh1, " ", Nlenv, Nweiv, Nhcv), 
  "Q1 Mean"=c(" ", " ", as.character(lazy2h3unadj$meanY[1]), as.character(wazy2h3unadj$meanY[1]), as.character(wlzy2h3unadj$meanY[1]), as.character(hczy2h3unadj$meanY[1]), " ",
              as.character(lazh1unadj$meanY[1]), as.character(wazh1unadj$meanY[1]), as.character(wlzh1unadj$meanY[1]), as.character(hczh1unadj$meanY[1]), " ",
              as.character(lenvunadj$meanY[1]), as.character(weivunadj$meanY[1]), as.character(hcvunadj$meanY[1])), 
  "Q2 Mean"=c(" ", " ", as.character(lazy2h3unadj$meanY[2]), as.character(wazy2h3unadj$meanY[2]), as.character(wlzy2h3unadj$meanY[2]), as.character(hczy2h3unadj$meanY[2]), " ",
              as.character(lazh1unadj$meanY[2]), as.character(wazh1unadj$meanY[2]), as.character(wlzh1unadj$meanY[2]), as.character(hczh1unadj$meanY[2]), " ",
              as.character(lenvunadj$meanY[2]), as.character(weivunadj$meanY[2]), as.character(hcvunadj$meanY[2])), 
  "Q3 Mean"=c(" ", " ", as.character(lazy2h3unadj$meanY[3]), as.character(wazy2h3unadj$meanY[3]), as.character(wlzy2h3unadj$meanY[3]), as.character(hczy2h3unadj$meanY[3]), " ",
              as.character(lazh1unadj$meanY[3]), as.character(wazh1unadj$meanY[3]), as.character(wlzh1unadj$meanY[3]), as.character(hczh1unadj$meanY[3]), " ",
              as.character(lenvunadj$meanY[3]), as.character(weivunadj$meanY[3]), as.character(hcvunadj$meanY[3])), 
  "Q4 Mean"=c(" ", " ", as.character(lazy2h3unadj$meanY[4]), as.character(wazy2h3unadj$meanY[4]), as.character(wlzy2h3unadj$meanY[4]), as.character(hczy2h3unadj$meanY[4]), " ",
              as.character(lazh1unadj$meanY[4]), as.character(wazh1unadj$meanY[4]), as.character(wlzh1unadj$meanY[4]), as.character(hczh1unadj$meanY[4]), " ",
              as.character(lenvunadj$meanY[4]), as.character(weivunadj$meanY[4]), as.character(hcvunadj$meanY[4])), 
  "Change in T/S Ratio between Year 1 to Year 2, Q1 vs. Q2"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h3unadj$ATE[2], " (", lazy2h3unadj$CI1[2], ", ", lazy2h3unadj$CI2[2], ")", sep=""),
                                  paste(wazy2h3unadj$ATE[2], " (", wazy2h3unadj$CI1[2], ", ", wazy2h3unadj$CI2[2], ")", sep=""),
                                  paste(wlzy2h3unadj$ATE[2], " (", wlzy2h3unadj$CI1[2], ", ", wlzy2h3unadj$CI2[2], ")", sep=""),
                                  paste(hczy2h3unadj$ATE[2], " (", hczy2h3unadj$CI1[2], ", ", hczy2h3unadj$CI2[2], ")", sep=""),
                                  " ",
                                  paste(lazh1unadj$ATE[2], " (", lazh1unadj$CI1[2], ", ", lazh1unadj$CI2[2], ")", sep=""),
                                  paste(wazh1unadj$ATE[2], " (", wazh1unadj$CI1[2], ", ", wazh1unadj$CI2[2], ")", sep=""),
                                  paste(wlzh1unadj$ATE[2], " (", wlzh1unadj$CI1[2], ", ", wlzh1unadj$CI2[2], ")", sep=""),
                                  paste(hczh1unadj$ATE[2], " (", hczh1unadj$CI1[2], ", ", hczh1unadj$CI2[2], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[2], " (", lenvunadj$CI1[2], ", ", lenvunadj$CI2[2], ")", sep=""),
                                  paste(weivunadj$ATE[2], " (", weivunadj$CI1[2], ", ", weivunadj$CI2[2], ")", sep=""),
                                  paste(hcvunadj$ATE[2], " (", hcvunadj$CI1[2], ", ", hcvunadj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h3unadj$Pval[2]), as.character(wazy2h3unadj$Pval[2]), as.character(wlzy2h3unadj$Pval[2]), as.character(hczy2h3unadj$Pval[2]), " ",
        as.character(lazh1unadj$Pval[2]), as.character(wazh1unadj$Pval[2]), as.character(wlzh1unadj$Pval[2]), as.character(hczh1unadj$Pval[2]), " ",
        as.character(lenvunadj$Pval[2]), as.character(weivunadj$Pval[2]), as.character(hcvunadj$Pval[2])), 
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h3adj$ATE[2], " (", lazy2h3adj$CI1[2], ", ", lazy2h3adj$CI2[2], ")", sep=""),
        paste(wazy2h3adj$ATE[2], " (", wazy2h3adj$CI1[2], ", ", wazy2h3adj$CI2[2], ")", sep=""),
        paste(wlzy2h3adj$ATE[2], " (", wlzy2h3adj$CI1[2], ", ", wlzy2h3adj$CI2[2], ")", sep=""),
        paste(hczy2h3adj$ATE[2], " (", hczy2h3adj$CI1[2], ", ", hczy2h3adj$CI2[2], ")", sep=""),
        " ",
        paste(lazh1adj$ATE[2], " (", lazh1adj$CI1[2], ", ", lazh1adj$CI2[2], ")", sep=""),
        paste(wazh1adj$ATE[2], " (", wazh1adj$CI1[2], ", ", wazh1adj$CI2[2], ")", sep=""),
        paste(wlzh1adj$ATE[2], " (", wlzh1adj$CI1[2], ", ", wlzh1adj$CI2[2], ")", sep=""),
        paste(hczh1adj$ATE[2], " (", hczh1adj$CI1[2], ", ", hczh1adj$CI2[2], ")", sep=""),
        " ",
        paste(lenvadj$ATE[2], " (", lenvadj$CI1[2], ", ", lenvadj$CI2[2], ")", sep=""),
        paste(weivadj$ATE[2], " (", weivadj$CI1[2], ", ", weivadj$CI2[2], ")", sep=""),
        paste(hcvadj$ATE[2], " (", hcvadj$CI1[2], ", ", hcvadj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h3adj$Pval[2]), as.character(wazy2h3adj$Pval[2]), as.character(wlzy2h3adj$Pval[2]), as.character(hczy2h3adj$Pval[2]), " ",
        as.character(lazh1adj$Pval[2]), as.character(wazh1adj$Pval[2]), as.character(wlzh1adj$Pval[2]), as.character(hczh1adj$Pval[2]), " ",
        as.character(lenvadj$Pval[2]), as.character(weivadj$Pval[2]), as.character(hcvadj$Pval[2])), 
  "Change in T/S Ratio between Year 1 to Year 2, Q1 vs. Q3"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h3unadj$ATE[3], " (", lazy2h3unadj$CI1[3], ", ", lazy2h3unadj$CI2[3], ")", sep=""),
                                  paste(wazy2h3unadj$ATE[3], " (", wazy2h3unadj$CI1[3], ", ", wazy2h3unadj$CI2[3], ")", sep=""),
                                  paste(wlzy2h3unadj$ATE[3], " (", wlzy2h3unadj$CI1[3], ", ", wlzy2h3unadj$CI2[3], ")", sep=""),
                                  paste(hczy2h3unadj$ATE[3], " (", hczy2h3unadj$CI1[3], ", ", hczy2h3unadj$CI2[3], ")", sep=""),
                                  " ",
                                  paste(lazh1unadj$ATE[3], " (", lazh1unadj$CI1[3], ", ", lazh1unadj$CI2[3], ")", sep=""),
                                  paste(wazh1unadj$ATE[3], " (", wazh1unadj$CI1[3], ", ", wazh1unadj$CI2[3], ")", sep=""),
                                  paste(wlzh1unadj$ATE[3], " (", wlzh1unadj$CI1[3], ", ", wlzh1unadj$CI2[3], ")", sep=""),
                                  paste(hczh1unadj$ATE[3], " (", hczh1unadj$CI1[3], ", ", hczh1unadj$CI2[3], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[3], " (", lenvunadj$CI1[3], ", ", lenvunadj$CI2[3], ")", sep=""),
                                  paste(weivunadj$ATE[3], " (", weivunadj$CI1[3], ", ", weivunadj$CI2[3], ")", sep=""),
                                  paste(hcvunadj$ATE[3], " (", hcvunadj$CI1[3], ", ", hcvunadj$CI2[3], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h3unadj$Pval[3]), as.character(wazy2h3unadj$Pval[3]), as.character(wlzy2h3unadj$Pval[3]), as.character(hczy2h3unadj$Pval[3]), " ",
        as.character(lazh1unadj$Pval[3]), as.character(wazh1unadj$Pval[3]), as.character(wlzh1unadj$Pval[3]), as.character(hczh1unadj$Pval[3]), " ",
        as.character(lenvunadj$Pval[3]), as.character(weivunadj$Pval[3]), as.character(hcvunadj$Pval[3])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h3adj$ATE[3], " (", lazy2h3adj$CI1[3], ", ", lazy2h3adj$CI2[3], ")", sep=""),
        paste(wazy2h3adj$ATE[3], " (", wazy2h3adj$CI1[3], ", ", wazy2h3adj$CI2[3], ")", sep=""),
        paste(wlzy2h3adj$ATE[3], " (", wlzy2h3adj$CI1[3], ", ", wlzy2h3adj$CI2[3], ")", sep=""), 
        paste(hczy2h3adj$ATE[3], " (", hczy2h3adj$CI1[3], ", ", hczy2h3adj$CI2[3], ")", sep=""),
        " ",
        paste(lazh1adj$ATE[3], " (", lazh1adj$CI1[3], ", ", lazh1adj$CI2[3], ")", sep=""),
        paste(wazh1adj$ATE[3], " (", wazh1adj$CI1[3], ", ", wazh1adj$CI2[3], ")", sep=""),
        paste(wlzh1adj$ATE[3], " (", wlzh1adj$CI1[3], ", ", wlzh1adj$CI2[3], ")", sep=""),
        paste(hczh1adj$ATE[3], " (", hczh1adj$CI1[3], ", ", hczh1adj$CI2[3], ")", sep=""),
        " ",
        paste(lenvadj$ATE[3], " (", lenvadj$CI1[3], ", ", lenvadj$CI2[3], ")", sep=""),
        paste(weivadj$ATE[3], " (", weivadj$CI1[3], ", ", weivadj$CI2[3], ")", sep=""),
        paste(hcvadj$ATE[3], " (", hcvadj$CI1[3], ", ", hcvadj$CI2[3], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h3adj$Pval[3]), as.character(wazy2h3adj$Pval[3]), as.character(wlzy2h3adj$Pval[3]), as.character(hczy2h3adj$Pval[3]), " ",
        as.character(lazh1adj$Pval[3]), as.character(wazh1adj$Pval[3]), as.character(wlzh1adj$Pval[3]), as.character(hczh1adj$Pval[3]), " ",
        as.character(lenvadj$Pval[3]), as.character(weivadj$Pval[3]), as.character(hcvadj$Pval[3])),
  "Change in T/S Ratio between Year 1 to Year 2, Q1 vs. Q4"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h3unadj$ATE[4], " (", lazy2h3unadj$CI1[4], ", ", lazy2h3unadj$CI2[4], ")", sep=""),
                                  paste(wazy2h3unadj$ATE[4], " (", wazy2h3unadj$CI1[4], ", ", wazy2h3unadj$CI2[4], ")", sep=""),
                                  paste(wlzy2h3unadj$ATE[4], " (", wlzy2h3unadj$CI1[4], ", ", wlzy2h3unadj$CI2[4], ")", sep=""),
                                  paste(hczy2h3unadj$ATE[4], " (", hczy2h3unadj$CI1[4], ", ", hczy2h3unadj$CI2[4], ")", sep=""),
                                  " ",
                                  paste(lazh1unadj$ATE[4], " (", lazh1unadj$CI1[4], ", ", lazh1unadj$CI2[4], ")", sep=""),
                                  paste(wazh1unadj$ATE[4], " (", wazh1unadj$CI1[4], ", ", wazh1unadj$CI2[4], ")", sep=""),
                                  paste(wlzh1unadj$ATE[4], " (", wlzh1unadj$CI1[4], ", ", wlzh1unadj$CI2[4], ")", sep=""),
                                  paste(hczh1unadj$ATE[4], " (", hczh1unadj$CI1[4], ", ", hczh1unadj$CI2[4], ")", sep=""),
                                  " ",
                                  paste(lenvunadj$ATE[4], " (", lenvunadj$CI1[4], ", ", lenvunadj$CI2[4], ")", sep=""),
                                  paste(weivunadj$ATE[4], " (", weivunadj$CI1[4], ", ", weivunadj$CI2[4], ")", sep=""),
                                  paste(hcvunadj$ATE[4], " (", hcvunadj$CI1[4], ", ", hcvunadj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h3unadj$Pval[4]), as.character(wazy2h3unadj$Pval[4]), as.character(wlzy2h3unadj$Pval[4]), as.character(hczy2h3unadj$Pval[4]), " ",
        as.character(lazh1unadj$Pval[4]), as.character(wazh1unadj$Pval[4]), as.character(wlzh1unadj$Pval[4]), as.character(hczh1unadj$Pval[4]), " ",
        as.character(lenvunadj$Pval[4]), as.character(weivunadj$Pval[4]), as.character(hcvunadj$Pval[4])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h3adj$ATE[4], " (", lazy2h3adj$CI1[4], ", ", lazy2h3adj$CI2[4], ")", sep=""),
        paste(wazy2h3adj$ATE[4], " (", wazy2h3adj$CI1[4], ", ", wazy2h3adj$CI2[4], ")", sep=""),
        paste(wlzy2h3adj$ATE[4], " (", wlzy2h3adj$CI1[4], ", ", wlzy2h3adj$CI2[4], ")", sep=""),
        paste(hczy2h3adj$ATE[4], " (", hczy2h3adj$CI1[4], ", ", hczy2h3adj$CI2[4], ")", sep=""),
        " ",
        paste(lazh1adj$ATE[4], " (", lazh1adj$CI1[4], ", ", lazh1adj$CI2[4], ")", sep=""),
        paste(wazh1adj$ATE[4], " (", wazh1adj$CI1[4], ", ", wazh1unadj$CI2[4], ")", sep=""),
        paste(wlzh1adj$ATE[4], " (", wlzh1adj$CI1[4], ", ", wlzh1adj$CI2[4], ")", sep=""),
        paste(hczh1adj$ATE[4], " (", hczh1adj$CI1[4], ", ", hczh1adj$CI2[4], ")", sep=""),
        " ",
        paste(lenvadj$ATE[4], " (", lenvadj$CI1[4], ", ", lenvadj$CI2[4], ")", sep=""),
        paste(weivadj$ATE[4], " (", weivadj$CI1[4], ", ", weivadj$CI2[4], ")", sep=""),
        paste(hcvadj$ATE[4], " (", hcvadj$CI1[4], ", ", hcvadj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h3adj$Pval[4]), as.character(wazy2h3adj$Pval[4]), as.character(wlzy2h3adj$Pval[4]), as.character(hczy2h3adj$Pval[4]), " ",
        as.character(lazh1adj$Pval[4]), as.character(wazh1adj$Pval[4]), as.character(wlzh1adj$Pval[4]), as.character(hczh1adj$Pval[4]), " ",
        as.character(lenvadj$Pval[4]), as.character(weivadj$Pval[4]), as.character(hcvadj$Pval[4]))
)


write.csv(tbl4, file=here("tables/mis011-table4.csv"))

