rm(list=ls())
source(here::here("0-config.R"))
source(here("audrie R scripts/observational/quartileTMLE_telo_growth.R"))

# round all summary values from data tables for input
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
h5unadj.res <- round_df(h5unadj.res, 2)
h5adj.res <- round_df(h5adj.res, 2)

lazy2h5unadj <- filter(h5unadj.res, Y == "laz_t3")
lazy2h5adj <- filter(h5adj.res, Y == "laz_t3")
wazy2h5unadj <- filter(h5unadj.res, Y == "waz_t3")
wazy2h5adj <- filter(h5adj.res, Y == "waz_t3")
wlzy2h5unadj <- filter(h5unadj.res, Y == "whz_t3")
wlzy2h5adj <- filter(h5adj.res, Y == "whz_t3")
hczy2h5unadj <- filter(h5unadj.res, Y == "hcz_t3")
hczy2h5adj <- filter(h5adj.res, Y == "hcz_t3")

Nlazy2h5 <- as.character(nrow(h5_laz_t3_vs_ts_t3gam.res))
Nwazy2h5<- as.character(nrow(h5_waz_t3_vs_ts_t3gam.res))
Nwlzy2h5 <- as.character(nrow(h5_whz_t3_vs_ts_t3gam.res))
Nhczy2h5 <- as.character(nrow(h5_hcz_t3_vs_ts_t3gam.res))


tbl3 <- data.table(
  "Outcome"=c(" ", " ", "LAZ, Year 2", "WAZ, Year 2", "WLZ, Year 2", "HCZ, Year 2"),
  "N"=c(" ", " ", Nlazy2h5, Nwazy2h5, Nwlzy2h5, Nhczy2h5), 
  "Q1 Mean"=c(" ", " ", as.character(lazy2h5unadj$meanY[1]), as.character(wazy2h5unadj$meanY[1]), as.character(wlzy2h5unadj$meanY[1]), as.character(hczy2h5unadj$meanY[1])),
  "Q2 Mean"=c(" ", " ", as.character(lazy2h5unadj$meanY[2]), as.character(wazy2h5unadj$meanY[2]), as.character(wlzy2h5unadj$meanY[2]), as.character(hczy2h5unadj$meanY[2])), 
  "Q3 Mean"=c(" ", " ", as.character(lazy2h5unadj$meanY[3]), as.character(wazy2h5unadj$meanY[3]), as.character(wlzy2h5unadj$meanY[3]), as.character(hczy2h5unadj$meanY[3])), 
  "Q4 Mean"=c(" ", " ", as.character(lazy2h5unadj$meanY[4]), as.character(wazy2h5unadj$meanY[4]), as.character(wlzy2h5unadj$meanY[4]), as.character(hczy2h5unadj$meanY[4])), 
  "T/S Ratio Year 2, Q1 vs. Q2"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h5unadj$ATE[2], " (", lazy2h5unadj$CI1[2], ", ", lazy2h5unadj$CI2[2], ")", sep=""),
                                  paste(wazy2h5unadj$ATE[2], " (", wazy2h5unadj$CI1[2], ", ", wazy2h5unadj$CI2[2], ")", sep=""),
                                  paste(wlzy2h5unadj$ATE[2], " (", wlzy2h5unadj$CI1[2], ", ", wlzy2h5unadj$CI2[2], ")", sep=""),
                                  paste(hczy2h5unadj$ATE[2], " (", hczy2h5unadj$CI1[2], ", ", hczy2h5unadj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h5unadj$Pval[2]), as.character(wazy2h5unadj$Pval[2]), as.character(wlzy2h5unadj$Pval[2]), as.character(hczy2h5unadj$Pval[2])), 
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h5adj$ATE[2], " (", lazy2h5adj$CI1[2], ", ", lazy2h5adj$CI2[2], ")", sep=""),
        paste(wazy2h5adj$ATE[2], " (", wazy2h5adj$CI1[2], ", ", wazy2h5adj$CI2[2], ")", sep=""),
        paste(wlzy2h5adj$ATE[2], " (", wlzy2h5adj$CI1[2], ", ", wlzy2h5adj$CI2[2], ")", sep=""),
        paste(hczy2h5adj$ATE[2], " (", hczy2h5adj$CI1[2], ", ", hczy2h5adj$CI2[2], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h5adj$Pval[2]), as.character(wazy2h5adj$Pval[2]), as.character(wlzy2h5adj$Pval[2]), as.character(hczy2h5adj$Pval[2])), 
  "T/S Ratio Year 2, Q1 vs. Q3"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h5unadj$ATE[3], " (", lazy2h5unadj$CI1[3], ", ", lazy2h5unadj$CI2[3], ")", sep=""),
                                  paste(wazy2h5unadj$ATE[3], " (", wazy2h5unadj$CI1[3], ", ", wazy2h5unadj$CI2[3], ")", sep=""),
                                  paste(wlzy2h5unadj$ATE[3], " (", wlzy2h5unadj$CI1[3], ", ", wlzy2h5unadj$CI2[3], ")", sep=""),
                                  paste(hczy2h5unadj$ATE[3], " (", hczy2h5unadj$CI1[3], ", ", hczy2h5unadj$CI2[3], ")", sep="")), 
  " "=c(" ", "P-value", as.character(lazy2h5unadj$Pval[3]), as.character(wazy2h5unadj$Pval[3]), as.character(wlzy2h5unadj$Pval[3]), as.character(hczy2h5unadj$Pval[3])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h5adj$ATE[3], " (", lazy2h5adj$CI1[3], ", ", lazy2h5adj$CI2[3], ")", sep=""),
        paste(wazy2h5adj$ATE[3], " (", wazy2h5adj$CI1[3], ", ", wazy2h5adj$CI2[3], ")", sep=""),
        paste(wlzy2h5adj$ATE[3], " (", wlzy2h5adj$CI1[3], ", ", wlzy2h5adj$CI2[3], ")", sep=""),
        paste(hczy2h5adj$ATE[3], " (", hczy2h5adj$CI1[3], ", ", hczy2h5adj$CI2[3], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h5adj$Pval[3]), as.character(wazy2h5adj$Pval[3]), as.character(wlzy2h5adj$Pval[3]), as.character(hczy2h5adj$Pval[3])),
  "T/S Ratio Year 2, Q1 vs. Q4"=c("Unadjusted", "Coefficient (95% CI)", paste(lazy2h5unadj$ATE[4], " (", lazy2h5unadj$CI1[4], ", ", lazy2h5unadj$CI2[4], ")", sep=""),
                                  paste(wazy2h5unadj$ATE[4], " (", wazy2h5unadj$CI1[4], ", ", wazy2h5unadj$CI2[4], ")", sep=""),
                                  paste(wlzy2h5unadj$ATE[4], " (", wlzy2h5unadj$CI1[4], ", ", wlzy2h5unadj$CI2[4], ")", sep=""),
                                  paste(hczy2h5unadj$ATE[4], " (", hczy2h5unadj$CI1[4], ", ", hczy2h5unadj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h5unadj$Pval[4]), as.character(wazy2h5unadj$Pval[4]), as.character(wlzy2h5unadj$Pval[4]), as.character(hczy2h5unadj$Pval[4])),
  " "=c("Fully adjusted", "Coefficient (95% CI)", paste(lazy2h5adj$ATE[4], " (", lazy2h5adj$CI1[4], ", ", lazy2h5adj$CI2[4], ")", sep=""),
        paste(wazy2h5adj$ATE[4], " (", wazy2h5adj$CI1[4], ", ", wazy2h5adj$CI2[4], ")", sep=""),
        paste(wlzy2h5adj$ATE[4], " (", wlzy2h5adj$CI1[4], ", ", wlzy2h5adj$CI2[4], ")", sep=""),
        paste(hczy2h5adj$ATE[4], " (", hczy2h5adj$CI1[4], ", ", hczy2h5adj$CI2[4], ")", sep="")),
  " "=c(" ", "P-value", as.character(lazy2h5adj$Pval[4]), as.character(wazy2h5adj$Pval[4]), as.character(wlzy2h5adj$Pval[4]), as.character(hczy2h5adj$Pval[4]))
)


write.csv(tbl3, file=here("tables/mis011-table3.csv"))

