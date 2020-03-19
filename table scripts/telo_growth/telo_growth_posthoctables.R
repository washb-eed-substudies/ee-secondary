rm(list=ls())
library("xtable")
source(here::here("0-config.R"))
load(here("audrie results/telo_growth_spline_fits.Rdata"))
load(here("audrie results/telo_growth_results.Rdata"))

# anthro is the string name of the anthro outcome to input into table
make_table <- function(anthro){
  
  second <- list(" ", " ", " ", " ", " ", " ", 
              "Unadjusted", " ", "Fully adjusted", " ", 
              "Unadjusted", " ", "Fully adjusted", " ", 
              "Unadjusted", " ", "Fully adjusted", " ")
  
  third <- list(" ", " ", " ", " ", " ", " ", 
             "Coefficient (95% CI)", "P-value", "Coefficient (95% CI)", "P-value",
             "Coefficient (95% CI)", "P-value", "Coefficient (95% CI)", "P-value",
             "Coefficient (95% CI)", "P-value", "Coefficient (95% CI)", "P-value")
  
  dat <- data.table("Outcome", "N", "Q1 Mean", "Q2 Mean", "Q3 Mean", "Q4 Mean", 
                      paste(anthro, ", Q1 vs. Q2", sep=""), " ", " ", " ",
                      paste(anthro, ", Q1 vs. Q3", sep=""), " ", " ", " ",
                      paste(anthro, ", Q1 vs. Q4", sep=""), " ", " ", " ")
  dat <- rbind(dat, second)
  dat <- rbind(dat, third)
  dat
  
}

# function will return TS row for specific anthro outcome
TSrows <- function(tbladj, tblunadj, var, tsyear, hypo){
  
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
  }
  
  # round numerical values and filter tables
  tbladj <- round_df(tbladj, 2)
  tblunadj <- round_df(tblunadj, 2)
  
  tbladj <- tbladj[tbladj$A == var,]
  tblunadj <- tblunadj[tblunadj$A == var,]
  
  coef <- function(tbl, num) {
    paste(tbl$ATE[num], " (", tbl$CI1[num], ", ", tbl$CI2[num], ")", sep="")
  }
  
  list(tsyear, as.character(nrow(hypo)), as.character(tblunadj$meanY[1]), as.character(tblunadj$meanY[2]), as.character(tblunadj$meanY[3]), as.character(tblunadj$meanY[4]),
       coef(tblunadj, 2), as.character(tblunadj$Pval[2]), coef(tbladj, 2), as.character(tbladj$Pval[2]),
       coef(tblunadj, 3), as.character(tblunadj$Pval[3]), coef(tbladj, 3), as.character(tbladj$Pval[3]),
       coef(tblunadj, 4), as.character(tblunadj$Pval[4]), coef(tbladj, 4), as.character(tbladj$Pval[4]))
  
}

save <- function(tbl, name){
  write.table(tbl, file=here(paste("tables/miso11-post-hoc-supplementary/", name, ".csv", sep="")), sep=",", col.names=F, row.names=F)
  print(xtable(tbl), type="html", file=here(paste("tables/miso11-post-hoc-supplementary/", name, ".html", sep="")))
}


## MAKING TABLES ##
# all will be saved under here("tables/miso11-post-hoc-supplementary/...")

# LAZ/WAZ/WLZ/HCZ at Month 3
lazm3 <- make_table("LAZ Month 3")
lazm3 <- rbind(lazm3, TSrows(h6cadj.res, h6cunadj.res, "laz_t1", "T/S Ratio Year 1", h6c_ts_t2_vs_laz_t1gam.res))
lazm3 <- rbind(lazm3, TSrows(h6dadj.res, h6dunadj.res, "laz_t1", "T/S Ratio Year 2", h6d_ts_t3_vs_laz_t1gam.res))
save(lazm3, "lazm3")

wazm3 <- make_table("WAZ Month 3")
wazm3 <- rbind(wazm3, TSrows(h6cadj.res, h6cunadj.res, "waz_t1", "T/S Ratio Year 1", h6c_ts_t2_vs_waz_t1gam.res))
wazm3 <- rbind(wazm3, TSrows(h6dadj.res, h6dunadj.res, "waz_t1", "T/S Ratio Year 2", h6d_ts_t3_vs_waz_t1gam.res))
save(wazm3, "wazm3")

whzm3 <- make_table("WLZ Month 3")
whzm3 <- rbind(whzm3, TSrows(h6cadj.res, h6cunadj.res, "whz_t1", "T/S Ratio Year 1", h6c_ts_t2_vs_whz_t1gam.res))
whzm3 <- rbind(whzm3, TSrows(h6dadj.res, h6dunadj.res, "whz_t1", "T/S Ratio Year 2", h6d_ts_t3_vs_whz_t1gam.res))
save(whzm3, "whzm3")

hczm3 <- make_table("HCZ Month 3")
hczm3 <- rbind(hczm3, TSrows(h6cadj.res, h6cunadj.res, "hcz_t1", "T/S Ratio Year 1", h6c_ts_t2_vs_hcz_t1gam.res))
hczm3 <- rbind(hczm3, TSrows(h6dadj.res, h6dunadj.res, "hcz_t1", "T/S Ratio Year 2", h6d_ts_t3_vs_hcz_t1gam.res))
save(hczm3, "hczm3")


# LAZ/WAZ/WLZ/HCZ at Year 1
lazy1 <- make_table("LAZ Year 1")
lazy1 <- rbind(lazy1, TSrows(h6badj.res, h6bunadj.res, "laz_t2", "T/S Ratio Year 2", h6b_ts_t3_vs_laz_t2gam.res))
save(lazy1, "lazy1")

wazy1 <- make_table("WAZ Year 1")
wazy1 <- rbind(wazy1, TSrows(h6badj.res, h6bunadj.res, "waz_t2", "T/S Ratio Year 2", h6b_ts_t3_vs_waz_t2gam.res))
save(wazy1, "wazy1")

whzy1 <- make_table("WLZ Year 1")
whzy1 <- rbind(whzy1, TSrows(h6badj.res, h6bunadj.res, "whz_t2", "T/S Ratio Year 2", h6b_ts_t3_vs_whz_t2gam.res))
save(whzy1, "whzy1")

hczy1 <- make_table("HCZ Year 1")
hczy1 <- rbind(hczy1, TSrows(h6badj.res, h6bunadj.res, "hcz_t2", "T/S Ratio Year 2", h6b_ts_t3_vs_hcz_t2gam.res))
save(hczy1, "hczy1")


# Velocities Year 1 to Year 2
lvy12 <- make_table("Length velocity (cm/month) Year 1 to 2")
lvy12 <- rbind(lvy12, TSrows(h7dadj.res, h7dunadj.res, "len_velocity_t2_t3", "T/S Ratio Year 2", h7d_ts_t3_vs_len_veloct2t3gam.res))
save(lvy12, "lenvelocityy12")

wvy12 <- make_table("Weight velocity (kg/month) Year 1 to 2")
wvy12 <- rbind(wvy12, TSrows(h7dadj.res, h7dunadj.res, "wei_velocity_t2_t3", "T/S Ratio Year 2", h7d_ts_t3_vs_wei_veloct2t3gam.res))
save(wvy12, "weivelocityy12")

hcvy12 <- make_table("Head circumference velocity (cm/month) Year 1 to 2")
hcvy12 <- rbind(hcvy12, TSrows(h7dadj.res, h7dunadj.res, "hc_velocity_t2_t3", "T/S Ratio Year 2", h7d_ts_t3_vs_hc_veloct2t3gam.res))
save(hcvy12, "hcvelocityy12")


# Velocities from Month 3 to Year 2
lvm3y2 <- make_table("Length velocity (cm/month) Month 3 to Year 2")
lvm3y2 <- rbind(lvm3y2, TSrows(h7eadj.res, h7eunadj.res, "len_velocity_t1_t3", "T/S Ratio Year 2", h7e_ts_t3_vs_len_veloct1t3gam.res))
save(lvm3y2, "lenvelocitym3y2")

wvm3y2 <- make_table("Weight velocity (kg/month) Month 3 to Year 2")
wvm3y2 <- rbind(wvm3y2, TSrows(h7eadj.res, h7eunadj.res, "wei_velocity_t1_t3", "T/S Ratio Year 2", h7e_ts_t3_vs_wei_veloct1t3gam.res))
save(wvm3y2, "weivelocitym3y2")

hcvm3y2 <- make_table("Head circumference velocity (cm/month) Month 3 to Year 2")
hcvm3y2 <- rbind(hcvm3y2, TSrows(h7eadj.res, h7eunadj.res, "hc_velocity_t1_t3", "T/S Ratio Year 2", h7e_ts_t3_vs_hc_veloct1t3gam.res))
save(hcvm3y2, "hcvelocitym3y2")


# Velocities from Month 3 to Year 1
lvm3y1 <- make_table("Length velocity (cm/month) Month 3 to Year 1")
lvm3y1 <- rbind(lvm3y1, TSrows(h7badj.res, h7bunadj.res, "len_velocity_t1_t2", "T/S Ratio Year 1", h7b_ts_t2_vs_len_veloct1t2gam.res))
lvm3y1 <- rbind(lvm3y1, TSrows(h7cadj.res, h7cunadj.res, "len_velocity_t1_t2", "T/S Ratio Year 2", h7c_ts_t3_vs_len_veloct1t2gam.res))
save(lvm3y1, "lenvelocitym3y1")

wvm3y1 <- make_table("Weight velocity (kg/month) Month 3 to Year 1")
wvm3y1 <- rbind(wvm3y1, TSrows(h7badj.res, h7bunadj.res, "wei_velocity_t1_t2", "T/S Ratio Year 1", h7b_ts_t2_vs_wei_veloct1t2gam.res))
wvm3y1 <- rbind(wvm3y1, TSrows(h7cadj.res, h7cunadj.res, "wei_velocity_t1_t2", "T/S Ratio Year 2", h7c_ts_t3_vs_wei_veloct1t2gam.res))
save(wvm3y1, "weivelocitym3y1")

hcvm3y1 <- make_table("Head circumference velocity (cm/month) Month 3 to Year 1")
hcvm3y1 <- rbind(hcvm3y1, TSrows(h7badj.res, h7bunadj.res, "hc_velocity_t1_t2", "T/S Ratio Year 1", h7b_ts_t2_vs_hc_veloct1t2gam.res))
hcvm3y1 <- rbind(hcvm3y1, TSrows(h7cadj.res, h7cunadj.res, "hc_velocity_t1_t2", "T/S Ratio Year 2", h7c_ts_t3_vs_hc_veloct1t2gam.res))
save(hcvm3y1, "hcvelocitym3y1")


# Change in LAZ/WAZ/WLZ/HCZ Year 1 to Year 2
deltalazyy12 <- make_table("Change in LAZ between Year 1 to Year 2")
deltalazyy12 <- rbind(deltalazyy12, TSrows(h8dadj.res, h8dunadj.res, "delta_laz_t2_t3", "T/S Ratio Year 2", h8d_ts_t3_v_delta_lazt2t3gam.res))
save(deltalazyy12, "deltalazyy12")

deltawazyy12 <- make_table("Change in WAZ between Year 1 to Year 2")
deltawazyy12 <- rbind(deltawazyy12, TSrows(h8dadj.res, h8dunadj.res, "delta_waz_t2_t3", "T/S Ratio Year 2", h8d_ts_t3_v_delta_wazt2t3gam.res))
save(deltawazyy12, "deltawazyy12")

deltawhzyy12 <- make_table("Change in WLZ between Year 1 to Year 2")
deltawhzyy12 <- rbind(deltawhzyy12, TSrows(h8dadj.res, h8dunadj.res, "delta_whz_t2_t3", "T/S Ratio Year 2", h8d_ts_t3_v_delta_whzt2t3gam.res))
save(deltawhzyy12, "deltawhzyy12")

deltahczyy12 <- make_table("Change in HCZ between Year 1 to Year 2")
deltahczyy12 <- rbind(deltahczyy12, TSrows(h8dadj.res, h8dunadj.res, "delta_hcz_t2_t3", "T/S Ratio Year 2", h8d_ts_t3_v_delta_hczt2t3gam.res))
save(deltahczyy12, "deltahczyy12")


# Change in LAZ/WAZ/WLZ/HCZ Month 3 to Year 2
deltalazym3y2 <- make_table("Change in LAZ between Month 3 to Year 2")
deltalazym3y2 <- rbind(deltalazym3y2, TSrows(h8eadj.res, h8eunadj.res, "delta_laz_t1_t3", "T/S Ratio Year 2", h8e_ts_t3_v_delta_lazt1t3gam.res))
save(deltalazym3y2, "deltalazym3y2")

deltawazym3y2 <- make_table("Change in WAZ between Month 3 to Year 2")
deltawazym3y2 <- rbind(deltawazym3y2, TSrows(h8eadj.res, h8eunadj.res, "delta_waz_t1_t3", "T/S Ratio Year 2", h8e_ts_t3_v_delta_wazt1t3gam.res))
save(deltawazym3y2, "deltawazym3y2")

deltawhzym3y2 <- make_table("Change in WLZ between Month 3 to Year 2")
deltawhzym3y2 <- rbind(deltawhzym3y2, TSrows(h8eadj.res, h8eunadj.res, "delta_whz_t1_t3", "T/S Ratio Year 2", h8e_ts_t3_v_delta_whzt1t3gam.res))
save(deltawhzym3y2, "deltawhzym3y2")

deltahczym3y2 <- make_table("Change in HCZ between Month 3 to Year 2")
deltahczym3y2 <- rbind(deltahczym3y2, TSrows(h8eadj.res, h8eunadj.res, "delta_hcz_t1_t3", "T/S Ratio Year 2", h8e_ts_t3_v_delta_hczt1t3gam.res))
save(deltahczym3y2, "deltahczym3y2")


# Change in LAZ/WAZ/WLZ/HCZ Month 3 to Year 1
deltalazym3y1 <- make_table("Change in LAZ between Month 3 to Year 1")
deltalazym3y1 <- rbind(deltalazym3y1, TSrows(h8badj.res, h8bunadj.res, "delta_laz_t1_t2", "T/S Ratio Year 1", h8b_ts_t2_v_delta_lazt1t2gam.res))
deltalazym3y1 <- rbind(deltalazym3y1, TSrows(h8cadj.res, h8cunadj.res, "delta_laz_t1_t2", "T/S Ratio Year 2", h8c_ts_t3_v_delta_lazt1t2gam.res))
save(deltalazym3y1, "deltalazym3y1")

deltawazym3y1 <- make_table("Change in WAZ between Month 3 to Year 1")
deltawazym3y1 <- rbind(deltawazym3y1, TSrows(h8badj.res, h8bunadj.res, "delta_waz_t1_t2", "T/S Ratio Year 1", h8b_ts_t2_v_delta_wazt1t2gam.res))
deltawazym3y1 <- rbind(deltawazym3y1, TSrows(h8cadj.res, h8cunadj.res, "delta_waz_t1_t2", "T/S Ratio Year 2", h8c_ts_t3_v_delta_wazt1t2gam.res))
save(deltawazym3y1, "deltawazym3y1")

deltawhzym3y1 <- make_table("Change in WLZ between Month 3 to Year 1")
deltawhzym3y1 <- rbind(deltawhzym3y1, TSrows(h8badj.res, h8bunadj.res, "delta_whz_t1_t2", "T/S Ratio Year 1", h8b_ts_t2_v_delta_whzt1t2gam.res))
deltawhzym3y1 <- rbind(deltawhzym3y1, TSrows(h8cadj.res, h8cunadj.res, "delta_whz_t1_t2", "T/S Ratio Year 2", h8c_ts_t3_v_delta_whzt1t2gam.res))
save(deltawhzym3y1, "deltawhzym3y1")

deltahczym3y1 <- make_table("Change in HCZ between Month 3 to Year 1")
deltahczym3y1 <- rbind(deltahczym3y1, TSrows(h8badj.res, h8bunadj.res, "delta_hcz_t1_t2", "T/S Ratio Year 1", h8b_ts_t2_v_delta_hczt1t2gam.res))
deltahczym3y1 <- rbind(deltahczym3y1, TSrows(h8cadj.res, h8cunadj.res, "delta_hcz_t1_t2", "T/S Ratio Year 2", h8c_ts_t3_v_delta_hczt1t2gam.res))
save(deltahczym3y1, "deltahczym3y1")

