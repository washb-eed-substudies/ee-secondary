rm(list=ls())
source(here::here("0-config.R"))

# load data to be included in Table 1
d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv"))

# calculating quantiles/n for each variable 
female <- round(length(d$sex[d$sex == "female"])/length(d$sex)* 100) #percentage
telo1med <- round(quantile(d$TS_t2, na.rm=TRUE),2)
telo1bpmed <- round(quantile(d$ts_t2_bp, na.rm=TRUE))
telo2med <- round(quantile(d$TS_t3, na.rm=TRUE),2)
telo2bpmed <- round(quantile(d$ts_t3_bp, na.rm=TRUE))
deltatsmed <- round(quantile(d$delta_TS, na.rm=TRUE),2)
deltatsbpmed <- round(quantile(d$delta_ts_bp, na.rm=TRUE))
length_y1 <- round(quantile(d$laz_t2, na.rm=TRUE),2)
weight_age_y1 <- round(quantile(d$waz_t2, na.rm=TRUE),2)
weight_length_y1 <- round(quantile(d$whz_t2, na.rm=TRUE),2)
headc_age_y1 <- round(quantile(d$hcz_t2, na.rm=TRUE),2)
length_y2 <- round(quantile(d$laz_t3, na.rm=TRUE),2)
weight_age_y2 <- round(quantile(d$waz_t3, na.rm=TRUE),2)
weight_length_y2 <- round(quantile(d$whz_t3, na.rm=TRUE),2)
headc_age_y2 <- round(quantile(d$hcz_t3, na.rm=TRUE),2)
d_y1 <- round(mean(d$diar7d_t2, na.rm=TRUE) * 100) #percentage
d_y2 <- round(mean(d$diar7d_t3, na.rm=TRUE)* 100) #percentage
agem <- round(quantile(d$momage, na.rm=TRUE))
heightm <- round(quantile(d$momheight, na.rm=TRUE),1)
edumom <- round(quantile(d$momeduy, na.rm=TRUE)) 
CES_D1 <- round(quantile(d$cesd_sum_t2, na.rm=TRUE))
CES_D2 <- round(quantile(d$cesd_sum_ee_t3, na.rm=TRUE))
PPS <- round(quantile(d$pss_sum_mom_t3, na.rm=TRUE))
viol <- round(mean(d$life_viol_any_t3, na.rm=TRUE) * 100) #percentage

#create table
tbl <- data.table(" "=character(), " "=character(), " "=character(), "n (%) or median (IQR)"=numeric())
tbl <- rbind(tbl, list(" " = "Child", " " = " ", " " = "Female (%)", 
                       "n (%) or median (IQR)" = paste(length(d$sex[d$sex == "female"]), " (", female, "%)", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Telomere length at Year 1", " " = "T/S Ratio", 
                       "n (%) or median (IQR)" = paste(telo1med[3]," (", telo1med[2], ", ", telo1med[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Base Pairs", 
                       "n (%) or median (IQR)" = paste(telo1bpmed[3]," (", telo1bpmed[2], ", ", telo1bpmed[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Telomere length at Year 2", " " = "T/S Ratio", 
                       "n (%) or median (IQR)" = paste(telo2med[3]," (", telo2med[2], ", ", telo2bpmed[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Base Pairs", 
                       "n (%) or median (IQR)" = paste(telo2bpmed[3]," (", telo2bpmed[2], ", ", telo2bpmed[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Change in telomere length between Year 1 and Year 2", " " = "T/S Ratio", 
                       "n (%) or median (IQR)" = paste(deltatsmed[3]," (", deltatsmed[2], ", ", deltatsmed[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Base Pairs", 
                       "n (%) or median (IQR)" = paste(deltatsbpmed[3]," (", deltatsbpmed[2], ", ", deltatsbpmed[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry (14 months, Year 1)", " " = "Length-for-age Z score", 
                       "n (%) or median (IQR)" = paste(length_y1[3], " (", length_y1[2], ", ", length_y1[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-age Z score", 
                       "n (%) or median (IQR)" = paste(weight_age_y1[3], " (", weight_age_y1[2], ", ", weight_age_y1[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-length Z score", 
                       "n (%) or median (IQR)" = paste(weight_length_y1[3], " (", weight_length_y1[2], ", ", weight_length_y1[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Head circumference-for-age Z score", 
                       "n (%) or median (IQR)" = paste(headc_age_y1[3], " (", headc_age_y1[2], ", ", headc_age_y1[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry (28 months, Year 2)", " " = "Length-for-age Z score", 
                       "n (%) or median (IQR)" = paste(length_y2[3], " (", length_y2[2], ", ", length_y2[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-age Z score", 
                       "n (%) or median (IQR)" = paste(weight_age_y2[3], " (", weight_age_y2[2], ", ", weight_age_y2[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-length Z score", 
                       "n (%) or median (IQR)" = paste(weight_length_y2[3], " (", weight_length_y2[2], ", ", weight_length_y2[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Head circumference-for-age Z score", 
                       "n (%) or median (IQR)" = paste(headc_age_y2[3], " (", headc_age_y2[2], ", ", headc_age_y2[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Diarrhea (14 months, Year 1)", " " = "Caregiver-reported 7-day recall (%)", 
                       "n (%) or median (IQR)" = paste(sum(d$diar7d_t2, na.rm=TRUE), " (", d_y1, "%)", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Diarrhea (28 months, Year 2)", " " = "Caregiver-reported 7-day recall (%)", 
                       "n (%) or median (IQR)" = paste(sum(d$diar7d_t3, na.rm=TRUE), " (", d_y2, "%)", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = "Mother", " " = " ", " " = "Age (years)", 
                       "n (%) or median (IQR)" = paste(agem[3], " (", agem[2], ", ", agem[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry at enrollment", " " = "Height (cm)", 
                       "n (%) or median (IQR)" = paste(heightm[3], " (", heightm[2], ", ", heightm[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Education", " " = "Schooling completed (years)", 
                       "n (%) or median (IQR)" = paste(edumom[3], " (", edumom[2], ", ", edumom[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Depression at Year 1", " " = "CES-D score", 
                       "n (%) or median (IQR)" = paste(CES_D1[3], " (", CES_D1[2], ", ", CES_D1[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Depression at Year 2", " " = "CES-D score", 
                       "n (%) or median (IQR)" = paste(CES_D2[3], " (", CES_D2[2], ", ", CES_D2[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Perceived stress at Year 2", " " = "Perceived Stress Scale score", 
                       "n (%) or median (IQR)" = paste(PPS[3], " (", PPS[2], ", ", PPS[4], ")", sep="")), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Intimate partner violence (%)", " " = "Any lifetime exposure: number of women", 
                       "n (%) or median (IQR)" = paste(sum(d$life_viol_any_t3, na.rm=TRUE), " (", viol, "%)", sep="")), stringsAsFactors=FALSE)

# export table as csv
write.csv(tbl, file = here("tables/table1.csv"))

