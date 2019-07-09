rm(list=ls())
source(here::here("0-config.R"))

# load data to be included in Table 1
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))

# calculating medians/n for each variable 
female <- length(d$sex[d$sex == "female"])/length(d$sex)* 100 #percentage
length_y1 <- median(d$laz_t2, na.rm=TRUE)
weight_age_y1 <- median(d$waz_t2, na.rm=TRUE)
weight_length_y1 <- median(d$whz_t2, na.rm=TRUE)
headc_age_y1 <- median(d$hcz_t2, na.rm=TRUE)
length_y2 <- median(d$laz_t3, na.rm=TRUE)
weight_age_y2 <- median(d$waz_t3, na.rm=TRUE)
weight_length_y2 <- median(d$hcz_t3, na.rm=TRUE)
headc_age_y2 <- median(d$diar7d_t2, na.rm=TRUE)
d_y1 <- mean(d$diar7d_t2, na.rm=TRUE) * 100 #percentage
d_y2 <- mean(d$diar7d_t3, na.rm=TRUE)* 100 #percentage
agem <- median(d$momage, na.rm=TRUE)
heightm <- median(d$momheight, na.rm=TRUE)
edumom <- median(d$momeduy, na.rm=TRUE) # this is null in the dataset--not sure why
CES_D1 <- median(d$cesd_sum_t2, na.rm=TRUE)
CES_D2 <- median(d$cesd_sum_ee_t3, na.rm=TRUE)
PPS <- median(d$pss_sum_mom_t3, na.rm=TRUE)
viol <- mean(d$life_viol_any_t3, na.rm=TRUE) * 100 #percentage

#create table
tbl <- data.table(" "=character(), " "=character(), " "=character(), "n (%) or median (IQR)"=numeric())
tbl <- rbind(tbl, list(" " = "Child", " " = " ", " " = "Female (%)", "n (%) or median (IQR)" = female), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry (14 months, Year 1)", " " = "Length-for-age Z score", "n (%) or median (IQR)" = length_y1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-age Z score", "n (%) or median (IQR)" = weight_age_y1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-length Z score", "n (%) or median (IQR)" = weight_length_y1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Head circumference-for-age Z score", "n (%) or median (IQR)" = headc_age_y1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry (28 months, Year 2)", " " = "Length-for-age Z score", "n (%) or median (IQR)" = length_y2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-age Z score", "n (%) or median (IQR)" = weight_age_y2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Weight-for-length Z score", "n (%) or median (IQR)" = weight_length_y2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = " ", " " = "Head circumference-for-age Z score", "n (%) or median (IQR)" = headc_age_y2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Diarrhea (14 months, Year 1)", " " = "Caregiver-reported 7-day recall (%)", "n (%) or median (IQR)" = d_y1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Diarrhea (28 months, Year 2)", " " = "Caregiver-reported 7-day recall (%)", "n (%) or median (IQR)" = d_y2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = "Mother", " " = " ", " " = "Age (years)", "n (%) or median (IQR)" = agem), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Anthropometry at enrollment", " " = "Height (cm)", "n (%) or median (IQR)" = heightm), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Education", " " = "Schooling completed (years)", "n (%) or median (IQR)" = edumom), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Depression at Year 1", " " = "CES-D score", "n (%) or median (IQR)" = CES_D1), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Depression at Year 2", " " = "CES-D score", "n (%) or median (IQR)" = CES_D2), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Perceived stress at Year 2", " " = "Perceived Stress Scale score", "n (%) or median (IQR)" = PPS), stringsAsFactors=FALSE)
tbl <- rbind(tbl, list(" " = " ", " " = "Intimate partner violence (%)", " " = "Any lifetime exposure", "n (%) or median (IQR)" = viol), stringsAsFactors=FALSE)

# export table as csv
write.csv(tbl, file = here("tables/table1.csv"))

# number participants
N <- nrow(d)

