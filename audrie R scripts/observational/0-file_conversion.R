#convert csv to RData file

d <- read.csv("~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv",colClasses=c("dataid"="character"))
save (d, file = "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData")
