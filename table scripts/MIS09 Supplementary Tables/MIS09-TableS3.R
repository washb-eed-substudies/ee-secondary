rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))

writeqntle<-function(vector) {
  quantiles<-round(quantile(vector, na.rm=TRUE), 2)
  paste(quantiles[3], "(", quantiles[2], ", ", quantiles[4], ")", sep="")
}

outcome<-c("Outcome", "IL-1b (pg/ml)", "Il-6 (pg/ml)", "TNF-a (pg/ml)", "CRP (mg/L)", "IL-12 (pg/ml)",
           "IFN-g (pg/ml)", "IL-4 (pg/ml)", "IL-5 (pg/ml)", "IL-13 (pg/ml)", "IL-17A (pg/ml)", 
           "IL-21 (pg/ml)", "IL-10 (pg/ml)", "IL-2 (pg/ml)", "GMCSF (pg/ml)", "AGP (g/L)", "IGF-1 (ug/L)")

t2<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t2), writeqntle(lab$il6_t2), writeqntle(lab$tnfa_t2), writeqntle(lab$crp_t2), writeqntle(lab$il12_t2), writeqntle(lab$ifng_t2), 
      writeqntle(lab$il4_t2), writeqntle(lab$il5_t2), writeqntle(lab$il13_t2), writeqntle(lab$il17_t2), writeqntle(lab$il21_t2), writeqntle(lab$il10_t2), writeqntle(lab$il2_t2), 
      writeqntle(lab$gmcsf_t2), writeqntle(lab$agp_t2), writeqntle(lab$igf_t2))

t3<-c("Median (25th, 75th percentile)", writeqntle(lab$il1_t3), writeqntle(lab$il6_t3), writeqntle(lab$tnfa_t3), " ", writeqntle(lab$il12_t3), writeqntle(lab$ifng_t3), 
      writeqntle(lab$il4_t3), writeqntle(lab$il5_t3), writeqntle(lab$il13_t3), writeqntle(lab$il17_t3), writeqntle(lab$il21_t3), writeqntle(lab$il10_t3), writeqntle(lab$il2_t3), 
      writeqntle(lab$gmcsf_t3), " ", writeqntle(lab$igf_t3))

tbls3<-data.table(" "=outcome,
                  "Child Age 14 Months"=t2,
                  "Child Age 28 Months"=t3)

write.csv(tbls3, file=here('tables/miso9-supptable2.csv'))
