rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

outcometbl6 <- c(paste("Ln deltaIL-1", expression(beta), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln deltaIL-6 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln deltaTNF-", expression(alpha), " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
                 "Ln deltaIL-12 (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln deltaIFN-", expression(gamma), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
                 "Ln deltaIL-4 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-5 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-13 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-17A (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-21 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-10 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaIL-2 (pg/ml)", "Control", "Nutrition + WSH", 
                 "Ln deltaGM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
                 paste("Ln deltaIGF-1 (", expression(mu), "g/L)", sep=""), "Control", "Nutrition + WSH")

Ntbl6 <- c()

absmeantbl6 <- c()

meantbl6 <- c()

sdtbl6 <- c()

d23_ln_il1_unadj_L <- round(d23_ln_il1_unadj_L, 2)
d23_ln_il6_unadj_L <- round(d23_ln_il6_unadj_L, 2)
d23_ln_tnf_unadj_L <- round(d23_ln_tnf_unadj_L, 2)
d23_ln_il12_unadj_L <- round(d23_ln_il12_unadj_L, 2)
d23_ln_ifn_unadj_L <- round(d23_ln_ifn_unadj_L, 2)
d23_ln_il4_unadj_L <- round(d23_ln_il4_unadj_L, 2)
d23_ln_il5_unadj_L <- round(d23_ln_il5_unadj_L, 2)
d23_ln_il13_unadj_L <- round(d23_ln_il13_unadj_L, 2)
d23_ln_il17_unadj_L <- round(d23_ln_il17_unadj_L, 2)
d23_ln_il21_unadj_L <- round(d23_ln_il21_unadj_L, 2)
d23_ln_il10_unadj_L <- round(d23_ln_il10_unadj_L, 2)
d23_ln_il2_unadj_L <- round(d23_ln_il2_unadj_L, 2)
d23_ln_gmc_unadj_L <- round(d23_ln_gmc_unadj_L, 2)
d23_ln_igf_unadj_L <- round(d23_ln_igf_unadj_L, 2)

unadjtbl6 <- c(" ", " ", paste(d23_ln_il1_unadj_L[1], "(", d23_ln_il1_unadj_L[2], ", ", d23_ln_il1_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il6_unadj_L[1], "(", d23_ln_il6_unadj_L[2], ", ", d23_ln_il6_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_tnf_unadj_L[1], "(", d23_ln_tnf_unadj_L[2], ", ", d23_ln_tnf_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il12_unadj_L[1], "(", d23_ln_il12_unadj_L[2], ", ", d23_ln_il12_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_ifn_unadj_L[1], "(", d23_ln_ifn_unadj_L[2], ", ", d23_ln_ifn_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il4_unadj_L[1], "(", d23_ln_il4_unadj_L[2], ", ", d23_ln_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il5_unadj_L[1], "(", d23_ln_il5_unadj_L[2], ", ", d23_ln_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il13_unadj_L[1], "(", d23_ln_il13_unadj_L[2], ", ", d23_ln_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il17_unadj_L[1], "(", d23_ln_il17_unadj_L[2], ", ", d23_ln_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il21_unadj_L[1], "(", d23_ln_il21_unadj_L[2], ", ", d23_ln_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il10_unadj_L[1], "(", d23_ln_il10_unadj_L[2], ", ", d23_ln_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il2_unadj_L[1], "(", d23_ln_il2_unadj_L[2], ", ", d23_ln_il2_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_gmc_unadj_L[1], "(", d23_ln_gmc_unadj_L[2], ", ", d23_ln_gmc_unadj_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_igf_unadj_L[1], "(", d23_ln_igf_unadj_L[2], ", ", d23_ln_igf_unadj_L[3], ")", sep=""))

d23_ln_il1_adj_sex_age_L <- round(d23_ln_il1_adj_sex_age_L, 2)
d23_ln_il6_adj_sex_age_L <- round(d23_ln_il6_adj_sex_age_L, 2)
d23_ln_tnf_adj_sex_age_L <- round(d23_ln_tnf_adj_sex_age_L, 2)
d23_ln_il12_adj_sex_age_L <- round(d23_ln_il12_adj_sex_age_L, 2)
d23_ln_ifn_adj_sex_age_L <- round(d23_ln_ifn_adj_sex_age_L, 2)
d23_ln_il4_adj_sex_age_L <- round(d23_ln_il4_adj_sex_age_L, 2)
d23_ln_il5_adj_sex_age_L <- round(d23_ln_il5_adj_sex_age_L, 2)
d23_ln_il13_adj_sex_age_L <- round(d23_ln_il13_adj_sex_age_L, 2)
d23_ln_il17_adj_sex_age_L <- round(d23_ln_il17_adj_sex_age_L, 2)
d23_ln_il21_adj_sex_age_L <- round(d23_ln_il21_adj_sex_age_L, 2)
d23_ln_il10_adj_sex_age_L <- round(d23_ln_il10_adj_sex_age_L, 2)
d23_ln_il2_adj_sex_age_L <- round(d23_ln_il2_adj_sex_age_L, 2)
d23_ln_gmc_adj_sex_age_L <- round(d23_ln_gmc_adj_sex_age_L, 2)
d23_ln_igf_adj_sex_age_L <- round(d23_ln_igf_adj_sex_age_L, 2)

asadjtbl6 <- c(" ", " ", paste(d23_ln_il1_adj_sex_age_L[1], "(", d23_ln_il1_adj_sex_age_L[2], ", ", d23_ln_il1_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il6_adj_sex_age_L[1], "(", d23_ln_il6_adj_sex_age_L[2], ", ", d23_ln_il6_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_tnf_adj_sex_age_L[1], "(", d23_ln_tnf_adj_sex_age_L[2], ", ", d23_ln_tnf_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il12_adj_sex_age_L[1], "(", d23_ln_il12_adj_sex_age_L[2], ", ", d23_ln_il12_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_ifn_adj_sex_age_L[1], "(", d23_ln_ifn_adj_sex_age_L[2], ", ", d23_ln_ifn_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il4_adj_sex_age_L[1], "(", d23_ln_il4_adj_sex_age_L[2], ", ", d23_ln_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il5_adj_sex_age_L[1], "(", d23_ln_il5_adj_sex_age_L[2], ", ", d23_ln_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il13_adj_sex_age_L[1], "(", d23_ln_il13_adj_sex_age_L[2], ", ", d23_ln_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il17_adj_sex_age_L[1], "(", d23_ln_il17_adj_sex_age_L[2], ", ", d23_ln_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il21_adj_sex_age_L[1], "(", d23_ln_il21_adj_sex_age_L[2], ", ", d23_ln_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il10_adj_sex_age_L[1], "(", d23_ln_il10_adj_sex_age_L[2], ", ", d23_ln_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_il2_adj_sex_age_L[1], "(", d23_ln_il2_adj_sex_age_L[2], ", ", d23_ln_il2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_gmc_adj_sex_age_L[1], "(", d23_ln_gmc_adj_sex_age_L[2], ", ", d23_ln_gmc_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(d23_ln_igf_adj_sex_age_L[1], "(", d23_ln_igf_adj_sex_age_L[2], ", ", d23_ln_igf_adj_sex_age_L[3], ")", sep=""))

d23_ln_il1_adj_L <- round(d23_ln_il1_adj_L, 2)
d23_ln_il6_adj_L <- round(d23_ln_il6_adj_L, 2)
d23_ln_tnf_adj_L <- round(d23_ln_tnf_adj_L, 2)
d23_ln_il12_adj_L <- round(d23_ln_il12_adj_L, 2)
d23_ln_ifn_adj_L <- round(d23_ln_ifn_adj_L, 2)
d23_ln_il4_adj_L <- round(d23_ln_il4_adj_L, 2)
d23_ln_il5_adj_L <- round(d23_ln_il5_adj_L, 2)
d23_ln_il13_adj_L <- round(d23_ln_il13_adj_L, 2)
d23_ln_il17_adj_L <- round(d23_ln_il17_adj_L, 2)
d23_ln_il21_adj_L <- round(d23_ln_il21_adj_L, 2)
d23_ln_il10_adj_L <- round(d23_ln_il10_adj_L, 2)
d23_ln_il2_adj_L <- round(d23_ln_il2_adj_L, 2)
d23_ln_gmc_adj_L <- round(d23_ln_gmc_adj_L, 2)
d23_ln_igf_adj_L <- round(d23_ln_igf_adj_L, 2)

adjtbl6 <- c(" ", " ", paste(d23_ln_il1_adj_L[1], "(", d23_ln_il1_adj_L[2], ", ", d23_ln_il1_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il6_adj_L[1], "(", d23_ln_il6_adj_L[2], ", ", d23_ln_il6_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_tnf_adj_L[1], "(", d23_ln_tnf_adj_L[2], ", ", d23_ln_tnf_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il12_adj_L[1], "(", d23_ln_il12_adj_L[2], ", ", d23_ln_il12_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_ifn_adj_L[1], "(", d23_ln_ifn_adj_L[2], ", ", d23_ln_ifn_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il4_adj_L[1], "(", d23_ln_il4_adj_L[2], ", ", d23_ln_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il5_adj_L[1], "(", d23_ln_il5_adj_L[2], ", ", d23_ln_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il13_adj_L[1], "(", d23_ln_il13_adj_L[2], ", ", d23_ln_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il17_adj_L[1], "(", d23_ln_il17_adj_L[2], ", ", d23_ln_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il21_adj_L[1], "(", d23_ln_il21_adj_L[2], ", ", d23_ln_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il10_adj_L[1], "(", d23_ln_il10_adj_L[2], ", ", d23_ln_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_il2_adj_L[1], "(", d23_ln_il2_adj_L[2], ", ", d23_ln_il2_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_gmc_adj_L[1], "(", d23_ln_gmc_adj_L[2], ", ", d23_ln_gmc_adj_L[3], ")", sep=""),
             " ", " ", paste(d23_ln_igf_adj_L[1], "(", d23_ln_igf_adj_L[2], ", ", d23_ln_igf_adj_L[3], ")", sep=""))

# Table 6: Effect of intervention on change in individual immune status and growth factor measurements between ages 14 and 28 months
tbl6 <- data.table(
  "Outcome, Arm" = outcometbl6,
  "N" = Ntbl6, 
  "Absolute Mean" = absmeantbl6,
  "Mean" = meantbl6, 
  "SD" = sdtbl6,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl6,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl6, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl6
)

write.csv(tbl6, file=here('tables/miso9-table6.csv'))
