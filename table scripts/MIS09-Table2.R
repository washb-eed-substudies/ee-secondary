rm(list=ls())
source(here::here("0-config.R"))

#NEEDS ABSOLUTE MEAN COLUMN STILL!

source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
outcometbl2 <- c(paste("Ln IL-1", expression(beta), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
             "Ln IL-6 (pg/ml)", "Control", "Nutrition + WSH", 
             paste("Ln TNF-", expression(alpha), " (pg/ml)", sep=""), "Control", "Nutrition + WSH",
             "Ln CRP (mg/L)", "Control", "Nutrition + WSH", 
             "Ln IL-12 (pg/ml)", "Control", "Nutrition + WSH", 
             paste("Ln IFN-", expression(gamma), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
             "Ln IL-4 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-5 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-13 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-17A (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-21 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-10 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln IL-2 (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln GM-CSF (pg/ml)", "Control", "Nutrition + WSH", 
             "Ln AGP (g/L)", "Control", "Nutrition + WSH",
             paste("Ln IGF-1 (", expression(mu), "g/L)", sep=""), "Control", "Nutrition + WSH")

Ntbl2 <- c(" ", as.character(il1_t2_N_tr$t2_ln_il1_N_tr[1]), as.character(il1_t2_N_tr$t2_ln_il1_N_tr[2]),
       " ", as.character(il6_t2_N_tr$t2_ln_il6_N_tr[1]), as.character(il6_t2_N_tr$t2_ln_il6_N_tr[2]),
       " ", as.character(tnf_t2_N_tr$t2_ln_tnf_N_tr[1]), as.character(tnf_t2_N_tr$t2_ln_tnf_N_tr[2]),
       " ", as.character(crp_t2_N_tr$t2_ln_crp_N_tr[1]), as.character(crp_t2_N_tr$t2_ln_crp_N_tr[2]),
       " ", as.character(il12_t2_N_tr$t2_ln_il12_N_tr[1]), as.character(il12_t2_N_tr$t2_ln_il12_N_tr[2]),
       " ", as.character(ifn_t2_N_tr$t2_ln_ifn_N_tr[1]), as.character(ifn_t2_N_tr$t2_ln_ifn_N_tr[2]),
       " ", as.character(il4_t2_N_tr$t2_ln_il4_N_tr[1]), as.character(il4_t2_N_tr$t2_ln_il4_N_tr[2]),
       " ", as.character(il5_t2_N_tr$t2_ln_il5_N_tr[1]), as.character(il5_t2_N_tr$t2_ln_il5_N_tr[2]),
       " ", as.character(il13_t2_N_tr$t2_ln_il13_N_tr[1]), as.character(il13_t2_N_tr$t2_ln_il13_N_tr[2]),
       " ", as.character(il17_t2_N_tr$t2_ln_il17_N_tr[1]), as.character(il17_t2_N_tr$t2_ln_il17_N_tr[2]),
       " ", as.character(il21_t2_N_tr$t2_ln_il21_N_tr[1]), as.character(il21_t2_N_tr$t2_ln_il21_N_tr[2]),
       " ", as.character(il10_t2_N_tr$t2_ln_il10_N_tr[1]), as.character(il10_t2_N_tr$t2_ln_il10_N_tr[2]),
       " ", as.character(il2_t2_N_tr$t2_ln_il2_N_tr[1]), as.character(il2_t2_N_tr$t2_ln_il2_N_tr[2]),
       " ", as.character(gmc_t2_N_tr$t2_ln_gmc_N_tr[1]), as.character(gmc_t2_N_tr$t2_ln_gmc_N_tr[2]),
       " ", as.character(agp_t2_N_tr$t2_ln_agp_N_tr[1]), as.character(agp_t2_N_tr$t2_ln_agp_N_tr[2]),
       " ", as.character(igf_t2_N_tr$t2_ln_igf_N_tr[1]), as.character(igf_t2_N_tr$t2_ln_igf_N_tr[2]))

meantbl2 <- c(" ", as.character(round(il1_t2_N_tr$mean[1], 2)), as.character(round(il1_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il6_t2_N_tr$mean[1], 2)), as.character(round(il6_t2_N_tr$mean[2], 2)),
              " ", as.character(round(tnf_t2_N_tr$mean[1], 2)), as.character(round(tnf_t2_N_tr$mean[2], 2)),
              " ", as.character(round(crp_t2_N_tr$mean[1], 2)), as.character(round(crp_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il12_t2_N_tr$mean[1], 2)), as.character(round(il12_t2_N_tr$mean[2], 2)),
              " ", as.character(round(ifn_t2_N_tr$mean[1], 2)), as.character(round(ifn_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il4_t2_N_tr$mean[1], 2)), as.character(round(il4_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il5_t2_N_tr$mean[1], 2)), as.character(round(il5_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il13_t2_N_tr$mean[1], 2)), as.character(round(il13_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il17_t2_N_tr$mean[1], 2)), as.character(round(il17_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il21_t2_N_tr$mean[1], 2)), as.character(round(il21_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il10_t2_N_tr$mean[1], 2)), as.character(round(il10_t2_N_tr$mean[2], 2)),
              " ", as.character(round(il2_t2_N_tr$mean[1], 2)), as.character(round(il2_t2_N_tr$mean[2], 2)),
              " ", as.character(round(gmc_t2_N_tr$mean[1], 2)), as.character(round(gmc_t2_N_tr$mean[2], 2)),
              " ", as.character(round(agp_t2_N_tr$mean[1], 2)), as.character(round(agp_t2_N_tr$mean[2], 2)),
              " ", as.character(round(igf_t2_N_tr$mean[1], 2)), as.character(round(igf_t2_N_tr$mean[2], 2)))

sdtbl2 <- c(" ", as.character(round(il1_t2_N_tr$sd[1], 2)), as.character(round(il1_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il6_t2_N_tr$sd[1], 2)), as.character(round(il6_t2_N_tr$sd[2], 2)),
            " ", as.character(round(tnf_t2_N_tr$sd[1], 2)), as.character(round(tnf_t2_N_tr$sd[2], 2)),
            " ", as.character(round(crp_t2_N_tr$sd[1], 2)), as.character(round(crp_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il12_t2_N_tr$sd[1], 2)), as.character(round(il12_t2_N_tr$sd[2], 2)),
            " ", as.character(round(ifn_t2_N_tr$sd[1], 2)), as.character(round(ifn_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il4_t2_N_tr$sd[1], 2)), as.character(round(il4_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il5_t2_N_tr$sd[1], 2)), as.character(round(il5_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il13_t2_N_tr$sd[1], 2)), as.character(round(il13_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il17_t2_N_tr$sd[1], 2)), as.character(round(il17_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il21_t2_N_tr$sd[1], 2)), as.character(round(il21_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il10_t2_N_tr$sd[1], 2)), as.character(round(il10_t2_N_tr$sd[2], 2)),
            " ", as.character(round(il2_t2_N_tr$sd[1], 2)), as.character(round(il2_t2_N_tr$sd[2], 2)),
            " ", as.character(round(gmc_t2_N_tr$sd[1], 2)), as.character(round(gmc_t2_N_tr$sd[2], 2)),
            " ", as.character(round(agp_t2_N_tr$sd[1], 2)), as.character(round(agp_t2_N_tr$sd[2], 2)),
            " ", as.character(round(igf_t2_N_tr$sd[1], 2)), as.character(round(igf_t2_N_tr$sd[2], 2)))

t2_agp_unadj_L <- round(t2_agp_unadj_L, 2)
t2_crp_unadj_L <- round(t2_crp_unadj_L, 2)
t2_gmc_unadj_L <- round(t2_gmc_unadj_L, 2)
t2_ifn_unadj_L <- round(t2_ifn_unadj_L, 2)
t2_igf_unadj_L <- round(t2_igf_unadj_L, 2)
t2_il1_unadj_L <- round(t2_il1_unadj_L, 2)
t2_il10_unadj_L <- round(t2_il10_unadj_L, 2)
t2_il12_unadj_L <- round(t2_il12_unadj_L, 2)
t2_il13_unadj_L <- round(t2_il13_unadj_L, 2)
t2_il17_unadj_L <- round(t2_il17_unadj_L, 2)
t2_il2_unadj_L <- round(t2_il2_unadj_L, 2)
t2_il21_unadj_L <- round(t2_il21_unadj_L, 2)
t2_il4_unadj_L <- round(t2_il4_unadj_L, 2)
t2_il5_unadj_L <- round(t2_il5_unadj_L, 2)
t2_il6_unadj_L <- round(t2_il6_unadj_L, 2)
t2_tnf_unadj_L <- round(t2_tnf_unadj_L, 2)

unadjtbl2 <- c(" ", " ", paste(t2_il1_unadj_L[1], "(", t2_il1_unadj_L[2], ", ", t2_il1_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il6_unadj_L[1], "(", t2_il6_unadj_L[2], ", ", t2_il6_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_tnf_unadj_L[1], "(", t2_tnf_unadj_L[2], ", ", t2_tnf_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_crp_unadj_L[1], "(", t2_crp_unadj_L[2], ", ", t2_crp_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il12_unadj_L[1], "(", t2_il12_unadj_L[2], ", ", t2_il12_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_ifn_unadj_L[1], "(", t2_ifn_unadj_L[2], ", ", t2_ifn_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il4_unadj_L[1], "(", t2_il4_unadj_L[2], ", ", t2_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il5_unadj_L[1], "(", t2_il5_unadj_L[2], ", ", t2_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il13_unadj_L[1], "(", t2_il13_unadj_L[2], ", ", t2_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il17_unadj_L[1], "(", t2_il17_unadj_L[2], ", ", t2_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il21_unadj_L[1], "(", t2_il21_unadj_L[2], ", ", t2_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il10_unadj_L[1], "(", t2_il10_unadj_L[2], ", ", t2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_il2_unadj_L[1], "(", t2_il2_unadj_L[2], ", ", t2_il2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_gmc_unadj_L[1], "(", t2_gmc_unadj_L[2], ", ", t2_gmc_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_agp_unadj_L[1], "(", t2_agp_unadj_L[2], ", ", t2_agp_unadj_L[3], ")", sep=""),
               " ", " ", paste(t2_igf_unadj_L[1], "(", t2_igf_unadj_L[2], ", ", t2_igf_unadj_L[3], ")", sep="")) 

source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))

t2_agp_adj_sex_age_L <- round(t2_agp_adj_sex_age_L, 2)
t2_crp_adj_sex_age_L <- round(t2_crp_adj_sex_age_L, 2)
t2_gmc_adj_sex_age_L <- round(t2_gmc_adj_sex_age_L, 2)
t2_ifn_adj_sex_age_L <- round(t2_ifn_adj_sex_age_L, 2)
t2_igf_adj_sex_age_L <- round(t2_igf_adj_sex_age_L, 2)
t2_il1_adj_sex_age_L <- round(t2_il1_adj_sex_age_L, 2)
t2_il10_adj_sex_age_L <- round(t2_il10_adj_sex_age_L, 2)
t2_il12_adj_sex_age_L <- round(t2_il12_adj_sex_age_L, 2)
t2_il13_adj_sex_age_L <- round(t2_il13_adj_sex_age_L, 2)
t2_il17_adj_sex_age_L <- round(t2_il17_adj_sex_age_L, 2)
t2_il2_adj_sex_age_L <- round(t2_il2_adj_sex_age_L, 2)
t2_il21_adj_sex_age_L <- round(t2_il21_adj_sex_age_L, 2)
t2_il4_adj_sex_age_L <- round(t2_il4_adj_sex_age_L, 2)
t2_il5_adj_sex_age_L <- round(t2_il5_adj_sex_age_L, 2)
t2_il6_adj_sex_age_L <- round(t2_il6_adj_sex_age_L, 2)
t2_tnf_adj_sex_age_L <- round(t2_tnf_adj_sex_age_L, 2)

asadjtbl2 <- c(" ", " ", paste(t2_il1_adj_sex_age_L[1], "(", t2_il1_adj_sex_age_L[2], ", ", t2_il1_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il6_adj_sex_age_L[1], "(", t2_il6_adj_sex_age_L[2], ", ", t2_il6_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_tnf_adj_sex_age_L[1], "(", t2_tnf_adj_sex_age_L[2], ", ", t2_tnf_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_crp_adj_sex_age_L[1], "(", t2_crp_adj_sex_age_L[2], ", ", t2_crp_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il12_adj_sex_age_L[1], "(", t2_il12_adj_sex_age_L[2], ", ", t2_il12_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_ifn_adj_sex_age_L[1], "(", t2_ifn_adj_sex_age_L[2], ", ", t2_ifn_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il4_adj_sex_age_L[1], "(", t2_il4_adj_sex_age_L[2], ", ", t2_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il5_adj_sex_age_L[1], "(", t2_il5_adj_sex_age_L[2], ", ", t2_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il13_adj_sex_age_L[1], "(", t2_il13_adj_sex_age_L[2], ", ", t2_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il17_adj_sex_age_L[1], "(", t2_il17_adj_sex_age_L[2], ", ", t2_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il21_adj_sex_age_L[1], "(", t2_il21_adj_sex_age_L[2], ", ", t2_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il10_adj_sex_age_L[1], "(", t2_il10_adj_sex_age_L[2], ", ", t2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_il2_adj_sex_age_L[1], "(", t2_il2_adj_sex_age_L[2], ", ", t2_il2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_gmc_adj_sex_age_L[1], "(", t2_gmc_adj_sex_age_L[2], ", ", t2_gmc_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_agp_adj_sex_age_L[1], "(", t2_agp_adj_sex_age_L[2], ", ", t2_agp_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t2_igf_adj_sex_age_L[1], "(", t2_igf_adj_sex_age_L[2], ", ", t2_igf_adj_sex_age_L[3], ")", sep=""))  

source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

t2_agp_adj_L <- round(t2_agp_adj_L, 2)
t2_crp_adj_L <- round(t2_crp_adj_L, 2)
t2_gmc_adj_L <- round(t2_gmc_adj_L, 2)
t2_ifn_adj_L <- round(t2_ifn_adj_L, 2)
t2_igf_adj_L <- round(t2_igf_adj_L, 2)
t2_il1_adj_L <- round(t2_il1_adj_L, 2)
t2_il10_adj_L <- round(t2_il10_adj_L, 2)
t2_il12_adj_L <- round(t2_il12_adj_L, 2)
t2_il13_adj_L <- round(t2_il13_adj_L, 2)
t2_il17_adj_L <- round(t2_il17_adj_L, 2)
t2_il2_adj_L <- round(t2_il2_adj_L, 2)
t2_il21_adj_L <- round(t2_il21_adj_L, 2)
t2_il4_adj_L <- round(t2_il4_adj_L, 2)
t2_il5_adj_L <- round(t2_il5_adj_L, 2)
t2_il6_adj_L <- round(t2_il6_adj_L, 2)
t2_tnf_adj_L <- round(t2_tnf_adj_L, 2)

adjtbl2 <- c(" ", " ", paste(t2_il1_adj_L[1], "(", t2_il1_adj_L[2], ", ", t2_il1_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il6_adj_L[1], "(", t2_il6_adj_L[2], ", ", t2_il6_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_tnf_adj_L[1], "(", t2_tnf_adj_L[2], ", ", t2_tnf_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_crp_adj_L[1], "(", t2_crp_adj_L[2], ", ", t2_crp_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il12_adj_L[1], "(", t2_il12_adj_L[2], ", ", t2_il12_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_ifn_adj_L[1], "(", t2_ifn_adj_L[2], ", ", t2_ifn_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il4_adj_L[1], "(", t2_il4_adj_L[2], ", ", t2_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il5_adj_L[1], "(", t2_il5_adj_L[2], ", ", t2_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il13_adj_L[1], "(", t2_il13_adj_L[2], ", ", t2_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il17_adj_L[1], "(", t2_il17_adj_L[2], ", ", t2_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il21_adj_L[1], "(", t2_il21_adj_L[2], ", ", t2_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il10_adj_L[1], "(", t2_il10_adj_L[2], ", ", t2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_il2_adj_L[1], "(", t2_il2_adj_L[2], ", ", t2_il2_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_gmc_adj_L[1], "(", t2_gmc_adj_L[2], ", ", t2_gmc_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_agp_adj_L[1], "(", t2_agp_adj_L[2], ", ", t2_agp_adj_L[3], ")", sep=""),
             " ", " ", paste(t2_igf_adj_L[1], "(", t2_igf_adj_L[2], ", ", t2_igf_adj_L[3], ")", sep="")) 

# Table 2: Effect of intervention on individual immune status and growth factor measurements at age 14 months
tbl2 <- data.table(
  "Outcome, Arm" = outcometbl2,
  "N" = Ntbl2, 
  "Absolute Mean" = c(),
  "Mean" = meantbl2, 
  "SD" = sdtbl2,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl2,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl2, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl2)

write.csv(tbl2, file=here('tables/mis09-table2.csv'))

