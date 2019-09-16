rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

outcometbl5 <- c(paste("Ln IL-1", expression(beta), "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-6/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln TNF-", expression(alpha), "/IL-10", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-10", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-10", sep=""), "Control", "Nutrition + WSH", 
                 "Ln IL-4/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-5/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-13/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-17A/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-21/IL-10", "Control", "Nutrition + WSH", 
                 "Ln IL-2/IL-10", "Control", "Nutrition + WSH", 
                 "Ln GM-CSF/IL-10", "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-4", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-4", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-5", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-5", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-13", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-13", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-17A", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-17A", sep=""), "Control", "Nutrition + WSH",
                 "Ln IL-12/IL-21", "Control", "Nutrition + WSH", 
                 paste("Ln IFN-", expression(gamma), "/IL-21", sep=""), "Control", "Nutrition + WSH",
                 "Ln Pro-inflammatory cytokines*/IL-10", "Control", "Nutrition + WSH",
                 "Ln Th1**/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th2***/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th17****/IL-10", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th2***", "Control", "Nutrition + WSH", 
                 "Ln Th1**/Th17****", "Control", "Nutrition + WSH")

Ntbl5 <- c(" ", as.character(t3_ratio_il1_il10_N_tr$t3_ratio_il1_il10_N_tr[1]), as.character(t3_ratio_il1_il10_N_tr$t3_ratio_il1_il10_N_tr[2]), 
           " ", as.character(t3_ratio_il6_il10_N_tr$t3_ratio_il6_il10_N_tr[1]), as.character(t3_ratio_il6_il10_N_tr$t3_ratio_il6_il10_N_tr[2]), 
           " ", as.character(t3_ratio_tnf_il10_N_tr$t3_ratio_tnf_il10_N_tr[1]), as.character(t3_ratio_tnf_il10_N_tr$t3_ratio_tnf_il10_N_tr[2]),
           " ", as.character(t3_ratio_il12_il10_N_tr$t3_ratio_il12_il10_N_tr[1]), as.character(t3_ratio_il12_il10_N_tr$t3_ratio_il12_il10_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il10_N_tr$t3_ratio_ifn_il10_N_tr[1]), as.character(t3_ratio_ifn_il10_N_tr$t3_ratio_ifn_il10_N_tr[2]),
           " ", as.character(t3_ratio_il4_il10_N_tr$t3_ratio_il4_il10_N_tr[1]), as.character(t3_ratio_il4_il10_N_tr$t3_ratio_il4_il10_N_tr[2]),
           " ", as.character(t3_ratio_il5_il10_N_tr$t3_ratio_il5_il10_N_tr[1]), as.character(t3_ratio_il5_il10_N_tr$t3_ratio_il5_il10_N_tr[2]),
           " ", as.character(t3_ratio_il13_il10_N_tr$t3_ratio_il13_il10_N_tr[1]), as.character(t3_ratio_il13_il10_N_tr$t3_ratio_il13_il10_N_tr[2]),
           " ", as.character(t3_ratio_il17_il10_N_tr$t3_ratio_il17_il10_N_tr[1]), as.character(t3_ratio_il17_il10_N_tr$t3_ratio_il17_il10_N_tr[2]),
           " ", as.character(t3_ratio_il21_il10_N_tr$t3_ratio_il21_il10_N_tr[1]), as.character(t3_ratio_il21_il10_N_tr$t3_ratio_il21_il10_N_tr[2]),
           " ", as.character(t3_ratio_il2_il10_N_tr$t3_ratio_il2_il10_N_tr[1]), as.character(t3_ratio_il2_il10_N_tr$t3_ratio_il2_il10_N_tr[2]),
           " ", as.character(t3_ratio_gmc_il10_N_tr$t3_ratio_gmc_il10_N_tr[1]), as.character(t3_ratio_gmc_il10_N_tr$t3_ratio_gmc_il10_N_tr[2]),
           " ", as.character(t3_ratio_il12_il4_N_tr$t3_ratio_il12_il4_N_tr[1]), as.character(t3_ratio_il12_il4_N_tr$t3_ratio_il12_il4_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il4_N_tr$t3_ratio_ifn_il4_N_tr[1]), as.character(t3_ratio_ifn_il4_N_tr$t3_ratio_ifn_il4_N_tr[2]),
           " ", as.character(t3_ratio_il12_il5_N_tr$t3_ratio_il12_il5_N_tr[1]), as.character(t3_ratio_il12_il5_N_tr$t3_ratio_il12_il5_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il5_N_tr$t3_ratio_ifn_il5_N_tr[1]), as.character(t3_ratio_ifn_il5_N_tr$t3_ratio_ifn_il5_N_tr[2]),
           " ", as.character(t3_ratio_il12_il13_N_tr$t3_ratio_il12_il13_N_tr[1]), as.character(t3_ratio_il12_il13_N_tr$t3_ratio_il12_il13_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il13_N_tr$t3_ratio_ifn_il13_N_tr[1]), as.character(t3_ratio_ifn_il13_N_tr$t3_ratio_ifn_il13_N_tr[2]),
           " ", as.character(t3_ratio_il12_il17_N_tr$t3_ratio_il12_il17_N_tr[1]), as.character(t3_ratio_il12_il17_N_tr$t3_ratio_il12_il17_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il17_N_tr$t3_ratio_ifn_il17_N_tr[1]), as.character(t3_ratio_ifn_il17_N_tr$t3_ratio_ifn_il17_N_tr[2]),
           " ", as.character(t3_ratio_il12_il21_N_tr$t3_ratio_il12_il21_N_tr[1]), as.character(t3_ratio_il12_il21_N_tr$t3_ratio_il12_il21_N_tr[2]),
           " ", as.character(t3_ratio_ifn_il21_N_tr$t3_ratio_ifn_il21_N_tr[1]), as.character(t3_ratio_ifn_il21_N_tr$t3_ratio_ifn_il21_N_tr[2]),
           " ", as.character(t3_ratio_pro_il10_N_tr$t3_ratio_pro_il10_N_tr[1]), as.character(t3_ratio_pro_il10_N_tr$t3_ratio_pro_il10_N_tr[2]),
           " ", as.character(t3_ratio_th1_il10_N_tr$t3_ratio_th1_il10_N_tr[1]), as.character(t3_ratio_th1_il10_N_tr$t3_ratio_th1_il10_N_tr[2]),
           " ", as.character(t3_ratio_th2_il10_N_tr$t3_ratio_th2_il10_N_tr[1]), as.character(t3_ratio_th2_il10_N_tr$t3_ratio_th2_il10_N_tr[2]),
           " ", as.character(t3_ratio_th17_il10_N_tr$t3_ratio_th17_il10_N_tr[1]), as.character(t3_ratio_th17_il10_N_tr$t3_ratio_th17_il10_N_tr[2]),
           " ", as.character(t3_ratio_th1_th2_N_tr$t3_ratio_th1_th2_N_tr[1]), as.character(t3_ratio_th1_th2_N_tr$t3_ratio_th1_th2_N_tr[2]),
           " ", as.character(t3_ratio_th1_th17_N_tr$t3_ratio_th1_th17_N_tr[1]), as.character(t3_ratio_th1_th17_N_tr$t3_ratio_th1_th17_N_tr[2])
           )

meantbl5 <- c(" ", as.character(round(t3_ratio_il1_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il1_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t3_ratio_il6_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il6_il10_N_tr$mean[2], 2)), 
              " ", as.character(round(t3_ratio_tnf_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_tnf_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il4_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il4_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il5_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il5_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il13_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il13_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il17_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il17_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il21_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il21_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il2_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_il2_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_gmc_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_gmc_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il4_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il4_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il4_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il4_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il5_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il5_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il5_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il5_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il13_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il13_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il13_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il13_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il17_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il17_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il17_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il17_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_il12_il21_N_tr$mean[1], 2)), as.character(round(t3_ratio_il12_il21_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_ifn_il21_N_tr$mean[1], 2)), as.character(round(t3_ratio_ifn_il21_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_pro_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_pro_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th2_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th2_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th17_il10_N_tr$mean[1], 2)), as.character(round(t3_ratio_th17_il10_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_th2_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_th2_N_tr$mean[2], 2)),
              " ", as.character(round(t3_ratio_th1_th17_N_tr$mean[1], 2)), as.character(round(t3_ratio_th1_th17_N_tr$mean[2], 2)))

sdtbl5 <- c(" ", as.character(round(t3_ratio_il1_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il1_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t3_ratio_il6_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il6_il10_N_tr$sd[2], 2)), 
            " ", as.character(round(t3_ratio_tnf_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_tnf_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il4_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il4_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il5_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il5_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il13_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il13_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il17_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il17_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il21_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il21_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il2_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_il2_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_gmc_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_gmc_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il4_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il4_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il4_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il4_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il5_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il5_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il5_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il5_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il13_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il13_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il13_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il13_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il17_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il17_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il17_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il17_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_il12_il21_N_tr$sd[1], 2)), as.character(round(t3_ratio_il12_il21_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_ifn_il21_N_tr$sd[1], 2)), as.character(round(t3_ratio_ifn_il21_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_pro_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_pro_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th2_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th2_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th17_il10_N_tr$sd[1], 2)), as.character(round(t3_ratio_th17_il10_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_th2_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_th2_N_tr$sd[2], 2)),
            " ", as.character(round(t3_ratio_th1_th17_N_tr$sd[1], 2)), as.character(round(t3_ratio_th1_th17_N_tr$sd[2], 2)))

t3_ratio_il1_il10_unadj_L <- round(t3_ratio_il1_il10_unadj_L, 2)
t3_ratio_il6_il10_unadj_L <- round(t3_ratio_il6_il10_unadj_L, 2)
t3_ratio_tnf_il10_unadj_L <- round(t3_ratio_tnf_il10_unadj_L, 2)
t3_ratio_il12_il10_unadj_L <- round(t3_ratio_il12_il10_unadj_L, 2)
t3_ratio_ifn_il10_unadj_L <- round(t3_ratio_ifn_il10_unadj_L, 2)
t3_ratio_il4_il10_unadj_L <- round(t3_ratio_il4_il10_unadj_L, 2)
t3_ratio_il5_il10_unadj_L <- round(t3_ratio_il5_il10_unadj_L, 2)
t3_ratio_il13_il10_unadj_L <- round(t3_ratio_il13_il10_unadj_L, 2)
t3_ratio_il17_il10_unadj_L <- round(t3_ratio_il17_il10_unadj_L, 2)
t3_ratio_il21_il10_unadj_L <- round(t3_ratio_il21_il10_unadj_L, 2)
t3_ratio_il2_il10_unadj_L <- round(t3_ratio_il2_il10_unadj_L, 2)
t3_ratio_gmc_il10_unadj_L <- round(t3_ratio_gmc_il10_unadj_L, 2)
t3_ratio_il12_il4_unadj_L <- round(t3_ratio_il12_il4_unadj_L, 2)
t3_ratio_ifn_il4_unadj_L <- round(t3_ratio_ifn_il4_unadj_L, 2)
t3_ratio_il12_il5_unadj_L <- round(t3_ratio_il12_il5_unadj_L, 2)
t3_ratio_ifn_il5_unadj_L <- round(t3_ratio_ifn_il5_unadj_L, 2)
t3_ratio_il12_il13_unadj_L <- round(t3_ratio_il12_il13_unadj_L, 2)
t3_ratio_ifn_il13_unadj_L <- round(t3_ratio_ifn_il13_unadj_L, 2)
t3_ratio_il12_il17_unadj_L <- round(t3_ratio_il12_il17_unadj_L, 2)
t3_ratio_ifn_il17_unadj_L <- round(t3_ratio_ifn_il17_unadj_L, 2)
t3_ratio_il12_il21_unadj_L <- round(t3_ratio_il12_il21_unadj_L, 2)
t3_ratio_ifn_il21_unadj_L <- round(t3_ratio_ifn_il21_unadj_L, 2)
t3_ratio_pro_il10_unadj_L <- round(t3_ratio_pro_il10_unadj_L, 2)
t3_ratio_th1_il10_unadj_L <- round(t3_ratio_th1_il10_unadj_L, 2)
t3_ratio_th2_il10_unadj_L <- round(t3_ratio_th2_il10_unadj_L, 2)
t3_ratio_th17_il10_unadj_L <- round(t3_ratio_th17_il10_unadj_L, 2)
t3_ratio_th1_th2_unadj_L <- round(t3_ratio_th1_th2_unadj_L, 2)
t3_ratio_th1_th17_unadj_L <- round(t3_ratio_th1_th17_unadj_L, 2)

unadjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_unadj_L[1], " (", t3_ratio_il1_il10_unadj_L[2], ", ", t3_ratio_il1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il6_il10_unadj_L[1], " (", t3_ratio_il6_il10_unadj_L[2], ", ", t3_ratio_il6_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_tnf_il10_unadj_L[1], " (", t3_ratio_tnf_il10_unadj_L[2], ", ", t3_ratio_tnf_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il10_unadj_L[1], " (", t3_ratio_il12_il10_unadj_L[2], ", ", t3_ratio_il12_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il10_unadj_L[1], " (", t3_ratio_ifn_il10_unadj_L[2], ", ", t3_ratio_ifn_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il4_il10_unadj_L[1], " (", t3_ratio_il4_il10_unadj_L[2], ", ", t3_ratio_il4_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il5_il10_unadj_L[1], " (", t3_ratio_il5_il10_unadj_L[2], ", ", t3_ratio_il5_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il13_il10_unadj_L[1], " (", t3_ratio_il13_il10_unadj_L[2], ", ", t3_ratio_il13_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il17_il10_unadj_L[1], " (", t3_ratio_il17_il10_unadj_L[2], ", ", t3_ratio_il17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il21_il10_unadj_L[1], " (", t3_ratio_il21_il10_unadj_L[2], ", ", t3_ratio_il21_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il2_il10_unadj_L[1], " (", t3_ratio_il2_il10_unadj_L[2], ", ", t3_ratio_il2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_gmc_il10_unadj_L[1], " (", t3_ratio_gmc_il10_unadj_L[2], ", ", t3_ratio_gmc_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il4_unadj_L[1], " (", t3_ratio_il12_il4_unadj_L[2], ", ", t3_ratio_il12_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il4_unadj_L[1], " (", t3_ratio_ifn_il4_unadj_L[2], ", ", t3_ratio_ifn_il4_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il5_unadj_L[1], " (", t3_ratio_il12_il5_unadj_L[2], ", ", t3_ratio_il12_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il5_unadj_L[1], " (", t3_ratio_ifn_il5_unadj_L[2], ", ", t3_ratio_ifn_il5_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il13_unadj_L[1], " (", t3_ratio_il12_il13_unadj_L[2], ", ", t3_ratio_il12_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il13_unadj_L[1], " (", t3_ratio_ifn_il13_unadj_L[2], ", ", t3_ratio_ifn_il13_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il17_unadj_L[1], " (", t3_ratio_il12_il17_unadj_L[2], ", ", t3_ratio_il12_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il17_unadj_L[1], " (", t3_ratio_ifn_il17_unadj_L[2], ", ", t3_ratio_ifn_il17_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il21_unadj_L[1], " (", t3_ratio_il12_il21_unadj_L[2], ", ", t3_ratio_il12_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il21_unadj_L[1], " (", t3_ratio_ifn_il21_unadj_L[2], ", ", t3_ratio_ifn_il21_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_pro_il10_unadj_L[1], " (", t3_ratio_pro_il10_unadj_L[2], ", ", t3_ratio_pro_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_il10_unadj_L[1], " (", t3_ratio_th1_il10_unadj_L[2], ", ", t3_ratio_th1_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th2_il10_unadj_L[1], " (", t3_ratio_th2_il10_unadj_L[2], ", ", t3_ratio_th2_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th17_il10_unadj_L[1], " (", t3_ratio_th17_il10_unadj_L[2], ", ", t3_ratio_th17_il10_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th2_unadj_L[1], " (", t3_ratio_th1_th2_unadj_L[2], ", ", t3_ratio_th1_th2_unadj_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th17_unadj_L[1], " (", t3_ratio_th1_th17_unadj_L[2], ", ", t3_ratio_th1_th17_unadj_L[3], ")", sep="")
)


t3_ratio_il1_il10_adj_sex_age_L <- round(t3_ratio_il1_il10_adj_sex_age_L, 2)
t3_ratio_il6_il10_adj_sex_age_L <- round(t3_ratio_il6_il10_adj_sex_age_L, 2)
t3_ratio_tnf_il10_adj_sex_age_L <- round(t3_ratio_tnf_il10_adj_sex_age_L, 2)
t3_ratio_il12_il10_adj_sex_age_L <- round(t3_ratio_il12_il10_adj_sex_age_L, 2)
t3_ratio_ifn_il10_adj_sex_age_L <- round(t3_ratio_ifn_il10_adj_sex_age_L, 2)
t3_ratio_il4_il10_adj_sex_age_L <- round(t3_ratio_il4_il10_adj_sex_age_L, 2)
t3_ratio_il5_il10_adj_sex_age_L <- round(t3_ratio_il5_il10_adj_sex_age_L, 2)
t3_ratio_il13_il10_adj_sex_age_L <- round(t3_ratio_il13_il10_adj_sex_age_L, 2)
t3_ratio_il17_il10_adj_sex_age_L <- round(t3_ratio_il17_il10_adj_sex_age_L, 2)
t3_ratio_il21_il10_adj_sex_age_L <- round(t3_ratio_il21_il10_adj_sex_age_L, 2)
t3_ratio_il2_il10_adj_sex_age_L <- round(t3_ratio_il2_il10_adj_sex_age_L, 2)
t3_ratio_gmc_il10_adj_sex_age_L <- round(t3_ratio_gmc_il10_adj_sex_age_L, 2)
t3_ratio_il12_il4_adj_sex_age_L <- round(t3_ratio_il12_il4_adj_sex_age_L, 2)
t3_ratio_ifn_il4_adj_sex_age_L <- round(t3_ratio_ifn_il4_adj_sex_age_L, 2)
t3_ratio_il12_il5_adj_sex_age_L <- round(t3_ratio_il12_il5_adj_sex_age_L, 2)
t3_ratio_ifn_il5_adj_sex_age_L <- round(t3_ratio_ifn_il5_adj_sex_age_L, 2)
t3_ratio_il12_il13_adj_sex_age_L <- round(t3_ratio_il12_il13_adj_sex_age_L, 2)
t3_ratio_ifn_il13_adj_sex_age_L <- round(t3_ratio_ifn_il13_adj_sex_age_L, 2)
t3_ratio_il12_il17_adj_sex_age_L <- round(t3_ratio_il12_il17_adj_sex_age_L, 2)
t3_ratio_ifn_il17_adj_sex_age_L <- round(t3_ratio_ifn_il17_adj_sex_age_L, 2)
t3_ratio_il12_il21_adj_sex_age_L <- round(t3_ratio_il12_il21_adj_sex_age_L, 2)
t3_ratio_ifn_il21_adj_sex_age_L <- round(t3_ratio_ifn_il21_adj_sex_age_L, 2)
t3_ratio_pro_il10_adj_sex_age_L <- round(t3_ratio_pro_il10_adj_sex_age_L, 2)
t3_ratio_th1_il10_adj_sex_age_L <- round(t3_ratio_th1_il10_adj_sex_age_L, 2)
t3_ratio_th2_il10_adj_sex_age_L <- round(t3_ratio_th2_il10_adj_sex_age_L, 2)
t3_ratio_th17_il10_adj_sex_age_L <- round(t3_ratio_th17_il10_adj_sex_age_L, 2)
t3_ratio_th1_th2_adj_sex_age_L <- round(t3_ratio_th1_th2_adj_sex_age_L, 2)
t3_ratio_th1_th17_adj_sex_age_L <- round(t3_ratio_th1_th17_adj_sex_age_L, 2)

asadjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_adj_sex_age_L[1], " (", t3_ratio_il1_il10_adj_sex_age_L[2], ", ", t3_ratio_il1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il6_il10_adj_sex_age_L[1], " (", t3_ratio_il6_il10_adj_sex_age_L[2], ", ", t3_ratio_il6_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_tnf_il10_adj_sex_age_L[1], " (", t3_ratio_tnf_il10_adj_sex_age_L[2], ", ", t3_ratio_tnf_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il10_adj_sex_age_L[1], " (", t3_ratio_il12_il10_adj_sex_age_L[2], ", ", t3_ratio_il12_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il10_adj_sex_age_L[1], " (", t3_ratio_ifn_il10_adj_sex_age_L[2], ", ", t3_ratio_ifn_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il4_il10_adj_sex_age_L[1], " (", t3_ratio_il4_il10_adj_sex_age_L[2], ", ", t3_ratio_il4_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il5_il10_adj_sex_age_L[1], " (", t3_ratio_il5_il10_adj_sex_age_L[2], ", ", t3_ratio_il5_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il13_il10_adj_sex_age_L[1], " (", t3_ratio_il13_il10_adj_sex_age_L[2], ", ", t3_ratio_il13_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il17_il10_adj_sex_age_L[1], " (", t3_ratio_il17_il10_adj_sex_age_L[2], ", ", t3_ratio_il17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il21_il10_adj_sex_age_L[1], " (", t3_ratio_il21_il10_adj_sex_age_L[2], ", ", t3_ratio_il21_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il2_il10_adj_sex_age_L[1], " (", t3_ratio_il2_il10_adj_sex_age_L[2], ", ", t3_ratio_il2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_gmc_il10_adj_sex_age_L[1], " (", t3_ratio_gmc_il10_adj_sex_age_L[2], ", ", t3_ratio_gmc_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il4_adj_sex_age_L[1], " (", t3_ratio_il12_il4_adj_sex_age_L[2], ", ", t3_ratio_il12_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il4_adj_sex_age_L[1], " (", t3_ratio_ifn_il4_adj_sex_age_L[2], ", ", t3_ratio_ifn_il4_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il5_adj_sex_age_L[1], " (", t3_ratio_il12_il5_adj_sex_age_L[2], ", ", t3_ratio_il12_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il5_adj_sex_age_L[1], " (", t3_ratio_ifn_il5_adj_sex_age_L[2], ", ", t3_ratio_ifn_il5_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il13_adj_sex_age_L[1], " (", t3_ratio_il12_il13_adj_sex_age_L[2], ", ", t3_ratio_il12_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il13_adj_sex_age_L[1], " (", t3_ratio_ifn_il13_adj_sex_age_L[2], ", ", t3_ratio_ifn_il13_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il17_adj_sex_age_L[1], " (", t3_ratio_il12_il17_adj_sex_age_L[2], ", ", t3_ratio_il12_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il17_adj_sex_age_L[1], " (", t3_ratio_ifn_il17_adj_sex_age_L[2], ", ", t3_ratio_ifn_il17_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_il12_il21_adj_sex_age_L[1], " (", t3_ratio_il12_il21_adj_sex_age_L[2], ", ", t3_ratio_il12_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_ifn_il21_adj_sex_age_L[1], " (", t3_ratio_ifn_il21_adj_sex_age_L[2], ", ", t3_ratio_ifn_il21_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_pro_il10_adj_sex_age_L[1], " (", t3_ratio_pro_il10_adj_sex_age_L[2], ", ", t3_ratio_pro_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_il10_adj_sex_age_L[1], " (", t3_ratio_th1_il10_adj_sex_age_L[2], ", ", t3_ratio_th1_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th2_il10_adj_sex_age_L[1], " (", t3_ratio_th2_il10_adj_sex_age_L[2], ", ", t3_ratio_th2_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th17_il10_adj_sex_age_L[1], " (", t3_ratio_th17_il10_adj_sex_age_L[2], ", ", t3_ratio_th17_il10_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th2_adj_sex_age_L[1], " (", t3_ratio_th1_th2_adj_sex_age_L[2], ", ", t3_ratio_th1_th2_adj_sex_age_L[3], ")", sep=""),
               " ", " ", paste(t3_ratio_th1_th17_adj_sex_age_L[1], " (", t3_ratio_th1_th17_adj_sex_age_L[2], ", ", t3_ratio_th1_th17_adj_sex_age_L[3], ")", sep=""))

t3_ratio_il1_il10_adj_L <- round(t3_ratio_il1_il10_adj_L, 2)
t3_ratio_il6_il10_adj_L <- round(t3_ratio_il6_il10_adj_L, 2)
t3_ratio_tnf_il10_adj_L <- round(t3_ratio_tnf_il10_adj_L, 2)
t3_ratio_il12_il10_adj_L <- round(t3_ratio_il12_il10_adj_L, 2)
t3_ratio_ifn_il10_adj_L <- round(t3_ratio_ifn_il10_adj_L, 2)
t3_ratio_il4_il10_adj_L <- round(t3_ratio_il4_il10_adj_L, 2)
t3_ratio_il5_il10_adj_L <- round(t3_ratio_il5_il10_adj_L, 2)
t3_ratio_il13_il10_adj_L <- round(t3_ratio_il13_il10_adj_L, 2)
t3_ratio_il17_il10_adj_L <- round(t3_ratio_il17_il10_adj_L, 2)
t3_ratio_il21_il10_adj_L <- round(t3_ratio_il21_il10_adj_L, 2)
t3_ratio_il2_il10_adj_L <- round(t3_ratio_il2_il10_adj_L, 2)
t3_ratio_gmc_il10_adj_L <- round(t3_ratio_gmc_il10_adj_L, 2)
t3_ratio_il12_il4_adj_L <- round(t3_ratio_il12_il4_adj_L, 2)
t3_ratio_ifn_il4_adj_L <- round(t3_ratio_ifn_il4_adj_L, 2)
t3_ratio_il12_il5_adj_L <- round(t3_ratio_il12_il5_adj_L, 2)
t3_ratio_ifn_il5_adj_L <- round(t3_ratio_ifn_il5_adj_L, 2)
t3_ratio_il12_il13_adj_L <- round(t3_ratio_il12_il13_adj_L, 2)
t3_ratio_ifn_il13_adj_L <- round(t3_ratio_ifn_il13_adj_L, 2)
t3_ratio_il12_il17_adj_L <- round(t3_ratio_il12_il17_adj_L, 2)
t3_ratio_ifn_il17_adj_L <- round(t3_ratio_ifn_il17_adj_L, 2)
t3_ratio_il12_il21_adj_L <- round(t3_ratio_il12_il21_adj_L, 2)
t3_ratio_ifn_il21_adj_L <- round(t3_ratio_ifn_il21_adj_L, 2)
t3_ratio_pro_il10_adj_L <- round(t3_ratio_pro_il10_adj_L, 2)
t3_ratio_th1_il10_adj_L <- round(t3_ratio_th1_il10_adj_L, 2)
t3_ratio_th2_il10_adj_L <- round(t3_ratio_th2_il10_adj_L, 2)
t3_ratio_th17_il10_adj_L <- round(t3_ratio_th17_il10_adj_L, 2)
t3_ratio_th1_th2_adj_L <- round(t3_ratio_th1_th2_adj_L, 2)
t3_ratio_th1_th17_adj_L <- round(t3_ratio_th1_th17_adj_L, 2)

adjtbl5 <- c(" ", " ", paste(t3_ratio_il1_il10_adj_L[1], " (", t3_ratio_il1_il10_adj_L[2], ", ", t3_ratio_il1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il6_il10_adj_L[1], " (", t3_ratio_il6_il10_adj_L[2], ", ", t3_ratio_il6_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_tnf_il10_adj_L[1], " (", t3_ratio_tnf_il10_adj_L[2], ", ", t3_ratio_tnf_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il10_adj_L[1], " (", t3_ratio_il12_il10_adj_L[2], ", ", t3_ratio_il12_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il10_adj_L[1], " (", t3_ratio_ifn_il10_adj_L[2], ", ", t3_ratio_ifn_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il4_il10_adj_L[1], " (", t3_ratio_il4_il10_adj_L[2], ", ", t3_ratio_il4_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il5_il10_adj_L[1], " (", t3_ratio_il5_il10_adj_L[2], ", ", t3_ratio_il5_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il13_il10_adj_L[1], " (", t3_ratio_il13_il10_adj_L[2], ", ", t3_ratio_il13_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il17_il10_adj_L[1], " (", t3_ratio_il17_il10_adj_L[2], ", ", t3_ratio_il17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il21_il10_adj_L[1], " (", t3_ratio_il21_il10_adj_L[2], ", ", t3_ratio_il21_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il2_il10_adj_L[1], " (", t3_ratio_il2_il10_adj_L[2], ", ", t3_ratio_il2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_gmc_il10_adj_L[1], " (", t3_ratio_gmc_il10_adj_L[2], ", ", t3_ratio_gmc_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il4_adj_L[1], " (", t3_ratio_il12_il4_adj_L[2], ", ", t3_ratio_il12_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il4_adj_L[1], " (", t3_ratio_ifn_il4_adj_L[2], ", ", t3_ratio_ifn_il4_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il5_adj_L[1], " (", t3_ratio_il12_il5_adj_L[2], ", ", t3_ratio_il12_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il5_adj_L[1], " (", t3_ratio_ifn_il5_adj_L[2], ", ", t3_ratio_ifn_il5_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il13_adj_L[1], " (", t3_ratio_il12_il13_adj_L[2], ", ", t3_ratio_il12_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il13_adj_L[1], " (", t3_ratio_ifn_il13_adj_L[2], ", ", t3_ratio_ifn_il13_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il17_adj_L[1], " (", t3_ratio_il12_il17_adj_L[2], ", ", t3_ratio_il12_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il17_adj_L[1], " (", t3_ratio_ifn_il17_adj_L[2], ", ", t3_ratio_ifn_il17_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_il12_il21_adj_L[1], " (", t3_ratio_il12_il21_adj_L[2], ", ", t3_ratio_il12_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_ifn_il21_adj_L[1], " (", t3_ratio_ifn_il21_adj_L[2], ", ", t3_ratio_ifn_il21_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_pro_il10_adj_L[1], " (", t3_ratio_pro_il10_adj_L[2], ", ", t3_ratio_pro_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_il10_adj_L[1], " (", t3_ratio_th1_il10_adj_L[2], ", ", t3_ratio_th1_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th2_il10_adj_L[1], " (", t3_ratio_th2_il10_adj_L[2], ", ", t3_ratio_th2_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th17_il10_adj_L[1], " (", t3_ratio_th17_il10_adj_L[2], ", ", t3_ratio_th17_il10_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_th2_adj_L[1], " (", t3_ratio_th1_th2_adj_L[2], ", ", t3_ratio_th1_th2_adj_L[3], ")", sep=""),
             " ", " ", paste(t3_ratio_th1_th17_adj_L[1], " (", t3_ratio_th1_th17_adj_L[2], ", ", t3_ratio_th1_th17_adj_L[3], ")", sep=""))

# Table 5: Effect of intervention on cytokine ratios at age 28 months
tbl5 <- data.table(
  "Outcome, Arm" = outcometbl5,
  "N" = Ntbl5, 
  "Mean" = meantbl5, 
  "SD" = sdtbl5,
  "Unadjusted difference: Intervention vs. Control (95% CI)" = unadjtbl5,
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = asadjtbl5, 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = adjtbl5
)

write.csv(tbl5, file=here('tables/miso9-table5.csv'))

