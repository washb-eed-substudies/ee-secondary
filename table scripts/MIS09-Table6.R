rm(list=ls())
source(here::here("0-config.R"))

# Table 6: Effect of intervention on change in individual immune status and growth factor measurements between ages 14 and 28 months
tbl6 <- data.table(
  "Outcome, Arm" = c(paste("Ln deltaIL-1", expression(beta), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
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
                     paste("Ln deltaIGF-1 (", expression(mu), "g/L)", sep=""), "Control", "Nutrition + WSH"),
  "N" = c(), 
  "Absolute Mean" = c(),
  "Mean" = c(), 
  "SD" = c(),
  "Unadjusted difference: Intervention vs. Control (95% CI)" = c(),
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = c(), 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = c()
)
