rm(list=ls())
source(here::here("0-config.R"))

# Table 2: Effect of intervention on individual immune status and growth factor measurements at age 14 months
tbl2 <- data.table(
  "Outcome, Arm" = c(paste("Ln IL-1", expression(beta), " (pg/ml)", sep=""), "Control", "Nutrition + WSH", 
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
                     paste("Ln IGF-1 (", expression(mu), "g/L)", sep=""), "Control", "Nutrition + WSH"),
  "N" = c(), 
  "Absolute Mean" = c(),
  "Mean" = c(), 
  "SD" = c(),
  "Unadjusted difference: Intervention vs. Control (95% CI)" = c(),
  "Age- and sex- adjusted difference: Intervention vs. Control (95% CI)" = c(), 
  "Fully adjusted difference: Intervention vs. Control (95% CI)" = c()
)
