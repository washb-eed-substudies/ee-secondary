




rm(list=ls())
source(here::here("0-config.R"))



#load immune outcomes
analysis_d<-read_dta("C:/Users/andre/Downloads/bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-immun-month.dta")




#' Review results section/Check opposing results (CRP @ Y1, Z-score ratio for Th1/Th2 at Y1 vs individual ratios)
#' 
#' 
#' Andrew. bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv
#' already has the raw cytokine numbers in it any biomarkers that don't have ln in the name e.g., igf_t2, crp_t2, agp_t2, il10_t2, or il10_t3. Hope that makes sense!
#' 
#' 
 outcomes <- c("igf_t2",          "igf_t3",          "crp_t2",         
"agp_t2",          "gmcsf_t2",        "ifng_t2",         "il10_t2",         "il12_t2",        
"il13_t2",         "il17_t2",         "il1_t2",          "il2_t2",          "il21_t2",        
"il4_t2",          "il5_t2",          "il6_t2",          "tnfa_t2",         "gmcsf_t3",       
"ifng_t3",         "il10_t3",         "il12_t3",         "il13_t3",         "il17_t3",        
"il1_t3",          "il2_t3",          "il21_t3",         "il4_t3",          "il5_t3",         
"il6_t3",          "tnfa_t3")



imm <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-anthro-diar-ee-med-plasma-blind-tr-enrol-covariates-lab.csv"))
head(imm)

#CRP
ggplot(imm) + geom_density(aes(x=crp_t2))
ggplot(imm) + geom_density(aes(x=log(crp_t2)))



colnames(imm)


# We assessed individual Th1-type to Th2-type cytokine ratios. At Y1, the combined interventions reduced the IL-12/IL-4 ratio 
# (-0.18, CI -0.31, -0.06), the IFN-??/IL-4 ratio (-0.11, CI -0.2, -0.01), the IL-12/IL-5 ratio (-0.27, CI -0.41, -0.13),
# the IFN-??/IL-5 ratio (-0.19, CI -0.32, -0.06), and the IL-12/IL-13 ratio (-0.22, CI -0.39, -0.05; Table 3).
# However, when examining the total combined ratio of Th1 cytokines (IL-12 and IFN-y) to Th2 cytokines (IL-4, IL-5, and IL-13), 
# children in the combined intervention arm exhibited a higher Th1/Th2 ratio compared to controls (1.27, CI 0.11, 2.44; Table 3),
# suggesting that exposure to the intervention skewed the balance of Th1 and Th2 responses in favor of a Th1 response.


ggplot(imm) + geom_density(aes(x=z_il12_t2))
ggplot(imm) + geom_density(aes(x=z_il12_t2))
ggplot(imm) + geom_density(aes(x=scale(il12_t2)))

#log transforming before Z-scoring
ggplot(imm) + geom_density(aes(x=scale(log(il12_t2))))


#Combined Z-scores have influential outliers
summary(imm$z_il1_t2)
summary(imm$z_il6_t2)
summary(imm$z_tnf_t2)
summary(imm$z_il10_t2)
#gen t2_ratio_pro_il10 = (z_il1_t2 + z_il6_t2 + z_tnf_t2) / z_il10_t2
ggplot(imm) + geom_density(aes(x=t2_ratio_pro_il10))
summary(imm$t2_ratio_pro_il10)

ggplot(imm) + geom_point(aes(x=z_il10_t2, y=t2_ratio_pro_il10))

ggplot(imm) + geom_density(aes(x=t2_ratio_pro_il10))


ggplot(imm) + geom_density(aes(x=scale(il1_t2/il10_t2)))

summary(imm$il1_t2)
summary(imm$ln_il1_t2)

ggplot(imm) + geom_density(aes(x=scale(il1_t2/il10_t2) + scale(il6_t2/il10_t2) + scale(tnfa_t2/il10_t2)))


summary(imm$il10_t2)
ggplot(imm) + geom_density(aes(x=il10_t2))
ggplot(imm) + geom_density(aes(x=log(il10_t2)))
ggplot(imm) + geom_density(aes(x=scale(il10_t2, center=F)))

ggplot(imm) + geom_density(aes(x=scale(il1_t2/il10_t2) + scale(il6_t2/il10_t2) + scale(tnfa_t2/il10_t2)))
ggplot(imm) + geom_density(aes(x=scale(il1_t2/il10_t2)))
ggplot(imm) + geom_density(aes(x=scale(il6_t2/il10_t2)))
ggplot(imm) + geom_density(aes(x=scale(tnfa_t2/il10_t2)))


ggplot(imm) + geom_point(aes(x=z_il10_t2, y=scale(il1_t2/il10_t2)))
ggplot(imm) + geom_point(aes(x=z_il10_t2, y=scale(il6_t2/il10_t2)))
ggplot(imm) + geom_point(aes(x=z_il10_t2, y=scale(tnfa_t2/il10_t2)))



Th1/Th2 


#Create Z-scores
scale(A, center = TRUE, scale = TRUE)










