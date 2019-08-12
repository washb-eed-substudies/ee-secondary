rm(list=ls())
source(here::here("0-config.R"))

source(here('audrie R scripts/immune/bangladesh-immune-adj-age-sex.R'))
source(here('audrie R scripts/immune/bangladesh-immune-subgroup.R'))
source(here('audrie R scripts/immune/bangladesh-immune-ages-unadjusted-glm.R'))
source(here('audrie R scripts/immune/bangladesh-immune-adj.R'))

# Table 1: Enrollment characteristics by intervention group
tbl1 <- data.table(
  "No. of compounds:" = c(" ", "Maternal", "Age(years)", "Years of education", 
                          "Paternal", "Years of education", "Works in agriculture", 
                          "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
                          "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
                          "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
                          "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
                          "Owned a child potty",
                          "Human feces observed in the", "House", "Child's play area",
                          "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
                          "Nutrition", "Household is food secure"),
  paste("Control (N=", len(), ")", sep="") = c("%/mean", ),
  paste("N + WSH (N=", len(), ")", sep="") = c("%/mean", )
)
