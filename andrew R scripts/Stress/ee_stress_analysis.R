######################
###Load in packages
######################

source(here::here("0-config.R"))

######################
###Load in data
######################

table(lab$tr) #crosstab of numbers in each treatment

source(here::here("0-config.R"))
lab <- readRDS(here("replication objects/simulated_stress_dataset.rds"))

# re-order the treatment factor for convenience, dropping the arms not included in immune
lab$tr <- factor(lab$tr,levels=c("Control","Nutrition + WSH"))

t2_ipf2a3_A<-lab %>%
  subset(t2_ipf2a3!="NA") %>%
  summarize(t2_ipf2a3_N_overall=n(), mean=mean(t2_ipf2a3, na.rm = T),  sd=sd(t2_ipf2a3, na.rm = T))

t2_ipf2a3_A

t2_23dinor_A<-lab %>%
  subset(t2_23dinor!="NA") %>%
  summarize(t2_23dinor_N_overall=n(), mean=mean(t2_23dinor, na.rm = T),  sd=sd(t2_23dinor, na.rm = T))
t2_23dinor_A
