

source(here::here("0-config.R"))
load(c(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-immune.Rdata")))


sim_data <- function(mean=100, sd=10, n){
 res = data.frame(
  t2_ipf2a3 = rnorm(n, mean, sd),
  t2_23dinor = rnorm(n, mean, sd),
  t2_ipf2a6 = rnorm(n, mean, sd),
  t2_812iso = rnorm(n, mean, sd),
  
  t3_pre_saa = rnorm(n, mean, sd),
  t3_pre_cort = rnorm(n, mean, sd),
  t3_post_saa = rnorm(n, mean, sd),
  t3_post_cort = rnorm(n, mean, sd),
  
  t3_sys = rnorm(n, mean, sd),
  t3_dia = rnorm(n, mean, sd),
  t3_heart = rnorm(n, mean, sd),
  
  t3_nr3c1 = rnorm(n, mean, sd),
  t3_cpg12 = rnorm(n, mean, sd)
 )
 return(res)
}


set.seed(12345)
outcomes <- sim_data(n = nrow(d))
d <- cbind(d, outcomes)

saveRDS(d, file = here("replication objects/simulated_stress_dataset.rds"))



