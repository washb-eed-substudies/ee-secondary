#### Adjust all pvalues with BH procedure ####
rm(list=ls())

source(here::here("0-config.R"))

# load all results
H1_res <- readRDS(here('ipv-stress/results/unadjusted/H1_res.RDS'))
H2_res <- readRDS(here('ipv-stress/results/unadjusted/H2_res.RDS'))
H3_res <- readRDS(here('ipv-stress/results/unadjusted/H3_res.RDS'))

H1_adj_res <- readRDS(here('ipv-stress/results/adjusted/H1_adj_res.RDS'))
H2_adj_res <- readRDS(here('ipv-stress/results/adjusted/H2_adj_res.RDS'))
H3_adj_res <- readRDS(here('ipv-stress/results/adjusted/H3_adj_res.RDS'))

H1_res$H = 1
H2_res$H = 2
H3_res$H = 3

H1_adj_res$H = 1
H2_adj_res$H = 2
H3_adj_res$H = 3

full_res <- rbind(H1_res, H2_res, H3_res)
full_adj_res <- rbind(H1_adj_res, H2_adj_res, H3_adj_res)

full_res <- full_res %>% group_by(Y) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

full_adj_res <- full_adj_res %>% group_by(Y) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

saveRDS(full_res %>% filter(H==1) %>% select(-H), here("ipv-stress/results/unadjusted/H1_res.RDS"))
saveRDS(full_res %>% filter(H==2) %>% select(-H), here("ipv-stress/results/unadjusted/H2_res.RDS"))
saveRDS(full_res %>% filter(H==3) %>% select(-H), here("ipv-stress/results/unadjusted/H3_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("ipv-stress/results/adjusted/H1_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("ipv-stress/results/adjusted/H2_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("ipv-stress/results/adjusted/H3_adj_res.RDS"))
