rm=list(ls())

source(here::here("0-config.R"))


estriol_S <- read.csv("C:/Users/Sophia/Documents/WASH/Maternal Estriol/compiled-estriol-ST.csv")
estriol_G <- readRDS("C:/Users/Sophia/Downloads/maternal_estriol_GH.RDS")
nrow(estriol_S)==nrow(estriol_G)
ncol(estriol_S)==ncol(estriol_G)
head(estriol_S)
head(estriol_G)

S <- estriol_S %>% arrange(run) %>% select(!c(date, assay))
summary_S <- S %>% group_by(run) %>% 
  summarize(conc_dil_avg = mean(conc_dil),
            abs_450_avg = mean(abs_450),
            avg_conc = mean(conc),
            overall_conc_avg = mean(conc_avg))

G <- estriol_G %>% arrange(run)%>% select(!c(date, assay))
summary_G <- G %>% group_by(run) %>% 
  summarize(conc_dil_avg = mean(conc_dil),
            abs_450_avg = mean(abs_450),
            avg_conc = mean(conc),
            overall_conc_avg = mean(conc_avg))

sum(S!=G)
summary_S==summary_G
view(summary_S)
view(summary_G)
