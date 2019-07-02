


rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)


#Load tmle results
load(here("/audrie results/telo_growth_spline_fits.Rdata"))


d <- rbind(
  data.frame(y="Change in LAZ", h1_delta_laz_v_delta_tsgam.res),
  data.frame(y="Change in WLZ", h1_delta_whz_v_delta_tsgam.res),
  data.frame(y="Change in WAZ", h1_delta_waz_v_delta_tsgam.res),
  data.frame(y="Change in HCZ", h1_delta_hcz_v_delta_tsgam.res)
)


p <- ggplot(d,aes(x = X)) +
  geom_smooth(aes(y = fit), se = F) +
  geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.1) +
  facet_wrap(~y)



ggsave(p, file = here("figures/telo-growth-splines-H1.png"), height=10, width=14)







