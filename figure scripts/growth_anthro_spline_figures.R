


rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
theme_set(theme_ki())


#Load tmle results
load(here("/audrie results/telo_growth_spline_fits.Rdata"))


d1 <- rbind(
  data.frame(y="Change in LAZ", h1_delta_laz_v_delta_tsgam.res),
  data.frame(y="Change in WLZ", h1_delta_whz_v_delta_tsgam.res),
  data.frame(y="Change in WAZ", h1_delta_waz_v_delta_tsgam.res))#,
  #data.frame(y="Change in HCZ", h1_delta_hcz_v_delta_tsgam.res))
d2 <- rbind(
  data.frame(y="Change in Length", h2_len_velocity_v_delta_tsgam.res),
  data.frame(y="Change in Weight", h2_wei_velocity_v_delta_tsgam.res),
  data.frame(y="Change in Head Circumference", h2_hc_velocity_v_delta_tsgam.res))
d3 <- rbind(
  data.frame(y="LAZ", h3_laz_t3_vs_delta_tsgam.res),
  data.frame(y="WLZ", h3_waz_t3_vs_delta_tsgam.res),
  data.frame(y="WAZ", h3_whz_t3_vs_delta_tsgam.res),
  data.frame(y="HCZ", h3_hcz_t3_vs_delta_tsgam.res))
d4 <- rbind(
  data.frame(y="LAZ", h4_laz_t2_vs_ts_t2gam.res),
  data.frame(y="WLZ", h4_waz_t2_vs_ts_t2gam.res),
  data.frame(y="WAZ", h4_whz_t2_vs_ts_t2gam.res),
  data.frame(y="HCZ", h4_hcz_t2_vs_ts_t2gam.res))



# 
# #Hypothesis 5
# h5_laz_t3_vs_ts_t3gam.res, 
# h5_waz_t3_vs_ts_t3gam.res, 
# h5_whz_t3_vs_ts_t3gam.res, 
# h5_hcz_t3_vs_ts_t3gam.res, 
# #Hypothesis 6
# h6_laz_t3_vs_ts_t2gam.res, 
# h6_waz_t3_vs_ts_t2gam.res, 
# h6_whz_t3_vs_ts_t2gam.res, 
# h6_hcz_t3_vs_ts_t2gam.res, 
# #Hypothesis 7
# h7_len_veloc_vs_ts_t2gam.res, 
# h7_wei_veloc_vs_ts_t2gam.res, 
# h7_hc_veloc_vs_ts_t2gam.res,
# #Hypothesis 8
# h8_delta_laz_v_ts_t2gam.res,
# h8_delta_waz_v_ts_t2gam.res,
# h8_delta_whz_v_ts_t2gam.res, 
# #h8_delta_hcz_v_ts_t2gam.res,  TEMP, not fitting

spline_plot_functions <- function(d){
  
  nlevels <- length(levels(d$y))

  quantiles <- d %>% group_by(y) %>%
    summarize(
      x.lb=as.numeric(quantile(X, probs = seq(0, 1, 0.05))[2]),
      x.ub=as.numeric(quantile(X, probs = seq(0, 1, 0.05))[20]),
      y.lb=as.numeric(quantile(Y, probs = seq(0, 1, 0.05))[2]),
      y.ub=as.numeric(quantile(Y, probs = seq(0, 1, 0.05))[20])
      )
  
  d <- left_join(d, quantiles, by="y")
  
  p1 <- d[d$y==levels(d$y)[1],] %>% {ggplot(.,aes(x = X)) +
    geom_smooth(aes(y = fit), se = F) +
    geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
    geom_point(aes(y=Y), alpha=0.1) +
    coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
    xlab("T/S ratio") + ylab(.$y[1])
  }
  p2 <- d[d$y==levels(d$y)[2],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit), se = F) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
      geom_point(aes(y=Y), alpha=0.1) +
      coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
      xlab("T/S ratio") + ylab(.$y[1])
  }
  p3 <- d[d$y==levels(d$y)[3],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit), se = F) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
      geom_point(aes(y=Y), alpha=0.1) +
      coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
      xlab("T/S ratio") + ylab(.$y[1])
  }
  if(nlevels==4){
    p4 <- d[d$y==levels(d$y)[4],] %>% {ggplot(.,aes(x = X)) +
        geom_smooth(aes(y = fit), se = F) +
        geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
        geom_point(aes(y=Y), alpha=0.1) +
        coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
        xlab("T/S ratio") + ylab(.$y[1])
    }    
  }

  if(nlevels==4){
    p <- p1 + p2 + p3 + p4
  }else{
    p <- p1 + p2 + p3 
  }
  
  return(p)
}

p1 <- spline_plot_functions(d1)
p2 <- spline_plot_functions(d2)
p3 <- spline_plot_functions(d3)
p4 <- spline_plot_functions(d4)



# ggplot(d,aes(x = X)) +
#   geom_smooth(aes(y = fit), se = F) +
#   geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
#   geom_rug(aes(y=Y)) +
#   facet_wrap(~y)



ggsave(p1, file = here("figures/telo-growth-splines-H1.png"), height=10, width=14)
ggsave(p2, file = here("figures/telo-growth-splines-H2.png"), height=10, width=14)
ggsave(p3, file = here("figures/telo-growth-splines-H3.png"), height=10, width=14)
ggsave(p4, file = here("figures/telo-growth-splines-H4.png"), height=10, width=14)
