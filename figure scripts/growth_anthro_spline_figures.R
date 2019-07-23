


rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
theme_set(theme_ki())


#Load tmle results
load(here("/audrie results/telo_growth_spline_fits.Rdata"))


d1 <- rbind(
  data.frame(x="Change in T/S ratio", y="Change in LAZ\nbetween years 1 and 2", h1_delta_laz_v_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="Change in WLZ\nbetween years 1 and 2", h1_delta_whz_v_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="Change in WAZ\nbetween years 1 and 2", h1_delta_waz_v_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="Change in HCZ\nbetween years 1 and 2", h1_delta_hcz_v_delta_tsgam.res))
d2 <- rbind(
  data.frame(x="Change in T/S ratio", y="Length velocity\nbetween years 1 and 2", h2_len_velocity_v_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="Weight velocity\nbetween years 1 and 2", h2_wei_velocity_v_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="Head circumference velocity\nbetween years 1 and 2", h2_hc_velocity_v_delta_tsgam.res))
d3 <- rbind(
  data.frame(x="Change in T/S ratio", y="LAZ - year 2", h3_laz_t3_vs_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="WLZ - year 2", h3_waz_t3_vs_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="WAZ - year 2", h3_whz_t3_vs_delta_tsgam.res),
  data.frame(x="Change in T/S ratio", y="HCZ - year 2", h3_hcz_t3_vs_delta_tsgam.res))
d4 <- rbind(
  data.frame(x="T/S ratio - year 1", y="LAZ - year 1", h4_laz_t2_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="WLZ - year 1", h4_waz_t2_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="WAZ - year 1", h4_whz_t2_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="HCZ - year 1", h4_hcz_t2_vs_ts_t2gam.res))
d5 <- rbind(
  data.frame(x="T/S ratio - year 2", y="LAZ - year 2", h5_laz_t3_vs_ts_t3gam.res),
  data.frame(x="T/S ratio - year 2", y="WLZ - year 2", h5_waz_t3_vs_ts_t3gam.res),
  data.frame(x="T/S ratio - year 2", y="WAZ - year 2", h5_whz_t3_vs_ts_t3gam.res),
  data.frame(x="T/S ratio - year 2", y="HCZ - year 2", h5_hcz_t3_vs_ts_t3gam.res))
d6 <- rbind(
  data.frame(x="T/S ratio - year 1", y="LAZ - year 2", h6_laz_t3_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="WLZ - year 2", h6_waz_t3_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="WAZ - year 2", h6_whz_t3_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="HCZ - year 2", h6_hcz_t3_vs_ts_t2gam.res))
d7 <- rbind(
  data.frame(x="T/S ratio - year 1", y="Length velocity\nbetween years 1 and 2", h7_len_veloc_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="Weight velocity\nbetween years 1 and 2", h7_wei_veloc_vs_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="Head circumference velocity\nbetween years 1 and 2", h7_hc_veloc_vs_ts_t2gam.res))
d8 <- rbind(
  data.frame(x="T/S ratio - year 1", y="Change in LAZ\nbetween years 1 and 2", h8_delta_laz_v_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="Change in WLZ\nbetween years 1 and 2", h8_delta_waz_v_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="Change in WAZ\nbetween years 1 and 2", h8_delta_whz_v_ts_t2gam.res),
  data.frame(x="T/S ratio - year 1", y="Change in HCZ\nbetween years 1 and 2", h8_delta_hcz_v_ts_t2gam.res))

d1$y <- factor(d1$y)
d2$y <- factor(d2$y)
d3$y <- factor(d3$y)
d4$y <- factor(d4$y)
d5$y <- factor(d5$y)
d6$y <- factor(d6$y)
d7$y <- factor(d7$y)
d8$y <- factor(d8$y)




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
    geom_smooth(aes(y = fit, color=y), se = F) +
    geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
    geom_point(aes(y=Y), alpha=0.5) +
    coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
    scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
    scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
    xlab(.$x[1]) + ylab(.$y[1])
  }
  p2 <- d[d$y==levels(d$y)[2],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit, color=y), se = F) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
      geom_point(aes(y=Y), alpha=0.5) +
      coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
      xlab(.$x[1]) + ylab(.$y[1])
  }
  p3 <- d[d$y==levels(d$y)[3],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit, color=y), se = F) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
      geom_point(aes(y=Y), alpha=0.5) +
      coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
      xlab(.$x[1]) + ylab(.$y[1])
  }
  if(nlevels==4){
    p4 <- d[d$y==levels(d$y)[4],] %>% {ggplot(.,aes(x = X)) +
        geom_smooth(aes(y = fit, color=y), se = F) +
        geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
        geom_point(aes(y=Y), alpha=0.5) +
        coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
        scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
        scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=FALSE) + 
        xlab(.$x[1]) + ylab(.$y[1])
    }    
  }

  if(nlevels==4){
    #p <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
    return(list(p1, p2, p3, p4))
  }else{
    #p <- p1 + p2 + p3 + plot_layout(nrow = 1) 
    return(list(p1, p2, p3))
  }
  
  return(p)
}

plist1 <- spline_plot_functions(d1)
plist2 <- spline_plot_functions(d2)
plist3 <- spline_plot_functions(d3)
plist4 <- spline_plot_functions(d4)
plist5 <- spline_plot_functions(d5)
plist6 <- spline_plot_functions(d6)
plist7 <- spline_plot_functions(d7)
plist8 <- spline_plot_functions(d8)


p1 <- plot_grid(plist1[[1]], plist1[[2]], plist1[[3]], plist1[[4]], nrow=1, labels = c("","","",""))
p2 <- plot_grid(plist2[[1]], plist2[[2]], plist2[[3]], nrow=1, labels = c("","",""))
p3 <- plot_grid(plist3[[1]], plist3[[2]], plist3[[3]], plist3[[4]], nrow=1, labels = c("","","",""))
p4 <- plot_grid(plist4[[1]], plist4[[2]], plist4[[3]], plist4[[4]], nrow=1, labels = c("","","",""))
p5 <- plot_grid(plist5[[1]], plist5[[2]], plist5[[3]], plist5[[4]], nrow=1, 
                labels = c("Adjusted differences between quartiles of telomere length at Year 2 for each growth outcome","","",""),
                hjust=1,vjust=1)
p6 <- plot_grid(plist6[[1]], plist6[[2]], plist6[[3]], plist6[[4]], nrow=1, labels = c("","","",""))
p7 <- plot_grid(plist7[[1]], plist7[[2]], plist7[[3]], nrow=1, labels = c("","",""))
p8 <- plot_grid(plist8[[1]], plist8[[2]], plist8[[3]], plist8[[4]], nrow=1, labels = c("","","",""))




pcomb1 <- plot_grid(p1,
                p2,
                p3,
                ncol=1,
                labels = c("Adjusted differences between quartiles of change in telomere length between Years 1 and 2 for each growth outcome","",""),
                hjust=0.5, vjust=0.5,
                rel_heights = c(1, 1, 1))
pcomb2 <- plot_grid(p4,
                p6,
                p8,
                p7,
                ncol=1,
                labels = c("Adjusted differences between quartiles of telomere length at Year 1 for each growth outcome","","",""),
                hjust=0.5,vjust=0.5,
                rel_heights = c(1, 1, 1, 1))




ggsave(pcomb1, file = here("figures/telo-growth-splines_1.png"), height=12, width=14)
ggsave(pcomb2, file = here("figures/telo-growth-splines_2.png"), height=16, width=14)
ggsave(p5, file = here("figures/telo-growth-splines_3.png"), height=4, width=14)



# ggplot(d,aes(x = X)) +
#   geom_smooth(aes(y = fit, color=y), se = F) +
#   geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
#   geom_rug(aes(y=Y)) +
#   facet_wrap(~y)


