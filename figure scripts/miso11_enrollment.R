rm(list=ls())
source(here::here("0-config.R"))
library(tibble)
data <- tibble(x = -10:100, y= -10:100)
head(data)

data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(-10, 100, 10)) +
  theme_void() ->
  p

p +
  geom_rect(xmin = 25, xmax=75, ymin=96, ymax=100, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=98,label= '13,279 compounds assessed for eligibility', size=3) ->
  p

p +
  geom_rect(xmin = 54, xmax=92, ymin=84, ymax=94, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 73, y=89, label= 'Excluded: 7,728 compounds \n 7,429 compounds excluded to create buffer zones\n 219 compounds did not meet enrollment criteria\n 80 compounds declined to participate', size=3) +
  annotate('text', x= 10, y=89,label= 'Enrollment', size=4) +
  
  
  geom_rect(xmin = 20, xmax=80, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=79,label= '720 clusters created and randomly allocated across 7 arms \n 5,551 compounds randomly allocated across 7 arms \n 2 of 7 arms selected into substudy', size=3)  +
  annotate('text', x= 10, y=79,label= 'Allocation', size=4) +
  
  geom_rect(xmin = 34, xmax=66, ymin=64, ymax=74, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=70,label= "paste(bold('    Control and Nutrition + Water \n + Sanitation + Handwashing arms'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=69,label= '\n\n270 clusters \n2,068 households', size=3) +
  annotate('text', x= 10, y=63,label= 'Subsample Target', size=4) +
  
  
  geom_rect(xmin = 70, xmax=90, ymin=58, ymax=68, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 80, y=63,label= 'Number of clusters not \n selected into substudy \n Year 1: 139 clusters \n Year 2: 135 clusters', size=3) +
  
  
  geom_rect(xmin = 42, xmax=58, ymin=52, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x=50, y=61.2,label= "paste(bold(' Year 1                    '))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=60.2,label= '\n\n            131 clusters                \n996 children   ', size=3) +
  annotate('text', x=50, y=56.2,label= "paste(bold(' Year 2                    '))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=55.2,label= '\n\n            135 clusters                \n1,021 children ', size=3) +
  
  
  geom_rect(xmin = 37, xmax=63, ymin=26, ymax=50, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=48.9,label= "paste(bold('Year 1                                       '))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=45.1,label= '\n\n  240 children lost to follow-up  \n23 moved                             \n45 absent                             \n76 withdrew                          \n66 no live birth                      \n30 child death                       ', size=3) + 
  annotate('text', x= 50, y=37.7,label= "paste(bold('Year 2                                       '))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=32.3,label= '\n  25 new children measured     \n  262 children lost to follow-up  \n63 moved                             \n5 absent                               \n90 withdrew                          \n67 no live birth                      \n37 child death                       ', size=3) + 
  annotate('text', x= 10, y=38.1,label= 'Follow-up', size=4) +
  
  geom_rect(xmin = 42, xmax=58, ymin=14, ymax=24, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=23.2,label= "paste(bold('Year 1                    '))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=22.2,label= '\n\n            131 clusters               \n756 children  ', size=3) + 
  annotate('text', x= 50, y=18.2,label= "paste(bold('Year 2                    '))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=17.2,label= '\n\n            135 clusters               \n759 children  ', size=3) + 
  annotate('text', x= 10, y=19,label= 'Subsample Enrollment', size=4) +
  
  
  geom_rect(xmin = 39, xmax=61, ymin=4, ymax=12, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=10.7,label= "paste(bold('Year 1                           '))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=9.2,label= ' 94 missing outcome', size=3) + 
  annotate('text', x= 50, y=7.2,label= "paste(bold('Year 2                           '))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=5.7,label= ' 46 missing outcome', size=3) + 
  annotate('text', x= 10, y=8,label= 'Specimen Collection', size=4) +
  
  
  geom_rect(xmin = 42, xmax=58, ymin=-8, ymax=2, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=1.2,label= "paste(bold('Year 1                    '))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=0.2,label= '\n\n            135 clusters               \n662 children  ', size=3) +
  annotate('text', x= 50, y=-3.8,label= "paste(bold('Year 2                    '))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=-4.8,label= '\n\n            135 clusters               \n713 children  ', size=3) +
  annotate('text', x= 10, y=-3,label= 'Analysis', size=4) ->
  p

ggsave(p, file = here("figures/telo-growth/telo_growth_enrollment.png"), height=14, width=9)


p +
  geom_segment(
    x=50, xend=50, y=96, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=54, y=89, yend=89, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=76, yend=74, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=64, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=70, y=63, yend=63, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=52, yend=50, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=26, yend=24, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=14, yend=12, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=4, yend=2, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

ggsave(p, file = here("figures/telo-growth/telo_growth_enrollment.png"), height=14, width=9)
ggsave(p, file = here("figures/telo-growth/telo_growth_enrollment.tiff"), height=14, width=9)


