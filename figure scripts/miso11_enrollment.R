rm(list=ls())
library(tibble)
data <- tibble(x = -10:100, y= -10:100)
head(data)

library(dplyr)
data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(-10, 100, 10)) +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) ->
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
  
  geom_rect(xmin = 30, xmax=70, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=79,label= '720 clusters created and randomly allocated \n 5,551 compounds randomly allocated', size=3)  +
  annotate('text', x= 10, y=79,label= 'Allocation', size=4) +
  
  geom_rect(xmin = 34, xmax=66, ymin=64, ymax=74, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=69,label= 'Control + Water + Sanitation\n + Handwashing + Nutrition\n 270 clusters \n 2,068 households', size=3) +
  annotate('text', x= 10, y=63,label= 'Subsample Target', size=4) +
  
  geom_rect(xmin = 70, xmax=90, ymin=58, ymax=68, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 80, y=63,label= 'Number of clusters not \n selected into substudy \n Year 1: 139 clusters \n Year 2: 135 clusters', size=3) +
  
  geom_rect(xmin = 40, xmax=60, ymin=52, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x=50, y=57,label= 'Year 1 \n 131 clusters \n 996 children \n Year 2 \n 135 clusters \n 1,021 children ', size=3)+
  
  geom_rect(xmin = 38, xmax=62, ymin=26, ymax=50, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=38,label= 'Year 1 \n 240 children lost to follow-up \n 23 moved \n 45 absent \n 76 withdrew \n 66 no live birth \n 30 child death \n Year 2 \n 25 new children measured  \n 262 children lost to follow-up \n 63 moved \n 5 absent \n 90 withdrew \n 67 no live birth \n 37 child death ', size=3) +
  annotate('text', x= 10, y=38,label= 'Follow-up', size=4) +
  
  geom_rect(xmin = 40, xmax=60, ymin=14, ymax=24, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=19,label= 'Year 1 \n 131 clusters \n 756 children \n Year 2 \n 135 clusters \n 759 children ', size=3) + 
  annotate('text', x= 10, y=19,label= 'Subsample Enrollment', size=4) +
  
  
  geom_rect(xmin = 40, xmax=60, ymin=4, ymax=12, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=8,label= 'Year 1 \n 94 missing outcome \n Year 2 \n 46 missing outcome', size=3) + 
  annotate('text', x= 10, y=8,label= 'Specimen Collection', size=4) +
  
  
  geom_rect(xmin = 40, xmax=60, ymin=-8, ymax=2, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-3,label= 'Year 1 \n 135 clusters \n 662 children \n Year 2 \n 135 clusters \n 713 children', size=3) +
  annotate('text', x= 10, y=-3,label= 'Analysis', size=4) ->
  p
p

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


