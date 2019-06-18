

rm(list=ls())
library(tidyverse)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
load("stool_figure_data.Rdata")
st <- stfull <- d
load("urine_figure_data.Rdata")
ur <- d

#Rename variables found in both datasets
st <- st %>% rename(
                st_staffid1= staffid1,
                st_date1= date1,            
                st_agem1= agem1,
                st_month1= month1,
                st_staffid2= staffid2,
                st_date2= date2,
                st_agem2= agem2,
                st_month2= month2,
                st_staffid3= staffid3,
                st_date3= date3,                         
                st_agem3= agem3,
                st_month3= month3,
                st_monsoon1= monsoon1,
                st_monsoon2= monsoon2,
                st_monsoon3= monsoon3,
                st_aged1= aged1,
                st_aged2= aged2,
                st_aged3= aged3)


ur <- ur %>% rename(
                  ur_staffid1= staffid1,
                ur_date1= date1,            
                ur_agem1= agem1,
                ur_month1= month1,
                ur_staffid2= staffid2,
                ur_date2= date2,
                ur_agem2= agem2,
                ur_month2= month2,
                ur_staffid3= staffid3,
                ur_date3= date3,                         
                ur_agem3= agem3,
                ur_month3= month3,
                ur_monsoon1= monsoon1,
                ur_monsoon2= monsoon2,
                ur_monsoon3= monsoon3,
                ur_aged1= aged1,
                ur_aged2= aged2,
                ur_aged3= aged3)


#find and subset kids with stool and not urine, and vice versa
head(ur)
head(st)


st_only <- anti_join(st, ur, by=c("dataid","childNo"))
dim(st_only)

ur_only <- anti_join(ur, st, by=c("dataid","childNo"))
dim(ur_only)

#subset to stool specific data
colnames(st)

st <- st %>% subset(., select=c(dataid,childNo, st_staffid1, st_date1,             
                st_agem1,   st_month1, st_staffid2, st_date2, st_agem2, st_month2, st_staffid3, st_date3,                           
                st_agem3, st_month3, aat1, aat2, 
                aat3, mpo1, mpo2, mpo3, neo1,              
                neo2, neo3, reg1b2,
                st_aged1, st_aged2, st_aged3,
               st_monsoon1, st_monsoon2, st_monsoon3))

#Drop unneeded variables and rename variables in common with stool dataset
ur <- ur %>% subset(., select = -c(consent1,nonconsent_reason1,h2aliqout1_t1,h2aliqout2_t1,h2aliqout3_t1,
                                    h2aliqout4_t1,h2aliqout5_t1,h2aliqout6_t1,h5aliqout7_t1,h5aliqout8_t1,h5aliqout9_t1,h5aliqout10_t1,h5aliqout11_t1,h5aliqout12_t1,
                                    preLMaliqout13_t1,preLMaliqout14_t1,preLMaliqout15_t1,preLMaliqout16_t1,preLMaliqout17_t1,preLMaliqout18_t1,preLMnonconsent_reason1,
                                    agey1,consent2,nonconsent_reason2,h2aliqout1_t2,h2aliqout2_t2,
                                    h2aliqout3_t2,h2aliqout4_t2,h2aliqout5_t2,h2aliqout6_t2,h5aliqout7_t2,h5aliqout8_t2,h5aliqout9_t2,h5aliqout10_t2,h5aliqout11_t2,
                                    h5aliqout12_t2,preLMaliqout13_t2,preLMaliqout14_t2,preLMaliqout15_t2,preLMaliqout16_t2,preLMaliqout17_t2,preLMaliqout18_t2,preLMnonconsent_reason2,
                                    agey2,consent3,nonconsent_reason3,h2aliqout1_t3,
                                    h2aliqout2_t3,h2aliqout3_t3,h2aliqout4_t3,h2aliqout5_t3,h2aliqout6_t3,h5aliqout7_t3,h5aliqout8_t3,h5aliqout9_t3,h5aliqout10_t3,
                                    h5aliqout11_t3,h5aliqout12_t3,preLMaliqout13_t3,preLMaliqout14_t3,preLMaliqout15_t3,preLMaliqout16_t3,preLMaliqout17_t3,preLMaliqout18_t3,preLMnonconsent_reason3,
                                    agey3,clusterid.y,block.y,union,fracode,svydate,
                                   
                                    Nhh,momeduy,dadeduy,dadagri,landacre,hfias))
# st_only <- st_only %>% subset(., select = -c(tubewell,storewat,treatwat,odmen,odwom,odch815,odch38,odchu3,latown,latslab,latseal,
#                                     latfeces,potty,humfeces,humfecesch,hwlat,hwlatwat,hwlatsoap,hwkit,hwkitwat,hwkitsoap,hwsw,hwss,hwsws,
#                                     n_asset_wardrobe,n_asset_table,n_asset_chair,n_asset_clock,n_asset_khat,n_asset_chouki,n_asset_mobile,n_cows,n_goats,n_chickens,
#                                     aliqout1_t1,aliqout2_t1,aliqout3_t1,aliqout4_t1,aliqout5_t1,nonconsent_reason1,agey1,
#                                     aliqout1_t2,aliqout2_t2,aliqout3_t2,aliqout4_t2,aliqout5_t2,nonconsent_reason2,agey2,aliqout1_t3,aliqout2_t3,aliqout3_t3,
#                                     aliqout4_t3,aliqout5_t3,nonconsent_reason3,agey3,clusterid.y,block.y,union,dadeduy,dadagri,landacre,hfias))


#Join urine and stool datasets, keeping only rows in both
d <- inner_join(st, ur, by=c("dataid","childNo"))
dim(ur)
dim(st)
dim(d)

d[d$dataid==1103,]

#Add in the rows only in the stool dataset
d <- bind_rows(d, st_only)
dim(d)
d[d$dataid==1103,]



#rename variables
colnames(d)


d <- d %>% rename(
  block = block.x,
  clusterid= clusterid.x
) 

table(is.na(d$block))
table(is.na(d$clusterid))

#Save dataset
write.csv(d, file = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/EE-BD_fulldata.csv")





