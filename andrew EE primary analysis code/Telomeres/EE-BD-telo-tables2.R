



#--------------------------------
#Table S1 (eLife table 2)
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_s.table1.Rdata")


s.balance.tab.mu_M 
s.balance.tab.n_M 
s.balance.tab.sd_M
s.balance.tab.mu_M<-t(s.balance.tab.mu_M[,-2])
s.balance.tab.n_M <-t(s.balance.tab.n_M[,-2])
s.balance.tab.sd_M<-t(s.balance.tab.sd_M[,-2]) 
s.balance.tab.mu_M 
s.balance.tab.n_M 
s.balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

Rownames<-c("No. of compounds",
            "Age (years)",
  "Years of education",
  "Years of education",
  "Works in agriculture",
  "Number of persons"
  ,"Has electricity"
  ,"Has a cement floor"
  ,"Acres of agricultural land owned",
      "Shallow tubewell primary water source",
    "Stored water observed at home",
    "Reported treating water yesterday",
    "Distance (mins) to primary water source",
        "Adult men",
        "Adult women",
        "Children: 8-\\textless 15 years",
        "Children: 3-\\textless 8 years",
        "Children: 0-\\textless 3 years",
        "Owned",
        "Concrete slab",
        "Functional water seal",
        "Visible stool on slab or floor",
        "Owned a potty",
        "House",
        "Child's play area",
        "Has water",
        "Has soap",
        "Has water",
        "Has soap",
    "Household is food secure"
  )





#--------------------------------
#Table S9
#--------------------------------
tab<-table1_create(mu=balance.tab.mu_M, 
                   n=balance.tab.n_M, 
                   sd=balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab

main.control<-c("1382",
                "24 (5)",
                "6 (3)",
                "5 (4)",
                "414 (30\\%)",
                "5 (2)",
                "784 (57\\%)",
                "145 (10\\%)",
                "0.15 (0.21)",
                "1038 (75\\%)",
                "666 (48\\%)",
                "4 (0\\%)",
                "1 (3)",
                "97 (7\\%)",
                "62 (4\\%)",
                "53 (10\\%)",
                "267 (38\\%)",
                "245 (82\\%)",
                "750 (54\\%)",
                "1251 (95\\%)",
                "358 (31\\%)",
                "625 (48\\%)",
                "61 (4\\%)",
                "114 (8\\%)",
                "21 (2\\%)",
                "178 (14\\%)",
                "88 (7\\%)",
                "118 (9\\%)",
                "33 (3\\%)",
                "932 (67\\%)"
                ) 

main.WSH<-c("702",
"24 (5)",
"6 (3)",
"5 (4)",
"216 (31\\%)",
"5 (2)",
"426 (61\\%)",
"77 (11\\%)",
"0.15 (0.23)",
"546 (78\\%)",
"304 (43\\%)",
"0 (0\\%)",
"1 (5)",
"54 (8\\%)",
"29 (4\\%)",
"30 (10\\%)",
"137 (38\\%)",
"123 (79\\%)",
"373 (53\\%)",
"620 (993\\%)",
"152 (26\\%)",
"289 (44\\%)",
"27 (4\\%)",
"48 (7\\%)",
"7 (1\\%)",
"67 (10\\%)",
"42 (7\\%)",
"61 (9\\%)",
"15 (2\\%)",
"482 (69\\%)"
)


main.N<-c(
"699",
"24 (5)",
"6 (3)",
"5 (4)",
"232 (33\\%)",
"5 (2)",
"409 (59\\%)",
"67 (10\\%)",
"0.16 (0.27\\%)",
"519 (74\\%)",
"301 (43\\%)",
"0 (0\\%)",
"1 (3)",
"59 (9\\%)",
"39 (6\\%)",
"23 (8\\%)",
"129 (39\\%)",
"128 (85\\%)",
"377 (54\\%)",
"620 (94\\%)",
"183 (31\\%)",
"331 (51\\%)",
"36 (5\\%)",
"58 (8\\%)",
"8 (1\\%)",
"62 (10\\%)",
"32 (5\\%)",
"61 (9\\%)",
"23 (4\\%)",
"479 (69\\%)") 

main.WSHN<-c("686",
                "24 (6)",
                "6 (3)",
                "5 (4)",
                "207 (30\\%)",
                "5 (2)",
                "412 (60\\%)",
                "72 (10\\%)",
                "0.14 (0.38)",
                "504 (73\\%)",
                "331 (48\\%)",
                "2 (0\\%)",
                "1 (2)",
                "50 (7\\%)",
                "24 (4\\%)",
                "28 (10\\%)",
                "134 (37\\%)",
                "123 (88\\%)",
                "367 (53\\%)",
                "621 (94\\%)",
                "155 (27\\%)",
                "298 (46\\%)",
                "30 (4\\%)",
                "49 (7\\%)",
                "7 (1\\%)",
                "72 (11\\%)",
                "36 (6\\%)",
                "60 (9\\%)",
                "18 (3\\%)",
                "485 (71\\%)"
                ) 


dim(tab)
length(main.control)
length(main.N)
length(main.WSH)
length(main.WSHN)

tab<-cbind(tab[,1],main.control,main.WSH,main.N,main.WSHN,tab[,2:5])


#Add in variable group labels
blank=rep("",4)

s.n.comp.f<-tab[1,]
tab<-tab[-1,]

s.table9_f=   rbind(
               c("\\textbf{Maternal}",blank,blank),
               tab[c(1:2),],
               c( "\\textbf{Paternal}",blank,blank),
               tab[c(3:4),],
               c("\\textbf{Household}",blank,blank),
               tab[c(5:8),],
               c("\\textbf{Drinking Water}",blank,blank),
               tab[c(9:12),],
               c("\\textbf{Sanitation}",blank,blank),
               c("Reported daily open defecation",blank,blank),
               tab[c(13:17),],
               c("Latrine",blank,blank),
               tab[c(18:22),],
               c("Human feces observed in the",blank,blank),
               tab[c(23:24),],
               c("\\textbf{Handwashing}",blank,blank),
               c("Within 6 steps of latrine",blank,blank),
               tab[c(25:26),],
               c("Within 6 steps of kitchen",blank,blank),
               tab[c(27:28),],
               c("\\textbf{Nutrition}",blank,blank),
               tab[c(29),])

rownames(s.table9_f)=NULL

s.table9_f





#FIX:
for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  s.table9_f[i,1]=paste("~~~",s.table9_f[i,1],sep="")
}
for(i in c(19:23,25:28,31,32,35:36,38:39)){
  s.table9_f[i,1]=paste("~~~~~",s.table9_f[i,1],sep="")
}



#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.n.comp.f, s.table9_f, file="s.table9_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(s.table9_f,digits=0)
