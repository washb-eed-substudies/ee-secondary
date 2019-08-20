

rm(list=ls())
library(tidyverse)
library(ggthemes) 
library(grid)
library(gridExtra)
library(scales)
library(lattice)
library(data.table)


#---------------------------------------
# Load and compile data
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_res_subset_unadj_sub.Rdata")
load("urine_res_subset_unadj.Rdata")
load("stool_res_unadj_M.Rdata")
load("urine_res_unadj_M.Rdata")
load("stool_res_adj_M.Rdata")
load("urine_res_adj_M.Rdata")
load("stool_ipcw_res.Rdata")
load("urine_ipcw_res.Rdata")
#load("urine_ipcw_res_SL.Rdata")

listnames <- ls()
objlist <- lapply(listnames, get)
names(objlist) <- listnames
objlist <- lapply(objlist, as.data.frame)
objlist <- lapply(objlist, function(x) x[,which(colnames(x) %in% c("RD","psi","ci.l","ci.u","V1","V2","V3"))])

 
res <- data.frame(rbindlist(objlist, use.names=F,  idcol = "id"))
res

#add contrast column
res$contrast <- c("CvWSH","CvN","CvNWSH","WSHvNWSH","NvNWSH")

res <- res %>% filter(contrast!="WSHvNWSH" & contrast!="NvNWSH")

# add a variable type
res$measure <- NA
res$measure[grep("adj", res$id)] <- "adj"
res$measure[grep("unadj_M", res$id)] <- "unadj"
res$measure[grep("unadj_sub", res$id)] <- "subset"
res$measure[grep("ipcw_M", res$id)] <- "ipcw"
res$measure[grep("ipcw_sl_M", res$id)] <- "SL ipcw"

#add time
res$round <- NA
res$round[grep("1", res$id)] <- "1"
res$round[grep("2", res$id)] <- "2"
res$round[grep("3", res$id)] <- "3"

#add biomarker
res$Y <- NA
res$Y[grep("^l", res$id)] <- "L"
res$Y[grep("^m", res$id)] <- "M"
res$Y[grep("^lm", res$id)] <- "LM"
res$Y[grep("^aat", res$id)] <- "AAT"
res$Y[grep("^neo", res$id)] <- "NEO"
res$Y[grep("^mpo", res$id)] <- "MPO"
res$Y[grep("^reg", res$id)] <- "Reg1b"

#drop unadjusted ipcw
res <- res[-grep("unadj_ipcw",res$id),]


res$measure <- factor(res$measure, levels=c("unadj","subset","adj","ipcw","SL ipcw"))
res$Y <- factor(res$Y, levels=c("AAT","MPO","NEO","Reg1b","L","M","LM" ))
res$contrast <- factor(res$contrast, levels=c("CvWSH","CvN","CvNWSH"))



#Set colors
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"

cols=c("CvWSH"=cblue,"CvN"=cred,"CvNWSH"=cgreen)


theme_set(theme_bw())

#plot
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EED-measure-comparison.pdf",width=10,height=8.5, paper="USr")

p1<-ggplot(subset(res, round==1), aes(measure)) + 
  geom_point(aes(x=measure, y=psi, fill=contrast, color=contrast), size = 4) +
  geom_linerange(aes(x=measure, ymin = ci.l, ymax = ci.u, color=contrast), 
                 alpha=0.5, size = 3) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual(values=cols) +
  scale_colour_manual(values=cols) +
  facet_grid(Y~contrast, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) +
  ylab("log-concentation difference")+ xlab("Treatment contrast") +
  ggtitle("Sampling round 1 (3 months)")
p1

p2<-ggplot(subset(res, round==2), aes(measure)) + 
  geom_point(aes(x=measure, y=psi, fill=contrast, color=contrast), size = 4) +
  geom_linerange(aes(x=measure, ymin = ci.l, ymax = ci.u, color=contrast), 
                 alpha=0.5, size = 3) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual(values=cols) +
  scale_colour_manual(values=cols) +
  facet_grid(Y~contrast, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) +
  ylab("log-concentation difference")+ xlab("Treatment contrast") +
  ggtitle("Sampling round 2 (Year 1)")
p2

p3<-ggplot(subset(res, round==3), aes(measure)) + 
  geom_point(aes(x=measure, y=psi, fill=contrast, color=contrast), size = 4) +
  geom_linerange(aes(x=measure, ymin = ci.l, ymax = ci.u, color=contrast), 
                 alpha=0.5, size = 3) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual(values=cols) +
  scale_colour_manual(values=cols) +
  facet_grid(Y~contrast, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) +
  ylab("log-concentation difference")+ xlab("Treatment contrast") +
  ggtitle("Sampling round 3 (Year 2)")
p3


dev.off()
