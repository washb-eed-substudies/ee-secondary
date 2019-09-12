


rm(list=ls())
load(here("replication objects/andrew_immune_ipcw_W.rdata"))
load(here("replication objects/audrie_immune_ipcw_W.rdata"))

dim(d)
dim(da)

head(d)
head(da)

table(d$tr)
table(da$tr)

d$dataid[!(da$dataid %in% d$dataid)]
da$dataid[!(d$dataid %in% da$dataid)]

d$childid[!(da$childid.x %in% d$childid)]
which(!(da$childid.x %in% d$childid))

da$childid.x[!(d$childid %in% da$childid.x)]
which(!(d$childid %in% da$childid.x))

# In Andrew but not audrie
# dataid==72003



#temp<-washb_tmle(Y=(Y[,i]), Delta=miss[,i], tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res <- washb_tmle(Y=d$igf_t2, Delta=d$igf_t2.miss, tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res

da$Delta <- ifelse(is.na(da$t2_ln_igf),0,1)
resA <- washb_tmle(Y=da$t2_ln_igf, Delta=da$Delta, tr=da$tr, W=wa, id=da$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
resA

colnames(W2)
colnames(wa)

summary(d$igf_t2)
summary(da$t2_ln_igf)

table(is.na(d$igf_t2))
table(is.na(da$t2_ln_igf))

table(d$igf_t2.miss)
table(da$Delta)

#Audrie is missing one outcome

load(here("replication objects/audrie_immune_W.rdata"))
load(here("replication objects/andrew_immune_W.rdata"))





res <- washb_tmle(Y=d$igf_t2, Delta=d$igf_t2.miss, tr=d$tr, W=W2 %>% subset(., select=c(sex, birthord)), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res

da$Delta <- ifelse(is.na(da$t2_ln_igf),0,1)
resA <- washb_tmle(Y=da$t2_ln_igf, Delta=da$Delta, tr=da$tr, W=wa %>% subset(., select=c(sex, birthord)), id=da$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
resA