


rm(list=ls())
source(here::here("0-config.R"))

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


#Check monsoon differences
d$childno <- d$childNo.x
da$dataid[da$childid==61031] <- 6103
da$childno[da$childid==61031] <- 1
d$childno[d$childid==61031] <- 1
df <- merge(d, da, by=c("dataid", "childno"), all.x=T, all.y=T)
table(df$ageday_bt2.x!=df$ageday_bt2.y)
table(df$monsoon2!=df$monsoon_bt2)
table(df$monsoon3!=df$monsoon_bt3)

df[is.na(df$monsoon2) | is.na(df$monsoon_bt2),]

df[is.na(df$monsoon3) | is.na(df$monsoon_bt3),]


df$ageday_bt2.x[df$ageday_bt2.x!=df$ageday_bt2.y]
df$ageday_bt2.y[df$ageday_bt2.x!=df$ageday_bt2.y]

df$monsoon2[df$monsoon2!=df$monsoon_bt2]
df$monsoon_bt2[df$monsoon2!=df$monsoon_bt2]
df$dataid[df$monsoon2!=df$monsoon_bt2]
df$childno[df$monsoon2!=df$monsoon_bt2]

df$month2[df$monsoon2!=df$monsoon_bt2]
df$month_bt2[df$monsoon2!=df$monsoon_bt2]

df$ageday_bt2.x[df$monsoon2!=df$monsoon_bt2]
df$ageday_bt2.y[df$monsoon2!=df$monsoon_bt2]



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

# load(here("replication objects/audrie_immune_W.rdata"))
# load(here("replication objects/andrew_immune_W.rdata"))





# dim(da)
# wa <- wa[which(!(is.na(da$childid.x) & !is.na(da$childid.y))),]
# da <- da[which(!(is.na(da$childid.x) & !is.na(da$childid.y))),]
# dim(da)

da$t2_ln_igf[!is.na(da$t2_ln_igf)][1:10]
d$igf_t2[!is.na(d$igf_t2)][1:10]
round(d$igf_t2[!is.na(d$igf_t2)][1:10], 6)


res <- washb_tmle(Y=d$igf_t2, Delta=d$igf_t2.miss, tr=d$tr, W=W2 %>% subset(., select=c(sex, birthord)), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res

da$Delta <- ifelse(is.na(da$t2_ln_igf),0,1)
resA <- washb_tmle(Y=da$t2_ln_igf, Delta=da$Delta, tr=da$tr, W=wa %>% subset(., select=c(sex, birthord)), id=da$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
resA

summary(d$igf_t2)
summary(da$t2_ln_igf)

summary(is.na(d$igf_t2))
summary(is.na(da$t2_ln_igf))


table(d$igf_t2.miss)
table(da$Delta)

table(d$tr)
table(da$tr)

#Audrie missing one control

table(W2$sex)
table(wa$sex)

table(W2$birthord)
table(wa$birthord)

summary(d$block)
summary(da$block)



# One igf_t2 is missing from Audrie but not Andrew
dim(d)
dim(da)
d$childno <- d$childNo.x
d_merge <- merge(d, da, by=c("dataid", "childno"), all.x=T, all.y=T)
dim(d_merge)

#One obs not merging
d_merge[d_merge$block.x!=d_merge$block.y,]

table(is.na(d_merge$dataid))

#dataid is missing from one of Audrie's rows
da[is.na(da$dataid),]




res <- washb_tmle(Y=d$igf_t2, Delta=d$igf_t2.miss, tr=d$tr, W=W2 %>% subset(., select = -c(ageday_bt2, monsoon2)), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res

da$Delta <- ifelse(is.na(da$t2_ln_igf),0,1)
resA <- washb_tmle(Y=da$t2_ln_igf, Delta=da$Delta, tr=da$tr, W=wa %>% subset(., select = -c(ageday_bt2, monsoon_bt2)), id=da$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
resA

summary(W2$ageday_bt2)
summary(wa$ageday_bt2)


summary(W2$monsoon2)
summary(wa$monsoon_bt2)




res <- washb_tmle(Y=d$igf_t2, Delta=d$igf_t2.miss, tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
res

da$Delta <- ifelse(is.na(da$t2_ln_igf),0,1)
resA <- washb_tmle(Y=da$t2_ln_igf, Delta=da$Delta, tr=da$tr, W=wa, id=da$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
resA
