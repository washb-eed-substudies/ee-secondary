

#da<-d

head(da)

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

names(df)[grep('t2_', names(df))]


  temp <- washb_tmle(Y=da$t2_ipf2a3, tr=da$tr, pair=NULL, W=NULL, id=da$block, family="gaussian",contrast = c("Control","Nutrition + WSH"),
                     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE)
  temp_metric <-t(as.matrix(unlist(temp$estimates$ATE)))
  rownames(temp_metric) <- c("Nutrition + WSH v C")
  temp_metric


# Ym <-Y
# dm<-d


#Unadjusted glm models
  temp<-washb_tmle(Y=(Y$t2_ipf2a3), tr=dm$tr, W=NULL, id=dm$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)
  unlist(temp$estimates$ATE)



