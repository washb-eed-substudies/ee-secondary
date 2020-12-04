
#d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-dep-stress-telo-covariates.RDS"))
# 
# Y="laz_t3"
# X="delta_TS"
# W=c("sex","n_chicken")
# id="clusterid"
# family = "gaussian"
# pval = 0.2
# print=TRUE
# 
# 
# 
# res <- fit_RE_gam(d=d, Y="laz_t3", X="delta_TS", W=c("sex","n_chicken"))
# preds <- predict_gam_diff(fit=res$fit, d=res$dat, quantile_diff=c(0.25,0.75))


#Note: update to allow 
plot_gam_diff <- function(plotdf){
  p<-ggplot(plotdf) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
    geom_path(aes(x=x, y=lb.diff), color="blue")+
    geom_path(aes(x=x, y=ub.diff), color="red")+
    geom_path(aes(x=x, y=point.diff), color="black") + 
    geom_vline(aes(xintercept=q1)) +
    geom_vline(aes(xintercept=q3)) + 
    xlab("Exposure") + 
    ylab("GAM-estimated differences from 25th percentile of exposure")
  return(p)
}




fit_RE_gam <- function(d, Y, X, W=NULL, V=NULL, id="clusterid", family = "gaussian", pval = 0.2, print=TRUE){
  
  if(!is.null(W)){
    W <- subset(d, select = W)
  }
  Y <- subset(d, select = Y)
  colnames(Y) <- "Y"
  X <- subset(d, select = X)
  colnames(X) <- "X"
  id <- subset(d, select = id)
  colnames(id) <- "id"
  
  if(!is.null(V)){
    Vvar <- subset(d, select = V)
    colnames(Vvar) <- "V"
  }else{
    Vvar <-data.frame(V=rep(1, nrow(d)))
  }
  
  if(!is.null(W)){
    gamdat <- data.frame(Y, X, id, Vvar, W)
  }else{
    gamdat <- data.frame(Y, X, id, Vvar)
  }
  
  
  n.orig <- dim(gamdat)[1]
  rowdropped <- rep(1, nrow(gamdat))
  rowdropped[which(complete.cases(gamdat))] <- 0
  gamdat <- gamdat[complete.cases(gamdat), ]
  n.sub <- dim(gamdat)[1]
  if(print == TRUE){ 
    if (n.orig > n.sub){ 
      cat("\n-----------------------------------------\nDropping", 
          n.orig - n.sub, "observations due to missing values in 1 or more variables\n", 
          "Final sample size:", n.sub, "\n-----------------------------------------\n")
    }
  }
  if(!is.null(W)){
    colnamesW <- names(W)
    screenW <- subset(gamdat, select = colnamesW)
  }else{
    screenW <- NULL
  }
  if(!is.null(screenW)){
    if (print == TRUE) 
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y = gamdat$Y, 
                                                Ws = screenW, family = family, pval = pval, print = print))
  }else{
    Wscreen = NULL
  }
  
  if(!is.null(Wscreen)){
    d <- subset(gamdat, select = c("Y","X","id", "V", Wscreen))
  }else{
    d <- subset(gamdat, select = c("Y","X","id", "V"))
  }
  
  d$dummy<-1
  
  if(!is.null(W) & length(Wscreen)>0){
    
    #Make formula for adjusted model
    Ws <- subset(gamdat, select = c(Wscreen))
    
    #seperate factors and numeric
    W_factors <- colnames(Ws)[(grepl("factor", sapply(Ws, class))|grepl("character", sapply(Ws, class)))]
    W_numeric <- colnames(Ws)[(grepl("integer", sapply(Ws, class))|grepl("numeric", sapply(Ws, class)))]
    
    #seperate numeric indicators/few levels from continious
    indicator_vec <- rep(TRUE, length(W_numeric))
    for(i in 1:length(W_numeric)){
      N_unique <- length(unique(Ws[,W_numeric[i]]))
      if(N_unique>20){
        indicator_vec[i] <- FALSE
      }
    }
    
    W_indicator <- W_numeric[indicator_vec]
    W_continious <- W_numeric[!indicator_vec]
    
    #Create GAM equation
    if(length(W_continious)>0){
      eq_num <- paste0("s(", W_continious, ", bs=\"cr\")", collapse=" + ")
    }else{
      eq_num=NULL
    }
    if(length(W_factors)+length(W_indicator)>0){
      eq_fact <- paste0(" + ",paste0(c(W_factors,W_indicator), collapse=" + "))
    }else{
      eq_fact=NULL
    }
    
    #---------------------------------
    #fit model
    #---------------------------------

    #Check if X is binary or continious
    if(length(unique(d$X))>2){
      if(!is.null(V)){
        form <- paste0("Y~s(X, bs=\"cr\")+",eq_fact," +",eq_num,"+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +","+",form, fixed=TRUE)
        equation <- as.formula(form)
      }else{
        form <- paste0("Y~s(X, bs=\"cr\")+",eq_fact," +",eq_num,"+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +","+",form, fixed=TRUE)
        equation <- as.formula(form)
      }
    }else{
      if(!is.null(V)){
        form <- paste0("Y~X+",eq_fact," +",eq_num,"+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +","+",form, fixed=TRUE)
        equation <- as.formula(form)
      }else{
        form <- paste0("Y~X+",eq_fact," +",eq_num,"+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +","+",form, fixed=TRUE)
        equation <- as.formula(form)
      }
    }
      fit <- mgcv::gam(formula = equation,data=d)
    }else{
      
      if(length(unique(d$X))>2){
        if(!is.null(V)){
          equation <- as.formula(paste0("Y~s(X, bs=\"cr\")+ V + X*V + s(id,bs=\"re\",by=dummy)"))
          fit <- mgcv::gam(formula = equation,data=d)
          
        }else{
          fit <- mgcv::gam(Y~s(X, bs="cr")+s(id,bs="re",by=dummy),data=d)
        }
      }else{
        if(!is.null(V)){
          equation <- as.formula(paste0("Y~X + V + X*V + s(id,bs=\"re\",by=dummy)"))
          fit <- mgcv::gam(formula = equation,data=d)
          
        }else{
          fit <- mgcv::gam(Y~X +s(id,bs="re",by=dummy),data=d)
        }
      }
    }
  
  return(list(fit=fit, dat=d))
}



Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

predict_gam_diff <- function(fit, d, quantile_diff=c(0.25,0.75), Xvar, Yvar){
  
  d$dummy<-0
  
  Wvars <- colnames(d)[!(colnames(d) %in% c("Y","X" ,"id" ,"dummy"))]
  #set covariates to the median/mode
  for(i in Wvars){
    if(class(d[,i])=="character"|class(d[,i])=="factor"){
      d[,i] <- Mode(d[,i])
    }else{
      d[,i] <- median(d[,i])
    }
  }
  
  d <- d[order(d$X),]
  
  #Make sure subset has overall quantiles within it
  q1 <- unname(quantile(d$X,quantile_diff[1]))
  q3 <- unname(quantile(d$X,quantile_diff[2]))
  q1_pos <- which(abs(d$X- q1)==min(abs(d$X- q1)))[1]
  q3_pos <- which(abs(d$X- q3)==min(abs(d$X- q3)))[1]
  d$X[q1_pos] <- q1
  d$X[q3_pos] <- q3
  
  #get the direct prediction
  preds <- predict(fit,newdata=d,type="response")
  
  #get the prediction matrix
  Xp <- predict(fit,newdata=d,type="lpmatrix")
  # order the prediction matrix in the order of the exposure
  Xp <- Xp[order(d$X),]
  
  
  
  # take difference from the 25th percentile of X
  diff <- t(apply(Xp,1,function(x) x - Xp[q1_pos,]))
  
  # calculate the predicted differences
  point.diff <- diff %*% coef(fit)
  
  # calculate the pointwise SE - naive SE
  se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )
  
  # calculate upper and lower bounds
  lb.diff <- point.diff - 1.96*se.diff
  ub.diff <- point.diff + 1.96*se.diff
  Zval <-  abs(point.diff/se.diff)
  Pval <- exp(-0.717*Zval - 0.416*Zval^2)
  
  plotdf<-data.frame(Y=Yvar, X= Xvar, q1=d$X[q1_pos], q3=d$X[q3_pos], 
                     pred.q1=preds[q1_pos], pred.q3=preds[q3_pos], 
                     point.diff, lb.diff=lb.diff, ub.diff=ub.diff, Pval=Pval)
  
  
  res <- plotdf[round(nrow(d)*quantile_diff[2],0),]
  return(list(res=res, plotdf=plotdf))
}


predict_gam_int <- function(fit, d, quantile_diff=c(0.25,0.75), Xvar, Yvar){
  
  d$dummy<-0
  
  Wvars <- colnames(d)[!(colnames(d) %in% c("Y","X","V" ,"id" ,"dummy"))]
  #set covariates to the median/mode
  for(i in Wvars){
    if(class(d[,i])=="character"|class(d[,i])=="factor"){
      d[,i] <- Mode(d[,i])
    }else{
      d[,i] <- median(d[,i])
    }
  }
  
  d <- d[order(d$X),]
  
  q1 <- unname(quantile(d$X,quantile_diff[1]))
  q3 <- unname(quantile(d$X,quantile_diff[2]))
  
  reslist <- plotdf_list <- list()
  for(i in 1:length(unique(d$V))){
    diff <- NULL
    subgroup <- unique(d$V)[i]
    dsub <- d[d$V==subgroup,]
    
    #Make sure subset has overall quantiles within it
    q1_pos <- which(abs(dsub$X- q1)==min(abs(dsub$X- q1)))
    q3_pos <- which(abs(dsub$X- q3)==min(abs(dsub$X- q3)))
    dsub$X[q1_pos] <- q1
    dsub$X[q3_pos] <- q3
    dsub <- dsub[order(dsub$X),]
    
    Xp <- predict(fit,newdata=dsub,type="lpmatrix")
    
    # order the prediction matrix in the order of the exposure
    Xp <- Xp[order(dsub$X),]
    
    # take difference from the 25th percentile of X
    diff <- t(apply(Xp,1,function(x) x - Xp[q1_pos,]))
    
    # calculate the predicted differences
    point.diff <- diff %*% coef(fit)
    
    # calculate the pointwise SE - naive SE
    se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )
    
    # calculate upper and lower bounds
    lb.diff <- point.diff - 1.96*se.diff
    ub.diff <- point.diff + 1.96*se.diff
    
    plotdf<-data.frame(Y=Yvar, X= Xvar, subgroup=subgroup, x=dsub$X, q1=q1, q3=q3, point.diff, lb.diff=lb.diff, ub.diff=ub.diff)
    reslist[[i]] <- plotdf[q3_pos,]
    plotdf_list[[i]] <- plotdf
  }
  
  res <- bind_rows(reslist)
  
  return(list(res=res, plotdf=plotdf_list))
}



# m=H2_models$fit[i][[1]]
# newdata=H2_models$dat[i][[1]]
# 
# xlab=res$X
# ylab=res$Y
# title=""



gam_simul_CI <- function(m,newdata,nreps=10000, xlab="", ylab="", title="") {
  set.seed(12345)
  require(mgcv)
  require(dplyr)
  
  newdata <- newdata %>% mutate(dummy=0)
  
  Wvars <- colnames(newdata)[!(colnames(newdata) %in% c("Y","X" ,"id" ,"dummy"))]
  #set covariates to the median/mode
  for(i in Wvars){
    if(class(newdata[,i])=="character"|class(newdata[,i])=="factor"){
      newdata[,i] <- Mode(newdata[,i])
    }else{
      newdata[,i] <- median(newdata[,i])
    }
  }
  
  newdata <- newdata[order(newdata$X),]
  
  Vb <- vcov(m,unconditional = TRUE)
  pred <- predict(m, newdata, se.fit = TRUE)
  fit <- pred$fit
  se.fit <- pred$se.fit
  BUdiff <- MASS::mvrnorm(n=nreps, mu = rep(0, nrow(Vb)), Sigma = Vb)
  Cg <- predict(m, newdata, type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- data.frame(newdata,fit=pred$fit,se.fit=pred$se.fit)
  pred <- mutate(pred,
                 uprP = fit + (2 * se.fit),
                 lwrP = fit - (2 * se.fit),
                 uprS = fit + (crit * se.fit),
                 lwrS = fit - (crit * se.fit)
  )
  
  pred <- pred %>% arrange(X)
  p <- ggplot(pred) + geom_ribbon(aes(x=X, ymin=lwrS, ymax=uprS), alpha=0.5) + 
    geom_path(aes(x=X, y=lwrS), color="blue")+
    geom_path(aes(x=X, y=uprS), color="red")+
    geom_path(aes(x=X, y=fit ), color="black") +
    xlab(xlab) + ylab(ylab) +
    ggtitle(title)
  
  return(list(p=p, pred=pred))
}






