

select_groups <- function(data, groups, ...) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]}






tmle_quart<-function(dat=d, 
                    Y="LAZ", 
                    W=NULL, 
                    n.cat=4, 
                    A="logval", 
                    id="block",
                    Alevels=c("Q1","Q2","Q3","Q4"), 
                    reflevel=1, 
                    family="gaussian", 
                    SLlibrary="SL.gam", 
                    outputdf=NULL,
                    sparseN=0){

  
  
  Acuts=as.numeric(summary( subset(dat, select=A)[,1])[c(2,3,5)])
   cat("Cutpoints: ", Acuts, "\nNumbers per category:\n")
  
  id<-subset(dat, select=id)
  dat$STUDYID<-id[,1]
  
  #get study name
  #study <-deparse(substitute(dat))
  study <-"Study"
  
  #get number of studies 
  #nstudies<-length(unique(dat$STUDYID))
  nstudies<-1
  
  if(A %in% W){W<-W[-which(W %in% A)]}
  y<-subset(dat, select=Y)
  a<-subset(dat, select=A)
  dat$STUDYID<-as.factor(dat$STUDYID)
  studyid<-subset(dat, select="STUDYID")
  
  
  reference<-Alevels[reflevel]
  comparisons<-Alevels[-reflevel]
  
  
  summary(a[,1])
  
  table(findInterval(a[,1], Acuts))
  
  if(!is.null(Acuts)){
    a[,1]<-findInterval(a[,1], Acuts)
    a[,1]<-factor(a[,1])
    if(!is.null(Alevels)){levels(a[,1])<-Alevels[as.numeric(levels(a[,1]))+1]}
  }
  
  a[,1]<-as.factor(a[,1])
  print(table(a[,1]))
  
  if(!is.null(W)){
    screenW <- subset(dat, select = W)
    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y = y[,1], 
                                                Ws = screenW, family = family, pval = 0.2, print = T))
    
  w<-subset(dat, select=Wscreen)
  w<-design_matrix(w)
  }else{
    w<-data.frame(w=rep(1, nrow(dat)))
  }
  
  
  fulldat<-data.frame(y,a,studyid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>% #fulldat[fulldat[,2]==levels(fulldat[,2])[1],] %>%
    group_by(.[[2]]) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) #%>% .[,-1]
  
  
  #Extract desired levels
  levelmeans<-levelmeans[1:n.cat,]

  
  res<-NULL
  for(i in comparisons){
    
    dat<-fulldat[fulldat[,2]==reference | fulldat[,2]==i,]
    # print(table(dat[,2], dat[,1]))
    
    print(table(dat[,2]))
    
    #Convert factor to binary indicator
    dat[,2]<-ifelse(dat[,2]==reference,0,1)
    
    sparse=F
    if(family=="binomial"){
      tab<-c(table(dat[,1],dat[,2]))
      if(tab[1]<sparseN+1 | tab[2]<sparseN+1  | tab[3]<sparseN+1  | tab[4]<sparseN+1 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
        sparse=T
      }
    }
    
    
    
    if(sum(dat[,2]==0)>sparseN & sum(dat[,2]==1)>sparseN & sparse==F){ #Make sure there is enough support in the data
      w<-as.data.frame(dat[,4:ncol(dat)])
      if(ncol(w)==1){colnames(w)<-"w"}
      
      fit<-tmle_wrapper(d=dat, 
                        Y=Y,
                        Avar=A, 
                        lib=SLlibrary,
                        family=family,
                        Wvars=colnames(w))
      
      out<-as.data.frame(fit)
      names(out)<-i
      out<-t(out)
      out<-data.frame(Y,A,i,out,compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
      
      if(family=="binomial"){
        out<-data.frame(out,t(c(table(dat[,1],dat[,2]))))
        colnames(out)<-c("Y","A","level","psi","var.psi","CI1","CI2","pvalue","RR", "RRCI1", "RRCI2","RRpvalue","log.RR","var.log.RR", "compN", "refN","d","c","b","a")
      }else{
        colnames(out)<-c('Y',"A","level","psi","var.psi","CI1","CI2","pvalue", "compN", "refN")
      }
      
      
    }else{
      if(family=="binomial"){
        out<-data.frame(Y=Y,A=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, RR=NA,  RRCI1=NA, RRCI2=NA, RRpvalue=NA, log.RR=NA, var.log.RR=NA, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1), d=tab[1], c=tab[2], b=tab[3], a=tab[4])
        
      }else{
        out<-data.frame(Y=Y,A=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
      }
      rownames(out)<-i
    }
    res<-rbind(res,out)
  }
  
  
  if(family=="binomial"){
    refrow<-data.frame(res[1,1:2],reference,t(rep(NA,17)))
  }else{
    refrow<-data.frame(res[1,1:2],reference,t(rep(NA,7)))
  }
  colnames(refrow)<-colnames(res)
  res<-rbind(refrow,res)
  
  res<-cbind(res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("Y","A","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var", "compN", "refN","a","b","c","d", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("Y","A","level","ATE","var","CI1","CI2", "Pval", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL

  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    return(res)
  }
}






tmle_wrapper <-function(d, 
                       Y,
                       Avar, 
                       lib=lib, 
                       Wvars,
                       family){
  
  set.seed(12345)
  d<-as.data.frame(d)
  Y<-subset(d, select=Y)
  
  W<-subset(d, select=Wvars)
  W<-design_matrix(as.data.frame(W))
  
  mixedCV.tmle.A<-tmle(Y=Y[,1], 
                       A=subset(d, select=Avar)[,1], 
                       W=W, 
                       family = family, 
                       Q.SL.library=lib,
                       g.SL.library = lib,
                       verbose = T)  
  
  
  if(family=="binomial"){
    return(c(unlist(mixedCV.tmle.A$estimates$ATE),unlist(mixedCV.tmle.A$estimates$RR)))
  }else{
    return(unlist(mixedCV.tmle.A$estimates$ATE))
  }
}



rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}



GAM_simulCI<-function (Y, X, W = NULL, id = NULL, SL.library = c( "SL.gam"), cvControl = list(V=5), 
    gamdf = 1:10, imputeX=T){
  
    require(SuperLearner)
    if(is.null(id)) 
        id <- 1:length(Y)
    if(is.null(W)){
        nullW <- TRUE
        fulld <- data.frame(id, Y, X)
    }else{
        nullW <- FALSE
        Wdesign <- design_matrix(W)
        fulld <- data.frame(id, Y, X, Wdesign)
    }
    if(imputeX==T){
    As <- seq(0, max(fulld$X, na.rm=T), by=0.1)
    }else{
    As <- unique(fulld$X)
    }
    pY <- rep(NA, length(As))
    fitd <- fulld[complete.cases(fulld), ]
    n.orig <- dim(fulld)[1]
    n.fit <- dim(fitd)[1]
    
    
    if (n.orig > n.fit) 
        warning(paste("\n\n", n.orig - n.fit, "observations were dropped due to missing values\n in the outcome, X, or adjustement covariates. \n The original dataset contained", 
            n.orig, "observations,\n but GAM_simulCI is fitting the curve using", 
            n.fit, "observations."))
    X <- subset(fitd, select = -c(1:2))
    if (length(grep("SL.gam", SL.library)) > 0) {
      set.seed(123456)
        cvGAM <- ab_cvGAM(Y = fitd$Y, X = data.frame(X=fitd$X, temp=1), 
                          #id = fitd$id, 
                          SL.library = SL.library, 
                          #cvControl = cvControl, 
                          df = gamdf)
        SL.library <- cvGAM$SL.library
    }

    try(detach(package:gam))
    require(mgcv)
        m <- gam(Y ~ s(X, k = cvGAM$df_opt), data = fitd,  method = "REML")
        pval<- unlist(summary(m))$s.pv
    
      
Vb <- vcov(m)
newd <- seq(min(X), max(X), length = nrow(fitd))
pred <- predict(m, data.frame(X = newd),  se.fit = TRUE)
se.fit <- pred$se.fit


N <- 10000

BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(m, data.frame(X = newd), type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)


absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))


masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)
pred <- transform(cbind(data.frame(pred), newd),
                  uprP = fit + (2 * se.fit),
                  lwrP = fit - (2 * se.fit),
                  uprS = fit + (crit * se.fit),
                  lwrS = fit - (crit * se.fit))  
        
pred<-data.frame(Y=fitd$Y, X=fitd$X, pred, Pval=rep(pval, nrow(fitd)))  


    return(pred)    
}




