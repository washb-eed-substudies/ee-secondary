

rm(list=ls())
source(here::here("0-config.R"))
library(tlverse)
library(lubridate)
library(sl3)
library(tmle3)
library(data.table)

#load data
d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/washb-bangladesh-dm-ee-vital-saa-cortisol-f2-gcr.csv"))
head(d)

#rename regression-based residuals
d <- d %>% rename(t3_residual_saa_lm=t3_residual_saa,
                  t3_residual_cort_lm=t3_residual_cort)

#One time should be NA not 0 :
#childid== 71061 t3_z03_time == 01jan1960 00:00:00
d$t3_z03_time[d$childid== 7106] <- NA

#Aren't ssa and cortisol taken at diff times, so t3_z01_time should vary between the two?
#should t3_z02_time be used when regressing on outcome t3_saa_z02?

#Clean time of day variable
d$t3_z01_time <- as.character(d$t3_z01_time)
d$t3_z01_time <- dmy_hms(d$t3_z01_time)
d$sampletime <- hour(d$t3_z01_time)*60+minute(d$t3_z01_time)

#Temporarily drop missing time
d$sampletime[d$sampletime==0] <- NA

# #Examine regression predicted variables by time of day
# ggplot(d, aes(x=t3_residual_saa_lm)) + geom_density()
# ggplot(d, aes(x=t3_residual_cort_lm)) + geom_density()
# 
# #
# ggplot(d, aes(x=sampletime, y=t3_residual_saa_lm)) + geom_point() + geom_smooth(method="lm")
# ggplot(d, aes(x=sampletime, y=t3_residual_cort_lm)) + geom_point() + geom_smooth(method="lm")




#---------------------------------------------------------------------------------------------
#Get Superlearner prediction of ssa and cort after stressor from baselins ssa and cort and time of day
#---------------------------------------------------------------------------------------------


#Set up SL components
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- make_learner(Lrnr_glmnet)
lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
lrnr_hal_simple <- make_learner(Lrnr_hal9001, degrees = 1, n_folds = 2)
lrnr_gam <- Lrnr_pkg_SuperLearner$new("SL.gam")
lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")




stack <- make_learner(
  Stack,
  lrnr_glm, lrnr_mean, lrnr_ranger100, lrnr_glmnet,
  lrnr_gam, lrnr_bayesglm
)

screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")

cor_pipeline <- make_learner(Pipeline, screen_cor, stack)

fancy_stack <- make_learner(Stack, cor_pipeline, stack)


metalearner <- make_learner(Lrnr_nnls)


sl <- make_learner(Lrnr_sl,
                   learners = fancy_stack,
                   metalearner = metalearner
)


#Set parameters
outcome1 = "t3_saa_z02"
covars1 = c("t3_saa_z01","t3_saa_min_elaps","sampletime")
outcome2 = "t3_cort_z03"
covars2 = c("t3_cort_z01","t3_cort_min_elaps","sampletime")
id="childid"
  

  #Drop missingness
  node_list1 <- list(
    W=covars1,
    A=NULL,
    Y=outcome1,
    id=id 
  )
  node_list2 <- list(
    W=covars2,
    A=NULL,
    Y=outcome2,
    id=id 
  )
  dat =data.table(d)
  processed1 <- process_missing(data=dat, node_list1,  max_p_missing = 0.5)
  processed2 <- process_missing(data=dat, node_list2,  max_p_missing = 0.5)
  dat1 <- processed1$data
  dat2 <- processed2$data
  id1 <- dat1$childid
  id2 <- dat2$childid
  
  # define covars
  X1 <- dat1[, -outcome1, with=FALSE]
  covars1 <- colnames(X1)
  X2 <- dat2[, -outcome2, with=FALSE]
  covars2 <- colnames(X2)
  
  n1= nrow(dat1)
  n2= nrow(dat2)
  y1 <- as.matrix(dat1[, outcome1, with=FALSE])
  y2 <- as.matrix(dat2[, outcome2, with=FALSE])
  colnames(y1) <- NULL
  rownames(y1) <- NULL
  colnames(y2) <- NULL
  rownames(y2) <- NULL
  
  
  #fit lm's for comparison
  y_lm1 <- y1
  colnames(y_lm1) <- "y_lm"
  dat_lm1 <- dat1[, -outcome1, with=FALSE]
  dat_lm1  <- cbind(y_lm1,dat_lm1)
  fit_lm1 <- lm( y_lm ~ .,data = dat_lm1 )
  
  y_lm2 <- y2
  colnames(y_lm2) <- "y_lm"
  dat_lm2 <- dat2[, -outcome2, with=FALSE]
  dat_lm2  <- cbind(y_lm2,dat_lm2)
  fit_lm2 <- lm( y_lm ~ .,data = dat_lm2 )
  
  
  p1 <- predict(fit_lm1)
  mse_lm1 <- mean( ((y_lm1 - p1))^2)
  R2_lm1 <- 1 - mse_lm1/var((y_lm1))
  colnames( R2_lm1 ) <- NULL
  row.names(R2_lm1) <- NULL
  
  p2 <- predict(fit_lm2)
  mse_lm2 <- mean( ((y_lm2 - p2))^2)
  R2_lm2 <- 1 - mse_lm2/var((y_lm2))
  colnames( R2_lm2 ) <- NULL
  row.names(R2_lm2) <- NULL
  
  mse_lm1
  mse_lm2
  R2_lm1
  R2_lm2
  
  # create the sl3 task
  washb_task1 <- make_sl3_Task(
    data = dat1,
    covariates = covars1,
    outcome = outcome1
  )
  washb_task2 <- make_sl3_Task(
    data = dat2,
    covariates = covars2,
    outcome = outcome2
  )
  
  
  ##3. Fit the full model
  sl_fit1 <- sl$train(washb_task1)
  sl_fit2 <- sl$train(washb_task2)
  yhat_full1 <- sl_fit1$predict_fold(washb_task1,"validation")
  yhat_full2 <- sl_fit2$predict_fold(washb_task2,"validation")
  
  
  mse_full1 <- 1/n1 * sum((yhat_full1-y1)^2)
  mse_full2 <- 1/n2 * sum((yhat_full2-y2)^2)
  mse_full1
  mse_full2
  
  #save SL residuals
  t3_residual_saa_sl <- y1-yhat_full1
  t3_residual_cort_sl <- y2-yhat_full2
  
  # #compare time of day to prediction
  # ggplot_dat1 <- data.frame(x=dat1$sampletime, y=yhat_full1)
  # ggplot_dat2 <- data.frame(x=dat2$sampletime, y=yhat_full2)
  # ggplot(ggplot_dat1, aes(x=x, y=y)) + geom_point() + geom_smooth() + xlab("Minute of day") + ylab("Predicted saa") 
  # ggplot(ggplot_dat2, aes(x=x, y=y)) + geom_point() + geom_smooth() + xlab("Minute of day") + ylab("Predicted cort") 
  
  
  #add in child ID to merge with primary data
  sl_res1 <- data.frame(childid=id1, t3_residual_saa_sl=t3_residual_saa_sl)
  sl_res2 <- data.frame(childid=id2, t3_residual_cort_sl=t3_residual_cort_sl)
  
  d <- left_join(d, sl_res1, by=c("childid"))
  d <- left_join(d, sl_res2, by=c("childid"))
  
  # #compare to lm residuals
  # ggplot(d, aes(x=sampletime, y=t3_residual_saa)) + geom_point() + geom_smooth(method="lm") 
  # 
  # #Examine regression predicted variables by time of day
  # ggplot(d, aes(x=t3_residual_saa_sl)) + geom_density()
  # ggplot(d, aes(x=t3_residual_cort_sl)) + geom_density()
  # 
  # #
  # ggplot(d, aes(x=t3_residual_saa_lm, y=t3_residual_saa_sl)) + geom_point() + geom_smooth(method="lm")
  # ggplot(d, aes(x=t3_residual_cort_lm, y=t3_residual_cort_sl)) + geom_point() + geom_smooth(method="lm")
  
  
  
  #Drop LM residuals and save dataset
  d <- d %>% subset(., select = -c(t3_residual_saa_lm, t3_residual_cort_lm)) %>% rename("t3_residual_saa"="t3_residual_saa_sl", "t3_residual_cort" = "t3_residual_cort_sl")
  saveRDS(d, file=paste0(dropboxDir,"Data/Cleaned/Audrie/washb-bangladesh-dm-ee-vital-saa-cortisol-f2-gcr-residuals.RDS"))
