###########################################################################
###########################################################################
###                                                                     ###
###                     Black MSM Causal Analysis                       ###
###                               Analysis                              ###
###                           Allison Meisner                           ###
###                          23 November 2021                           ###
###                                                                     ###
###########################################################################
###########################################################################

rm(list=ls())

library(mice)
library(missRanger)
library(survey)
library(Ecume)
library(dplyr)
library(nnet)
library(CBPS)
library(cobalt)

load(".../alldata_wide20230413.Rda")

`%!in%` = Negate(`%in%`)

###########################################################################
###########################################################################
###                                                                     ###
###                             SECTION 1                               ###
###                            Set up data                              ###
###                                                                     ###
###########################################################################
###########################################################################

#### Remove transgender people from 061 (exclusion factor in 073)
alldata_wide <- alldata_wide[which(alldata_wide$gender=="male"),]
#### Remove ppts from 061 who reported only TGM partners at baseline (this would have made them not eligible for HPTN 073)
alldata_wide <- alldata_wide[-which(alldata_wide$study=="HPTN061" & 
                                      alldata_wide$id %in% c(119,308,875,529,1453,368,1122,999,666)), ]

#### Some variables should be excluded from dataset (variables that have been categorized - keep categorical version)
alldata_wide$newid <- 1:nrow(alldata_wide) ## mice doesn't like having character variables (including 'studyid') but we need a unique ID for each ppt
alldata_wide$totalMPcat_0 <- ifelse(alldata_wide$totalMP_0==0 | alldata_wide$totalMP_0==1, 0, 
                                    ifelse(alldata_wide$totalMP_0==2 | alldata_wide$totalMP_0==3, 1,
                                           ifelse(alldata_wide$totalMP_0>=4, 2, NA)))
alldata_wide$totalMPcat_1 <- ifelse(alldata_wide$totalMP_1==0 | alldata_wide$totalMP_1==1, 0, 
                                    ifelse(alldata_wide$totalMP_1==2 | alldata_wide$totalMP_1==3, 1,
                                           ifelse(alldata_wide$totalMP_1>=4, 2, NA)))
alldata_wide$totalMPcat_2 <- ifelse(alldata_wide$totalMP_2==0 | alldata_wide$totalMP_2==1, 0, 
                                    ifelse(alldata_wide$totalMP_2==2 | alldata_wide$totalMP_2==3, 1,
                                           ifelse(alldata_wide$totalMP_2>=4, 2, NA)))
toimpute <- alldata_wide[,c("study","multisti_2","totalMPcat_2","primaryMP_2","gavemoneyforsex_2","gotmoneyforsex_2",
                            "HIVpuMP_2","UIAI_2","URAI_2","UIAIHIVpu_2","URAIHIVpu_2","stimulants_2",     
                            "poppers_2","marijuana_2","heroin_2","vicodin_2","alcohol_unpro_2","alcohol_pro_2","stimulant_unpro_2",
                            "stimulant_pro_2","popper_unpro_2","popper_pro_2","marijuana_unpro_2","marijuana_pro_2",
                            "trusthcp1_2","trusthcp2_2","trusthcp3_2","trusthcp4_2",     
                            "multisti_1","totalMPcat_1","primaryMP_1","gavemoneyforsex_1","gotmoneyforsex_1","HIVpuMP_1","UIAI_1",           
                            "URAI_1","UIAIHIVpu_1","URAIHIVpu_1","stimulants_1","poppers_1","marijuana_1",      
                            "heroin_1","vicodin_1","alcohol_unpro_1","alcohol_pro_1","stimulant_unpro_1","stimulant_pro_1","popper_unpro_1",   
                            "popper_pro_1","marijuana_unpro_1","marijuana_pro_1",       
                            "trusthcp1_1","trusthcp2_1","trusthcp3_1","trusthcp4_1","multisti_0","totalMPcat_0",        
                            "primaryMP_0","gavemoneyforsex_0","gotmoneyforsex_0","HIVpuMP_0","UIAI_0","URAI_0","UIAIHIVpu_0",   
                            "URAIHIVpu_0","stimulants_0","poppers_0","marijuana_0","heroin_0","vicodin_0",    
                            "alcohol_unpro_0","alcohol_pro_0","stimulant_unpro_0","stimulant_pro_0","popper_unpro_0","popper_pro_0",
                            "marijuana_unpro_0","marijuana_pro_0",
                            "trusthcp1_0","trusthcp2_0","trusthcp3_0","trusthcp4_0","age","region","education",          
                            "work","sexorient","hispanic","healthins","house",            
                            "marital","prepinit1","prepinit2","adherence_1","adherence_2",      
                            "hiv1","hiv2","hiv1yr","audit_0","audit_1","audit_2","depression_0","depression_1",
                            "depression_2","conspiracy_0","conspiracy_1","conspiracy_2","inthomoph_0",   
                            "inthomoph_1","inthomoph_2","ssupport_0","ssupport_1","ssupport_2","hivstigma_0","hivstigma_1",   
                            "hivstigma_2","incomecat","nomoneycat","nightinjailcat","newid")]
## should usedpep be included in imputation?? NO 
toimpute$study <- ifelse(toimpute$study=="HPTN061",0,1)
### Change to factor variables (only non-binary variables that should be treated as nominal vars)
categoricalvars <- c("region","house","totalMPcat_2","totalMPcat_1","totalMPcat_0","sexorient","incomecat","education","work")
## totalMPcat will not be modeled as an ordinal variable in IPW models
toimpute[,categoricalvars] <- lapply(toimpute[,categoricalvars], factor)
## Converting some variables from character to numeric (ordinal)
## NB: missRanger pmm algorithm will ensure the imputed variable takes values observed in data
toimpute$incomecat <- relevel(toimpute$incomecat, ref="less than $10K")
toimpute$incomecat <- as.numeric(toimpute$incomecat)
toimpute$education <- factor(toimpute$education, levels = c("HS or less", "some college", "college graduate"))
toimpute$education <- as.numeric(toimpute$education)
toimpute$work <- factor(toimpute$work, levels = c("unemployed", "parttime", "fulltime")) 
toimpute$work <- as.numeric(toimpute$work)
## re-code marital status
toimpute$marital <- ifelse(toimpute$marital=="single",0,1) ## partner/no partner
## change some vars from character to numeric
toimpute$hispanic <- ifelse(toimpute$hispanic=="Yes",1,0)
toimpute$healthins <- ifelse(toimpute$healthins=="Yes",1,0)
## change prep init vars to be 1 if ever initiated
toimpute$prepinit2 <- ifelse(toimpute$prepinit1==1 | toimpute$prepinit2==1, 1, 0)


###########################################################################
###########################################################################
###                                                                     ###
###                             SECTION 2                               ###
###                         Define functions                            ###
###                                                                     ###
###########################################################################
###########################################################################

###############################
###       SECTION 2.1       ###
###    General functions    ###  
###############################

### Bootstrap function (stratified by study)
bootfunc_strat <- function(dataset,setseed){
  set.seed(setseed)
  data061 <- dataset[which(dataset$study==0),]
  data073 <- dataset[which(dataset$study==1),]
  
  bootdata061 <- data061[sample(1:nrow(data061), nrow(data061), replace = TRUE),]
  bootdata073 <- data073[sample(1:nrow(data073), nrow(data073), replace = TRUE),]
  
  bootdata <- rbind(bootdata061, bootdata073)
  bootdata$newid <- c(1:nrow(bootdata))
  return(bootdata)
}

### Function to create 10 imputed datasets
imputefunc <- function(dataset,seed1,quiet=TRUE,ntrees=100,pmmk=3){
  set.seed(seed1)
  seeds <- as.integer(runif(10,100,10000000)) 
  imputed <- list(NA)
  for(i in 1:10){
    if(quiet==FALSE){print(i)}
    imputed[[i]] <- missRanger(dataset, . ~ . - newid, pmm.k = pmmk, seed = seeds[i], num.trees=ntrees, verbose=0)
  }
  return(imputed)
}

###############################
###       SECTION 2.2       ###
###    Balance functions    ###  
###############################

### Removed for now

###############################
###       SECTION 2.3       ###
###  Estimation function    ###  
###############################

### Function to estimate D effect
esteffect <- function(dataset){
  
  datatouse <- dataset
  
  datatouse$totalMPcat_0 <- as.numeric(levels(datatouse$totalMPcat_0))[datatouse$totalMPcat_0]
  datatouse$totalMPcat_1 <- as.numeric(levels(datatouse$totalMPcat_1))[datatouse$totalMPcat_1]
  datatouse$totalMPcat_2 <- as.numeric(levels(datatouse$totalMPcat_2))[datatouse$totalMPcat_2]
  datatouse$sexorient <- ifelse(datatouse$sexorient=="gay only", 1, 0)
  
  ##################
  ##### ACCESS #####
  ##################
  
  ## Fit model
  Amodel <- CBPS(study ~ age + factor(region) + multisti_0 + totalMPcat_0 + URAI_0 + sexorient, data=datatouse, ATT=1, standardize=FALSE, method="exact")
  datatouse$Awt <- Amodel$weights*(226/(226+1134))
  
  ## Check balance
  Abaltab <- bal.tab(datatouse[,c("age","region","multisti_0","totalMPcat_0","URAI_0","sexorient")],
                     treat = datatouse$study, weights = datatouse$Awt,un=TRUE,abs=TRUE, binary="std",s.d.denom="1")
  
  ## Summarize weights - mean, SD, min/max
  mean_Awt <- mean(datatouse$Awt)
  sd_Awt <- sd(datatouse$Awt)
  min_Awt <- min(datatouse$Awt)
  max_Awt <- max(datatouse$Awt)
  
  ## Estimate effect
  estAwt <- summary(glm(hiv1yr ~ study, data=datatouse, family=quasibinomial(link="logit"), weights=Awt))$coefficients[2,"Estimate"]
  estAwt_LL <- summary(glm(hiv1yr ~ study, data=datatouse, family=quasipoisson(link="log"), weights=Awt))$coefficients[2,"Estimate"]
  
  ## Sensitivity analysis
  RR_Asens <- c(0.5, 0.625, 0.7143, 0.8, 1, 1.25, 1.4, 1.6, 2)
  Asens <- function(RR_Asens){
    est <- rep(NA, length(RR_Asens))
    for(i in 1:length(RR_Asens)){
      dataAsens <- datatouse
      dataAsens[,"biascor"] <- rep(NA, nrow(dataAsens))
      for(j in 1:nrow(dataAsens)){
        if(dataAsens[j,"study"]==0){
          dataAsens[j,"biascor"] <- (RR_Asens[i]*Amodel$fitted.values[j] + (1-Amodel$fitted.values[j]))*dataAsens[j,"hiv1yr"]
        }else if(dataAsens[j,"study"]==1){
          dataAsens[j,"biascor"] <- (Amodel$fitted.values[j] + (1-Amodel$fitted.values[j])/RR_Asens[i])*dataAsens[j,"hiv1yr"] 
        }
      }
      est[i] <- summary(glm(biascor ~ study, data=dataAsens, family=quasipoisson(link="log"), weights=Awt))$coefficients[2,"Estimate"]
    }
    return(est)
  }
  Asens_rslt <- Asens(RR_Asens)
  
  ## Return result
  rsltA <- c("mean_Awt"=mean_Awt, "sd_Awt"=sd_Awt, "min_Awt"=min_Awt, "max_Awt"=max_Awt, 
             "estAwt"=estAwt, "Abaltab_unadj"=Abaltab$Balance[,2], "Abaltab_adj"=Abaltab$Balance[,3],
             "Asens_rslt"=Asens_rslt, "estAwt_LL"=estAwt_LL)
  
  ################################
  ##### INITIATION/ADHERENCE #####
  ################################
  
  ### Create datasets for model fitting
  ### Time = 1
  datasubt1 <- datatouse[,c("newid","study","multisti_0","totalMPcat_0","URAI_0","age","region",
                            "prepinit1","adherence_1","hiv1","sexorient")]
  datasubt1$time <- rep(1, nrow(datasubt1))
  names(datasubt1) <- c("newid","study","multisti","totalMPcat","URAI","age","region",
                        "prepinit","adherence","hiv","sexorient","time")
  
  ### Time = 2
  datasubt2 <- datatouse[,c("newid","study","multisti_1","totalMPcat_1","URAI_1","age","region",
                            "prepinit2","adherence_2","hiv2","sexorient")]
  datasubt2$time <- rep(2, nrow(datasubt2))
  names(datasubt2) <- c("newid","study","multisti","totalMPcat","URAI",
                        "age","region","prepinit","adherence","hiv","sexorient","time")
  alltime <- rbind(datasubt1, datasubt2)
  
  ### Remove individuals from t=2 part of dataset who had HIV diagnosed at time point 1
  tormID <- alltime[which(alltime$time==1 & alltime$hiv==1),"newid"]
  alltime <- alltime[which(alltime$time==1 | (alltime$newid %!in% tormID)),]
  
  ### Get A weights for 061
  alltime$Dwt <- rep(NA, nrow(alltime))
  
  AgivenX1 <- CBPS(study ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, data=alltime[which(alltime$time==1),], ATT=1, standardize=FALSE, method="exact")
  AgivenX1y1 <- CBPS(study ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, data=alltime[which(alltime$time==1 & (alltime$newid %!in% tormID)),], 
                     ATT=1, standardize=FALSE, method="exact") ## estimate Pr(A=1 | X_0, Y_1 = 0) -> these are the 061 wts for the 2nd time interval
  
  alltime[which(alltime$time==1), "Dwt"] <- AgivenX1$weights*(226/(226+1134))
  n061 <- sum(alltime[which(alltime$time==1 & (alltime$newid %!in% tormID)),"study"]==0)
  n073 <- sum(alltime[which(alltime$time==1 & (alltime$newid %!in% tormID)),"study"]==1)
  alltime[which(alltime$time==2), "Dwt"] <- AgivenX1y1$weights*(n073/(n073+n061))
  ## NB: wts = 1 for 073 (for now...)
  
  ### Determine D value for each observation
  alltime$dvar <- rep(NA, nrow(alltime))
  alltime$prevdvar <- rep(NA, nrow(alltime))
  for(i in 1:nrow(alltime)){
    if(alltime[i,"study"]==0){
      alltime[i,"dvar"] <- 0 
      alltime[i,"prevdvar"] <- 0
    }else if(alltime[i,"study"]==1){
      prepinit1 <- alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==1),"prepinit"]
      prepinit2 <- alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==2),"prepinit"]
      prepadh1 <- alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==1),"adherence"]
      prepadh2 <- alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==2),"adherence"]
      alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==1),"dvar"] <- ifelse(prepinit1==0, 0,ifelse((prepinit1==1 & prepadh1==0),1,
                                                                                                                  ifelse((prepinit1==1 & prepadh1==1),2,NA))) 
      alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==2),"dvar"] <- ifelse(prepinit2==0, 0,ifelse((prepinit2==1 & prepadh2==0),1,
                                                                                                                  ifelse((prepinit2==1 & prepadh2==1),2,NA))) 
      alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==1),"prevdvar"] <- 0 
      alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==2),"prevdvar"] <- 
        alltime[which(alltime$newid==alltime[i,"newid"] & alltime$time==1),"dvar"]
    }
  }
  
  ### Estimate Pr(D|X)
  D1givenX <- CBPS(factor(dvar) ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, 
                   data=alltime[which(alltime$study==1 & alltime$time==1),], ATT=0, standardize=FALSE, method="exact")
  D2givenX <- CBPS(factor(dvar) ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, 
                   data=alltime[which(alltime$study==1 & alltime$time==2),], ATT=0, standardize=FALSE, method="exact")
  
  alltime[which(alltime$study==1 & alltime$time==1),"Dwt"] <- D1givenX$weights ## the wts are 1/pr(D=d)
  alltime[which(alltime$study==1 & alltime$time==2),"Dwt"] <- D2givenX$weights
  ## NB: these wts replace the wt == 1 from AgivenX1 and AgivenX1y1for 073
  
  ### Check weights 
  
  ### Overall
  mean_Dwt <- mean(alltime[,"Dwt"])
  sd_Dwt <- sd(alltime[,"Dwt"]) 
  min_Dwt <- min(alltime[,"Dwt"]) 
  max_Dwt <- max(alltime[,"Dwt"]) 
  
  ### time = 1
  mean_Dwt_t1 <- mean(alltime[which(alltime$time==1),"Dwt"])
  sd_Dwt_t1 <- sd(alltime[which(alltime$time==1),"Dwt"]) 
  min_Dwt_t1 <- min(alltime[which(alltime$time==1),"Dwt"]) 
  max_Dwt_t1 <- max(alltime[which(alltime$time==1),"Dwt"]) 
  
  ### time = 2
  mean_Dwt_t2 <- mean(alltime[which(alltime$time==2),"Dwt"])
  sd_Dwt_t2 <- sd(alltime[which(alltime$time==2),"Dwt"]) 
  min_Dwt_t2 <- min(alltime[which(alltime$time==2),"Dwt"]) 
  max_Dwt_t2 <- max(alltime[which(alltime$time==2),"Dwt"]) 
  
  #######################
  ### Check balance
  
  ### 061/073
  Danalysis_AbalX1 <- bal.tab(alltime[which(alltime$time==1),c("age","region","multisti","totalMPcat","URAI","sexorient")],
                              treat = factor(alltime[which(alltime$time==1),]$study), weights = AgivenX1$weights,un=TRUE,abs=TRUE,binary="std",
                              s.d.denom="1")
  Danalysis_AbalX1y1 <- bal.tab(alltime[which(alltime$time==1 & (alltime$newid %!in% tormID)),c("age","region","multisti","totalMPcat","URAI","sexorient")],
                                treat = factor(alltime[which(alltime$time==1 & (alltime$newid %!in% tormID)),]$study), weights = AgivenX1y1$weights,un=TRUE,abs=TRUE,binary="std",
                                s.d.denom="1") ### WTS: balance in A=0 vs. A=1 on X0 covars for ppl who are in period 2
  
  ### Dvar
  Danalysis_Dbal1 <- bal.tab(alltime[which(alltime$time==1 & alltime$study==1),c("age","region","multisti","totalMPcat","URAI","sexorient")],
                             treat = factor(alltime[which(alltime$time==1 & alltime$study==1),]$dvar), weights = alltime[which(alltime$time==1 & alltime$study==1),]$Dwt,
                             un=TRUE,abs=TRUE,binary="std",s.d.denom="pooled") ## pooled SD in 073
  Danalysis_Dbal2 <- bal.tab(alltime[which(alltime$time==2 & alltime$study==1),c("age","region","multisti","totalMPcat","URAI","sexorient")],
                             treat = factor(alltime[which(alltime$time==2 & alltime$study==1),]$dvar), weights = alltime[which(alltime$time==2 & alltime$study==1),]$Dwt,
                             un=TRUE,abs=TRUE,binary="std",s.d.denom="pooled") ## pooled SD in 073
  
  ## Estimate effect
  estDwt <- summary(glm(hiv ~ time + dvar, data=alltime, family=quasibinomial(link="logit"), weights=Dwt))$coefficients[c(3),"Estimate"]
  estDwt_LL <- summary(glm(hiv ~ time + dvar, data=alltime, family=quasipoisson(link="log"), weights=Dwt))$coefficients[c(3),"Estimate"]
  
  ## Sensitivity analysis - only approx d/t log-linear model
  alltime$biascorD1 <- rep(NA, nrow(alltime))
  alltime$biascorD2 <- rep(NA, nrow(alltime))
  alltime$probD0 <- rep(NA, nrow(alltime))
  alltime$probD1 <- rep(NA, nrow(alltime))
  alltime$probD2 <- rep(NA, nrow(alltime))
  probD1 <- model.matrix(multinom(dvar ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, data=alltime[which(alltime$time==1),], trace=FALSE))
  probD2 <- model.matrix(multinom(dvar ~ age + factor(region) + multisti + totalMPcat + URAI + sexorient, data=alltime[which(alltime$time==2),], trace=FALSE))
  ## NB: above is needed because D1/D2givenX only fit in 073 and we need Pr(D) for 061 and 073
  
  LPD1t1 <- probD1 %*% D1givenX$coefficients[,1]
  LPD2t1 <- probD1 %*% D1givenX$coefficients[,2]
  alltime[which(alltime$time==1),"probD0"] <- 1/(1 + exp(LPD1t1) + exp(LPD2t1)) 
  alltime[which(alltime$time==1),"probD1"] <- exp(LPD1t1)/(1 + exp(LPD1t1) + exp(LPD2t1))
  alltime[which(alltime$time==1),"probD2"] <- exp(LPD2t1)/(1 + exp(LPD1t1) + exp(LPD2t1))
  
  LPD1t2 <- probD2 %*% D2givenX$coefficients[,1]
  LPD2t2 <- probD2 %*% D2givenX$coefficients[,2]
  alltime[which(alltime$time==2),"probD0"] <- 1/(1 + exp(LPD1t2) + exp(LPD2t2)) 
  alltime[which(alltime$time==2),"probD1"] <- exp(LPD1t2)/(1 + exp(LPD1t2) + exp(LPD2t2))
  alltime[which(alltime$time==2),"probD2"] <- exp(LPD2t2)/(1 + exp(LPD1t2) + exp(LPD2t2))
  
  RR_Dsens <- c(0.3, 0.4, 0.5, 0.625, 0.7143, 0.8, 1, 1.25, 1.4, 1.6, 2)
  Dsens <- function(RR_Dsens){
    estDwt <- rep(NA, length(RR_Dsens))
    for(i in 1:length(RR_Dsens)){
      dataDsens <- alltime
      dataDsens[,"biascor"] <- rep(NA, nrow(dataDsens))
      for(j in 1:nrow(dataDsens)){
        if(dataDsens[j,"dvar"]==0){
          dataDsens[j,"biascor"] <- (dataDsens[j,"probD0"]*1 + dataDsens[j,"probD1"]*RR_Dsens[i] + dataDsens[j,"probD2"]*RR_Dsens[i]*RR_Dsens[i])*dataDsens[j,"hiv"]
        }else if(dataDsens[j,"dvar"]==1){
          dataDsens[j,"biascor"] <- ((dataDsens[j,"probD0"]*1 + dataDsens[j,"probD1"]*RR_Dsens[i] + dataDsens[j,"probD2"]*RR_Dsens[i]*RR_Dsens[i])/RR_Dsens[i])*dataDsens[j,"hiv"] 
        }else if(dataDsens[j,"dvar"]==2){
          dataDsens[j,"biascor"] <- ((dataDsens[j,"probD0"]*1 + dataDsens[j,"probD1"]*RR_Dsens[i] + dataDsens[j,"probD2"]*RR_Dsens[i]*RR_Dsens[i])/(RR_Dsens[i]*RR_Dsens[i]))*dataDsens[j,"hiv"]
        }
      }
      estDwt[i] <- summary(glm(biascor ~ time + dvar, data=dataDsens, family=quasipoisson(link="log"), weights=Dwt))$coefficients[c(3),"Estimate"]
    }
    return(estDwt)
  }
  Dsens_rslt <- Dsens(RR_Dsens)
  
  hivD <- sum(alltime[which(alltime$hiv==1),"dvar"]) ## to flag samples with no d > 0 for HIV cases
  
  ## Return result
  rsltD <- c("mean_Dwt"=mean_Dwt, "sd_Dwt"=sd_Dwt, "min_Dwt"=min_Dwt, "max_Dwt"=max_Dwt, 
             "mean_Dwt_t1"=mean_Dwt_t1, "sd_Dwt_t1"=sd_Dwt_t1, "min_Dwt_t1"=min_Dwt_t1, "max_Dwt_t1"=max_Dwt_t1, 
             "mean_Dwt_t2"=mean_Dwt_t2, "sd_Dwt_t2"=sd_Dwt_t2, "min_Dwt_t2"=min_Dwt_t2, "max_Dwt_t2"=max_Dwt_t2, 
             "estDwt"=estDwt, "hivD"=hivD, 
             "Danalysis_AbalX1_unadj"=Danalysis_AbalX1$Balance[,2], "Danalysis_AbalX1_adj"=Danalysis_AbalX1$Balance[,3], 
             "Danalysis_AbalX1y1_unadj"=Danalysis_AbalX1y1$Balance[,2], "Danalysis_AbalX1y1_adj"=Danalysis_AbalX1y1$Balance[,3],
             "Danalysis_Dbal1_unadj"=Danalysis_Dbal1$Balance.Across.Pairs[,2], "Danalysis_Dbal1_adj"=Danalysis_Dbal1$Balance.Across.Pairs[,3], 
             "Danalysis_Dbal2_unadj"=Danalysis_Dbal2$Balance.Across.Pairs[,2], "Danalysis_Dbal2_adj"=Danalysis_Dbal2$Balance.Across.Pairs[,3], 
             "Dsens_rslt"=Dsens_rslt,"estDwt_LL"=estDwt_LL)
  
  return(c(rsltA,rsltD,"extAprob"=mean(Amodel$fitted.values>0.999),
           "extAprob1"=mean(AgivenX1$fitted.values>0.999),
           "extAprob1y1"=mean(AgivenX1y1$fitted.values>0.999), ## for A, ext prob means very high prob of A = 1 (this would be a positivity issue; very small probs could be an extrapolation issue)
           "extDprob1d0"=mean(D1givenX$fitted.values[,1]<0.001 | D1givenX$fitted.values[,1]>0.999),
           "extDprob1d1"=mean(D1givenX$fitted.values[,2]<0.001 | D1givenX$fitted.values[,2]>0.999),
           "extDprob1d2"=mean(D1givenX$fitted.values[,3]<0.001 | D1givenX$fitted.values[,3]>0.999),
           "extDprob2d0"=mean(D2givenX$fitted.values[,1]<0.001 | D2givenX$fitted.values[,1]>0.999),
           "extDprob2d1"=mean(D2givenX$fitted.values[,2]<0.001 | D2givenX$fitted.values[,2]>0.999),
           "extDprob2d2"=mean(D2givenX$fitted.values[,3]<0.001 | D2givenX$fitted.values[,3]>0.999),
           "minAprob"=min(Amodel$fitted.values), "maxAprob"=max(Amodel$fitted.values),
           "minAprob1"=min(AgivenX1$fitted.values), "maxAprob1"=max(AgivenX1$fitted.values),
           "minAprob1y1"=min(AgivenX1y1$fitted.values), "maxAprob1y1"=max(AgivenX1y1$fitted.values),
           "minDprob1d0"=min(D1givenX$fitted.values[,1]), "maxDprob1d0"=max(D1givenX$fitted.values[,1]),
           "minDprob1d1"=min(D1givenX$fitted.values[,2]), "maxDprob1d1"=max(D1givenX$fitted.values[,2]),
           "minDprob1d2"=min(D1givenX$fitted.values[,3]), "maxDprob1d2"=max(D1givenX$fitted.values[,3]),
           "minDprob2d0"=min(D2givenX$fitted.values[,1]), "maxDprob2d0"=max(D2givenX$fitted.values[,1]),
           "minDprob2d1"=min(D2givenX$fitted.values[,2]), "maxDprob2d1"=max(D2givenX$fitted.values[,2]),
           "minDprob2d2"=min(D2givenX$fitted.values[,3]), "maxDprob2d2"=max(D2givenX$fitted.values[,3]))) 
}


#####################################
###         SECTION 2.4           ###
###  Bootstrap and get results    ###  
#####################################

### Impute original dataset + get effect estimates
effect_imp <- function(dataset,setseed1,quiet=TRUE,ntrees=100,pmmk=3){
  imputedata <- imputefunc(dataset,setseed1,quiet,ntrees,pmmk)
  estimates_imp <- matrix(NA,nrow=10,ncol=148)
  rsltnames <- rep(NA,148)
  for(i in 1:10){
    rslt <- esteffect(imputedata[[i]])
    estimates_imp[i,] <- rslt
    rsltnames <- names(rslt)
  }
  meanrslt <- colMeans(estimates_imp)
  names(meanrslt) <- rsltnames
  colnames(estimates_imp) <- rsltnames
  
  ## max imbalance
  maximbalAunadj <- max(estimates_imp[,c("Abaltab_unadj1","Abaltab_unadj2","Abaltab_unadj3","Abaltab_unadj4","Abaltab_unadj5",
                                         "Abaltab_unadj6","Abaltab_unadj7","Abaltab_unadj8")])
  maximbalAadj <- max(estimates_imp[,c("Abaltab_adj1","Abaltab_adj2","Abaltab_adj3","Abaltab_adj4","Abaltab_adj5","Abaltab_adj6",
                                       "Abaltab_adj7","Abaltab_adj8")])
  
  maxDanalysis_AbalX1_unadj <- max(estimates_imp[,c("Danalysis_AbalX1_unadj1","Danalysis_AbalX1_unadj2","Danalysis_AbalX1_unadj3","Danalysis_AbalX1_unadj4",
                                                    "Danalysis_AbalX1_unadj5","Danalysis_AbalX1_unadj6","Danalysis_AbalX1_unadj7","Danalysis_AbalX1_unadj8")])
  maxDanalysis_AbalX1_adj <- max(estimates_imp[,c("Danalysis_AbalX1_adj1","Danalysis_AbalX1_adj2","Danalysis_AbalX1_adj3","Danalysis_AbalX1_adj4",
                                                  "Danalysis_AbalX1_adj5","Danalysis_AbalX1_adj6","Danalysis_AbalX1_adj7","Danalysis_AbalX1_adj8")])
  maxDanalysis_AbalX1y1_unadj <- max(estimates_imp[,c("Danalysis_AbalX1y1_unadj1","Danalysis_AbalX1y1_unadj2","Danalysis_AbalX1y1_unadj3","Danalysis_AbalX1y1_unadj4",
                                                      "Danalysis_AbalX1y1_unadj5","Danalysis_AbalX1y1_unadj6","Danalysis_AbalX1y1_unadj7","Danalysis_AbalX1y1_unadj8")])
  maxDanalysis_AbalX1y1_adj <- max(estimates_imp[,c("Danalysis_AbalX1y1_adj1","Danalysis_AbalX1y1_adj2","Danalysis_AbalX1y1_adj3","Danalysis_AbalX1y1_adj4",
                                                    "Danalysis_AbalX1y1_adj5","Danalysis_AbalX1y1_adj6","Danalysis_AbalX1y1_adj7","Danalysis_AbalX1y1_adj8")])
  
  maxDanalysis_Dbal1_unadj <- max(estimates_imp[,c("Danalysis_Dbal1_unadj1","Danalysis_Dbal1_unadj2","Danalysis_Dbal1_unadj3","Danalysis_Dbal1_unadj4",
                                                   "Danalysis_Dbal1_unadj5","Danalysis_Dbal1_unadj6","Danalysis_Dbal1_unadj7","Danalysis_Dbal1_unadj8")])  
  maxDanalysis_Dbal1_adj <- max(estimates_imp[,c("Danalysis_Dbal1_adj1","Danalysis_Dbal1_adj2","Danalysis_Dbal1_adj3","Danalysis_Dbal1_adj4",
                                                 "Danalysis_Dbal1_adj5","Danalysis_Dbal1_adj6","Danalysis_Dbal1_adj7","Danalysis_Dbal1_adj8")])
  maxDanalysis_Dbal2_unadj <- max(estimates_imp[,c("Danalysis_Dbal2_unadj1","Danalysis_Dbal2_unadj2","Danalysis_Dbal2_unadj3","Danalysis_Dbal2_unadj4",
                                                   "Danalysis_Dbal2_unadj5","Danalysis_Dbal2_unadj6","Danalysis_Dbal2_unadj7","Danalysis_Dbal2_unadj8")])
  maxDanalysis_Dbal2_adj <- max(estimates_imp[,c("Danalysis_Dbal2_adj1","Danalysis_Dbal2_adj2","Danalysis_Dbal2_adj3","Danalysis_Dbal2_adj4",
                                                 "Danalysis_Dbal2_adj5","Danalysis_Dbal2_adj6","Danalysis_Dbal2_adj7","Danalysis_Dbal2_adj8")])
  
  ### individual
  maximbalAindiv_unadj <- apply(estimates_imp[,c("Abaltab_unadj1","Abaltab_unadj2","Abaltab_unadj3","Abaltab_unadj4","Abaltab_unadj5",
                                                 "Abaltab_unadj6","Abaltab_unadj7","Abaltab_unadj8")], 2, max)
  names(maximbalAindiv_unadj) <- paste0("maximbalA_unadj",c(1:8))
  maximbalAindiv_adj <- apply(estimates_imp[,c("Abaltab_adj1","Abaltab_adj2","Abaltab_adj3","Abaltab_adj4","Abaltab_adj5",
                                               "Abaltab_adj6","Abaltab_adj7","Abaltab_adj8")], 2, max)
  names(maximbalAindiv_adj) <- paste0("maximbalA_adj",c(1:8))
  
  
  maxDanalysis_AbalX1_unadj_indiv <- apply(estimates_imp[,c("Danalysis_AbalX1_unadj1","Danalysis_AbalX1_unadj2","Danalysis_AbalX1_unadj3","Danalysis_AbalX1_unadj4",
                                                            "Danalysis_AbalX1_unadj5","Danalysis_AbalX1_unadj6","Danalysis_AbalX1_unadj7","Danalysis_AbalX1_unadj8")], 2, max)
  names(maxDanalysis_AbalX1_unadj_indiv) <- paste0("maxDanalysis_AbalX1_unadj_indiv",c(1:8))
  maxDanalysis_AbalX1_adj_indiv <- apply(estimates_imp[,c("Danalysis_AbalX1_adj1","Danalysis_AbalX1_adj2","Danalysis_AbalX1_adj3","Danalysis_AbalX1_adj4",
                                                          "Danalysis_AbalX1_adj5","Danalysis_AbalX1_adj6","Danalysis_AbalX1_adj7","Danalysis_AbalX1_adj8")], 2, max)
  names(maxDanalysis_AbalX1_adj_indiv) <- paste0("maxDanalysis_AbalX1_adj_indiv",c(1:8))
  
  
  maxDanalysis_AbalX1y1_unadj_indiv <- apply(estimates_imp[,c("Danalysis_AbalX1y1_unadj1","Danalysis_AbalX1y1_unadj2","Danalysis_AbalX1y1_unadj3","Danalysis_AbalX1y1_unadj4",
                                                              "Danalysis_AbalX1y1_unadj5","Danalysis_AbalX1y1_unadj6","Danalysis_AbalX1y1_unadj7","Danalysis_AbalX1y1_unadj8")], 2, max)
  names(maxDanalysis_AbalX1y1_unadj_indiv) <- paste0("maxDanalysis_AbalX1y1_unadj_indiv",c(1:8))
  maxDanalysis_AbalX1y1_adj_indiv <- apply(estimates_imp[,c("Danalysis_AbalX1y1_adj1","Danalysis_AbalX1y1_adj2","Danalysis_AbalX1y1_adj3","Danalysis_AbalX1y1_adj4",
                                                            "Danalysis_AbalX1y1_adj5","Danalysis_AbalX1y1_adj6","Danalysis_AbalX1y1_adj7","Danalysis_AbalX1y1_adj8")], 2, max)
  names(maxDanalysis_AbalX1y1_adj_indiv) <- paste0("maxDanalysis_AbalX1y1_adj_indiv",c(1:8))
  
  maxDanalysis_Dbal1_unadj_indiv <- apply(estimates_imp[,c("Danalysis_Dbal1_unadj1","Danalysis_Dbal1_unadj2","Danalysis_Dbal1_unadj3","Danalysis_Dbal1_unadj4",
                                                           "Danalysis_Dbal1_unadj5","Danalysis_Dbal1_unadj6","Danalysis_Dbal1_unadj7","Danalysis_Dbal1_unadj8")], 2, max)
  names(maxDanalysis_Dbal1_unadj_indiv) <- paste0("maxDanalysis_Dbal1_unadj_indiv",c(1:8))
  
  maxDanalysis_Dbal1_adj_indiv <- apply(estimates_imp[,c("Danalysis_Dbal1_adj1","Danalysis_Dbal1_adj2","Danalysis_Dbal1_adj3","Danalysis_Dbal1_adj4",
                                                         "Danalysis_Dbal1_adj5","Danalysis_Dbal1_adj6","Danalysis_Dbal1_adj7","Danalysis_Dbal1_adj8")], 2, max)
  names(maxDanalysis_Dbal1_adj_indiv) <- paste0("maxDanalysis_Dbal1_adj_indiv",c(1:8))
  
  maxDanalysis_Dbal2_unadj_indiv <- apply(estimates_imp[,c("Danalysis_Dbal1_unadj1","Danalysis_Dbal1_unadj2","Danalysis_Dbal1_unadj3","Danalysis_Dbal1_unadj4",
                                                           "Danalysis_Dbal1_unadj5","Danalysis_Dbal1_unadj6","Danalysis_Dbal1_unadj7","Danalysis_Dbal1_unadj8")], 2, max)
  names(maxDanalysis_Dbal2_unadj_indiv) <- paste0("maxDanalysis_Dbal2_unadj_indiv",c(1:8))
  
  maxDanalysis_Dbal2_adj_indiv <- apply(estimates_imp[,c("Danalysis_Dbal2_adj1","Danalysis_Dbal2_adj2","Danalysis_Dbal2_adj3","Danalysis_Dbal2_adj4",
                                                         "Danalysis_Dbal2_adj5","Danalysis_Dbal2_adj6","Danalysis_Dbal2_adj7","Danalysis_Dbal2_adj8")], 2, max)
  names(maxDanalysis_Dbal2_adj_indiv) <- paste0("maxDanalysis_Dbal2_adj_indiv",c(1:8))

  
  ## max wt
  maxAwt <- max(estimates_imp[,"max_Awt"])
  maxDwt <- max(estimates_imp[,"max_Dwt"])
  
  ## extreme probs
  minAprob_allimp <- min(estimates_imp[,"minAprob"])
  maxAprob_allimp <- max(estimates_imp[,"maxAprob"])
  minAprob1_allimp <- min(estimates_imp[,"minAprob1"])
  maxAprob1_allimp <- max(estimates_imp[,"maxAprob1"])
  minAprob1y1_allimp <- min(estimates_imp[,"minAprob1y1"])
  maxAprob1y1_allimp <- max(estimates_imp[,"maxAprob1y1"])
  minDprob1d0_allimp <- min(estimates_imp[,"minDprob1d0"])
  maxDprob1d0_allimp <- max(estimates_imp[,"maxDprob1d0"])
  minDprob1d1_allimp <- min(estimates_imp[,"minDprob1d1"])
  maxDprob1d1_allimp <- max(estimates_imp[,"maxDprob1d1"])
  minDprob1d2_allimp <- min(estimates_imp[,"minDprob1d2"])
  maxDprob1d2_allimp <- max(estimates_imp[,"maxDprob1d2"])
  minDprob2d0_allimp <- min(estimates_imp[,"minDprob2d0"])
  maxDprob2d0_allimp <- max(estimates_imp[,"maxDprob2d0"])
  minDprob2d1_allimp <- min(estimates_imp[,"minDprob2d1"])
  maxDprob2d1_allimp <- max(estimates_imp[,"maxDprob2d1"])
  minDprob2d2_allimp <- min(estimates_imp[,"minDprob2d2"])
  maxDprob2d2_allimp <- max(estimates_imp[,"maxDprob2d2"])
  
  max_extAprob <- max(estimates_imp[,"extAprob"]) ## this allows us to say "up to x% of obs had extreme probabilities", or "no more than x%"
  max_extAprob1 <- max(estimates_imp[,"extAprob1"])
  max_extAprob1y1 <- max(estimates_imp[,"extAprob1y1"])
  max_extDprob1d0 <- max(estimates_imp[,"extDprob1d0"])
  max_extDprob1d1 <- max(estimates_imp[,"extDprob1d1"])
  max_extDprob1d2 <- max(estimates_imp[,"extDprob1d2"])
  max_extDprob2d0 <- max(estimates_imp[,"extDprob2d0"])
  max_extDprob2d1 <- max(estimates_imp[,"extDprob2d1"])
  max_extDprob2d2 <- max(estimates_imp[,"extDprob2d2"])
  
  meanrslt <- c(meanrslt, maximbalAunadj, maximbalAadj, maxDanalysis_AbalX1_unadj, maxDanalysis_AbalX1_adj, maxDanalysis_AbalX1y1_unadj, 
                maxDanalysis_AbalX1y1_adj, maxDanalysis_Dbal1_unadj, maxDanalysis_Dbal1_adj, maxDanalysis_Dbal2_unadj, maxDanalysis_Dbal2_adj, 
                maximbalAindiv_unadj, maximbalAindiv_adj, maxDanalysis_AbalX1_unadj_indiv, maxDanalysis_AbalX1_adj_indiv,
                maxDanalysis_AbalX1y1_unadj_indiv, maxDanalysis_AbalX1y1_adj_indiv, maxDanalysis_Dbal1_unadj_indiv,
                maxDanalysis_Dbal1_adj_indiv, maxDanalysis_Dbal2_unadj_indiv, maxDanalysis_Dbal2_adj_indiv,
                maxAwt, maxDwt,
                minAprob_allimp,maxAprob_allimp,minAprob1_allimp,maxAprob1_allimp,minAprob1y1_allimp,maxAprob1y1_allimp,
                minDprob1d0_allimp,maxDprob1d0_allimp,minDprob1d1_allimp,maxDprob1d1_allimp,minDprob1d2_allimp,maxDprob1d2_allimp,
                minDprob2d0_allimp,maxDprob2d0_allimp,minDprob2d1_allimp,maxDprob2d1_allimp,minDprob2d2_allimp,maxDprob2d2_allimp,
                max_extAprob,max_extAprob1,max_extAprob1y1,max_extDprob1d0,max_extDprob1d1,max_extDprob1d2,
                max_extDprob2d0,max_extDprob2d1,max_extDprob2d2)
  names(meanrslt) <- c(rsltnames, "maximbalAunadj", "maximbalAadj", "maxDanalysis_AbalX1_unadj", "maxDanalysis_AbalX1_adj", "maxDanalysis_AbalX1y1_unadj", 
                       "maxDanalysis_AbalX1y1_adj", "maxDanalysis_Dbal1_unadj", "maxDanalysis_Dbal1_adj", "maxDanalysis_Dbal2_unadj", 
                       "maxDanalysis_Dbal2_adj", 
                       names(maximbalAindiv_unadj), names(maximbalAindiv_adj), names(maxDanalysis_AbalX1_unadj_indiv),
                       names(maxDanalysis_AbalX1_adj_indiv), names(maxDanalysis_AbalX1y1_unadj_indiv), 
                       names(maxDanalysis_AbalX1y1_adj_indiv), names(maxDanalysis_Dbal1_unadj_indiv), names(maxDanalysis_Dbal1_adj_indiv),
                       names(maxDanalysis_Dbal2_unadj_indiv), names(maxDanalysis_Dbal2_adj_indiv),
                       "maxAwt", "maxDwt",
                       "minAprob_allimp","maxAprob_allimp","minAprob1_allimp","maxAprob1_allimp","minAprob1y1_allimp","maxAprob1y1_allimp",
                       "minDprob1d0_allimp","maxDprob1d0_allimp","minDprob1d1_allimp","maxDprob1d1_allimp","minDprob1d2_allimp","maxDprob1d2_allimp",
                       "minDprob2d0_allimp","maxDprob2d0_allimp","minDprob2d1_allimp","maxDprob2d1_allimp","minDprob2d2_allimp","maxDprob2d2_allimp",
                       "max_extAprob","max_extAprob1","max_extAprob1y1","max_extDprob1d0","max_extDprob1d1","max_extDprob1d2",
                       "max_extDprob2d0","max_extDprob2d1","max_extDprob2d2")
  
  return(meanrslt)
}

### Estimates in original dataset
print(effect_imp(toimpute,10,quiet=FALSE))

### colnames(estimates_imp)[colSums(is.na(estimates_imp)) > 0]

### Bootstrap and get bootstrap estimates
effect_bootresult1 <- matrix(NA, nrow=100, ncol=267)
set.seed(14)
allseeds <- sample(1:10000,200,replace=FALSE)
allseeds <- matrix(allseeds, ncol=2, nrow=100, byrow=TRUE)
for(i in 1:100){
  print(paste0("BOOTSTRAP SAMPLE ",i))
  bootdata <- bootfunc_strat(dataset=toimpute,setseed=allseeds[i,1])
  effect_bootresult1[i,] <- effect_imp(dataset=bootdata,setseed1=allseeds[i,2])
}

write.csv(effect_bootresult1,".../BMSManalysis_pt1.csv")
