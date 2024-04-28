####################################################################################
### 10-fold Cross-Validation with the 'one-standard-error' rule for variable selection.
### R version: 4.1.2 and "lme4" package version: 1.1-35.1
####################################################################################

rm(list=ls()); gc()
library(dplyr)
library(lmerTest)
library(caret)
library(parallel) # parallel version of lapply

load("Preprocessing/AggregatedSDoH_data.RData")

head(df.sdoh)
df.sdoh <- df.sdoh %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
                              Female, HISP, AFAM, ASIAN, NHPI, OTHERRACE, TWOMORE,
                              Age25_34, Age35_44, Age45_54, Age55_64, 
                              Age65_74, Age75_84, Age85over, 
                              NOHSDP)

# remove Census tracts whose NOHSDP is NA.
dat.sdoh=df.sdoh %>% filter(!is.na(NOHSDP))

# variables
sdoh.var <- c("Female", "HISP", "AFAM", "ASIAN", "NHPI", "OTHERRACE", "TWOMORE",
              "Age25_34", "Age35_44", "Age45_54", #"Age55_64", 
              "Age65_74", "Age75_84", "Age85over", 
              "NOHSDP")
z <- c("NOHSDP")
x <- setdiff(sdoh.var,z)
p<-length(sdoh.var)
p.z<- length(z)

# check the number or tracts in each state
state.tb <- dat.sdoh%>% group_by(ST,STATE,ST_ABBR) %>% 
  summarise(n=n(), na.NOHSDP=sum(!is.na(NOHSDP))) %>% ungroup()

dat.sdoh<- dat.sdoh %>% select(STATE, tract, all_of(sdoh.var), TOTPOP)
names(dat.sdoh)[-c(1:2)]<-c(paste0(sdoh.var,"bar"), "nk")

##############
#########
tmp1<-paste0(sdoh.var,"bar")
independentVariableIndices<- paste0(x,"bar")

dependentVariable = tmp1[(p-p.z+1):p]
print(dependentVariable)

#--- Calculate the weight
phat<-dat.sdoh[,dependentVariable]

phat1<- phat
phat1[phat==0] =min(phat[phat!=0])/2 
phat1[phat==1] = (max(phat[phat!=1])+1)/2
rm(phat);gc()

yhat<-qnorm(phat1)
s2hat<-phat1*(1-phat1)/(dnorm(yhat)*dnorm(yhat))/dat.sdoh$nk
what<-1/s2hat

#--- check the distribution of weight 
#hist(what)

#--- update dataframe
dat.sdoh$what <- what
dat.sdoh$yhat <- yhat
rm(what, yhat, phat1, df.sdoh, tmp1, s2hat);gc()

#######################################
#----- 10-fold cross validation --------------------------------------------------------
folds <- list()
n.folds <- 10
set.seed(22824)
folds <- createFolds(factor(dat.sdoh$STATE), k = n.folds, list = TRUE) # stratified by STATE
save(n.folds, folds, dat.sdoh, file=paste0("NOHSDP/VariableSelection-foldsinfo.RData"))

train <- validation <- list()
validation<- lapply(folds, function(x) dat.sdoh[x,])
train<- lapply(folds, function(x) dat.sdoh[-x,])

#------------------------------------------------------------------------
candidate.fixed <- independentVariableIndices
candidate.random <- independentVariableIndices
k=0
# save AIC, lmerMod and weighted correlation for updated models.
AIC.list<- M.list <- evaluation.list <- list()

#--- check if incorporating random effect improve the AIC
# REML=FALSE in variable selection with AIC.
Null <- list()
Null[[1]]=mclapply(train, function(x) lm(formula=as.formula(paste0("yhat ~1")),weights=what, data=x))

Null[[2]]=mclapply(train, function(x) lme4::lmer(formula=as.formula(paste0("yhat ~1", "+(1|STATE)")),
                                                 weights=what, data=x, REML=FALSE,
                                                 control=lmerControl(calc.derivs = FALSE)))
AIC.candidate<- sapply(Null, function(x) mean(sapply(x, function(y) AIC(y))))


if(which.min(AIC.candidate)!=2){
  cat("!!! check the Null model !!")
}else{
  k=k+1
  M.list[[k]] <- Null[[which.min(AIC.candidate)]]
  AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
  
  # predicted yhat in validation datasets
  predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
  
  evaluation.list[[k]] <- list()
  # Weighted correlation
  evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
    cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
  
  current.fixed <- "1"
  current.random <- "1"
}

cat("Step0: \n")
print(formula(M.list[[k]][[1]]))
print(sapply(evaluation.list[[k]], mean))

#--- Choose variable that achieves the lowest AIC at each step.

for(iter in seq(candidate.fixed)){
  
  cat("* ", iter, "th iteration\n")
  
  Fixed <- list()
  for(i in seq(candidate.fixed)){
    cat(i , "th candidate variable out of", length(candidate.fixed) ,"\n")
    fml <- paste0("yhat ~",paste0(current.fixed, collapse = "+"),"+",candidate.fixed[i], 
                  "+(", paste0(current.random, collapse = "+"),"|STATE)")
    Fixed[[i]] <- mclapply(train, function(x) 
      lme4::lmer(formula=as.formula(fml),weights=what, data=x, REML=FALSE,
                 control=lmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 5e5))))
    rm(fml); gc()
  }
  
  AIC.fixed=sapply(Fixed, function(x) mean(sapply(x, function(y) AIC(y))))
  converge.check = sapply(Fixed, function(x) sum(sapply(x, function(y) is.null(y@optinfo$conv$lme4))))
  AIC.fixed[converge.check<n.folds]<- NA # Exclude any covariate that causes a convergence issue.
  singular.check = sapply(Fixed, function(x) sum(sapply(x, function(y) isSingular(y))))
  AIC.fixed[singular.check>0]<- NA # Exclude any covariate that causes a singularity issue.
  idx=which.min(AIC.fixed)
  cat("AIC.fixed: ", AIC.fixed,"\n")
  cat("converge.check: ", converge.check,"\n") # should be equal to n.folds
  cat("singular.check: ", singular.check,"\n") # should be equal to 0.
  
  if(length(idx)==0){ 
    print("!!!!! STOP HERE !!!!!")
    #save.image(paste0("NOHSDP/VariableSelection-iter",iter,".RData"))
    break
  }else if(mean(AIC.list[[k]])<AIC.fixed[idx] | singular.check[idx]>0){
    print("!!!!! STOP HERE !!!!!")
    #save.image(paste0("NOHSDP/VariableSelection-iter",iter,".RData"))
    break
  }else{
    current.fixed<- c(current.fixed, candidate.fixed[idx])
    candidate.fixed <- setdiff(candidate.fixed, current.fixed)
    k=k+1
    M.list[[k]] <- Fixed[[idx]]
    AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
    
    # predicted yhat in validation datasets
    predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
    
    evaluation.list[[k]] <- list()
    # Weighted correlation
    evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
      cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
    
    cat("-Fixed effect update: ",current.fixed[-1], "\n")
    cat("WCor=", round(sapply(evaluation.list, function(x) mean(x$WCor)),3), "\n")
    
    rm(predicted);gc()  
  }
  #cat("sd=", sd(evaluation.list[[k]]$WCor),"\n")
  rm(converge.check, singular.check); gc()
  
  # Add the selected variable as the covariate for random effects if it results in a lower AIC.
  fml <- paste0("yhat ~",paste0(current.fixed, collapse = "+"),"+(",
                paste0(current.random, collapse = "+"),"+",paste0(current.fixed[length(current.fixed)], collapse = "+"),"|STATE)")
  Random <- mclapply(train, function(x) 
    lme4::lmer(formula=as.formula(fml),
               weights=what, data=x, REML=FALSE,
               control=lmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 5e5))))
  
  converge.check = sum(sapply(Random, function(x) is.null(x@optinfo$conv$lme4)))
  singular.check = sum(sapply(Random, function(y) isSingular(y)))
  
  # convergence and singularity check
  if(converge.check<n.folds){
    print("!Go to the next iteration: Singular !")
    #save.image(paste0("NOHSDP/VariableSelection-iter",iter,".RData"))
    cat("* Result of ", iter,"th iteration:\n")
    print(formula(M.list[[k]][[1]]))
    next
  }
  if(singular.check>0){
    print("!Go to the next iteration: Convergence issue !")
    #save.image(paste0("NOHSDP/VariableSelection-iter",iter,".RData"))
    cat("* Result of ", iter,"th iteration:\n")
    print(formula(M.list[[k]][[1]]))
    next    
  }
  
  # Does it improve AIC?
  AIC.random=mean(sapply(Random, function(y) AIC(y)))
  AIC.comparison=c(AIC.fixed[idx], AIC.random)
  if(which.min(AIC.comparison)==2){
    current.random=c(current.random, current.fixed[length(current.fixed)])
    k=k+1
    M.list[[k]] <- Random
    AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
    
    predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
    
    evaluation.list[[k]] <- list()
    # Weighted correlation
    evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
      cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
    
    cat("-Random effect update: ",current.random[-1], "\n")
    cat("WCor=", round(sapply(evaluation.list, function(x) mean(x$WCor)),3), "\n")
    
    rm(predicted);gc()
  }
  #cat("sd=", sd(evaluation.list[[k]]$WCor),"\n")
  
  cat("* Result of ", iter,"th iteration:\n")
  print(formula(M.list[[k]][[1]]))
  
  cat("-------------------------------------------------------------\n")
  
  #save.image(paste0("NOHSDP/VariableSelection-iter",iter,".RData"))
  rm(Fixed, Random, AIC.fixed, AIC.random, idx, AIC.comparison, fml, converge.check, singular.check);gc()
}

#summary(M.list[[k]])
save(M.list, AIC.list, Fixed, Random, current.fixed, current.random, iter, 
     file="NOHSDP/VariableSelection.RData")
