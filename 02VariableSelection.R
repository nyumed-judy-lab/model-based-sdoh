# Agegroups: "Age25_34, Age35_44, Age45_54, Age55_64, Age65_74, Age75-84, Age85over
rm(list=ls()); gc()
library(dplyr)
library(corrplot)
#library(lme4)
library(ggplot2)
library(caret)
library(parallel) # parallel version of lapply

load("01BerksonEstimate-byState/RData/02AggregatedSDoH_data.RData")

head(df.sdoh)
df.sdoh <- df.sdoh %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
                              Female, HISP, AFAM, ASIAN, NHPI, OTHERRACE, TWOMORE,
                              Age20_24, 
                              Age25_34, Age35_44, Age45_54, Age55_59, Age60_64, 
                              Age65_74, Age75_84, Age85over, 
                              NOHSDP, UNINSUR)
# combine age variables
df.sdoh <- df.sdoh %>% mutate(Age55_64 = Age55_59+Age60_64) %>% 
  select(-Age55_59, -Age60_64)

# remove 63 Census tracts that total population is less then 10.
dat.sdoh=df.sdoh %>% filter(TOTPOP>=10)
#ref.var <- "STATE"
sdoh.var <- c("Female", "HISP", "AFAM", "ASIAN", "NHPI", "OTHERRACE", "TWOMORE",
              "Age20_24", "Age25_34", "Age35_44", "Age45_54", "Age55_64", 
              "Age65_74", "Age75_84", "Age85over", 
              "NOHSDP", "UNINSUR")
z <- c("NOHSDP", "UNINSUR")
x <- setdiff(sdoh.var,z)
p<-length(sdoh.var)
p.z<- length(z)

# check the number or tracts in each state
state.tb <- dat.sdoh%>% group_by(ST,STATE,ST_ABBR) %>% 
  summarise(n=n(), na.NOHSDP=sum(!is.na(NOHSDP)), na.UNINSUR=sum(!is.na(UNINSUR))) %>% ungroup()
#state.tb <- df.sdoh%>% select(ST, STATE, tract) %>% group_by(ST,STATE) %>% summarise(n.C=n()) %>% ungroup()
state <- as.vector(state.tb$STATE)
state_abb <- as.vector(state.tb$ST_ABBR)


dat.sdoh<- dat.sdoh %>% select(STATE, tract, all_of(sdoh.var), TOTPOP)
names(dat.sdoh)[-c(1:2)]<-c(paste0(sdoh.var,"bar"), "nk")

##############
#########
#Use only individual level observed Zs. no iterations needed.
tmp1<-paste0(sdoh.var,"bar")
independentVariableIndices <- list()
xtmp<- x[!grepl(c("Age20"), x)]
independentVariableIndices[[1]]<- paste0(xtmp[!grepl("Age55", xtmp)],"bar")
xtmp<- x[!grepl(c("Age65"), x)]
xtmp<- xtmp[!grepl(c("Age75"), xtmp)]
independentVariableIndices[[2]]<- paste0(xtmp[!grepl(c("Age85"), xtmp)],"bar")
alpha.mat.x <- list()


#for(i in seq(state)){
#  dat.st<- dat.sdoh %>% filter(STATE==state[i])
alpha.mat.x<-matrix(NA, nrow=p.z, ncol=p-p.z+1)
rownames(alpha.mat.x)<-tmp1[(p-p.z+1):p]
#colnames(alpha.mat.x)<-c("(Intercept)",paste0("factor(STATE)",state.tb$STATE), tmp1[2:(p-p.z)])
colnames(alpha.mat.x)<-c("(Intercept)",tmp1[1:(p-p.z)])
#fit.x[[i]]<- list()
#fit.x<- ranef.mat <- list()

#dependentVariable<-names(dat.sdoh)[5]
#for (j in seq(tmp1[(p-p.z+1):p])){
j=1
dependentVariable = tmp1[(p-p.z+1):p][j]
print(dependentVariable)
dat.temp=dat.sdoh[which(!is.na(dat.sdoh[,dependentVariable])),]
#dat.temp<- dat.st %>% filter(!is.na(eval(parse_expr(dependentVariable))))
#dat.temp<- dat.st %>% filter(!is.na(eval(dependentVariable)))
#independentVariableIndices<-tmp1[1:(p-p.z)]
#names(dat.sdoh)[independentVariableIndices]
phat<-dat.temp[,dependentVariable]

phat1<- phat
phat1[phat==0] =min(phat[phat!=0])/2 
phat1[phat==1] = (max(phat[phat!=1])+1)/2
rm(phat);gc()

yhat<-qnorm(phat1)
# if(sum(yhat==-Inf)>0){
#   yhat[yhat==-Inf]<-min(yhat[yhat!=-Inf])}
# if(sum(yhat==Inf)>0){
#   yhat[yhat==Inf]<-min(yhat[yhat!=Inf])}
s2hat<-phat1*(1-phat1)/(dnorm(yhat)*dnorm(yhat))/dat.temp$nk
what<-1/s2hat
#what[s2hat==0]<-0

# 
hist(what)
# 'comment out'
#what[what>quantile(what, 0.995)]<- quantile(what, 0.995)
#hist(what)

## update
dat.temp$what <- what
dat.temp$yhat <- yhat
rm(what, yhat, phat1, df.sdoh, dat.sdoh, xtmp, tmp1, s2hat, state, state_abb);gc()

#dat.temp<- dat.temp %>% filter(what!=0)

#######################################
#----- 10-fold cross validation --------------------------------------------------------
folds <- list()
n.folds <- 10
set.seed(22824)
folds <- createFolds(factor(dat.temp$STATE), k = n.folds, list = TRUE) # stratified by STATE
#table(dat.temp[folds[[1]],]$STATE)
#table(dat.temp[-folds[[1]],]$STATE)

train <- validation <- list()
validation<- lapply(folds, function(x) dat.temp[x,])
train<- lapply(folds, function(x) dat.temp[-x,])

#------------------------------------------------------------------------
library(lmerTest)

candidate.fixed <- independentVariableIndices[[1]]
candidate.random <- independentVariableIndices[[1]]
k=0
AIC.list<- BIC.list <- M.list <- Randvar.list <- list()
evaluation.list <- list()
#evaluation.list <- save.fixed.candidates <- save.random.candidates<- list()
#truey=validation$yhat

Null <- list()
Null[[1]]=mclapply(train, function(x) lm(formula=as.formula(paste0("yhat ~1")),weights=what, data=x))

Null[[2]]=mclapply(train, function(x) lme4::lmer(formula=as.formula(paste0("yhat ~1", "+(1|STATE)")),
                                                 weights=what, data=x, REML=FALSE,
                                                 control=lmerControl(calc.derivs = FALSE)))
AIC.candidate<- sapply(Null, function(x) mean(sapply(x, function(y) AIC(y))))
#save.candidates[[k]] <- list()
#save.candidates[[k+1]] <- Null

if(which.min(AIC.candidate)!=2){
  cat("!!! check the Null model !!")
}else{
  k=k+1
  M.list[[k]] <- Null[[which.min(AIC.candidate)]]
  AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
  BIC.list[[k]] <- sapply(M.list[[k]], function(y) BIC(y))
  
  predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
  
  evaluation.list[[k]] <- list()
  # MSE, weighted MSE, and correlation
  evaluation.list[[k]]$WMSE <- sapply(seq(n.folds), function(l) 
    sum(validation[[l]]$what*(validation[[l]]$yhat-predicted[[l]])**2)/sum(validation[[l]]$what))
  evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
    cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
  
  Randvar.list[[k]]<- NA
  current.fixed <- "1"
  current.random <- "1"
}

cat("Step0: \n")
print(formula(M.list[[k]][[1]]))
print(sapply(evaluation.list[[k]], mean))

#iter=7
#load(paste0("06ModelSelection/RData/010ModelSelection-AIC-ML_iter",iter,".RData"))


for(iter in seq(candidate.fixed)){
#for(iter in 1:8){
#for(iter in 8:13){
  
  cat("* ", iter, "th iteration\n")

  Fixed <- list()
  for(i in seq(candidate.fixed)){
    cat(i , "th fixed variable out of", length(candidate.fixed) ,"\n")
    fml <- paste0("yhat ~",paste0(current.fixed, collapse = "+"),"+",candidate.fixed[i], 
                  "+(", paste0(current.random, collapse = "+"),"|STATE)")
    Fixed[[i]] <- mclapply(train, function(x) 
      lme4::lmer(formula=as.formula(fml),weights=what, data=x, REML=FALSE,
                 control=lmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 5e5))))
    rm(fml); gc()
  }
  
  AIC.fixed=sapply(Fixed, function(x) mean(sapply(x, function(y) AIC(y))))
  converge.check = sapply(Fixed, function(x) sum(sapply(x, function(y) is.null(y@optinfo$conv$lme4))))
  AIC.fixed[converge.check<n.folds]<- NA
  singular.check = sapply(Fixed, function(x) sum(sapply(x, function(y) isSingular(y))))
  AIC.fixed[singular.check>0]<- NA
  idx=which.min(AIC.fixed)
  cat("AIC.fixed: ", AIC.fixed,"\n")
  cat("converge.check: ", converge.check,"\n") # should be equal to n.folds
  cat("singular.check: ", singular.check,"\n") # should be equal to 0.
  #save.fixed.candidates[[iter]] <- Fixed
  
  if(length(idx)==0 | mean(AIC.list[[k]])<AIC.fixed[idx] | singular.check[idx]>0){ 
    print("!!!!! STOP HERE !!!!!")
    save.image(paste0("06ModelSelection/RData/010ModelSelection-AIC-ML_iter",iter,".RData"))
    break
  }else{
    current.fixed<- c(current.fixed, candidate.fixed[idx])
    candidate.fixed <- setdiff(candidate.fixed, current.fixed)
    k=k+1
    M.list[[k]] <- Fixed[[idx]]
    AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
    BIC.list[[k]] <- sapply(M.list[[k]], function(y) BIC(y))
    
    #save.candidates[[k]]$AIC <- AIC.fixed
    #save.candidates[[k]]$F_stat=sapply(Fixed, function(x) anova(x)[[4]])
  
    predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
    
    evaluation.list[[k]] <- list()
    # MSE, weighted MSE, and correlation
    evaluation.list[[k]]$WMSE <- sapply(seq(n.folds), function(l) 
      sum(validation[[l]]$what*(validation[[l]]$yhat-predicted[[l]])**2)/sum(validation[[l]]$what))
    evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
      cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
    
    Randvar.list[[k]]<- NA
    cat("-Fixed effect update: ",current.fixed[-1], "\n")
    
    
    cat("WMSE=", round(sapply(evaluation.list, function(x) mean(x$WMSE)),3), "\n")
    cat("WCor=", round(sapply(evaluation.list, function(x) mean(x$WCor)),3), "\n")
    
    rm(predicted);gc()  
  }
  cat("sd=",sd(evaluation.list[[k]]$WMSE),",", sd(evaluation.list[[k]]$WCor),"\n")
  rm(converge.check, singular.check); gc()
  #sd(evaluation.list[[k]]$WMSE); sd(evaluation.list[[k]]$WCor)
  
  fml <- paste0("yhat ~",paste0(current.fixed, collapse = "+"),"+(",
                paste0(current.random, collapse = "+"),"+",paste0(current.fixed[length(current.fixed)], collapse = "+"),"|STATE)")
  Random <- mclapply(train, function(x) 
    lme4::lmer(formula=as.formula(fml),
               weights=what, data=x, REML=FALSE,
               control=lmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 5e5))))
  #save.random.candidates[[iter]] <- Random
  
  converge.check = sum(sapply(Random, function(x) is.null(x@optinfo$conv$lme4)))
  singular.check = sum(sapply(Random, function(y) isSingular(y)))
  
  if(converge.check<n.folds){
    print("!Go to the next iteration: Singular !")
    save.image(paste0("06ModelSelection/RData/010ModelSelection-AIC-ML_iter",iter,".RData"))
    #break
    cat("* Result of ", iter,"th iteration:\n")
    print(formula(M.list[[k]][[1]]))
    next
  }
  if(singular.check>0){
    print("!Go to the next iteration: Convergence issue !")
    save.image(paste0("06ModelSelection/RData/010ModelSelection-AIC-ML_iter",iter,".RData"))
    #break
    cat("* Result of ", iter,"th iteration:\n")
    print(formula(M.list[[k]][[1]]))
    next    
  }
  
  #print(VarCorr(Random),comp="Variance")
  
  AIC.random=mean(sapply(Random, function(y) AIC(y)))
  AIC.comparison=c(AIC.fixed[idx], AIC.random)
  if(which.min(AIC.comparison)==2){
    current.random=c(current.random, current.fixed[length(current.fixed)])
    k=k+1
    M.list[[k]] <- Random
    AIC.list[[k]] <- sapply(M.list[[k]], function(y) AIC(y))
    BIC.list[[k]] <- sapply(M.list[[k]], function(y) BIC(y))
    
    predicted=mclapply(seq(n.folds), function(l) predict(M.list[[k]][[l]], newdata=validation[[l]]))
    
    # MSE, weighted MSE, and correlation
    evaluation.list[[k]] <- list()
    # MSE, weighted MSE, and correlation
    evaluation.list[[k]]$WMSE <- sapply(seq(n.folds), function(l) 
      sum(validation[[l]]$what*(validation[[l]]$yhat-predicted[[l]])**2)/sum(validation[[l]]$what))
    evaluation.list[[k]]$WCor <-sapply(seq(n.folds), function(l) 
      cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
    
    Randvar.list[[k]] <- sapply(Random, function(x) unclass(VarCorr(x))$STATE[length(current.random),length(current.random)])
    cat("-Random effect update: ",current.random[-1], "\n")
    
    cat("WMSE=", round(sapply(evaluation.list, function(x) mean(x$WMSE)),3), "\n")
    cat("WCor=", round(sapply(evaluation.list, function(x) mean(x$WCor)),3), "\n")
    
    rm(predicted);gc()
  }#else if(which.min(AIC.comparison)==2){
  #  current <- Fixed[[which.min(AIC.fixed)]]
  #}
  cat("sd=",sd(evaluation.list[[k]]$WMSE),",", sd(evaluation.list[[k]]$WCor),"\n")
  
  cat("* Result of ", iter,"th iteration:\n")
  print(formula(M.list[[k]][[1]]))
  
  
  cat("-------------------------------------------------------------\n")
  
  save.image(paste0("06ModelSelection/RData/010ModelSelection-AIC-ML_iter",iter,".RData"))
  
  rm(Fixed, Random, AIC.fixed, AIC.random, idx, AIC.comparison, fml, converge.check, singular.check);gc()
}

#summary(M.list[[k]])
save.image("06ModelSelection/RData/010ModelSelection-AIC-ML.RData")

