# Agegroups: "Age25_34, Age35_44, Age45_54, Age55_64, Age65_74, Age75-84, Age85over
rm(list=ls()); gc()
library(dplyr)
library(corrplot)
library(lme4)
library(ggplot2)
library(stats)
library(parallel) # parallel version of lapply

load("06ModelSelection/RData/010ModelSelection-AIC-ML_iter10.RData")
criteria=AIC.list
#load("04BerksonEstimate-mixedeffect-weightupdate/RData/020VariableSelection-BIC.RData")
#criteria=BIC.list
getME(M.list[[k]][[1]],"Lambdat")
summary(M.list[[k]][[1]])
sapply(M.list[[k]], function(x) isSingular(x))
sapply(M.list[[k]], function(x) is.null(x@optinfo$conv$lme4)) # should be TRUE

str<- lapply(seq(k), function(l) as.character(formula(M.list[[l]][[1]]))[3])
current.fixed <- lapply(str, function(x) substr(x, 1, regexpr("\\(", x)-1))
current.fixed<- lapply(current.fixed, function(x) strsplit(x, split=' + ', fixed=TRUE)[[1]])
num.fixed<- lapply(current.fixed, length)
num.fixed<- unlist(num.fixed)-1

current.random <- lapply(str, function(x) substr(x, regexpr("\\(", x)+1, nchar(x)))
current.random <- lapply(current.random, function(x) gsub(" \\| STATE)", "", x))
current.random<- lapply(current.random, function(x) strsplit(x, split=' + ', fixed=TRUE)[[1]])
num.random<- lapply(current.random, length)
num.random<- unlist(num.random)-1
#current.random <- c("1", "ASIANbar") # worse result than BIC based final model
#current.random<- current.random[diag(unclass(VarCorr(M.list[[k]]))$STATE)>0.1]#[length(current.random),length(current.random)]

current.fixed[[k]]
current.random[[k]]

# get weighted correlation
truey=validation$yhat
weights = validation$what


e.list.nw <- list()
for(iter.k in seq(k)){
  
  predicted=mclapply(seq(n.folds), function(l) predict(M.list[[iter.k]][[l]], newdata=validation[[l]]))
  
  e.list.nw[[iter.k]] <- list()
  # MSE, weighted MSE, and correlation
  e.list.nw[[iter.k]]$MSE <- sapply(seq(n.folds), function(l) 
    mean((validation[[l]]$yhat-predicted[[l]])**2))
  e.list.nw[[iter.k]]$Cor <-sapply(seq(n.folds), function(l) cor(validation[[l]]$yhat,predicted[[l]]))
  rm(predicted);gc()
}



dat<- data.frame(name=paste0("(",num.fixed,",", num.random,")"),
                 criteria=unlist(sapply(criteria, mean)),
                 sd.criteria=sapply(criteria, sd),
                 MSE=sapply(e.list.nw, function(x) mean(x$MSE)),
                 sd.MSE=sapply(e.list.nw, function(x) sd(x$MSE)),
                 WMSE=sapply(evaluation.list, function(x) mean(x$WMSE)),
                 sd.WMSE=sapply(evaluation.list, function(x) sd(x$WMSE)),
                 Cor=sapply(e.list.nw, function(x) mean(x$Cor)),
                 sd.Cor=sapply(e.list.nw, function(x) sd(x$Cor)),
                 WCor=sapply(evaluation.list, function(x) mean(x$WCor)),
                 sd.WCor=sapply(evaluation.list, function(x) sd(x$WCor)))
dat$name <- factor(dat$name, levels=dat$name)

ggplot(dat, aes(x=name, y=criteria, group=1)) + geom_point() + geom_line() + 
  #geom_errorbar(aes(x=name, ymin=criteria-sd.criteria, ymax=criteria+sd.criteria), position=position_dodge(.9),
  #              width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  #geom_vline(xintercept=which.min(dat$criteria), linetype="dashed", color = "red") + 
  xlab("the number of (fixed effects, random effects) variables") + ylab("AIC")

dat[which.min(dat$criteria),]


which(dat$WMSE - lag(dat$WMSE) >0)
which(dat$WCor - lag(dat$WCor) <0)

library(patchwork)
library(RColorBrewer)
library(grid)
#mycolor=brewer.pal(n = 11, name = "Spectral")
#mycolor=mycolor[c(10,5,9,2)]
mycolor=c("dodgerblue2", "orange", "forestgreen", "violetred", "ivory4", "red2", "black")
mylines=c("solid", "dashed","dotdash", "dotted", "longdash")

##### 1. MSE

op.idx=which.min(dat$MSE)
yline<- dat$MSE[op.idx]+dat$sd.MSE[op.idx]
xline<- min(which(dat$MSE<yline))

ggplot(dat, aes(x=name, y=MSE, group=1)) + geom_point() + geom_line() + 
  geom_errorbar(aes(x=name, ymin=MSE-sd.MSE, ymax=MSE+sd.MSE), position=position_dodge(.9),
                width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  geom_hline(yintercept=yline, linetype="dashed", color = mycolor[3]) +
  geom_vline(xintercept=xline, linetype="dashed", color = mycolor[6]) +
  xlab("the number of (fixed effects, random effects) variables")


##### 2. Weighted MSE

op.idx=which.min(dat$WMSE)
yline<- dat$WMSE[op.idx]+dat$sd.WMSE[op.idx]
xline<- min(which(dat$WMSE<yline))

ggplot(dat, aes(x=name, y=WMSE, group=1)) + geom_point() + geom_line() + 
  geom_errorbar(aes(x=name, ymin=WMSE-sd.WMSE, ymax=WMSE+sd.WMSE), position=position_dodge(.9),
                width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  geom_hline(yintercept=yline, linetype="dashed", color = mycolor[3]) +
  geom_vline(xintercept=xline, linetype="dashed", color = mycolor[6]) +
  xlab("the number of (fixed effects, random effects) variables")


##### 3. Correlation

op.idx=which.max(dat$Cor)
yline<- dat$Cor[op.idx]-dat$sd.Cor[op.idx]
xline<- min(which(dat$Cor>yline))

ggplot(dat, aes(x=name, y=Cor, group=1)) + geom_point() + geom_line() + 
  geom_errorbar(aes(x=name, ymin=Cor-sd.Cor, ymax=Cor+sd.Cor), position=position_dodge(.9),
                width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  geom_hline(yintercept=yline, linetype="dashed", color = mycolor[3]) +
  geom_vline(xintercept=xline, linetype="dashed", color = mycolor[6]) +
  xlab("the number of (fixed effects, random effects) variables")

##### 4. Weighted Correlation

op.idx=which.max(dat$WCor)
yline<- dat$WCor[op.idx]-dat$sd.WCor[op.idx]
xline<- min(which(dat$WCor>yline))

ggplot(dat, aes(x=name, y=WCor, group=1)) + geom_point() + geom_line() + 
  geom_errorbar(aes(x=name, ymin=WCor-sd.WCor, ymax=WCor+sd.WCor), position=position_dodge(.9),
                width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  geom_hline(yintercept=yline, linetype="dashed", color = mycolor[3]) +
  geom_vline(xintercept=xline, linetype="dashed", color = mycolor[6]) +
  xlab("the number of (fixed effects, random effects) variables")

dat %>% mutate_if(is.numeric, round, 3)

selected.fml=formula(M.list[[xline]]$Fold01)
selected.fixed=current.fixed[[xline]]
selected.random=current.random[[xline]]
save(selected.fml, selected.fixed, selected.random,
     file="06ModelSelection/RData/011SelectedModelInfo-ML.RData")

# 
# #--------------------------------------------------------------------------------------------#
# # Check 
# aa <- list()
# #WCorrelation <- vector()
# for(iter.f in seq(save.fixed.candidates[[5]])){
#   predicted=predict(save.fixed.candidates[[5]][[iter.f]], newdata=validation)
#   
#   weighted_corr <- cov.wt(data.frame(truey, predicted), wt = weights, cor = TRUE)
#   #WCorrelation<- c(WCorrelation , weighted_corr$cor[1,2])
# 
#   # MSE, weighted MSE, and correlation
#   aa[[iter.f]] <- c(mean((truey-predicted)**2), 
#                             sum(weights*(truey-predicted)**2)/sum(weights), 
#                             cor(truey, predicted),  weighted_corr$cor[1,2])
#   rm(predicted, weighted_corr);gc()
#   
# }
# 
# aa<- do.call(rbind, aa)
# colnames(aa) <- c("MSE", "WMSE", "Cor","WCor")
# which.min(aa[,"WMSE"])
# which.max(aa[,"WCor"])
