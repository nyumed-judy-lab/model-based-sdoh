####################################################################################
### Estimated weighted correlation and their standard deviations with
### the 'one-standard-error' rule for variable selection.
####################################################################################

rm(list=ls()); gc()
library(dplyr)
library(lmerTest)
library(ggplot2)
library(parallel)

mycolor=c("dodgerblue2", "orange", "forestgreen", "violetred", "ivory4", "red2", "black")

load("NOHSDP/VariableSelection-foldsinfo.RData")
load("NOHSDP/VariableSelection.RData")
k=length(M.list)

#--- check the model at the last step
summary(M.list[[k]][[1]])
sapply(M.list[[k]], function(x) isSingular(x))
sapply(M.list[[k]], function(x) is.null(x@optinfo$conv$lme4)) # should be TRUE

#--- extract fixed and random effects from updated model at each step
str<- lapply(seq(k), function(l) as.character(formula(M.list[[l]][[1]]))[3])
current.fixed <- lapply(str, function(x) substr(x, 1, regexpr("\\(", x)-1))
current.fixed<- lapply(current.fixed, function(x) strsplit(x, split=' + ', fixed=TRUE)[[1]])
num.fixed<- sapply(current.fixed, length)-1

current.random <- lapply(str, function(x) substr(x, regexpr("\\(", x)+1, nchar(x)))
current.random <- lapply(current.random, function(x) gsub(" \\| STATE)", "", x))
current.random<- lapply(current.random, function(x) strsplit(x, split=' + ', fixed=TRUE)[[1]])
num.random<- sapply(current.random, length)-1

current.fixed[[k]]
current.random[[k]]

#--- get Estimated weighted correlation in validation set
validation<- lapply(folds, function(x) dat.sdoh[x,])

truey=validation$yhat
weights = validation$what


e.list.nw <- list()
for(iter.k in seq(k)){
  
  predicted=mclapply(seq(n.folds), function(l) predict(M.list[[iter.k]][[l]], newdata=validation[[l]]))
  
  # MSE, weighted MSE, weighted Correlation and correlation from updated model at each step
  e.list.nw[[iter.k]] <- list()
  
  e.list.nw[[iter.k]]$WMSE <- sapply(seq(n.folds), function(l) 
    sum(validation[[l]]$what*(validation[[l]]$yhat-predicted[[l]])**2)/sum(validation[[l]]$what))
  e.list.nw[[iter.k]]$MSE <- sapply(seq(n.folds), function(l) 
    mean((validation[[l]]$yhat-predicted[[l]])**2))
  e.list.nw[[iter.k]]$WCor <-sapply(seq(n.folds), function(l) 
    cov.wt(data.frame(validation[[l]]$yhat, predicted[[l]]), wt = validation[[l]]$what, cor = TRUE)$cor[1,2])
  e.list.nw[[iter.k]]$Cor <-sapply(seq(n.folds), function(l) cor(validation[[l]]$yhat,predicted[[l]]))
  rm(predicted);gc()
}


#--- make the dataframe
dat<- data.frame(name=paste0("(",num.fixed,",", num.random,")"),
                 AIC=unlist(sapply(AIC.list, mean)),
                 sd.AIC=sapply(AIC.list, sd),
                 MSE=sapply(e.list.nw, function(x) mean(x$MSE)),
                 sd.MSE=sapply(e.list.nw, function(x) sd(x$MSE)),
                 WMSE=sapply(e.list.nw, function(x) mean(x$WMSE)),
                 sd.WMSE=sapply(e.list.nw, function(x) sd(x$WMSE)),
                 Cor=sapply(e.list.nw, function(x) mean(x$Cor)),
                 sd.Cor=sapply(e.list.nw, function(x) sd(x$Cor)),
                 WCor=sapply(e.list.nw, function(x) mean(x$WCor)),
                 sd.WCor=sapply(e.list.nw, function(x) sd(x$WCor)))
dat$name <- factor(dat$name, levels=dat$name)

dat %>% mutate_if(is.numeric, round, 3)

# check the AIC plot
# ggplot(dat, aes(x=name, y=AIC, group=1)) + geom_point() + geom_line() + 
#   xlab("the number of (fixed effects, random effects) variables") + ylab("AIC")


#--- Estimated Weighted Correlation Plot

# the best model in terms of the estimated weighted correlation
op.idx=which.max(dat$WCor)
#one standard deviation below the estimated weighted correlation of the best model.
yline<- dat$WCor[op.idx]-dat$sd.WCor[op.idx]
#the most parsimonious model within one standard deviation of the best model
xline<- min(which(dat$WCor>yline))

### Supplementary Figure 2
p.nohsdp <- ggplot(dat, aes(x=name, y=WCor, group=1)) + geom_point() + geom_line() + 
  geom_errorbar(aes(x=name, ymin=WCor-sd.WCor, ymax=WCor+sd.WCor), position=position_dodge(.9),
                width=0.2, colour=mycolor[1], alpha=0.9, linewidth=0.4)+
  geom_hline(yintercept=yline, linetype="dashed", color = mycolor[3]) +
  geom_vline(xintercept=xline, linetype="dashed", color = mycolor[6]) +
  xlab("The number of (fixed effects, random effects) covariates")+ ylab("Weighted correlation")+
  labs(title = "A")+
  theme(text=element_text(size=12))+
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 90))
#p.nohsdp

png("fig/NOHSDPEstimatedCorrelation.png", units="in", height=6.0*0.6, width=3.8*1.6, res=300)
p.nohsdp
dev.off()

#--- save the final selected covariates information
selected.fml=formula(M.list[[xline]]$Fold01)
selected.fixed=current.fixed[[xline]]
selected.random=current.random[[xline]]
save(selected.fml, selected.fixed, selected.random, file="NOHSDP/SelectedModelInfo.RData")
