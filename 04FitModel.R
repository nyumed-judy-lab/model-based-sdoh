# Agegroups: "Age25_34, Age35_44, Age45_54, Age55_64, Age65_74, Age75-84, Age85over
rm(list=ls()); gc()
library(dplyr)
library(corrplot)
library(lme4)
library(ggplot2)

load("01BerksonEstimate-byState/RData/02AggregatedSDoH_data.RData")
load("06ModelSelection/RData/011SelectedModelInfo-ML.RData")

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
ref.var <- "STATE"
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
alpha.mat.x <- fit.x<- list()


#for(i in seq(state)){
#  dat.st<- dat.sdoh %>% filter(STATE==state[i])
alpha.mat.x<-matrix(NA, nrow=p.z, ncol=p-p.z+1)
rownames(alpha.mat.x)<-tmp1[(p-p.z+1):p]
#colnames(alpha.mat.x)<-c("(Intercept)",paste0("factor(STATE)",state.tb$STATE), tmp1[2:(p-p.z)])
colnames(alpha.mat.x)<-c("(Intercept)",tmp1[1:(p-p.z)])
#fit.x[[i]]<- list()
fit.x<- ranef.mat <- list()

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


# fit.x[[j]] = lm(formula = as.formula(paste("yhat ~", paste(independentVariableIndices[[j]], collapse = "+"), sep = "" )), 
#                 weights=what, data = dat.temp) 
# fit.x[[j]] =lme4::lmer(formula=as.formula(paste0("yhat ~", paste0(independentVariableIndices[[j]], collapse = "+"), "+(1|STATE)")),
#                        weights = what, data=dat.temp)
selected.fml
a=Sys.time()
fml=paste0("yhat ~", paste0(selected.fixed, collapse = "+"), "+(",paste0(selected.random, collapse = "+"),"|STATE)")
fit.x[[j]] =lme4::lmer(formula=as.formula(fml),weights = what, data=dat.temp, control=lmerControl(calc.derivs = FALSE,
                                                     optCtrl = list(maxfun = 5e5)))
b=Sys.time()
print(b-a)
tmp<- fixef(fit.x[[j]])
ranef.mat[[j]]<- ranef(fit.x[[j]])
alpha.mat.x[dependentVariable,match(names(tmp),colnames(alpha.mat.x))]<-tmp

save.image("06ModelSelection/RData/020Berkson-estimate-mixedeffect-MLselected.RData")

#rownames(ranef.mat[[1]]$STATE)<- state.tb$ST_ABBR[match(rownames(ranef.mat[[1]]$STATE), state.tb$STATE)]
#save(alpha.mat.x,ranef.mat, sdoh.var, z, x, independentVariableIndices, file="04BerksonEstimate-mixedeffect-weightupdate/RData/010Berkson-estimate-mixedeffect-fullmodel-default1e5-upload.RData")



#load("04BerksonEstimate-mixedeffect-weightupdate/RData/040Berkson-estimate-nointeraction-mixedeffect-default1e5.RData")

#----- lmerTest in r
# library(lmerTest)
# a1=Sys.time()
# # fixed effect
# fixef.test<- drop1(fit.x[[1]]) # Using Satterthwaite degrees of freedom
# # if(requireNamespace("pbkrtest", quietly = TRUE))
# #   drop1(fit.x[[1]], ddf="Kenward-Roger") # Alternative DenDF and F-test method
# # drop1(fit.x[[1]], ddf="lme4", test="Chi") # Asymptotic Likelihood ratio tests
# b1=Sys.time()
# print(b1-a1)
# 
# save.image("04BerksonEstimate-mixedeffect-weightupdate/RData/010Berkson-estimate-mixedeffect-fullmodel-default1e5-randomtestresult.RData")
