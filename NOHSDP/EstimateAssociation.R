# Agegroups: "Age25_34, Age35_44, Age45_54, Age55_64, Age65_74, Age75over
rm(list=ls()); gc()
library(dplyr)
library(corrplot)
#library(lme4)
library(ggplot2)
library(caret)
library(parallel) # parallel version of lapply
library(lmerTest)

load("01BerksonEstimate-byState/RData/02AggregatedSDoH_data.RData")

head(df.sdoh)
df.sdoh <- df.sdoh %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
                              Male, Female, HISP, WHITE, AFAM, ASIAN, NHPI, OTHERRACE, TWOMORE,
                              Age20_24, 
                              Age25_34, Age35_44, Age45_54, Age55_59, Age60_64, 
                              Age65_74, Age75_84, Age85over, 
                              NOHSDP, UNINSUR)

#NHPI median = 0, OTHERRACE Median 0.0006 -> combine
# TWOMORE could be different than NHPI and OTHERRACE.
# Combine Age75_84 (Median 0.043) and Age85over (Median 015) 

# combine age variables
df.sdoh <- df.sdoh %>% mutate(Age55_64 = Age55_59+Age60_64, Age75over=Age75_84+Age85over,
                              OTHERRACE=NHPI+OTHERRACE) %>%
  select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
         Male, Female, HISP, WHITE, AFAM, ASIAN, OTHERRACE, TWOMORE,
         Age25_34, Age35_44, Age45_54, Age55_64, 
         Age65_74, Age75over, 
         NOHSDP, UNINSUR)


# remove 63 Census tracts that total population is less then 10.
dat.sdoh=df.sdoh %>% filter(TOTPOP>=10)
dat.sdoh=dat.sdoh %>% filter(!is.na(NOHSDP))

# rescaling age variables
scale <- function(x, denom) x/denom
dat.sdoh<- dat.sdoh %>% mutate(scale.age=Age25_34+Age35_44+Age45_54+Age55_64+ Age65_74+Age75over) %>%
  mutate( across(Age25_34:Age75over, ~ scale(.,scale.age))) %>% select(-scale.age)

#check their summation is 1.
summary(apply(dat.sdoh%>% select(Male, Female),1,sum))
summary(apply(dat.sdoh%>% select(contains("Age")),1,sum))
summary(apply(dat.sdoh%>% select(HISP, WHITE, AFAM, ASIAN, OTHERRACE, TWOMORE),1,sum))

# check standard deviation by state
#check.sd=dat.sdoh %>% group_by(ST_ABBR) %>% summarise(across(Male:Age75over, ~sd(., na.rm=TRUE)))
#  mutate(scale.age=Age25_34+Age35_44+Age45_54+Age55_59+Age60_64+ 
#                                          Age65_74+Age75_84)

dat.sdoh <- dat.sdoh %>% select(-Male, -WHITE)
#ref.var <- "STATE"
sdoh.var <- c("Female", "HISP", "AFAM", "ASIAN", "OTHERRACE", "TWOMORE",
              "Age25_34", "Age35_44", "Age45_54", "Age55_64",
              "Age65_74", "Age75over",
              "NOHSDP", "UNINSUR")
z <- c("NOHSDP", "UNINSUR")
x <- setdiff(sdoh.var,z)
p<-length(sdoh.var)
p.z<- length(z)

# check the number or tracts in each state
state.tb <- dat.sdoh%>% group_by(ST,STATE,ST_ABBR) %>% 
  summarise(n=n(), na.NOHSDP=sum(!is.na(NOHSDP)), na.UNINSUR=sum(!is.na(UNINSUR))) %>% ungroup()
#state.tb <- df.sdoh%>% select(ST, STATE, tract) %>% group_by(ST,STATE) %>% summarise(n.C=n()) %>% ungroup()
#state <- as.vector(state.tb$STATE)
#state_abb <- as.vector(state.tb$ST_ABBR)


dat.sdoh<- dat.sdoh %>% select(STATE, tract, all_of(sdoh.var), TOTPOP)
names(dat.sdoh)[-c(1:2)]<-c(paste0(sdoh.var,"bar"), "nk")

##############
#########
#Use only individual level observed Zs. no iterations needed.
tmp1<-paste0(sdoh.var,"bar")
independentVariableIndices <- list()
independentVariableIndices[[1]]<- paste0(x[!grepl("Age55", x)],"bar")
#xtmp<- x[!grepl(c("Age65"), x)]
#xtmp<- xtmp[!grepl(c("Age75"), xtmp)]
#independentVariableIndices[[2]]<- paste0(xtmp[!grepl(c("Age85"), xtmp)],"bar")
alpha.mat.x <- list()


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
rm(what, yhat, phat1, df.sdoh, dat.sdoh, tmp1, s2hat);gc()



# fit.x[[j]] = lm(formula = as.formula(paste("yhat ~", paste(independentVariableIndices[[j]], collapse = "+"), sep = "" )), 
#                 weights=what, data = dat.temp) 
# fit.x[[j]] =lme4::lmer(formula=as.formula(paste0("yhat ~", paste0(independentVariableIndices[[j]], collapse = "+"), "+(1|STATE)")),
#                        weights = what, data=dat.temp)
a=Sys.time()
fml<- paste0("yhat ~", paste0(independentVariableIndices[[j]], collapse = "+"), "+(",paste0(independentVariableIndices[[j]], collapse = "+"),"|STATE)")
print(fml)
fit.x[[j]] =lme4::lmer(formula=as.formula(fml),weights = what, data=dat.temp, control=lmerControl(calc.derivs = FALSE,
                                                                                                  optCtrl = list(maxfun = 6e1)))
#fit.x[[j]] =lme4::lmer(formula=as.formula(
#  paste0("yhat ~1+(",paste0(independentVariableIndices[[j]], collapse = "+"),"|STATE)")
#),weights = what, data=dat.temp, control=lmerControl(calc.derivs = FALSE))
b=Sys.time()
print(b-a)
tmp<- fixef(fit.x[[j]])
ranef.mat[[j]]<- ranef(fit.x[[j]])
alpha.mat.x[dependentVariable,match(names(tmp),colnames(alpha.mat.x))]<-tmp

rownames(ranef.mat[[1]]$STATE)<- state.tb$ST_ABBR[match(rownames(ranef.mat[[1]]$STATE), state.tb$STATE)]
#rownames(ranef.mat[[2]]$STATE)<- state.tb$ST_ABBR[match(rownames(ranef.mat[[2]]$STATE), state.tb$STATE)]
#save(alpha.mat.x,ranef.mat, sdoh.var, z, x, independentVariableIndices, file="06ModelSelection/RData/030Berkson-estimate-fullmodel-rescaling.RData")
save.image(file="06ModelSelection/RData/030Berkson-estimate-fullmodel-rescaling.RData")

getME(fit.x[[1]], name="Lambdat")[1:13,1:13]
anova(fit.x[[1]])
summary(fit.x[[1]])


# rescaling: singular


#-----old version ----------------------------------------------------------

# Fullmodel (upload) - Full
# Age45_54bar make the singularity problem.


# Fullmodel (upload2) - Full model with Age75over
# Age75over make the singularity problem.


# Fullmodel (upload3) - Full model with Age65over
# Age35_44 make the singularity problem.

# Fullmodel (upload4) - Full model with OTHERRACE=OTHERRACE+NHPI
# Age65_74, Age75_84, Age 85over make the singularity problem.

# Fullmodel (upload5) - Full model with age reference Age 45_54, Age 55_64
# Age85overbar make the singularity problem.

# #----- lmerTest in r
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
