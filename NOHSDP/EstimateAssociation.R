####################################################################################
### State-Specific Associations between no high school diploma and demographic covariates
### R version: 4.1.2 and "lme4" package version: 1.1-35.1
####################################################################################

rm(list=ls()); gc()
library(dplyr)
library(lmerTest)

load("Preprocessing/AggregatedSDoH_data.RData")

head(df.sdoh)

# combine age variables
df.sdoh <- df.sdoh %>% mutate(Age75over=Age75_84+Age85over, OTHERRACE=NHPI+OTHERRACE) %>%
  select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
         Male, Female, HISP, WHITE, AFAM, ASIAN, OTHERRACE, TWOMORE,
         Age25_34, Age35_44, Age45_54, Age55_64, 
         Age65_74, Age75over, 
         NOHSDP)

# remove Census tracts whose NOHSDP is NA.
dat.sdoh=df.sdoh %>% filter(!is.na(NOHSDP))

# remove Male, White and Age55_64 dummy. It is a reference group.
dat.sdoh <- dat.sdoh %>% select(-Male, -WHITE, -Age55_64)

# variables
sdoh.var <- c("Female", "HISP", "AFAM", "ASIAN", "OTHERRACE", "TWOMORE",
              "Age25_34", "Age35_44", "Age45_54", #"Age55_64",
              "Age65_74", "Age75over",
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

#--- fit the mixed effect model
# a=Sys.time()
fml<- paste0("yhat ~", paste0(independentVariableIndices, collapse = "+"), "+(",
             paste0(independentVariableIndices, collapse = "+"),"|STATE)")
print(fml)
fit.x =lme4::lmer(formula=as.formula(fml),weights = what, data=dat.sdoh, 
                  control=lmerControl(calc.derivs = FALSE))
# b=Sys.time()
# print(b-a)

# check the model
#summary(fit.x)

# coefficients
fixef(fit.x)
ranef(fit.x)$STATE
coef(fit.x)$STATE

save.image(file="NOHSDP/Association.RData")

