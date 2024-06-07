####################################################################################
### Predict (calibrated) Model-based SDoH
### Individuals' Demographic data are required.
####################################################################################

rm(list=ls()); gc()
library(lmerTest)
library(dplyr)
library(data.table)

#----- Define a function for Prediction
# data: individual data with selected demographic variables
# alpha.m: estimated associations (model coefficients)
# ind.var: names of independent variables (predictors)
# dep.var: name of dependent variable. e.g. NOHSDP
get.pred <- function(data, alpha.m, ind.var, dep.var){
  xtrue <- data[, ind.var]
  # SDoH Prediction
  pred <- pnorm(cbind(rep(1, nrow(xtrue)), as.matrix(xtrue)) %*% as.matrix(alpha.m))
  # Calibrated Model-based SDoH
  cali.pred <- pred * mean(data[[paste0(dep.var,"bar")]])/mean(pred)
  cali.pred[cali.pred>1]<- 1
  return(data.frame(person_id=data$person_id, pred=as.numeric(pred), cali.pred=as.numeric(cali.pred)))
}


#----- Predict individual-level SDoH

#--- Load final SDoH Model
load("NOHSDP/FinalModel.RData")

#--- Load your own individual demographic data
df.individual <- fread("yourdata")

#--- Predict individual-level SDoH by states and residential units.
# Replace the first element in selected.fixed
selected.fixed[1] <- "(Intercept)"

# Initialize a list to store predictions
hat.list <- list()

# Loop through each state
for(st in state.tb$ST_ABBR){
  alpha.m <- t(mixef.mat[st, selected.fixed]) # The model coefficients for the current state
  dat.tmp <- df.individual %>% filter(ST_ABBR==st) # Filter individual data for the current state
  state.zip=unique(dat.tmp$zip) # Get unique zip codes for the current state
  hat.list[[st]] <- list() # Initialize a list to store predictions for each zip code
  
  # Loop through each zip code
  for(i in seq(state.zip)){
    zip.nm=state.zip[i]
    dat.ttmp <- dat.tmp %>% filter(zip==zip.nm) # Filter individual data for the current zip code
    # Predict SDoH for individuals in the current zip code
    hat.list[[st]][[i]] <- get.pred(dat.ttmp, alpha.m, gsub("bar", "", selected.fixed[-1]), z)
    rm(dat.ttmp);gc()
  }
  rm(alpha.m, dat.tmp, state.zip);gc()
}

# Combine all predictions into a single dataframe
pred.temp <- lapply(hat.list, function(x) do.call(rbind, x))
pred.df=do.call(rbind, pred.temp)

# pred.df is a dataframe with columns: 
# person_id, pred (model-based SDoH), and cali.pred (calibrated model-based SDoH)