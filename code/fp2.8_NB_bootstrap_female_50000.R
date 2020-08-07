### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

### This program calculates Net Benefit in the validation cohort for each model derived on cohorts of sample size 50000 
### It could be done in the main simulation files (fp2.1 - fp2.5), but it was a reviewer suggested addition and I did not want to 
### re-run the entire simulation.

gender.var <- 1

library(dca)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)
library(Hmisc)

### First load the data (2016 cohort, development population, validation cohort)
load("R_out_C4/female_datasets_loaded_sample_size.RData")

### PART A: DEFINE FUNCTIONS, STORAGE DATA FRAMES AND QUANTITES TO BE USED IN THE SIMULATION ###
### PART A: DEFINE FUNCTIONS, STORAGE DATA FRAMES AND QUANTITES TO BE USED IN THE SIMULATION ###
### PART A: DEFINE FUNCTIONS, STORAGE DATA FRAMES AND QUANTITES TO BE USED IN THE SIMULATION ###


## Create a vector to store the net benefits
NB.bootstrap <- rep(0,1000)

## Next define function for creating risks for a given model, and new set of data to calculate risks for
create_risks<-function(model,new.dat){
  
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(patid,cens_fake, type='right') ~ age + BMI + Cholesterol_HDL_Ratio + Famhis_lstrict + Hypertension+ 
                    SBP+ Smoking + T2dia+ Townsend, data=new.dat)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  ## Extract the cumulative hazard at 10 years
  ## Note there will not always be a person with 3652 days follow up, and therefore the hazard function
  ## may be undefined at this point in the R function. It is however defined mathematically, as the cumulative hazard
  ## is constant until the next event. We therefore take the hazard as the nearest time point prior to 3653, to be
  ## the cumulative hazard at 10 years
  basehaz10y<-basehaz$hazard[which.max(basehaz$time[basehaz$time < 3653])]
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}


## Before going into the loop, we want to calculate the observed risk for the calibration cohort - "km.risk.valid"
## When calculating the calibration in the large for each model, this quantity will be the same everytime, as the observed
## risk in the calibration cohort will not change. Therefore better to calculate it now, and store it for use later

## Start by generating a KM curve in the validation cohort
valid.survfit <- survfit(Surv(CVD_time,CVD_cens_R, type='right') ~ 1,
                         data=data_valid)

## km.survfit may not have a 3652 value, so find a value that it does have (similar process to above, when deriving predicted
## risks at 3653, find the closest time prior to 3653 at which it is defined)
km.risk.valid <- 1-valid.survfit$surv[which.max(valid.survfit$time[valid.survfit$time < 3653])]



### PART B: RUN THE SIMULATION ###
### PART B: RUN THE SIMULATION ###
### PART B: RUN THE SIMULATION ###

## Set seed
set.seed(500)

### Create a loop in which we sample the development population 1000 times, and each time calculate the required statistics
for (k in 1:1000){
  
  ## Want to reduce development data to a random subset of 50000 patients, called random.dat
  ## Extract 50000 row numbers at random
  row.ids <- sample(1:nrow(data_devel),50000, replace = FALSE)
  random.dat <- data_devel[row.ids,]
  
  ## Fit a model on this sampled data
  fit_data<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ age + BMI + Cholesterol_HDL_Ratio + Famhis_lstrict + Hypertension+ 
                    SBP+ Smoking + T2dia+ Townsend, data=random.dat)
  
  
  ### 1. Now want to calculate risks in the validation cohort, using the above model
  ### 1. Now want to calculate risks in the validation cohort, using the above model
  
  ## Generate risks for the 2016 cohort using the above defined function
  risk.all<-1-create_risks(fit_data, data_valid)$lin.pred
  
  data.for.dca <- data_valid
  data.for.dca$risk <- risk.all

  dca.out <- stdca(data = data.for.dca, outcome = "CVD_cens_R", ttoutcome = "CVD_time", 
                   timepoint = 3652, predictors = "risk", xstart = 0.1, xstop = 0.105, graph = FALSE)

  NB.bootstrap[k] <- dca.out$net.benefit$risk
  
  print(Sys.time())
  print(k)}


rm(list=setdiff(ls(),list("NB.bootstrap")))

save.image("R_out_C4/NB_bootstrap_female_50000.RData")
print("saved")
