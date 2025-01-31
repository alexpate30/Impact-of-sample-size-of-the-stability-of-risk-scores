### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

### In this I am going to calculate the population derived risks, that is risks derived from a model developed on the entire development population
### I will also calculate the calibration in the large of the validation dataset when using a model derived from the entire
### development popluation, i.e. the model used for population derived risks.

library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(Hmisc)
library(reshape2)

### First load the 2016 data
load("R_out_C4/female_datasets_loaded_sample_size.RData")

### 1. First do population derived risks for 2016 cohort
### 1. First do population derived risks for 2016 cohort

## First define function for creating risks for a given model, and new set of data to calculate risks for
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

## Now fit the model on the entire development population
## Analyse the imputed datasets
fit_data_population<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ age + BMI + Cholesterol_HDL_Ratio + Famhis_lstrict + Hypertension+ 
                           SBP+ Smoking + T2dia+ Townsend, data=data_devel)

## Now calculate risks for each of the 2016 cohort
risks.all.population <- 1-create_risks(fit_data_population,data.2016)$lin.pred



### 2. Now calculate calibration in the large for this model
### 2. Now calculate calibration in the large for this model

## Start by generating a KM curve in the validation cohort
valid.survfit <- survfit(Surv(CVD_time,CVD_cens_R, type='right') ~ 1,
                         data=data_valid)

## km.survfit may not have a 3652 value, so find a value that it does have (similar process to above, when deriving predicted
## risks at 3653, find the closest time prior to 3653 at which it is defined)
km.risk.valid <- 1-valid.survfit$surv[which.max(valid.survfit$time[valid.survfit$time < 3653])]


## Next generate risks for people in the validation cohort
risks.all.valid <- 1-create_risks(fit_data_population,data_valid)$lin.pred
pred.risk.mean <- mean(risks.all.valid)


## Calculate difference between this and the observed risk in the validation cohort
calibration.in.large.population <- pred.risk.mean - km.risk.valid



### 3. Now calculate the Harrels C for this model
### 3. Now calculate the Harrels C for this model

## Generate linear predictors for this dataset using the model (note we dont need risks, just linear predictor, so can sue R function predict)
pred.model <- predict(fit_data_population, newdata= data_valid)

## Calculate Harrels C and store it
HarrelsC.population <- 1-rcorr.cens(x=pred.model,S=Surv(data_valid$CVD_time,data_valid$CVD_cens_R))["C Index"]



### 4. Now want to do a calibration plot by decile, for all the patients in the development cohort, and calculate calibration
### 4. Now want to do a calibration plot by decile, for all the patients in the development cohort, and calculate calibration

# ### This is to make sure the miss-claibration we see in validation cohort is not a random cohort selection issue
# 
# ## Create a variable that groups per 10th of predicted risk
# risks.devel <- 1-create_risks(fit_data_population,data_devel)$lin.pred
# 
# centile<-as.integer(cut(risks.devel, 
#                                  breaks=quantile(risks.devel,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
#                                  )))
# 
# data_devel$risks <- risks.devel
# data_devel$centile <- centile
# 
# 
# ## Now calculate the kaplan meier survival for each of these subgroups
# cl <- makeCluster(11)
# registerDoParallel(11)
# km.overall<-(foreach(input=c(1,2,3,4,5,6,7,8,9,10), .combine=list, .multicombine=TRUE, 
#                      .packages=c("dplyr","mice","tidyr","survival"))
#              %dopar%{temp.dat<-subset(data_devel,centile==input)
#                      temp<-survfit(Surv(CVD_time,CVD_cens_R, type='right') ~ 1,
#                                    data=temp.dat)
#                      # temp$time does not always have a 3653 value, so find a value that it does have
#                      numbers<-3652
#                      range<-0
#                      arbitrary.numbers<-sort(temp.dat$CVD_time)
#                      nearest<-findInterval(numbers, arbitrary.numbers - range)
#                      return(1-temp$surv[temp$time==arbitrary.numbers[nearest]]) 
#              })
# stopCluster(cl)
# 
# km.overall[[1]]
# km.overall[[2]]
# km.overall[[3]]
# km.overall[[4]]
# km.overall[[5]]
# km.overall[[6]]
# km.overall[[7]]
# km.overall[[8]]
# km.overall[[9]]
# km.overall[[10]]
# 
# km.all<-unlist(km.overall)
# 
# ## Now need to calculate the average predicted risk per group
# cl <- makeCluster(11)
# registerDoParallel(11)
# predrisk.overall<-(foreach(input=c(1,2,3,4,5,6,7,8,9,10), .combine=list, .multicombine=TRUE, 
#                            .packages=c("dplyr","mice","tidyr","survival"))
#                    %dopar%{temp<-subset(data_devel,centile==input)
#                            return(mean(temp$risks))
#                            
#                    })
# stopCluster(cl)
# 
# predrisk.overall[[1]]
# predrisk.overall[[2]]
# predrisk.overall[[3]]
# predrisk.overall[[4]]
# predrisk.overall[[5]]
# predrisk.overall[[6]]
# predrisk.overall[[7]]
# predrisk.overall[[8]]
# predrisk.overall[[9]]
# predrisk.overall[[10]]
# 
# predrisk.all<-unlist(predrisk.overall)
# 
# km.all
# predrisk.all
# 
# # Now make a data frame with everything I want to plot
# plot.data<-data.frame(xvals=1:10,km.all,predrisk.all)
# 
# ## All patients
# temp.dat<-select(plot.data,c("xvals","km.all","predrisk.all"))
# colnames(temp.dat)<-c("xvals","observed","predicted")
# 
# # Now reshapre into long format
# temp.dat<-melt(temp.dat,id="xvals")
# 
# # Now plot
# p<-ggplot(temp.dat) 
# p+ geom_point(aes(x=xvals,y=value,shape=variable)) + scale_shape_manual(values=c(1,19)) + ggtitle("Female - development cohort")



rm(list=setdiff(ls(),list("risks.all.population","calibration.in.large.population","HarrelsC.population")))

save.image("R_out_C4/generate_risks_calibration_population_female.RData")

