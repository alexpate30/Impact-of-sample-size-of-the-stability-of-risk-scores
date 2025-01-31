### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

### NOTE THIS CODE IS NOT REPRODUCIBLE ON THE FAKE DATASET
### RUNNING THROUGH THIS CODE ON THE REAL DATA LEAD TO N=1434 

library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)
library(Hmisc)


### First load the development dataset

## Calculate the model on the full dataset
fit_data<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ age + BMI + Cholesterol_HDL_Ratio + Famhis_lstrict + Hypertension+ 
                  SBP+ Smoking + T2dia+ Townsend, data=data_devel)


### Is criterion (i) satisfied?
### Is criterion (i) satisfied?

## First define n from model
n <- nrow(data_devel)
p <- 13

## First calculate likelihood ratio from the model
LR <- -2*(fit_data$loglik[1]-fit_data$loglik[2]) ##eq4

## Can now actually straight up calculate the shrinkage factor using an equation
S_VH <- 1-p/(LR) ##eq6
S_VH


## There is another way to calculate S_VH (they are exactly the same)
## Need to calculate R2_CS_APP for that
R2_CS_APP <- 1 - exp(-LR/n)
R2_CS_APP

S_VH2 <- 1 + p/(n*(log(1-R2_CS_APP)))
S_VH2

### Is criterion (ii) satisfied?
### Is criterion (ii) satisfied?

## This checks theat the sample size ensuers a small absolute difference between models apparaent and adjusted R2_NAGELKERKE
R2_CS_ADJ <- S_VH*R2_CS_APP ##eq8
R2_CS_APP_MAX <- 1 - exp(2*fit_data$loglik[1]/n) ##eq23

## Require that S_VH > R2_CS_ADJ/(R2_CS_ADJ + 0.05*R2_CS_APP_MAX) (##eq26)
S_VH
R2_CS_ADJ/(R2_CS_ADJ + 0.05*R2_CS_APP_MAX)

## It is true!


### Is criterion (iii) satisfied?
### Is criterion (iii) satisfied?

## Calculate number of years follow up
total.followup <- sum(random.dat$CVD_time)/365.25
## Calculate number of events
total.events <- sum(random.dat$CVD_cens_R)

## Calculate rate 
lambda <- total.events/total.followup

## Consider estimate at ten years, as that is what is important to us
t <- 10

## Assuming exponential hazard, then estimated cumulative incidence (risk) is 
av.risk <- 1-exp(-lambda*t)

# Upper bound is
upper.bound <- 1-exp(-(lambda + 1.96*sqrt(lambda/total.followup))*t)

# Lower bound is
lower.bound <- 1-exp(-(lambda - 1.96*sqrt(lambda/total.followup))*t)

upper.bound - av.risk
lower.bound - av.risk

## They are small, so everything is satisfied!!!!!!


### Now going to do the three steps, but in order to calculate the n required for a model


### Calculating N that meets  criterion (i)
### Calculating N that meets  criterion (i)

## Suppose that the LR ratio is available online, or that R2_CS_APP is, as well as n and p
## From this we can calculate R2_CS_ADJ
## We then say in our particular study we want enough patients so that we can achieve a shrinkage factor of 0.9
## Therefore we calculate n as:
## So to get a S of 0.9, we only need
n.req <- p/((0.9-1)*log(1-R2_CS_ADJ/0.9))
n.req

### Calculating N that meets  criterion (ii)
### Calculating N that meets  criterion (ii)

## We require that S_VH > R2_CS_ADJ/(R2_CS_ADJ + 0.05*R2_CS_APP_MAX)
## We want S_VH to be 0.9

## Therefore we require the right hand side of the hinequality in smaller than 0.9
R2_CS_ADJ/(R2_CS_ADJ + 0.05*R2_CS_APP_MAX)



## It is.
## If it was not, and say the right hand side of this equation was 0.92
## Then we would need to make sure our S_VH is bigger than 0.9
## Then we would want to recalculate n, for criterion (i) to give a S_VH of 0.92 rather than 0.9.


### Calculating N that meets  criterion (iii)
### Calculating N that meets  criterion (iii)

## First need to calculate the rate, this would be done using existing papers/literature
## Calculate number of years follow up
total.followup <- sum(as.numeric(data_devel$CVD_time))/365.25
## Calculate number of events
total.events <- sum(data_devel$CVD_cens_R)
## Get average follow up
av.followup <- total.followup/n

## Calculate rate 
lambda <- total.events/total.followup


## We then calculate total follow up (average) based on require sample size from (i), and average follow up
total.followup.new.model <- av.followup*n.req

## Calculate the confidence interval
upper.new.model <- 1 - exp(-(lambda + 1.96*sqrt(lambda/total.followup.new.model))*10)
lower.new.model <- 1 - exp(-(lambda - 1.96*sqrt(lambda/total.followup.new.model))*10)
mean.new.model <- 1 - exp(-lambda*10)

c(lower.new.model, mean.new.model, upper.new.model)
upper.new.model - lower.new.model

## The CI is smaller than the suggested 0.05 minimum

## Therefore n.req
n.req
## should be viewed as enough participants, according to these criteria????

### NOW GOING TO ESTIMATE SAME STUFF ASSUMING WE ONY HAVE QRISK£ TO USE

## Estimate n using D from QRISK3
D2 <- 2.49*2.49
R2_D_APP <- (3.14*D2/8)/((3.14^2)/6 + (3.14*D2/8))
R2_OQ_APP <- -(R2_D_APP*(3.14^2)/6)/(((1-(3.14^2)/6)*R2_D_APP)-1)

LR <- -160549*(1-R2_OQ_APP)
LR

## Can now actually straight up calculate the shrinkage factor using an equation
p <- 40
n <- 4019956

S_VH <- 1-p/(LR) ##eq6
S_VH

## However now want to see what smaple size was req, to get an S_VH of 0.9 (assuming we hadn't already got the model)
## Now lets calcualte it as an estimate
## Need to calculate R2_CS_APP for that
R2_CS_APP <- 1 - exp(-LR/n)
R2_CS_APP

## This checks theat the sample size ensuers a small absolute difference between models apparaent and adjusted R2_NAGELKERKE
R2_CS_ADJ <- S_VH*R2_CS_APP ##eq8

n.req <- 11/((0.9-1)*log(1-R2_CS_ADJ/0.9))
n.req
