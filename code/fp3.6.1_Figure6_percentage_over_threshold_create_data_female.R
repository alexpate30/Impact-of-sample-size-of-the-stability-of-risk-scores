### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## This code will generate the table for % of patients that are classified to the other side of the 10% threshold, based off their population risk, and save into a dataframe called "Figure6_data"
## This will be used to produce Figure6

library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)

## First load the population derived risks
load("R_out_C4/generate_risks_calibration_population_female.RData")

## Turn risks into percentages
risks.all.population <- 100*risks.all.population

### Next do the 10000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_10000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Now split into groups by this average risk
groups <- vector("list",30)
for (i in 1:30){groups[[i]] <- risks.bootstrap[(risks.all.population > (i-1)) & (risks.all.population <= i),]}

## Alternatively
## Just look at all the patients with risks 9-10%, what proportion are missclassified
table2.10000 <- 100*c(sum(groups[[6]] > 10)/(nrow(groups[[6]])*1000),
sum(groups[[7]] > 10)/(nrow(groups[[7]])*1000),
sum(groups[[8]] > 10)/(nrow(groups[[8]])*1000),
sum(groups[[9]] > 10)/(nrow(groups[[9]])*1000),
sum(groups[[10]] > 10)/(nrow(groups[[10]])*1000),
sum(groups[[11]] < 10)/(nrow(groups[[11]])*1000),
sum(groups[[12]] < 10)/(nrow(groups[[12]])*1000),
sum(groups[[13]] < 10)/(nrow(groups[[13]])*1000),
sum(groups[[14]] < 10)/(nrow(groups[[14]])*1000),
sum(groups[[15]] < 10)/(nrow(groups[[15]])*1000))

## Next do the 50,000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_50000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Now split into groups by this average risk
groups <- vector("list",30)
for (i in 1:30){groups[[i]] <- risks.bootstrap[(risks.all.population > (i-1)) & (risks.all.population <= i),]}

## Alternatively
## Just look at all the patients with risks 9-10%, what proportion are missclassified
table2.50000 <- 100*c(sum(groups[[6]] > 10)/(nrow(groups[[6]])*1000),
                      sum(groups[[7]] > 10)/(nrow(groups[[7]])*1000),
                      sum(groups[[8]] > 10)/(nrow(groups[[8]])*1000),
                      sum(groups[[9]] > 10)/(nrow(groups[[9]])*1000),
                      sum(groups[[10]] > 10)/(nrow(groups[[10]])*1000),
                      sum(groups[[11]] < 10)/(nrow(groups[[11]])*1000),
                      sum(groups[[12]] < 10)/(nrow(groups[[12]])*1000),
                      sum(groups[[13]] < 10)/(nrow(groups[[13]])*1000),
                      sum(groups[[14]] < 10)/(nrow(groups[[14]])*1000),
                      sum(groups[[15]] < 10)/(nrow(groups[[15]])*1000))


## Next do the 100,000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_100000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Now split into groups by this average risk
groups <- vector("list",30)
for (i in 1:30){groups[[i]] <- risks.bootstrap[(risks.all.population > (i-1)) & (risks.all.population <= i),]}

## Alternatively
## Just look at all the patients with risks 9-10%, what proportion are missclassified
table2.100000 <- 100*c(sum(groups[[6]] > 10)/(nrow(groups[[6]])*1000),
                      sum(groups[[7]] > 10)/(nrow(groups[[7]])*1000),
                      sum(groups[[8]] > 10)/(nrow(groups[[8]])*1000),
                      sum(groups[[9]] > 10)/(nrow(groups[[9]])*1000),
                      sum(groups[[10]] > 10)/(nrow(groups[[10]])*1000),
                      sum(groups[[11]] < 10)/(nrow(groups[[11]])*1000),
                      sum(groups[[12]] < 10)/(nrow(groups[[12]])*1000),
                      sum(groups[[13]] < 10)/(nrow(groups[[13]])*1000),
                      sum(groups[[14]] < 10)/(nrow(groups[[14]])*1000),
                      sum(groups[[15]] < 10)/(nrow(groups[[15]])*1000))


## Next do the Nmin cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_1434.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Now split into groups by this average risk
groups <- vector("list",30)
for (i in 1:30){groups[[i]] <- risks.bootstrap[(risks.all.population > (i-1)) & (risks.all.population <= i),]}

## Alternatively
## Just look at all the patients with risks 9-10%, what proportion are missclassified
table2.1434 <- 100*c(sum(groups[[6]] > 10)/(nrow(groups[[6]])*1000),
                       sum(groups[[7]] > 10)/(nrow(groups[[7]])*1000),
                       sum(groups[[8]] > 10)/(nrow(groups[[8]])*1000),
                       sum(groups[[9]] > 10)/(nrow(groups[[9]])*1000),
                       sum(groups[[10]] > 10)/(nrow(groups[[10]])*1000),
                       sum(groups[[11]] < 10)/(nrow(groups[[11]])*1000),
                       sum(groups[[12]] < 10)/(nrow(groups[[12]])*1000),
                       sum(groups[[13]] < 10)/(nrow(groups[[13]])*1000),
                       sum(groups[[14]] < 10)/(nrow(groups[[14]])*1000),
                       sum(groups[[15]] < 10)/(nrow(groups[[15]])*1000))


## Next do the Nepv10 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_2954.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Now split into groups by this average risk
groups <- vector("list",30)
for (i in 1:30){groups[[i]] <- risks.bootstrap[(risks.all.population > (i-1)) & (risks.all.population <= i),]}

## Alternatively
## Just look at all the patients with risks 9-10%, what proportion are missclassified
table2.2954 <- 100*c(sum(groups[[6]] > 10)/(nrow(groups[[6]])*1000),
                       sum(groups[[7]] > 10)/(nrow(groups[[7]])*1000),
                       sum(groups[[8]] > 10)/(nrow(groups[[8]])*1000),
                       sum(groups[[9]] > 10)/(nrow(groups[[9]])*1000),
                       sum(groups[[10]] > 10)/(nrow(groups[[10]])*1000),
                       sum(groups[[11]] < 10)/(nrow(groups[[11]])*1000),
                       sum(groups[[12]] < 10)/(nrow(groups[[12]])*1000),
                       sum(groups[[13]] < 10)/(nrow(groups[[13]])*1000),
                       sum(groups[[14]] < 10)/(nrow(groups[[14]])*1000),
                       sum(groups[[15]] < 10)/(nrow(groups[[15]])*1000))


## Finally combine into one table and save
Figure6_data <- rbind(round(table2.1434,2),round(table2.2954,2),round(table2.10000,2),round(table2.50000,2),round(table2.100000,2))
colnames(Figure6_data) <- paste("Risk",5:14,"-",6:15,"%",sep="")
rownames(Figure6_data) <- c(1434,2954,10000,50000,100000)
Figure6_data

rm(list=setdiff(ls(),list("table2")))
save.image("R_out_C4/Figure6_data_female_percentage_over_threshold.RData")
