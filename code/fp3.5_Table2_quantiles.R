### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## This code will generate the range of quantiles for each sample size

library(ggplot2)
library(ggpubr)
library(dplyr)
library(matrixStats)

#### First do the 1434 cohort size (Nmin)
load("R_out_C4/generate_risks_different_cohort_sizes_female_1434.RData")

## First extract the quantiles for N = 1434
quantiles.C.nmin.female <- round(quantile(HarrelsC.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)
quantiles.calib.nmin.female <- round(100*quantile(calibration.in.large.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),2)
quantiles.MAPE.nmin.female <- round(quantile(100*APE.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)


#### Next do the 2954 cohort size (Nepv10)
load("R_out_C4/generate_risks_different_cohort_sizes_female_2954.RData")

## First extract the quantiles for N = 2954
quantiles.C.nepv10.female <- round(quantile(HarrelsC.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)
quantiles.calib.nepv10.female <- round(100*quantile(calibration.in.large.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),2)
quantiles.MAPE.nepv10.female <- round(quantile(100*APE.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)



#### Next do the 10000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_10000.RData")

## First calculate the quantiles for thirds of the C statistics, so I can subset based on these values
quantiles.C.10000.female <- round(quantile(HarrelsC.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)
quantiles.calib.10000.female <- round(100*quantile(calibration.in.large.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),2)
quantiles.MAPE.10000.female <- round(quantile(100*APE.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)



#### Next do the 50000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_50000.RData")

## First calculate the quantiles for thirds of the C statistics, so I can subset based on these values
quantiles.C.50000.female <- round(quantile(HarrelsC.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)
quantiles.calib.50000.female <- round(100*quantile(calibration.in.large.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),2)
quantiles.MAPE.50000.female <- round(quantile(100*APE.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)



#### Next do the 100000 cohort size
load("R_out_C4/generate_risks_different_cohort_sizes_female_100000.RData")

## First calculate the quantiles for thirds of the C statistics, so I can subset based on these values
quantiles.C.100000.female <- round(quantile(HarrelsC.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)
quantiles.calib.100000.female <- round(100*quantile(calibration.in.large.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),2)
quantiles.MAPE.100000.female <- round(quantile(100*APE.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)



quantiles.C.table.female <- rbind(quantiles.C.nmin.female,quantiles.C.nepv10.female,quantiles.C.10000.female,
                         quantiles.C.50000.female,quantiles.C.100000.female)


quantiles.calib.table.female <- rbind(quantiles.calib.nmin.female,quantiles.calib.nepv10.female,quantiles.calib.10000.female,
                                  quantiles.calib.50000.female,quantiles.calib.100000.female)

quantiles.MAPE.table.female <- rbind(quantiles.MAPE.nmin.female,quantiles.MAPE.nepv10.female,quantiles.MAPE.10000.female,
                                      quantiles.MAPE.50000.female,quantiles.MAPE.100000.female)





### Now to do the Net Benefits (the net benefit of each model was stored in a different area as it was ran seperately from the rest of the simulation)
load("R_out_C4/NB_bootstrap_female_1434.RData")
quantiles.NB.nmin.female <- round(quantile(NB.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)

load("R_out_C4/NB_bootstrap_female_2954.RData")
quantiles.NB.nepv10.female <- round(quantile(NB.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)

load("R_out_C4/NB_bootstrap_female_10000.RData")
quantiles.NB.10000.female <- round(quantile(NB.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)

load("R_out_C4/NB_bootstrap_female_50000.RData")
quantiles.NB.50000.female <- round(quantile(NB.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)

load("R_out_C4/NB_bootstrap_female_100000.RData")
quantiles.NB.100000.female <- round(quantile(NB.bootstrap,probs = c(0.025,0.25,0.5,0.75,0.975)),3)

quantiles.NB.table.female <- rbind(quantiles.NB.nmin.female,quantiles.NB.nepv10.female,quantiles.NB.10000.female,
                                     quantiles.NB.50000.female,quantiles.NB.100000.female)


rm(list=setdiff(ls(),list("quantiles.C.table.female",
                          "quantiles.calib.table.female",
                          "quantiles.MAPE.table.female",
                          "quantiles.NB.table.female")))


save.image("R_out_C4/quantiles_table.RData")