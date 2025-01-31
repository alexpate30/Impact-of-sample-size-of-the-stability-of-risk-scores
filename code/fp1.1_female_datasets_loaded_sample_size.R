### SET ROOT DIRECTORY

### SET ROOT DIRECTORY


### Load the historical cohort and the contemporary cohort
load("R_out_C4/chapter4_fake_data.RData")


## Now to seperate the validation cohort and the development population, as we will be calculating HarrelsC and the calibration
## in a internal validation dataset of 100,000

## Set seed
set.seed(501)

## Next want to extract the rest of the data at random and combine (doing 109,998 row id.s so i have an extra 100,000 to do Harrels C with)
row.ids <- sample(1:nrow(data.historical),100000, replace = FALSE)
data_devel <- data.historical[-row.ids,]
data_valid <- data.historical[row.ids,]

rm(list=setdiff(ls(),list("data_devel","data_valid","data.2016")))

save.image("R_out_C4/female_datasets_loaded_sample_size.RData")
print("saved")