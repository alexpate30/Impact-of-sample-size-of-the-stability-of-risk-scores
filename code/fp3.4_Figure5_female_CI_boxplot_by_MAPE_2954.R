### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## This code generates the boxplots of confidence interval size, when sample size = 2954, but straitified by mean absolute prediction error (low, medium and high)


library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)

## Load the discriminations measures and population derived risks
load("R_out_C4/generate_risks_calibration_population_female.RData")

## Turn risks into percentages
risks.all.population <- 100*risks.all.population

## Load the risks from simulation
load("R_out_C4/generate_risks_different_cohort_sizes_female_2954.RData")


## Check str
str(risks.bootstrap)
str(APE.bootstrap)

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## First calculate the quantiles for thirds of the C statistics, so I can subset based on these values
quantiles.APE <- quantile(APE.bootstrap,probs = c(0,0.333,0.667,1))
quantiles.APE

# Step 1: Split models into groups based on their mean asbolute prediction error
# Step 2: Repeat the main analysis within model groups

## Now extract based on this
APE.groups <- vector("list",4)

## First group extracts all columns of risks.bootstrap, where the HarrelsC value from HarrelsC.bootstrap is between 0% and 33% quantiles
APE.groups[[1]] <- risks.bootstrap[, (APE.bootstrap <= quantiles.APE["100%"])]
## Second group 33% and 67%
APE.groups[[2]] <- risks.bootstrap[, (APE.bootstrap < quantiles.APE["66.7%"])]
## Last group 67% and 100%
APE.groups[[3]] <- risks.bootstrap[, (APE.bootstrap < quantiles.APE["33.3%"])]


## Check sizes
print("Check dimensions of subsets of all bootstrapped models, should all have same number of rows, but 1000/3 columns")
dim(APE.groups[[1]])
dim(APE.groups[[2]])
dim(APE.groups[[3]])

## Now for each of these, I effectively want to produce the same plot as before
## I will write a function to do this, so I just have to enter the data

######### FUNCTION STARTS HERE
## Next define the function to create the boxplot for a dataframe (data.in), which has a row for each patient (same order 
## as risks.all.population), and a column for each bootstrapped risk score. The output is a list, which also stores the data
## required to do the actual plot
create.box.plot <- function(data.in){
  
  ## Now split into groups by this average risk
  groups <- vector("list",30)
  for (i in 1:30){groups[[i]] <- data.in[(risks.all.population > (i-1)) & (risks.all.population <= i),]}
  
  ## Create data which contains the CI size for each patient
  CI.size.all <- vector("list",30)
  for (i in 1:30){CI.size.all[[i]] <- data.frame("lower" = rowQuantiles(as.matrix(groups[[i]]),probs=0.05), "upper" = rowQuantiles(as.matrix(groups[[i]]),probs=0.95), 
                                                 "CIsize" = rowQuantiles(as.matrix(groups[[i]]),probs=0.95) - rowQuantiles(as.matrix(groups[[i]]),probs=0.05))}
  
  ## Each in each 'CI.size.all' list element, I have a dataframe with each patients from that risk group, columns being
  ## lower bound on CI, upper bound on CI, and size of CI.
  
  ## Make boxplots of this
  ## First I need to restructure my data
  ## For each group I want to add a column called 'risk' with the risk score for that group
  for (i in 1:30){CI.size.all[[i]]$"risk" <- rep(paste(i-1,"-",i,"%",sep=""),nrow(CI.size.all[[i]]))}
  
  ## Now concatenate them into one big dataset
  plot.data.all <- do.call("rbind",CI.size.all)
  
  
  ## Now extract only risk scores we want to plot
  plot.data <- plot.data.all[plot.data.all$risk %in% c("4-5%","9-10%","14-15%","19-20%"),]
  plot.data$risk <- factor(plot.data$risk, levels <- c("4-5%","9-10%","14-15%","19-20%"))
  
  ## Now create the boxplot
  
  ## First create dataframe with data for labels
  label.data <- data.frame("x" = c("4-5%","9-10%","14-15%","19-20%"),y = 70, label = paste("n = ", c(sum(plot.data$risk == "4-5%"),sum(plot.data$risk == "9-10%"),
                                                                                                     sum(plot.data$risk == "14-15%"),sum(plot.data$risk == "19-20%"))))
  
  
  ## Now do the boxplot
  plot.box <- ggplot(data = plot.data, aes(x=risk,y=CIsize)) + geom_boxplot(outlier.size = 0.01, lwd = 0.25) + stat_boxplot(geom = "errorbar", lwd = 0.25) +
    xlab("Population derived risk") + ylab("5 - 95 percentile range across models") + scale_y_continuous(minor_breaks = NULL, breaks = seq(0,70,5), limits = c(0,70)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size = .5, color = "white"), plot.title = element_text(size=9.5),
          axis.title = element_text(size = 9)) +
    geom_text(data = label.data, aes(x=x,y=y,label = label), size = 3)
  
  
  ## Return both the data derived to do the plot, and the boxplot itself, this means the whole thing doesnt have to be run again
  ## if you want to edit the boxplot only
  return(list("plot.data.all" = plot.data.all,"plot.data" = plot.data,"plot.box" = plot.box))}

print("start low")
Sys.time()
res.APE1.obj <- create.box.plot(APE.groups[[1]])
res.APE1 <- res.APE1.obj$"plot.box" + ggtitle(paste("All models\nNumber of models =",dim(APE.groups[[1]])[2],sep=" "))
print("start med")
Sys.time()
res.APE2.obj <- create.box.plot(APE.groups[[2]])
res.APE2 <- res.APE2.obj$"plot.box" + ggtitle(paste("Best performing two thirds of models by MAPE\nNumber of models =",dim(APE.groups[[2]])[2],sep=" "))
print("start high")
Sys.time()
res.APE3.obj <- create.box.plot(APE.groups[[3]])
res.APE3 <- res.APE3.obj$"plot.box" + ggtitle(paste("Best performing third of models by MAPE\nNumber of models =",dim(APE.groups[[3]])[2],sep=" "))
Sys.time()

#rm(list=setdiff(ls(),list("res.APE1.obj","res.APE2.obj","res.APE3.obj","quantiles.APE")))

## Make combined plot
Figure5_female_CI_boxplot_by_MAPE_2954 <- ggarrange(res.APE1,res.APE2,res.APE3,nrow = 1,ncol=3)

ggsave("figures/Figure5_female_CI_boxplot_by_MAPE_2954.png",Figure5_female_CI_boxplot_by_MAPE_2954, dpi = 600, height = 3.5, width = 10.5)


## Let's also get information for the text
summary(res.APE1$"data"$CIsize[res.APE1$"data"$risk == "4-5%"])
summary(res.APE1$"data"$CIsize[res.APE1$"data"$risk == "9-10%"])
summary(res.APE1$"data"$CIsize[res.APE1$"data"$risk == "14-15%"])
summary(res.APE1$"data"$CIsize[res.APE1$"data"$risk == "19-20%"])

summary(res.APE2$"data"$CIsize[res.APE2$"data"$risk == "4-5%"])
summary(res.APE2$"data"$CIsize[res.APE2$"data"$risk == "9-10%"])
summary(res.APE2$"data"$CIsize[res.APE2$"data"$risk == "14-15%"])
summary(res.APE2$"data"$CIsize[res.APE2$"data"$risk == "19-20%"])

summary(res.APE3$"data"$CIsize[res.APE3$"data"$risk == "4-5%"])
summary(res.APE3$"data"$CIsize[res.APE3$"data"$risk == "9-10%"])
summary(res.APE3$"data"$CIsize[res.APE3$"data"$risk == "14-15%"])
summary(res.APE3$"data"$CIsize[res.APE3$"data"$risk == "19-20%"])
