### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

library(ggplot2)
library(reshape)
library(ggpubr)

load("R_out_C4/Figure6_data_female_percentage_over_threshold.RData")

rownames(Figure6_data) <- c("Nmin","Nepv10","10000","50000","100000")
colnames(Figure6_data) <- c("5-6%","6-7%","7-8%","8-9%","9-10%","10-11%","11-12%",
                      "12-13%","13-14%","14-15%")
Figure6.plot.dat <- melt(Figure6_data)

colnames(Figure6.plot.dat)[1] <- "sample.size"

Figure6.plot.dat$value <- Table3.plot.dat$value/100

Figure6.plot.dat

Figure6.plot.dat$sample.size <- factor(Figure6.plot.dat$sample.size, levels = c("Nmin","Nepv10","10000","50000","100000"))
Figure6.plot.dat$X2 <- factor(Figure6.plot.dat$X2, 
                                      levels = c("5-6%","6-7%","7-8%","8-9%","9-10%","10-11%","11-12%",
                                                 "12-13%","13-14%","14-15%"))

Figure6.plot.female <- ggplot(Figure6.plot.dat, aes(x=X2,y=value)) +
  geom_bar(aes(color = sample.size, fill = sample.size), stat = "identity", 
           position = position_dodge(0.8), width = 0.7) + geom_vline(xintercept = 5.5) +
  annotate("text", x = 4.7, y = 0.5, size = 4.3, label = "Treatment threshold") + xlab("Population derived risk") +
  ylab("Probability of risk being on the other side of the treatment threshold") + 
  theme(legend.title = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),panel.background = element_blank()) 

ggsave("Figure6_female.png", dpi = 600,width = 14, height = 7)

rm(list=setdiff(ls(),list("Figure6.plot.female")))

