#### Clean Up environment -----------------------------
rm(list=ls())

library(psych)
library(readxl)
library(tidyverse)
library(here)

# Enter data file: Item/Result 1/Result 2 -------------------------
here::here()
data.in <- read_excel("data/Book1.xlsx")
data.in$diff <- data.in$A - data.in$B
bucket <- rep(NA,10000)
n=nrow(data.in)


# Bootstrapping ----------------------------------------------------------
for(i in 1:10000){

set <- as.data.frame(sample(data.in$diff, n, replace=TRUE))
colnames(set)[1] <- "Diff"


temp <- set %>%
        summarise (std = sqrt(sum(Diff)/(2*n)))

bucket[i] = temp
}

bucket_2 <- as.data.frame(unlist(bucket))
colnames(bucket_2)[1] <- "Number"

# Histogram of possible answers ------------------------------------------
hist(bucket_2$Number, breaks = 20)

summary(bucket_2$Number)
describe(bucket_2$Number)

bucket_2 <- na.omit(bucket_2)

SEM <- sd(bucket_2$Number)/sqrt(n)

SEM

hranges <- quantile(bucket_2$Number, c(0.025, 0.975))
hranges
plot <- ggplot(bucket_2, aes(x=Number)) +
        geom_histogram(binwidth = 0.03, colour="darkgreen", fill = "cornflowerblue") +
        geom_vline(xintercept = hranges[1], lty=2, colour = "red") +
        geom_vline(xintercept = hranges[2], lty=2, colour = "red") +
        labs(x = "Standard Deviation") +
        theme_bw()+
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
        
plot
