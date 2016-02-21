library(psych)
library(dplyr)
library(ggplot2)

# Enter data as csv file: Item/Result 1/Result 2 -------------------------
data.in <- read.csv("Protein.csv", as.is=TRUE, header=TRUE)
bucket <- rep(NA,1000)
n=nrow(data.in)

# Bootstrapping ----------------------------------------------------------
for(i in 1:1000){

samp <- sample(data.in$Item, n, replace=TRUE)

set <- data.in[samp,]

temp <- set %>%
        mutate(Diff2 = (A-B)^2) %>%
        summarise (std = sqrt(sum(Diff2)/(2*n)))

bucket[i] = temp
}

bucket_2 <- as.data.frame(unlist(bucket))
colnames(bucket_2)[1] <- "Number"

# Histogram of possible answers ------------------------------------------
hist(bucket_2$Number, breaks = 20)

summary(bucket_2$Number)
describe(bucket_2$Number)

SEM <- sd(bucket_2$Number)/sqrt(n)
SEM

hranges <- quantile(bucket_2$Number, c(0.025, 0.975))
hranges
plot <- ggplot(bucket_2, aes(x=Number)) +
        geom_histogram(binwidth = 0.005, colour="darkgreen", fill = "burlywood1") +
        geom_vline(xintercept = hranges[1], lty=2, colour = "red") +
        geom_vline(xintercept = hranges[2], lty=2, colour = "red") +
        theme_bw()
plot
