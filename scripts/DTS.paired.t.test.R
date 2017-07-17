# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
 qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
 y <- x
 y[x < (qnt[1] - H)] <- NA
 y[x > (qnt[2] + H)] <- NA
 y
}

cohens_d <- function(x, y) {
        lx <- length(x)- 1
        ly <- length(y)- 1
        md  <- abs(mean(x) - mean(y))
        csd <- lx * var(x) + ly * var(y)
        csd <- csd/(lx + ly)
        csd <- sqrt(csd)
        
        cd  <- md/csd   ## cohen's d
}


# Data Input -------------------------------------------------------------

data.in <- read.csv("vitamind.csv", header=TRUE)

# Data Cleaning ----------------------------------------------------------

data.in <- data.in[1:15,]

t1 <- data.in$A
t2 <- data.in$B


boxplot(t1, t2)
summary(data.in)

t.test(t2, t1, paired = TRUE)

aa <- cohens_d(data.in$A, data.in$B)
round(aa,2)
