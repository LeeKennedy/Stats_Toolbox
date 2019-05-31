# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(LK.Toolbox)
library(readxl)

# Functions --------------------------------------------------------------

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

data.in <- read_excel("data/Book1.xlsx")

# Data Cleaning ----------------------------------------------------------

data.in <- data.in[16:30,]

t1 <- data.in$A
t2 <- data.in$B


boxplot(t1, t2)
summary(data.in[,2:3])

t.test(t2, t1, paired = TRUE)

aa <- cohens_d(data.in$A, data.in$B)
round(aa,2)
