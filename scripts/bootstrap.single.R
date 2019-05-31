#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------

library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(psych)
library(here)

#### Functions -----------------------------


#### Data Input -----------------------------

here::here()

data.in <- read_excel("data/Book1.xlsx")

data.in$A <- as.numeric(data.in$A)

bucket <- rep(NA,10000)
n <- nrow(data.in)

for(i in 1:10000){
  samp = sample(data.in$A, n, replace=TRUE)
  bucket[i] = mean(samp)
}

hist(bucket, breaks = 40)

summary(bucket)
describe(bucket, skew = FALSE)


