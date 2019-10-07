#### Intent: estimate MU from bootstrapping a dataset, 
#### typically an SRM set but can be others.

##### Clean Up environment -----------------------------
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

data.in <- read_excel("MU_Book_1.xlsx", 
                      sheet = "xxxx")

data.in$A <- as.numeric(data.in$ENTRY)

data.in <- data.in %>% 
        filter(REPORTABLE == "T")
        #filter(REPORTABLE != "X")

bucket <- rep(NA,10000)
n <- nrow(data.in)

for(i in 1:10000){
  samp = sample(data.in$A, n, replace=TRUE)
  bucket[i] = sd(samp)
}

hist(bucket, breaks = 40)

summary(bucket)
xx <- describe(bucket, skew = FALSE)
xx

xxx <- describe(data.in$A, skew = FALSE)
xxx

### MU as  percentage of the Mean

 MU <- 200*xx$mean/xxx$mean
 MU_low <- 2*100*(xx$mean-2*xx$sd)/xxx$mean
 MU_high <- 2*100*(xx$mean+2*xx$sd)/xxx$mean
 
 