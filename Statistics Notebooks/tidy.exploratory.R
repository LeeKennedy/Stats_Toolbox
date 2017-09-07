# Clean Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyr)
library(readxl)
library(dplyr)

# Data in Example 1 ------------------------------------------------------
df <- data.frame(Sample = c(1,1,1,2,2,2,3,3,3), Test = c("a","b","c","a","b","c","a","b","c"), Result = c(1,2,3,4,5,6,7,8,9))
df

# Spreading data ---------------------------------------------------------
df2 <- spread(df, Test, Result)
df2



# Data in Example 2 ------------------------------------------------------
data.in <- read_excel("PMBROAD.xlsx", sheet=1)
data.in
data1 <- data.in[, c(13,8)]
data1

data2 <- data1 %>% group_by(PRODUCT_GRADE) %>% mutate(Item = seq_along(PRODUCT_GRADE))
wide_data <- spread(data2, PRODUCT_GRADE, ENTRY, fill = "")
wide_data 

# Data in Example 3 ------------------------------------------------------
x <- seq(from = 90, to = 120, by = 0.5)
y1 <- dnorm(x, mean = 100, sd = 5)
y2 <- dnorm(x, mean = 101, sd = 7)

data1 <- as.data.frame(y1)
data2 <- as.data.frame(y2)
data1 <- cbind(x, data1, data2)

# Gathering data ---------------------------------------------------------
data3 <- gather(data1, c, x)
head(data3, 10)

# Data in Example 4 ------------------------------------------------------
data.in <- read.csv("Book1.csv", as.is=TRUE, header = TRUE)
data.in

# Gathering data ---------------------------------------------------------
data2 <-gather(data=data.in,key=Group,value=Result,na.rm=FALSE,A,B,C)
data2
