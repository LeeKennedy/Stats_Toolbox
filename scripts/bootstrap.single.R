data.in <- read.csv("data/Book1.csv", as.is=TRUE, header=TRUE)

data.in$A <- as.numeric(data.in$A)

bucket <- rep(NA,10000)
n <- nrow(data.in)

for(i in 1:10000){
  samp = sample(data.in$A, n, replace=TRUE)
  bucket[i] = mean(samp)
}

hist(bucket, breaks = 20)


summary(bucket)
library("psych")
describe(bucket)
describe(data.in$A)

sd(bucket)*sqrt(200)
sd(data.in$A)

