#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------

library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(equivalence)
library(here)

#### Data Input -----------------------------

here::here()

data.in <- read_excel("data/Test_equiv.xlsx")

boxplot(data.in)

sdA <- sd(data.in$A, na.rm=TRUE)
sdB <- sd(data.in$B, na.rm=TRUE)
meanA <- mean(data.in$A, na.rm=TRUE)
meanB <- mean(data.in$B, na.rm=TRUE)
nA <- length(na.omit(data.in$A))
nB <- length(na.omit(data.in$B))
sdpooled <- sqrt(((nA-1)*sdA^2+(nB-1)*sdB^2)/(nA+nB-2))

#-----------estimating the acceptable maximum difference------------------
#-----------(reference?)--------------------------------------------------

s_star <- sdB*sqrt((nB-1)/(qchisq(0.99,(nB-1))))
delta <- 0
epsiln <- delta + s_star*(2*qt(0.95,(2*nB-2))*sqrt(2/nB))


#  Ho = the two sets are different. (The revers to the normal Ho.)
# "Not rejected" means that we have not proven that the two sets are equivalent.
#  Generally different if epsilon, the acceptable difference, > pooled sd.
tost_xy <- tost(data.in$A, data.in$B, alpha = 0.05, epsiln)
tost_xy



ll <- ((meanA+meanB)/2)-4*sdpooled
ul <- ((meanA+meanB)/2)+4*sdpooled

SetA <- function(x) dnorm(x, mean = meanA, sd = sdA)
SetB <- function(x) dnorm(x, mean = meanB, sd = sdB)


myYLim <- c(0, 0.01)

plot(SetA, from = ll, to = ul, ylim = myYLim, col = "red", lwd = 4, xlab = "", ylab = "")
plot(SetB, from = ll, to = ul, col = "blue", lwd = 4, add = TRUE)

abline(v = meanA)
abline(v = meanB)

#### ggplot version -----------------------------

tost_plot <- ggplot(data.frame(x = c(ll, ul), y = myYLim), aes(x = x)) +
        stat_function(fun = dnorm)
tost_plot
#### Data Cleaning -----------------------------


#### Visualising Data -----------------------------
p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = dnorm)
p9