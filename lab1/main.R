library(tidyverse)

mean <- 2
sd <- 3
n <- 10
q <- seq(0, .9, .1)

r <- rnorm(n, mean, sd)

densities <- dnorm(r, mean, sd) 
cumprobs <- pnorm(r, mean, sd)
quantiles <- qnorm(q, mean, sd)


m1 <- cbind(densities, cumprobs, quantiles)

m1
