library(tidyverse)

# require(tidyverse)

a1 <- 2
a1

assign('a11', 2)
a11

(a2 <- 2)

v1 <- c(2,3,4)
v1
v1[1]

is.integer(a1)
a3 <- 2L
is.integer(a3)

a12 <- as.integer(a1)
as.integer(2.5)

v2 <- c(TRUE, FALSE, FALSE)
v3 <- c(T, F, T)
v2 == v3
v2 != v3
v2 <= v3
v2 < v3
v2 & v3
v2 | v3
v2 && v3
v2 || v3

sum(v2)
sum(v3)

v4 <- seq (from = 1, to = 4, by = .5)
v4
v5 <- seq(1, 4, .5)
v5
v4 == v5

?seq
help(seq)

1:4
4:2
-1:3
-1:-3
0:5
0:5.5

v6 <- v4 + v5
v6
(v6 + (1:2))
(v6 + (1:3))

v4
ceiling(v4)
floor(v4)
log(v4)

m1 <- matrix(1:4, ncol = 2, nrow = 4)
m1
m2 <- matrix(1:4, ncol = 2)
m2
m3 <- matrix(1:4, ncol = 2, byrow = TRUE)
m3

df1 <- data.frame(x1 = rep('a',4), numbers = 1:4, logic = sample(c(TRUE,FALSE), 4, replace = TRUE))
df1

sum(log(v4))

v1 |>
  log() |>
  sum()

library(tidyverse)

v4 %>%
  log() %>%
  sum()

?pnorm

dystrybuantaNormalnego <- seq(0, 3.99, .01) %>%
  pnorm() %>%
  round(digits = 9) %>%
  matrix(ncol = 10, byrow = TRUE)

seq(0, .09, .01) -> 
  colnames(dystrybuantaNormalnego)

dystrybuantaNormalnego

acsNew <- read.csv("http://www.jaredlander.com/data/acsNew.csv")

summary(acsNew)

acsNewFactor <- acsNew %>%
  mutate(across(where(is.character), as.factor(.x), .names = "{.col}.factor"))

acsNewFactor

m <- seq(0, 3.99, .02) 
m
