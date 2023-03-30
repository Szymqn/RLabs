rm(list = ls())

library(tidyverse)
library(readr)


acsNew <- read_csv("./acsNew.csv")

str_cols <- sapply(acsNew, is.character)

acsNew[str_cols] <- lapply(acsNew[str_cols], factor)

summary(acsNew)

# Dla zmiennej FamilyIncome wyznacz wszystkie możliwe parametry liczbowe i pozycyjne.
library(e1071)
library(DescTools)

x <- unlist(acsNew['FamilyIncome'])

# Parametry liczbowe
mean(x)
var(x)
sd(x)
IQR(x)
quantile(x)
min(x)
max(x)
range(x)

(mz_3 <- e1071::moment(x = x, order = 3, na.rm = TRUE))
(mz_4 <- e1071::moment(x = x, order = 4, na.rm = TRUE))
(ma_4 <- e1071::moment(x = x, order = 4, absolute = TRUE, na.rm = TRUE))
(mc_4 <- e1071::moment(x = x, order = 4, center = TRUE, na.rm = TRUE))
(mac_4 <- e1071::moment(x = x, order = 4, center = TRUE, absolute = TRUE, na.rm = TRUE))
(ma_3 <- e1071::moment(x = x, order = 3, absolute = TRUE, na.rm = TRUE))
(mc_3 <- e1071::moment(x = x, order = 3, center = TRUE, na.rm = TRUE))
(mac_3 <- e1071::moment(x = x, order = 3, center = TRUE, absolute = TRUE, na.rm = TRUE))

# Parametry pozycyjne
quantile(x)
(Me <- quantile(x, .5))
(q1q3 <- quantile(x, c(.25, .75)))
names(q1q3) <- NULL
q1q3
quantile(x, seq(.1, .9, .1))
quantile(x, seq(.01, .99, .01))

DescTools::Mode(x)
(Q <- (quantile(x, .75) - quantile(x, .25)) / 2 )
(Q <- as.numeric((quantile(x, .75) - quantile(x, .25)) / 2 ))
(Q <- IQR(x) / 2)

# Wyznacz pozostałe parametry pozycyjne przedstawione na wykładzie 3 
# (definicje od 17 do 22 tj. od pozycyjnego typowego obszaru zmienności
# do pozycyjnego współczynnika koncentracji).

(odch_st_pop <- sqrt(e1071::moment(x = x, order = 2, center = TRUE, na.rm = TRUE)))
(odch_st_pr <-  sd(x))
(odch_p <- e1071::moment(x, center = TRUE, absolute = TRUE, na.rm = TRUE))
(srednia <- mean(x))

# 17) T_Q = (Me-Q, Me+Q)
D17 <- c(Me-Q, Me+Q)
names(D17) <- NULL
D17

(typowy_obszar_zmiennosci <- list(populacyjny = list(LeftEnd = srednia - odch_st_pop,
                                                     RightEnd = srednia + odch_st_pop),
                                  probkowy = list(LeftEnd = srednia - odch_st_pr,
                                                  RightEnd = srednia + odch_st_pr),
                                  przecietny = list(LeftEnd = srednia - odch_p,
                                                    RightEnd = srednia + odch_p)
))

(wspolczynnik_koncentracji <- list(populacyjny = mc_4 / war_pop^2,
                                   probkowy = mc_4 / war_pr^2))

(wspolczynnik_aymetrii <- list(populacyjny = mc_3 / ((odch_st_pop)^3),
                               probkowy = mc_3 / (odch_st_pr)^3))

# 18 V_Q = (Q/Me), (Me != 0) oraz V_Q_1, Q_3 = (Q_3 - Q_1)/(Q_3 + Q_1)

