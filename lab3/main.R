library(tidyverse)
library(readr)

rm(list = ls())

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
# (definicje od 17 do 22 tj. od pozyqcyjnego typowego obszaru zmienności
# do pozycyjnego współczynnika koncentracji).

(odch_st_pop <- sqrt(e1071::moment(x = x, order = 2, center = TRUE, na.rm = TRUE)))
(odch_st_pr <-  sd(x))
(odch_p <- e1071::moment(x, center = TRUE, absolute = TRUE, na.rm = TRUE))
(srednia <- mean(x))

(war_pop <- e1071::moment(x = x, order = 2, center = TRUE, na.rm = TRUE))
(war_pr <- var(x))
sum((x - mean(x))^2) / (length(x) - 1)

q1 = q1q3[1]
q3 = q1q3[2]
EX <- srednia
Mo <- DescTools::Mode(x)[1]
DX <- odch_st_pop
dX <- odch_st_pr
D_9 <- quantile(x, .9)
D_1 <- quantile(x, .1)

# 17) T_Q = (Me-Q, Me+Q)
D17 <- c(Me-Q, Me+Q)
names(D17) <- NULL
D17

# 18) 
# V_Q = (Q/Me), (Me != 0)
D18_1 <- (Q / Me)
names(D18_1) <- NULL
D18_1

# V_Q_1,Q_2 = (Q_3 - Q_1)/(Q_3 + Q_1)
D18_2 <- (q3 - q1)/(q3 + q1)
D18_2

# 19) 
# W_S^Q = (Q_3 - Me) - (Me - Q_1)
D19_1 <- (q3 - Me) - (Me - q1)
names(D19_1) <- NULL
D19_1

# A_Q = ((Q_3 - Me) - (Me - Q_1)) / 2Q
D19_2 <- ((q3 - Me) - (Me - q1)) / 2 * Q
names(D19_2) <- NULL
D19_2

# 20)
# W_S = E(X) - Mo
D20 = EX - Mo
names(D20) <- NULL
D20

# 21)
# A_s = (E(X) - Mo)/D(X)
D21_1 = (EX - Mo)/DX
names(D21_1) <- NULL
D21_1

# A_d = (E(X) - Mo)/d(X)
D21_2 = (EX - Mo)/dX
names(D21_2) <- NULL
D21_2

# W_s,2 = (E(X) - Me)/D(X)
D21_3 = (EX - Me)/DX
names(D21_3) <- NULL
D21_3

# W_d,2 = (E(X) - Me)/d(X)
D21_4 = (EX - Me)/dX
names(D21_4) <- NULL
D21_4

# 22)
# W_s = (D_9 - D_1)/(Q_3 - Q_1)
D22 = (D_9 - D_1)/(q3 - q1)
names(D22) <- NULL
D22
