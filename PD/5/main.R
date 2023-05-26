# Wybierz 16 obserwacji zaczynając od obserwacji o numerze 7901 oraz 31 obserwacji zaczynająac od obserwacji o
# numerze 35201 i na poziomie istotności równym 3% przetestuj hipotezę o równości wartości przeciętnych w obu
# grupach dla zmiennej price. Zinterpretuj wyniki.

library(tidyverse)
dane <- ggplot2::diamonds
summary(dane)

x1 <- as.vector(unlist(dane[c(7901:7916), 'price']), 'numeric')

x2 <- as.vector(unlist(dane[c(35201:35231), 'price']), 'numeric')

shapiro.test(x1)
shapiro.test(x2)
# Na poziomie istotności 0.03 należy odrzucić hipotezę zerową, 
# że próba pochodzi z rozkładu normalnego

# Wykonujemy test nieparametryczny, gdyż nie jest to rozkład normalny  
wilcox.test(x1, x2, alternative = 'two.sided', conf.level = .97)

# Na poziomie istotności 0.03 należy odrzucić hipotezę zerową, 
# że wartość przeciętna w obu grupach jest równa,
# na korzyść hipotezy alternatywnej, że wartość przeciętna w obu
# grupach nie jest równa.
