# Wyczyść środowisko.
rm(list = ls())

# Załaduj bibliotekę tidyverse.
library(tidyverse)

# Zaimportuj zbiór danych diamonds z bibliotek ggplot2 pod nazwą dane.
dane <- ggplot2::diamonds

# Wyświetl podsumowanie danych.
summary(dane)

# Ustaw ziarno generatora liczb pseudolosowych na numer Swojego album.
set.seed(84647)

# Wygeneruj jedną liczbę pseudolosową z zakresu od 1 do liczby powstałej z numeru Twojego albumu po usunięciu cyfry dziesięciu tysięcy. W przypadku, gdy cyfra tysięcy jest zerem pomiń ją (oznaczmy tą liczbę p1).
p1 <- as.integer(runif(1, min = 1, max = 4647))

# Wygeneruj jedną liczbę psuedolosową z zakresu od 20 do 40 (oznaczmy ją p2).
p2 <- as.integer(runif(1, min = 20, max = 40))

# Dla zmiennej x ze zbioru dane wybierz wartości od numeru p1 + 1 do p1 + p2. Jest to Twoja próba.
# Wykorzystując test Kołmogorowa-Smirnowa sprawdź hipotezę, że próba pochodzi z rozkładu normalnego.
# Zinterpretuj wynik testu.
h0 <- dane[(p1+1):(p1+p2), "x"]
h0 <- unique(h0)


ks.test(h0, "pnorm")

# Na poziomie istotności α=0.05 należy odrzucić hipotezę zerową, że próba pochodzi z populacji o rozkładzie normalnym z parametrami μ=0 i σ=1,
# na korzyść hipotezy, że próba nie pochodzi z populacji o rozkładzie normalnym z parametrami μ=0 i σ=1.
