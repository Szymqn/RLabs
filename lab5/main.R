rm(list = ls())


set.seed(250323)
x1 <- rnorm(50, mean = 1, sd = 5)
x2 <- rnorm(50, mean = 1, sd = 1)
x3 <- rnorm(50)
x4 <- rt(50, df = 1)

# 1. Przyjmując poziom istotności α=0.05 sformułuj odpowiednie wnioski
# po wykonaniu przeze mnie w tym pliku testów.

t.test(x1, mu = 1.5, alternative = "less")
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia w populacji jest równa 1.5

t.test(x1, mu = 1, alternative = "less")
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia w populacji jest równa 1

t.test(x1, mu = 3, alternative = "less")
# Na podstawie istotności 0.05 należy odrzucić hipotezę zerową, że
# średnia w populacji jest rónwa 3, na korzyść hipotezy alternatywnej,
# że średnia nie jest mniejsza niż 3

t.test(x1, mu = 1, alternative = "greater")
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia populacji jest równa 1

t.test(x1, mu = 3, alternative = "greater")
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia populacja jest równa 3

t.test(x1, mu = 3, alternative = "greater")
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia populacji jest równa 3

x <- c(506, 502, 498, 501, 503, 504, 498, 501, 503, 504)

t.test(x = x, mu = 500, alternative = 'two.sided', conf.level = 0.95)
# Na poziomie istotności 0.05 należy odrzucić hipoteze zerową, że
# średnia populacji jest równa 500, na korzyść hipotezy alternatywnej,
# że średnia populacji jest różna od 500

t.test(x = x, mu = 500)
# Na poziomie istotności 0.05 należy odrzucić hipoteze zerową, że 
# średnia populacji jest równa 500

t.test(x = x, mu = 500, alternative = 'greater')
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia populacji jest równa 500

t.test(x = x, mu = 500, alternative = 'less')
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy
# zerowej, że średnia populacji jest równa 500

# 2. Napisz funkcję, która wyznaczy z-test dwustronny dla jednej frakcji. Funkcja, 
# jako dane ma przyjmować liczbę sukcesów, liczbę prób, poziom istotności, teoretyczną
# frakcję, a zwracać wartość statystyki testowej, p-value, przedział ufności dla frakcji.

# x - liczba sukcesów, n - liczba prób, a - poziom istotności, p - teorytyczna frakcja
fun2 <- function(x, n, a, p) {
  binom <- binom.test(x, n, p, alternative = 'two.sided', conf.level = 1 - a)
  list(list(binom$statistic, binom$parameter, binom$estimate), binom$p.value, binom$conf.int[1:2])
}

fun2(8, 10, 0.05, 0.8)

# 3. Napisz funkcję, która wyznaczy z-test jednostronny dla jednej frakcji. Funkcja,
# jako dane ma przyjmować liczbę sukcesów, liczbę prób, poziom istotności, teoretyczną 
# frakcję, a zwracać wartość statystyki testowej, p-value, przedział ufności dla frakcji.

# x - liczba sukcesów, n - liczba prób, a - poziom istotności, p - teorytyczna frakcja
fun3 <- function(x, n, a, p) {
  binom <- binom.test(x, n, p, alternative = 'greater', conf.level = 1 - a)
  list(list(binom$statistic, binom$parameter, binom$estimate), binom$p.value, binom$conf.int[1:2])
}

fun3(8, 10, 0.05, 0.8)

# 4. Wykonaj zadania 2 i 3 dla dwóch frakcji.
fun4_1 <- function(x, n, a, p) {
  prop <- prop.test(c(x[1], x[2]), c(n[1], n[2]), alternative = 'two.sided', conf.level = 1 - a)
  list(prop, list(prop$statistic, prop$parameter, prop$estimate), prop$p.value, prop$conf.int[1:2])
}

fun4_1(c(8, 6), c(10, 12), 0.05, 0.8)

fun4_2 <- function(x, n, a, p) {
  prop <- prop.test(c(x[1], x[2]), c(n[1], n[2]), alternative = 'greater', conf.level = 1 - a)
  list(list(prop$statistic, prop$parameter, prop$estimate), prop$p.value, prop$conf.int[1:2])
}

fun4_2(c(8, 6), c(10, 12), 0.05, 0.8)

# Rozwiąż zadania 5.11 - 5.21 z podręcznika [7]. Plik samochody.csv znajduje się w eduPortalu.

# Zadanie 5.11. Pewien ichtiolog pobrał losową próbę 15 ryb i zmierzył ich długość.
# Otrzymał następujące wyniki (w mm):
  # 92, 88, 85, 82, 89, 86, 81, 66, 75, 61, 78, 76, 91, 82, 82.
# Zakładając, że rozkład długości ryb badanego gatunku jest normalny, zweryfikuj hipotęze, 
# że średnia długość ryb tego gatunku przekracza 78 mm. Przyjmij poziom istotności α = 0.01

x5_11 = c(92, 88, 85, 82, 89, 86, 81, 66, 75, 61, 78, 76, 91, 82, 82)

t.test(x5_11, mu = 78, alternative = 'greater', conf.level = 0.98)
# Na poziomie istotności 0.02, nie ma podstaw do odrzucenia hipotezy zerowej,
# że średnia populacji jest równa 78

# Zadanie 5.12. Pewien księgowy przypuszcza, że przeciętne saldo na kontach klientów
# jego firmy jest mniejsze niż 31 tys. euro. Żeby to sprawdzić, pobrał losową próbę kont,
# otrzymując następujące wyniki dotyczące przeciętnego salda (w tys. euro):
  # 30.0, 30.0, 29.9, 31.3, 32.0, 32.0, 32.1, 30.5, 32.3, 29.5, 27.8, 27.3, 31.1,
# 30.7, 24.5, 28.3, 31.3, 32.7, 33.3, 26.8.
# Czy prawdziwe jest przypuszczenie księgowego?
# Zweryfikuj stosowną hipotezę na poziomie istotności 0.01.

x5_12 = c(30.0, 30.0, 29.9, 31.3, 32.0, 32.0, 32.1, 30.5, 32.3, 29.5, 27.8, 27.3, 31.1, 30.7, 24.5, 28.3, 31.3, 32.7, 33.3, 26.8)

t.test(x5_12, mu = 31, alternative = 'less', conf.level = 0.99)
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy zerowej,
# że średnia populacji wynosi 31

# Zadanie 5.13. Na podstawie danych zawartych w pliku samochody.csv zweryfikuj
# przypuszczenie, że średnia moc silnika samochodów wyprodukowanych w latach 1979–
# 1981 wynosi 84 KM (wykorzystaj zmienne moc i rok). Przyjmij poziom istotności 0.01.
library(magrittr)
library(dplyr)

x5_13 <- read.csv("./samochody.csv", sep = ';') %>%
  filter(rok >= 79 & rok <= 81) %>%
  select(moc, rok) %>%
  na.omit() %>%
  pull(moc) %>%
  as.numeric()

x5_13_1 <- length(x13[which(x13 == 84)])

prop.test(x5_13_1, length(x5_13), alternative = 'two.sided', conf.level = 0.99)
# Na poziomie istotności 0.01 należy odrzucić hipoteze zerową, że średnia populacji
# jest równa 84 na korzyść hipotezy alternatywnej, że średnia populacji
# jest różna od 84

# Zadanie 5.14. Oszacowano przeciętną długość życia w wybranych losowo 18 krajach.
# Wyniki przedstawia poniższa tabela:
# Kraj Długość życia
# Argentyna 70.5, Etiopia 51.5, Niemcy 76, Indie 57.5, Iran 64.5, Włochy 78.5
# Japonia 79, Kenia 61, Meksyk 72, Maroko 64.5, RPA 64, Hiszpania 78.5, Sudan 53
# Tajwan 75, Tajlandia 68.5, Turcja 70, Ukraina 70.5, USA 75.5
# Czy na podstawie tych danych możemy twierdzić, że średnia długość życia przekracza
# 62 lata? Przyjmij poziom istotności 0.05.

x5_14 <- c(70.5, 51.5, 76, 57.5, 64.5, 78.5, 79, 61, 72, 64.5, 64, 78.5, 53, 75, 68.5, 70, 70.5, 75.5)

t.test(x5_14, mu = 62, alternative = 'greater')
# Na poziomie istotności 0.05 należy odrzucić hipoteze zerową, że średnia populacji
# jest równa 62, na korzyść alternatywnej, że średnia populacji nie jest większa od 62

# Zadanie 5.15. Na podstawie danych dotyczących parametrów kilku wybranych marek samochodów
# (plik samochody.csv) stwierdź, czy występuje statystycznie istotna różnica w przyspieszeniu
# samochodów produkowanych w USA i w Japonii. Przyjmij poziom istotności α = 0.05

x15_5_1 <- read.csv("./samochody.csv", sep = ';', dec = ',') %>%
  filter(producent == 1) %>%
  select(producent, przysp) %>%
  na.omit() %>%
  pull(przysp) %>%
  as.double()

x15_5_2 <- read.csv("./samochody.csv", sep = ';', dec = ',') %>%
  filter(producent == 3) %>%
  select(producent, przysp) %>%
  na.omit() %>%
  pull(przysp) %>%
  as.double()

t.test(x5_15_1, x15_2, alternative = 'two.sided', var.equal = TRUE)
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że występuje statystyczna różnica

# Zadanie 5.16. Badano wytrzymałość 20 losowo wybranych wsporników betonowych,
# przy czym 10 z nich wykonano metodą tradycyjną, a pozostałe niedawno opatentowaną
# nową metodą. Wyniki pomiarów (w MPa) podano w poniższej tabeli:
# Metoda tradycyjna 53, 51, 62, 55, 59, 56, 61, 54, 47, 57
# Nowa metoda 62, 55, 61, 58, 54, 49, 56, 60, 52, 63
# Czy na podstawie tych danych można stwierdzić, że wytrzymałość wsporników wykonanych 
# nową metodą przewyższa istotnie wytrzymałość wsporników wykonanych metodą
# tradycyjną? Przyjmij poziom istotności 0.04

x5_16_1 <- c(53, 51, 62, 55, 59, 56, 61, 54, 47, 57)   
x15_6_2 <- c(62, 55, 61, 58, 54, 49, 56, 60, 52, 63)  

t.test(x5_16_2, x5_16_1, alternative = 'greater', var.equal = TRUE, conf.level = 0.96)
# Na poziomie istotności 0.04 nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że występuje wytrzymałość wsporników wykonanych 
# nową metodą nie przewyższa istotnie wytrzymałości wsporników wykonanych metodą tradycyjną

# Zadanie 5.17. Badano liczbę recept wypisywanych w ciągu 14 losowo wybranych dni
# przez pewnych dwóch lekarzy. Otrzymano następujące wyniki:
  # Lekarz I 19, 21, 15, 17, 24, 12, 19, 14, 20, 18, 23, 21, 17, 12
# Lekarz II 17, 15, 12, 12, 16, 15, 11, 13, 14, 21, 19, 15, 11, 10
# Zakładając, że badana cecha ma rozkład normalny, zweryfikuj przypuszczenie, że lekarz
# I wypisuje średnio więcej recept niż lekarz II. Przyjmij poziom istotności 0.05.

x5_17_1 <- c(19, 21, 15, 17, 24, 12, 19, 14, 20, 18, 23, 21, 17, 12)
x5_17_2 <- c(17, 15, 12, 12, 16, 15, 11, 13, 14, 21, 19, 15, 11, 10)

t.test(x5_17_1, x5_17_2, alternative = 'two.sided', var.equal = TRUE)
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że lekarz I wypisuje średnio więcej recept niż lekarz II

# Zadanie 5.18. Badano przeciętną długość filmów produkowanych przez dwie konkurujące
# ze sobą firmy. W tym celu wylosowano do badania kilka filmów i otrzymano
# następujące dane (w minutach):
# Długości filmów produkcji A 102, 86, 98, 109, 92, 102, 95, 120
# Długości filmów produkcji B 81, 165, 97, 134, 92, 87, 114, 120, 95, 136, 170
# Czy można twierdzić, że przeciętna długość filmów produkcji A przewyższa przeciętną
# długość filmów produkcji B? Zweryfikuj stosowną hipotezę na poziomie istotności 0.01.

x5_18_1 <- c(102, 86, 98, 109, 92, 102, 95, 120)
x5_18_2 <- c(81, 165, 97, 134, 92, 87, 114, 120, 95, 136, 170)

t.test(x5_18_1, x5_18_2, alternative = 'greater', var.equal = TRUE, conf.level = 0.99)
# Na poziomie istotności 0.01, nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że przeciętna długość filmów produkcji A przewyższa
# przeciętną długość filmów produkcji B 

# Zadanie 5.19. Badano wpływ nowego leku na zmianę poziomu pewnej substancji we
# krwi (w mg/ml). W tym celu zmierzono poziom tej substancji u 8 losowo wybranych
# osób, a następnie, po upływie 30 minut od podania owego leku, powtórzono badanie na
# tej samej grupie osób. Otrzymano następujące wyniki:
# Pacjent 1 2 3 4 5 6 7 8
# Poziom przed 2.76 5.18 2.68 3.05 4.10 7.05 6.60 4.79
# Poziom po 7.02 3.10 5.44 3.99 5.21 10.26 13.91 14.53
# Czy na podstawie powyższych danych można stwierdzić, że nowy lek powoduje istotne
# podwyższenie poziomu owej substancji we krwi? Przyjmij poziom istotności α = 0.05
# oraz założenie o normalności rozkładu badanej cechy.

x5_19_1 <- c(2.76, 5.18, 2.68, 3.05, 4.10, 7.05, 6.60, 4.79)
x5_19_2 <- c(7.02, 3.10, 5.44, 3.99, 5.21, 10.26, 13.91, 14.53)

t.test(x5_19_1, x5_19_2, alternative = 'greater', var.equal = TRUE)
# Na poziomie istotności 0.05, nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że nowy lek powoduje istotne
# podwyższenie poziomu owej substancji we krwi

# Zadanie 5.20. Badano wagę (w kilogramach) losowo wybranych kobiet palących papierosy
# przed i 5 tygodni po rzuceniu przez nich palenia. Otrzymano następujące wyniki:
# Waga przed 67 65 62 62 66 65 61 63 64 71 69 65 61 60
# Waga po 69 71 65 67 74 62 69 64 70 68 73 71 67 62
# Czy na podstawie powyższych danych można stwierdzić, że rzucenie palenia wpływa na
# wzrost wagi palącej papierosy kobiety? Przyjmij poziom istotności α = 0.05.

x5_20_1 <- c(67, 65, 62, 62, 66, 65, 61, 63, 64, 71, 69, 65, 61, 60)
x5_20_2 <- c(69, 71, 65, 67, 74, 62, 69, 64, 70, 68, 73, 71, 67, 62)

t.test(x5_20_1, x5_20_2, alternative = 'greater', var.equal = TRUE)
# Na poziomie istotności 0.05, nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że rzucenie palenia wpływa na
# wzrost wagi palącej papierosy kobiety

# Zadanie 5.21. Wśród pracowników naukowych pewnej uczelni przeprowadzono ankietę
# dotyczącą stażu pracy i stanu cywilnego. Otrzymano następujące wyniki liczby
# osób wedle stanu i stażu pracy (w latach):
# Staż pracy Panna/kawaler Mężatka/żonaty
# 0–5 6 20
# 5–10 8 20
# 10–15 3 60
# 15–20 2 25
# 20–25 1 15
# Zweryfikuj hipotezę, że w grupie mężatek i żonatych odsetek osób pracujących na owej
# uczelni dłużej niż 15 lat wynosi 0.3. Przyjmij poziom istotności α = 0.05

x5_21_1 <- c(6, 8, 3, 2, 1)
x5_21_2 <- c(20, 20, 60, 25, 15)

prop.test(sum(x5_21_2[4:5]), sum(x5_21_2[4:5]), p = 0.3, alternative = 'two.sided')
# Na podstawie istotności 0.05 należy odrzucić hipotezę zerową, że
# średnia w populacji jest rónwa 0.3, na korzyść hipotezy alternatywnej,
# że średnia nie jest równa 0.3

# 4.11. Lider pewnej partii politycznej powiedział w wywiadzie, że jego partia ma poparcie
# 25% społeczeństwa. W odpowiedzi przytoczono wyniki ankiety przeprowadzonej wśród tysiąca osób.
# Spośród ankietowanych tylko 240 osób popierało wspomnianą partię. Czy wyniki ankiety dowodzą,
# że lider nie jest zorientowany w rzeczywistym poparciu dla swojej partii?

x4_11_n <- 1000
x4_11_x <- 240

x4_11_p0 <- .25

prop.test(x4_11_x, x4_11_n, p = x4_11_p0)
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej, że lider nie 
# jest zorientowany w rzeczywistym poparciu dla swojej partii

# 4.12. Wyprodukowano pewien nowy środek owadobójczy. Producent gwarantuje 90% skuteczności.
# Środek ten zastosowano na tysiącu owadach, z których 852 padły. Czy środek ma taką skuteczność
# jaką gwarantuje producent?

x4_12_n <- 1000
x4_12_x <- 852

x4_12_p0 <- .9

prop.test(x4_12_x, x4_12_n, p = x4_12_p0)
# Na poziomie istotności 0.05 należy odrzucić hipotezę zerową, że środek ma skuteczność
# jaką gwarantuje producent, czyli 90%
  
# 4.13. Czy można twierdzić, że wadliwość procesu produkcyjnego wynosi 2%, jeżeli na 
# 50 przebadanych wyrobów stwierdzono dwa braki.

x4_13_n <- 50
x4_13_x <- 2

x4_13_p0 <- 0.02

prop.test(x4_13_x, x4_13_n, p = x4_13_p0)
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowaj, że
# wadliwość procesu produkcyjnego wynosi 2%

# 4.14. Czy można stwierdzić, że w transporcie psuje się 25% owoców, jeżeli na 
# 200 przebadanych owoców było 60 zepsutych.

x4_14_n <- 200
x4_14_x <- 60

x4_14_p0 <- .25

prop.test(x4_14_x, x4_14_n, p = x4_14_p0)
# Na poziomie istotnosci 0.05 nie ma podstaw do odrzucenia hipotezy zerowaj, że
# w transporcie psuje się 25% owoców

# 4.15. Na 800 zbadanych pacjentów pewnego szpitala 320 miało grupę krwi „O”. 
# Zweryfikować hipotezę, że procent pacjentów z tą grupą wynosi 35.

x4_15_n <- 800
x4_15_x <- 320

x4_15_p0 <- .35

prop.test(x4_15_x, x4_15_n, p = x4_15_p0)
# Na poziomie istotności 0.05 należy odrzucić hipotezę zerowa, że procent pacjętów
# z grupą krwi "O" wynosi 35%

# 5.6. Dwóm grupom robotników zlecono wykonanie tej samej pracy z tym jednak, że robotnicy grupy 
# pierwszej przeszli wcześniej przeszkolenie. Zaobserwowana wydajność pracy w pierwszej grupie kształtowała
# się następująco (w szt/h): 18.6, 17.9, 18.1, 17.0, 18.7, 18.3, podczas gdy w grupie drugiej zaobserwowano
# następujące wydajności: 17.3, 17.6, 17.1, 16.0, 17.8. Na poziomie istotności 0.05 sprawdzić, czy przeszkolenie
# zmieniło wydajność pracy robotnika.

x5_6_1 <- c(18.6, 17.9, 18.1, 17.0, 18.7, 18.3)
x5_6_2 <- c(17.3, 17.6, 17.1, 16.0, 17.8)

t.test(x5_6_1, x5_6_2, alternative = 'two.sided')
# Na poziomie istotności 0.05 należy odrzucić hipotezę zerową, że przeszkolenie zmieniło
# wydajność pracy robootnika, na korzyść hipotezy alternatywnej, że wydajność się nie zminiła

# 5.11. Wysunięto przypuszczenie, że jakość produkcji pewnego wyrobu po wprowadzeniu nowej, tańszej technologii nie
# uległa zmianie. Wylosowano próbę 120 sztuk tego wyrobu spośród wyprodukowanych starą technologią i otrzymano 12 sztuk złych.
# Wśród 160 wylosowanych sztuk wyprodukowanych nową technologią było 20 sztuk wadliwych. Czy wysunięte przypuszczenie można
# w świetle uzyskanych wyników uznać za uzasadnione?

prop.test(c(12, 20), c(120, 160), alternative = 'two.sided')
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej, że 
# jakość produkcji pewnego wyrobu po wporwadzeniu nowej technologi nie uległa zmianie
  
# 5.12. W pewnej szkole rozeszła się plotka, że uczniowie chcą ogolić dyrektora. Nauczyciel matematyki zapytał
# o to 150 dziewcząt i 200 chłopców. Dziewięćdziesięciu chłopców i 70% dziewcząt odpowiedziało twierdząco. 
# Czy można uznać, że chęć ogolenia dyrektora zależy od płci?
  
prop.test(c(90, 150 * .7), c(200, 150), alternative = 'two.sided')
# Na poziomie istotności 0.05 należy odrzucić hipotezę zerową na ktorzyść hipotezy alternatynej, że
# chęć ogolenia dyrektora zależy od płci

# 5.13. Na 200 przebadanych szczurów u 60 stwierdzono objawy obniżonego refleksu. Wśród chorych szczurów tylko
# 20 dostawało pewien preparat P , a wszystkich szczurów karmionych tym preparatem było 80. Czy można uznać, 
# że karmienie preparatem P wpływa na obniżenie refleksu u szczurów?

prop.test(c(20, 40), c(80, 120), alternative = 'two.sided')
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej, że
# karminie preparaem P wpływa na odniżnie refleksu u szczórow
