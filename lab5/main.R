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

x11 = c(92, 88, 85, 82, 89, 86, 81, 66, 75, 61, 78, 76, 91, 82, 82)

t.test(x11, mu = 78, alternative = 'greater', conf.level = 0.98)
# Na poziomie istotności 0.02, nie ma podstaw do odrzucenia hipotezy zerowej,
# że średnia populacji jest równa 78

# Zadanie 5.12. Pewien księgowy przypuszcza, że przeciętne saldo na kontach klientów
# jego firmy jest mniejsze niż 31 tys. euro. Żeby to sprawdzić, pobrał losową próbę kont,
# otrzymując następujące wyniki dotyczące przeciętnego salda (w tys. euro):
  # 30.0, 30.0, 29.9, 31.3, 32.0, 32.0, 32.1, 30.5, 32.3, 29.5, 27.8, 27.3, 31.1,
# 30.7, 24.5, 28.3, 31.3, 32.7, 33.3, 26.8.
# Czy prawdziwe jest przypuszczenie księgowego?
# Zweryfikuj stosowną hipotezę na poziomie istotności 0.01.

x12 = c(30.0, 30.0, 29.9, 31.3, 32.0, 32.0, 32.1, 30.5, 32.3, 29.5, 27.8, 27.3, 31.1, 30.7, 24.5, 28.3, 31.3, 32.7, 33.3, 26.8)

t.test(x12, mu = 31, alternative = 'less', conf.level = 0.99)
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy zerowej,
# że średnia populacji wynosi 31

# Zadanie 5.13. Na podstawie danych zawartych w pliku samochody.csv zweryfikuj
# przypuszczenie, że średnia moc silnika samochodów wyprodukowanych w latach 1979–
# 1981 wynosi 84 KM (wykorzystaj zmienne moc i rok). Przyjmij poziom istotności 0.01.
x13 <- read.csv("./samochody.csv", sep = ';') %>%
  filter(rok >= 79 & rok <= 81) %>%
  select(moc, rok) %>%
  na.omit() %>%
  pull(moc) %>%
  as.numeric()

x13_s <- length(x13[which(x13 == 84)])
x13_s

prop.test(x13_s, length(x13), alternative = 'two.sided', conf.level = 0.99)
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

x14 <- c(70.5, 51.5, 76, 57.5, 64.5, 78.5, 79, 61, 72, 64.5, 64, 78.5, 53, 75, 68.5, 70, 70.5, 75.5)

t.test(x14, mu = 62, alternative = 'greater')
# Na poziomie istotności 0.05 należy odrzucić hipoteze zerową, że średnia populacji
# jest równa 62, na korzyść alternatywnej, że średnia populacji nie jest większa od 62

# Zadanie 5.15. Na podstawie danych dotyczących parametrów kilku wybranych marek samochodów
# (plik samochody.csv) stwierdź, czy występuje statystycznie istotna różnica w przyspieszeniu
# samochodów produkowanych w USA i w Japonii. Przyjmij poziom istotności α = 0.05

x15_1 <- read.csv("./samochody.csv", sep = ';', dec = ',') %>%
  filter(producent == 1) %>%
  select(producent, przysp) %>%
  na.omit() %>%
  pull(przysp) %>%
  as.double()

x15_2 <- read.csv("./samochody.csv", sep = ';', dec = ',') %>%
  filter(producent == 3) %>%
  select(producent, przysp) %>%
  na.omit() %>%
  pull(przysp) %>%
  as.double()

t.test(x15_1, x15_2, alternative = 'two.sided', var.equal = TRUE)
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

x16_1 <- c(53, 51, 62, 55, 59, 56, 61, 54, 47, 57)   
x16_2 <- c(62, 55, 61, 58, 54, 49, 56, 60, 52, 63)  

t.test(x16_2, x16_1, alternative = 'greater', var.equal = TRUE, conf.level = 0.96)
# Na poziomie istotności 0.04 nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że występuje wytrzymałość wsporników wykonanych 
# nową metodą nie przewyższa istotnie wytrzymałości wsporników wykonanych metodą tradycyjną

# Zadanie 5.17. Badano liczbę recept wypisywanych w ciągu 14 losowo wybranych dni
# przez pewnych dwóch lekarzy. Otrzymano następujące wyniki:
  # Lekarz I 19, 21, 15, 17, 24, 12, 19, 14, 20, 18, 23, 21, 17, 12
# Lekarz II 17, 15, 12, 12, 16, 15, 11, 13, 14, 21, 19, 15, 11, 10
# Zakładając, że badana cecha ma rozkład normalny, zweryfikuj przypuszczenie, że lekarz
# I wypisuje średnio więcej recept niż lekarz II. Przyjmij poziom istotności 0.05.

x17_1 <- c(19, 21, 15, 17, 24, 12, 19, 14, 20, 18, 23, 21, 17, 12)
x17_2 <- c(17, 15, 12, 12, 16, 15, 11, 13, 14, 21, 19, 15, 11, 10)

t.test(x17_1, x17_2, alternative = 'two.sided', var.equal = TRUE)
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że lekarz I wypisuje średnio więcej recept niż lekarz II

# Zadanie 5.18. Badano przeciętną długość filmów produkowanych przez dwie konkurujące
# ze sobą firmy. W tym celu wylosowano do badania kilka filmów i otrzymano
# następujące dane (w minutach):
# Długości filmów produkcji A 102, 86, 98, 109, 92, 102, 95, 120
# Długości filmów produkcji B 81, 165, 97, 134, 92, 87, 114, 120, 95, 136, 170
# Czy można twierdzić, że przeciętna długość filmów produkcji A przewyższa przeciętną
# długość filmów produkcji B? Zweryfikuj stosowną hipotezę na poziomie istotności 0.01.

x18_1 <- c(102, 86, 98, 109, 92, 102, 95, 120)
x18_2 <- c(81, 165, 97, 134, 92, 87, 114, 120, 95, 136, 170)

t.test(x18_1, x18_2, alternative = 'greater', var.equal = TRUE, conf.level = 0.99)
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

x19_1 <- c(2.76, 5.18, 2.68, 3.05, 4.10, 7.05, 6.60, 4.79)
x19_2 <- c(7.02, 3.10, 5.44, 3.99, 5.21, 10.26, 13.91, 14.53)

t.test(x19_1, x19_2, alternative = 'greater', var.equal = TRUE)
# Na poziomie istotności 0.05, nie ma podstaw do odrzucenia hipotezy zerowej,
# więc nie można stwierdzić, że nowy lek powoduje istotne
# podwyższenie poziomu owej substancji we krwi

# Zadanie 5.20. Badano wagę (w kilogramach) losowo wybranych kobiet palących papierosy
# przed i 5 tygodni po rzuceniu przez nich palenia. Otrzymano następujące wyniki:
# Waga przed 67 65 62 62 66 65 61 63 64 71 69 65 61 60
# Waga po 69 71 65 67 74 62 69 64 70 68 73 71 67 62
# Czy na podstawie powyższych danych można stwierdzić, że rzucenie palenia wpływa na
# wzrost wagi palącej papierosy kobiety? Przyjmij poziom istotności α = 0.05.

x20_1 <- c(67, 65, 62, 62, 66, 65, 61, 63, 64, 71, 69, 65, 61, 60)
x20_2 <- c(69, 71, 65, 67, 74, 62, 69, 64, 70, 68, 73, 71, 67, 62)

t.test(x20_1, x20_2, alternative = 'greater', var.equal = TRUE)
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

x21_1 <- c(6, 8, 3, 2, 1)
x21_2 <- c(20, 20, 60, 25, 15)

n <- x21_1 + x21_2

prop.test(sum(x21_2[4:5]), sum(x21_2[4:5]), p = 0.3, alternative = "two.sided")
# Na podstawie istotności 0.05 należy odrzucić hipotezę zerową, że
# średnia w populacji jest rónwa 0.3, na korzyść hipotezy alternatywnej,
# że średnia nie jest równa 0.3

