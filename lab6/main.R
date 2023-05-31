# Rozwiąż zadania 6.11 - 6.20 z podręcznika [11].

# Zadanie 6.11. Spośród studentów czterech wydziałów, na których pan Iksiński wykłada
# najciekawszy przedmiot świata (osobom niezorientowanym wyjaśniamy, że mowa tu
# oczywiście o statystyce matematycznej), pobrano próbki losowe i zliczono studentów
# (zwanych dalej „szczęśliwcami”), którym udało się zdać egzamin z tego przedmiotu.
# Wyniki zamieszczono w poniższej tabeli:

# Wydział                   Liczność próbki Liczba szczęśliwców
# Nauk niepotrzebnych       206             61
# Mniemanologii stosowanej  164             34
# Nauk ciekawych            98              38
# Nauk przydatnych          102             35
# Czy w świetle zebranych danych można stwierdzić, że występują istotne różnice między
# odsetkami osób na poszczególnych wydziałach, które zdały statystykę? Przyjmij poziom
# istotności α = 0.05.
 
prop.test(c(61, 34, 38, 35), c(206, 164, 98, 102))

# Na poziomie istotności 0.05 należ odrzucić hipotezę zerową, że
# nie występują istotne różnice między odsetkami osób na poszczególnych wydziałach,
# które zdały statystykę, na korzyść hipotezy alternatywnej, że 
# występują istotne różnice między odsetkami osób, które zdały egzmin

