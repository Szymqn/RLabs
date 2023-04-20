# 1. Napisz funkcję, która przyjmuje dwa argumenty: pierwszym jest wektor, a drugim liczba całkowita b.
# Program zwraca tylko te elementy wektora, które dzielą się bez reszty przez b. Wywołaj funkcję z przykładowymi danymi.

fun1 <- function(values, b) {
  vector[vector %% b == 0]
}

vector1 <- seq(1, 6)
b <- 3
filtered_vector <- fun1(vector1, b)
print(filtered_vector)

# Napisz funkcję, która wyznacza pierwiastki równania kwadratowego. Funkcja ta przyjmuje trzy argumenty: a, b i c. 
# Wywołaj funkcję z przykładowymi danymi.

fun2 <- function(a, b, c) {
  stopifnot('Współczynik przy x^2 musi być niezerowy' = a != 0)
  delta <- b^2 - 4 * a * c
  result <- list(message = NA_character_, solution_first = NULL, solution_second = NULL)
  if (delta < 0) {
    result[[1]] <- "Równanie nie ma pierwiastków (wyróżnik jest ujemny)."
    result
  } else {
    if (delta == 0) {
      result[[1]] <- "Równanie ma jeden pierwiastek podwójny."
      result[[2]] <- -b/(2*a)
      result
    } else {
      result[[1]] <- "Równanie ma dwa pierwiastki"
      result[[2]] <- (-b - sqrt(delta))/(2*a)
      result[[3]] <- (-b + sqrt(delta))/(2*a)
      result
    }
  }
}

fun2(1, -5, 6)

# Napisz funkcję, która jako argument przyjmuje wektor liczb i sortuje te liczby wykorzystując funkcję sort().
# Następnie w funkcji wyświetl pierwsze 3 liczby (czyli 3 najmniejsze liczby z wektora).
# Wywołaj funkcję z przykładowymi danymi.

func3 <- function(vector) {
  stopifnot('Podany wektor jest na mały (min 3).' = length(vector) >= 3)
  sorted_vector <- sort(vector)
  sorted_vector
  paste('3 najmniejsze wartości:', sorted_vector[1], sorted_vector[2], sorted_vector[3])
  # paste('3 największe wartości:', sorted_vector[-1], sorted_vector[-2], sorted_vector[-3])
  }

vector2 = c(4, 1, 6, 8, 2)

func3(vector2)

