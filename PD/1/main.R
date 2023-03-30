library(tidyverse)
library(DT)

x_axis <- c(seq(from = 0.4, to = 0.001, by = -0.1), 0.05, 0.025, 0.02, 0.01, 0.005, 0.001)
y_axis <- c(seq(from = 1, to = 35, by = 1), seq(from = 40, to = 100, by = 5))

rozkladTStudenta <- outer(
  function(x_axis, y_axis) 
  qt(1 - y_axis/2, x_axis)) %>%
  matrix(ncol = length(y_axis), byrow = FALSE) %>%
  round(digits = 4)

rownames(rozkladTStudenta) <- x_axis
colnames(rozkladTStudenta) <- y_axis

datatable(rozkladTStudenta, 
          caption = 'Tabela: Tablica rozkładu t-Studenta.',
          options = list(lengthMenu = c(50, 100)))

