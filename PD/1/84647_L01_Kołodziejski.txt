library(tidyverse)
library(DT)

x_axis <- c(seq(from = 0.4, to = 0.001, by = -0.1), 0.05, 0.025, 0.02, 0.01, 0.005, 0.001)
y_axis <- c(seq(from = 1, to = 35, by = 1), seq(from = 40, to = 100, by = 5))

t_table <- matrix(NA, nrow = length(y_axis), ncol = length(x_axis)) %>%
  `colnames<-`(x_axis) %>%
  `rownames<-`(y_axis)

for (i in seq_along(y_axis)) {
  for (j in seq_along(x_axis)) {
    t_table[i, j] <- round(qt(1 - x_axis[j]/2, y_axis[i]), digits = 4)
  }
}

t_table

datatable(t_table, caption = 'Tabela: Tablica rozkładu t-Studenta.')

