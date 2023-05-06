x_i <- c(-5, -2, 0, 1, 3, 8)
p_i <- c(.1, .2, .1, .2, .2, .1)

# parametry liczbowe
p_liczowe <- function(x_i, p_i, k) {
  # wartość oczekiwana
  EX <- sum(x_i*p_i)
  EX2 <- sum((x_i**2)*p_i)
  # moment zwykły
  m_z <- 1/length(x_i) * sum(((x_i**k)*p_i))
  # moment absolutny 
  m_a <- 1/length(x_i) * sum((abs(x_i)**k)*p_i)
  # wariacja
  D2X <- EX2 - EX
  # odchylenie standardowe
  DX <- sqrt(D2X)
  # odchylenie przeciętne
  d <- sum(abs(x_i - mean(x_i))) / length(x_i)
  
  list(m_z, m_a, D2X, DX, d)
}

p_liczowe(x_i, p_i, 3)

# parametry pozycyjne

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

p_pozycyjne <- function(x_i, p_i) {
  Q_1 <- quantile(x_i, .25)
  Q_3 <- quantile(x_i, .75)
  EX <- sum(x_i*p_i)
  # kwantyl
  q <- quantile(x_i)
  names(q) <- NULL
  # mediana
  Me <- median(x_i)
  # moda
  Mo <- getmode(x_i)
  # odchylenie ćwiartkowe
  Q <- (Q_3 - Q_1) / 2
  names(Q) <- NULL
  # pozycyjny typowy obszar zmienności
  T_Q <- c(Me - Q, Me + Q)
  # pozycyjny wskaźnik asymetrii 
  W_S <- (Q_3 - Me) - (Me - Q_1)
  names(W_S) <- NULL
  # pozycyjny współczynnik asymetrii
  A_Q <- ((Q_3 - Me) - (Me - Q_1))/2 * Q
  names(A_Q) <- NULL
  # wskaźnik asymetrii
  W_S <- EX - Mo
  
  list(q, Me, Mo, Q, T_Q, W_S, A_Q, W_S)
}

p_pozycyjne(x_i, p_i)
