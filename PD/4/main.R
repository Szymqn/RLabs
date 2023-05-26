# Każdy z dwóch niezależnych systemów alarmowych działa z prawdopodobieństwem 0.9
# Jakie jest prawdopodobieństwo, że oba zawiodą jednocześnie.

# prawdopodobieńswto, że system działa
p1 <- .9
# prawdopodobieńswo, że system zawodzi
p2 <- 1 - p1

# oba systemy zawodzą
result <- p2 * p2
result

# Prawdopodobieństwo, że aba systemy alarmowe zawiodą jednocześnie wynosi 0.01
