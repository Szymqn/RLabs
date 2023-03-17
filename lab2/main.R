# Zbuduj odpowiadający mu wykres kołowy.
# Zbuduj odpowiadający mu wykres pierścieniowy.


library(tidyverse)

acsNew <- read_csv("http://www.jaredlander.com/data/acsNew.csv")

summary(acsNew)

acsNewFactor <- acsNew %>%
  mutate(across(where(is.character), as.factor))

summary(acsNewFactor)

l1 <- c('Azjatycki Pacyfik', 'Angielski', 'Inny', 'Inny Europejski', 'Hiszpański')

p1 <- acsNew %>%
  ggplot(aes(x = Language, fill = `Language`)) +
  geom_bar(width = 0.5) +
  labs(title = "Języki",
      caption = "Opracowano na podstawie Jared Lander/acsNew",
  ) +
  scale_x_discrete(name = "Języki", labels = l1) +
  scale_fill_discrete(name = "Języki", labels = l1)

p1

p1 + theme_linedraw()

p2 <- ggplot(Lang)

p2
