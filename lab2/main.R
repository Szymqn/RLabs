library(tidyverse)
library(ggplot2)
library(readr)

acsNew <- read_csv("./acsNew.csv", show_col_types = FALSE)

summary(acsNew)

l1 <- c('Azjatycki Pacyfik', 'Angielski', 'Inny', 'Inny Europejski', 'Hiszpański')

p1 <- acsNew %>%
  ggplot(aes(x = Language, fill = `Language`)) +
  geom_bar(width = 0.5) +
  labs(title = "Język",
      caption = "Opracowano na podstawie Jared Lander/acsNew",
      y = "ilość",
      subtitle = "Dane z 2002",
      
  ) +
  scale_x_discrete(name = "Język", labels = l1) +
  scale_fill_discrete(name = "Język", labels = l1)

p1

p1 + theme_linedraw()

p2 <- acsNew %>%
  group_by(Language) %>%
  summarise(count = n()) %>%
  arrange(desc(Language)) %>%
  mutate(prop = count / sum(count) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
  ggplot(aes(x = "", y = prop, fill = Language)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  labs(title = "Język",
       caption = "Opracowano na podstawie Jared Lander/acsNew",
       subtitle = "Dane z 2002",
  ) +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "black", size = 4, angle = 50) +
  theme_void() +
  scale_fill_discrete(name = "Język", labels = l1)

p2

p3 <- acsNew %>%
  group_by(Language) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(prop = count / sum(count) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1))) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Language)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) + 
  labs(title = "Język",
       caption = "Opracowano na podstawie Jared Lander/acsNew",
       subtitle = "Dane z 2002",
  ) +
  theme_void() +
  scale_fill_discrete(name = "Język", labels = l1) +
  geom_text(x = 3.5, aes(y = (ymax + ymin) / 2, label = paste0(round(prop, 1), "%")), color = "black", size = 4, angle = 50)

p3

p4 <- acsNew %>%
  ggplot(aes(x = HeatingFuel, y = HouseCosts)) +
  geom_violin(color = "brown", fill = "antiquewhite") +
  geom_jitter(color = "red", alpha = .3) +
  labs(title = "Wykresy wioliniowe kosztów utrzymania domu",
       y = "Koszt utrzymania",
       x = "Rodzaj ogrzewania",
       caption = "Opracowanie własne na podstawie dany J.Landera",
       subtitle = "Dane z roku 2002") +
  theme(legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(colour = "red"),
        legend.title = element_text(face = "bold"))

p4

library(scales)
library(hrbrthemes)

p5 <- acsNew %>%
  ggplot(aes(x=x)) +
  geom_histogram(aes(x = Insurance, y = -..count..), color = "darkolivegreen", fill = "darkolivegreen1", bins = 65) +
  geom_histogram(aes(x = HouseCosts, y = ..count..), color = "red3", fill = "lightsalmon1", bins = 65) +
  labs(title = "Podwójny histogram",
       x = "Ubezpieczenie domu",
       y = "Liczba przypadków",
       caption = "Opracowanie własne na podstawie dany J.Landera",
       subtitle = "Dane z roku 2002 (plik acsNew.csv).",
       ) +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = .5, color = "brown", face = "bold.italic"),
        
        plot.subtitle = element_text(hjust = 1, color = "navy"),
        
        axis.title.x.bottom = element_text(size = 12, hjust = .5),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(.3, "cm"), ends = "both"), size = 1),
        
        axis.title.y.left = element_text(size = 15, color = "green4", hjust = .5),
        axis.text.y.left = element_text(size = 10, color = "green4", hjust = 1, vjust = .3, angle = 65),
        axis.line.y.left = element_line(arrow = grid::arrow(length = unit(.3, "cm"), ends = "last", type = "closed"), size = 1, color = "green4"),
        
        axis.line.y.right = element_line(arrow = grid::arrow(length = unit(.3, "cm"), ends = "last", type = "closed"), size = 1, color = "orangered3"),
        axis.title.y.right = element_text(size = 15, color = "orangered3", hjust = .5, angle = -90),
        axis.text.y.right = element_text(size = 10, color = "orangered3", hjust = -0.1, vjust = .3, angle = -80),
        
        panel.background = element_rect()
       ) +
  scale_y_continuous(breaks = seq(350, -350, by = -50),
                     labels = c(seq(350, 50, -50), seq(0, 350 , 50))) +
  scale_x_continuous(labels = dollar,
                     sec.axis = sec_axis(~., name = "Koszty utrzymania domu", labels = dollar)) +
  coord_flip()

p5
