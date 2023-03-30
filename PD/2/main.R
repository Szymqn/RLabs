library(tidyverse)
library(ggplot2)
library(readr)

acsNew <- read_csv("./acsNew.csv", show_col_types = FALSE)

summary(acsNew)

p1 <- acsNew %>%
  group_by(Language) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(prop = count / sum(count) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)), 
         angle = c(-50, -35, -70, -80, -85)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Language)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  labs(title = "Wykres pierścieniowy rodzin w NY wg języka",
       caption = "Źródło: Opracowano własne na podstawie J.Landera",
       subtitle = "Dane z roku 2002",
  ) +
  geom_text(aes(x = c(2.55, 2, 2.45, 2.1, 2.65), y = (ymax + ymin) / 2, label = Language, angle = angle), fontface = "bold", size = 5, color = "grey20") +
  theme_void() +
  theme(plot.title = element_text(hjust = .5, color = "limegreen"),
        plot.subtitle = element_text(hjust = 1),
        legend.position = "none")

p1
