install.packages("tidyverse")
install.packages("ggthemes")
remotes::install_github("christianholland/AachenColorPalette")
install.packages("esquisse")

library(tidyverse)
library(ggthemes)
library(AachenColorPalette)

df <- readRDS("data/data.rds")

display_aachen_colors()

library(ggplot2)

ggplot(df) +
  aes(x = Age) +
  geom_histogram(bins = 30L, fill = "#112446") +
  geom_vline(xintercept = mean(df$Age, na.rm = TRUE)) +
  geom_text(x = mean(df$Age, na.rm = TRUE), y = 20, label = paste0("M = ", round(mean(df$Age, na.rm = TRUE), 2)), angle = 90, vjust = 1.5 ) +
  labs(x = "Alter in Jahren", y = " Anzahl der Proband*innen ", title = paste0("Altersverteiliung n = (", nrow(df),")"), 
       subtitle = "Histogramm der Altersverteilung", caption = " 30 Bins ") +
  theme_minimal()
ggsave(filename = "histogramAlter.png", width = 10, height = 8, units = "cm")

