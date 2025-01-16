install.packages("tidyverse")
install.packages("ggthemes")
remotes::install_github("christianholland/AachenColorPalette")
install.packages("esquisse")

library(tidyverse)
library(ggthemes)
library(AachenColorPalette)
library(plotrix)

rwthcolor <- hcictools::rwth.colorpalette()
df <- readRDS("data/data.rds")



library(ggplot2)

ggplot(df) +
 aes(x = Age) +
  geom_histogram(bins = 30L, fill = "#112446") +
  geom_vline(xintercept = mean(df$Age, na.rm = TRUE)) +
  geom_text(x = mean(df$Age, na.rm = TRUE), y = 20, label = paste0("M = ", round(mean(df$Age, na.rm = TRUE), 2)), angle = 90, vjust = 1.5 ) +
 labs(x = "Alter in Jahren", y = "Anzahl der Proband*innen", title = paste0("Altersverteiliung n = (", nrow(df),")"), 
 subtitle = " Histogram der Altersverteilung", caption = " 30 Bins ") +
 theme_minimal()
ggsave(filename = "histogramAlter.png", width = 10, height = 8, units = "cm")


library(ggplot2)

ggplot(df) +
  aes(x = ATI, y = Mn) +
  geom_boxplot(fill = "#112446") +
  labs(x = "Technikaffinität UV ", y = " Digitale Medienutzung (AV)", title = " Zusammenhang zwischen Technikaffinität und digialer Mediennutzung n = (", nrow(df),") ", 
       subtitle = " Bloxplot Technikaffinität und digialer Mediennutzung ") +
  theme_minimal()
ggsave(filename = "BoxplotATI&MN.png", width = 15, height = 10, units = "cm")

library(ggplot2)

ggplot(df) +
  aes(x = Age, y = PD) +
  geom_point(colour = "#112446") +
  geom_point(colour = "#112446") +
  labs(x = " Alter in Jahren ", y = " Privatsphärendisposition ", title = " Zusammenhang zwischen Alter und Privatsphärendisposition n = (", nrow(df),") ", 
       subtitle = " Punktdiagramm Alter und  Privatsphärendisposition ") +
  theme_minimal()
ggsave(filename = "PunktdiagrammAlter&Privatsphäre.png", width = 10, height = 8, units = "cm")


library(ggplot2)
library(dplyr)

df_clean <- df %>% 
  filter(!is.na(Bildungsabschluss) & !is.na(ATI)) %>%
  mutate(Bildungsgruppe = case_when( 
    Bildungsabschluss > "Abitur" ~ "Höher als Abitur", 
    Bildungsabschluss < "Abitur" ~ "Niedriger als Abitur"
  ))

df_summary <- df_clean %>%
  group_by(Bildungsgruppe) %>%
  summarise(
    mean_ATI = mean(ATI, na.rm = TRUE),
    sd_ATI = sd(ATI, na.rm = TRUE)
  )

ggplot(df_summary, aes(x = Bildungsgruppe, y = mean_ATI)) +
  geom_bar(width = 0.5, stat = "identity", fill = rwthcolor$bordeaux) +
  geom_errorbar(aes(ymin = mean_ATI - sd_ATI, ymax = mean_ATI + sd_ATI), width = 0.2) +
  labs(title = "Mittelwert des ATI-Werts nach Bildungsgruppe", x = "Bildungsgruppe", y = "Mittlerer ATI-Wert") +
  theme_minimal()

#t.test( filter(df, Gender == "Weiblich")$PW , filter(df, Gender == "Männlich")$PW )


library(dplyr)
library(ggplot2)

data %>%
 filter(Gender %in% c("Männlich", "Weiblich")) %>%
 ggplot() +
 aes(x = Gender, y = PW, fill = Gender) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 labs(x = "Geschlecht", y = "Privatsphärewahrnehmung", title = "Frauen haben eine höheres Empfinden der Privatsphäre bei der Nutzung eines KI-Chatbots als Männer.", subtitle = " ", caption = " ") +
 theme_minimal()



