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


library(dplyr)
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
library(dplyr)

ggplot(df) +
 aes(x = Age, y = Mn) +
 geom_boxplot(fill = "#112446") +
 labs(x = "Alter in Jahren", y = "Digitale Mediennutzung", 
 title = "Zusammenhang von Alter und Nutzung digitaler Medien") +
 theme_minimal()
ggsave(filename = "Hypothese1.png", width = 10)

library(ggplot2)
library(dplyr)

ggplot(df) +
 aes(x = Age, y = PD) +
 geom_point(colour = "#112446") +
 labs(x = "Alter in Jahren", y = "Privatsphärenakzeptanz", 
 title = "Zusammenhang von Alter und Privatspährendisposition", subtitle = "Punktdiagramm") +
 theme_minimal()
ggsave(filename = "Hypothese2.png", width = 10)


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
ggsave("Hypothese5.png", width = 6)

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
ggsave("Hypothese6.png", width = 6)

#H7
library(dplyr)
library(ggplot2)

ggplot(df) +
  aes(x = BI) +
  geom_histogram(bins = 35L, fill = "black") +
  labs(title = "Verteilung der Nutzungsintentionen (BI)", 
       x = "Nutzungsintention (BI)", 
       y = "Häufigkeit") +
  theme_minimal()
ggsave("Hypothese_7.png", width = 6)

