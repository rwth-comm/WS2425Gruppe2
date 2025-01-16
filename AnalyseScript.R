# Pakete aktivieren ----
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/Methodenseminar+WS2425_10.+Januar+2025_09.51.csv")


# Rohdaten filtern ----
raw <-filter(raw,Progress == 100) 
raw <-filter(raw,Status == 0)


# Überflüssige Rohdaten entfernen ----

raw.short <- raw[c(16:244),c(6,9,19:64,94:104)]

dput(names(raw.short))

names(raw.short) <- c("Duration", "ResponseId", "Gender", "Age", "Bildungsabschluss","Wohnort", "Income", 
    "big5_1", "big5_2n", 
    "nzv_1n", "nzv_2", "nzv_3", 
    "ati_1", "ati_2", "ati_3n", "ati_4", "ati_5", "ati_6n", "ati_7", "ati_8n", "ati_9", 
    "Mn", 
    "pd_1n", "pd_2n", "pd_3", 
    "egki_1n", "egki_2", "egki_3n", "egki_4", "egki_5n", 
    "expkic_1", "expkic_2", "expkic_3", 
    "ezkv_1", "ezkv_2", "ezkv_3", "ezkv_4", "ezkv_5", "ezkv_6n", 
    "bsszo_1", "bsszo_2n", "bsszo_3n", "bsszo_4n", "bsszo_5", 
    "egn_1", "egn_2", "Kontrolle", "egn_4", 
    "bi_c_1", "bi_c_2", "bi_c_3", 
    "att_c_4", "att_c_5", "att_c_6", 
    "pw_c_1n", "pw_c_2n", "pw_c_3n", 
    "tia_c_1", "tia_c_2")


# Korrekte Datentypen zuordnen ----
raw.short$Age <- as.numeric(raw.short$Age)

raw.short$Gender <- as.factor(recode(raw.short$Gender, 
  `1` = "Männlich", `2` = "Weiblich", `3` = "Divers", `4` = "Keine Angabe"))

raw.short$Bildungsabschluss <- ordered(raw.short$Bildungsabschluss,levels = c(1:5),
          labels = c("(noch) kein Schulabschluss",
                     "Hauptschulabschluss",
                     "Realschulabschluss",
                     "Abitur",
                     "Hochschulabschluss"))

raw.short$Wohnort <- as.factor(recode(raw.short$Wohnort,
  `1` = "Ländlich", `2` = "Wohnort/Kleinstadt", `3` = "Großstadt"))
  

raw.short$Income <- ordered(raw.short$Income, levels = c(1:4),
          labels = c("unter 25.000€",
                     "25.000€ bis 49.999€",
                     "50.000€ bis 75.000€",
                     "über 75.000€"))

raw.short$Mn <- ordered(raw.short$Mn, levels = c(1:6),
          labels = c("nie",
                     "einmal im Monat",
                     "mehrmals pro Monat",
                     "einmal pro Woche",
                     "mehrmals in der Woche",
                     "täglich"))


# Skalenwerte berechnen ----

schluesselliste <- list(
  BIG5 = c("big5_1","-big5_2n"),
  NZV = c("-nzv_1n","nzv_2","nzv_3"),
  ATI = c("ati_1","ati_2","-ati_3n","ati_4","ati_5","-ati_6n","ati_7","-ati_8n","ati_9"),
  PD = c("-pd_1n","-pd_2n", "pd_3"),
  EGKI = c("-egki_1n","egki_2","-egki_3n","egki_4","-egki_5n"),
  EXPKIC = c("expkic_1","expkic_2","expkic_3"),
  EZKV = c("ezkv_1","ezkv_2","ezkv_3","ezkv_4","ezkv_5","-ezkv_6n"),
  BSSZO = c("bsszo_1","-bsszo_2n","-bsszo_3n","-bsszo_4n","bsszo_5"),
  EGN = c("egn_1","egn_2","egn_4"),
  BI = c("bi_c_1","bi_c_2","bi_c_3"),
  ATT = c("att_c_4","att_c_5","att_c_6"),
  PW = c("-pw_c_1n","-pw_c_2n","-pw_c_3n"),
  TIA = c("tia_c_1","tia_c_2")
)

scores <- scoreItems(schluesselliste, items = raw.short, min = 1, max =6)

scores$alpha

scores$scores

data <- bind_cols(raw.short, scores$scores)

# Daten exportieren ----
write_rds(data, "data/data.rds")

# Qualitätskontrolle ----

raw.short.quality <- careless_indices(raw.short, likert_vector = c(7:39), duration_column = "Duration", speeder_analysis = "median/2")

raw.short.quality %>% 
  filter(speeder_flag == FALSE) %>% 
  filter(careless_longstr < 20) %>% 
  filter(careless_psychsyn > 0) %>% 
  filter(careless_mahadflag == FALSE) -> raw.short.quality
