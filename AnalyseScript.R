# Pakete aktivieren ----
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/Methodenseminar+WS2425_6.+Dezember+2024_13.12.csv")

# Rohdaten filtern ----
raw %>% 
  filter(Progress == 100) %>% 
  filter(Status == 2) -> raw


# Überflüssige Rohdaten entfernen ----
raw.short <- raw[,c(-1:-18)]

# Variablen umbenennen ----
generate_codebook(raw.short, "data/Methodenseminar+WS2425_6.+Dezember+2024_13.12.csv","data/codebook.csv")
codebook <- read_codebook("data/codebook_final.csv")
names(raw.short) <- codebook$variable

# Korrekte Datentypen zuordnen ----
raw.short$Age %>% 
  as.numeric(raw.short$age) -> raw.short$Age

raw.short$gender %>% 
  recode(`1` = "Männlich", `2` = "Weiblich", `3` = "Divers", `4` = "Keine Angabe") %>%
as.factor() -> raw.short$gender

raw.short$Bildungsabschluss %>% 
  ordered(levels = c(1:5),
          labels = c("(noch)Kein Schulabschluss",
                     "Hauptschulabschluss",
                     "Realschulabschluss",
                     "Abitur",
                     "Hochschulabschluss")) -> raw.short$Bildungsabschluss

raw.short$JobType %>% recode(`1` = "In Ausbildung / Studium", 
                             `2` = "Arbeitnehmer/-in und Studierende/-r",
                             `3` = "Arbeitnehmer/-in",
                             `4` = "Arbeitgeber/-in",
                             `5` = "Selbstständig ohne Mitarbeiter",
                             `6` = "Rentner/-in") %>% 
  as.factor() -> raw.short$JobType

raw.short$Wohnort %>% 
  recode(`1` = "Ländlich", `2` = "Wohnort/Kleinstadt", `3` = "Großstadt") %>%
  as.factor() -> raw.short$Wohnort

raw.short$income %>% 
  ordered(levels = c(1:4),
          labels = c("unter 25.000€",
                     "25.000€ bis 49.999€",
                     "50.000€ bis 75.000€",
                     "über 75.000€")) -> raw.short$income
raw.short$MN %>% 
  ordered(levels = c(1:6),
          labels = c("nie",
                     "einmal im Monat",
                     "mehrmals pro Monat",
                     "einmal pro Woche",
                     "mehrmals in der Woche",
                     "täglich"))-> raw.short$MN


# Qualitätskontrolle ----

# Skalenwerte berechnen ----

schluesselliste <- list(
  BIG5 = c("BIG5_1","-BIG5_2n"),
  NZV = c("-NZV_1n","NZV_2","NZV_3"),
  ATI = c("ATI_1","ATI_2","-ATI_3n","ATI_4","ATI_5","-ATI_6n","ATI_7","-ATI_8n","ATI_9"),
  PD = c("-PD_1n","-PD_2n", "PD_3"),
  EGKI = c("-EKGI_1n","EKGI_2","-EKGI_3n","EKGI_4","-EKGI_5n"),
  EXPKIC = c("EXPKIC_1","XPKIC_2","XPKIC_3"),
  EZKV = c("EZKV_1","EZKV_2","EZKV_3","EZKV_4","EZKV_5","EZKV_6"),
  BSSZO = c("BSSZO_1","BSSZO_2","BSSZO_3","BSSZO_4","BSSZO_5"),
  EGN = c("EGN_1","EGN_2","EGN_3","EGN_4"),
  BIATT = c("BIATT_A_1","BIATT_A_2","BIATT_A_3","BIATT_A_4","BIATT_A_5","BIATT_A_6"),
  PW = c("PW_A_1","PW_A_2","PW_A_3","PW_A_4","PW_A_5"),
  TIA = c("TIA_A_1","TIA_A_2"),
  BIATT = c("BIATT_B_1","BIATT_B_2","BIATT_B_3","BIATT_B_4","BIATT_B_5","BIATT_B_6"),
  PW = c("PW_B_1","PW_B_2","PW_B_3","PW_B_4","PW_B_5"),
  TIA = c("TIA_B_1","TIA_B_2"),
  USA = c("USA_B_1","USA_B_2","USA_B_3","USA_B_4", "USA_B_5"),
  EZA = c("EZA_B_1","EZA_B_2"),
  BIATT = c("BIATT_C_1","BIATT_C_2","BIATT_C_3","BIATT_C_4","BIATT_C_5","BIATT_C_6"),
  PW = c("PW_C_1","PW_C_2","PW_C_3","PW_C_4","PW_C_5"),
  TIA = c("TIA_C_1","TIA_C_2"),
)

scores <- scoreItems(schluesselliste, items = raw.short, min = 1, max =9)

scores$alpha

scores$scores

data <- bind_cols(raw.short, scores$scores)

# Daten exportieren ----
write_rds(data, "data/data.rds")


### Poweranalyse ###

install.packages("pwr")
library(pwr)

# Variante 1: Stichprobengröße gesucht ----
pwr::pwr.t.test(n = NULL, sig.level = 0.05 , d = 0.8, power = 0.8)

# Variante 2: Signifikanzniveau gesucht ----
pwr::pwr.t.test(n = 200, sig.level = NULL , d = 0.2, power = 0.8)

# Variante 3: Effektstärke gesucht ----
pwr::pwr.t.test(n = 110, sig.level = 0.05 , d = NULL, power = 0.8)

# Variante 4: Power gesucht ----

install.packages("dataforsocialscience")
install.packages("lsr")
library(dataforsocialscience)
library(tidyverse)
library(lsr)
df <- robo_care
df_male <- filter(df, df$gender == "male")
df_female <- filter(df, df$gender == "female")

t.test(df_male$privacy_concerns, df_female$privacy_concerns)
cohensD(df_male$privacy_concerns, df_female$privacy_concerns)

pwr::pwr.t.test(n = 110, sig.level = 0.05, d = NULL , power = 0.5)