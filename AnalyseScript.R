# Pakete aktivieren ----
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/datacleaning_Beispieldaten.csv")

# Rohdaten filtern ----
raw %>% 
  filter(Progress == 100) %>% 
  filter(Status == 0) -> raw

# Überflüssige Rohdaten entfernen ----
raw.short <- raw[,c(6,9,18:54)]

# Variablen umbenennen ----
generate_codebook(raw.short, "data/datacleaning_Beispieldaten.csv","data/codebook.csv")
codebook <- read_codebook("data/codebook_final.csv")
names(raw.short) <- codebook$variable

# Korrekte Datentypen zuordnen ----
raw.short$Age %>% 
  as.numeric(raw.short$age) -> raw.short$Age

raw.short$Gender %>% 
  recode(`1` = "Männlich", `2` = "Weiblich", `3` = "Divers")
  as.factor() -> raw.short$Gender

raw.short$Edu %>% 
  ordered(levels = c(1:5),
          labels = c("Haupt- oder Realschulabschluss",
                                     "Fach-/Hochschulreife (Abitur)",
                                     "Ausbildung",
                                     "Hochschulabschluss",
                                     "Promotion")) -> raw.short$Edu

raw.short$JobType %>% recode(`1` = "In Ausbildung / Studium", 
                             `2` = "Arbeitnehmer/-in und Studierende/-r",
                             `3` = "Arbeitnehmer/-in",
                             `4` = "Arbeitgeber/-in",
                             `5` = "Selbstständig ohne Mitarbeiter",
                             `6` = "Rentner/-in") %>% 
  as.facotor() -> raw.short$JobType


# Qualitätskontrolle ----

# Skalenwerte berechnen ----

schluesselliste <- list(
  BF_Extraversion = c("-bf_1n","bf_6"),
  BF_Agreeableness = c("bf_2","-bf_7n"),
  BF_Openness = c("-bf_5n", "bf_10"),
  BF_Neuroticism = c("-bf_4n", "bf_9"),
  BF_Concientiousness= c("-bf_3n", "bf_8"),
  ATI = vars4psych(raw.short, "ati"),
  PRO = c("wrfq_1","wrfq_2","wrfq_3","wrfq_4","wrfq_9"),
  PRE = c("wrfq_5","wrfq_6","wrfq_7","wrfq_8"),
  SVI = vars4psych(raw.short, "svi")
)

scores <- scoreItems(schluesselliste, items = raw.short, min = 1, max = 6)

scores$alpha

scores$scores

data <- bind_cols(raw.short, scores$scores)

# Daten exportieren ----
write_rds(data, "data/data.rds")

