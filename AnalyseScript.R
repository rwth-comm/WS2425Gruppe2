# Pakete aktivieren ----
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/Methodenseminar_WS2425_Fragebogen.docx")

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
